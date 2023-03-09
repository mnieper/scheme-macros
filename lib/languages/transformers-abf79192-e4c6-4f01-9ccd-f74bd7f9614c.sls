#!r6rs

;; Copyright (C) Marc Nieper-Wißkirchen (2023).  All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(library (languages transformers-abf79192-e4c6-4f01-9ccd-f74bd7f9614c)
  (export
    define-language-transformer
    nonterminals
    terminals
    rules
    start
    ->
    &syntax-error
    make-syntax-error-condition
    syntax-error-condition?)
  (import
    (rnrs)
    (auxiliary-syntax)
    (languages parser-generators-312c1223-af3a-40a8-b0b9-8cc07df14edc)
    (languages parsers-b81a7c77-7b66-494f-87ab-e1fdbe9de679)
    (languages parser-tables-1729d467-cf02-480c-9da3-5c1f662abf8b))

  ;; Auxiliary syntax

  (define-auxiliary-syntax nonterminals)
  (define-auxiliary-syntax terminals)
  (define-auxiliary-syntax rules)
  (define-auxiliary-syntax start)
  (define-auxiliary-syntax ->)

  (define who 'define-language)

  ;; Names (for terminals and non-terminals)

  (define name?
    (lambda (stx)
      (define e (syntax->datum stx))
      (or (symbol? e)
          (string? e)
          (char? e)
          (number? e)
          (null? e)
          (bytevector? e))))

  (define canonical-name
    (lambda (name)
      (define e (syntax->datum name))
      (if (symbol? e)
          (let ([s (symbol->string e)])
            (let f ([n (string-length s)])
              (if (zero? n)
                  e
                  (let ([n (- n 1)])
                    (if (char-numeric? (string-ref s n))
                        (f n)
                        (string->symbol (substring s 0 (+ n 1))))))))
          e)))

  (define name-hash
    (lambda (name)
      (equal-hash (canonical-name name))))

  (define name=?
    (lambda (name1 name2)
      (equal? (canonical-name name1) (canonical-name name2))))

  (define make-name-hashtable
    (lambda ()
      (make-hashtable name-hash name=?)))

  ;; Record-types

  (define-record-type component
    (nongenerative)
    (fields symbol name))

  (define make-terminal-component
    (lambda (index name)
      (make-component (index->terminal index) name)))

  (define make-nonterminal-component
    (lambda (index name)
      (make-component (index->nonterminal index) name)))

  (define-record-type rule
    (nongenerative)
    (fields head body syntax names action))

  (define rule-head-symbol
    (lambda (rule)
      (index->nonterminal (rule-head rule))))

  (define rule-body-symbols
    (lambda (rule)
      (vector-map component-symbol (rule-body rule))))

  ;; Transformer

  (define define-language-transformer
    (lambda (stx)
      (syntax-case stx ()
        [(_ (make-parser token token?) cl ...)
         (for-all identifier? #'(make-parser token token?))
         (compile stx #'make-parser #'token #'token? #'(cl ...))]
        [_
         (syntax-violation who "invalid syntax" stx)])))

  (define compile
    (lambda (stx make-parser-id token-id token?-id cl*)
      (let-values ([(nonterminal-count
                     terminal-names
                     terminal-indices
                     rules)
                    (parse-grammar stx cl*)])
        (define terminal-count
          (+ 1 (vector-length terminal-names)))
        (let-values
            ([(actions gotos)
              (guard-grammar-conflict (stx)
               (generate-parser-tables nonterminal-count
                                       terminal-count
                                       (vector-map rule-head-symbol rules)
                                       (vector-map rule-body-symbols rules)))])
          (generate-output nonterminal-count
                           terminal-names
                           terminal-indices
                           rules
                           make-parser-id token-id token?-id
                           actions gotos)))))

  (define-syntax guard-grammar-conflict
    (syntax-rules ()
      [(guard-grammar-conflict (stx-expr) body1 ... body2)
       (let ([stx stx-expr])
         (guard
             (c [(shift/reduce-conflict-condition? c)
                 (syntax-violation who "shift/reduce conflict in grammar" stx)]
                [(reduce/reduce-conflict-condition? c)
                 (syntax-violation who "reduce/reduce conflict in grammar" stx)])
           body1 ... body2))]))

  (define rule->reducer
    (lambda (rule)
      (define names (vector->list (rule-names rule)))
      (define tmp* (generate-temporaries names))
      (with-syntax ([(tmp ...) tmp*]
                    [action-expr (rule-action rule)]
                    [(arg ...)
                     (map
                      (lambda (name tmp)
                        (if (identifier? name)
                            name
                            tmp))
                      names tmp*)])
        #`(lambda (value-stack)
            #,(fold-left
               (lambda (expr tmp)
                 (with-syntax ([tmp tmp] [expr expr])
                   #'(let ([tmp (car value-stack)]
                           [value-stack (cdr value-stack)])
                       expr)))
               #'(cons (let ([arg tmp] ...)
                         action-expr)
                       value-stack)
               #'(tmp ...))))))

  (define generate-output
    (lambda (nonterminal-count terminal-names terminal-indices
                               rules make-parser token token? actions gotos)
      (define terminal-count (+ 1 (vector-length terminal-indices)))
      (define reducer* (vector-map rule->reducer rules))
      (define heads
        (vector-map rule-head rules))
      (define body-lengthes
        (vector-map
         (lambda (rule)
           (vector-length (rule-body rule)))
         rules))
      (with-syntax ([(tmp) (generate-temporaries '(tmp))])
        (with-syntax
            ([actions actions]
             [gotos gotos]
             [#(reducer ...) reducer*]
             [heads heads]
             [body-lengthes body-lengthes]
             [make-parser make-parser]
             [token token]
             [token? token?]
             [record-name (datum->syntax #'tmp (syntax->datum #'token))]
             [#(terminal-name ...) terminal-names]
             [#(terminal-index ...) terminal-indices]
             [terminal-count terminal-count]
             [nonterminal-count nonterminal-count])
          #'(begin
              (define parser-table
                (make-parser-table
                 nonterminal-count
                 terminal-count
                 'actions
                 'gotos
                 'heads
                 'body-lengthes
                 (vector reducer ...)))
              (define-record-type (record-name make-token token?)
                (sealed #t) (opaque #t)
                (fields kind value))
              (define-syntax token
                (lambda (stx)
                  (define who 'token)
                  (define generate-output
                    (lambda (name val-expr)
                      (define e (syntax->datum name))
                      (define terminal
                        (cond
                         [(equal? e 'terminal-name)
                          terminal-index]
                         ...
                         [else
                          (syntax-violation who "unknown terminal" stx name)]))
                      (with-syntax ([terminal terminal]
                                    [val val-expr])
                        #'(make-token terminal val))))
                  (syntax-case stx ()
                    [(_ name)
                     (name? #'name)
                     (generate-output #'name #'#f)]
                    [(_  name val-expr)
                     (generate-output #'name #'val-expr)]
                    [_ (syntax-violation who "invalid syntax" stx)])))
              (define token-values
                (lambda (token)
                  (unless (token? token)
                    (assertion-violation #f "invalid token" token))
                  (values (token-kind token) (token-value token))))
              (define make-parser (generate-parser parser-table token-values)))))))

  ;; Parsers

  (define parse-grammar
    (lambda (stx cl*)
      (define components (make-name-hashtable))
      (define name->component
        (lambda (name)
          (or (hashtable-ref components name #f)
              (syntax-violation who "unknown symbol" stx name))))
      (let-values ([(nonterminals-cl terminals-cl rules-cl start-cl)
                    (parse-clauses stx cl*)])
        (define nonterminal-table
          (parse-nonterminals-clause! stx nonterminals-cl components))
        (define nonterminal-count
          (+ 1 (hashtable-size nonterminal-table)))
        (define nonterminal->index
          (lambda (name)
            (or (hashtable-ref nonterminal-table name #f)
                (syntax-violation who "unknown nonterminal" stx name))))
        (let-values ([(terminal-names terminal-indices)
                      (parse-terminals-clause! stx terminals-cl components)])
          (define start (parse-start-clause stx start-cl name->component))
          (define rules (parse-rules-clause stx rules-cl name->component nonterminal->index start))
          (values nonterminal-count terminal-names terminal-indices rules)))))

  (define parse-clauses
    (lambda (stx cl*)
      (let f ([cl* cl*]
              [nonterminals-cl #f]
              [terminals-cl #f]
              [rules-cl #f]
              [start-cl #f])
        (syntax-case cl* ()
          [()
           (begin
             (unless nonterminals-cl
               (syntax-violation who "missing nonterminals clause" stx))
             (unless terminals-cl
               (syntax-violation who "missing terminals clause" stx))
             (unless rules-cl
               (syntax-violation who "missing rules clause" stx))
             (unless start-cl
               (syntax-violation who "missing start clause" stx))
             (values nonterminals-cl
                     terminals-cl
                     rules-cl
                     start-cl))]
          [(cl . cl*)
           (let ([cl #'cl] [cl* #'cl*])
             (syntax-case cl (terminals nonterminals rules start)
               [(nonterminals . _)
                (when nonterminals-cl
                  (syntax-violation who "duplicate nonterminals clause" stx cl))
                (f cl* cl terminals-cl rules-cl start-cl)]
               [(terminals . _)
                (when terminals-cl
                  (syntax-violation who "duplicate terminals clause" stx cl))
                (f cl* nonterminals-cl cl rules-cl start-cl)]
               [(rules . _)
                (when rules-cl
                  (syntax-violation who "duplicate rules clause" stx cl))
                (f cl* nonterminals-cl terminals-cl cl start-cl)]
               [(start . _)
                (when start-cl
                  (syntax-violation who "duplicate start clause" stx cl))
                (f cl* nonterminals-cl terminals-cl rules-cl cl)]
               [_
                (syntax-violation who "invalid grammar clause" stx cl)]))]))))

  (define parse-terminals-clause!
    (lambda (stx cl components)
      (define terminal-table (make-name-hashtable))
      (syntax-case cl ()
        [(_ name ...)
         (for-all name? #'(name ...))
         (let ([names #'#(name ...)])
           (define n (vector-length names))
           (do ([i 1 (+ i 1)])
               ((> i n)
                (hashtable-entries terminal-table))
             (let ([name (vector-ref names (- i 1))])
               (hashtable-update!
                components
                name
                (lambda (val)
                  (when val
                    (syntax-violation who "duplicate symbol" stx name))
                  (let ([terminal (make-terminal-component i name)])
                    terminal))
                #f)
               (hashtable-set! terminal-table name i))))]
        [_
         (syntax-violation who "invalid terminals clause" stx cl)])))

  (define parse-nonterminals-clause!
    (lambda (stx cl components)
      (define nonterminal-table (make-name-hashtable))
      (syntax-case cl ()
        [(_ name ...)
         (for-all name? #'(name ...))
         (let ([names #'#(name ...)])
           (define n (vector-length names))
           (do ([i 0 (+ i 1)])
               ((= i n) nonterminal-table)
             (let ([name (vector-ref names i)])
               (hashtable-update!
                components
                name
                (lambda (val)
                  (when val
                    (syntax-violation who "duplicate symbol" stx name))
                  (let ([nonterminal (make-nonterminal-component i name)])
                    nonterminal))
                #f)
               (hashtable-set! nonterminal-table name i))))]
        [_
         (syntax-violation who "invalid nonterminals clause" stx cl)])))

  (define parse-start-clause
    (lambda (stx cl name->component)
      (syntax-case cl ()
        [(_ name)
         (identifier? #'name)
         (name->component #'name)]
        [_
         (syntax-violation who "invalid start clause" stx cl)])))

  (define parse-rules-clause
    (lambda (stx cl name->component nonterminal->index start)
      (syntax-case cl ()
        [(_ rule ...)
         (let ([rule* #'#(rule ...)])
           (define n (vector-length rule*))
           (define rules (make-vector (+ n 1)))
           (do ([i 1 (+ i 1)]
                [rules (make-vector (+ n 1))])
               ((> i n)
                (vector-set! rules 0
                             (make-rule 0 (vector start
                                                  (make-component (end-terminal)
                                                                  #'$end)) #f
                                                                  (vector #'tmp #'#f)
                                                                  #'tmp))
                rules)
             (vector-set! rules i (parse-rule stx
                                              (vector-ref rule* (- i 1)) i
                                              name->component
                                              nonterminal->index))))]
        [_
         (syntax-violation who "invalid rules clause" stx cl)])))

  (define parse-rule
    (lambda (stx rl i name->component nonterminal->index)
      (define do-rule
        (lambda (symbol body* name* action-expr)
          (define head (nonterminal->index symbol))
          (define body (vector-map name->component body*))
          (make-rule head body rl name* action-expr)))
      (syntax-case rl (->)
        [[(symbol -> body ...) action]
         (for-all name? #'(symbol body ...))
         (do-rule #'symbol #'#(body ...) #'#(body ...) #'action)]
        [[(symbol -> body)]
         (for-all name? #'(symbol body))
         (do-rule #'symbol #'#(body) (vector #'tmp) #'tmp)]
        [_
         (syntax-violation who "invalid rule" stx rl)]))))
