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

(library (languages parser-generators-312c1223-af3a-40a8-b0b9-8cc07df14edc)
  (export
    generate-parser-tables
    index->terminal index->nonterminal
    end-terminal
    terminal? nonterminal?
    &grammar-conflict
    make-grammar-conflict-condition
    grammar-conflict-condition?
    &shift/reduce-conflict
    make-shift/reduce-conflict-condition
    shift/reduce-conflict-condition?
    &reduce/reduce-conflict
    make-reduce/reduce-conflict-condition
    reduce/reduce-conflict-condition?)
  (import
    (rnrs)
    (languages parser-tables-1729d467-cf02-480c-9da3-5c1f662abf8b))

  ;; Conditions

  (define-condition-type &grammar-conflict &condition
    make-grammar-conflict-condition
    grammar-conflict-condition?)

  (define-condition-type &shift/reduce-conflict &grammar-conflict
    make-shift/reduce-conflict-condition
    shift/reduce-conflict-condition?)

  (define-condition-type &reduce/reduce-conflict &grammar-conflict
    make-reduce/reduce-conflict-condition
    reduce/reduce-conflict-condition?)

  (define shift/reduce-conflict-condition
    (lambda ()
      (raise (make-shift/reduce-conflict-condition))))

  (define reduce/reduce-conflict-condition
    (lambda ()
      (raise (make-reduce/reduce-conflict-condition))))

  ;; Record-type definitions

  (define index->nonterminal
    (lambda (idx)
      (assert (integer? idx))
      (bitwise-not idx)))

  (define index->terminal
    (lambda (idx)
      (assert (integer? idx))
      idx))

  (define terminal->index
    (lambda (sym)
      (assert (terminal? sym))
      sym))

  (define nonterminal->index
    (lambda (sym)
      (assert (nonterminal? sym))
      (bitwise-not sym)))

  (define terminal?
    (lambda (sym)
      (not (negative? sym))))

  (define nonterminal?
    (lambda (sym)
      (negative? sym)))

  (define grammar-symbol=?
    (lambda (sym1 sym2)
      (= sym1 sym2)))

  ;; Helper macros

  (define-syntax block
    (syntax-rules ()
      [(block (repeat!) body1 ... body2)
       (let f ()
         (define changed #f)
         (define repeat!
           (lambda ()
             (set! changed #t)))
         body1 ... body2
         (when changed (f)))]))

  ;; Table generation

  (define generate-parser-tables
    (lambda (nonterminals terminals heads bodies)
      (define nullable? (make-nullable? heads bodies))
      (define first-sets
        (make-first-sets nonterminals heads bodies nullable?))
      (let-values ([(states edges) (make-states+edges heads bodies nullable? first-sets)])
        (define actions (make-action-table terminals heads bodies states edges))
        (define gotos (make-goto-table nonterminals states edges))
        (values actions gotos))))

  ;; Parser generation algorithm

  ;; See, for example, A. Appel: Modern Compiler Implementation in C.

  (define make-nullable?
    (lambda (heads bodies)
      (define nullable-set 0)
      (block (repeat!)
        (for-each-rule
         (lambda (head-idx j)
           (define body (vector-ref bodies j))
           (unless (bitwise-bit-set? nullable-set head-idx)
             (when
                 (body-for-all-symbol
                  (lambda (sym)
                    (and (nonterminal? sym)
                         (bitwise-bit-set? nullable-set (nonterminal->index sym))))
                  body)
               (set! nullable-set
                     (bitwise-ior nullable-set
                                  (bitwise-arithmetic-shift-left 1 head-idx)))
               (repeat!))))
         heads))
      (let ([nullable-set nullable-set])
        (define nullable?
          (lambda (nonterminal)
            (assert (nonterminal? nonterminal))
            (bitwise-bit-set? nullable-set (nonterminal->index nonterminal))))
        nullable?)))

  (define make-first-sets
    (lambda (nonterminals heads bodies nullable?)
      (define first-sets (make-vector nonterminals 0))
      (block (repeat!)
        (for-each-rule
         (lambda (head-idx j)
           (define body (vector-ref bodies j))
           (body-for-all-symbol
            (lambda (sym)
              (define old-first (vector-ref first-sets head-idx))
              (define delta-first
                (cond
                 [(terminal? sym)
                  (bitwise-arithmetic-shift-left 1 (terminal->index sym))]
                 [(nonterminal? sym)
                  (vector-ref first-sets (nonterminal->index sym))]
                 [else (assert #f)]))
              (define new-first (bitwise-ior old-first delta-first))
              (unless (= new-first old-first)
                (vector-set! first-sets head-idx new-first)
                (repeat!))
              (and (nonterminal? sym) (nullable? sym)))
            body))
         heads))
      first-sets))

  (define closure!
    (lambda (heads bodies nullable? first-sets state)
      (block (repeat!)
        (state-for-each-left-item/lookahead
         (lambda (body rl pos lookahead)
           (define sym (vector-ref body pos))
           (when (nonterminal? sym)
             (for-each-rule
              (lambda (head-idx i)
                (when (grammar-symbol=? sym (index->nonterminal head-idx))
                  (item-for-each-first
                   (lambda (first)
                     (when (state-adjoin! state i 0 first)
                       (repeat!)))
                   body (+ pos 1) lookahead
                   nullable? first-sets)))
              heads)))
         state heads bodies))))

  (define goto
    (lambda (heads bodies nullable? first-sets state symbol)
      (define output (make-state heads bodies))
      (state-for-each-left-item
       (lambda (body rl pos lookahead-set)
         (define n (vector-length body))
         (when (and (< pos n)
                    (grammar-symbol=? symbol (vector-ref body pos)))
           (let ([i rl])
             (define lookahead-sets (vector-ref output i))
             (vector-set! lookahead-sets
                          (+ pos 1)
                          (bitwise-ior (vector-ref lookahead-sets (+ pos 1))
                                       lookahead-set)))))
       state heads bodies)
      (closure! heads bodies nullable? first-sets output)
      output))

  (define make-states+edges
    (lambda (heads bodies nullable? first-sets)
      (define state-indices (make-state-hashtable))
      (define edge-table (make-edge-hashtable))
      (define start-state (make-start-state heads bodies nullable? first-sets))
      (define states (list start-state))
      (hashtable-set! state-indices start-state 0)
      (block (repeat!)
       (for-each
        (lambda (state)
          (define state-idx (assert (hashtable-ref state-indices state #f)))
          (state-for-each-left-item
           (lambda (body rl pos lookahead-set)
             (define n (vector-length body))
             (when (< pos n)
               (let ([sym (vector-ref body pos)])
                 (when (or (nonterminal? sym)
                           (> (terminal->index sym) 0))
                   (let ([goto-state (goto heads bodies nullable? first-sets state sym)])
                     (define goto-state-idx
                       (or (hashtable-ref state-indices goto-state #f)
                           (let ([idx (hashtable-size state-indices)])
                             (hashtable-set! state-indices goto-state idx)
                             (set! states (cons goto-state states))
                             (repeat!)
                             idx)))
                     (define edge (make-edge state-idx goto-state-idx sym))
                     (unless (hashtable-ref edge-table edge #f)
                       (hashtable-set! edge-table edge #t)
                       (repeat!)))))))
           state heads bodies))
        states))
      (values
       (list->vector (reverse states))
       (hashtable-keys edge-table))))

  (define make-start-state
    (lambda (heads bodies nullable? first-sets)
      (define state (make-state heads bodies))
      (vector-set! (vector-ref state 0) 0 1)
      (closure! heads bodies nullable? first-sets state)
      state))

  (define make-action-table
    (lambda (terminals heads bodies states edges)
      (define actions (make-actions (vector-length states) terminals))
      (vector-for-each
       (lambda (edge)
         (define sym (edge-symbol edge))
         (when (terminal? sym)
           (actions-set-shift! actions terminals (edge-from edge) (terminal->index sym) (edge-to edge))))
       edges)
      (do ([i 0 (+ i 1)])
          ((= i (vector-length states)))
        (state-for-each-right-item/lookahead
         (lambda (rl z)
           (define j (terminal->index z))
           (cond
            [(actions-ref actions terminals i z)
             => (lambda (action)
                  (if (action-reduce-rule action)
                      (reduce/reduce-conflict-condition)
                      (shift/reduce-conflict-condition)))])
           (actions-set-reduce! actions terminals i j rl))
         (vector-ref states i) heads bodies)
        (when (state-acceptable? (vector-ref states i))
          (when (actions-ref actions terminals i (index->terminal 0))
            (shift/reduce-conflict-condition))
          (actions-set-shift! actions terminals i (index->terminal 0) 0))
        states)
      actions))

  (define make-goto-table
    (lambda (nonterminals states edges)
      (define gotos (make-gotos (vector-length states) nonterminals))
      (vector-for-each
       (lambda (edge)
         (define sym (edge-symbol edge))
         (when (nonterminal? sym)
           (gotos-set-goto! gotos nonterminals (edge-from edge) (nonterminal->index sym) (edge-to edge))))
       edges)
      gotos))

  ;; States

  (define make-state
    (lambda (heads bodies)
      (define state (make-vector (vector-length heads)))
      (for-each-rule
       (lambda (head-idx i)
         (define body (vector-ref bodies i))
         (vector-set! state i (make-vector (+ (vector-length body) 1) 0)))
       heads)
      state))

  (define state=?
    (lambda (st1 st2)
      (equal? st1 st2)))

  (define state-hash
    (lambda (st)
      (equal-hash st)))

  (define make-state-hashtable
    (lambda ()
      (make-hashtable state-hash state=?)))

  ;; Edges

  (define-record-type edge
    (fields from to symbol))

  (define edge=?
    (lambda (edge1 edge2)
      (and (= (edge-from edge1) (edge-from edge2))
           (= (edge-to edge1) (edge-to edge2))
           (grammar-symbol=? (edge-symbol edge1) (edge-symbol edge2)))))

  (define edge-hash
    (lambda (E)
      (define symbol (edge-symbol E))
      (equal-hash (vector (edge-from E) (edge-to E)
                          symbol))))

  (define make-edge-hashtable
    (lambda ()
      (make-hashtable edge-hash edge=?)))

  ;; Helper procedures

  (define for-each-rule
    (lambda (proc heads)
      (define n (vector-length heads))
      (do ([i 0 (+ i 1)])
          ((= i n))
        (proc (nonterminal->index (vector-ref heads i)) i))))

  (define body-for-each-symbol
    (lambda (proc body)
      (define n (vector-length body))
      (do ([i 0 (+ i 1)])
          ((= i n))
        (proc (vector-ref body i)))))

  (define body-for-all-symbol
    (lambda (proc body)
      (define n (vector-length body))
      (let f ([i 0])
        (or (= i n)
            (and (proc (vector-ref body i))
                 (f (+ i 1)))))))

  (define for-each-state
    (lambda (proc states)
      (do ([i 0 (+ i 1)]
           [states states (cdr states)])
          ((null? states))
        (proc (car states) i))))

  (define terminal-set-for-each
    (lambda (proc set)
      (let f ([i 0] [set set])
        (define j (bitwise-first-bit-set set))
        (unless (= j -1)
          (proc (index->terminal (+ i j)))
          (f (+ i j 1)
             (bitwise-arithmetic-shift-right set (+ j 1)))))))

  (define state-for-each-left-item
    (lambda (proc state heads bodies)
      (for-each-rule
       (lambda (head-idx rl)
         (define body (vector-ref bodies rl))
         (define lookahead-sets (vector-ref state rl))
         (define n (vector-length body))
         (do ([i 0 (+ i 1)])
             ((= i n))
           (let ([lookahead-set (vector-ref lookahead-sets i)])
             (unless (zero? lookahead-set)
               (proc body rl i lookahead-set)))))
       heads)))

  (define state-for-each-left-item/lookahead
    (lambda (proc state heads bodies)
      (state-for-each-left-item
       (lambda (body rl i lookahead-set)
         (terminal-set-for-each
          (lambda (terminal)
            (proc body rl i terminal))
          lookahead-set))
       state heads bodies)))

  (define state-for-each-right-item/lookahead
    (lambda (proc state heads bodies)
      (for-each-rule
       (lambda (head-idx rl)
         (define body (vector-ref bodies rl))
         (define lookahead-sets (vector-ref state rl))
         (define n (vector-length body))
         (terminal-set-for-each
          (lambda (terminal)
            (proc rl terminal))
          (vector-ref lookahead-sets n)))
       heads)))

  (define state-acceptable?
    (lambda (state)
      (define lookahead-sets (vector-ref state 0))
      (not (zero? (vector-ref lookahead-sets (- (vector-length lookahead-sets) 2))))))

  (define item-for-each-first
    (lambda (proc body pos lookahead nullable? first-sets)
      (define n (vector-length body))
      (let f ([pos pos])
        (if (= pos n)
            (proc lookahead)
            (let ([sym (vector-ref body pos)])
              (cond
               [(terminal? sym)
                (proc sym)]
               [(nonterminal? sym)
                (terminal-set-for-each proc (vector-ref first-sets (nonterminal->index sym)))
                (when (nullable? sym)
                  (f (+ pos 1)))]
               [else (assert #f)]))))))

  (define state-adjoin!
    (lambda (state rule-idx pos lookahead)
      (define lookahead-sets (vector-ref state rule-idx))
      (define old-set (vector-ref lookahead-sets pos))
      (define new-set (bitwise-ior old-set
                                   (bitwise-arithmetic-shift-left 1 (terminal->index lookahead))))
      (and (not (= old-set new-set))
           (vector-set! lookahead-sets pos new-set)
           #t)))

  )
