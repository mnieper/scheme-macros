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

(library (languages parsers-b81a7c77-7b66-494f-87ab-e1fdbe9de679)
  (export
    generate-parser
    &syntax-error
    make-syntax-error-condition
    syntax-error-condition?)
  (import
    (rnrs)
    (languages parser-tables-1729d467-cf02-480c-9da3-5c1f662abf8b))

  ;; Conditions

  (define-condition-type &syntax-error &condition
    make-syntax-error-condition
    syntax-error-condition?)

  (define syntax-error-condition
    (lambda ()
      (raise (make-syntax-error-condition))))

  ;; State machine

  (define generate-parser
    (lambda (parser-table token-values)
      (lambda ()
        (let f ([state-stack '(0)]
                [value-stack '()])
          (define process
            (lambda (terminal-idx value)
              (let f ([state-stack state-stack]
                      [value-stack value-stack])
                (define state (car state-stack))
                (define action
                  (parser-table-action parser-table state terminal-idx))
                (cond
                 [(action-shift-state action)
                  =>
                  (lambda (state)
                    (if (end-terminal? terminal-idx)
                        (values '() value-stack)
                        (values
                         (cons state state-stack)
                         (cons value value-stack))))]
                 [(action-reduce-rule action)
                  =>
                  (lambda (rl)
                    (define head (parser-table-head parser-table rl))
                    (define body-length (parser-table-body-length parser-table rl))
                    (define popped-stack
                      (list-tail state-stack body-length))
                    (let ([n (parser-table-goto parser-table (car popped-stack) head)])
                      (f (cons n popped-stack)
                         (parser-table-reduce parser-table rl value-stack))))]
                 [else
                   (syntax-error-condition)]))))
          (case-lambda
            [()
             (let-values ([(state-stack value-stack)
                           (process (end-terminal) #f)])
               (if (null? state-stack)
                   (car value-stack)
                   (syntax-error-condition)))]
            [(token)
             (let-values ([(kind value) (token-values token)])
               (let-values ([(state-stack value-stack)
                             (process kind value)])
                 (f state-stack value-stack)))]))))))
