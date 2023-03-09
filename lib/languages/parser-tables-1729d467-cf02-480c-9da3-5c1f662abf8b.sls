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

(library (languages parser-tables-1729d467-cf02-480c-9da3-5c1f662abf8b)
  (export
    make-parser-table
    parser-table?
    parser-table-action
    parser-table-goto
    parser-table-head
    parser-table-body-length
    parser-table-reduce
    make-actions
    action-shift-state
    action-reduce-rule
    actions-set-shift!
    actions-set-reduce!
    actions-ref
    make-gotos
    gotos-set-goto!
    gotos-ref
    end-terminal?
    end-terminal)
  (import
    (rnrs))

  ;; Tables

  (define-record-type parser-table
    (nongenerative parser-table-3f7e02ce-1819-4a51-b570-b5a78284fedd)
    (sealed #t) (opaque #t)
    (fields
      nonterminal-count
      terminal-count
      actions
      gotos
      heads
      body-lengthes
      reducers))

  (define parser-table-action
    (lambda (parser-table state terminal)
      (vector-ref (parser-table-actions parser-table)
                  (actions-index (parser-table-terminal-count
                                  parser-table)
                                 state
                                 terminal))))

  (define parser-table-goto
    (lambda (parser-table state nonterminal)
      (vector-ref (parser-table-gotos parser-table)
                  (+ (* (parser-table-nonterminal-count
                         parser-table)
                        state)
                     nonterminal))))

  (define parser-table-head
    (lambda (parser-table rule)
      (vector-ref (parser-table-heads parser-table) rule)))

  (define parser-table-body-length
    (lambda (parser-table rule)
      (vector-ref (parser-table-body-lengthes parser-table) rule)))

  (define parser-table-reduce
    (lambda (parser-table rule values)
      ((vector-ref (parser-table-reducers parser-table) rule) values)))

  (define make-actions
    (lambda (state-count terminal-count)
      (make-vector (* state-count terminal-count)
                   #f)))

  (define actions-ref
    (lambda (actions terminal-count state terminal)
      (vector-ref actions (actions-index terminal-count state terminal))))

  (define action-shift-state
    (lambda (action)
      (and action (not (negative? action)) action)))

  (define action-reduce-rule
    (lambda (action)
      (and action (negative? action) (bitwise-not action))))

  (define actions-set-shift!
    (lambda (actions terminal-count state terminal new-state)
      (vector-set! actions
                   (actions-index terminal-count state terminal)
                   new-state)))

  (define actions-set-reduce!
    (lambda (actions terminal-count state terminal rule)
      (vector-set! actions
                   (actions-index terminal-count state terminal)
                   (bitwise-not rule))))

  (define make-gotos
    (lambda (state-count nonterminal-count)
      (make-vector (* state-count nonterminal-count)
                   #f)))

  (define gotos-ref
    (lambda (gotos nonterminal-count state nonterminal)
      (vector-ref gotos (gotos-index nonterminal-count state nonterminal))))

  (define gotos-set-goto!
    (lambda (gotos nonterminal-count state nonterminal new-state)
      (vector-set! gotos
                   (gotos-index nonterminal-count state nonterminal)
                   new-state)))

  (define end-terminal
    (lambda ()
      0))

  (define end-terminal?
    (lambda (n)
      (zero? n)))

  (define actions-index
    (lambda (terminal-count state terminal)
      (+ (* terminal-count state) terminal)))

  (define gotos-index
    (lambda (nonterminal-count state nonterminal)
      (+ (* nonterminal-count state) nonterminal)))

  )
