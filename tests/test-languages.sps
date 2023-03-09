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

(import (rnrs)
        (languages))

(define-language (make-parser token token?)
  (nonterminals exp term  factor)
  (terminals NUM "+" "-" "*" "/" "(" ")")
  (rules
   [(exp -> exp "+" term)
    (+ exp term)]
   [(exp -> exp "-" term)
    (- exp term)]
   [(exp -> term)]
   [(term -> term "*" factor)
    (* term factor)]
   [(term -> term "/" factor)
    (/ term factor)]
   [(term -> factor)]
   [(factor -> NUM)]
   [(factor -> "(" exp ")")
    exp])
  (start exp))

(assert
 (equal?
  16
  (parse (make-parser)
         (token NUM 3)
         (token "+")
         (token NUM 2)
         (token "*")
         (token NUM 7)
         (token "-")
         (token NUM 1))))
