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

;;; Packages

(when (require 'package nil t)
   (package-initialize))

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("nongnu-devel" . "https://elpa.nongnu.org/nongnu-devel/"))

(add-to-list 'package-selected-packages 'geiser)
(add-to-list 'package-selected-packages 'geiser-chez)
(add-to-list 'package-selected-packages 'paredit)

(package-read-all-archive-contents)
(unless (package-refresh-contents)
  (package-install-selected-packages))

;;; scheme

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code" t)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(show-paren-mode t)
(setq auto-mode-alist (cons '("\\.ss" . scheme-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.sls" . scheme-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.sps" . scheme-mode) auto-mode-alist))

;;; Chez Scheme

(require 'compile)
(add-to-list 'compilation-error-regexp-alist
             '("^\\(Exception\\|Warning\\).*: .* \\(line \\([0-9]+\\), char \\([0-9]+\\) of \\(.*\\)\\)" 5 3 4 nil 2))

;;; Geiser

(setq geiser-default-implementation 'chez)

;;; Orgmode

(org-babel-do-load-languages
 'org-babel-load-languages
 '((scheme . t)))
(setq org-confirm-babel-evaluate nil)

;;; Scheme syntax

(put 'with-syntax 'scheme-indent-function 1)

;; Local Variables:
;; mode: emacs-lisp
;; End:
