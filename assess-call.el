;;; assess-call.el --- Call and Return -*- lexical-binding: t -*-

;;; Header:

;; This file is not part of Emacs

;; Author: Phillip Lord <phillip.lord@russet.org.uk>
;; Maintainer: Phillip Lord <phillip.lord@russet.org.uk>
;; Version: 0.1
;; Package-Requires: ((emacs "24.4")(m-buffer "0.14")(dash "2.12.0"))

;; The contents of this file are subject to the GPL License, Version 3.0.

;; Copyright (C) 2016, Phillip Lord

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Capture calls to functions, checking parameters and return values.

;;; Code:

;; ** Call Capture

;; Here we provide a function for tracing calls to a particular function. This
;; can be a direct or indirect call; parameters and return values are available
;; for inspection afterwards. For example:

;; #+begin_src elisp
;;   (assess-call-capture
;;     '+
;;     (lambda()
;;       (+ 1 1)))
;;   ;; => (((1 1) . 2))
;; #+end_src

;; The return value is a list of cons cells, one for each invocation, of the
;; parameters and return values.


;; #+begin_src emacs-lisp
(defvar assess-call--capture-store nil
  "Store for parameters captured.")

(defun assess-call--capture-advice (fn &rest args)
  "Advice for capturing args and return from a function."
  (let ((rtn (apply fn args)))
    (setq assess-call--capture-store
          (cons (cons args rtn)
                assess-call--capture-store))
    rtn))

(defun assess-call-capture (sym-fn fn)
  "Trace all calls to SYM-FN when FN is called with no args.

The return value is a list of cons cells, with car being the
parameters of the calls, and the cdr being the return value."
  (setq assess-call--capture-store nil)
  (advice-add sym-fn :around #'assess-call--capture-advice)
  (funcall fn)
  (advice-remove sym-fn #'siyphus-call--capture-advice)
  assess-call--capture-store)

(provide 'assess-call)
;;; assess-call.el ends here
;; #+end_src
