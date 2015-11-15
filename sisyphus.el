;;; sisyphus.el --- Test support functions -*- lexical-binding: t -*-

;;; Header:

;; This file is not part of Emacs

;; Author: Phillip Lord <phillip.lord@russet.org.uk>
;; Maintainer: Phillip Lord <phillip.lord@russet.org.uk>
;; Version: 0.1
;; Package-Requires: ((emacs "24.4")(m-buffer "0.13"))

;; The contents of this file are subject to the GPL License, Version 3.0.

;; Copyright (C) 2015, Phillip Lord

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

;; This file will provide functions to support ert, including a set of
;; predicates, some nicer reporter functions and so forth.

;;; Thoughts:

;; Really, all the functions in here should be tested. But some of the tests are
;; really, realy hard to test because they depend on the output of ert test,





;;; Code:

;; ** Require

;; #+begin_src emacs-lisp
(require 'pp)
(require 'ert)
(require 'm-buffer-at)
(require 'm-buffer)
(require 'dash)
;; #+end_src

;; #+begin_src emacs-lisp
(define-error 'deliberate-error
  "An error deliberately caused during testing."
  'error)
;; #+end_src


;; ** Advice


;; #+begin_src emacs-lisp
(when (= emacs-major-version 24)

  (defun sisyphus--ert-pp-with-indentation-and-newline (orig object)
    (let ((pp-escape-newlines nil))
      (funcall orig object)))

  (advice-add
   'ert--pp-with-indentation-and-newline
   :around
   #'sisyphus--ert-pp-with-indentation-and-newline))
;; #+end_src


;; ** Explainers

;; Explainer functions add "explainations" for things that fail. We can use this
;; to make `string=' work properly. Can we do this without diff?

;; This is critical because most of our comparitors depend on this.

;; So this works but the output is printed out as a string with slash-n's
;; rather than as a multi-line string. Bollocks

;; Think it is ert--pp-with-indentation-and-newline which is the evil function
;; causing the problem. Which in turn backs onto pp.

;; pp has a variable pp-escape-newlines which set to nil solves the problem.

;; How do do this cleanly? Apply patch to ert.el?


;; #+begin_src emacs-lisp
(defun sisyphus-to-string (x)
  "Turn X into a string in a type appropriate way."
  (pcase x
   ((pred stringp) x)
   ((pred bufferp) (m-buffer-at-string x))
   (`(:buffer ,b) (sisyphus-to-string (get-buffer-create b)))
   (`(:file ,f)
    (with-temp-buffer
      (insert-file-contents f)
      (buffer-string)))
   ;; error condition
   (_ (error "Type not recognised"))))

(defun sisyphus-buffer (b)
  "Add type data to the string B marking it as a buffer."
  `(:buffer b))

(defun sisyphus-file (f)
  "Add type data to the string F marking it as a file."
  `(:file ,f))
;; #+end_src

;; *** String Comparision

;; #+begin_src emacs-lisp
(defun sisyphus--write-file-silently (filename)
  "Write current buffer into FILENAME.
Unlike most other ways of saving a file, this should not
print any messages!"
  (write-region
   (point-min) (point-max)
   filename nil
   'dont-display-wrote-file-message))

(defun sisyphus--explainer-diff-string= (a b)
  "Compare strings using diff output."
  (sisyphus-with-preserved-buffer-list
   (let* ((diff
           (executable-find "diff"))
          (a-buffer
           (generate-new-buffer "a"))
          (b-buffer
           (generate-new-buffer "b"))
          (a-file
           (make-temp-file
            (buffer-name a-buffer)))
          (b-file
           (make-temp-file
            (buffer-name b-buffer))))
     (with-current-buffer
         a-buffer
       (insert a)
       (sisyphus--write-file-silently a-file))
     (with-current-buffer
         b-buffer
       (insert b)
       (sisyphus--write-file-silently b-file))
     (prog1
         (format "Strings:\n%s\nand\n%s\nDiffer at:%s\n"
                 a b
                 (with-temp-buffer
                   (call-process
                    diff
                    ;; no infile
                    nil
                    ;; dump to current buffer
                    t
                    nil
                    "-c"
                    a-file
                    b-file)
                   (buffer-string)))))))

(defun sisyphus--explainer-simple-string= (a b)
  "Compare strings for first difference."
  ;; We could do a bit more here.
  (format "String :%s:%s: are not equal."))

(defun sisyphus= (a b)
  "Compare A and B to see if they are the same.

Equality in this sense means compare the contents in a way which
is appropriate for the type of the two arguments. So, if they are
strings, the compare strings, if buffers, then compare the buffer
contents and so on."
  (string=
   (sisyphus-to-string a)
   (sisyphus-to-string b)))

(defun sisyphus-explain= (a b)
  "Compare A and B and return an explanation.
See `sisyphus=' for more information."
  (let ((a (sisyphus-to-string a))
        (b (sisyphus-to-string b)))
    (cond
     ((sisyphus= a b)
      t)
     ((executable-find "diff")
      (sisyphus--explainer-diff-string= a b))
     (t
      (sisyphus--explainer-simple-string= a b)))))

(put 'sisyphus= 'ert-explainer 'sisyphus-explain=)
;; #+end_src


;; ** create buffers
(defmacro sisyphus-with-preserved-buffer-list (&rest body)
  "Evaluate BODY, but delete any buffers that have been created."
  (declare (debug t))
  `(let ((before-buffer-list
          (buffer-list)))
     (unwind-protect
         (progn
           ,@body)
       (--map
        (kill-buffer it)
        (-difference (buffer-list)
                     before-buffer-list)))))

(defun sisyphus--temp-buffer-let-form (item)
  (if (not (listp item))
      (sisyphus--temp-buffer-let-form
       (list item))
    `(,(car item)
      (with-current-buffer
          (generate-new-buffer "sisyphus-with-temp-buffers")
        ,@(cdr item)
        (current-buffer)))))

(defmacro sisyphus-with-temp-buffers (varlist &rest body)
  "Bind variables in varlist to temp buffers, then eval BODY.

VARLIST is of the same form as a `let' binding. Each element is a
symbol or a list (symbol valueforms). Each symbol is bound to a
buffer generated with `generate-new-buffer'. VALUEFORMS are
evaluated with the buffer current. Buffers are unconditionally
killed at the end of the form."
  (declare (indent 1)
           (debug let))
  (let ((let-form
         (-map
          #'sisyphus--temp-buffer-let-form
          varlist)))
    `(sisyphus-with-preserved-buffer-list
      (let ,let-form
        ,@body))))

;; ** Open files

;; Again, same issues -- what if the file is already open. Especially if are
;; going to save it.

;; ** Indentation functions

;; This is largely a re--implementation of `indent-region' but without the
;; noise.
(defun sisyphus--indent-buffer ()
  (cond
   ;; if indent-region-function is set, use it, and hope that it is not
   ;; noisy.
   (indent-region-function
    (funcall indent-region-function (point-min) (point-max)))
   (t
    (-map
     (lambda (m)
       (goto-char m)
       (indent-according-to-mode))
     (m-buffer-match-line-start (current-buffer))))))

(defun sisyphus--indent-in-mode (mode unindented)
  (with-temp-buffer
    (insert
     (sisyphus-to-string unindented))
    (funcall mode)
    (sisyphus--indent-buffer)
    (buffer-string)))

(defun sisyphus-indentation= (mode unindented indented)
  "Return non-nil if UNINDENTED indents in MODE to INDENTED.
Both UNINDENTED and INDENTED can be any value usable by
`sisyphus-to-string'. Indentation is performed using
`indent-region'."
  (sisyphus=
   (sisyphus--indent-in-mode
    mode
    unindented)
   indented))

(defun sisyphus-explain-indentation= (mode unindented indented)
  (sisyphus-explain=
   (sisyphus--indent-in-mode
    mode
    unindented)
   indented))

(put 'sisyphus-indentation= 'ert-explainer 'sisyphus-explain-indentation=)

;; Set mode, indent normally, then compare

;; ** Font-lock support functions

;; Set up a buffer from string or file, font-lock it, test it.

;; ** Pre/post command support functions

;; Not sure how I can test these better -- but worth thinking about -- I guess do
;; some set up, then and buffer-local pre or post command, run some stuff, compare.

;; #+begin_src emacs-lisp
(provide 'sisyphus)
;; #+end_src


;; #+begin_src emacs-lisp
;;; sisyphus.el ends here
;; #+end_src
