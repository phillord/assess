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
;; wh




;;; Code:

;; ** Require

;; #+begin_src emacs-lisp
(require 'pp)
(require 'ert)
(require 'm-buffer-at)
;; #+end_src

;; ** Advice


;; #+begin_src emacs-lisp
(defun sisyphus--ert-pp-with-indentation-and-newline (orig object)
  (let ((pp-escape-newlines nil))
    (funcall orig object)))

(advice-add
 'ert--pp-with-indentation-and-newline
 :around
 #'sisyphus--ert-pp-with-indentation-and-newline)
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

;; *** String Comparision

;; #+begin_src emacs-lisp
(defun sisyphus--write-file-silently (filename)
  "Write current buffer into FILENAME.
Unlike most other ways of saving a file, this should not
print any messages!."
  (write-region
   (point-min) (point-max)
   filename nil
   'dont-display-wrote-file-message))

(defun sisyphus--explainer-diff-string= (a b)
  "Compare strings using diff output."
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
                  (buffer-string)))
      (kill-buffer a-buffer)
      (kill-buffer b-buffer))))

(defun sisyphus--explainer-simple-string= (a b)
  "Compare strings for first difference."
  ;; We could do a bit more here.
  (format "String :%s:%s: are not equal."))

(defun sisyphus-explain-string= (a b)
  "Compare strings and return an explanation."
  (cond
   ((string= a b)
    t)
   ((executable-find "diff")
    (sisyphus--explainer-diff-string= a b))
   (t
    (sisyphus--explainer-simple-string= a b))))

(put 'string= 'ert-explainer 'sisyphus-explain-string=)
;; #+end_src

;;; *** String to buffer

;; #+begin_src emacs-lisp
(defun sisyphus-buffer-string= (buffer string)
  (string=
   (m-buffer-at-string buffer)
   string))

(defun sisyphus-explain-buffer-string= (buffer string)
  (sisyphus-explain-string=
   (m-buffer-at-string buffer)
   string))

(put 'sisyphus-buffer-string=
     'ert-explainer
     'sisyphus-explain-buffer-string=)

;; #+end_src


;; Compare buffer to buffer
;; #+begin_src emacs-lisp

;; #+end_src

;; Compare string to file

;; Compare buffer to file

;; Compare file to file.

;; ** Create buffers

;; Need to think carefully about this, but would like to create buffers. Temp
;; buffers are generally a good option but what if we have a named buffer, but
;; the named buffer is already open. What happens if a test creates a buffer? Can
;; we detect the buffer creation and close them again?
;; "with-protected-buffer-list" perhaps, or "save-buffer-list-excursion".

;; ** Open files

;; Again, same issues -- what if the file is already open. Especially if are
;; going to save it.

;; ** Indentation functions

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
