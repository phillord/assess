;;; assess-test.el --- Tests for assess.el -*- lexical-binding: t -*-

;;; Header:

;; The contents of this file are subject to the GPL License, Version 3.0.

;; Copyright (C) 2015, 2016, Phillip Lord, Newcastle University

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


;;; Code:

;; ** Requires

;; #+begin_src emacs-lisp
(require 'load-relative)
(require 'assess)
(require 'cl-lib)

;; #+end_src

;; ** Test Extraction

;; Assess supports tests functions which means that we need the ability to test
;; tests. This code simple extracts knowledge from the results of tests.

;; #+begin_src emacs-lisp
(defun assess-test--plist-from-result (result)
  (cl-cdadr
   (ert-test-result-with-condition-condition result)))

(ert-deftest plist-extraction ()
  (should
   (equal
    (assess-test--plist-from-result
     (ert-run-test
      (make-ert-test
       :body
       (lambda ()
         (should
          (eq 1 2))))))
    '(:form (eq 1 2) :value nil))))

(defun assess-test--explanation-from-result (result)
  (plist-get
   (assess-test--plist-from-result result)
   :explanation))

(ert-deftest explanation-extraction-from-result ()
  "Test that explanation is extractable from failing test.
This also tests the advice on string=."
  (should
   (assess-test--explanation-from-result
    (ert-run-test
     (make-ert-test
      :body
      (lambda ()
        (should
         (assess= "1" "2"))))))))

(defun assess-test--explanation (f)
  (assess-test--explanation-from-result
   (ert-run-test
    (make-ert-test
     :body f))))

(ert-deftest explanation-extraction ()
  "Test that explanation is extractable from failing test.
This also tests the advice on string=."
  (should
   (assess-test--explanation
    (lambda ()
      (should
       (assess= "1" "2"))))))
;; #+end_src

;; ** To-String testing

;; #+begin_src emacs-lisp
(defvar assess-test-hello.txt
  (assess-file
   (relative-expand-file-name "../dev-resources/hello.txt")))

(ert-deftest to-string ()
  (should
   (equal "hello"
          (assess-to-string "hello")))
  (should
   (with-temp-buffer
     (equal "hello"
            (progn
              (insert "hello")
              (assess-to-string (current-buffer))))))
  (should
   (with-temp-buffer
     (equal "hello"
            (progn
              (insert "hello")
              (assess-to-string
               (list
                :buffer
                (buffer-name (current-buffer))))))))
  (should
   (with-temp-buffer
     (equal "hello\n"
            (assess-to-string
             assess-test-hello.txt))))
  (should-error
   (assess-to-string :hello)))

;; #+end_src

;; ** Compare Buffer to String

;; #+begin_src emacs-lisp

(ert-deftest buffer-string= ()
  (with-temp-buffer
    (insert "hello")
    (should
     (assess=
      (current-buffer)
      "hello")))
  (with-temp-buffer
    (insert "goodbye")
    (should-not
     (assess=
      (current-buffer)
      "hello")))
  (should
   (assess-test--explanation
    (lambda ()
      (with-temp-buffer
        (insert "goodbye")
        (should
         (assess=
          (current-buffer)
          "hello")))))))

;; #+end_src

;; ** Buffer to Buffer

;; #+begin_src emacs-lisp

(ert-deftest buffer= ()
  (assess-with-temp-buffers
      ((a
        (insert "hello"))
       (b
        (insert "hello")))
    (should
     (assess= a b)))
  (assess-with-temp-buffers
      ((a
        (insert "hello"))
       (b
        (insert "goodbye")))
    (should-not
     (assess=
      a b)))
  (should
   (assess-with-temp-buffers
       ((a (insert "hello"))
        (b (insert "goodbye")))
     (assess-test--explanation
      (lambda ()
        (should
         (assess=
          a b)))))))

;; #+end_src

;; ** Buffer to file

;; #+begin_src emacs-lisp
(ert-deftest file-string= ()
  (should
   (assess=
    assess-test-hello.txt
    "hello\n"))
  (should-not
   (assess=
    assess-test-hello.txt
    "goodbye"))
  (should
   (assess-test--explanation
    (lambda ()
      (should
       (assess=
        assess-test-hello.txt
        "goodbye"))))))


;; #+end_src

;; ** Preserved Buffer List and With Temp Buffers

;; #+begin_src emacs-lisp

(ert-deftest preserved-buffer-list ()
  (should
   (=
    (length (buffer-list))
    (progn
      (assess-with-preserved-buffer-list
       (generate-new-buffer "preserved-buffer-list"))
      (length (buffer-list)))))

  (should
   (=
    (length (buffer-list))
    (condition-case e
        (assess-with-preserved-buffer-list
         (generate-new-buffer "preserved-buffer-list")
         (signal 'assess-deliberate-error nil))
      (assess-deliberate-error
       (length (buffer-list)))))))

(ert-deftest with-temp-buffers ()
  (should
   (bufferp
    (assess-with-temp-buffers (a) a)))
  (should
   (bufferp
    (assess-with-temp-buffers
        (a (insert "hello"))
      a)))
  (should
   (equal
    "hello"
    (assess-with-temp-buffers
        ((a (insert "hello")))
      (with-current-buffer
          a
        (buffer-string)))))
  (should
   (=
    (+ 2 (length (buffer-list)))
    (assess-with-temp-buffers (a b)
      (length (buffer-list)))))
  (should
   (=
    (length (buffer-list))
    (progn
      (assess-with-temp-buffers (a b))
      (length (buffer-list))))))

;; #+end_src

;; ** Open Close files

;; #+begin_src emacs-lisp

(ert-deftest assess-test-related-file ()
  (should
   (file-exists-p
    (assess-to-file-name
     (assess-make-related-file assess-test-hello.txt))))
  (should
   (assess=
    assess-test-hello.txt
    (assess-make-related-file assess-test-hello.txt))))

(ert-deftest assess-test-with-find-file ()
  (should
   (assess-with-find-file
       (assess-make-related-file assess-test-hello.txt)))
  (should-not
   (assess=
    assess-test-hello.txt
    (assess-with-find-file
        (assess-make-related-file assess-test-hello.txt)
      (insert "hello")
      (buffer-string)))))

;; #+end_src

;; ** Indentation Tests

;; #+begin_src emacs-lisp

(ert-deftest assess--test-indent-in-mode ()
  (should
   (assess=
    "(
 (
  (
   (
    ))))"
    (assess--indent-in-mode
     'emacs-lisp-mode
     "(\n(\n(\n(\n))))"))))

(ert-deftest assess--test-indentation= ()
  (should
   (assess-indentation=
    'emacs-lisp-mode
    "(\n(\n(\n(\n))))"
    "(
 (
  (
   (
    ))))"))
  (should-not
   (assess-indentation=
    'emacs-lisp-mode
    "hello"
    "goodbye"))
  (should
   (assess-test--explanation
    (lambda ()
      (should
       (assess-indentation=
        'emacs-lisp-mode
        "hello"
        "goodbye"))))))

(defvar assess-dev-resources
  (relative-expand-file-name "../dev-resources/"))

(defvar assess-dev-elisp-indented
  (assess-file
   (concat assess-dev-resources
           "elisp-indented.el")))

(defvar assess-dev-elisp-unindented
  (assess-file
   (concat assess-dev-resources
           "elisp-unindented.el")))

(ert-deftest assess-test-roundtrip-indentation= ()
  (should
   (assess-roundtrip-indentation=
    'emacs-lisp-mode
    assess-dev-elisp-indented))
  (should-not
   (assess-roundtrip-indentation=
    'emacs-lisp-mode
    assess-dev-elisp-unindented)))

(ert-deftest assess-test-roundtrip-indentation-explain= ()
  (should
   (assess-test--explanation
    (lambda ()
      (should
       (assess-roundtrip-indentation=
        'emacs-lisp-mode
        assess-dev-elisp-unindented))))))

(ert-deftest assess-test-file-roundtrip-indentation= ()
  (should
   (assess-file-roundtrip-indentation=
    assess-dev-elisp-indented))
  (should-not
   (assess-file-roundtrip-indentation=
    assess-dev-elisp-unindented)))

(ert-deftest assess-test-file-roundtrip-indentation-explain= ()
  (should
   (assess-test--explanation
    (lambda ()
      (should
       (assess-file-roundtrip-indentation=
        assess-dev-elisp-unindented))))))

;; ** Face Tests
(defvar assess-dev-elisp-fontified
  (assess-file
   (concat assess-dev-resources
           "elisp-fontified.el")))

(ert-deftest assess-test-face-at-simple ()
  (should
   (assess-face-at=
    "(defun x ())"
    'emacs-lisp-mode
    2
    'font-lock-keyword-face))
  (should-not
   (assess-face-at=
    "(not-defun x ())"
    'emacs-lisp-mode
    2
    'font-lock-keyword-face)))

(ert-deftest assess-test-face-at-multiple-positions ()
  (should
   (assess-face-at=
    "(defun x ())
(defun y ())
(defun z ())"
    'emacs-lisp-mode
    '(2 15 28)
    'font-lock-keyword-face))
  (should-not
   (assess-face-at=
    "(defun x ())
(defun y ())
(not-defun z ())"
    'emacs-lisp-mode
    '(2 15 28)
    'font-lock-keyword-face)))

(ert-deftest assess-test-face-at-multiple-faces ()
  (should
   (assess-face-at=
    "(defun x ())"
    'emacs-lisp-mode
    '(2 8)
    '(font-lock-keyword-face font-lock-function-name-face)))
  (should-not
   (assess-face-at=
    "(defun x ())"
    'emacs-lisp-mode
    '(2 10)
    '(font-lock-keyword-face font-lock-function-name-face))))

(ert-deftest assess-test-face-at-with-m-buffer ()
  (should
   (assess-face-at=
    "(defun x ())\n(defun y ())\n(defun z ())"
    'emacs-lisp-mode
    (lambda(buf)
      (m-buffer-match buf "defun"))
    'font-lock-keyword-face)))

(ert-deftest assess-test-face-at-with-strings ()
  (should
   (assess-face-at=
    "(defun x ())\n(defun y ())\n(defun z ())"
    'emacs-lisp-mode
    "defun"
    'font-lock-keyword-face))
  (should
   (assess-face-at=
    "(defun x ())\n(defmacro y ())\n(defun z ())"
    'emacs-lisp-mode
    '("defun" "defmacro" "defun")
    'font-lock-keyword-face)))

(ert-deftest assess-test-file-face-at ()
  (should
   (assess-file-face-at=
    assess-dev-elisp-fontified
    (lambda (buffer)
      (m-buffer-match buffer "defun"))
    'font-lock-keyword-face)))

(ert-deftest assess-discover-test ()
  "Test to see if another test has been defined, which should be auto-discovered"
  (should
   (get 'assess-discover-test-has-this-been-defined 'ert--test)))

;; https://github.com/phillord/assess/issues/4
(ert-deftest issue-4-has-type-face ()
  "Test that no faces are present at point."
  (should-not
   (assess-face-at= "foo bar" 'fundamental-mode
                    "bar" 'font-lock-type-face))
  (should-not
   (assess-face-at= "def" 'python-mode "def" nil)))

;; https://github.com/phillord/assess/issues/5
(ert-deftest issue-5-test-example ()
  (should-not (assess-indentation= 'fundamental-mode "foo" "bar")))
;; #+end_src
