;;; sisyphus-test.el --- Tests for sisyphus.el -*- lexical-binding: t -*-

;;; Header:

;; The contents of this file are subject to the GPL License, Version 3.0.

;; Copyright (C) 2015, Phillip Lord, Newcastle University

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
(require 'sisyphus)
(require 'cl-lib)
;; #+end_src

;; ** Test Extraction

;; Sisyphus supports tests functions which means that we need the ability to test
;; tests. This code simple extracts knowledge from the results of tests.

;; #+begin_src emacs-lisp
(defun sisyphus-test--plist-from-result (result)
  (cl-cdadr
   (ert-test-result-with-condition-condition result)))

(ert-deftest plist-extraction ()
  (should
   (equal
    (sisyphus-test--plist-from-result
     (ert-run-test
      (make-ert-test
       :body
       (lambda ()
         (should
          (eq 1 2))))))
    '(:form (eq 1 2) :value nil))))

(defun sisyphus-test--explanation-from-result (result)
  (plist-get
   (sisyphus-test--plist-from-result result)
   :explanation))

(ert-deftest explanation-extraction-from-result ()
  "Test that explanation is extractable from failing test.
This also tests the advice on string=."
  (should
   (sisyphus-test--explanation-from-result
    (ert-run-test
     (make-ert-test
      :body
      (lambda ()
        (should
         (sisyphus= "1" "2"))))))))

(defun sisyphus-test--explanation (f)
  (sisyphus-test--explanation-from-result
   (ert-run-test
    (make-ert-test
     :body f))))

(ert-deftest explanation-extraction ()
  "Test that explanation is extractable from failing test.
This also tests the advice on string=."
  (should
   (sisyphus-test--explanation
    (lambda ()
      (should
       (sisyphus= "1" "2"))))))
;; #+end_src

;; ** To-String testing

;; #+begin_src emacs-lisp
(defvar sisyphus-test-hello.txt
  (sisyphus-file
   (relative-expand-file-name "../dev-resources/hello.txt")))

(ert-deftest to-string ()
  (should
   (equal "hello"
          (sisyphus-to-string "hello")))
  (should
   (with-temp-buffer
     (equal "hello"
            (progn
              (insert "hello")
              (sisyphus-to-string (current-buffer))))))
  (should
   (with-temp-buffer
     (equal "hello"
            (progn
              (insert "hello")
              (sisyphus-to-string
               (list
                :buffer
                (buffer-name (current-buffer))))))))
  (should
   (with-temp-buffer
     (equal "hello\n"
            (sisyphus-to-string
             sisyphus-test-hello.txt))))
  (should-error
   (sisyphus-to-string :hello)))

;; #+end_src

;; ** Compare Buffer to String

;; #+begin_src emacs-lisp

(ert-deftest buffer-string= ()
  (with-temp-buffer
    (insert "hello")
    (should
     (sisyphus=
      (current-buffer)
      "hello")))
  (with-temp-buffer
    (insert "goodbye")
    (should-not
     (sisyphus=
      (current-buffer)
      "hello")))
  (should
   (sisyphus-test--explanation
    (lambda ()
      (with-temp-buffer
        (insert "goodbye")
        (should
         (sisyphus=
          (current-buffer)
          "hello")))))))

;; #+end_src

;; ** Buffer to Buffer

;; #+begin_src emacs-lisp

(ert-deftest buffer= ()
  (sisyphus-with-temp-buffers
      ((a
        (insert "hello"))
       (b
        (insert "hello")))
    (should
     (sisyphus= a b)))
  (sisyphus-with-temp-buffers
      ((a
        (insert "hello"))
       (b
        (insert "goodbye")))
    (should-not
     (sisyphus=
      a b)))
  (should
   (sisyphus-with-temp-buffers
       ((a (insert "hello"))
        (b (insert "goodbye")))
     (sisyphus-test--explanation
      (lambda ()
        (should
         (sisyphus=
          a b)))))))

;; #+end_src

;; ** Buffer to file

;; #+begin_src emacs-lisp
(ert-deftest file-string= ()
  (should
   (sisyphus=
    sisyphus-test-hello.txt
    "hello\n"))
  (should-not
   (sisyphus=
    sisyphus-test-hello.txt
    "goodbye"))
  (should
   (sisyphus-test--explanation
    (lambda ()
      (should
       (sisyphus=
        sisyphus-test-hello.txt
        "goodbye"))))))


;; #+end_src

;; ** Preserved Buffer List and With Temp Buffers

;; #+begin_src emacs-lisp

(ert-deftest preserved-buffer-list ()
  (should
   (=
    (length (buffer-list))
    (progn
      (sisyphus-with-preserved-buffer-list
       (generate-new-buffer "preserved-buffer-list"))
      (length (buffer-list)))))

  (should
   (=
    (length (buffer-list))
    (condition-case e
        (sisyphus-with-preserved-buffer-list
         (generate-new-buffer "preserved-buffer-list")
         (signal 'sisyphus-deliberate-error nil))
      (sisyphus-deliberate-error
       (length (buffer-list)))))))

(ert-deftest with-temp-buffers ()
  (should
   (bufferp
    (sisyphus-with-temp-buffers (a) a)))
  (should
   (bufferp
    (sisyphus-with-temp-buffers
        (a (insert "hello"))
      a)))
  (should
   (equal
    "hello"
    (sisyphus-with-temp-buffers
        ((a (insert "hello")))
      (with-current-buffer
          a
        (buffer-string)))))
  (should
   (=
    (+ 2 (length (buffer-list)))
    (sisyphus-with-temp-buffers (a b)
      (length (buffer-list)))))
  (should
   (=
    (length (buffer-list))
    (progn
      (sisyphus-with-temp-buffers (a b))
      (length (buffer-list))))))

;; #+end_src

;; ** Open Close files

;; #+begin_src emacs-lisp

(ert-deftest sisyphus-test-related-file ()
  (should
   (file-exists-p
    (sisyphus-to-file-name
     (sisyphus-make-related-file sisyphus-test-hello.txt))))
  (should
   (sisyphus=
    sisyphus-test-hello.txt
    (sisyphus-make-related-file sisyphus-test-hello.txt))))

(ert-deftest sisyphus-test-with-find-file ()
  (should
   (sisyphus-with-find-file
       (sisyphus-make-related-file sisyphus-test-hello.txt)))
  (should-not
   (sisyphus=
    sisyphus-test-hello.txt
    (sisyphus-with-find-file
        (sisyphus-make-related-file sisyphus-test-hello.txt)
      (insert "hello")
      (buffer-string)))))

;; #+end_src

;; ** Indentation Tests

;; #+begin_src emacs-lisp

(ert-deftest sisyphus--test-indent-in-mode ()
  (should
   (sisyphus=
    "(
 (
  (
   (
    ))))"
    (sisyphus--indent-in-mode
     'emacs-lisp-mode
     "(\n(\n(\n(\n))))"))))

(ert-deftest sisyphus--test-indentation= ()
  (should
   (sisyphus-indentation=
    'emacs-lisp-mode
    "(\n(\n(\n(\n))))"
    "(
 (
  (
   (
    ))))"))
  (should-not
   (sisyphus-indentation=
    'emacs-lisp-mode
    "hello"
    "goodbye"))
  (should
   (sisyphus-test--explanation
    (lambda ()
      (should
       (sisyphus-indentation=
        'emacs-lisp-mode
        "hello"
        "goodbye"))))))

(defvar sisyphus-dev-resources
  (relative-expand-file-name "../dev-resources/"))

(defvar sisyphus-dev-elisp-indented
  (sisyphus-file
   (concat sisyphus-dev-resources
           "elisp-indented.el")))

(defvar sisyphus-dev-elisp-unindented
  (sisyphus-file
   (concat sisyphus-dev-resources
           "elisp-unindented.el")))

(ert-deftest sisyphus-test-roundtrip-indentation= ()
  (should
   (sisyphus-roundtrip-indentation=
    'emacs-lisp-mode
    sisyphus-dev-elisp-indented))
  (should-not
   (sisyphus-roundtrip-indentation=
    'emacs-lisp-mode
    sisyphus-dev-elisp-unindented)))

(ert-deftest sisyphus-test-roundtrip-indentation-explain= ()
  (should
   (sisyphus-test--explanation
    (lambda ()
      (should
       (sisyphus-roundtrip-indentation=
        'emacs-lisp-mode
        sisyphus-dev-elisp-unindented))))))

(ert-deftest sisyphus-test-file-roundtrip-indentation= ()
  (should
   (sisyphus-file-roundtrip-indentation=
    sisyphus-dev-elisp-indented))
  (should-not
   (sisyphus-file-roundtrip-indentation=
    sisyphus-dev-elisp-unindented)))

(ert-deftest sisyphus-test-file-roundtrip-indentation-explain= ()
  (should
   (sisyphus-test--explanation
    (lambda ()
      (should
       (sisyphus-file-roundtrip-indentation=
        sisyphus-dev-elisp-unindented))))))

;; ** Face Tests
(defvar sisyphus-dev-elisp-fontified
  (sisyphus-file
   (concat sisyphus-dev-resources
           "elisp-fontified.el")))

(ert-deftest sisyphus-test-face-at-simple ()
  (should
   (sisyphus-face-at=
    "(defun x ())"
    'emacs-lisp-mode
    2
    'font-lock-keyword-face))
  (should-not
   (sisyphus-face-at=
    "(not-defun x ())"
    'emacs-lisp-mode
    2
    'font-lock-keyword-face)))


(ert-deftest sisyphus-test-face-at-multiple-positions ()
  (should
   (sisyphus-face-at=
    "(defun x ())
(defun y ())
(defun z ())"
    'emacs-lisp-mode
    '(2 15 28)
    'font-lock-keyword-face))
  (should-not
   (sisyphus-face-at=
    "(defun x ())
(defun y ())
(not-defun z ())"
    'emacs-lisp-mode
    '(2 15 28)
    'font-lock-keyword-face)))

(ert-deftest sisyphus-test-face-at-multiple-faces ()
  (should
   (sisyphus-face-at=
    "(defun x ())"
    'emacs-lisp-mode
    '(2 8)
    '(font-lock-keyword-face font-lock-function-name-face)))
  (should-not
   (sisyphus-face-at=
    "(defun x ())"
    'emacs-lisp-mode
    '(2 10)
    '(font-lock-keyword-face font-lock-function-name-face))))

(ert-deftest sisyphus-test-face-at-with-m-buffer ()
  (should
   (sisyphus-face-at=
    "(defun x ())\n(defun y ())\n(defun z ())"
    'emacs-lisp-mode
    (lambda(buf)
      (m-buffer-match buf "defun"))
    'font-lock-keyword-face)))

(ert-deftest sisyphus-test-file-face-at ()
  (should
   (sisyphus-file-face-at=
    sisyphus-dev-elisp-fontified
    (lambda (buffer)
      (m-buffer-match buffer "defun"))
    'font-lock-keyword-face)))

;; #+end_src
