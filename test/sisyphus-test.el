;; The contents of this file are subject to the GPL License, Version 3.0.
;;
;; Copyright (C) 2015, Phillip Lord, Newcastle University
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'load-relative)
(require 'sisyphus)
(require 'cl-lib)

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
         (string= "1" "2"))))))))

(ert-deftest sisyphus-test--string= ()
  "Test that string= works after explanation added."
  (should
   (string= "1" "1"))
  (should-not
   (string= "1" "2")))

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
       (string= "1" "2"))))))

(ert-deftest buffer-string= ()
  (with-temp-buffer
    (insert "hello")
    (should
     (sisyphus-buffer-string=
      (current-buffer)
      "hello")))
  (with-temp-buffer
    (insert "goodbye")
    (should-not
     (sisyphus-buffer-string=
      (current-buffer)
      "hello")))
  (should
   (sisyphus-test--explanation
    (lambda ()
      (with-temp-buffer
        (insert "goodbye")
        (should
         (sisyphus-buffer-string=
          (current-buffer)
          "hello")))))))

(ert-deftest buffer= ()
  (let (a)
    (with-temp-buffer
      (setq a (current-buffer))
      (insert "hello")
      (with-temp-buffer
        (insert "hello")
        (should
         (sisyphus-buffer=
          (current-buffer)
          a)))))
  (let (a)
    (with-temp-buffer
      (setq a (current-buffer))
      (insert "hello")
      (with-temp-buffer
        (insert "goodbye")
        (should-not
         (sisyphus-buffer=
          (current-buffer)
          a)))))
  (should
   (let (a b)
     (with-temp-buffer
       (setq a (current-buffer))
       (insert "hello")
       (with-temp-buffer
         (setq b (current-buffer))
         (insert "goodbye")
         (sisyphus-test--explanation
          (lambda ()
            (should
             (sisyphus-buffer=
              a b)))))))))

(defvar sisyphus-test-hello.txt
  (relative-expand-file-name "../dev-resources/hello.txt"))


(ert-deftest file-string= ()
  (should
   (sisyphus-file-string=
    sisyphus-test-hello.txt
    "hello\n"))
  (should-not
   (sisyphus-file-string=
    sisyphus-test-hello.txt
    "goodbye"))
  (should
   (sisyphus-test--explanation
    (lambda ()
      (should
       (sisyphus-file-string=
        sisyphus-test-hello.txt
        "goodbye"))))))


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
         (signal 'deliberate-error nil))
      (deliberate-error
       (length (buffer-list)))))))
