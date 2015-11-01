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
         (sisyphus= "1" "2"))))))))

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
       (sisyphus= "1" "2"))))))

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
