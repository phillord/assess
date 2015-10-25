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

(require 'sisyphus)

(defun sisyphus-test--plist-from-test (result)
  (cdadr
   (ert-test-result-with-condition-condition result)))

(ert-deftest plist-extraction ()
  (should
   (equal
    (sisyphus-test--plist-from-test
     (ert-run-test
      (make-ert-test
       :body
       (lambda ()
         (should
          (eq 1 2))))))
    '(:form (eq 1 2) :value nil))))

(defun sisyphus-test--explanation-from-test (result)
  (plist-get
   (sisyphus-test--plist-from-test result)
   :explanation))

(ert-deftest explanation-extraction ()
  "Test that explanation is extractable from failing test.
This also tests the advice on string=."
  (should
   (sisyphus-test--explanation-from-test
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
