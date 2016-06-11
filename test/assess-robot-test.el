;;; assess-robot-test.el --- Test support functions -*- lexical-binding: t -*-

;;; Header:

;; This file is not part of Emacs

;; Author: Phillip Lord <phillip.lord@russet.org.uk>
;; Maintainer: Phillip Lord <phillip.lord@russet.org.uk>

;; The contents of this file are subject to the GPL License, Version 3.0.

;; Copyright (C) 2015, 2016, Phillip Lord

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

(require 'assess)
(require 'assess-robot)
(require 'ert)

(ert-deftest assess-robot-test-with-switched-buffer ()
  (should
   (with-temp-buffer
     (let ((c (current-buffer)))
       (assess-robot-with-switched-buffer
           (current-buffer))
       (buffer-live-p c))))
  (should-not
   (buffer-live-p
    (with-temp-buffer
      (assess-robot-with-switched-buffer
          (current-buffer)
        (current-buffer))))))

(ert-deftest assess-robot-test-with-temp-switched-buffer ()
  (should-not
   (let ((b4 (current-buffer)))
     (assess-robot-with-temp-switched-buffer
       (equal
        b4 (current-buffer)))))
  (should-not
   (buffer-live-p
    (assess-robot-with-temp-switched-buffer
      (current-buffer)))))

(ert-deftest assess-robot-test-with-switched-buffer-string ()
  (should
   (assess=
    "hello"
    (assess-robot-with-switched-buffer-string
     (insert "hello")))))

(ert-deftest assess-robot-test-execute-kmacro ()
  (should
   (assess=
    "hello"
    (assess-robot-with-switched-buffer-string
     (assess-robot-execute-kmacro
"
hello			;; self-insert-command * 5
")))))


(provide 'assess-robot-test)
