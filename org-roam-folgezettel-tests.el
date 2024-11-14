;;; org-roam-folgezettel-tests.el --- Test suite for org-roam-folgezettel  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords: files, text, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for org-roam-folgezettel.

;;; Code:
(require 'ert)
(require 'org-roam-folgezettel)

(defmacro org-roam-folgezettel-deftest (name doc &rest body)
  "Generate an ert test for `org-roam-folgezettel'.
NAME is the label of the test.  DOC is its docsting.  BODY contains the
tests that should be run."
  (let ((xname (intern (concat "org-roam-folgezettel-defun-test-"
                               (symbol-name name)))))
    `(ert-deftest ,xname ()
       ,doc
       ,@body)))

;;; Index numbering sorting
(org-roam-folgezettel-deftest
 internal-sorting-functions
 "Tests internal functions for folgezettel sorting of index numberings."
 (let ((i1 "1.3a")
       (i2 "1.1a1")
       (i3 "")
       (i4 "1234"))
   ;; Basic
   (should (equal (org-roam-folgezettel--index-normalize i1)
                  "1.3.a"))
   (should (equal (org-roam-folgezettel--index-normalize i2)
                  "1.1.a.1"))
   (should (equal (org-roam-folgezettel--index-split i1)
                  '("1" "3" "a")))
   (should (equal (org-roam-folgezettel--index-split i2)
                  '("1" "1" "a" "1")))
   (should (equal (org-roam-folgezettel--index-padded-parts i1)
                  "    1.    3.    a"))
   (should (equal (org-roam-folgezettel--index-padded-parts i2)
                  "    1.    1.    a.    1"))

   ;; With "/" as a section delimiter
   (should (equal (org-roam-folgezettel--index-normalize "1/1/a/1")
                  "1.1.a.1"))
   (should (equal (org-roam-folgezettel--index-normalize "1/a/a/1")
                  "1.a.a.1"))

   ;; Negative numbers
   (should (equal (org-roam-folgezettel--index-normalize "1.-1ab-30")
                  "1.-1.ab.-30"))
   (should (equal (org-roam-folgezettel--index-split "1.-1ab-30")
                  '("1" "-1" "ab" "-30")))

   ;; Empty
   (should (equal (org-roam-folgezettel--index-padded-parts i3)
                  ""))

   ;; Single section
   (should (equal (org-roam-folgezettel--index-padded-parts i4)
                  " 1234"))))

(org-roam-folgezettel-deftest
 sorting
 "Tests folgezettel sorting of index numberings."
 (let ((i1 "1.3a")
       (i2 "1.1a1"))
   ;; Basic
   (should (equal (org-roam-folgezettel--index-lessp i1 i2)
                  nil))
   ;; Equal
   (should (equal (org-roam-folgezettel--index-lessp i1 i2)
                  nil))
   ;; One is empty
   (should (equal (org-roam-folgezettel--index-lessp "" i1)
                  t))              ; We want non-indexed nodes sorted to the top

   ;; Both are empty
   (should (equal (org-roam-folgezettel--index-lessp "" "")
                  nil))        ; nil means the order is undisturbed when sorting

   ;; 10 vs 1
   (should (equal (org-roam-folgezettel--index-lessp "1.10a" "1.1a")
                  nil))

   ;; One is a subset of the other
   (should (equal (org-roam-folgezettel--index-lessp "2.10a" "2.10a30")
                  t))

   ;; Long first section
   (should (equal (org-roam-folgezettel--index-lessp "1234" "2.10a30")
                  nil))

   ;; Long vs long
   (should (equal (org-roam-folgezettel--index-lessp "16.23a54z34" "16.23a54z35")
                  t))

   ;; Zero numbering
   (should (equal (org-roam-folgezettel--index-lessp "1.0" "1.1")
                  t))
   (should (equal (org-roam-folgezettel--index-lessp "1.1" "1.0")
                  nil))

   ;; Negative numbering
   (should (equal (org-roam-folgezettel--index-lessp "1.1" "1.-1")
                  nil))
   (should (equal (org-roam-folgezettel--index-lessp "1.-1" "1.1")
                  t))
   (should (equal (org-roam-folgezettel--index-lessp "1.-1" "1.-2")
                  nil))
   (should (equal (org-roam-folgezettel--index-lessp "1.-10" "1.-2")
                  t))))

;;; Provide
(provide 'org-roam-folgezettel-tests)
;;; org-roam-folgezettel-tests.el ends here
