;;; org-roam-folgezettel.el --- Folgezettel interface for Org-roam  -*- lexical-binding: t; -*-

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

;; Folgezettel interface for Org-roam.

;;; Code:
(require 'vtable)
(require 'org-roam-db)

;;; Variables
;;;; Options
(defgroup org-roam-folgezettel ()
  "Interfaces for org-roam nodes."
  :group 'files)

;;;; Internal

;;; Functions
(defun org-roam-folgezettel-list--objects ()
  "Get objects for vtable.
Returns a list of lists, one for every org-roam node.  Each list
contains the cached information for that node."
  (org-roam-db-query [ :select [nodes:id
                                nodes:file
                                nodes:level
                                nodes:pos
                                nodes:todo
                                nodes:priority
                                nodes:scheduled
                                nodes:deadline
                                nodes:title
                                nodes:properties
                                nodes:olp]
                       :from nodes]))

(defun org-roam-folgezettel-list--getter (object column vtable)
  "Getter for vtable objects.
OBJECT is an object of the type returned by
`org-roam-folgezettel-list--objects'.  COLUMN is the index of the column
the returned data is for.  VTABLE is the vtable this getter is for."
  (pcase (vtable-column vtable column)
    ("Index"
     (or (cdr (assoc "ROAM_PLACE" (nth 9 object) #'string-equal))
         ""))
    ("Title"
     (or (nth 8 object) "(No Title)"))
    ("Tags"
     (or (cdr (assoc "ALLTAGS" (nth 9 object) #'string-equal)) ""))))

;;; Major mode and keymap
(defvar-keymap org-roam-folgezettel-mode-map
  :doc "Mode map for `org-roam-folgezettel-mode'."
  "q" #'quit-window)

(define-derived-mode org-roam-folgezettel-mode fundamental-mode "ORF"
  "Major mode for listing org-roam nodes."
  :interactive nil
  :group 'org-roam-folgezettel)

;;; Commands
;;;###autoload
(defun org-roam-folgezettel-list ()
  "List org-roam nodes."
  (interactive)
  (let ((buf (get-buffer-create "*Node Listing*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-roam-folgezettel-mode)
        (make-vtable
         :columns '((:name "Index" :align right)
                    (:name "Title" :align left)
                    (:name "Tags" :align right))
         :objects-function #'org-roam-folgezettel-list--objects
         :getter #'org-roam-folgezettel-list--getter
         :separator-width 2))
      (setq-local buffer-read-only t))
    (display-buffer buf)))

;;; Provide
(provide 'org-roam-folgezettel)
;;; org-roam-folgezettel.el ends here
