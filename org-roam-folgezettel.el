;;; org-roam-folgezettel.el --- Folgezettel interface for Org-roam  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; URL: https://github.com/krisbalintona/org-roam-folgezettel
;; Keywords: files, text, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

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
  :group 'files
  :prefix "org-roam-folgezettel-")

(defcustom org-roam-folgezettel-filter-function nil
  "A function that filters the current node listing.
This function takes one argument: the object described by
`org-roam-folgezettel-list--objects'.  It should return non-nil if that
node should be excluded from the listing, and nil otherwise."
  :local t
  :safe t
  :type 'function)

;;;; Faces

;;;; Internal
(defvar org-roam-folgezettel-filter-indicator ""
  "Mode line indicator for current filter.
Inspired by tablist.el's filter indicator.  Is added to
`mode-line-misc-info'.")

;;; Functions
;;;; Index numbering sorter
(defun org-roam-folgezettel--index-normalize (index)
  "Normalized INDEX into a signature whose parts are separated by \".\".

This means that every transition from number to letter or letter to
number warrants the insertion of a \".\" to delimit it.  This is a
useful operation for later index numbering operations for sorting; see
`org-roam-folgezettel--index-split' and
`org-roam-folgezettel--index-padded-parts'.

A special case is when INDEX is an empty string.  Since we want nodes
without indices to be positioned above nodes with an index, we assign
these nodes an index of \"0\", which sorts them above every other index.

This function is a modified version from Protesilaos Stavrou, found in
https://protesilaos.com/codelog/2024-08-01-emacs-denote-luhmann-signature-sort/."
  (replace-regexp-in-string
   (rx (group (+? alpha)) (group digit)) "\\1.\\2"
   (replace-regexp-in-string
    (rx (group (+? digit)) (group alpha)) "\\1.\\2"
    (or index "0"))))

(defun org-roam-folgezettel--index-split (index)
  "Split INDEX into Luhmann-style parts.
Returns a list of strings wherein each string is a part as described by
the docstring of `org-roam-folgezettel--signature-normalize'.

This is a useful operation for later index numbering operations for
sorting; see ``org-roam-folgezettel--index-padded-parts' and
`org-roam-folgezettel--index-lessp'."
  (string-split (org-roam-folgezettel--index-normalize index) "\\." t))

(defun org-roam-folgezettel--index-padded-parts (index)
  "Add padded spaces for all parts of INDEX.
For example, \"3.1b23d\" becomes \"    3.    1.    b.   23.    d\".
This is useful for operations such as
`org-roam-folgezettel--index-lessp' which compares strings.

This function is a modified version from Protesilaos Stavrou, found in
https://protesilaos.com/codelog/2024-08-01-emacs-denote-luhmann-signature-sort/."
  (combine-and-quote-strings
   (mapcar (lambda (x)
             (string-pad x 5 32 t))
           (org-roam-folgezettel--index-split index))
   "."))

(defun org-roam-folgezettel--index-lessp (index1 index2)
  "Compare two strings based on my index numbering sorting rules.
Returns t if INDEX1 should be sorted before INDEX2, nil otherwise.  Uses
`string<' to compare strings."
  (string< (org-roam-folgezettel--index-padded-parts index1)
           (org-roam-folgezettel--index-padded-parts index2)))

;;;; Vtable
;;;;; Retrieving values
(defun org-roam-folgezettel-list--retrieve-id (object)
  "Retrieve the id of OBJECT.
OBJECT is a list containing the information pertaining to a node.  See
`org-roam-folgezettel-list--objects' for the format of this list."
  (nth 0 object))

(defun org-roam-folgezettel-list--retrieve-file (object)
  "Retrieve the file path of OBJECT.
OBJECT is a list containing the information pertaining to a node.  See
`org-roam-folgezettel-list--objects' for the format of this list."
  (nth 1 object))

(defun org-roam-folgezettel-list--retrieve-pos (object)
  "Retrieve the node's position in OBJECT.
OBJECT is a list containing the information pertaining to a node.  See
`org-roam-folgezettel-list--objects' for the format of this list."
  (nth 3 object))

(defun org-roam-folgezettel-list--retrieve-index (object)
  "Retrieve the index string of OBJECT.
OBJECT is a list containing the information pertaining to a node.  See
`org-roam-folgezettel-list--objects' for the format of this list."
  (cdr (assoc "ROAM_PLACE" (nth 9 object) #'string-equal)))

(defun org-roam-folgezettel-list--retrieve-title (object)
  "Retrieve the title string of OBJECT.
OBJECT is a list containing the information pertaining to a node.  See
`org-roam-folgezettel-list--objects' for the format of this list."
  (nth 8 object))

(defun org-roam-folgezettel-list--retrieve-tags (object)
  "Retrieve the tags of OBJECT.
OBJECT is a list containing the information pertaining to a node.  See
`org-roam-folgezettel-list--objects' for the format of this list."
  (cdr (assoc "ALLTAGS" (nth 9 object) #'string-equal)))

;;;;; Formatters
(defun org-roam-folgezettel--index-formatter (index)
  "Propertize index INDEX.
Meant to be used as the formatter for index numberings."
  (let* ((parts (org-roam-folgezettel--index-split index))
         (level (1- (length parts)))
         (face
          (if (= 0 (+ 1 (% (1- level) 8)))
              'font-lock-warning-face
            (intern (format "outline-%s" (+ 1 (% (1- level) 8)))))))
    (replace-regexp-in-string "\\." (propertize "." 'face 'shadow)
                              (propertize index 'face face))))

(defun org-roam-folgezettel--title-formatter (title)
  "Propertize TITLE.
Meant to be used as the formatter for titles."
  (if (string= title "(No title)")
      (propertize title 'face 'shadow)
    title))

(defun org-roam-folgezettel--tags-formatter (tags)
  "Propertize a series of TAGS.
Meant to be used as the formatter for tags."
  (propertize tags 'face 'org-tag))

;;;;; Composition of vtable
(defun org-roam-folgezettel-list--objects ()
  "Get objects for vtable.
Returns a list of lists, one for every org-roam node.  Each list
contains the cached information for that node."
  (let* ((nodes (org-roam-db-query [ :select [id         ; 0
                                              file       ; 1
                                              level      ; 2
                                              pos        ; 3
                                              todo       ; 4
                                              priority   ; 5
                                              scheduled  ; 6
                                              deadline   ; 7
                                              title      ; 8
                                              properties ; 9
                                              olp]       ; 10
                                     :from nodes]))
         (nodes-filtered
          (cl-remove-if org-roam-folgezettel-filter-function nodes)))
    (cl-sort nodes-filtered #'org-roam-folgezettel--index-lessp
             :key #'org-roam-folgezettel-list--retrieve-index)))

(defun org-roam-folgezettel-list--getter (object column vtable)
  "Getter for vtable objects.
OBJECT is an object of the type returned by
`org-roam-folgezettel-list--objects'.  COLUMN is the index of the column
the returned data is for.  VTABLE is the vtable this getter is for."
  (pcase (vtable-column vtable column)
    ("Index"
     (or (org-roam-folgezettel-list--retrieve-index object) ""))
    ("Title"
     (or (org-roam-folgezettel-list--retrieve-title object) "(No Title)"))
    ("Tags"
     (or (org-roam-folgezettel-list--retrieve-tags object) ""))))

;;; Commands
;;;###autoload
(defun org-roam-folgezettel-list ()
  "List org-roam nodes."
  (interactive)
  (let ((buf (get-buffer-create "*Node Listing*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (unless (save-excursion (goto-char (point-min)) (vtable-current-table))
          (org-roam-folgezettel-mode)
          (make-vtable
           :columns '(( :name "Index"
                        :align left
                        :formatter org-roam-folgezettel--index-formatter)
                      ( :name "Title"
                        :align left
                        ;; TODO 2024-11-10: Figure out how to use a percentage.
                        ;; I think setting a percentage only works if the table
                        ;; is called in a visible buffer.  Additionally, for
                        ;; better comparability with `display-buffer-alist',
                        ;; ideally the major mode must also be set before
                        ;; creating the table.
                        :width 130
                        :formatter org-roam-folgezettel--title-formatter)
                      ( :name "Tags"
                        :align right
                        :formatter org-roam-folgezettel--tags-formatter))
           :objects-function #'org-roam-folgezettel-list--objects
           :getter #'org-roam-folgezettel-list--getter
           :separator-width 2)
          (setq-local buffer-read-only t)
          (toggle-truncate-lines 1))))
    (display-buffer buf)))

(defun org-roam-folgezettel-open-node (object)
  "Open the node at point.
Opens the node associated with OBJECT."
  (interactive (list (vtable-current-object)) org-roam-folgezettel-mode)
  (let ((file (org-roam-folgezettel-list--retrieve-file object))
        (location (org-roam-folgezettel-list--retrieve-pos object)))
    (find-file file)
    (goto-char location)))

(defun org-roam-folgezettel-edit-index (object)
  "Edit the index of the node at point.
Prompts for a new index for the node associated with OBJECT."
  (interactive (list (vtable-current-object)) org-roam-folgezettel-mode)
  (let* ((file (org-roam-folgezettel-list--retrieve-file object))
         (current-index (org-roam-folgezettel-list--retrieve-index object))
         (node-point (org-roam-folgezettel-list--retrieve-pos object))
         (save-silently t)
         (get-index-func
          (lambda (node) (cdr (assoc "ROAM_PLACE" (org-roam-node-properties node) #'string-equal))))
         (all-index-numbers          ; All index numbers in current subdirectory
          (cl-loop for node in (org-roam-node-list)
                   when (and (file-in-directory-p (org-roam-node-file node) (file-name-directory file))
                             (not (or (equal nil (funcall get-index-func node))
                                      (string-empty-p (funcall get-index-func node)))))
                   collect (funcall get-index-func node)))
         (prompt "New index numbering: ")
         (retry-p t)
         new-index)
    (while retry-p
      (setq new-index (read-string prompt current-index))
      (if (member new-index all-index-numbers)
          (progn
            (setq retry-p t
                  prompt (format "Index number %s taken! Please choose another index numbering: " new-index)))
        (setq retry-p nil)
        (message "Setting index to %s" new-index)))
    (unless (string= current-index new-index)
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char node-point)
          (org-roam-node-at-point 'assert)
          (org-set-property "ROAM_PLACE" new-index))
        (save-buffer)
        (org-roam-db-update-file file))
      (vtable-update-object (vtable-current-table) object))))

(defun org-roam-folgezettel-filter-directory (&optional subdir)
  "Prompts for a directory to filter the current buffer's node listing.
If SUBDIR is provided, then this subdirectory (of the
`org-roam-directory') will be filtered."
  (interactive (list nil) org-roam-folgezettel-mode)
  (let ((subdir (if subdir
                    (string-trim subdir "/" "/")
                  (completing-read "Subdirectory: "
                                   (mapcar (lambda (dir) (file-relative-name dir org-roam-directory))
                                           (seq-filter #'file-directory-p
                                                       (directory-files org-roam-directory t "^[^.]" t)))))))
    (setq-local org-roam-folgezettel-filter-function
                `(lambda (object)
                   ,(format "Filter nodes to ones only in the %s subdirectory." subdir)
                   (not (string-prefix-p ,(expand-file-name subdir org-roam-directory)
                                         (expand-file-name
                                          (org-roam-folgezettel-list--retrieve-file object)))))
                org-roam-folgezettel-filter-indicator (format "%s" subdir))
    (message "Filtered nodes to the %s subdirectory" subdir)
    (org-roam-folgezettel-refresh)))

(defun org-roam-folgezettel-refresh ()
  "Refresh the current `org-roam-folgezettel-mode' buffer."
  (interactive)
  (let ((object (vtable-current-object)))
    (widen)
    (vtable-revert-command)
    (vtable-goto-object object)))

(defun org-roam-folgezettel-store-link (object)
  "Call `org-store-link' on the node at point.
OBJECT contains information about a node.  See
`org-roam-folgezettel-list--objects' for the format OBJECT comes in."
  (interactive (list (vtable-current-object)) org-roam-folgezettel-mode)
  (let* ((node-id (org-roam-folgezettel-list--retrieve-id object))
         (description (org-roam-folgezettel-list--retrieve-title object)))
    ;; Populate `org-store-link-plist'
    (org-link-store-props
     :type "id"
     :description description
     :link (concat "id:" node-id))
    ;; Then add to `org-stored-links'
    (push (list (plist-get org-store-link-plist :link)
                (plist-get org-store-link-plist :description))
          org-stored-links)
    (message "Stored %s!" title)))

(defun org-roam-folgezettel-move-down (nlines)
  "Move the current line down NLINES."
  (interactive "p" org-roam-folgezettel-mode)
  (org-roam-folgezettel-move-up (- nlines)))

(defun org-roam-folgezettel-move-up (nlines)
  "Move the current line up NLINES."
  (interactive "p" org-roam-folgezettel-mode)
  (let ((table (vtable-current-table))
        (object (vtable-current-object))
        (object-location (- (line-number-at-pos)
                            (save-excursion
                              (vtable-beginning-of-table)
                              (line-number-at-pos)))))
    (vtable-remove-object table object)
    (vtable-insert-object table object (+ object-location (- nlines)))
    (vtable-goto-object object)))

;;; Major mode and keymap
(defvar-keymap org-roam-folgezettel-mode-map
  :doc "Mode map for `org-roam-folgezettel-mode'."
  "p" #'previous-line
  "n" #'next-line
  "g" #'org-roam-folgezettel-refresh
  "q" #'quit-window
  "RET" #'org-roam-folgezettel-open-node
  "i" #'org-roam-folgezettel-edit-index
  "d" #'org-roam-folgezettel-filter-directory
  "M-<up>" #'org-roam-folgezettel-move-up
  "M-<down>" #'org-roam-folgezettel-move-down
  "w" #'org-roam-folgezettel-store-link)

(define-derived-mode org-roam-folgezettel-mode fundamental-mode "ORF"
  "Major mode for listing org-roam nodes."
  :interactive nil
  :group 'org-roam-folgezettel
  :after-hook (set (make-local-variable 'mode-line-misc-info)
                   (append
                    (list
                     (list 'org-roam-folgezettel-filter-function
                           '(:eval (format " [%s]" org-roam-folgezettel-filter-indicator)))))))

;;; Provide
(provide 'org-roam-folgezettel)
;;; org-roam-folgezettel.el ends here
