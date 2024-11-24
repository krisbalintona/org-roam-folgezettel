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
(require 'org-roam-node)
(require 'org-roam-ql)
(require 'seq)

;;; Variables
;;;; Options
(defgroup org-roam-folgezettel ()
  "Interfaces for org-roam nodes."
  :group 'files
  :prefix "org-roam-folgezettel-")

(defcustom org-roam-folgezettel-filter-query nil
  "A symbol representing a filter for the buffer.
This symbol is of the form of SOURCE-OR-QUERY that org-roam-ql commands
like `org-roam-ql-search'accepts.

This is a buffer-local variable.  Users can set the default value of
this variable to create a \"default\" filter; that is, a filter for
every fresh `org-roam-folgezettel-mode' buffer."
  :local t
  :safe t
  :type 'symbol)

(defcustom org-roam-folgezettel-index-color-style 'color-last
  "The coloring style for index numbering."
  :type '(choice
          (const :tag "Color only the last index numbering section" color-last)
          (const :tag "Color the entire index number" color-full)))

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
This means that every transition from integer (positive or negative) to
letter or letter to integer (positive or negative) warrants the
insertion of a \".\" to delimit it.  Additionally, every \"/\" is
treated as a delimiter by replacing it with a \".\".

This is a useful operation for later index numbering operations for
sorting; see `org-roam-folgezettel--index-split' and
`org-roam-folgezettel--index-padded-parts'.

A special case is when INDEX is an empty string.  Since we want nodes
without indices to be positioned above nodes with an index, we assign
these nodes an index of \"0\", which sorts them above every other index.

This function is a modified version from Protesilaos Stavrou, found in
https://protesilaos.com/codelog/2024-08-01-emacs-denote-luhmann-signature-sort/."
  (replace-regexp-in-string
   (rx (group (+? alpha)) (group (or digit "-"))) "\\1.\\2"
   (replace-regexp-in-string
    (rx (group (+? digit)) (group alpha)) "\\1.\\2"
    (replace-regexp-in-string "/" "." (or index "0")))))

(defun org-roam-folgezettel--index-split (index)
  "Split INDEX into Luhmann-style parts.
Returns a list of strings wherein each string is a part as described by
the docstring of `org-roam-folgezettel--signature-normalize'.

This is a useful operation for later index numbering operations for
sorting; see ``org-roam-folgezettel--index-padded-parts' and
`org-roam-folgezettel--index-lessp'."
  ;; We split by "." because each portion of the index numbering is separeted by
  ;; a "." from `org-roam-folgezettel--index-normalize'
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
  "Compare INDEX1 and INDEX2 based on Luhmann-style numbering.
Return t if INDEX1 should be sorted before INDEX2, nil otherwise.
Handles mixed numeric and alphabetic components in index parts."
  (let ((parts1 (org-roam-folgezettel--index-split index1))
        (parts2 (org-roam-folgezettel--index-split index2)))
    (catch 'done
      (while (or parts1 parts2)
        (let ((part1 (pop parts1))
              (part2 (pop parts2)))
          (cond
           ;; If only one part is empty, it should be considered "less"
           ((and (null part1) part2) (throw 'done t))
           ((and (null part2) part1) (throw 'done nil))
           ;; Compare numeric parts as numbers
           ((and (string-match-p "^-?[0-9]+$" part1)
                 (string-match-p "^-?[0-9]+$" part2))
            (let ((num1 (string-to-number part1))
                  (num2 (string-to-number part2)))
              (when (/= num1 num2)
                (throw 'done (< num1 num2)))))
           ;; Otherwise, compare alphabetic or mixed parts as strings
           (t (when (not (string= part1 part2))
                (throw 'done (string< part1 part2)))))))
      ;; If we loop through all parts without difference, they are equal
      nil)))

(defun org-roam-folgezettel--node-index-lessp (node1 node2)
  "Compare the indices of NODE1 and NODE2.
See the docstring of `org-roam-folgezettel--index-lessp'."
  (let ((index1 (org-roam-folgezettel-list--retrieve-index node1))
        (index2 (org-roam-folgezettel-list--retrieve-index node2)))
    (org-roam-folgezettel--index-lessp index1 index2)))

;;;; Vtable
;;;;; Retrieving values
(defun org-roam-folgezettel-list--retrieve-index (node)
  "Retrieve the index number of NODE.
Returns the trimmed value of the \"ROAM_PLACE\" property.
Additionally,if the index string is empty, return nil."
  (let ((values (assoc "ROAM_PLACE" (org-roam-node-properties node) #'string-equal)))
    (when (and (cdr values) (not (string-empty-p (cdr values))))
      (cdr values))))

(defun org-roam-folgezettel-list--retrieve-person (node)
  "Retrieve the person of NODE.
Returns the trimmed value of the \"ROAM_PERSON\" property.
Additionally,if the index string is empty, return nil."
  (let ((values (assoc "ROAM_PERSON" (org-roam-node-properties node) #'string-equal)))
    (when (and (cdr values) (not (string-empty-p (cdr values))))
      (cdr values))))

;;;;; Formatters
(defun org-roam-folgezettel--index-formatter (index)
  "Propertize index INDEX.
Meant to be used as the formatter for index numberings."
  (let* ((parts (org-roam-folgezettel--index-split index))
         (level (1- (length parts)))
         (outline-color
          (when parts
            (face-foreground
             (if (= 0 (+ 1 (% (1- level) 8)))
                 'font-lock-warning-face
               (intern (format "outline-%s" (+ 1 (% (1- level) 8)))))
             nil t))))
    (pcase org-roam-folgezettel-index-color-style
      ('color-last
       (if parts
           (concat (propertize (string-remove-suffix (car (last parts)) index) 'face 'shadow)
                   (propertize (car (last parts)) 'face `(:weight bold :foreground ,outline-color)))
         ""))
      ('color-full
       (replace-regexp-in-string "\\." (propertize "." 'face 'shadow)
                                 (propertize index 'face `(:foreground ,outline-color)))))))

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

;;;;; Org-roam-ql integration
;;;;;; Sorting functions
(org-roam-ql-register-sort-fn "index" #'org-roam-folgezettel--node-index-lessp)

;;;;;; Node predicates
(org-roam-ql-defpred 'subdir
  "A predicate for the directory of a node.
Returns non-nil when a node is within (at any level) the subdirectory."
  (lambda (node) (expand-file-name (org-roam-node-file node)))
  (lambda (path subdir) (string-prefix-p (expand-file-name subdir org-roam-directory) path)))

(org-roam-ql-defpred 'box
  "A predicate for the slip-box of a node.
Returns non-nil when a node's \"ROAM_BOX\" property matches the provided
argument (a string)."
  (lambda (node)
    (let ((values (assoc "ROAM_BOX" (org-roam-node-properties node) #'string-equal)))
      (when (and (cdr values) (not (string-empty-p (cdr values))))
        (cdr values))))
  (lambda (box-value box-query)
    (when (and box-value (not (string-empty-p box-value)))
      (string-equal box-value
                    (string-trim box-query)))))

(org-roam-ql-defpred 'person
  "A predicate for the person associated with node.
Returns non-nil when a node's \"ROAM_PERSON\" property matches the
provided argument (a string)."
  #'org-roam-folgezettel-list--retrieve-person
  (lambda (person-value person-query)
    (unless (stringp person-query) (error "Person argument should be a string!"))
    (when (and person-value (not (string-empty-p person-value)))
      (string-equal person-value
                    (string-trim person-query)))))

(org-roam-ql-defpred 'siblings
  "A predicate for the siblings of an index numbering.
A sibling of a node is one that, speaking from in folgezettel terms, is
a child of the same parent of that node."
  #'org-roam-folgezettel-list--retrieve-index
  (lambda (index-value index-query)
    (unless (stringp index-query) (error "Index argument should be a string!"))
    (let* ((index-value-parts
            (org-roam-folgezettel--index-split index-value))
           (index-query-parts
            (org-roam-folgezettel--index-split index-query)))
      (equal (butlast index-value-parts) (butlast index-query-parts)))))

(org-roam-ql-defpred 'children
  "A predicate for the children of an index numbering.
A child of a node is one that, speaking from in folgezettel terms, is
one nesting level below that node."
  #'org-roam-folgezettel-list--retrieve-index
  (lambda (index-value index-query)
    (unless (stringp index-query) (error "Index argument should be a string!"))
    (let* ((index-value-parts
            (org-roam-folgezettel--index-split index-value))
           (index-query-parts
            (org-roam-folgezettel--index-split index-query)))
      (and (string-prefix-p index-query index-value)
           (= (1+ (length index-value-parts)) (length index-query-parts))))))

(org-roam-ql-defpred 'descendants
  "A predicate for the descendants of an index numbering.
A descendant of a node is one that, speaking from in folgezettel terms,
is a child of that node or a child of one of the children of that node.

The returned nodes list does not include the node from which all other
returned nodes are descended from."
  #'org-roam-folgezettel-list--retrieve-index
  (lambda (index-value index-query)
    (unless (stringp index-query) (error "Index argument should be a string!"))
    (and (string-prefix-p index-query index-value)
         (not (string-equal index-query index-value)))))

;; TODO 2024-11-19: Finish implementation
(org-roam-ql-defpred 'first-children
  "A predicate for the first children of an index numbering.
The first child of a node, speaking in folgezettel terms, is the
immediate descendant of the node whose index numbering is the
smallest (lexicographically) among its siblings.  For example, if a node
has children indexed as 1.1, 1.2, and 1.3, the first child is 1.1."
  #'org-roam-folgezettel-list--retrieve-index
  (lambda (index-value index-query)
    (unless (stringp index-query) (error "Index argument should be a string!"))
    (when (and (string-prefix-p index-query index-value)
               (not (string-equal index-query index-value)))
      (let* ((suffix (string-remove-prefix index-query index-value))
             (suffix-parts (org-roam-folgezettel--index-split suffix)))
        ;; TODO 2024-11-23: We assume that the first child is either an index
        ;; numbering that ends with a 1 or a.  This is incompatible with index
        ;; numbering systems which use a different value (e.g. negative
        ;; numbers).
        (not (cl-member-if-not
              (lambda (part) (or (string= part "1") (string= part "a")))
              suffix-parts))))))

;;;;; Composition of vtable
(defun org-roam-folgezettel-list--objects ()
  "Get objects for vtable.
Returns a list of lists, one for every org-roam node.  Each list
contains the cached information for that node."
  (let ((org-roam-folgezettel-filter-query
         (or org-roam-folgezettel-filter-query (org-roam-node-list))))
    (org-roam-ql-nodes org-roam-folgezettel-filter-query "index")))

(defun org-roam-folgezettel-list--getter (node column vtable)
  "Getter for vtable objects.
NODE is an org-roam node.  COLUMN is the index of the column the
returned data is for.  VTABLE is the vtable this getter is for."
  (pcase (vtable-column vtable column)
    ("Index"
     (or (org-roam-folgezettel-list--retrieve-index node) ""))
    ("Title"
     (or (org-roam-node-title node) "(No Title)"))
    ("Tags"
     (or (propertize
          (mapconcat (lambda (s) (concat "#" s)) (org-roam-node-tags node))
          'face 'org-tag)
         ""))))

;;; Commands
;;;###autoload
(defun org-roam-folgezettel-list (&optional arg buf-name)
  "List org-roam nodes with vtable.el.
If BUF-NAME is provided, that will be the name of the buffer created.
BUF-NAME defaults to \"*Node Listing*\".

If a buffer with such a name exists already, open that buffer instead.

If ARG is supplied (prefix-argument when called interactively), then
create a new buffer whose name is unique (using
`generate-new-buffer-name')."
  (interactive (list current-prefix-arg nil))
  (let ((default-buf-name "*Node Listing*"))
    (setq buf-name
          (cond
           (buf-name default-buf-name)
           ((and arg (not buf-name))
            (generate-new-buffer-name default-buf-name))
           ((not buf-name) default-buf-name))))
  (let ((buf (get-buffer-create buf-name)))
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
          (setq-local buffer-read-only t
                      truncate-lines t
                      org-roam-folgezettel-filter-indicator
                      (lambda () (prin1-to-string org-roam-folgezettel-filter-query))))))
    (display-buffer buf)
    buf))

;;;; Showing nodes
(defun org-roam-folgezettel-open-node (node &optional display-action no-select)
  "Open the NODE at point.
If DISPLAY-ACTION is supplied, then use that function as the ACTION
argument for the `display-buffer' function.

If NO-SELECT is supplied, then don't select the buffer."
  (interactive (list (vtable-current-object) nil) org-roam-folgezettel-mode)
  (let* ((file (org-roam-node-file node))
         (location (org-roam-node-point node))
         (buf (find-file-noselect file))
         (display-buffer-overriding-action
          (list (or display-action 'display-buffer-same-window)))
         (window (display-buffer buf)))
    ;; FIXME 2024-11-14: Point is not moved when NO-SELECT is non-nil
    (with-current-buffer buf
      (goto-char location))
    (unless no-select
      (select-window window))))

(defun org-roam-folgezettel-open-node-other-window (node)
  "Show NODE in a new window, selected the buffer.
If called interactively, NODE is the node corresponding to the vtable
object at point."
  (interactive (list (vtable-current-object)) org-roam-folgezettel-mode)
  (org-roam-folgezettel-open-node node 'display-buffer-pop-up-window))

(defun org-roam-folgezettel-display-node (node)
  "Show NODE in a new window without selected the buffer.
If called interactively, NODE is the node corresponding to the vtable
object at point."
  (interactive (list (vtable-current-object)) org-roam-folgezettel-mode)
  (org-roam-folgezettel-open-node node 'display-buffer-pop-up-window :no-select))

;;;; Editing
(defun org-roam-folgezettel-edit-index (node)
  "Edit the index of NODE.
Prompts for a new index for NODE.  If called interactively, NODE is the
node at point."
  (interactive (list (vtable-current-object)) org-roam-folgezettel-mode)
  (let* ((file (org-roam-node-file node))
         (current-index (org-roam-folgezettel-list--retrieve-index node))
         (node-point (org-roam-node-point node))
         (save-silently t)
         (all-index-numbers          ; All index numbers in current subdirectory
          (cl-loop for node in (org-roam-node-list)
                   when (and (file-in-directory-p (org-roam-node-file node) (file-name-directory file))
                             (not (or (equal nil (org-roam-folgezettel-list--retrieve-index node))
                                      (string-empty-p (org-roam-folgezettel-list--retrieve-index node)))))
                   collect (org-roam-folgezettel-list--retrieve-index node)))
         (prompt "New index numbering: ")
         (retry-p t)
         new-index)
    (while retry-p
      (setq new-index (read-string prompt current-index))
      (if (member new-index all-index-numbers)
          (progn
            (setq retry-p t
                  prompt (format "Index number %s taken! Please choose another index numbering: " new-index)))
        (setq retry-p nil)))
    (unless (string= current-index new-index)
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char node-point)
          (org-roam-node-at-point 'assert)
          (org-set-property "ROAM_PLACE" new-index))
        (save-buffer)
        (org-roam-db-update-file file)
        (message "Set index of %s to %s" (org-roam-node-title node) new-index))
      (vtable-update-object (vtable-current-table) node))))

;;;; Filtering
(defun org-roam-folgezettel-filter-query-modify (new-query)
  "Manually modify the filter for the current `org-roam-folgezettel' buffer.
In lisp, if NEW-QUERY is provided, use that string as the new query."
  (interactive)
  (let* ((current-query-string (and org-roam-folgezettel-filter-query (prin1-to-string org-roam-folgezettel-filter-query)))
         (new-query-string (or new-query (read-string "New filter query: " current-query-string))))
    (setq-local org-roam-folgezettel-filter-query (unless (string-empty-p new-query-string) (read new-query-string))))
  (org-roam-folgezettel-refresh))

(defun org-roam-folgezettel-filter-directory (subdir)
  "Filter the current buffer's node listing to SUBDIR.
SUBDIR is a subdirectory of the `org-roam-directory'.

If called interactively, SUBDIR is prompted for."
  (interactive (list (completing-read "Subdirectory: "
                                      (mapcar (lambda (dir) (file-relative-name dir org-roam-directory))
                                              (seq-filter #'file-directory-p
                                                          (directory-files org-roam-directory t "^[^.]" t)))))
               org-roam-folgezettel-mode)
  (let ((subdir (string-trim subdir "/" "/")))
    (setq-local org-roam-folgezettel-filter-query
                (if org-roam-folgezettel-filter-query
                    `(and ,org-roam-folgezettel-filter-query
                          (subdir ,subdir))
                  `(subdir ,subdir)))
    (message "Filtered nodes to the %s subdirectory" subdir)
    (org-roam-folgezettel-refresh)))

(defun org-roam-folgezettel-filter-person (person)
  "Filter the current node listing by PERSON.
PERSON is the value of the \"ROAM_PERSON\" property.

If called interactively, prompts for a person to filter by."
  (interactive (list (read-string "Filter by the following person: ")) org-roam-folgezettel-mode)
  (setq-local org-roam-folgezettel-filter-query
              (if org-roam-folgezettel-filter-query
                  `(and ,org-roam-folgezettel-filter-query
                        (person ,person))
                `(person ,person)))
  (message "Filtered nodes by the %s person" person)
  (org-roam-folgezettel-refresh))

(defun org-roam-folgezettel-filter-tags (tags)
  "Filter the current node listing by TAGS.
If called interactively, prompts for the tags to filter by."
  (interactive (list (let ((crm-separator "[    ]*:[    ]*"))
                       (mapconcat #'identity (completing-read-multiple "Tag(s): " (org-roam-tag-completions)))))
               org-roam-folgezettel-mode)
  (setq-local org-roam-folgezettel-filter-query
              (if org-roam-folgezettel-filter-query
                  `(and ,org-roam-folgezettel-filter-query
                        (tags ,tags))
                `(tags ,tags)))
  (message "Filtered nodes by the tags: %s" tags)
  (org-roam-folgezettel-refresh))

;;;; Movement via index numbers
(defun org-roam-folgezettel-upward (&optional dist)
  "Move point to DIST parents upward in the vtable.
DIST is an integer representing the number of parents to move upwards
by.  If DIST is negative, move downward."
  (interactive "p")
  (setq dist (or dist 1))
  (if (<= 0 dist)
      (let* ((node (vtable-current-object))
             (index (org-roam-folgezettel-list--retrieve-index node))
             (index-parts (org-roam-folgezettel--index-split index))
             (parent-index
              ;; TODO 2024-11-17: For now, we assume the only delimiter is a period
              ;; (".") and manually remove it.  Is there a more versatile,
              ;; assumption-less way to accomplish this?
              (string-remove-suffix "." (string-remove-suffix
                                         (mapconcat #'identity (last index-parts (min (1- (length index-parts)) dist)))
                                         index)))
             (parent-node (cl-find parent-index
                                   (vtable-objects (vtable-current-table))
                                   :key #'org-roam-folgezettel-list--retrieve-index
                                   :test #'string-equal))
             (orig-pt (point)))
        (if (vtable-goto-object parent-node)
            (push-mark orig-pt t)
          (message "No parent visible with index numbering %s" parent-index))
        (when (< (1- (length index-parts)) dist)
          (message "Cannot go that high; going to top-level parent")))
    (org-roam-folgezettel-downward dist)))

(defun org-roam-folgezettel-downward (&optional dist)
  "Move point to DIST parents downward in the vtable.
DIST is an integer representing the number of parents to move downwards
by.  If DIST is negative, move upward."
  (interactive "p")
  (setq dist (or dist 1))
  (if (<= 0 dist)
      (let* ((node (vtable-current-object))
             (index (org-roam-folgezettel-list--retrieve-index node))
             ;; We want to traverse children that are present in the current
             ;; vtable
             (children (org-roam-ql-nodes `(and (nodes-list ,(vtable-objects (vtable-current-table)))
                                                (first-children ,index))
                                          "index"))
             (orig-pt (point)))
        (if (vtable-goto-object (nth (1- dist) children))
            (push-mark orig-pt t)
          (message "There is no first child %s levels down" dist)))
    (org-roam-folgezettel-upward dist)))

(defun org-roam-folgezettel-forward-sibling (&optional dist)
  "Move point to DIST visible siblings forward or backward in the vtable.
DIST is an integer representing the number of siblings to move across.
If DIST is negative, move backward."
  (interactive "p")
  (let* ((node (vtable-current-object))
         (index (org-roam-folgezettel-list--retrieve-index node))
         ;; We want to traverse siblings that are present in the current vtable
         (siblings (org-roam-ql-nodes `(and (nodes-list ,(vtable-objects (vtable-current-table)))
                                            (siblings ,index))
                                      "index"))
         ;; Find the current node in the sorted list
         (current-pos (cl-position node siblings :test #'eq))
         ;; Calculate the target position
         (target-pos (+ current-pos (or dist 1)))
         (orig-pt (point)))
    (if (or (< target-pos 0) (>= target-pos (length siblings)))
        (message "No sibling at target position")
      (vtable-goto-object (nth target-pos siblings))
      (push-mark orig-pt t))))

(defun org-roam-folgezettel-backward-sibling (&optional dist)
  "Move point to DIST siblings backward from the vtable object at point.
DIST is an integer representing the number of siblings to move across.
If DIST is negative, move forward."
  (interactive "p" org-roam-folgezettel-mode)
  (org-roam-folgezettel-forward-sibling (- (or dist 1))))

;;;; Movement via `completing-read'
(defun org-roam-folgezettel-goto-node ()
  "Go to node using `org-roam-node-find'-like interface.
Use `org-roam-node-read' (which `org-roam-node-find' uses) to prompt for
a node in the current `org-roam-folgezettel-mode' buffer, then go to it.

The benefit of using this command over isearch of consult.el's
`consult-line' is that org-roam's display template (see
`org-roam-node-display-template') is leveraged, letting users see more
or different information (possibly in bespoke formatting) than the
columns in the `org-roam-folgezettel-mode' table."
  (interactive nil org-roam-folgezettel-mode)
  (vtable-goto-object (org-roam-node-read
                       nil
                       (lambda (node) (member node (vtable-objects (vtable-current-table))))
                       nil t "Go to node: ")))

;;;; Moving nodes
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

;;;; Other
;; FIXME 2024-11-12: This command is being overshadowed by the vtable local map.
(defun org-roam-folgezettel-refresh ()
  "Refresh the current `org-roam-folgezettel-mode' buffer."
  (interactive)
  (widen)
  (save-excursion (goto-char (point-min)))
  (vtable-revert-command))

(defun org-roam-folgezettel-store-link (node)
  "Call `org-store-link' on NODE.
If called interactively, NODE is the node at point.

The stored description uses the value of `org-roam-node-formatter', if
provided."
  (interactive (list (vtable-current-object)) org-roam-folgezettel-mode)
  (let ((description (org-roam-node-formatted node)))
    (org-link-store-props :type "id"
                          :link (concat "id:" (org-roam-node-id node))
                          :description description)
    ;; Push the link into `org-stored-links'
    (push (list (plist-get org-store-link-plist :link)
                (plist-get org-store-link-plist :description))
          org-stored-links)
    (message "Stored link to %s!" description)))

;;;###autoload
(defun org-roam-folgezettel-show-node-in-list (node)
  "Opens NODE in a new `org-roam-folgezettel-mode' buffer.
If called interactively, NODE is the org-roam node at point."
  (interactive (list (org-roam-node-at-point :assert)) org-mode)
  (progn
    (let ((buf (org-roam-folgezettel-list :new-buffer)))
      (switch-to-buffer buf)
      (goto-char (point-min))         ; Ensure point is in vtable
      (vtable-goto-object node)
      (message "Going to node titled \"%s\"..." (org-roam-node-title node)))))

;;; Major mode and keymap
(defvar-keymap org-roam-folgezettel-mode-map
  :doc "Mode map for `org-roam-folgezettel-mode'."
  "SPC" #'scroll-up-command
  "DEL" #'scroll-down-command
  "p" #'previous-line
  "n" #'next-line
  ;; TODO 2024-11-14: I haven't figure out a way to shadow the
  ;; `vtable-revert-command' binding in `vtable-map'
  "r" #'org-roam-folgezettel-refresh
  "q" #'quit-window
  "x" #'kill-current-buffer
  "RET" #'org-roam-folgezettel-open-node
  "o" #'org-roam-folgezettel-open-node-other-window
  "C-o" #'org-roam-folgezettel-display-node
  "i" #'org-roam-folgezettel-edit-index
  "M-u" #'org-roam-folgezettel-upward
  "M-d" #'org-roam-folgezettel-downward
  "M-n" #'org-roam-folgezettel-forward-sibling
  "M-p" #'org-roam-folgezettel-backward-sibling
  "M-<up>" #'org-roam-folgezettel-move-up
  "M-<down>" #'org-roam-folgezettel-move-down
  "w" #'org-roam-folgezettel-store-link
  "s" #'org-roam-folgezettel-goto-node
  "/ /" #'org-roam-folgezettel-filter-query-modify
  "/ d" #'org-roam-folgezettel-filter-directory
  "/ p" #'org-roam-folgezettel-filter-person
  "/ t" #'org-roam-folgezettel-filter-tags)

(define-derived-mode org-roam-folgezettel-mode fundamental-mode "ORF"
  "Major mode for listing org-roam nodes."
  :interactive nil
  :group 'org-roam-folgezettel
  :after-hook (set (make-local-variable 'mode-line-misc-info)
                   (append
                    (list
                     (list 'org-roam-folgezettel-filter-query
                           '(:eval (format " [Query:%s]"
                                           (string-remove-suffix "," (string-trim (funcall org-roam-folgezettel-filter-indicator))))))))))

;;; Provide
(provide 'org-roam-folgezettel)
;;; org-roam-folgezettel.el ends here
