;;; org-roam-folgezettel.el --- Folgezettel interface for Org-roam  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; URL: https://github.com/krisbalintona/org-roam-folgezettel
;; Keywords: files, text, convenience
;; Version: 0.2.3
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
  :type '(restricted-sexp :match-alternatives (listp)))

(defcustom org-roam-folgezettel-index-color-style 'color-last
  "The coloring style for index numbering."
  :type '(choice
          (const :tag "Color only the last index numbering section" color-last)
          (const :tag "Color the entire index number" color-full)))

(defcustom org-roam-folgezettel-default-buffer-name "*Node listing*"
  "The default name for `org-roam-folgezettel-mode' buffers."
  :type 'string)

;;;; Faces

;;;; Internal
(defvar org-roam-folgezettel-filter-indicator ""
  "Mode line indicator for current filter.
Inspired by tablist.el's filter indicator.  Is added to
`mode-line-misc-info'.")

(defvar-local org-roam-folgezettel-filter-query-history nil
  "History of `org-roam-folgezettel-filter-query' values.")

(defvar-local org-roam-folgezettel-filter-query-history-place nil
  "Current place in `org-roam-folgezettel-filter-query-history'.
Is zero-indexed.")

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

;;;; Filter queries
(defun org-roam-folgezettel-filter--modify (query new-buffer)
  "Modify the filter for the current buffer and update listing.
If QUERY is non-nil, use that string as the new query.

If NEW-BUFFER is non-nil, pass that argument to
`org-roam-folgezettel-list'.  For a description of the behavior of
NEW-BUFFER, see the docstring of `org-roam-folgezettel-list'."
  (if new-buffer
      (org-roam-folgezettel-list new-buffer query)
    (setq-local org-roam-folgezettel-filter-query query
                org-roam-folgezettel-filter-query-history
                (append (list query)
                        (nthcdr org-roam-folgezettel-filter-query-history-place
                                org-roam-folgezettel-filter-query-history))
                org-roam-folgezettel-filter-query-history-place 0)
    (org-roam-folgezettel-refresh)))

;;;; Buffer names
(defun org-roam-folgezettel--buffer-name-concat (query)
  "Returns buffer name according to QUERY.
QUERY is a query (a string is also accepted) form accepted by
`org-roam-ql-nodes'.

This function returns a string intended to convey the QUERY by combining
it with the value of `org-roam-folgezettel-default-buffer-name'.  For
example, if QUERY is \"(title \"bar\")\" and the value of
`org-roam-folgezettel-default-buffer-name' is \"*foo*\", then this
function returns \"*foo [bar]*\"."
  (let ((trimmed-default
         (string-trim org-roam-folgezettel-default-buffer-name "*" "*")))
    (format "*%s [%s]*" trimmed-default query)))

;;;; Vtable
;;;;; Retrieving values
(defun org-roam-folgezettel-list--retrieve-index (node)
  "Retrieve the index number of NODE.
Returns the trimmed value of the \"ROAM_PLACE\" property.  Additionally,
if the index string is empty, return nil."
  (let ((values (assoc "ROAM_PLACE" (org-roam-node-properties node) #'string-equal)))
    (when (and (cdr values) (not (string-empty-p (cdr values))))
      (string-trim (cdr values)))))

(defun org-roam-folgezettel-list--retrieve-person (node)
  "Retrieve the person of NODE.
Returns the trimmed value of the \"ROAM_PERSON\" property.
Additionally, if the index string is empty, return nil."
  (let ((values (assoc "ROAM_PERSON" (org-roam-node-properties node) #'string-equal)))
    (when (and (cdr values) (not (string-empty-p (cdr values))))
      (cdr values))))

(defun org-roam-folgezettel-list--retrieve-box (node)
  "Retrieve the box of NODE.
Returns the trimmed value of the \"ROAM_BOX\" property.  Additionally,
if the index string is empty, return nil."
  (let ((values (assoc "ROAM_BOX" (org-roam-node-properties node) #'string-equal)))
    (when (and (cdr values) (not (string-empty-p (cdr values))))
      (cdr values))))

;;;;; Formatters
(defun org-roam-folgezettel--index-formatter (index)
  "Propertize index INDEX.
Meant to be used as the formatter for index numberings."
  (let* ((parts (org-roam-folgezettel--index-split index))
         (level (1- (length parts)))
         (outline-face
          (when parts
            (if (= 0 (+ 1 (% (1- level) 8)))
                'font-lock-warning-face
              (intern (format "outline-%s" (+ 1 (% (1- level) 8))))))))
    (pcase org-roam-folgezettel-index-color-style
      ('color-last
       (if parts
           (concat (propertize (string-remove-suffix (car (last parts)) index) 'face 'shadow)
                   (propertize (car (last parts)) 'face `(:weight bold :inherit ,outline-face)))
         ""))
      ('color-full
       (replace-regexp-in-string "\\." (propertize "." 'face 'shadow)
                                 (propertize index 'face `outline-face))))))

(defun org-roam-folgezettel--path-formatter (path)
  "Propertize PATH for `org-roam-folgezettel-mode' path column.
PATH is a list of strings representing the file and headline outline
path of a node, with the last string representing the title of the node."
  (let* ((path-face '(:weight light :inherit shadow))
         (separator " > ")
         (file-title (car path))
         (propertized-file-title
          (propertize (concat file-title separator) 'face 'shadow))
         (olp (butlast (cdr path)))
         (propertized-olp
          (string-join (mapcar (lambda (s) (propertize s 'face path-face)) olp)
                       (propertize separator 'face 'shadow)))
         (title (car (last path)))
         (propertized-title (propertize title 'face '(:height 1.0 :inherit variable-pitch))))
    (concat propertized-title
            (unless (string-empty-p propertized-olp)
              (concat
               (propertize " (" 'face 'shadow)
               propertized-file-title
               propertized-olp
               (propertize ")" 'face 'shadow))))))

(defun org-roam-folgezettel--tags-formatter (tags)
  "Propertize a series of TAGS.
Meant to be used as the formatter for tags."
  (propertize tags 'face 'org-tag))

;;;;; Org-roam-ql integration
;;;;;; Helper functions
(defun org-roam-folgezettel--index-prefix-p (index1 index2)
  "Check if the parts of INDEX1 are the prefix of the parts of INDEX2.
For example, this function returns non-nil when INDEX1 is \"1.1\" and
INDEX2 is \"1.1a\", but returns nil when INDEX1 is \"1.1\" and INDEX2 is
\"10.1a\"."
  (let ((index1-parts
         (org-roam-folgezettel--index-split index1))
        (index2-parts
         (org-roam-folgezettel--index-split index2)))
    (and (<= (length index1-parts) (length index2-parts))
         (cl-every #'equal index1-parts (cl-subseq index2-parts 0 (length index1-parts))))))

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
  #'org-roam-folgezettel-list--retrieve-box
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

(org-roam-ql-defpred 'descendants
  "A predicate for the descendants of an index numbering.
A descendant of a node is one that, speaking from in folgezettel terms,
is a child of that node or a child of one of the children of that node.

The returned nodes list does not include the node from which all other
returned nodes are descended from."
  #'org-roam-folgezettel-list--retrieve-index
  (lambda (index-value index-query)
    (unless (stringp index-query) (error "Index argument should be a string!"))
    (and (org-roam-folgezettel--index-prefix-p index-query index-value)
         (not (string-equal index-query index-value)))))

(org-roam-ql-defpred 'children
  "A predicate for the children of an index numbering.
A child of a node is one that, speaking from in folgezettel terms, is
one nesting level below that node."
  #'org-roam-folgezettel-list--retrieve-index
  (lambda (index-value index-query)
    (unless (stringp index-query) (error "Index argument should be a string!"))
    (and (org-roam-folgezettel--index-prefix-p index-query index-value)
         (= (length (org-roam-folgezettel--index-split index-value))
            (1+ (length (org-roam-folgezettel--index-split index-query)))))))

(org-roam-ql-defpred 'first-children
  "A predicate for the first children of an index numbering.
The first child of a node, speaking in folgezettel terms, is the
immediate descendant of the node whose index numbering is the
smallest (lexicographically) among its siblings.  For example, if a node
has children indexed as 1.1, 1.2, and 1.3, the first child is 1.1."
  #'org-roam-folgezettel-list--retrieve-index
  (lambda (index-value index-query)
    (unless (stringp index-query) (error "Index argument should be a string!"))
    (when (and (org-roam-folgezettel--index-prefix-p index-query index-value)
               (not (string-equal index-query index-value)))
      ;; TODO 2024-11-23: We assume that the first child is either an index
      ;; numbering that ends with a 1 or a.  This is incompatible with index
      ;; numbering systems which use a different value (e.g. negative numbers).
      (not (cl-member-if-not
            (lambda (part) (or (string= part "1") (string= part "a")))
            (org-roam-folgezettel--index-split (string-remove-prefix index-query index-value)))))))

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
returned data is for.  VTABLE is the vtable this getter is for.

This function supports getting data for columns with the following
names:
- Index
- Path (i.e. `org-roam-node-olp' appended with `org-roam-node-title')
- Tags"
  (pcase (vtable-column vtable column)
    ("Index"
     (or (org-roam-folgezettel-list--retrieve-index node) ""))
    ("Path"
     (append (list (org-roam-node-file-title node))
             (org-roam-node-olp node)
             (list (org-roam-node-title node))))
    ("Tags"
     (or (propertize
          (mapconcat (lambda (s) (concat "#" s)) (org-roam-node-tags node))
          'face 'org-tag)
         ""))))

;;; Commands
;;;###autoload
(defun org-roam-folgezettel-list (&optional buf-name filter-query)
  "List org-roam nodes with vtable.el.
The first optional argument is NEW-BUFFER:
- If BUF-NAME is a string, that will be the name of the buffer created.  If a
  buffer with such a name exists already, open that buffer instead.
- If BUF-NAME is non-nil but not a string, create a new buffer whose name is
  unique (using `generate-new-buffer-name').
- If BUF-NAME is nil, the string specified by
  `org-roam-folgezettel-default-buffer-name' is used.
When called interactively, BUF-NAME is the `current-prefix-arg'.

A second optional argument is available: FILTER-QUERY.  If FILTER-QUERY
is supplied, use that form (see `org-roam-ql-nodes') to filter the nodes
list.

See the bindings in `org-roam-folgezettel-table-map' below:
\\{org-roam-folgezettel-mode-map}"
  (interactive (list current-prefix-arg nil))
  (setq buf-name
        (cond
         ((stringp buf-name) buf-name)
         ((and buf-name (not (stringp buf-name)))
          (generate-new-buffer-name org-roam-folgezettel-default-buffer-name))
         ((not buf-name) org-roam-folgezettel-default-buffer-name)))
  (let ((buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        ;; Only insert vtable and set buffer-local values if the buffer doesn't
        ;; already have one
        (unless (save-excursion (goto-char (point-min)) (vtable-current-table))
          (org-roam-folgezettel-mode)
          (setq-local buffer-read-only t
                      truncate-lines t
                      org-roam-folgezettel-filter-query
                      (or filter-query org-roam-folgezettel-filter-query)
                      org-roam-folgezettel-filter-query-history
                      (list (or filter-query org-roam-folgezettel-filter-query))
                      org-roam-folgezettel-filter-query-history-place 0
                      org-roam-folgezettel-filter-indicator
                      (lambda () (prin1-to-string org-roam-folgezettel-filter-query)))
          ;; Create vtable after setting buffer-local value for
          ;; `org-roam-folgezettel-filter-query'
          (make-vtable
           :columns '(( :name "Index"
                        :align left
                        :formatter org-roam-folgezettel--index-formatter)
                      ( :name "Path"
                        :align left
                        ;; TODO 2024-11-10: Figure out how to use a percentage.
                        ;; I think setting a percentage only works if the table
                        ;; is called in a visible buffer.  Additionally, for
                        ;; better comparability with `display-buffer-alist',
                        ;; ideally the major mode must also be set before
                        ;; creating the table.
                        :width 130
                        :formatter org-roam-folgezettel--path-formatter)
                      ( :name "Tags"
                        :align right
                        :formatter org-roam-folgezettel--tags-formatter))
           :keymap org-roam-folgezettel-table-map
           :objects-function #'org-roam-folgezettel-list--objects
           :getter #'org-roam-folgezettel-list--getter
           :separator-width 2))))
    (select-window (display-buffer buf))
    buf))

;;;; Showing nodes
(defun org-roam-folgezettel-open-node (node &optional display-action no-select)
  "Open the NODE at point.
If DISPLAY-ACTION is supplied, use it as the buffer display function.
If NO-SELECT is supplied, then don't select the buffer."
  (interactive (list (vtable-current-object) nil) org-roam-folgezettel-mode)
  (let* ((file (org-roam-node-file node))
         (location (org-roam-node-point node))
         (buf (find-file-noselect file))
         (window (display-buffer buf display-action)))
    ;; Select the window unless NO-SELECT is true
    (unless no-select
      (select-window window))
    (with-current-buffer buf
      (if (< (point-min) location (point-max))
          (when-let* ((window (get-buffer-window buf)))
            (goto-char location)
            ;; We must call `set-window-point' to move point in the buffer to
            ;; cover the case when NO-SELECT is non-nil
            (set-window-point window location))
        (message "Node point is outside the visible part of the buffer")))))

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
  (let* ((save-silently t)
         (file (org-roam-node-file node))
         (current-index (org-roam-folgezettel-list--retrieve-index node))
         (node-point (org-roam-node-point node))
         (node-box (org-roam-folgezettel-list--retrieve-box node))
         (all-box-nodes (org-roam-ql-nodes `(box ,node-box)))
         (all-index-numbers
          (cl-loop for node in (org-roam-node-list)
                   when (and (member node all-box-nodes)
                             (not (or (equal nil (org-roam-folgezettel-list--retrieve-index node))
                                      (string-empty-p (org-roam-folgezettel-list--retrieve-index node)))))
                   collect (org-roam-folgezettel-list--retrieve-index node)))
         (prompt (format "New index numbering for %s: " (org-roam-node-formatted node)))
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
        (save-restriction
          (save-excursion
            (widen)
            (goto-char node-point)
            (org-roam-node-at-point 'assert)
            (org-set-property "ROAM_PLACE" new-index))
          (save-buffer)
          (org-roam-db-update-file file)
          (message "Set index of %s to %s" (org-roam-node-title node) new-index)))
      (vtable-update-object (vtable-current-table) node))))

;;;; Filtering
(defun org-roam-folgezettel-filter-undo ()
  "Apply previous filter query.
Queries are preserved in `org-roam-folgezettel-filter-query-history'."
  (interactive)
  (let ((previous-place
         (1+ org-roam-folgezettel-filter-query-history-place)))
    (if (< (1- (length org-roam-folgezettel-filter-query-history))
           previous-place)
        (message "No previous query in filter history")
      (setq-local org-roam-folgezettel-filter-query
                  (nth previous-place org-roam-folgezettel-filter-query-history)
                  org-roam-folgezettel-filter-query-history-place previous-place)
      (message "Going back in filter history")
      (org-roam-folgezettel-refresh))))

(defun org-roam-folgezettel-filter-redo ()
  "Apply next filter query.
Queries are preserved in `org-roam-folgezettel-filter-query-history'."
  (interactive)
  (let ((next-place
         (1- org-roam-folgezettel-filter-query-history-place)))
    (if (> 0 next-place)
        (message "No next query in filter history")
      (setq-local org-roam-folgezettel-filter-query
                  (nth next-place org-roam-folgezettel-filter-query-history)
                  org-roam-folgezettel-filter-query-history-place next-place)
      (message "Going forward in filter history")
      (org-roam-folgezettel-refresh))))

(defun org-roam-folgezettel-filter-query-edit (new-query new-buffer-p)
  "Manually modify the filter for the current `org-roam-folgezettel' buffer.
If NEW-QUERY is non-nil, use that string as the new query.

If NEW-BUFFER-P is non-nil, then apply this filter to a new
`org-roam-folgezettel-mode' buffer named according to the filter
applied.

Other filtering commands are available in
`org-roam-folgezettel-table-map':
\\{org-roam-folgezettel-mode-map}"
  (interactive (list (read-string "New filter query: "
                                  (and org-roam-folgezettel-filter-query (prin1-to-string org-roam-folgezettel-filter-query)))
                     current-prefix-arg)
               org-roam-folgezettel-mode)
  (org-roam-folgezettel-filter--modify
   (read new-query)
   (when new-buffer-p
     (org-roam-folgezettel--buffer-name-concat new-query))))

(defun org-roam-folgezettel-filter-directory (subdir new-buffer-p)
  "Filter the current buffer's node listing to SUBDIR.
SUBDIR is a subdirectory of the `org-roam-directory'.

If called interactively, SUBDIR is prompted for.

If NEW-BUFFER-P is non-nil, then apply this filter to a new
`org-roam-folgezettel-mode' buffer named according to the filter
applied.

Other filtering commands are available in
`org-roam-folgezettel-table-map':
\\{org-roam-folgezettel-mode-map}"
  (interactive (list (completing-read "Subdirectory: "
                                      (mapcar (lambda (dir) (file-relative-name dir org-roam-directory))
                                              (seq-filter #'file-directory-p
                                                          (directory-files org-roam-directory t "^[^.]" t))))
                     current-prefix-arg)
               org-roam-folgezettel-mode)
  (let ((subdir (string-trim subdir "/" "/"))
        (new-query (if org-roam-folgezettel-filter-query
                       `(and ,org-roam-folgezettel-filter-query
                             (subdir ,subdir))
                     `(subdir ,subdir))))
    (org-roam-folgezettel-filter--modify
     new-query
     (when new-buffer-p
       (org-roam-folgezettel--buffer-name-concat new-query)))
    (message "Filtered nodes to the %s subdirectory" subdir)))

(defun org-roam-folgezettel-filter-person (person new-buffer-p)
  "Filter the current node listing by PERSON.
PERSON is the value of the \"ROAM_PERSON\" property.

If called interactively, prompts for a person to filter by.

If NEW-BUFFER-P is non-nil, then apply this filter to a new
`org-roam-folgezettel-mode' buffer named according to the filter
applied.

Other filtering commands are available in
`org-roam-folgezettel-table-map':
\\{org-roam-folgezettel-mode-map}"
  (interactive (list (read-string "Filter by the following person: ")
                     current-prefix-arg)
               org-roam-folgezettel-mode)
  (let ((new-query (if org-roam-folgezettel-filter-query
                       `(and ,org-roam-folgezettel-filter-query
                             (person ,person))
                     `(person ,person))))
    (org-roam-folgezettel-filter--modify
     new-query
     (when new-buffer-p
       (org-roam-folgezettel--buffer-name-concat new-query)))
    (message "Filtered nodes by the %s person" person)))

(defun org-roam-folgezettel-filter-title (regexp new-buffer-p)
  "Filter the current node titles by REGEXP.
If called interactively, prompts for the regexp to match node titles by.

If NEW-BUFFER-P is non-nil, then apply this filter to a new
`org-roam-folgezettel-mode' buffer named according to the filter
applied.

Other filtering commands are available in
`org-roam-folgezettel-table-map':
\\{org-roam-folgezettel-mode-map}"
  (interactive (list (read-regexp "Regexp to filter titles by: ")
                     current-prefix-arg)
               org-roam-folgezettel-mode)
  (let ((new-query (if org-roam-folgezettel-filter-query
                       `(and ,org-roam-folgezettel-filter-query
                             (title ,regexp))
                     `(title ,regexp))))
    (org-roam-folgezettel-filter--modify
     new-query
     (when new-buffer-p
       (org-roam-folgezettel--buffer-name-concat new-query)))
    (message "Filtered node titles by regexp: %s" regexp)))

(defun org-roam-folgezettel-filter-tags (tags new-buffer-p)
  "Filter the current node listing by TAGS.
If called interactively, prompts for the tags to filter by.

If NEW-BUFFER-P is non-nil, then apply this filter to a new
`org-roam-folgezettel-mode' buffer named according to the filter
applied.

Other filtering commands are available in
`org-roam-folgezettel-table-map':
\\{org-roam-folgezettel-mode-map}"
  (interactive (list (let ((crm-separator "[    ]*:[    ]*"))
                       (mapconcat #'identity (completing-read-multiple "Tag(s): " (org-roam-tag-completions))))
                     current-prefix-arg)
               org-roam-folgezettel-mode)
  (let ((new-query (if org-roam-folgezettel-filter-query
                       `(and ,org-roam-folgezettel-filter-query
                             (tags ,tags))
                     `(tags ,tags))))
    (org-roam-folgezettel-filter--modify
     new-query
     (when new-buffer-p
       (org-roam-folgezettel--buffer-name-concat new-query)))
    (message "Filtered nodes by the tags: %s" tags)))

(defun org-roam-folgezettel-filter-children (node new-buffer-p)
  "Filter the current node listing to the children of NODE.
The filtered results also include NODE.

If called interactively, NODE is the vtable object at point.

If NEW-BUFFER-P is non-nil, then apply this filter to a new
`org-roam-folgezettel-mode' buffer named according to the filter
applied.

Other filtering commands are available in
`org-roam-folgezettel-table-map':
\\{org-roam-folgezettel-mode-map}"
  (interactive (list (vtable-current-object) current-prefix-arg)
               org-roam-folgezettel-mode)
  (let* ((index (org-roam-folgezettel-list--retrieve-index node))
         (id (org-roam-node-id node))
         (new-query (if org-roam-folgezettel-filter-query
                        `(and ,org-roam-folgezettel-filter-query
                              (or (id ,id)
                                  (children ,index)))
                      `(or (id ,id)
                           (children ,index)))))
    (org-roam-folgezettel-filter--modify
     new-query
     (when new-buffer-p
       (org-roam-folgezettel--buffer-name-concat new-query)))
    (message "Filtered nodes to the children of %s" (org-roam-node-formatted node))))

(defun org-roam-folgezettel-filter-descendants (node new-buffer-p)
  "Filter the current node listing to the descendants of NODE.
The filtered results also include NODE.

If called interactively, NODE is the vtable object at point.

If NEW-BUFFER-P is non-nil, then apply this filter to a new
`org-roam-folgezettel-mode' buffer named according to the filter
applied.

Other filtering commands are available in
`org-roam-folgezettel-table-map':
\\{org-roam-folgezettel-mode-map}"
  (interactive (list (vtable-current-object) current-prefix-arg)
               org-roam-folgezettel-mode)
  (let* ((index (org-roam-folgezettel-list--retrieve-index node))
         (id (org-roam-node-id node))
         (new-query (if org-roam-folgezettel-filter-query
                        `(and ,org-roam-folgezettel-filter-query
                              (or (id ,id)
                                  (descendants ,index)))
                      `(or (id ,id)
                           (descendants ,index)))))
    (org-roam-folgezettel-filter--modify
     new-query
     (when new-buffer-p
       (org-roam-folgezettel--buffer-name-concat new-query)))
    (message "Filtered nodes to the descendants of %s" (org-roam-node-formatted node))))

;;;; Movement via index numbers
(defun org-roam-folgezettel-upward (&optional dist)
  "Move point to DIST parents upward in the vtable.
DIST is an integer representing the number of parents to move upwards
by.  If DIST is negative, move downward.

Other movement commands are available in
`org-roam-folgezettel-table-map':
\\{org-roam-folgezettel-mode-map}"
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
by.  If DIST is negative, move upward.

Other movement commands are available in
`org-roam-folgezettel-table-map':
\\{org-roam-folgezettel-mode-map}"
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
If DIST is negative, move backward.

Other movement commands are available in
`org-roam-folgezettel-table-map':
\\{org-roam-folgezettel-mode-map}"
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
If DIST is negative, move forward.

Other movement commands are available in
`org-roam-folgezettel-table-map':
\\{org-roam-folgezettel-mode-map}"
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
(defun org-roam-folgezettel-refresh ()
  "Refresh the current `org-roam-folgezettel-mode' buffer."
  (interactive)
  ;; We rely on the node's ID in case any data of the node was changed, e.g.,
  ;; its tags
  (let ((id (org-roam-node-id (vtable-current-object)))
        (col (current-column)))
    (widen)
    (text-property-search-backward 'vtable)
    (vtable-revert-command)
    ;; We must manually go to the original object because this command first
    ;; ensures we are on the vtable (e.g. not at the end of the buffer) by
    ;; moving point, then calling `vtable-revert-command', which reverts the
    ;; table but "restores" the point to the location we moved the point to.
    ;; This is still the case even with `save-excursion'.
    (vtable-goto-object
     (cl-find-if (lambda (object) (equal id (org-roam-node-id object)))
                 (vtable-objects (vtable-current-table))))
    (move-to-column col)))

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
  (interactive (list (or (vtable-current-object) (org-roam-node-at-point :assert)))
               org-mode org-roam-folgezettel-mode)
  (let* ((node-formatted (org-roam-node-formatted node))
         (buf (org-roam-folgezettel-list
               (org-roam-folgezettel--buffer-name-concat node-formatted))))
    (switch-to-buffer buf)
    (goto-char (point-min))             ; Ensure point is in vtable
    (if (let* ((objects (vtable-objects (vtable-current-table)))
               (target
                ;; We match by ID just in case there is a mismatch in any data
                ;; between the actual node and the node data in the org-roam
                ;; database
                (cl-find (org-roam-node-id node)
                         objects
                         :key #'org-roam-node-id
                         :test #'string=)))
          (vtable-goto-object target))
        (message "Going to %s..." node-formatted)
      (message "Could not find %s" node-formatted))))

(defun org-roam-folgezettel-kill-line (node)
  "Visually remove NODE from table at point.
Internally, calls `vtable-remove-object' on the vtable at point."
  (interactive (list (vtable-current-object)) org-roam-folgezettel-mode)
  (let ((inhibit-read-only t))
    (vtable-remove-object (vtable-current-table) node)))

;;; Major mode and keymap
(defvar-keymap org-roam-folgezettel-mode-map
  :doc "Keymap for vtables in `org-roam-folgezettel-mode'."
  "p" #'previous-line
  "n" #'next-line
  "SPC" #'scroll-up-command
  "DEL" #'scroll-down-command
  "q" #'quit-window
  "x" #'kill-current-buffer)

(defvar-keymap org-roam-folgezettel-table-map
  :doc "Keymap for vtables in `org-roam-folgezettel-mode'."
  "p" #'previous-line
  "n" #'next-line
  "g" #'org-roam-folgezettel-refresh
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
  "C-k" #'org-roam-folgezettel-kill-line
  "C-/" #'org-roam-folgezettel-filter-undo
  "C-?" #'org-roam-folgezettel-filter-redo
  "/ /" #'org-roam-folgezettel-filter-query-edit
  "/ d" #'org-roam-folgezettel-filter-directory
  "/ p" #'org-roam-folgezettel-filter-person
  "/ t" #'org-roam-folgezettel-filter-tags
  "/ n" #'org-roam-folgezettel-filter-title
  "/ i c" #'org-roam-folgezettel-filter-children
  "/ i d" #'org-roam-folgezettel-filter-descendants)

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
