;;; org-roam-folgezettel.el --- Folgezettel interface for Org-roam  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; URL: https://github.com/krisbalintona/org-roam-folgezettel
;; Keywords: files, text, convenience
;; Version: 0.3.2
;; Package-Requires: ((emacs "29.1") (org-roam-ql "0.3-pre"))

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
(require 'transient)

;;; Variables

;;;; Options
(defgroup org-roam-folgezettel ()
  "Interfaces for org-roam nodes."
  :group 'files
  :prefix "org-roam-folgezettel-")

(defcustom org-roam-folgezettel-default-filter-query nil
  "A symbol representing a filter for the buffer.
This symbol is of the form of SOURCE-OR-QUERY that org-roam-ql commands
like `org-roam-ql-search'accepts."
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

(defcustom org-roam-folgezettel-make-table-parameters
  (list :objects-function #'org-roam-folgezettel-list--objects
        :getter #'org-roam-folgezettel-list--getter
        :object-equal #'org-roam-folgezettel-list--object-equal
        :columns `(( :name "Index"
                     :primary ascend
                     :align left
                     :formatter ,#'org-roam-folgezettel--index-formatter
                     :comparator ,#'org-roam-folgezettel--index-lessp
                     :truncate-guess-tolerance 0)
                   ( :name "Path"
                     :align left
                     :max-width "65%"
                     :formatter ,#'org-roam-folgezettel--path-formatter
                     :truncate-guess-tolerance 0)
                   ( :name "Tags"
                     :align right
                     :formatter ,#'org-roam-folgezettel--tags-formatter
                     :max-width "25%"
                     :truncate-guess-tolerance 0))
        :use-header-line t
        :separator-width 2
        :column-color-function #'org-roam-folgezettel--column-color-function
        :use-navigation-keymap t
        :keymap org-roam-folgezettel-table-map
        :actions org-roam-folgezettel-action-map)
  "A list of parameters passed to `make-vtable' to create a node listing.
This option is useful for users who want to customize the parameters
used to created the vtable.  See the vtable manual for the parameters
`make-vtable' accepts:

    (info \"(vtable) Making A Table\")

The parameters set here will be the default values used for every vtable
created by `org-roam-folgezettel'.

However, some parameters may be overridden by `org-roam-folgezettel'
under various conditions.  For instance, the :insert parameter will
always be set to nil, even if it is set to non-nil here.

Users may use the :extra-data slot for their own purposes, but (1) it
should be a plist and (2) several properties in this plist are used
internally by `org-roam-folgezettel' and should not be used to prevent
undefined behavior.  (See also `org-roam-folgezettel--table-get-data'
and `org-roam-folgezettel--table-set-data'.)  You may see the internally
used properties by calling the following on an existing vtable created
by `org-roam-folgezettel--table-set-data':

    (vtable-extra-data (vtable-current-vtable))"
  :type '(plist :key-type symbol :value-type sexp))

;;;; Faces

;;;; History
(defvar org-roam-folgezettel-filter-query-edit-history (list)
  "History for the `org-roam-folgezettel-filter-query-edit' command.")

(defvar org-roam-folgezettel-filter-directory-history (list)
  "History for the `org-roam-folgezettel-filter-directory' command.")

(defvar org-roam-folgezettel-filter-person-history (list)
  "History for the `org-roam-folgezettel-filter-person' command.")

(defvar org-roam-folgezettel-filter-title-history (list)
  "History for the `org-roam-folgezettel-filter-title' command.")

(defvar org-roam-folgezettel-filter-tags-history (list)
  "History for the `org-roam-folgezettel-filter-tags' command.")

(defvar org-roam-folgezettel-edit-index-history (list)
  "History for the `org-roam-folgezettel-edit-index' command.")

;;;; Internal

;;; Functions and macros

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

;;;; Vtable local data
(defmacro org-roam-folgezettel--table-get-data (var &optional table)
  "Get the value of VAR in TABLE.
Additional data may be set in the vtable using its :extra-data slot.
Use this macro to access that data.

The form `org-roam-folgezettel' structures this data slot as a plist.
VAR should be plist property like :foo-bar.  If TABLE is nil, use the
vtable at point."
  `(plist-get (vtable-extra-data (or ,table (vtable-current-table))) ,var))

(defmacro org-roam-folgezettel--table-set-data (table &rest args)
  "Set the value of VAR in TABLE.
Additional data may be set in the vtable using its :extra-data slot.
Use this macro to set variables that data.

The form `org-roam-folgezettel' structures this data slot as a plist.
VAR should be plist property like :foo-bar.  If TABLE is nil, use the
vtable at point.

ARGS should have the form (VAR VAL VAR VAL ...).  For example:

    (org-roam-folgezettel--table-set-data (vtable-current-table)
          :test \\='test
          :foo \"foo1\"
          :bar \"bar2\")"
  (declare (indent 1))
  `(let ((extra-data (vtable-extra-data (or ,table (vtable-current-table)))))
     ,@(cl-loop for (var val) on args by #'cddr
                collect `(setq extra-data (plist-put extra-data ,var ,val)))
     (vtable-set-extra-data ,table extra-data)))

;;;; Filter queries
(defun org-roam-folgezettel-filter--modify (query new-buffer)
  "Modify the filter for the current buffer and update listing.
If QUERY is non-nil, use that string as the new query.

If NEW-BUFFER is non-nil, pass that argument to
`org-roam-folgezettel-list'.  For a description of the behavior of
NEW-BUFFER, see the docstring of `org-roam-folgezettel-list'."
  (if new-buffer
      (org-roam-folgezettel-list new-buffer query)
    (org-roam-folgezettel--table-set-data (vtable-current-table)
      :filter-query query
      :filter-query-history (append
                             (list query)
                             (nthcdr (org-roam-folgezettel--table-get-data :filter-query-history-index)
                                     (org-roam-folgezettel--table-get-data :filter-query-history)))
      :filter-query-history-index 0)
    (vtable-revert-command)))

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

;;;; Making the vtable

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

;;;;; Special formatting
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

(defun org-roam-folgezettel--column-color-function (_line-index column-index _value node &optional _computed-color-faces)
  "Color function for table columns.
For a description of _LINE-INDEX, COLUMN-INDEX, _VALUE, NODE, and
_COMPUTED-COLOR-FACES, refer to the vtable manual:
    (info \"(vtable) Making A Table\")"
  (when (eq column-index 1)
    (pcase (org-roam-node-type node)
      ("collection" '(:inherit diff-refine-changed))
      ("pointer" '(:inherit diff-refine-added)))))

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

;;;;;; Index sorter
(defun org-roam-folgezettel--node-index-lessp (node1 node2)
  "Compare the indices of NODE1 and NODE2.
See the docstring of `org-roam-folgezettel--index-lessp'."
  (let ((index1 (org-roam-folgezettel-list--retrieve-index node1))
        (index2 (org-roam-folgezettel-list--retrieve-index node2)))
    (org-roam-folgezettel--index-lessp index1 index2)))

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

(org-roam-ql-defpred 'in-org-buffer
  "A predicate for the nodes in a given org-mode buffer.
Accepts either a buffer object or a buffer name.

The reason we define this predicate is because org-roam-ql's
\"in-buffer\" expansion accepts only `org-roam-mode' and
`org-agenda-mode' buffers."
  #'org-roam-node-file
  (lambda (file-value buf-query)
    (let ((file-query (buffer-file-name (get-buffer buf-query))))
      (string-equal (expand-file-name file-value) (expand-file-name file-query)))))

;;;;; Composition of vtable
(defun org-roam-folgezettel-list--objects ()
  "Get objects for vtable.
Returns a list of lists, one for every org-roam node.  Each list
contains the cached information for that node."
  (let ((filter-query
         (or (when (vtable-current-table) (org-roam-folgezettel--table-get-data :filter-query))
             org-roam-folgezettel-default-filter-query
             (org-roam-node-list))))
    (or (org-roam-ql-nodes filter-query)
        (prog1
            (org-roam-node-list)
          (message "Query yields no results! Showing all nodes instead.")))))

(defun org-roam-folgezettel-list--object-equal (node1 node2)
  "Predicate for equality between NODE1 and NODE2.
Used for the :object-equal slot in the vtable.  We only compare the IDs
of NODE1 and NODE2 for robustness against changes to node data (e.g.,
tags) made after a table is created."
  (equal (org-roam-node-id node1) (org-roam-node-id node2)))

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
(defun org-roam-folgezettel-list (&optional buf-name filter-query display-buffer-action)
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

A third and final optional argument is available: DISPLAY-BUFFER-ACTION.
DISPLAY-BUFFER-ACTION is the action passed to `display-buffer'; it must
be a list, satisfying the description provided by `display-buffer's
docstring.  By default, the buffer is displayed in the same window in
which this function is called.  (NOTE: Due to bug#69837,
DISPLAY-BUFFER-ACTION must be a display buffer action that renders the
buffer visible.  Otherwise, the widths of columns will not be correctly
calculated during the initial creation of the vtable.)

See the bindings in `org-roam-folgezettel-table-map' below:
\\{org-roam-folgezettel-mode-map}"
  (interactive (list current-prefix-arg nil '(display-buffer-same-window)))
  (setq buf-name
        (cond
         ((stringp buf-name) buf-name)
         ((and buf-name (not (stringp buf-name)))
          (generate-new-buffer-name org-roam-folgezettel-default-buffer-name))
         ((not buf-name) org-roam-folgezettel-default-buffer-name)))
  (let ((buf (get-buffer-create buf-name))
        (filter-query (or filter-query org-roam-folgezettel-default-filter-query)))
    ;; NOTE: Due to a limitation in vtable (see bug#69837), the width of object
    ;; representations can only be properly calculated when the buffer the
    ;; vtable is created on is currently visible.  Therefore, we must switch to
    ;; a buffer then create the vtable.
    (with-selected-window (select-window (display-buffer buf display-buffer-action))
      ;; Only insert vtable and set buffer-local and table local values if the
      ;; buffer doesn't already have a table
      (unless (save-restriction (save-excursion (widen) (goto-char (point-min)) (vtable-current-table)))
        (let ((inhibit-read-only t)
              (table (apply #'make-vtable (append org-roam-folgezettel-make-table-parameters '(:insert nil)))))
          (org-roam-folgezettel-mode)
          (vtable-insert table)
          ;; Set table local variables for vtable
          (org-roam-folgezettel--table-set-data table
            :filter-query filter-query
            :filter-query-history (list filter-query)
            :filter-query-history-index 0
            :filter-query-mode-line-indicator
            `(lambda () (prin1-to-string (org-roam-folgezettel--table-get-data :filter-query ,table))))
          (setq-local buffer-read-only t
                      truncate-lines t))))
    buf))

;;;; Showing nodes
(defun org-roam-folgezettel-open-node (node &optional display-buffer-action no-select indirect-buffer-p)
  "Open the NODE in its file and move point to its location.
If NODE is in a folded or invisible region, reveal its heading.  If NODE
is outside the visible part of the buffer, optionally open it in an
indirect buffer.

DISPLAY-BUFFER-ACTION is a list that the ACTION parameter of
`display-buffer' accepts.

When NO-SELECT is nil, open the buffer in a window and push the current
point this function was called from to `org-mark-ring'.  When NO-SELECT
is non-nil, do not select the window of the node's buffer and do not
push the current point to `org-mark-ring'.

When INDIRECT-BUFFER-P IS t forces opening NODE in an indirect buffer,
whereas normally an indirect buffer is created only when the node is in
an inaccessible portion of a narrowed buffer and the user accepts the
prompt to create a new indirect buffer.  If INDIRECT-BUFFER-P is set to
\\='accept, then this prompt is not shown and the indirect buffer is
created anyway."
  (interactive (list (vtable-current-object)) org-roam-folgezettel-mode)
  (let* ((file (org-roam-node-file node))
         (location (org-roam-node-point node))
         (buf (find-file-noselect file))
         ;; Set buf to a widened indirect clone buffer if INDIRECT-BUFFER-P is
         ;; non-nil or if accepted by user prompt, which is only shown if
         ;; INDIRECT-BUFFER-P is not 'accept
         (_ (with-current-buffer buf
              (when (or (and indirect-buffer-p (not (eq 'accept indirect-buffer-p)))
                        (and (not (<= (point-min) location (point-max)))
                             (or (eq 'accept indirect-buffer-p)
                                 (y-or-n-p "Node point is outside the visible part of the buffer.  Open in new indirect buffer?"))))
                (with-current-buffer (clone-indirect-buffer nil nil)
                  (widen)
                  (setq buf (current-buffer))))))
         (display-buffer-action (or display-buffer-action '(display-buffer-same-window)))
         (window (display-buffer buf display-buffer-action)))
    ;; Only select the window and push to the org mark ring when NO-SELECT is
    ;; nil
    (unless no-select
      (org-mark-ring-push)
      (select-window window))
    (with-current-buffer buf
      (if (<= (point-min) location (point-max))
          (progn
            (goto-char location)
            ;; We must call `set-window-point' to move point in the buffer to
            ;; cover the case when NO-SELECT is non-nil
            (set-window-point window location)
            (when (org-fold-folded-p)
              (message "Node in folded region of buffer. Revealing node heading and its heading ancestors")
              (org-fold-show-context 'ancestors)))
        (message "Node is not in visible part of buffer. Not moving point")))))

(defun org-roam-folgezettel-open-node-other-window (node)
  "Show NODE in a new window, selected the buffer.
If called interactively, NODE is the node corresponding to the vtable
object at point."
  (interactive (list (vtable-current-object)) org-roam-folgezettel-mode)
  (org-roam-folgezettel-open-node node '(display-buffer-pop-up-window)))

(defun org-roam-folgezettel-display-node (node)
  "Show NODE in a new window without selected the buffer.
If called interactively, NODE is the node corresponding to the vtable
object at point."
  (interactive (list (vtable-current-object)) org-roam-folgezettel-mode)
  (org-roam-folgezettel-open-node node '(display-buffer-pop-up-window) :no-select))

;;;###autoload
(defun org-roam-folgezettel-show-node-in-list (node)
  "Opens NODE in a new `org-roam-folgezettel-mode' buffer.
If called interactively, NODE is the org-roam node at point."
  (interactive (list (or (vtable-current-object) (org-roam-node-at-point :assert)))
               org-mode org-roam-folgezettel-mode)
  (let* ((node-formatted (org-roam-node-formatted node))
         (buf (org-roam-folgezettel-list
               (org-roam-folgezettel--buffer-name-concat node-formatted)
               nil                      ; Maybe be the box this node belongs to?
               '(display-buffer-same-window))))
    (display-buffer buf '(display-buffer-same-window))
    (with-current-buffer buf
      (if (let* ((objects (vtable-objects (vtable-current-table)))
                 (target
                  ;; We match by ID just in case there is a mismatch in any data
                  ;; between the actual node and the node data in the org-roam
                  ;; database
                  (cl-find (org-roam-node-id node)
                           objects
                           :key #'org-roam-node-id
                           :test #'string=)))
            (push-mark)
            (goto-char (point-min))       ; Ensure point is in vtable
            (vtable-goto-object target))
          (message "Going to %s..." node-formatted)
        (message "Could not find %s" node-formatted)))))

;;;; Marking
(defun org-roam-folgezettel-toggle-mark-all ()
  "Toggle the marked state of all nodes in the current vtable."
  (interactive)
  (dolist (node (vtable-objects (vtable-current-table)))
    (vtable-toggle-marked-object node)))

(defun org-roam-folgezettel-marked-eval (form &optional accept-indirect-buffer)
  "Evaluate FORM on each node marked in the vtable.
For each marked node, FORM will be evaluated with point at the beginning
of the node.  Internally, the file corresponding to a node will be
opened with `org-roam-folgezettel-open-node', which may create indirect
buffers.  Read its docstring for more information.

When called interactively, FORM will be prompted for.

When ACCEPT-INDIRECT-BUFFER is non-nil, pass \\='accept to the
INDIRECT-BUFFER-P parameter of `org-roam-folgezettel-open-node'.  When
called interactively, this is the universal argument."
  (interactive (list (read--expression "Eval in marked nodes: ") current-prefix-arg))
  (if-let ((nodes (vtable-marked-objects (vtable-current-table))))
      (let (result)
        ;; Save window state to restore it after all FORMs are evaluated, just
        ;; in case FORM alters the window state
        (save-window-excursion
          (dolist (node nodes result)
            (save-mark-and-excursion
              (org-roam-folgezettel-open-node node nil t accept-indirect-buffer)
              ;; 2025-04-26: Not sure if dynamic or lexical bind is more appropriate
              (setq result (eval form t))))))
    (message "No marked nodes!")))

;;;; Filtering
(defun org-roam-folgezettel-filter-undo ()
  "Apply previous filter query.
Queries are preserved in the table's local data."
  (interactive)
  (let ((previous-place
         (1+ (org-roam-folgezettel--table-get-data :filter-query-history-index)))
        (history (org-roam-folgezettel--table-get-data :filter-query-history)))
    (if (< (1- (length history))
           previous-place)
        (message "No previous query in filter history")
      (org-roam-folgezettel--table-set-data (vtable-current-table)
        :filter-query (nth previous-place history)
        :filter-query-history-index previous-place)
      (message "Going back in filter history")
      (vtable-revert-command))))

(defun org-roam-folgezettel-filter-redo ()
  "Apply next filter query.
Queries are preserved in the table's local data."
  (interactive)
  (let ((next-place
         (1- (org-roam-folgezettel--table-get-data :filter-query-history-index)))
        (history (org-roam-folgezettel--table-get-data :filter-query-history)))
    (if (> 0 next-place)
        (message "No next query in filter history")
      (org-roam-folgezettel--table-set-data (vtable-current-table)
        :filter-query (nth next-place history)
        :filter-query-history-index next-place)
      (message "Going forward in filter history")
      (vtable-revert-command))))

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
                                  (funcall (org-roam-folgezettel--table-get-data :filter-query-mode-line-indicator))
                                  'org-roam-folgezettel-filter-query-edit-history)
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
                                                          (directory-files org-roam-directory t "^[^.]" t)))
                                      nil t nil 'org-roam-folgezettel-filter-directory-history)
                     current-prefix-arg)
               org-roam-folgezettel-mode)
  (let* ((subdir (string-trim subdir "/" "/"))
         (filter-query (org-roam-folgezettel--table-get-data :filter-query))
         (new-query (if filter-query
                        `(and ,filter-query
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
  (interactive (list (read-string "Filter by the following person: "
                                  nil 'org-roam-folgezettel-filter-person-history)
                     current-prefix-arg)
               org-roam-folgezettel-mode)
  (let* ((filter-query (org-roam-folgezettel--table-get-data :filter-query))
         (new-query (if filter-query
                        `(and ,filter-query
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
  (interactive (list (read-regexp "Regexp to filter titles by: "
                                  nil 'org-roam-folgezettel-filter-title-history)
                     current-prefix-arg)
               org-roam-folgezettel-mode)
  (let* ((filter-query (org-roam-folgezettel--table-get-data :filter-query))
         (new-query (if filter-query
                        `(and ,filter-query
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
                       (completing-read-multiple "Tag(s): " (org-roam-tag-completions)
                                                 nil nil nil 'org-roam-folgezettel-filter-tags-history))
                     current-prefix-arg)
               org-roam-folgezettel-mode)
  (let* ((filter-query (org-roam-folgezettel--table-get-data :filter-query))
         (new-query (if filter-query
                        `(and ,filter-query
                              ,(flatten-list `(tags ,tags)))
                      (flatten-list `(tags ,tags)))))
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
         (filter-query (org-roam-folgezettel--table-get-data :filter-query))
         (new-query (if filter-query
                        `(and ,filter-query
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
         (filter-query (org-roam-folgezettel--table-get-data :filter-query))
         (new-query (if filter-query
                        `(and ,filter-query
                              (or (id ,id)
                                  (descendants ,index)))
                      `(or (id ,id)
                           (descendants ,index)))))
    (org-roam-folgezettel-filter--modify
     new-query
     (when new-buffer-p
       (org-roam-folgezettel--buffer-name-concat new-query)))
    (message "Filtered nodes to the descendants of %s" (org-roam-node-formatted node))))

(defun org-roam-folgezettel-filter-buffer (buffer new-buffer-p)
  "Filter the current node listing to nodes in FILE.
BUFFER is either a buffer name or a buffer object.

If called interactively, prompts for the file to filter by.

If NEW-BUFFER-P is non-nil, then apply this filter to a new
`org-roam-folgezettel-mode' buffer named according to the filter
applied.

Other filtering commands are available in
`org-roam-folgezettel-table-map':
\\{org-roam-folgezettel-mode-map}"
  (interactive (list (read-buffer "Which buffer's nodes? " (current-buffer) t
                                  (lambda (b)
                                    (let ((buf (get-buffer (if (stringp b) b (car b)))))
                                      (eq 'org-mode (buffer-local-value 'major-mode buf)))))
                     current-prefix-arg)
               org-roam-folgezettel-mode)
  (let* ((filter-query (org-roam-folgezettel--table-get-data :filter-query))
         (new-query (if filter-query
                        `(and ,filter-query
                              (in-org-buffer ,buffer))
                      `(in-org-buffer ,buffer))))
    (org-roam-folgezettel-filter--modify
     new-query
     (when new-buffer-p
       (org-roam-folgezettel--buffer-name-concat new-query)))
    (message "Filtered nodes to those in %s"
             (cond ((stringp buffer) buffer)
                   ((bufferp buffer) (buffer-name buffer))
                   (t (error "BUFFER should be a buffer name or buffer object"))))))

;;;; Editing
(defun org-roam-folgezettel-edit-index (node)
  "Edit the index of NODE.
Prompts for a new index for NODE.  If called interactively, NODE is the
node at point."
  (interactive (list (vtable-current-object)) org-roam-folgezettel-mode)
  (let* ((save-silently t)
         ;; Just in case NODE is outdated (e.g., this function is called
         ;; interactively on a vtable listing whose corresponding node has since
         ;; been changed), we get the updated node from NODE's ID
         (node (org-roam-node-from-id (org-roam-node-id node)))
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
            (add-to-history 'org-roam-folgezettel-edit-index-history new-index)
            (org-set-property "ROAM_PLACE" new-index))
          (save-buffer)
          (org-roam-db-update-file file)
          (message "Set index of %s to %s" (org-roam-node-title node) new-index))))))

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
  (let ((table-nodes (vtable-objects (vtable-current-table)))
        (initial-point (point)))
    (when (vtable-goto-object (org-roam-node-read
                               nil
                               (lambda (node) (member node table-nodes))
                               nil t "Go to node: "))
      (push-mark initial-point :nomsg))))

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

(defun org-roam-folgezettel-kill-line (node)
  "Visually remove NODE from table at point.
Internally, calls `vtable-remove-object' on the vtable at point."
  (interactive (list (vtable-current-object)) org-roam-folgezettel-mode)
  (let ((inhibit-read-only t))
    (vtable-remove-object (vtable-current-table) node)))

;;; Major mode, keymaps, and transient menus
(defvar-keymap org-roam-folgezettel-mode-map
  :doc "Keymap for vtables in `org-roam-folgezettel-mode'."
  "SPC" #'scroll-up-command
  "DEL" #'scroll-down-command
  "x" #'kill-current-buffer)

(defvar-keymap org-roam-folgezettel-table-map
  :doc "Keymap for vtables in `org-roam-folgezettel-mode'."
  "T" #'org-roam-folgezettel-toggle-mark-all
  "e" #'org-roam-folgezettel-marked-eval
  "s" #'org-roam-folgezettel-goto-node
  "M-u" #'org-roam-folgezettel-upward
  "M-d" #'org-roam-folgezettel-downward
  "M-n" #'org-roam-folgezettel-forward-sibling
  "M-p" #'org-roam-folgezettel-backward-sibling
  "M-<up>" #'org-roam-folgezettel-move-up
  "M-<down>" #'org-roam-folgezettel-move-down
  "C-/" #'org-roam-folgezettel-filter-undo
  "C-?" #'org-roam-folgezettel-filter-redo
  "/ ?" #'org-roam-folgezettel-filter-menu
  "/ /" #'org-roam-folgezettel-filter-query-edit
  "/ d" #'org-roam-folgezettel-filter-directory
  "/ p" #'org-roam-folgezettel-filter-person
  "/ t" #'org-roam-folgezettel-filter-tags
  "/ n" #'org-roam-folgezettel-filter-title
  "/ b" #'org-roam-folgezettel-filter-buffer
  "/ i c" #'org-roam-folgezettel-filter-children
  "/ i d" #'org-roam-folgezettel-filter-descendants)

(defcustom org-roam-folgezettel-action-map
  '("RET" org-roam-folgezettel-open-node
    "C-o" org-roam-folgezettel-display-node
    "o" org-roam-folgezettel-open-node-other-window
    "i" org-roam-folgezettel-edit-index
    "w" org-roam-folgezettel-store-link
    "C-k" org-roam-folgezettel-kill-line
    "m" vtable-mark-object
    "M" vtable-mark-all-objects
    "u" vtable-unmark-object
    "U" vtable-unmark-all-objects
    "t" vtable-toggle-marked-object)
  "A list of actions org-roam-folgezettel vtables.
This is a list whose syntax is the same as ‘define-keymap’.  Functions
in this list are called on the vtable object at point as the first and
only argument.

Users should add to this list functions that act on a given node by
taking that node as its only argument.

Evaluate the following for more information on vtable action maps:

    \(info \"(vtable) Making A Table\"\)"
  :type '(repeat (choice (string :tag "Keybind")
                         (function :tag "Function"))))

(define-derived-mode org-roam-folgezettel-mode fundamental-mode "ORF"
  "Major mode for listing org-roam nodes."
  :interactive nil
  :group 'org-roam-folgezettel
  :after-hook
  (set (make-local-variable 'mode-line-misc-info)
       (append
        (list
         '(:eval
           (save-excursion
             (goto-char (point-min))
             (when-let ((indicator-func (org-roam-folgezettel--table-get-data :filter-query-mode-line-indicator)))
               (format " [Query:%s]" (string-remove-suffix "," (string-trim (funcall indicator-func)))))))))))

(transient-define-prefix org-roam-folgezettel-filter-menu ()
  "Transient menu for Org-Roam Folgezettel filters."
  ["Filter By"
   ["Index numbering"
    ("c" "Children" org-roam-folgezettel-filter-children)
    ("d" "Descendants" org-roam-folgezettel-filter-descendants)]
   ["Other"
    ("s" "Directory" org-roam-folgezettel-filter-directory)
    ("p" "Person" org-roam-folgezettel-filter-person)
    ("t" "Tags" org-roam-folgezettel-filter-tags)
    ("r" "Title" org-roam-folgezettel-filter-title)
    ("b" "Buffer" org-roam-folgezettel-filter-buffer)]]
  ["Modify filter query"
   [("e" "Edit query" org-roam-folgezettel-filter-query-edit)
    ("U" "Forward in history" org-roam-folgezettel-filter-redo)
    ("u" "Backward in history" org-roam-folgezettel-filter-undo)]])

;;; Provide
(provide 'org-roam-folgezettel)
;;; org-roam-folgezettel.el ends here
