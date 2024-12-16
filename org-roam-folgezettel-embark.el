;;; org-roam-folgezettel-embark.el --- Embark integration with org-roam-folgezettel  -*- lexical-binding: t; -*-

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

;; Integration between org-roam-folgezettel and embark.el.

;;; Code:
(require 'org-roam-folgezettel)
(require 'embark)

;;; Export in `org-roam-folgezettel-mode' buffer
(defun org-roam-folgezettel--embark-export (candidates)
  "Export CANDIDATES to a new `org-roam-folgezettel-mode' buffer."
  (interactive)
  (org-roam-folgezettel-list t ; The buffer gets renamed by embark automatically
                             (mapcar (lambda (cand) (get-text-property 0 'node cand))
                                     candidates)))

(add-to-list 'embark-exporters-alist
             '(org-roam-node . org-roam-folgezettel--embark-export))

;;; Provide
(provide 'org-roam-folgezettel-embark)
;;; org-roam-folgezettel-embark.el ends here
