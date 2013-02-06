;;; rsp-misc.el --- my miscellaneous functions

;; Copyright (C) 2013

;; Author:  R. Peele
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is elisp that is either too general to belong in another
;; module, or too small and specific to be its own module.

;;; Code:

(defun combinations (lists)
  "Return all possible combinations resulting from taking one element from each list in LISTS.

For example, (combinations '((1 2) (3 4))) equals ((1 3) (1 4) (2 3) (2 4))."
  (when lists
    (let ((head (car lists))
          (tail (cdr lists)))
      (if tail
          (mapcan
           #'(lambda (x)
               (mapcar (apply-partially 'cons x)
                       (combinations tail)))
           head)
        (mapcar 'list head)))))

(defun recursive-type-of (object)
  "Return a composite representation of the type of OBJECT.

For most inputs, behaves identically to `type-of'.
However, when OBJECT is a vector or cons, recursive-type-of maps itself over the sequence.
For example, (recursive-type-of [1 (\"str\" 2) 3.14]) returns [integer (string integer) float].

Additionally, (recursive-type-of nil) returns nil, not symbol."
  (and object
       (or
        (cond
         ((listp object)
          (cons (recursive-type-of (car object))
                (recursive-type-of (cdr object))))
         ((vectorp object)
          (apply 'vector (mapcar 'recursive-type-of object))))
        (type-of object))))

(defun reverse-string (string)
  "Return STRING with its characters in reverse order."
  (apply 'string (reverse (string-to-list string))))

(defun only-matches (regexp)
  "Output matches of REGEXP from the current buffer to a temporary buffer, one per line.

Only the matched text will be output, not the entire line it occured on."
  (interactive "*MOnly: ")
  (with-output-to-temp-buffer "*only*"
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (princ (concat (match-string-no-properties 0) "\n"))))))

(provide 'rsp-misc)
;;; rsp-misc.el ends here
