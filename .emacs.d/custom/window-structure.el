;;; window-structure.el --- tiling window manipulation functions

;; Copyright (C) 2013

;; Author:  R. Peele
;; Keywords: frames

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

;; This module provides functions for manipulating the window tree.
;; You can use it to do things like:
;;
;; * Swap the buffers two windows are displaying.
;; * Rotate your window configuration (change |- to T, -|, or _|_).
;; * Delete windows inside or outside a window branch.
;; * Split out a new top level window from anywhere in the window tree.

;;; Code:

(defun rotate-windows-right ()
  "Rotate the windows in the current frame clockwise."
  (interactive)
  (rotate-all-windows t))
(defun rotate-windows-left ()
  "Rotate the windows in the current frame counter-clockwise."
  (interactive)
  (rotate-all-windows nil))

(defun transpose-next-window ()
  "Exchange the place of the cyclic next window with the current window."
  (interactive)
  (exchange-window-buffers (selected-window) (next-window)))
(defun transpose-previous-window ()
  "Exchange the place of the cyclic previous window with the current window."
  (interactive)
  (exchange-window-buffers (selected-window) (previous-window)))

;; More general, non-interactive funs
(defun window-buffer-state (window)
  "Return a list representing the reproducible state of WINDOW."
  (list
   (window-buffer window)
   (window-start window)
   (window-point window)
   (eq (selected-window) window)))

(defun set-window-buffer-state (window state)
  "Apply STATE to WINDOW.
STATE should be a list representing window state, as returned from `window-buffer-state'."
  (set-window-buffer window (pop state))
  (set-window-start window (pop state))
  (set-window-point window (pop state))
  (if (pop state) (select-window window)))

(defun window-buffer-tree (&optional tree)
  "Return a tree of window buffer states from the given window tree TREE."
  (unless tree
    (setq tree (car (window-tree))))
  (if (windowp tree)
      (window-buffer-state tree)
    (let ((vert (car tree))
          (wins (nthcdr 2 tree)))
      (cons vert (mapcar 'window-buffer-tree wins)))))

(defun realize-window-buffer-tree (tree &optional root)
  "Beginning from the window ROOT, split windows so as to reproduce TREE."
  (unless root (setq root (selected-window)))
  (if (bufferp (car tree))
      (set-window-buffer-state root tree)
    (let ((hrz (not (car tree)))
          (wins (cdr tree))
          (nxt nil))
      (mapc (lambda (x) (split-window root nil hrz)) (cdr wins))
      (while wins
        (setq nxt (next-window root))
        (realize-window-buffer-tree (car wins) root)
        (setq root nxt)
        (setq wins (cdr wins))))))

(defun turn-window-buffer-tree (tree clockwise)
  "Rotate TREE left if CLOCKWISE is nil, or right if CLOCKWISE is t.
TREE should be a tree of window buffer state lists, as returned from `window-buffer-tree'."
  (if (bufferp (car tree))
      tree
    (let ((ornt (not (car tree)))
          (wbfs (cdr tree)))
      (let ((flip (if (eq (not clockwise) (not ornt)) wbfs (reverse wbfs))))
        (cons ornt
         (mapcar #'(lambda (f) (turn-window-buffer-tree f clockwise)) flip))))))

(defun rotate-all-windows (clockwise)
  "Rotate all windows in the current frame right or left."
  (let ((tr (window-buffer-tree)))
    (delete-other-windows)
    (realize-window-buffer-tree (turn-window-buffer-tree tr clockwise))))

(defun exchange-window-buffers (w p)
  "Exchange the buffers and associated state of windows W and P."
  (let ((ws (window-buffer-state w))
        (ps (window-buffer-state p)))
    (set-window-buffer-state w ps)
    (set-window-buffer-state p ws)))

(defun window-tree-to-list (tree)
  "Extract a flat list of constituent windows from TREE."
  (if (windowp tree)
      (list tree)
    (apply 'append (mapcar 'window-tree-to-list (cddr tree)))))

(defun find-window-branch (window branch)
  "Return the sub-branch of BRANCH which contains WINDOW as an immediate leaf."
  (if (windowp branch)
      (if (eq window branch) branch)
    (let ((wins (cddr branch)))
      (if (some '(lambda (b) (eq window b)) wins)
          branch
        (some '(lambda (b) (find-window-branch window b)) wins)))))

(defun trim-window-branch (&optional window)
  "Delete other windows at the branch level of WINDOW."
  (interactive)
  (unless window (setq window (selected-window)))
  (let ((tree (car (window-tree))))
    (mapc 'delete-window
          (remq window
                (window-tree-to-list
                 (find-window-branch window tree))))))

(defun preserve-window-branch (&optional window)
  "Delete all windows outside the branch containing WINDOW as a leaf."
  (interactive)
  (unless window (setq window (selected-window)))
  (let ((branch (find-window-branch window (car (window-tree)))))
    (let ((bufs (window-buffer-tree branch)))
      (delete-other-windows)
      (realize-window-buffer-tree bufs))))

(defun split-root-window (&optional window size side)
  "Create a new top level window alongside the existing window tree.
The new window will display the same buffer as WINDOW, which defaults to the selected window.
The arguments SIZE and SIDE work as in `split-window'."
  (interactive)
  (let ((tree (window-buffer-tree)))
    (delete-other-windows window)
    (split-window window size side)
    (realize-window-buffer-tree tree window)))

(defun split-root-window-left (&optional window size)
  (interactive)
  (split-root-window window size 'left))

(defun split-root-window-right (&optional window size)
  (interactive)
  (split-root-window window size 'right))

(defalias 'split-root-window-horizontally 'split-root-window-right)

(defun split-root-window-above (&optional window size)
  (interactive)
  (split-root-window window size 'above))

(defun split-root-window-below (&optional window size)
  (interactive)
  (split-root-window window size 'below))

(defalias 'split-root-window-vertically 'split-root-window-below)

(provide 'window-structure)
;;; window-structure.el ends here
