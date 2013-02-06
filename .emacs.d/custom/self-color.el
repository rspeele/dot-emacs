;;; self-color.el --- highlight text describing color in the color it describes

;; Copyright (C) 2013

;; Author:  R. Peele
;; Keywords: faces, convenience

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

;; This code is based on the examples at
;; <http://www.emacswiki.org/emacs/HexColour>.

;; I've just adapted it for my preferences and wrapped it as two minor
;; modes, one for hexadecimal colors and one for named colors (they
;; can both be active at the same time but I rarely have use for the
;; latter).

;;; Code:

(defsubst self-color-contrast (color)
  "Return white or black, whichever has greater contrast against COLOR."
  (let ((white "#ffffff")
        (black "#000000"))
    (if (> (color-distance color white) (color-distance color black))
        white
      black)))

(defsubst self-color-face (color)
  "Return a face for highlighting text in COLOR."
  (list :underline t
        :background color
        :foreground (self-color-contrast color)))

;; matches 3 or 6 digit hexadecimal numbers prefixed by #, #x, or 0x
(defconst hex-color-keywords
  '(("\\(#x?\\|\\<0x\\)\\([[:xdigit:]]\\{3\\}\\([[:xdigit:]]\\{3\\}\\)?\\)\\>"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face
         (self-color-face (concat "#" (match-string-no-properties 2))))))))

;; matches Emacs color names
(defconst named-color-keywords
  `((,(regexp-opt (defined-colors) 'words)
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face
         (self-color-face (match-string-no-properties 0)))))))

(defsubst rgb-byte-color-name (red green blue)
  (let ((r (read red))
        (g (read green))
        (b (read blue)))
    (format "#%02x%02x%02x" r g b)))

;; matches space separated decimal RGB colors like 0 255 127
(defconst rgb-byte-color-keywords
  '(("\\<\\([0-9]\\{1,3\\}\\)\\>\\s-+\\<\\([0-9]\\{1,3\\}\\)\\>\\s-+\\<\\([0-9]\\{1,3\\}\\)\\>"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face
         (self-color-face
          (apply 'rgb-byte-color-name
                 (mapcar 'match-string-no-properties '(1 2 3)))))))))

;;;###autoload
(define-minor-mode rgb-byte-color-mode
  "Highlight space separated RGB colors in the color they represent."
  nil "" nil
  (if rgb-byte-color-mode
      (font-lock-add-keywords nil rgb-byte-color-keywords t)
    (font-lock-remove-keywords nil rgb-byte-color-keywords)))

;;;###autoload
(define-minor-mode named-color-mode
  "Highlight Emacs color names in the color they represent."
  nil "" nil
  (if named-color-mode
      (font-lock-add-keywords nil named-color-keywords t)
    (font-lock-remove-keywords nil named-color-keywords)))

;;;###autoload
(define-minor-mode hex-color-mode
  "Highlight hexadecimal colors in the color they represent."
  nil "" nil
  (if hex-color-mode
      (font-lock-add-keywords nil hex-color-keywords t)
    (font-lock-remove-keywords nil hex-color-keywords)))

(provide 'self-color)
;;; self-color.el ends here
