(require 'color)
;; fixme try sherbet-mode on keep-lines from replace.el
;; whole chunk following docstring is highlighted @ depth 1
(defvar sherbet-light-colors
  ["#eeffff"
   "#ffeeff"
   "#ffffee"
   "#ffeeee"
   "#eeffee"
   "#eeeeff"])

(defvar sherbet-dark-colors
  ["#111122"
   "#112211"
   "#221111"
   "#112222"
   "#221122"
   "#222211"])

(defvar sherbet-colors sherbet-light-colors)

(defun sherbet-positive-color (depth)
  (let* ((cycle (length sherbet-colors))
         (index (mod depth cycle))
         (darken (/ depth cycle)))
    (color-darken-name
     (elt sherbet-colors index) depth)))

(defun sherbet-negative-color (depth)
  (color-darken-name "#fff0f0" depth))

(defun sherbet-depth-face (depth)
  (cond
   ((= depth 0) 'default)
   ((> depth 0) `(:foreground ,(sherbet-positive-color depth)))
   ((< depth 0) `(:underline ,(sherbet-negative-color (- depth))))))

(defun sherbet-shade-region (from to depth)
  (when (/= 0 depth)
    (put-text-property
     from to 'font-lock-face (sherbet-depth-face depth))))

(defun sherbet-forward-delim (from to)
  "Move point forward to the next delimiter between FROM and TO."
  (let* ((open-depth (car (parse-partial-sexp from to 1)))
         (open-point (point))
         (close-depth (car (parse-partial-sexp from to -1))))
    (if (>= open-point (point))
        close-depth
      (goto-char open-point)
      open-depth)))

(defun sherbet-debug (from to depth)
  (message (format "%s to %s at depth %s" from to depth)))

; syntax parsing has depth changing after the delimiter
;      1     2     1    2    1   0
;      v     v     v    v    v   v
;     (     (     )    (    )   )
;
; sherbet-map-depth-regions maps the following regions
;     1----12-----21--12----21--1
;     v    vv     vv  vv    vv  v
;     (     (     )    (    )   )
;

(defun sherbet-map-depth-regions (from to fun)
  (let ((depth (car (syntax-ppss from)))
        (back from)
        (front from)
        (delta))
    (while (< back to)
      (setq delta (sherbet-forward-delim from to))
      (setq front
            (if (> delta 0) (1- (point)) (point)))
      (funcall fun back front depth)
      (setq back front)
      (setq from (point))
      (setq depth (+ delta depth)))))

(defun sherbet-fontify-region (from to)
  (save-excursion
    (sherbet-map-depth-regions from to 'sherbet-shade-region)))

;;;###autoload
(defun sherbet-mode-enable ()
  (jit-lock-register 'sherbet-fontify-region t))

;;;###autoload
(defun sherbet-mode-disable ()
  (jit-lock-unregister 'sherbet-fontify-region)
  (with-silent-modifications
    (put-text-property (point-min) (point-max) 'font-lock-face nil)
    (font-lock-fontify-buffer)))

;;;###autoload
(define-minor-mode sherbet-mode
  "Fontify text according to its syntactic depth."
  nil "" nil
  (if sherbet-mode
      (sherbet-mode-enable)
    (sherbet-mode-disable)))

(provide 'sherbet-mode)
