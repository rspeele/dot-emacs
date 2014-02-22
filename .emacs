;; custom
(setq custom-file "~/.emacs-custom.el")
(load custom-file)

;; load path
(add-to-list 'load-path "~/.emacs.d/borrowed/")
(add-to-list 'load-path "~/.emacs.d/custom/")

;; requires and loads
(require 'go-mode-load)
(require 'cubescript-mode-load)
(require 'self-color-load)
(require 'sherbet-mode-load)
(require 'window-structure)
(require 'rsp-misc)
(require 'infix)
(load "haskell-mode/haskell-site-file")
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append
       '(("\\.csproj$" . nxml-mode)
         ("\\.cs$" . csharp-mode)
         ("/TODO$" . org-mode))
       auto-mode-alist))

(defun visibly-distinguish-tabs ()
  "Change the display of ASCII tab characters for the current buffer."
  (interactive)
  (standard-display-ascii ?\t "\u00bb  "))

;; use color escape sequences for rcirc
(eval-after-load 'rcirc '(require 'rcirc-controls))

;; use cmi utils and indentation for sql mode
(eval-after-load 'sql
  '(progn
     (require 'sql-indent)))

(add-hook 'sql-mode-hook 'visibly-distinguish-tabs)

;; sql-interactive-mode behavior
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (hl-line-mode t)
            (setq show-trailing-whitespace nil)
            (setq truncate-lines t)))

;; picture-mode behavior
(add-hook 'picture-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))

;; use smart indent and nice features for haskell
(add-hook 'haskell-mode-hook
          (lambda ()
            (visibly-distinguish-tabs)
            (turn-on-haskell-indent)
            (turn-on-haskell-doc-mode)
            (turn-on-haskell-decl-scan)))

;; use hex-color-mode with CSS
(add-hook 'css-mode-hook 'hex-color-mode)

;; don't ask about killing comint interpreters
(add-hook 'comint-exec-hook
          (lambda ()
            (set-process-query-on-exit-flag
             (get-buffer-process (current-buffer)) nil)))

;; always use y-or-n-p
(defalias 'yes-or-no-p 'y-or-n-p)

;; disable beep if the option exists
(when (fboundp 'set-message-beep)
  (set-message-beep 'silent))

;; disable auto backup
(setq backup-inhibited t)

;; enabled non-default
(put 'downcase-region 'disabled nil)

;; glyphs
(define-key 'iso-transl-ctl-x-8-map "E" [?\u2203])
(define-key 'iso-transl-ctl-x-8-map "V" [?\u2200])
;; remember: C-u C-x = for info about a character

;;; binds

;; unbind C-z, C-x C-z will still work if `suspend-frame' is needed
(global-unset-key [?\C-z])

;; window tree
(global-set-key [?\C-c ?w ?n] 'transpose-next-window)
(global-set-key [?\C-c ?w ?p] 'transpose-previous-window)
(global-set-key [?\C-c ?w ?r] 'rotate-windows-right)
(global-set-key [?\C-c ?w ?l] 'rotate-windows-left)
(global-set-key [?\C-c ?w ?z] 'trim-window-branch)
(global-set-key [?\C-c ?w ?1] 'preserve-window-branch)
(global-set-key [?\C-c ?w ?2] 'split-root-window-vertically)
(global-set-key [?\C-c ?w ?3] 'split-root-window-horizontally)

;; regexp
(global-set-key [?\C-c ?a ?r] 'align-regexp)
(global-set-key [?\C-c ?r ?r] 'replace-regexp)
(global-set-key [?\C-c ?r ?s] 'replace-string)
(global-set-key [?\C-c ?k ?l] 'keep-lines)
(global-set-key [?\C-c ?f ?l] 'flush-lines)
(global-set-key [?\C-c ?d ?w] 'delete-trailing-whitespace)

;; buffer
(global-set-key [?\C-c ?r ?b] 'revert-buffer)

;; comment
(global-set-key [?\C-c ?c ?r] 'comment-or-uncomment-region)

;; shell
(global-set-key [?\C-c ?s] 'shell)

