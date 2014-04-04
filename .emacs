;; start the emacs server, load (+ save) sessions

(server-start)
;(package-initialize)
(desktop-save-mode)

;; no menu-bar, toolbar or scroll bar

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; clean *scratch*, no more startup screen

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; window dimensions

;(add-to-list 'default-frame-alist '(width . 155))
;(add-to-list 'default-frame-alist '(height . 43))

;; Split windows horizontally by default

;(setq split-height-threshold 80)
;(setq split-width-threshold 160)

;; color theme and transparency

(add-to-list 'default-frame-alist '(foreground-color . "gray"))
(add-to-list 'default-frame-alist '(background-color . "gray10"))
(add-to-list 'default-frame-alist '(cursor-color . "white"))
(add-to-list 'default-frame-alist '(alpha . (95 90)))

;; Window title for topbar.

;; (setq frame-title-format '("xi: " (buffer-file-name "%f" (dired-directory dired-directory "%b"))))
(setq frame-title-format "xi")




;; truncate lines and make column numbers visible

(setq truncate-partial-width-windows t)

;; Add column number and buffer size to modeline

(column-number-mode 1)
(size-indication-mode 1)

;; no restrictions

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

;; yes-or-no --> y-or-n, kill-line --> kill-whole-line

(defalias 'yes-or-no-p 'y-or-n-p)
(setq kill-whole-line t)

;; highlight brackets

(show-paren-mode t)
(setq show-paren-style 'expression)

;; delete-selection-mode

(delete-selection-mode)

;; Scrolling

(setq scroll-margin 5)
(setq scroll-conservatively 5)
(setq scroll-error-top-bottom nil)
(setq scroll-preserve-screen-position t)

(eval-after-load "comint"
  '(add-to-list 'comint-mode-hook
     (lambda () (set (make-local-variable 'scroll-margin) 0))))

;; Echo keystrokes faster

(setq echo-keystrokes 0.1)

;; Add line numbers to programming buffers and colorize their colour codes.

(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)
(eval-after-load "rainbow-mode" '(diminish 'rainbow-mode))



;; Undo-tree mode, make it global, diminish it from modelines.

(eval-after-load "undo-tree" '(global-undo-tree-mode))
(eval-after-load "diminish" '(diminish 'undo-tree-mode))

;; Configure package archives

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")))

;; ;; Structured-Haskell-Mode

;; (add-to-list 'load-path "~/Programming/Haskell/structured-haskell-mode/elisp")
;; (setq shm-program-name "/home/kron/.cabal/bin/structured-haskell-mode")
;; (require 'shm)

;; (add-hook 'haskell-mode-hook 'structured-haskell-mode)



;; Registers!

(set-register ?e '(file . "~/.emacs"))
(set-register ?m '(file . "~/.xmonad/xmonad.hs"))
(set-register ?b '(file . "~/.bashrc"))
(set-register ?c '(file . "~/.xmonad/.conky_dzen"))
(set-register ?v '(file . "~/.vimperatorrc"))
(set-register ?z '(file . "~/.zshrc"))
(set-register ?i '(file . "~/.xinitrc"))
(set-register ?d '(file . "~/.Xdefaults"))

(set-register ?t '(file . "~/todo.navi"))

;; Set auto-mode-alist for various modes and autload others

(setq auto-mode-alist (append '(("\\.vimperatorrc\\'" . vimrc-mode)
				("\\.h\\'" . c++-mode)
				("\\.rkt\\'" . scheme-mode)
				("\\.pl\\'" . prolog-mode)) auto-mode-alist))

;; Custom keybindings!

(global-set-key "\M-9" 'insert-parentheses)
(global-set-key "\M-8" (lambda () (interactive) (insert "*")))

(global-set-key (kbd "C-0") 'delete-window)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-2") 'split-window-below)
(global-set-key (kbd "C-3") 'split-window-right)
(global-set-key (kbd "C-4") ctl-x-4-map)
(global-set-key (kbd "C-5") ctl-x-5-map)
(global-set-key (kbd "C-6") '2C-command)
;(global-set-key (kbd "C-7") ) UNDEFINED
;(global-set-key C-x 8 is a complex translation map not loaded by default.
;(global-set-key (kbd "C-9") ) UNDEFINED

(global-set-key [C-tab] 'other-window)
(global-set-key [C-S-iso-lefttab] (lambda () (interactive) (other-window -1)))
(global-set-key [M-right] 'next-buffer)
(global-set-key [M-left] 'previous-buffer)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(global-set-key (kbd "M-`") 'jump-to-mark)
(global-set-key (kbd "C-'") 'push-mark-no-activate)

;; ;; Load VIP and move some keys around for vim-like movement

;; (load "vip")
;; (global-set-key "\C-z" 'vip-change-mode-to-vi)

;; ; (backward-char) C-b <- C-h is the help map (help-command)
;; ; (next-line) C-n     <- C-j is (newline-and-indent)
;; ; (previous-line) C-p <- C-k is (kill-line)
;; ; (forward-char) C-f  <- C-l is (recenter-top-bottom)

;; (global-set-key "\C-b" 'help-command)
;; (global-set-key "\C-n" 'newline-and-indent)
;; (global-set-key "\C-p" 'kill-line)
;; (global-set-key "\C-f" 'recenter-top-bottom)

;; (global-set-key "\C-h" 'backward-char)
;; (global-set-key "\C-j" 'next-line)
;; (global-set-key "\C-k" 'previous-line)
;; (global-set-key "\C-l" 'forward-char)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-mode-hook (quote (turn-on-haskell-simple-indent)))
 '(prolog-indent-width 2)
 '(prolog-system (quote swi))
 '(recenter-positions (quote (bottom middle top)))
 '(scheme-program-name "racket"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:inherit (shadow default) :foreground "dim gray"))))
 '(mode-line ((t (:foreground "white"))))
 '(mode-line-highlight ((t nil)))
 '(mode-line-inactive ((t (:inherit mode-line :foreground "dim gray" :weight light))))
 '(shm-current-face ((t (:background "gray20"))))
 '(shm-quarantine-face ((t (:background "black"))))
 '(show-paren-match ((t (:background "gray20")))))
