;;-----------------------------------------------------------------------------
;;
;; System
;;
;;-----------------------------------------------------------------------------

(setq inhibit-startup-message t)

;; Turn off all alarms (ring-bell and visible-bell)
(setq ring-bell-function 'ignore)

;; https://emacsredux.com/blog/2020/12/04/maximize-the-emacs-frame-on-startup
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; https://stackoverflow.com/a/30900018
(setq vc-follow-symlinks t)

;; Don't use system clipboard for kill-ring-save, kill-region and yank
;; Use their clipboard-* counterparts for working with system clipboard
(setq select-enable-clipboard nil)

;; It's considered a good practice to have newline at the end of file
(setq require-final-newline t)

;; Automatically switch to help windows
(setq help-window-select t)

;;-----------------------------------------------------------------------------
;;
;; Appearance
;;
;;-----------------------------------------------------------------------------

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(setq column-number-mode t)

;; https://emacs.stackexchange.com/a/21865
(setq-default show-trailing-whitespace t)

;; nowrap
(setq-default truncate-lines 1)

(setq show-paren-delay 0)

(global-display-fill-column-indicator-mode 1)
(setq-default display-fill-column-indicator-column 82)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;;(load-theme 'spacemacs-light t)
;;(load-theme 'tango-plus t)
(load-theme 'aircon t)

;; https://emacs.stackexchange.com/a/69091
(set-face-foreground 'vertical-border "#BABACA")

;; https://www.emacswiki.org/emacs/SetFonts
(set-face-attribute 'default nil :font "Input-15")
;;(set-face-attribute 'region nil :background "#A7D2E8")
;;(set-face-attribute 'region nil :foreground "#FFFFFF")
;; https://stackoverflow.com/a/22951243/3632318
;;(set-face-attribute 'show-paren-match nil :background "#FBDE41")
;;(set-face-attribute 'show-paren-match nil :background "#CDCDFA")
;;(set-face-attribute 'show-paren-match nil :background "#D8B188")

;;-----------------------------------------------------------------------------
;;
;; Editing
;;
;;-----------------------------------------------------------------------------

;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Replacing-highlighted-text.html
;; Replace selected region with inserted text
(delete-selection-mode 1)

(electric-pair-mode 1)
(setq electric-pair-delete-adjacent-pairs t)

;; https://stackoverflow.com/a/1819405/3632318
;; Use whitespaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;;-----------------------------------------------------------------------------
;;
;; Search
;;
;; https://www.reddit.com/r/emacs/comments/e2uaaw/evilsearchwordforwardbackward_doesnt_highlight
;;
;; Matches are not highlighted immediately after calling evil-search-next -
;; that's how isearch works, IDK how to circumvent this behaviour now
;;
;;-----------------------------------------------------------------------------

;; https://www.emacswiki.org/emacs/InteractivelyDoThings
;; https://www.masteringemacs.org/article/introduction-to-ido-mode
(ido-mode 1)

(setq lazy-highlight-initial-delay 0)

;;-----------------------------------------------------------------------------
;;
;; Registers (C-x r j)
;;
;; https://stackoverflow.com/a/12558095/3632318
;;
;;-----------------------------------------------------------------------------

(set-register ?e (cons 'file user-init-file))
(set-register ?z (cons 'file (substitute-in-file-name "${ZDOTDIR}/.zshenv")))

;;-----------------------------------------------------------------------------
;;
;; Keybindings
;;
;;-----------------------------------------------------------------------------

;; https://www.emacswiki.org/emacs/DvorakKeyboard
;; NOTE: keyboard-translate doesn’t work in daemon mode
;; UPDATE: Now it's not important when using evil-mode
;;(keyboard-translate ?\C-u ?\C-x)
;;(keyboard-translate ?\C-x ?\C-u)

;; https://www.emacswiki.org/emacs/DvorakKeyboard
;;
;; Define key in evil-normal-state-map as well
;; for it to work in insert and emacs states
(global-set-key [?\C-.] 'execute-extended-command)

(global-set-key (kbd "s-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "s-x") 'clipboard-kill-region)
(global-set-key (kbd "s-v") 'clipboard-yank)

;; https://stackoverflow.com/a/455703
;; Don't change default-directory on find-file
(defun my-find-file ()
  (interactive)
  (setq saved-default-directory default-directory)
  (ido-find-file)
  (setq default-directory saved-default-directory))

(global-set-key (kbd "C-x C-f") 'my-find-file)

;;-----------------------------------------------------------------------------
;;
;; Packages
;;
;;-----------------------------------------------------------------------------

;; https://melpa.org/#/getting-started
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;-----------------------------------------------------------------------------
;; evil
;;-----------------------------------------------------------------------------

;; https://emacs.stackexchange.com/a/41701
;; Use Emacs keybindings in insert state
(setq evil-disable-insert-state-bindings t)
;; https://stackoverflow.com/a/18851955
(setq evil-want-C-u-scroll t)
;; Applies to shifting operators >> and <<
(setq evil-shift-width 2)

(evil-mode 1)

;; Highlight search results for this period
(setq evil-flash-delay 999)

;; https://www.reddit.com/r/emacs/comments/n1pibp/comment/gwei7fw
(evil-set-undo-system 'undo-redo)

;; https://emacs.stackexchange.com/a/20717/39266
;; Search for symbol, not for word when using *
;; (foo-bar: foo - word, foo-bar - symbol)
(setq-default evil-symbol-word-search t)

;; https://stackoverflow.com/a/14189981
(defun my-insert-newline-below ()
  (interactive)
  (end-of-line)
  (newline))

(defun my-insert-newline-above ()
  (interactive)
  (beginning-of-line)
  (open-line 1))

;; https://emacs.stackexchange.com/a/72123/39266
(defun my-insert-whitespace ()
  (interactive)
  (insert " "))

;; https://emacs.stackexchange.com/a/40823
(defun my-evil-window-split ()
  (interactive)
  (evil-window-split)
  (balance-windows)
  (other-window 1))

(defun my-evil-window-vsplit ()
  (interactive)
  (evil-window-vsplit)
  (balance-windows)
  (other-window 1))

(defun my-unhighlight-regexp-all ()
  (interactive)
  (unhighlight-regexp t))

(evil-set-leader 'normal (kbd ","))
(evil-set-leader 'visual (kbd ","))

;; -------------------- insert state ------------------------------------------

;; https://emacs.stackexchange.com/a/62011
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
;;(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
(define-key evil-insert-state-map (kbd "TAB") 'evil-complete-next)

;; -------------------- normal state ------------------------------------------

(define-key evil-normal-state-map (kbd "C-.") 'execute-extended-command)

(define-key evil-normal-state-map (kbd "H") 'evil-first-non-blank)
(define-key evil-normal-state-map (kbd "L") 'evil-last-non-blank)
(define-key evil-normal-state-map (kbd "TAB") 'save-buffer)

(define-key evil-normal-state-map (kbd "RET") 'my-insert-newline-below)
(define-key evil-normal-state-map (kbd "S-<return>") 'my-insert-newline-above)
(define-key evil-normal-state-map (kbd "SPC") 'my-insert-whitespace)

(define-key evil-normal-state-map (kbd "C-M-f") 'evil-jump-item)
(define-key evil-normal-state-map (kbd "C-M-b") 'evil-jump-item)

(define-key evil-normal-state-map (kbd "C-w C-s") 'my-evil-window-split)
(define-key evil-normal-state-map (kbd "C-w s") 'my-evil-window-split)
(define-key evil-normal-state-map (kbd "C-w C-v") 'my-evil-window-vsplit)
(define-key evil-normal-state-map (kbd "C-w v") 'my-evil-window-vsplit)

(define-key evil-normal-state-map (kbd "C-w C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-w C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-w C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-w C-j") 'evil-window-down)

(define-key evil-normal-state-map (kbd "S-<right>") 'evil-window-increase-width)
(define-key evil-normal-state-map (kbd "S-<left>") 'evil-window-decrease-width)
(define-key evil-normal-state-map (kbd "S-<up>") 'evil-window-increase-height)
(define-key evil-normal-state-map (kbd "S-<down>") 'evil-window-decrease-height)

(define-key evil-normal-state-map (kbd "<leader>t") 'dired-jump)
(define-key evil-normal-state-map (kbd "<leader>n") 'projectile-find-file)
(define-key evil-normal-state-map (kbd "<leader>hh") 'highlight-symbol-at-point)
(define-key evil-normal-state-map (kbd "<leader>hu") 'my-unhighlight-regexp-all)

;; https://github.com/noctuid/evil-guide#binding-keys-to-keys-keyboard-macros
(evil-define-key 'normal 'global
	"gp" "`[v`]")

(evil-define-key 'normal 'cider-repl-mode-map
  ;; Close *cider-error* windows with q
	"q" 'cider-popup-buffer-quit-function)

;; -------------------- visual state ------------------------------------------

(define-key evil-visual-state-map (kbd "C-.") 'execute-extended-command)
(define-key evil-visual-state-map (kbd "H") 'evil-first-non-blank)
(define-key evil-visual-state-map (kbd "L") 'evil-last-non-blank)

(define-key evil-visual-state-map (kbd "<leader>hh") 'highlight-regexp)

;;-----------------------------------------------------------------------------
;; evil-surround
;;-----------------------------------------------------------------------------

(global-evil-surround-mode 1)

;;-----------------------------------------------------------------------------
;; evil-visualstar
;;
;; https://stackoverflow.com/a/203026/3632318
;;
;; There's a similar built-in behaviour to search for words:
;; C-s C-w (press C-w again to include more words)
;; => you can press C-w multiple times to select the whole symbol
;;
;;-----------------------------------------------------------------------------

(global-evil-visualstar-mode 1)

;;-----------------------------------------------------------------------------
;; helm
;;-----------------------------------------------------------------------------

(helm-mode 1)

;; https://emacs.stackexchange.com/a/9446
;; Disable helm for cd command
(add-to-list 'helm-completing-read-handlers-alist '(cd))

(set-face-attribute 'helm-selection nil :background "#ECE2D8")

;;-----------------------------------------------------------------------------
;; projectile
;;-----------------------------------------------------------------------------

(projectile-mode 1)

(setq projectile-completion-system 'helm)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)

;;-----------------------------------------------------------------------------
;; rainbow-delimiters
;;-----------------------------------------------------------------------------

(rainbow-delimiters-mode 1)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;-----------------------------------------------------------------------------
;; tab-bar (built-in)
;;-----------------------------------------------------------------------------

;; http://www.gonsie.com/blorg/tab-bar.html
(tab-bar-mode 1)
(setq tab-bar-close-button-show nil)
(setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
;; ZWSP is used to prevent last tab from filling all available space
(setq tab-bar-separator "​")

;; https://christiantietze.de/posts/2022/02/emacs-tab-bar-numbered-tabs/
(defun my-tab-bar-tab-name-format-function (tab i)
  (propertize
   (concat " " (alist-get 'name tab) " ")
   'face (funcall tab-bar-tab-face-function tab)))

(setq tab-bar-tab-name-format-function #'my-tab-bar-tab-name-format-function)

(global-set-key (kbd "s-{") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "s-}") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "s-t") 'tab-bar-new-tab)
(global-set-key (kbd "s-w") 'tab-bar-close-tab)
(global-set-key (kbd "C-<backspace>") 'tab-bar-close-tab)

;;-----------------------------------------------------------------------------
;;
;; Managed by package.el and customize-group
;;
;;-----------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(rainbow-delimiters evil-surround helm evil-visualstar evil spacemacs-theme projectile cider)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fill-column-indicator ((t (:foreground "gray83")))))
