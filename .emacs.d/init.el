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

;; https://lists.gnu.org/archive/html/help-gnu-emacs/2016-05/msg00148.html
;;(if (daemonp)
;;    (add-hook 'after-make-frame-functions #'startup)
;;    (add-hook 'window-setup-hook #'startup))

;; It's considered a good practice to have newline at the end of file
(setq require-final-newline t)

;; Automatically switch to help windows
(setq help-window-select t)

;;-----------------------------------------------------------------------------
;;
;; Appearance
;;
;;-----------------------------------------------------------------------------

(setq column-number-mode t)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

;; http://www.gonsie.com/blorg/tab-bar.html
(tab-bar-mode 1)
(setq tab-bar-close-button-show nil)
(setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))

;; https://emacs.stackexchange.com/a/21865
(setq-default show-trailing-whitespace t)

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
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
;; Pressing TAB autoindents current line
;; (even if point is not at the beginning of the line)
(setq indent-line-function 'insert-tab)

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

;; https://emacs.stackexchange.com/a/40823
;; See also my-evil-window-split
(defun my-split-window-below ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

;; See also my-evil-window-vsplit
(defun my-split-window-right ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

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

(global-set-key (kbd "C-x 2") 'my-split-window-below)
(global-set-key (kbd "C-x 3") 'my-split-window-right)

(global-set-key (kbd "s-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "s-x") 'clipboard-kill-region)
(global-set-key (kbd "s-v") 'clipboard-yank)

(global-set-key (kbd "s-{") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "s-}") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "s-t") 'tab-bar-new-tab)
(global-set-key (kbd "s-w") 'tab-bar-close-tab)

;;-----------------------------------------------------------------------------
;;
;; Packages
;;
;;-----------------------------------------------------------------------------

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
(setq evil-flash-delay 5)

;; https://www.reddit.com/r/emacs/comments/n1pibp/comment/gwei7fw
(evil-set-undo-system 'undo-redo)

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

(evil-set-leader 'normal (kbd ","))

;; -------------------- insert state --------------------

;; https://emacs.stackexchange.com/a/62011
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
;; Now TAB autoindents current line - see indent-line-function above
;;(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)

;; -------------------- normal state --------------------

(define-key evil-normal-state-map (kbd "C-.") 'execute-extended-command)

(define-key evil-normal-state-map (kbd "H") 'evil-first-non-blank)
(define-key evil-normal-state-map (kbd "L") 'evil-last-non-blank)
(define-key evil-normal-state-map (kbd "TAB") 'save-buffer)

(define-key evil-normal-state-map (kbd "RET") 'my-insert-newline-below)
(define-key evil-normal-state-map (kbd "<S-return>") 'my-insert-newline-above)
(define-key evil-normal-state-map (kbd "SPC") 'my-insert-whitespace)

(define-key evil-normal-state-map (kbd "C-w C-s") 'my-evil-window-split)
(define-key evil-normal-state-map (kbd "C-w s") 'my-evil-window-split)
(define-key evil-normal-state-map (kbd "C-w C-v") 'my-evil-window-vsplit)
(define-key evil-normal-state-map (kbd "C-w v") 'my-evil-window-vsplit)

(define-key evil-normal-state-map (kbd "C-w C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-w C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-w C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-w C-j") 'evil-window-down)

(define-key evil-normal-state-map (kbd "<S-right>") 'evil-window-increase-width)
(define-key evil-normal-state-map (kbd "<S-left>") 'evil-window-decrease-width)
(define-key evil-normal-state-map (kbd "<S-up>") 'evil-window-increase-height)
(define-key evil-normal-state-map (kbd "<S-down>") 'evil-window-decrease-height)

(define-key evil-normal-state-map (kbd "<leader>t") 'dired-jump)

;; https://github.com/noctuid/evil-guide#binding-keys-to-keys-keyboard-macros
(evil-define-key 'normal 'global
	"gp" "`[v`]")

;; -------------------- visual state --------------------

(define-key evil-visual-state-map (kbd "C-.") 'execute-extended-command)
(define-key evil-visual-state-map (kbd "H") 'evil-first-non-blank)
(define-key evil-visual-state-map (kbd "L") 'evil-last-non-blank)

;;-----------------------------------------------------------------------------
;; evil-visualstar
;;-----------------------------------------------------------------------------

(global-evil-visualstar-mode 1)

;;-----------------------------------------------------------------------------
;; projectile
;;-----------------------------------------------------------------------------

;; https://github.com/bbatsov/projectile
(projectile-mode 1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)

;;-----------------------------------------------------------------------------
;;
;; Managed by package.el
;;
;;-----------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(evil-visualstar evil spacemacs-theme projectile cider)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
