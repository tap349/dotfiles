;;----------------------------------------------------------
;; System
;;----------------------------------------------------------

(setq inhibit-startup-message t)
;; Turn off all alarms (ring-bell and visible-bell)
(setq ring-bell-function 'ignore)

;; https://emacsredux.com/blog/2020/12/04/maximize-the-emacs-frame-on-startup
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

(electric-pair-mode 1)

;; https://stackoverflow.com/a/30900018
(setq vc-follow-symlinks t)

;; http://www.gonsie.com/blorg/tab-bar.html
(tab-bar-mode 1)
(setq tab-bar-close-button-show nil)
(setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))

;;----------------------------------------------------------
;; Appearance
;;----------------------------------------------------------

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(setq column-number-mode t)

;; https://www.emacswiki.org/emacs/SetFonts
(set-face-attribute 'default nil :font "Input-15")
;;(set-face-attribute 'region nil :background "#A7D2E8")
;;(set-face-attribute 'region nil :foreground "#FFFFFF")
;; https://stackoverflow.com/a/22951243/3632318
;;(set-face-attribute 'show-paren-match nil :background "#FBDE41")
;;(set-face-attribute 'show-paren-match nil :background "#CDCDFA")
;;(set-face-attribute 'show-paren-match nil :background "#D8B188")

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;;(load-theme 'spacemacs-light t)
;;(load-theme 'tango-plus t)
(load-theme 'aircon t)

;;----------------------------------------------------------
;; Keybindings
;;----------------------------------------------------------

;; https://www.emacswiki.org/emacs/DvorakKeyboard
(global-set-key [?\C-.] 'execute-extended-command)

(global-set-key (kbd "s-{") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "s-}") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "s-t") 'tab-bar-new-tab)
(global-set-key (kbd "s-w") 'tab-bar-close-tab)

;;----------------------------------------------------------
;; Packages
;;----------------------------------------------------------

;; https://github.com/bbatsov/projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
