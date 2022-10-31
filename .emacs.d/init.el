;; https://www.emacswiki.org/emacs/DvorakKeyboard
(global-set-key [?\C-.] 'execute-extended-command)

(setq inhibit-startup-message t)
;; Turn off all alarms (ring-bell and visible-bell)
(setq ring-bell-function 'ignore)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

;; https://www.emacswiki.org/emacs/SetFonts
(set-face-attribute 'default nil :font "Input-15")
;;(set-face-attribute 'region nil :background "#A7D2E8")
(set-face-attribute 'region nil :foreground "#FFFFFF")
;; https://stackoverflow.com/a/22951243/3632318
;;(set-face-attribute 'show-paren-match nil :background "#FBDE41")
;;(set-face-attribute 'show-paren-match nil :background "#CDCDFA")
(set-face-attribute 'show-paren-match nil :background "#D8B188")

;;(load-theme 'modus-operandi t)
;; dark solarized
;;(load-theme 'deeper-blue t)
;; looks like github theme in vim
;;(load-theme 'tango t)

(electric-pair-mode 1)

;; https://github.com/bbatsov/projectile
;; (custom-set-variables
;; '(package-selected-packages '(projectile)))

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map):
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d916b686ba9f23a46ee9620c967f6039ca4ea0e682c1b9219450acee80e10e40" default))
 '(package-selected-packages '(projectile)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
