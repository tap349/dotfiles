;;-----------------------------------------------------------------------------
;;
;; Startup
;;
;; https://www.emacswiki.org/emacs/OptimizingEmacsStartup
;;
;;-----------------------------------------------------------------------------

(setq inhibit-startup-message t)

;; https://emacsredux.com/blog/2020/12/04/maximize-the-emacs-frame-on-startup
;; (add-hook 'window-setup-hook 'toggle-frame-maximized t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; https://emacs.stackexchange.com/a/62959
(defun startup (_frame)
  (tab-bar-mode 1))

(add-hook 'after-make-frame-functions #'startup)

;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Lower threshold back to 10 MB (default is 800kB)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 10 1000 1000))))

;;-----------------------------------------------------------------------------
;;
;; System
;;
;;-----------------------------------------------------------------------------

;; https://stackoverflow.com/a/30900018
(setq vc-follow-symlinks t)

;; Don't use system clipboard for kill-ring-save, kill-region and yank
;; Use their clipboard-* counterparts for working with system clipboard
(setq select-enable-clipboard nil)

;; It's considered a good practice to have newline at the end of file
(setq require-final-newline t)

;; Automatically switch to help windows
(setq help-window-select t)

;; https://stackoverflow.com/a/151946
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))

(defalias 'yes-or-no-p 'y-or-n-p)

;;-----------------------------------------------------------------------------
;;
;; Scrolling
;;
;; https://www.emacswiki.org/emacs/SmoothScrolling
;;
;;-----------------------------------------------------------------------------

(setq scroll-step 1)
(setq scroll-margin 2)

(setq mouse-wheel-progressive-speed nil)

;;-----------------------------------------------------------------------------
;;
;; Look and feel
;;
;;-----------------------------------------------------------------------------

;; Turn off all alarms (ring-bell and visible-bell)
(setq ring-bell-function 'ignore)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(global-display-fill-column-indicator-mode 1)
(setq-default display-fill-column-indicator-column 82)

(setq column-number-mode t)
(setq show-paren-delay 0)

;; https://emacs.stackexchange.com/a/21865
(setq-default show-trailing-whitespace t)

;; nowrap
(setq-default truncate-lines 1)

;;-----------------------------------------------------------------------------
;; Theme
;;-----------------------------------------------------------------------------

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;; (load-theme 'tango-plus t)
(load-theme 'aircon t)

;; https://emacs.stackexchange.com/a/69091
(set-face-foreground 'vertical-border "#D8D8DE")

;; https://www.emacswiki.org/emacs/SetFonts
(set-face-attribute 'default nil :font "Input-15")
;; (set-face-attribute 'region nil :background "#BFE2EF")
;; https://stackoverflow.com/a/22951243/3632318
;; (set-face-attribute 'show-paren-match nil :background "#FBDE41")
;; (set-face-attribute 'show-paren-match nil :background "#CDCDFA")
;; (set-face-attribute 'show-paren-match nil :background "#D8B188")

;;-----------------------------------------------------------------------------
;;
;; Editing
;;
;;-----------------------------------------------------------------------------

;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Replacing-highlighted-text.html
;; Replace selected region with inserted text
(delete-selection-mode 1)

(electric-pair-mode 1)

(defun my/electric-pair-inhibit-predicate (char)
  (minibufferp))

(setq electric-pair-delete-adjacent-pairs t)
(setq electric-pair-inhibit-predicate 'my/electric-pair-inhibit-predicate)

;; https://stackoverflow.com/a/1819405/3632318
;; Use whitespaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

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
;; (keyboard-translate ?\C-u ?\C-x)
;; (keyboard-translate ?\C-x ?\C-u)

;; https://www.emacswiki.org/emacs/DvorakKeyboard
;;
;; Define key in evil-normal-state-map as well
;; for it to work in insert and emacs states
(global-set-key [?\C-.] 'execute-extended-command)

(global-set-key (kbd "s-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "s-x") 'clipboard-kill-region)
(global-set-key (kbd "s-v") 'clipboard-yank)

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
;;
;; NOTE: evil package should come first so that other packages can define
;;       their keybindings in evil state keymaps and use leader key
;;-----------------------------------------------------------------------------

;; https://emacs.stackexchange.com/a/41701
;; Use Emacs keybindings in insert state
(setq evil-disable-insert-state-bindings t)
;; https://stackoverflow.com/a/18851955
(setq evil-want-C-u-scroll t)
;; Applies to shifting operators >> and <<
(setq evil-shift-width 2)

(evil-mode 1)

;; https://www.reddit.com/r/emacs/comments/n1pibp/comment/gwei7fw
(evil-set-undo-system 'undo-redo)

;; https://www.reddit.com/r/emacs/comments/345by9
;; https://emacs.stackexchange.com/a/58846/39266
;; https://github.com/emacs-evil/evil/blob/master/evil-search.el#L672
;;
;; evil-search creates overlay to highlight search results with priority 1000
;; which is higher than priority of region overlay (100) => highlighted search
;; results are always on top of region selection
(evil-select-search-module 'evil-search-module 'evil-search)

(evil-set-leader 'normal (kbd ","))
(evil-set-leader 'visual (kbd ","))

;; -------------------- insert state ------------------------------------------

(defun my/insert-tab-or-complete ()
  (interactive)
  (let ((chr (preceding-char)))
    ;; if beginning of line or preceding character is whitespace
    (if (or (bolp) (= chr 32))
        ;; insert tab
        (tab-to-tab-stop)
      ;; else complete
      (evil-complete-next))))

;; https://emacs.stackexchange.com/a/62011
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "RET") 'comment-indent-new-line)
(define-key evil-insert-state-map (kbd "TAB") 'my/insert-tab-or-complete)

;; -------------------- normal state ------------------------------------------

;; https://stackoverflow.com/a/14189981
(defun my/insert-newline-below ()
  (interactive)
  (end-of-line)
  (newline))

(defun my/insert-newline-above ()
  (interactive)
  (beginning-of-line)
  (open-line 1))

;; https://emacs.stackexchange.com/a/72123/39266
(defun my/insert-whitespace ()
  (interactive)
  (insert " "))

;; Continues comment on next line unlike evil-open-below
(defun my/evil-open-below ()
  (interactive)
  (evil-append-line 1)
  (comment-indent-new-line))

;; https://emacs.stackexchange.com/a/40823
(defun my/evil-window-split ()
  (interactive)
  (evil-window-split)
  (balance-windows)
  (other-window 1))

(defun my/evil-window-vsplit ()
  (interactive)
  (evil-window-vsplit)
  (balance-windows)
  (other-window 1))

(defun my/keyboard-quit ()
  (interactive)
  (evil-ex-nohighlight)
  (keyboard-quit))

(define-key evil-normal-state-map (kbd "C-g") 'my/keyboard-quit)
(define-key evil-normal-state-map (kbd "C-.") 'execute-extended-command)

(define-key evil-normal-state-map (kbd "TAB") 'save-buffer)
(define-key evil-normal-state-map (kbd "C-o") 'evil-switch-to-windows-last-buffer)

(define-key evil-normal-state-map (kbd "H") 'evil-first-non-blank)
(define-key evil-normal-state-map (kbd "L") 'evil-last-non-blank)

(define-key evil-normal-state-map (kbd "RET") 'my/insert-newline-below)
(define-key evil-normal-state-map (kbd "S-<return>") 'my/insert-newline-above)
(define-key evil-normal-state-map (kbd "SPC") 'my/insert-whitespace)
(define-key evil-normal-state-map (kbd "o") 'my/evil-open-below)

(define-key evil-normal-state-map (kbd "s-[") 'evil-jump-backward)
(define-key evil-normal-state-map (kbd "s-]") 'evil-jump-forward)

(define-key evil-normal-state-map (kbd "C-M-f") 'evil-jump-item)
(define-key evil-normal-state-map (kbd "C-M-b") 'evil-jump-item)

(define-key evil-normal-state-map (kbd "C-w C-s") 'my/evil-window-split)
(define-key evil-normal-state-map (kbd "C-w s") 'my/evil-window-split)
(define-key evil-normal-state-map (kbd "C-w C-v") 'my/evil-window-vsplit)
(define-key evil-normal-state-map (kbd "C-w v") 'my/evil-window-vsplit)

(define-key evil-normal-state-map (kbd "C-w C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-w C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-w C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-w C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-w x") 'window-swap-states)

(define-key evil-normal-state-map (kbd "S-<right>") 'evil-window-increase-width)
(define-key evil-normal-state-map (kbd "S-<left>") 'evil-window-decrease-width)
(define-key evil-normal-state-map (kbd "S-<up>") 'evil-window-increase-height)
(define-key evil-normal-state-map (kbd "S-<down>") 'evil-window-decrease-height)

(define-key evil-normal-state-map (kbd "<leader>t") 'dired-jump)

;; https://github.com/noctuid/evil-guide#binding-keys-to-keys-keyboard-macros
(evil-define-key 'normal 'global
	"gp" "`[v`]")

;; -------------------- visual state ------------------------------------------

;; Bind to keyboard-quit explicitly - otherwise binding for normal state is used
(define-key evil-visual-state-map (kbd "C-g") 'keyboard-quit)
(define-key evil-visual-state-map (kbd "C-.") 'execute-extended-command)

(define-key evil-visual-state-map (kbd "C-s") 'sort-lines)

(define-key evil-visual-state-map (kbd "H") 'evil-first-non-blank)
(define-key evil-visual-state-map (kbd "L") 'evil-last-non-blank)

;; -------------------- replace state ------------------------------------------

(define-key evil-replace-state-map (kbd "C-g") 'evil-normal-state)

;;-----------------------------------------------------------------------------
;; avy
;;-----------------------------------------------------------------------------

(define-key evil-normal-state-map (kbd "<leader>w") 'avy-goto-word-1)

;;-----------------------------------------------------------------------------
;; cider
;;-----------------------------------------------------------------------------

(evil-define-key 'normal 'cider-repl-mode-map
  ;; Close *cider-error* windows with q
	"q" 'cider-popup-buffer-quit-function)

;;-----------------------------------------------------------------------------
;; clojure-mode
;;-----------------------------------------------------------------------------

;; https://stackoverflow.com/a/4200242
;; Fix indentation for failjure library (indent like `let`)
(put 'f/attempt-all 'clojure-indent-function 1)
(put 'f/try-all 'clojure-indent-function 1)
(put 'f/when-failed 'clojure-indent-function 1)
(put 'f/when-let-failed? 'clojure-indent-function 1)

(put 'as-> 'clojure-indent-function 1)

;;-----------------------------------------------------------------------------
;; dired-mode (built-in)
;;-----------------------------------------------------------------------------

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "u") 'dired-up-directory)))

;;-----------------------------------------------------------------------------
;; dockerfile-mode
;;-----------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-mode))

;;-----------------------------------------------------------------------------
;; evil-nerd-commenter
;;-----------------------------------------------------------------------------

(global-set-key (kbd "<leader>SPC") 'evilnc-comment-or-uncomment-lines)

;;-----------------------------------------------------------------------------
;; evil-surround
;;-----------------------------------------------------------------------------

(global-evil-surround-mode 1)

;;-----------------------------------------------------------------------------
;; evil-visualstar
;;-----------------------------------------------------------------------------

(global-evil-visualstar-mode 1)

(defun my/evil-visualstar-asterisk ()
  (interactive)
  (when (region-active-p)
    (evil-visualstar/begin-search (region-beginning) (region-end) t)
    (evil-ex-search-previous)))

(define-key evil-normal-state-map (kbd "z*") 'my/evil-visualstar-asterisk)

;;-----------------------------------------------------------------------------
;; counsel (ivy / counsel / swiper)
;;-----------------------------------------------------------------------------

(ivy-mode 1)

;; https://github.com/junegunn/fzf#respecting-gitignore
;; For counsel-fzf
(setenv
 "FZF_DEFAULT_COMMAND"
 "fd --type f --strip-cwd-prefix -H -I --exclude .git --exclude target --exclude .cpcache")

;; See g:ackprg in vimrc
;; --no-line-number breaks syntax highlighting
;; --field-match-separator breaks navigation to selected line
;;
;; UPDATE: git grep works better with special characters
;;         (say, ripgrep breaks when searching for '->')
;; (setq
;;  counsel-git-grep-cmd-default
;;  "rg -FS --no-column --sort-files --vimgrep \"%s\"")

(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

(setq ivy-display-style 'fancy)
(setq ivy-height 15)
(setq ivy-count-format "")
(setq ivy-initial-inputs-alist nil)
(setq ivy-on-del-error-function #'ignore)
(setq ivy-more-chars-alist '((counsel-grep . 2)
                             (counsel-git-grep . 2)
                             (t . 3)))

(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)

(define-key evil-normal-state-map (kbd "<leader>n") 'counsel-fzf)
(define-key evil-normal-state-map (kbd "<leader>/") 'counsel-git-grep)

;;-----------------------------------------------------------------------------
;; projectile
;;-----------------------------------------------------------------------------

(projectile-mode 1)

(setq projectile-completion-system 'ivy)
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
(defun my/tab-bar-tab-name-format-function (tab i)
  (propertize
   (concat " " (alist-get 'name tab) " ")
   'face (funcall tab-bar-tab-face-function tab)))

(setq tab-bar-tab-name-format-function #'my/tab-bar-tab-name-format-function)

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
   '(yaml-mode json-mode counsel avy dockerfile-mode magit evil-visualstar evil-nerd-commenter rainbow-delimiters evil-surround evil projectile cider)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fill-column-indicator ((t (:foreground "#DEE4EF"))))
 '(ivy-minibuffer-match-face-1 ((t nil)))
 '(ivy-minibuffer-match-face-2 ((t (:inherit 'lazy-highlight))))
 '(ivy-minibuffer-match-face-3 ((t (:inherit 'lazy-highlight))))
 '(ivy-minibuffer-match-face-4 ((t (:inherit 'lazy-highlight))))
 '(swiper-match-face-1 ((t nil)))
 '(swiper-match-face-2 ((t (:inherit 'isearch))))
 '(swiper-match-face-3 ((t (:inherit 'isearch))))
 '(swiper-match-face-4 ((t (:inherit 'isearch)))))
