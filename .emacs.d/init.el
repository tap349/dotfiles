;;-----------------------------------------------------------------------------
;;
;; Package management
;;
;;-----------------------------------------------------------------------------

;; https://github.com/radian-software/straight.el#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name
                       "straight/repos/straight.el/bootstrap.el"
                       user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent
         'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;;-----------------------------------------------------------------------------
;;
;; Startup
;;
;; https://www.emacswiki.org/emacs/OptimizingEmacsStartup
;;
;;-----------------------------------------------------------------------------

(setq inhibit-startup-message t)
;; (setq warning-minimum-level :emergency)

;; https://emacsredux.com/blog/2020/12/04/maximize-the-emacs-frame-on-startup
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

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
(setq pixel-scroll-precision-mode 1)

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
(set-face-attribute 'default nil :font "Input-15")
(set-face-attribute 'default nil :font "Input-15")
(set-face-foreground 'fill-column-indicator "#DEE4EF")
(set-face-foreground 'vertical-border "#D8D8DE")

;;-----------------------------------------------------------------------------
;;
;; Editing
;;
;; Use narrowing to edit part of buffer:
;; - "C-x n d" - narrow-to-defun
;; - "C-x n n" - narrow-to-region
;; - "C-x n w" - widen
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

;; Style for comment-region command
;; Used by my/toggle-comment
(setq comment-style 'indent)

;;-----------------------------------------------------------------------------
;;
;; Registers (C-x r j)
;;
;; https://stackoverflow.com/a/12558095/3632318
;;
;;-----------------------------------------------------------------------------

;; Register ?e is overwritten by nREPL server (started by cider-jack-in)
(set-register ?c (cons 'file user-init-file))
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

;; Install it first to make :delight option work
(use-package delight
  :straight t)

(use-package auto-package-update
  :straight t)

(use-package autorevert
  :straight t
  :delight auto-revert-mode)

;; For some reason <return> and RET keys are not the same: keybinding for
;; <return> key in evil-normal-state-map (insert newline below) is also
;; active in dired-mode but keybinding for RET is not.
;; Most likely RET key has lower precedence and can be overriden by other
;; modes while <return> can't be
;; => use RET and TAB where possible to allow other modes to override them
(use-package evil
  :straight t
  :demand t
  :init
  ;; https://emacs.stackexchange.com/a/41701
  ;; Use Emacs keybindings in insert state
  (setq evil-disable-insert-state-bindings t)
  ;; https://stackoverflow.com/a/18851955
  (setq evil-want-C-u-scroll t)
  ;; Applies to shifting operators >> and <<
  (setq evil-shift-width 2)

  (defun my/insert-tab-or-complete ()
    (interactive)
    (let ((chr (preceding-char)))
      ;; if beginning of line or preceding character is whitespace
      (if (or (bolp) (= chr 32))
          ;; insert tab
          (tab-to-tab-stop)
        ;; else complete
        (company-complete))))

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

  ;; https://stackoverflow.com/a/9697222/3632318
  (defun my/toggle-comment ()
    (interactive)
    (let (beg end)
      (if (region-active-p)
          (setq beg (region-beginning) end (region-end))
        (setq beg (line-beginning-position) end (line-end-position)))
      (comment-or-uncomment-region beg end)))

  :config
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

  :bind
  (:map evil-insert-state-map
        ;; https://emacs.stackexchange.com/a/62011
        ("C-g" . evil-normal-state)
        ("RET" . comment-indent-new-line)
        ("TAB" . my/insert-tab-or-complete))

  (:map evil-normal-state-map
        ("C-g" . my/keyboard-quit)
        ("C-." . execute-extended-command)

        ("TAB" . save-buffer)
        ("C-o" . evil-switch-to-windows-last-buffer)

        ("H" . evil-first-non-blank)
        ("L" . evil-last-non-blank)

        ("RET" . my/insert-newline-below)
        ;; S-RET translates to just RET
        ("S-<return>" . my/insert-newline-above)
        ("SPC" . my/insert-whitespace)
        ("o" . my/evil-open-below)

        ("s-[" . evil-jump-backward)
        ("s-]" . evil-jump-forward)

        ("C-M-f" . evil-jump-item)
        ("C-M-b" . evil-jump-item)

        ("C-w C-s" . my/evil-window-split)
        ("C-w s" . my/evil-window-split)
        ("C-w C-v" . my/evil-window-vsplit)
        ("C-w v" . my/evil-window-vsplit)

        ("C-w C-l" . evil-window-right)
        ("C-w C-h" . evil-window-left)
        ("C-w C-k" . evil-window-up)
        ("C-w C-j" . evil-window-down)
        ("C-w x" . window-swap-states)

        ("S-<right>" . evil-window-increase-width)
        ("S-<left>" . evil-window-decrease-width)
        ("S-<up>" . evil-window-increase-height)
        ("S-<down>" . evil-window-decrease-height)

        ("<backspace>" . evil-toggle-fold)
        ("<leader>SPC" . my/toggle-comment)

        ("<leader>t" . dired-jump))

  (:map evil-visual-state-map
        ;; Bind to keyboard-quit explicitly -
        ;; otherwise binding for normal state is used
        ("C-g" . keyboard-quit)
        ("C-." . execute-extended-command)

        ("C-s" . sort-lines)

        ("H" . evil-first-non-blank)
        ("L" . evil-last-non-blank))

  (:map evil-replace-state-map
        ("C-g" . evil-normal-state)))

(use-package avy
  :straight t
  :after (evil)
  :bind
  (:map evil-normal-state-map
        ("<leader>w" . avy-goto-word-1)))

;; - "C-c C-x j j" - cider-jack-in
;; - "C-c C-d C-c" - cider-clojuredocs
;; - "C-c C-d C-w" - cider-clojuredocs-web
(use-package cider
  :straight t
  :delight " CIDER"
  :init
  (defun my/hide-trailing-whitespace ()
    (setq show-trailing-whitespace nil))

  :hook
  ((cider-test-report-mode . my/hide-trailing-whitespace)
   ;; For *cider-clojuredocs* buffer
   (cider-popup-buffer-mode . my/hide-trailing-whitespace))

  :bind
  (:map cider-repl-mode-map
        ;; Close *cider-error* window with q
        ("q" . cider-popup-buffer-quit-function)))

(use-package clojure-mode
  :straight t)

(use-package company
  :straight t
  :demand t
  :delight company-mode
  :custom
  ;; http://company-mode.github.io/manual/Customization.html#Customization
  ;; Disable automatic completion
  (company-idle-delay nil)
  (company-selection-wrap-around t)
  (company-require-match nil)

  ;; http://company-mode.github.io/manual/Frontends.html#Frontends
  (company-tooltip-align-annotations t)
  (company-tooltip-minimum 4)
  (company-tooltip-limit 8)
  (company-tooltip-width-grow-only t)
  (company-tooltip-margin 1)
  (company-format-margin-function 'company-vscode-light-icons-margin)

  :config
  (global-company-mode)

  :bind
  (:map company-active-map
        ;; Use <tab> instead of TAB to override other keybindings
        ("<tab>" . company-complete-common)))

;; ivy / counsel / swiper
(use-package counsel
  :straight t
  :delight ivy-mode
  :after (evil)
  :init
  ;; https://github.com/junegunn/fzf#respecting-gitignore
  ;; For counsel-fzf
  (setenv
   "FZF_DEFAULT_COMMAND"
   "fd --type f --strip-cwd-prefix -H -I \
    --exclude target \
    --exclude .git \
    --exclude .cpcache \
    --exclude .clj-kondo \
    --exclude .gradle")

  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)

  (ivy-display-style 'fancy)
  (ivy-height 15)
  (ivy-count-format "")
  (ivy-initial-inputs-alist nil)
  (ivy-on-del-error-function 'ignore)
  (ivy-more-chars-alist '((counsel-grep . 2)
                          (counsel-git-grep . 2)
                          (t . 3)))

  :config
  (ivy-mode 1)

  :bind
  (("C-s" . swiper-isearch)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("M-y" . counsel-yank-pop)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-x b" . ivy-switch-buffer)
   (:map evil-normal-state-map
         ("<leader>n" . counsel-fzf)
         ("<leader>/" . counsel-git-grep))))

;; - "(" - dired-hide-details-mode
(use-package dired
  :straight nil
  :bind
  (:map dired-mode-map
        ("p" . dired-up-directory)))

(use-package dired-subtree
  :straight t
  :demand t
  :custom
  (dired-subtree-use-backgrounds t)

  :custom-face
  (dired-subtree-depth-1-face ((t (:background "#F4F4F4"))))
  (dired-subtree-depth-2-face ((t (:background "#E4E4E4"))))
  (dired-subtree-depth-3-face ((t (:background "#D0D0D0"))))
  (dired-subtree-depth-4-face ((t (:background "#D0D0D0"))))
  (dired-subtree-depth-5-face ((t (:background "#D0D0D0"))))
  (dired-subtree-depth-6-face ((t (:background "#D0D0D0"))))

  :bind
  (:map dired-mode-map
        ("TAB" . dired-subtree-toggle)))

(use-package dockerfile-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-mode)))

;; Eglot automatically finds LSP server binaries in PATH:
;;
;; - clojure-mode => clojure-lsp
;; - kotlin-mode => kotlin-language-server
;;
;; - "C-]" - xref-find-definitions
;; - "M-?" - xref-find-references
(use-package eglot
  :straight nil
  :demand t
  :init
  (defun my/eglot-clojure-add-save-hooks ()
    ;; Calls cljfmt on current buffer
    (add-hook 'before-save-hook 'eglot-format-buffer))

  (defun my/show-flymake-eldoc-first ()
    ;; Show flymake diagnostics first
    (setq eldoc-documentation-functions
          (cons 'flymake-eldoc-function
                (remove 'flymake-eldoc-function
                        eldoc-documentation-functions)))
    ;; Show all eldoc feedback
    (setq eldoc-documentation-strategy 'eldoc-documentation-compose))

  :hook
  ((clojure-mode . eglot-ensure)
   (kotlin-mode . eglot-ensure)
   (clojure-mode . my/eglot-clojure-add-save-hooks)
   (eglot-managed-mode . my/show-flymake-eldoc-first))

  :custom
  (eglot-connect-timeout 60)
  ;; https://github.com/joaotavora/eglot/issues/334
  ;; Disable highlight at point feature
  (eglot-ignored-server-capabilities '(:documentHighlightProvider))

  :bind
  (:map eglot-mode-map
        ;; Keybindings used by lsp-mode by default
        ("s-l = =" . eglot-format-buffer)
        ("s-l a a" . eglot-code-actions)
        ("s-l g g" . xref-find-definitions)
        ("s-l g r" . xref-find-references)
        ("s-l r o" . eglot-code-action-organize-imports)
        ("s-l r r" . eglot-rename)))

(use-package eldoc
  :straight nil
  :delight eldoc-mode
  :custom
  (eldoc-echo-area-use-multiline-p nil))

(use-package eldoc-box
  :straight (eldoc-box :type git :host github :repo "tap349/eldoc-box")
  :after (evil)
  :init
  (defun my/hide-trailing-whitespace ()
    (setq show-trailing-whitespace nil))

  :hook
  ((eldoc-box-buffer . my/hide-trailing-whitespace))

  :custom
  (eldoc-box-max-pixel-height 600)
  (eldoc-box-max-pixel-width 800)

  :custom-face
  (eldoc-box-body ((t (:background "#F2F8FA"))))

  :bind
  (:map evil-normal-state-map
        ("C-n" . eldoc-box-eglot-toggle-help-at-point)))

(use-package evil-surround
  :straight t
  :demand t
  :config
  (global-evil-surround-mode 1))

(use-package evil-visualstar
  :straight t
  :demand t
  :after (evil)
  :init
  (defun my/evil-visualstar-asterisk ()
    (interactive)
    (when (region-active-p)
      (evil-visualstar/begin-search (region-beginning) (region-end) t)
      (evil-ex-search-previous)))

  :config
  (global-evil-visualstar-mode 1)

  :bind
  (:map evil-normal-state-map
        ("z*" . my/evil-visualstar-asterisk)))

;; hs-minor-mode can be enabled not in all major modes
;; Use evil-toggle-fold to toggle folding
(use-package hideshow
  :straight nil
  :delight hs-minor-mode
  :hook (prog-mode . hs-minor-mode))

(use-package jarchive
  :straight t
  :config
  (jarchive-setup))

(use-package json-mode
  :straight t)

(use-package kotlin-mode
  :straight t
  :bind
  ;; Unset default keybindings - REPL integration provided
  ;; by kotlin-mode is not very useful
  (:map kotlin-mode-map
        ("C-c C-z" . nil)
        ("C-c C-n" . nil)
        ("C-c C-r" . nil)
        ("C-c C-c" . nil)
        ("C-c C-b" . nil)))

(use-package markdown-mode
  :straight t
  :custom-face
  (markdown-code-face ((t (:font "Input-15"))))
  (markdown-inline-code-face ((t (:font "Input-15")))))

;; It allows counsel-fzf to search from project root regardless of current file
(use-package projectile
  :straight t
  :demand t
  :delight
  :after (evil)
  :init
  (defun my/toggle-test-vsplit ()
    (interactive)
    (my/evil-window-vsplit)
    (projectile-toggle-between-implementation-and-test))

  :custom
  (projectile-completion-system 'ivy)
  (projectile-create-missing-test-files t)

  :config
  (projectile-mode 1)

  :bind
  (:map projectile-mode-map
        ("s-p" . projectile-command-map))
  (:map evil-normal-state-map
        ("<leader>," . projectile-toggle-between-implementation-and-test)
        ("<leader>v" . my/toggle-test-vsplit)))

(use-package magit
  :straight t)

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package tab-bar
  :straight nil
  :demand t
  :init
  ;; https://christiantietze.de/posts/2022/02/emacs-tab-bar-numbered-tabs/
  (defun my/tab-bar-tab-name-format-function (tab i)
    (propertize
     (concat " " (alist-get 'name tab) " ")
     'face (funcall tab-bar-tab-face-function tab)))

  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  ;; ZWSP is used to prevent last tab from filling all available space
  (tab-bar-separator "​")

  (tab-bar-tab-name-format-function 'my/tab-bar-tab-name-format-function)

  :config
  ;; http://www.gonsie.com/blorg/tab-bar.html
  (tab-bar-mode 1)

  :bind
  (("s-{" . tab-bar-switch-to-prev-tab)
   ("s-}" . tab-bar-switch-to-next-tab)
   ("s-t" . tab-bar-new-tab)
   ("s-w" . tab-bar-close-tab)
   ("C-<backspace>" . tab-bar-close-tab)))

(use-package xref
  :straight nil
  :after (evil)
  :config
  (evil-make-overriding-map xref--xref-buffer-mode-map 'normal)
  :bind
  (:map xref--xref-buffer-mode-map
        ("S-<return>" . xref-show-location-at-point)
        ("<return>" . xref-goto-xref)
        ("C-<return>" . xref-quit-and-goto-xref)
        ("q" . xref-quit-and-pop-marker-stack)))

(use-package yaml-mode
  :straight t)

;;-----------------------------------------------------------------------------
;;
;; Managed by customize-group
;;
;;-----------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values '((cider-clojure-cli-aliases . ":dev"))))

;; Use list-faces-display to find specific face
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-minibuffer-match-face-1 ((t nil)))
 '(ivy-minibuffer-match-face-2 ((t (:inherit 'lazy-highlight))))
 '(ivy-minibuffer-match-face-3 ((t (:inherit 'lazy-highlight))))
 '(ivy-minibuffer-match-face-4 ((t (:inherit 'lazy-highlight))))
 '(swiper-match-face-1 ((t nil)))
 '(swiper-match-face-2 ((t (:inherit 'isearch))))
 '(swiper-match-face-3 ((t (:inherit 'isearch))))
 '(swiper-match-face-4 ((t (:inherit 'isearch)))))

(put 'narrow-to-region 'disabled nil)
