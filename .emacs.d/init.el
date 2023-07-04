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

;; - used for automatic line-wrapping (when enabled)
;; - can be used for global-display-fill-column-indicator-mode
;; - can be used to get max line length in user functions
(setq-default fill-column 80)
(setq column-number-mode 1)
(setq show-paren-delay 0)

(global-display-fill-column-indicator-mode 1)
;; Use specific value (82) instead of fill-column value to make column
;; indicator appear exactly at the center of the screen on my laptop
(setq-default display-fill-column-indicator-column 82)

;; nowrap
(setq-default truncate-lines 1)

;;-----------------------------------------------------------------------------
;; Theme (faces)
;;
;; Use list-faces-display to list all current faces
;;-----------------------------------------------------------------------------

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;; (load-theme 'tango-plus t)
(load-theme 'aircon t)

;; https://emacs.stackexchange.com/a/69091
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

;; https://stackoverflow.com/a/1819405/3632318
;; Use whitespaces instead of tabs
(setq-default indent-tabs-mode nil)

(setq-default tab-width 2)
(setq sh-basic-offset 2)

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
;;
;; Define key in evil-normal-state-map as well
;; for it to work in insert and emacs states
(global-set-key [?\C-.] 'execute-extended-command)

(global-set-key (kbd "s-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "s-x") 'clipboard-kill-region)
(global-set-key (kbd "s-v") 'clipboard-yank)

;; Use touchpad to scroll horizontally
;; (can be useful, say, in eldoc-box popups)
(global-set-key (kbd "<triple-wheel-left>")
                (lambda ()
                  (interactive)
                  (if truncate-lines (scroll-right 1))))
(global-set-key (kbd "<triple-wheel-right>")
                (lambda ()
                  (interactive)
                  (if truncate-lines (scroll-left 1))))

;;-----------------------------------------------------------------------------
;;
;; Packages
;;
;;-----------------------------------------------------------------------------

;; Install it first to make :delight option work
(use-package delight
  :straight t)

;; Enablc gcmh-mode first to decrease Emacs startup time
(use-package gcmh
  :straight t
  :delight gcmh-mode
  :config
  (gcmh-mode 1))

(use-package auto-package-update
  :straight t)

(use-package autorevert
  :straight t
  :delight auto-revert-mode)

(use-package consult
  :straight t
  :custom
  (consult-async-input-debounce 0.01)
  (consult-async-input-throttle 0.01)
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.1)
  (consult-find-args
   "find . -type f \
    -not ( -path ./.clj-kondo/* -prune ) \
    -not ( -path ./.cpcache/* -prune ) \
    -not ( -path ./.git/* -prune ) \
    -not ( -path ./.gradle/* -prune ) \
    -not ( -path ./.idea/* -prune ) \
    -not ( -path ./build/* -prune ) \
    -not ( -path ./docker/cockroach-data/* -prune ) \
    -not ( -path ./target/* -prune )")
  (consult-preview-key "C-<return>")

  :custom-face
  (consult-file ((t (:foreground "#777777"))))

  :config
  (consult-customize
   consult-find :prompt ""
   consult-line :preview-key 'any :prompt "Filter: "
   consult-ripgrep :group nil :prompt ""
   ;; mode line disappears when prompt is ""
   consult-xref :prompt "Filter: "))

(use-package emacs
  :straight nil
  :init
  ;; https://github.com/minad/vertico#configuration
  (defun my/setup-minibuffer-mode ()
    (setq minibuffer-prompt-properties
          '(cursor-intangible t face minibuffer-prompt read-only t)))

  :hook
  ((minibuffer-setup . my/setup-minibuffer-mode)))

(use-package embark
  :straight t
  :init
  (defun my/find-file-split (filename)
    (my/evil-window-split)
    (find-file filename))

  (defun my/find-file-vsplit (filename)
    (my/evil-window-vsplit)
    (find-file filename))

  (defun my/find-file-tab (filename)
    (tab-bar-new-tab)
    (find-file filename))

  :custom
  (embark-indicators '(embark--vertico-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))

  :bind
  (("C-u" . embark-act))

  (:map embark-file-map
        ("C-s" . #'my/find-file-split)
        ("C-v" . #'my/find-file-vsplit)
        ("C-t" . #'my/find-file-tab)))

(use-package embark-consult
  :straight t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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
  ;; https://github.com/emacs-evil/evil/issues/576
  (setq evil-want-Y-yank-to-eol t)

  (defun my/c-g ()
    (interactive)
    ;; cond should be faster than pcase because pcase is a macro
    (cond
     ((eq evil-state 'insert) (evil-normal-state))
     ((eq evil-state 'normal) (evil-ex-nohighlight))
     ((eq evil-state 'visual) (evil-exit-visual-state))
     ((eq evil-state 'replace) (evil-normal-state))))

  (defun my/insert-tab-or-complete ()
    (interactive)
    (let ((chr (preceding-char)))
      ;; - beginning of line OR
      ;; - preceding character is whitespace OR
      ;; - preceding character is tab (for go-mode)
      (if (or (bolp) (= chr 32) (= chr 9))
          ;; insert tab
          (tab-to-tab-stop)
        ;; else complete
        (company-complete-common))))

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

  ;; https://stackoverflow.com/a/9697222/3632318
  (defun my/toggle-comment ()
    (interactive)
    (let (beg end)
      (if (region-active-p)
          (setq beg (region-beginning) end (region-end))
        (setq beg (line-beginning-position) end (line-end-position)))
      (comment-or-uncomment-region beg end)))

  :custom
  (evil-ex-search-case 'smart)
  (evil-visual-update-x-selection-p nil)

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

  ;; https://stackoverflow.com/a/23918497
  (evil-set-initial-state 'Buffer-menu-mode 'emacs)

  :bind
  (:map evil-insert-state-map
        ("C-g" . my/c-g)
        ("RET" . comment-indent-new-line)
        ("TAB" . my/insert-tab-or-complete))

  (:map evil-normal-state-map
        ("C-g" . my/c-g)
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
        ("C-g" . my/c-g)
        ("C-." . execute-extended-command)

        ("C-s" . sort-lines)

        ("H" . evil-first-non-blank)
        ("L" . evil-last-non-blank))

  (:map evil-replace-state-map
        ("C-g" . my/c-g))

  ;; https://github.com/noctuid/evil-guide#global-keybindings-and-evil-states
  ;; > motion state is the default state for help-mode
  ;; > only keys bound in motion state will work in help-mode
  (:map evil-motion-state-map
        ("H" . evil-first-non-blank)
        ("L" . evil-last-non-blank)))

(use-package avy
  :straight t
  :after evil
  :bind
  (:map evil-normal-state-map
        ("<leader>w" . avy-goto-word-1)))

;; https://docs.cider.mx/cider/repl/keybindings.html
;;
;; - "C-c C-x j j" - cider-jack-in
;; - "C-c C-d C-c" - cider-clojuredocs
;; - "C-c C-d C-w" - cider-clojuredocs-web
;;
;; - "C-c C-t C-t" - cider-test-run-test
;; - "C-c C-t C-n" - cider-test-run-ns-tests
;; - "C-c C-t C-p" - cider-test-run-project-tests
;; - "C-c C-t C-r" - cider-test-rerun-failed-tests
(use-package cider
  :straight t
  :delight " CIDER"
  :init
  (defun my/setup-cider-mode ()
    (setq-local show-trailing-whitespace nil)
    ;; https://github.com/emacs-evil/evil/issues/511#issuecomment-273754917
    (define-key evil-normal-state-local-map "q" 'cider-popup-buffer-quit-function))

  :hook
  ((cider-popup-buffer-mode . my/setup-cider-mode)
   (cider-repl-mode . my/setup-cider-mode)
   (cider-stacktrace-mode . my/setup-cider-mode)
   (cider-test-report-mode . my/setup-cider-mode))

  :custom
  ;; Use xref backend provided by Eglot
  (cider-use-xref nil))

(use-package clojure-mode
  :straight t)

(use-package company
  :straight t
  :demand t
  :delight company-mode
  :custom
  ;; http://company-mode.github.io/manual/Customization.html#Customization
  ;; Set to nil to disable automatic completion
  (company-idle-delay nil)
  (company-selection-wrap-around t)
  (company-require-match nil)

  ;; http://company-mode.github.io/manual/Frontends.html#Frontends
  (company-frontends
   '(company-pseudo-tooltip-unless-just-one-frontend
     company-preview-frontend))
  (company-tooltip-align-annotations t)
  (company-tooltip-minimum 4)
  (company-tooltip-limit 8)
  (company-tooltip-width-grow-only t)
  (company-tooltip-margin 1)
  (company-format-margin-function 'company-vscode-light-icons-margin)

  :custom-face
  (company-preview ((t (:background "white" :foreground "#999999"))))
  (company-preview-common ((t (:background "white" :foreground "#999999"))))
  (company-tooltip ((t (:background "#FFFAEA"))))
  (company-tooltip-selection ((t (:background "#CBE6ED"))))

  :config
  (global-company-mode)

  :bind
  (:map company-active-map
        ;; evil-force-normal-state doesn't record current command but we
        ;; don't need it here
        ("C-g" . evil-force-normal-state)
        ;; http://company-mode.github.io/manual/Getting-Started.html#Getting-Started
        ;; Use <tab> instead of TAB to override other keybindings
        ("<tab>" . company-complete-common)))

;; https://github.com/company-mode/company-mode/issues/340
(use-package company-anywhere
  :straight (company-anywhere :type git :host github :repo "zk-phi/company-anywhere")
  :after company)

;; - "(" - dired-hide-details-mode
(use-package dired
  :straight nil
  :init
  (defun my/setup-dired-mode ()
    (dired-hide-details-mode 1))

  (defun my/dired-up-directory ()
    (interactive)
    (find-alternate-file ".."))

  ;; https://emacs.stackexchange.com/a/204/39266
  (defun my/kill-dired-buffers ()
    (interactive)
    (mapc (lambda (buffer)
            (when (eq (buffer-local-value 'major-mode buffer) 'dired-mode)
              (kill-buffer buffer)))
          (buffer-list)))

  :hook
  ((dired-mode . my/setup-dired-mode))

  :bind
  (:map dired-mode-map
        ;; http://xahlee.info/emacs/emacs/emacs_dired_tips.html
        ;; ("RET" . dired-find-alternate-file)
        ;; ("p" . my/dired-up-directory)
        ("p" . dired-up-directory)
        ("q" . my/kill-dired-buffers)))

(use-package dired-subtree
  :straight t
  :demand t
  :custom
  (dired-subtree-use-backgrounds t)

  :custom-face
  (dired-subtree-depth-1-face ((t (:background "#F9F9F9"))))
  (dired-subtree-depth-2-face ((t (:background "#F3F3F3"))))
  (dired-subtree-depth-3-face ((t (:background "#F3F3F3"))))
  (dired-subtree-depth-4-face ((t (:background "#F3F3F3"))))
  (dired-subtree-depth-5-face ((t (:background "#F3F3F3"))))
  (dired-subtree-depth-6-face ((t (:background "#F3F3F3"))))

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
;; - go-mode => gopls
;; - haskell-mode => haskell-language-server-wrapper
;; - kotlin-mode => kotlin-language-server
;;
;; - eglot-events-buffer (show Eglot logs)
(use-package eglot
  ;; Built-in package since Emacs 29
  :straight nil
  :demand t
  :init
  (defun my/setup-eglot-managed-mode ()
    (setq-local eldoc-documentation-functions '(flymake-eldoc-function)))

  (defun my/eglot-organize-imports ()
    ;; https://github.com/joaotavora/eglot/issues/574#issuecomment-1401023985
    (eglot-code-actions nil nil "source.organizeImports" t))

  (defun my/eglot-clojure-mode-add-hooks ()
    ;; Calls cljfmt on current buffer
    (add-hook 'before-save-hook 'eglot-format-buffer -10 t))

  ;; NOTE: if any hook returns error, subsequent hooks are not executed
  ;;
  ;; For example my/eglot-organize-imports returns error when there is
  ;; nothing to import => make sure it comes last (using DEPTH parameter)
  (defun my/eglot-go-mode-add-hooks ()
    ;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md#loading-eglot-in-emacs
    ;; > The depth of -10 places this before eglot's willSave notification,
    ;; > so that that notification reports the actual contents that will be saved.
    ;;
    ;; Calls gofmt on current buffer
    (add-hook 'before-save-hook 'eglot-format-buffer -10 t)
    (add-hook 'before-save-hook 'my/eglot-organize-imports -5 t))

  ;; NOTE: ktfmt style is very different from IntelliJ IDEA coding conventions
  ;; and kotlin-mode indentation rules (the latter is more important for me)
  (defun my/eglot-kotlin-mode-add-hooks ()
    ;; Calls ktfmt on current buffer
    ;; See eglot-format function implementation for options
    (add-hook 'before-save-hook 'eglot-format-buffer -10 t))

  ;; NOTE: currently not used because I need to figure out how to set
  ;; indentation width to 4 for yapf (it's 8 by default)
  (defun my/eglot-python-mode-add-hooks ()
    (add-hook 'before-save-hook 'eglot-format-buffer -10 t))

  :hook
  ((eglot-managed-mode . my/setup-eglot-managed-mode)
   (clojure-mode . eglot-ensure)
   (clojure-mode . my/eglot-clojure-mode-add-hooks)
   (go-mode . eglot-ensure)
   (go-mode . my/eglot-go-mode-add-hooks)
   (haskell-mode . eglot-ensure)
   (kotlin-mode . eglot-ensure)
   (kotlin-mode . my/eglot-kotlin-mode-add-hooks)
   (python-mode . eglot-ensure))

  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits nil)
  ;; [kotlin-language-server] It might take a lot of time to resolve all
  ;; dependencies when there are no caches in ~/.gradle/caches
  (eglot-connect-timeout 300)
  ;; Disable events buffer for performance reasons
  (eglot-events-buffer-size 0)
  ;; https://github.com/joaotavora/eglot/issues/334
  ;; :documentHighlightProvider - highlight variable at point
  ;; :hoverProvider - fetch docs from LSP server (don't disable)
  (eglot-ignored-server-capabilities '(:documentHighlightProvider))

  :config
  ;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md#configuring-gopls-via-eglot
  ;; See all server settings in https://github.com/emacs-lsp/lsp-mode/tree/master/clients
  (setq-default eglot-workspace-configuration
                '((:gopls . ((gofumpt . t)
                             (matcher . "CaseSensitive")
                             (staticcheck . t)))
                  ;; https://github.com/python-lsp/python-lsp-server/blob/develop/CONFIGURATION.md
                  (:pylsp . (:plugins (:yapf (:enabled t))))))

  (add-to-list 'eglot-server-programs
               '(kotlin-mode . ("kotlin-language-server"
                                :initializationOptions
                                (:kotlin (:compiler (:jvm (:target "1.8")))))))

  :bind
  (:map eglot-mode-map
        ;; https://emacs-lsp.github.io/lsp-mode/page/keybindings/
        ;; Default keybindings in lsp-mode
        ("s-l = =" . eglot-format-buffer)
        ("s-l a a" . eglot-code-actions)
        ("s-l g g" . xref-find-definitions)
        ("s-l g r" . xref-find-references)
        ("s-l g t" . eglot-find-typeDefinition)
        ("s-l r o" . eglot-code-action-organize-imports)
        ("s-l r r" . eglot-rename)
        ;; Default keybindings in IntelliJ IDEA
        ("s-b" . xref-find-references)
        ("s-B" . eglot-find-typeDefinition)))

(use-package eldoc
  :straight nil
  :delight eldoc-mode
  :custom
  ;; Show all eldoc feedback
  (eldoc-documentation-strategy 'eldoc-documentation-compose)
  ;; Always show single line in echo area
  (eldoc-echo-area-use-multiline-p nil)
  ;; eldoc-box uses eldoc buffer to show docs so content for
  ;; eldoc-box is ready only after this number of seconds
  (eldoc-idle-delay 0.5))

(use-package eldoc-box
  :straight (eldoc-box :type git :host github :repo "tap349/eldoc-box")
  :demand t
  :after evil
  :init
  (defun my/setup-eldoc-box-buffer ()
    (setq-local inhibit-message t)
    (setq-local show-trailing-whitespace nil))

  :hook
  ((eldoc-box-buffer . my/setup-eldoc-box-buffer))

  :custom
  ;; https://github.com/sebastiencs/company-box/blob/master/company-box-doc.el#L86
  ;; Set height and width to a big number to disable wrapping
  (eldoc-box-max-pixel-height 700)
  (eldoc-box-max-pixel-width 700)

  :custom-face
  (eldoc-box-body ((t (:background "#F5F5F7"))))
  (eldoc-box-border ((t (:background "#C5C5C7"))))
  ;; (eldoc-box-body ((t (:background "#F9F9F5"))))
  ;; (eldoc-box-border ((t (:background "#C9C9C5"))))

  :config
  (advice-add 'my/c-g :after 'eldoc-box-quit-frame)

  :bind
  (:map evil-normal-state-map
        ("C-n" . eldoc-box-eglot-toggle-help-at-point)))

(use-package elec-pair
  :straight nil
  :init
  (defun my/electric-pair-inhibit-predicate (_char)
    (minibufferp))

  :custom
  (electric-pair-delete-adjacent-pairs t)
  (electric-pair-inhibit-predicate 'my/electric-pair-inhibit-predicate)

  :config
  (electric-pair-mode 1))

(use-package evil-surround
  :straight t
  :demand t
  :config
  (global-evil-surround-mode 1))

(use-package evil-visualstar
  :straight (evil-visualstar :type git
                             :host github
                             :repo "tap349/evil-visualstar")
  :demand t
  :after evil
  :init
  (setq my/evil-ex-search-next-offset 0)

  (defun my/evil-ex-search-forward ()
    (interactive)
    (setq my/evil-ex-search-next-offset 0)
    (evil-ex-search-forward))

  (defun my/evil-ex-search-backward ()
    (interactive)
    (setq my/evil-ex-search-next-offset 0)
    (evil-ex-search-backward))

  (defun my/evil-ex-search-next ()
    (interactive)
    (evil-ex-search-next)
    (forward-char my/evil-ex-search-next-offset))

  (defun my/evil-ex-search-previous ()
    (interactive)
    (when (> my/evil-ex-search-next-offset 0)
      (evil-ex-search-previous))
    (evil-ex-search-previous)
    (forward-char my/evil-ex-search-next-offset))

  ;; https://stackoverflow.com/a/26650886/3632318
  ;; https://github.com/noctuid/evil-guide#declaring-a-motion
  ;;
  ;; evil-declare-motion:
  ;; - sets `:repeat motion` => don't count functions as repeatables
  ;; - sets `:keep-visual t` => make functions work in visual state
  (evil-declare-motion 'my/evil-ex-search-forward)
  (evil-declare-motion 'my/evil-ex-search-backward)
  (evil-declare-motion 'my/evil-ex-search-next)
  (evil-declare-motion 'my/evil-ex-search-previous)

  ;; See https://github.com/noctuid/evil-guide#buffer-local-keybindings
  ;; if you ever need to define this keybinding for specific mode only
  (defun my/asterisk-normal ()
    (interactive)
    ;; Hyphen inside character class indicates range and IDK how to escape
    ;; it in Rx notation => don't use it inside `any` construct if you need
    ;; literal value
    ;;
    ;; Don't add optional leading colon to vim-word-regexp - it causes a
    ;; lot of problems when using word search in `begin-search` because
    ;; colon character has different syntax classes in different modes
    (let ((vim-word-regexp
           (pcase major-mode
             ('clojure-mode
              (rx (one-or-more (or "-" (any "0-9A-Za-z" "!<>?_")))))
             (_
              (rx (one-or-more (or "-" (any "0-9A-Za-z" "!?_"))))))))
      (when (thing-at-point-looking-at vim-word-regexp)
        (setq my/evil-ex-search-next-offset (- (point) (match-beginning 0)))
        (evil-visualstar/begin-search (match-beginning 0) (match-end 0) t t)
        (forward-char my/evil-ex-search-next-offset))))

  (defun my/asterisk-visual (beg end)
    (interactive "r")
    (setq my/evil-ex-search-next-offset 0)
    (evil-visualstar/begin-search-forward beg end))

  (defun my/asterisk-z-normal ()
    (interactive)
    (my/asterisk-normal)
    (evil-ex-search-previous))

  (defun my/asterisk-z-visual (beg end)
    (interactive "r")
    (evil-visualstar/begin-search-forward beg end)
    (evil-ex-search-previous))

  ;; global-evil-visualstar-mode is not enabled so define all keybindings
  :bind
  (:map evil-normal-state-map
        ("/" . my/evil-ex-search-forward)
        ("?" . my/evil-ex-search-backward)
        ("n" . my/evil-ex-search-next)
        ("N" . my/evil-ex-search-previous)
        ("*" . my/asterisk-normal)
        ("z*" . my/asterisk-z-normal))

  (:map evil-visual-state-map
        ("/" . my/evil-ex-search-forward)
        ("?" . my/evil-ex-search-backward)
        ("n" . my/evil-ex-search-next)
        ("N" . my/evil-ex-search-previous)
        ("*" . my/asterisk-visual)
        ("z*" . my/asterisk-z-visual)))

;; - flymake-show-buffer-diagnostics (show all buffer errors)
(use-package flymake
  :straight nil
  :bind
  (("M-]" . flymake-goto-next-error)
   ("M-[" . flymake-goto-prev-error)))

(use-package go-mode
  :straight t
  :init
  ;; https://www.masteringemacs.org/article/executing-shell-commands-emacs
  ;; This is pretty heavy operation since it runs external shell command
  ;; => don't add it as before-save-hook - execute manually when needed
  (defun my/format-buffer-with-golines ()
    (interactive)
    ;; https://stackoverflow.com/a/24283996
    (let ((old-point (point))
          ;; golines wraps at this line length
          ;; (or else set to fill-column)
          (max-len display-fill-column-indicator-column))
      (shell-command-on-region
       (point-min)
       (point-max)
       (format "golines --no-reformat-tags -m %s" max-len)
       (current-buffer)
       t
       "*golines errors*"
       t)
      (goto-char old-point))))

(use-package haskell-mode
  :straight t)

;; hs-minor-mode can be enabled not in all major modes
;; Use evil-toggle-fold to toggle folding
(use-package hideshow
  :straight nil
  :delight hs-minor-mode
  :hook
  ((prog-mode . hs-minor-mode)))

(use-package jarchive
  :straight t
  :config
  (jarchive-setup))

(use-package json-mode
  :straight t)

(use-package kotlin-mode
  :straight t
  :init
  (defun my/setup-kotlin-mode ()
    ;; For kotlin-language-server
    (setq-local tab-width 4))

  :hook
  ((kotlin-mode . my/setup-kotlin-mode))

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

(use-package magit
  :straight t)

;; Fixes a bug in some consult commands (say, consult-xref)
;; when you cannot search for substring
(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  ;; https://github.com/minad/vertico/issues/237#issuecomment-1134000907
  ;;
  ;; completions-first-difference face is used by `basic` completion style
  ;; but not by orderless: if this variable is nil, orderless is used for
  ;; all completion categories => completions-first-difference is not used
  (completion-category-overrides nil))

(use-package projectile
  :straight t
  :demand t
  :delight
  :after evil
  :init
  (defun my/toggle-test-vsplit ()
    (interactive)
    (my/evil-window-vsplit)
    (projectile-toggle-between-implementation-and-test))

  :custom
  (projectile-completion-system 'auto)
  (projectile-create-missing-test-files t)
  (projectile-switch-project-action 'consult-find)

  :config
  (projectile-mode 1)

  ;; https://github.com/bbatsov/projectile/blob/31b87151b1fe43221736ded957a1123a54e32531/projectile.el#L3390
  ;; https://stackoverflow.com/a/72046404/3632318
  ;; By default `Spec` test suffix is used
  (projectile-register-project-type 'gradlew '("gradlew")
                                    :project-file "gradlew"
                                    :compile "./gradlew build"
                                    :test "./gradlew test"
                                    :test-suffix "Test")
  :bind
  (:map projectile-mode-map
        ("s-p" . projectile-command-map))

  (:map evil-normal-state-map
        ("<leader>," . projectile-toggle-between-implementation-and-test)
        ("<leader>v" . my/toggle-test-vsplit)))

(use-package python
  :straight nil
  :custom
  (python-indent-offset 4)

  :config
  (setq python-indent-guess-indent-offset nil))

(use-package rainbow-delimiters
  :straight t
  :hook
  ((prog-mode . rainbow-delimiters-mode)))

;; For camel-case motions
(use-package subword
  :straight nil
  :after go-mode
  :delight
  :config
  (global-subword-mode 1)

  :hook
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Class-Table.html
  (;; It looks like underscore has word syntax class in go-mode - when paired
   ;; with subword-mode it causes evil-forward-word-begin command to get stuck
   ;; after words containing underscores => set syntax class to "_" explicitly
   (go-mode . (lambda () (modify-syntax-entry ?_ "_")))
   (kotlin-mode . (lambda () (modify-syntax-entry ?$ "_")))))

(use-package tab-bar
  :straight nil
  :demand t
  :init
  ;; https://christiantietze.de/posts/2022/02/emacs-tab-bar-numbered-tabs/
  (defun my/tab-bar-tab-name-format-function (tab i)
    (propertize
     (concat " " (alist-get 'name tab) " ")
     'face (funcall tab-bar-tab-face-function tab)))

  (defun my/tab-bar-move-tab-left ()
    (interactive)
    (tab-bar-move-tab -1))

  (defun my/tab-bar-move-tab-right ()
    (interactive)
    (tab-bar-move-tab 1))

  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  (tab-bar-new-tab-choice t)
  ;; ZWSP is used to prevent last tab from filling all available space
  (tab-bar-separator "​")

  (tab-bar-tab-name-format-function 'my/tab-bar-tab-name-format-function)

  :config
  ;; http://www.gonsie.com/blorg/tab-bar.html
  (tab-bar-mode 1)

  :bind
  (("s-{" . tab-bar-switch-to-prev-tab)
   ("s-}" . tab-bar-switch-to-next-tab)
   ("C-s-{" . my/tab-bar-move-tab-left)
   ("C-s-}" . my/tab-bar-move-tab-right)
   ("s-t" . tab-bar-new-tab)
   ("s-w" . tab-bar-close-tab)
   ("C-<tab>" . tab-recent)
   ("C-<backspace>" . tab-bar-close-tab)))

(use-package vertico
  :straight t
  :after evil
  :init
  (vertico-mode 1)

  :custom
  (vertico-count 15)
  (vertico-cycle t)
  (vertico-resize nil)
  (vertico-scroll-margin 2)

  :custom-face
  (vertico-current ((t (:background "#E6E6F0"))))
  (vertico-group-title ((t (:foreground "#888878"))))

  :bind
  ;; https://www.reddit.com/r/emacs/comments/zznamq/comment/j2g9ci4/
  ;; https://emacs.stackexchange.com/a/2473/39266
  (:map vertico-map
        ("C-s" . (lambda ()
                   (interactive)
                   (execute-kbd-macro (kbd "C-u C-s"))))
        ("C-v" . (lambda ()
                   (interactive)
                   (execute-kbd-macro (kbd "C-u C-v"))))
        ("C-t" . (lambda ()
                   (interactive)
                   (execute-kbd-macro (kbd "C-u C-t")))))

  (:map evil-normal-state-map
        ("<leader>n" . consult-find)
        ("<leader>f" . consult-flymake)
        ("<leader>/" . consult-ripgrep)
        ("C-s" . consult-line)))

(use-package whitespace
  :straight nil
  :delight
  :custom
  ;; https://emacs.stackexchange.com/a/21865
  ;;
  ;; 2 ways to show trailing whitespaces:
  ;; - show-trailing-whitespace (trailing-whitespace face) OR
  ;; - whitespace-mode (whitespace-trailing face)
  ;;
  ;; 2 ways to visualize whitespace characters (they can be combined):
  ;; - face (with different face) or
  ;; - *-mark (with ASCII characters)
  ;;
  ;; Add tab-mark to visualize tabs with ASCII characters
  (whitespace-style '(face missing-newline-at-eof tabs trailing))
  ;; 183 - middle dot
  ;; 8250 - small right angle arrow
  (whitespace-display-mappings '((tab-mark 9 [183 9])))

  :custom-face
  ;; foreground is used, say, for tab marks
  ;;
  ;; Use `:background unspecified` instead of `:background "white"`
  ;; to allow background of vertico-current to override it
  (whitespace-tab ((t (:background unspecified :foreground "#DDDDDD"))))
  (whitespace-trailing ((t (:background "#E3A8A8" :foreground "#C38888"))))
  (whitespace-missing-newline-at-eof ((t (:background "#E3A8A8"))))

  :config
  (global-whitespace-mode 1))

(use-package xr
  :straight t)

;; https://emacs.stackexchange.com/a/61387
;;
;; - "C-]" - xref-find-definitions
;; - "M-?" - xref-find-references
(use-package xref
  :straight nil
  :after evil
  :custom
  ;; - xref-find-definitions jumps to the definition right away
  ;;   when only one definition is found
  ;; - xref-find-references always shows references list - even
  ;;   if only one reference is found (true for type definitions
  ;;   which are displayed with xref-find-references)
  (xref-show-definitions-function #'consult-xref)
  ;; In Emacs 27+ it will affect all xref-based commands
  ;; except for xref-find-definitions
  (xref-show-xrefs-function #'consult-xref)

  :config
  (evil-make-overriding-map xref--xref-buffer-mode-map 'normal)

  :bind
  ;; These keybindings have effect only in default xref buffer
  (:map xref--xref-buffer-mode-map
        ;; Same as consult-preview-key
        ("C-<return>" . xref-show-location-at-point)
        ;; ("o" . xref-goto-xref)
        ("<return>" . xref-quit-and-goto-xref)
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

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
