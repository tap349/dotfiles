;; -*- lexical-binding: t; -*-

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
;; Look and feel
;;
;;-----------------------------------------------------------------------------

;; Turn off all alarms (ring-bell and visible-bell)
(setq ring-bell-function 'ignore)

;; Cursor (can be overidden by evil-STATE-state-cursor)
(setq-default cursor-type '(bar . 2))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

;; - used for automatic line-wrapping (when enabled)
;; - can be used for global-display-fill-column-indicator-mode
;; - can be used to get max line length in user functions
(setq-default fill-column 80)

;; Show column in mode line
(column-number-mode 1)

(show-paren-mode 1)
(setq show-paren-delay 0)

(global-display-fill-column-indicator-mode 1)
;; This value can be different from fill-column value and is chosen to
;; make column indicator appear exactly at the center of the screen on
;; my personal laptop
(setq-default display-fill-column-indicator-column 82)

;;-----------------------------------------------------------------------------
;; Wrapping
;;
;; Hard wrap - insert actual newlines automatically
;; (at a fixed column set by fill-column variable)
;; => (auto-fill-mode 1)
;;
;; Soft wrap - wrap visually without insert newlines
;; (dynamically based on the window width)
;; => (visual-line-mode 1)
;;-----------------------------------------------------------------------------

;; Don't display continuation lines (don't wrap by default)
(setq-default truncate-lines 1)

;;-----------------------------------------------------------------------------
;; Mode line
;;-----------------------------------------------------------------------------

(setq mode-line-percent-position nil)

(set-face-attribute 'mode-line
                    nil
                    :background "#C3C3CA"
                    :box '(:color "#8F8F92" :line-width -1 :style flat-button))

;; - "M-=" - count-words-region (show region stats)
(setq-default mode-line-format
              (list " "
                    'mode-line-mule-info
                    'mode-line-modified
                    ;; <= evil-mode-line-format
                    "  "
                    'mode-line-buffer-identification
                    "  "
                    'mode-line-position
                    " "
                    'mode-line-modes
                    'mode-line-misc-info
                    'mode-line-end-spaces))

;;-----------------------------------------------------------------------------
;; Theme (faces)
;;
;; Use list-faces-display to list all current faces
;;-----------------------------------------------------------------------------

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'aircon t)

;; https://emacs.stackexchange.com/a/69091
(set-face-attribute 'default nil
                    :font "Input-15"
                    :weight 'regular
                    :slant 'normal)

(set-face-foreground 'fill-column-indicator "#DEE4EF")
(set-face-foreground 'vertical-border "#D8D8DE")

;; Bright green #3FFF9C also looks good
(defface vim-isearch
  '((((class color) (min-colors 89))
     (:background "#FFFF9C" :foreground "black" :weight bold)))
  "Face for highlighting search results like in Vim.")

;; These faces already inherit from aircon-vim-isearch face in
;; aircon theme but I want to make these customizations global
(set-face-attribute 'isearch nil :inherit 'vim-isearch)
(set-face-attribute 'lazy-highlight nil :inherit 'vim-isearch)
(set-face-attribute 'match nil :inherit 'vim-isearch)

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

;; Style for comment-region command, used by my/toggle-comment
(setq comment-style 'indent)

;;-----------------------------------------------------------------------------
;;
;; Scrolling
;;
;; https://www.emacswiki.org/emacs/SmoothScrolling
;;
;; See also ultra-scroll package configuration
;;
;;-----------------------------------------------------------------------------

;; Smooth scrolling without cursor jumps
(setq scroll-conservatively 101)
(setq scroll-margin 2)

;;-----------------------------------------------------------------------------
;;
;; Registers (C-x r j)
;;
;; https://stackoverflow.com/a/12558095/3632318
;;
;;-----------------------------------------------------------------------------

(set-register ?c (cons 'file user-init-file))
(set-register ?z (cons 'file (substitute-in-file-name "${ZDOTDIR}/.zshenv")))

;;-----------------------------------------------------------------------------
;;
;; Keybindings
;;
;; `global-set-key' = `define-key global-map'
;;
;; - TAB - <tab> - [tab]
;; - S-TAB - S-<tab> - [backtab]
;; - RET - <return> - [return]
;;
;; ## <tab> vs. TAB
;;
;; https://github.com/syl20bnr/spacemacs/issues/4024#issuecomment-161122099
;;
;; - <tab> = Tab (GUI only)
;; - TAB = C-i = Tab (Terminal, or GUI if <tab> is not set)
;; => in GUI <tab> keybinding overrides TAB one
;;
;; ## <tab> vs. [tab]
;;
;; [tab] can't be used in keybindings with prefix keys like "C-[tab]":
;;
;; - "<tab>" = [tab] (ok)
;; - "C-<tab>" (ok)
;; - "C-[tab]" (error)
;; =>
;; - use [tab] style in keybindings that use only one key
;; - use <tab> style in keybindings that use prefix keys
;;
;;-----------------------------------------------------------------------------

;; Works for keybindings physically present under global `ctl-x-map'
;; but some packages can add keybindings through minor-mode keymaps.
;; Say, `diff-hl' package adds keybindings in `diff-hl-command-map'
;; (C-x v) keymap which has higher precedence than `global-map'
;; (global-set-key (kbd "C-q") ctl-x-map)
(define-key key-translation-map (kbd "C-q") (kbd "C-x"))
(global-set-key (kbd "C-'") #'quoted-insert)

(global-set-key (kbd "C-c d") #'duplicate-dwim)

;; https://www.emacswiki.org/emacs/DvorakKeyboard
;;
;; Define key in evil-normal-state-map as well
(global-set-key [?\C-.] #'execute-extended-command)

(global-set-key (kbd "s-c") #'clipboard-kill-ring-save)
(global-set-key (kbd "s-x") #'clipboard-kill-region)
(global-set-key (kbd "s-v") #'clipboard-yank)

;; Original keybindings scroll by a near full screen
(global-set-key (kbd "M-v") #'my/scroll-half-page-backward)
(global-set-key (kbd "C-v") #'my/scroll-half-page-forward)

;; Use touchpad to scroll horizontally (useful, say, in eldoc-box popups)
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
;; Helpers
;;
;;-----------------------------------------------------------------------------

;; https://stackoverflow.com/a/9697222/3632318
(defun my/toggle-comment ()
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

;;-----------------------------------------------------------------------------
;; Insert newline / whitespace
;;-----------------------------------------------------------------------------

(defun my/insert-newline-below ()
  (interactive)
  (end-of-line)
  (newline))

(defun my/insert-newline-above ()
  (interactive)
  (beginning-of-line)
  (open-line 1))

(defun my/insert-whitespace ()
  (interactive)
  (insert " "))

;;-----------------------------------------------------------------------------
;; Scroll page
;;-----------------------------------------------------------------------------

(defvar my/scroll-half-page-column nil)

(defun my/scroll-half-page-backward ()
  (interactive)
  (my/scroll-half-page -1))

(defun my/scroll-half-page-forward ()
  (interactive)
  (my/scroll-half-page +1))

;; Vim-like C-u/C-d: point always moves by half a window while
;; the window scrolls by the same amount stopping at buffer edges
;; (in particular never showing empty space below the last line)
(defun my/scroll-half-page (dir)
  (let ((column (if (memq last-command '(my/scroll-half-page-backward
                                         my/scroll-half-page-forward))
                    my/scroll-half-page-column
                  (current-column)))
        (count (* dir (/ (window-body-height) 2)))
        (opoint (point))
        ;; Is the buffer edge we are scrolling towards already visible?
        (at-edge (if (< dir 0)
                     (<= (window-start) (point-min))
                   (>= (window-end nil t) (point-max)))))
    (setq my/scroll-half-page-column column)
    ;; Scroll window unless it's already at the buffer edge
    ;; (`scroll-up' with negative count scrolls backward)
    (unless at-edge
      (scroll-up count)
      ;; If the last line came into view, put it at the window bottom
      (when (and (> dir 0) (>= (window-end nil t) (point-max)))
        (save-excursion (goto-char (point-max))
                        (recenter -1))))
    ;; Move point by the same amount, independently of the scroll
    (goto-char opoint)
    (vertical-motion count)
    (move-to-column column)))

;;-----------------------------------------------------------------------------
;; Split window
;;-----------------------------------------------------------------------------

(defun my/window-split ()
  (interactive)
  (let ((new-window (split-window-below)))
    (balance-windows)
    (select-window new-window)
    (run-at-time 0 nil #'select-window new-window)))

(defun my/window-vsplit ()
  (interactive)
  (let ((new-window (split-window-right)))
    (balance-windows)
    (select-window new-window)
    (run-at-time 0 nil #'select-window new-window)))

;;-----------------------------------------------------------------------------
;; Find file
;;-----------------------------------------------------------------------------

(defun my/find-file-split (filename)
  (my/window-split)
  (find-file filename))

(defun my/find-file-vsplit (filename)
  (my/window-vsplit)
  (find-file filename))

(defun my/find-file-tab (filename)
  (tab-bar-new-tab)
  (find-file filename))

;;-----------------------------------------------------------------------------
;; Switch to buffer
;;-----------------------------------------------------------------------------

(defun my/switch-to-buffer-split (buffer)
  (my/window-split)
  (switch-to-buffer buffer))

(defun my/switch-to-buffer-vsplit (buffer)
  (my/window-vsplit)
  (switch-to-buffer buffer))

(defun my/switch-to-buffer-tab (buffer)
  (tab-bar-new-tab)
  (switch-to-buffer buffer))

;;-----------------------------------------------------------------------------
;;
;; Packages
;;
;;-----------------------------------------------------------------------------

;; Install it first to make :delight option available for other packages
(use-package delight
  :straight t)

;; Enable gcmh-mode first to decrease Emacs startup time
(use-package gcmh
  :straight t
  :delight gcmh-mode
  :config
  (gcmh-mode 1))

;; Part of magit project
(use-package git-modes
  :straight t)

;; Update buffers when files are modified on disk
;; (useful when working with Claude Code or Codex)
(use-package autorevert
  :straight t
  :delight auto-revert-mode
  :config
  (global-auto-revert-mode 1))

(use-package avy
  :straight t)

(use-package compile
  :straight (:type built-in)
  :init
  (defun my/setup-compilation-mode ()
    (visual-line-mode 1))

  :hook
  (compilation-mode . my/setup-compilation-mode)

  :custom
  (compilation-scroll-output t)
  ;; When enabled it can open Finder window instead
  (compilation-auto-jump-to-first-error nil))

(use-package consult
  :straight t
  :init
  (defun my/consult-project-flymake ()
    (interactive)
    (consult-flymake t))

  :custom
  (consult-async-input-debounce 0.01)
  (consult-async-input-throttle 0.01)
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.1)

  (consult-project-buffer-sources '(consult-source-project-buffer
                                    consult-source-project-recent-file))

  ;; TODO: --full-path matches against absolute path, not relative one.
  (consult-fd-args
   '("fd"
     "--full-path"
     "--color=never"
     ;; ----- custom args
     "-t" "f"
     "--hidden"
     "--no-ignore-vcs"
     "--ignore-file" (expand-file-name "~/.fdignore")))

  (consult-ripgrep-args
   '("rg"
     "--null"
     "--line-buffered"
     "--color=never"
     "--max-columns=1000"
     "--path-separator" "/"
     "--smart-case"
     "--no-heading"
     "--with-filename"
     "--line-number"
     "--search-zip"
     ;; ----- custom args
     "--hidden"
     "--no-ignore-vcs"
     "--ignore-file" (expand-file-name "~/.fdignore")))

  :custom-face
  (consult-file ((t (:foreground "#777777"))))

  :config
  (consult-customize
   consult-fd :prompt "Find: "
   consult-line :preview-key 'any :prompt "Filter: "
   consult-ripgrep :group nil :preview-key '(:debounce 0.3 any) :prompt "Search: "
   consult-xref :preview-key 'any :prompt "Filter: "))

(use-package corfu
  :straight t
  :after evil
  :init
  (global-corfu-mode 1)

  (defun my/corfu-quit ()
    (interactive)
    (corfu-quit)
    (evil-force-normal-state))

  :custom
  (corfu-count 8)
  (corfu-cycle t)
  (corfu-min-width 50)
  (corfu-preselect 'first)
  (corfu-preview-current nil)

  :custom-face
  (corfu-current ((t (:background "#D8E8F4"))))
  (corfu-default ((t (:background "#FFFFF6"))))

  :bind
  (:map corfu-map
        ("C-g" . my/corfu-quit)
        ([tab] . corfu-next)
        ([backtab] . corfu-previous)
        ([return] . corfu-complete)))

(use-package dart-mode
  :straight t)

;; - "C-x v [" - diff-hl-previous-hunk
;; - "C-x v ]" - diff-hl-next-hunk
(use-package diff-hl
  :straight t
  :hook
  ((magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh)))

;; - "(" - dired-hide-details-mode
;; - "C-p" - remove autosuggestion when renaming file
(use-package dired
  :straight (:type built-in)
  :init
  (defun my/setup-dired-mode ()
    (dired-hide-details-mode 1))

  ;; https://emacs.stackexchange.com/a/204/39266
  ;; Dired creates a separate buffer for each new directory you visit and
  ;; pressing `q' kills only one buffer at a time by default => you should
  ;; press `q' to exit as many times as many directories you have visited
  (defun my/kill-dired-buffers ()
    (interactive)
    (mapc (lambda (buffer)
            (when (eq (buffer-local-value 'major-mode buffer) 'dired-mode)
              (kill-buffer buffer)))
          (buffer-list)))

  :hook
  (dired-mode . my/setup-dired-mode)

  :custom
  (dired-listing-switches "-alh --group-directories-first")

  :bind
  (:map dired-mode-map
        ("p" . dired-up-directory)
        ("q" . my/kill-dired-buffers)))

(use-package dired-subtree
  :straight t
  :after dired
  :custom
  (dired-subtree-use-backgrounds t)

  :custom-face
  (dired-subtree-depth-1-face ((t (:background "#F6F6F6"))))
  (dired-subtree-depth-2-face ((t (:background "#EBEBEB"))))
  (dired-subtree-depth-3-face ((t (:background "#EBEBEB"))))
  (dired-subtree-depth-4-face ((t (:background "#EBEBEB"))))
  (dired-subtree-depth-5-face ((t (:background "#EBEBEB"))))
  (dired-subtree-depth-6-face ((t (:background "#EBEBEB"))))

  :bind
  (:map dired-mode-map
        ("TAB" . dired-subtree-toggle)))

(use-package dockerfile-mode
  :straight t
  :mode "\\`Dockerfile")

;; - eglot-events-buffer (show Eglot logs)
(use-package eglot
  ;; Built-in package since Emacs 29
  :straight (:type built-in)
  ;; Load Eglot lazily using `eglot-ensure' so that face/font caches have
  ;; time to be properly set up - otherwise Emacs might crash on startup
  :init
  (defun my/setup-eglot-managed-mode ()
    (setq-local eldoc-documentation-functions '(flymake-eldoc-function)))

  (defun my/eglot-before-save ()
    (when eglot--managed-mode
      (eglot-format-buffer)
      (call-interactively 'eglot-code-action-organize-imports)))

  :hook
  ((eglot-managed-mode . my/setup-eglot-managed-mode)
   (before-save . my/eglot-before-save)
   (dart-mode . eglot-ensure)
   (go-mode . eglot-ensure)
   (typescript-mode . eglot-ensure))

  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits nil)
  ;; Disable events buffer for performance reasons
  (eglot-events-buffer-size 0)
  ;; https://github.com/joaotavora/eglot/issues/334
  ;; :documentHighlightProvider - highlight variable at point
  ;; :documentOnTypeFormattingProvider - on-type formatting
  ;; :hoverProvider - fetch docs from LSP server (don't disable)
  ;; :inlayHintProvider - show inlay hints about variable types
  (eglot-ignored-server-capabilities '(:documentHighlightProvider
                                       :documentOnTypeFormattingProvider
                                       :inlayHintProvider))

  :config
  ;; https://github.com/emacs-lsp/lsp-mode/tree/master/clients
  ;; https://github.com/shuxiao9058/poly-emacs/blob/main/programming.org
  ;;
  ;; It's possible to use both map and list syntax (and mix them)
  (setq-default eglot-workspace-configuration
                ;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
                ;; (analyses . (:ST1005 :json-false))
                '((:gopls . ((gofumpt . t)
                             (matcher . "CaseSensitive")
                             (staticcheck . t)
                             ;; ST1000: Incorrect or missing package comment
                             (analyses . ((ST1000 . :json-false)
                                          (ST1005 . :json-false)))))))

  :bind
  (:map eglot-mode-map
        ;; https://emacs-lsp.github.io/lsp-mode/page/keybindings/
        ;; Default keybindings in lsp-mode
        ("s-l = =" . eglot-format-buffer)
        ("s-l a a" . eglot-code-actions)
        ("s-l g g" . xref-find-definitions)
        ("s-l g i" . eglot-find-implementation)
        ("s-l g r" . xref-find-references)
        ("s-l g t" . eglot-find-typeDefinition)
        ("s-l r o" . eglot-code-action-organize-imports)
        ("s-l r r" . eglot-rename)
        ;; Default keybindings in IntelliJ IDEA
        ("M-<return>" . eglot-code-action-quickfix)
        ("s-b" . xref-find-references)
        ("s-B" . eglot-find-typeDefinition)
        ("s-M-b" . eglot-find-implementation)))

(use-package emacs
  :straight (:type built-in)
  :custom
  ;; First indent current line, then complete
  (tab-always-indent 'complete)

  ;; https://github.com/minad/vertico#configuration
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

(use-package embark
  :straight t
  :init
  ;; embark-prompter is not loaded yet => declare it early enough
  ;; so that it can be dynamically bound in my/embark-act-with-key
  (defvar embark-prompter)

  ;; Reuse Embark's target detection and minibuffer quit handling
  ;; but skip the action prompt by returning the action bound to KEY
  (defun my/embark-act-with-key (key)
    "Act on the current Embark target with the action bound to KEY."
    (let ((embark-prompter
           (lambda (keymap _update)
             (let ((action (lookup-key keymap (kbd key))))
               (if (functionp action)
                   action
                 (user-error "No Embark action bound to %s" key))))))
      (embark-act)))

  (defun my/embark-split ()
    (interactive)
    (my/embark-act-with-key "C-s"))

  (defun my/embark-vsplit ()
    (interactive)
    (my/embark-act-with-key "C-v"))

  (defun my/embark-tab ()
    (interactive)
    (my/embark-act-with-key "C-t"))

  :custom
  (embark-indicators '(embark--vertico-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))

  :bind
  (:map minibuffer-mode-map
        ("C-u" . embark-act)
        ("C-l" . embark-export))

  (:map embark-file-map
        ("C-s" . my/find-file-split)
        ("C-v" . my/find-file-vsplit)
        ("C-t" . my/find-file-tab))

  (:map embark-buffer-map
        ("C-s" . my/switch-to-buffer-split)
        ("C-v" . my/switch-to-buffer-vsplit)
        ("C-t" . my/switch-to-buffer-tab)))

(use-package embark-consult
  :straight t
  :config
  ;;---------------------------------------------------------------------------
  ;; For consult-line and consult-flymake
  ;;---------------------------------------------------------------------------

  (defun my/consult-location-split (target)
    (my/window-split)
    (embark-consult-goto-location target))

  (defun my/consult-location-vsplit (target)
    (my/window-vsplit)
    (embark-consult-goto-location target))

  (defun my/consult-location-tab (target)
    (tab-bar-new-tab)
    (embark-consult-goto-location target))

  (defvar-keymap my/embark-consult-location-map
    :doc "Keymap for consult-location actions."
    :parent nil
    "C-s" #'my/consult-location-split
    "C-v" #'my/consult-location-vsplit
    "C-t" #'my/consult-location-tab)

  (add-to-list 'embark-keymap-alist
               '(consult-location my/embark-consult-location-map))

  ;;---------------------------------------------------------------------------
  ;; For consult-ripgrep
  ;;---------------------------------------------------------------------------

  (defun my/consult-grep-split (location)
    (my/window-split)
    (embark-consult-goto-grep location))

  (defun my/consult-grep-vsplit (location)
    (my/window-vsplit)
    (embark-consult-goto-grep location))

  (defun my/consult-grep-tab (location)
    (tab-bar-new-tab)
    (embark-consult-goto-grep location))

  (defvar-keymap my/embark-consult-grep-map
    :doc "Keymap for consult-grep actions."
    :parent nil
    "C-s" #'my/consult-grep-split
    "C-v" #'my/consult-grep-vsplit
    "C-t" #'my/consult-grep-tab)

  (add-to-list 'embark-keymap-alist
               '(consult-grep my/embark-consult-grep-map))

  ;;---------------------------------------------------------------------------
  ;; For consult-xref
  ;;---------------------------------------------------------------------------

  (defun my/consult-xref-split (candidate)
    (my/window-split)
    (embark-consult-xref candidate))

  (defun my/consult-xref-vsplit (candidate)
    (my/window-vsplit)
    (embark-consult-xref candidate))

  (defun my/consult-xref-tab (candidate)
    (tab-bar-new-tab)
    (embark-consult-xref candidate))

  (defvar-keymap my/embark-consult-xref-map
    :doc "Keymap for consult-xref actions."
    :parent nil
    "C-s" #'my/consult-xref-split
    "C-v" #'my/consult-xref-vsplit
    "C-t" #'my/consult-xref-tab)

  (add-to-list 'embark-keymap-alist
               '(consult-xref my/embark-consult-xref-map))

  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package eldoc
  :straight (:type built-in)
  :delight eldoc-mode
  :custom
  ;; Show all eldoc feedback
  (eldoc-documentation-strategy #'eldoc-documentation-compose)
  ;; Always show single line in echo area
  (eldoc-echo-area-use-multiline-p nil)
  ;; eldoc-box uses eldoc buffer to show docs so content for
  ;; eldoc-box is ready only after this number of seconds
  (eldoc-idle-delay 0.5))

(use-package eldoc-box
  :straight (eldoc-box :type git :host github :repo "tap349/eldoc-box")
  :commands eldoc-box-toggle-help-at-point
  :init
  (defun my/setup-eldoc-box-buffer ()
    (setq-local cursor-in-non-selected-windows nil)
    (setq-local show-trailing-whitespace nil))

  :hook
  (eldoc-box-buffer . my/setup-eldoc-box-buffer)

  :custom
  (eldoc-box-max-pixel-height 700)
  (eldoc-box-max-pixel-width 700)

  (eldoc-box-bottom-padding 2)
  (eldoc-box-position-offset '(9 . 4))

  :custom-face
  (eldoc-box-body ((t (:background "#F6F6F8"))))
  (eldoc-box-border ((t (:background "#C5C5C7"))))
  ;; (eldoc-box-body ((t (:background "#F9F9F5"))))
  ;; (eldoc-box-border ((t (:background "#C9C9C5"))))

  :config
  (advice-add 'keyboard-quit :before #'eldoc-box-quit-frame))

(use-package elec-pair
  :straight (:type built-in)
  :init
  (defun my/electric-pair-inhibit-predicate (_char)
    (minibufferp))

  :custom
  (electric-pair-inhibit-predicate #'my/electric-pair-inhibit-predicate)

  :config
  ;; Type "C-q (" if you don't want closing paren to be inserted
  (electric-pair-mode 1))

;; - "\" - evil-execute-in-emacs-state (switch to emacs state for one command)
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

  ;; Continue comment on next line unlike evil-open-below
  (defun my/evil-open-below ()
    (interactive)
    (evil-append-line 1)
    (comment-indent-new-line))

  ;; Bypass different checks and hooks in evil-normal-state command
  (defun my/evil-change-to-normal-state ()
    (interactive)
    (evil-change-state 'normal))

  :custom
  (evil-default-state 'emacs)

  ;; Can be useful to distinguish between <E> and Vim-like states
  (evil-mode-line-format '(after . mode-line-modified))

  (evil-ex-search-case 'smart)
  (evil-symbol-word-search t)

  (evil-auto-balance-windows t)
  (evil-visual-update-x-selection-p nil)

  ;; Customize evil-STATE-state-X variables in config section after evil
  ;; package is loaded or else they'll be overidden with default values
  :config
  (evil-mode 1)

  (evil-set-initial-state 'prog-mode 'normal)

  (evil-set-initial-state 'conf-toml-mode 'normal)
  (evil-set-initial-state 'gitignore-mode 'normal)
  (evil-set-initial-state 'go-dot-mod-mode 'normal)
  (evil-set-initial-state 'markdown-mode 'normal)
  (evil-set-initial-state 'yaml-mode 'normal)

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

  ;; Disable echo area messages on evil state change
  (setq evil-insert-state-message nil)
  (setq evil-normal-state-message nil)
  (setq evil-visual-state-message nil)
  (setq evil-replace-state-message nil)

  ;; Use <V> tag for all types of visual state (characterwise, linewise
  ;; and blockwise selection) so that the text in mode line doesn't jump
  (setq evil-visual-state-tag " <V> ")

  (setq evil-emacs-state-cursor '(bar . 2))
  (setq evil-insert-state-cursor '(bar . 2))

  :bind
  (:map evil-insert-state-map
        ("C-c" . my/evil-change-to-normal-state)
        ("RET" . comment-indent-new-line))

  (:map evil-normal-state-map
        ("C-c" . evil-ex-nohighlight)
        ("C-." . execute-extended-command)

        ("TAB" . save-buffer)
        ("C-o" . evil-switch-to-windows-last-buffer)

        ;; Should be faster than evil-next-line and evil-previous-line
        ("j" . next-line)
        ("k" . previous-line)

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

        ("C-w C-s" . my/window-split)
        ("C-w s" . my/window-split)
        ("C-w C-v" . my/window-vsplit)
        ("C-w v" . my/window-vsplit)

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

        ("<leader>d" . dired-jump))

  (:map evil-visual-state-map
        ("C-c" . evil-exit-visual-state)
        ("C-." . execute-extended-command)

        ("C-s" . sort-lines)

        ("H" . evil-first-non-blank)
        ("L" . evil-last-non-blank))

  (:map evil-replace-state-map
        ("C-c" . my/evil-change-to-normal-state))

  ;; https://github.com/noctuid/evil-guide#global-keybindings-and-evil-states
  ;; > motion state is the default the state for help-mode
  ;; > only keys bound in motion state will work in help-mode
  (:map evil-motion-state-map
        ("H" . evil-first-non-blank)
        ("L" . evil-last-non-blank)))

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode 1))

;; TODO: create package for searching with offset.
;;
;; See also evil-symbol-word-search variable
;; See https://emacs.stackexchange.com/a/76430 for alternative implementation
(use-package evil-visualstar
  :straight t
  ;; Eager load for searching with offset to work
  :demand t
  :after evil
  :init
  (setq my/evil-ex-search-offset 0)

  ;; Signatures of advised functions:
  ;; - before/around advice: arguments of original function
  ;; - around advice: orig-fun + arguments of original function
  ;; - original advised function always has zero arguments

  (defun my/evil-ex-search-set-offset (&rest _)
    (setq my/evil-ex-search-offset (- (point)
                                      (beginning-of-thing 'symbol))))

  (defun my/evil-ex-search-reset-offset (&rest _)
    (setq my/evil-ex-search-offset 0))

  (defun my/evil-ex-search-goto-offset (orig-fun &rest args)
    (when (and (eq evil-ex-search-direction 'backward)
               (> my/evil-ex-search-offset 0))
      ;; Or just call (apply orig-fun args)
      (goto-char (beginning-of-thing 'symbol)))
    (apply orig-fun args)
    (forward-char my/evil-ex-search-offset))

  (defun my/asterisk-z-normal ()
    (interactive)
    ;; count is 1 when you don't pass it or pass nil for all functions
    ;; using evil-ex-search under the hood (all evil search functions)
    (evil-ex-search-word-forward nil evil-symbol-word-search)
    (evil-ex-search-previous))

  (defun my/asterisk-z-unbounded-normal ()
    (interactive)
    (evil-ex-search-unbounded-word-forward nil evil-symbol-word-search)
    (evil-ex-search-previous))

  (defun my/asterisk-z-visual (beg end)
    (interactive "r")
    (evil-visualstar/begin-search-forward beg end)
    (evil-ex-search-previous))

  :config
  ;; evil-ex-start-word-search is used internally by
  ;; - evil-ex-search-word-forward (*)
  ;; - evil-ex-search-word-backward (#)
  ;; - evil-ex-search-unbounded-word-forward (g*)
  ;; - evil-ex-search-unbounded-word-backward (g#)
  (advice-add 'evil-ex-start-word-search :before #'my/evil-ex-search-set-offset)

  ;; evil-ex-start-search is used internally by
  ;; - evil-ex-search-forward (/)
  ;; - evil-ex-search-backward (?)
  (advice-add 'evil-ex-start-search :before #'my/evil-ex-search-reset-offset)

  ;; evil-ex-search is used internally by
  ;; - evil-ex-search-next (n)
  ;; - evil-ex-search-previous (N)
  (advice-add 'evil-ex-search :around #'my/evil-ex-search-goto-offset)

  ;; evil-visualstar/begin-search is used internally by
  ;; - evil-visualstar/begin-search-forward
  ;; - evil-visualstar/begin-search-backward
  (advice-add 'evil-visualstar/begin-search :before #'my/evil-ex-search-reset-offset)

  ;; global-evil-visualstar-mode is not enabled, define keybindings manually
  :bind
  (:map evil-normal-state-map
        ("z*" . my/asterisk-z-normal)
        ("zg*" . my/asterisk-z-unbounded-normal))

  ;; Search in visual state is always unbounded = word (\<, \>) and
  ;; symbol (\_<, \_>) boundaries are not added to search pattern
  (:map evil-visual-state-map
        ("*" . evil-visualstar/begin-search-forward)
        ("z*" . my/asterisk-z-visual)))

;;-----------------------------------------------------------------------------
;; Evil keybindings for other packages
;;-----------------------------------------------------------------------------

;; `with-eval-after-load' is redundant here because evil has been eagerly
;; loaded with `:demand t' - still keep it to document dependency cleanly
(with-eval-after-load 'evil
  ;; avy
  (define-key evil-normal-state-map (kbd "<leader>w") #'avy-goto-word-1)

  ;; diff-hl
  (define-key evil-normal-state-map (kbd "<leader>g") #'diff-hl-mode)

  ;; eldoc-box
  (define-key evil-normal-state-map (kbd "C-n") #'eldoc-box-toggle-help-at-point)

  ;; files
  (define-key evil-normal-state-map (kbd "<leader>,") #'find-sibling-file)
  (define-key evil-normal-state-map (kbd "<leader>v") #'my/find-sibling-file-vsplit)

  ;; magit
  (define-key evil-normal-state-map (kbd "<leader>M") #'magit)
  (define-key evil-normal-state-map (kbd "<leader>m") #'magit-log-buffer-file)

  ;; consult
  (define-key evil-normal-state-map (kbd "<leader>b") #'consult-project-buffer)
  (define-key evil-normal-state-map (kbd "<leader>f") #'consult-flymake)
  (define-key evil-normal-state-map (kbd "<leader>F") #'my/consult-project-flymake)
  (define-key evil-normal-state-map (kbd "<leader>n") #'consult-fd)
  (define-key evil-normal-state-map (kbd "<leader>/") #'consult-ripgrep)
  (define-key evil-normal-state-map (kbd "C-s") #'consult-line))

(with-eval-after-load 'go-mode
  (evil-define-key 'normal go-mode-map
    (kbd "<leader>t") #'my/go-test-current-test
    (kbd "<leader>T") #'my/go-test-current-package))

(use-package ffap
  :straight (:type built-in)
  :config
  ;; See ffap-bindings variable
  (global-set-key [remap find-file] #'find-file-at-point))

;; TODO: add functionality to create missing sibling file.
(use-package files
  :straight (:type built-in)
  :init
  (defun my/find-sibling-file-vsplit (file)
    (interactive (list buffer-file-name))
    (unless (find-sibling-file-search file)
      (user-error "Couldn't find any sibling files"))
    (my/window-vsplit)
    (call-interactively #'find-sibling-file))

  :custom
  (find-sibling-rules '(;; go-mode
                        ("\\([^/]+\\)_test\\.go\\'" "\\1\\.go")
                        ("\\([^/]+\\)\\.go\\'" "\\1_test\\.go"))))

;; - flymake-show-buffer-diagnostics (show all buffer errors)
(use-package flymake
  :straight (:type built-in)
  :bind
  (:map prog-mode-map
        ("M-]" . flymake-goto-next-error)
        ("M-[" . flymake-goto-prev-error)))

(use-package go-mode
  :straight t
  :init
  (defun my/setup-go-mode ()
    ;; Setup compilation-environment in Go buffer from which
    ;; `compile' is run (not in compilation buffer itself)
    (setq-local compilation-environment '("RUN_API_TESTS=1"
                                          "RUN_DB_TESTS=1"
                                          "RUN_KUBE_TESTS=1")))

  (defun my/setup-go-test ()
    (font-lock-add-keywords
     nil
     '(("^\s*--- FAIL.*" . 'go-test-fail)
       ("^\s*--- PASS.*" . 'go-test-pass))))

  (defun my/go-test-current-test ()
    (interactive)
    (let* ((rx "^func \\(([^)]*) \\)?\\(Test[[:alnum:]_]+\\)")
           (found (save-excursion (re-search-backward rx nil t))))
      (unless found (user-error "No Test* function found"))

      (let* ((receiver (match-string 1))
             (test-name (match-string 2))
             (cmd (if receiver
                      (format "go test -v -testify.m '^%s$'" test-name)
                    (format "go test -v -run '^%s$'" test-name))))
        (compilation-start cmd 'compilation-mode (lambda (_) "*go test*"))
        (pop-to-buffer "*go test*"))))

  (defun my/go-test-current-package ()
    (interactive)
    (let* ((cmd "go test -v ."))
      (compilation-start cmd 'compilation-mode (lambda (_) "*go test*"))
      (pop-to-buffer "*go test*")))

  (defun my/format-with-golangci-lint-fmt ()
    (interactive)
    (unless buffer-file-name
      (user-error "Current buffer is not visiting a file"))

    (let ((inhibit-message t)
          (old-point (point)))
      (save-buffer)
      (let ((exit-code (call-process
                        "golangci-lint"
                        nil
                        "*golangci-lint fmt errors*"
                        nil
                        "fmt"
                        buffer-file-name)))

        (unless (zerop exit-code)
          (display-buffer "*golangci-lint fmt errors*")
          (error "golangci-lint fmt failed"))

        (revert-buffer :ignore-auto :noconfirm)
        (goto-char old-point))))

  :hook
  ((go-mode . my/setup-go-mode)
   (compilation-mode . my/setup-go-test))

  :custom-face
  (go-test-pass ((t (:background "#77FF77" :foreground "#000000"))))
  (go-test-fail ((t (:background "#FFBBBB" :foreground "#000000")))))

;; - evil-toggle-fold (toggle folding)
(use-package hideshow
  :straight (:type built-in)
  :delight hs-minor-mode
  :hook
  (prog-mode . hs-minor-mode))

(use-package json-mode
  :straight t
  :mode ("\\.jsonc\\'" . jsonc-mode)
  :custom
  ;; Affects both JavaScript and JSON buffers
  (js-indent-level 2))

(use-package ls-lisp
  :straight (:type built-in)
  :custom
  (ls-lisp-dirs-first t)
  (ls-lisp-use-insert-directory-program nil))

(use-package markdown-mode
  :straight t
  :init
  (defun my/setup-markdown-mode ()
    (visual-line-mode 1))

  :hook
  (markdown-mode . my/setup-markdown-mode)

  :custom-face
  (markdown-code-face ((t (:font "Input-15"))))
  (markdown-inline-code-face ((t (:font "Input-15")))))

(use-package magit
  :straight t
  :init
  (defun my/setup-magit-log-mode ()
    (whitespace-mode -1))

  (defun my/setup-magit-revision-mode ()
    (setq-local require-final-newline nil)
    (whitespace-mode -1))

  :hook
  ((magit-log-mode . my/setup-magit-log-mode)
   (magit-revision-mode . my/setup-magit-revision-mode))

  :custom
  (magit-diff-paint-whitespace nil)
  (magit-diff-paint-whitespace-lines nil)

  ;; Use customize-face to see current colors interactively
  :custom-face
  (magit-section-highlight ((t (:background "#D0D5E5"))))
  (magit-diff-added ((t (:background "#DFFAE1"))))
  (magit-diff-removed ((t (:background "#F5E5E5"))))
  (magit-diff-hunk-heading-highlight ((t (:background "#C3C4CD"))))
  ;; (magit-diff-context-highlight ((t (:background "#E7E9ED"))))
  (magit-diff-added-highlight ((t (:background "#B7EBBC"))))
  (magit-diff-removed-highlight ((t (:background "#F3C1BF")))))

;; Fixes a bug in some consult commands (say, consult-xref) when you
;; cannot search for substring
(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  ;; https://github.com/minad/vertico/issues/237#issuecomment-1134000907
  ;;
  ;; completions-first-difference face is used by `basic' completion style
  ;; but not by orderless: if this variable is nil, orderless is used for
  ;; all completion categories => completions-first-difference is not used
  (completion-category-overrides nil))

;; - "C-c C-x C-f" - org-emphasize (surround with emphasis - bold, italic, code)
;; - "C-c C-," - org-insert-structure-template (insert template, say, src block)
;; - "C-M-\" - indent-region (indents the whole src block without selection)
(use-package org
  :straight (:type built-in)
  :init
  (defun my/setup-org-mode ()
    (visual-line-mode 1))

  (defun my/org-toggle-emphasis-markers ()
    (interactive)
    (setq-local org-hide-emphasis-markers (not org-hide-emphasis-markers))
    (font-lock-flush))

  :hook
  (org-mode . my/setup-org-mode)

  :custom
  (org-adapt-indentation nil)
  (org-hide-emphasis-markers nil)
  ;; Allow to set width of inline images
  (org-image-actual-width nil)
  ;; Insert newline before a new headline (for M-RET)
  (org-blank-before-new-entry '((heading . t)
                                (plain-list-item . auto)))

  :custom-face
  (org-level-1 ((t (:weight bold))))
  (org-level-2 ((t (:weight bold))))

  :bind
  (:map org-mode-map
        ("C-c e" . my/org-toggle-emphasis-markers)))

;; - yank-media (insert image from clipboard)
;;
;; - "C-c C-l" - org-insert-link (insert or edit link)
;; - "C-c C-x C-v" - org-link-preview (toggle display of link preview)
(use-package org-attach
  :straight (:type built-in)
  :custom
  (org-attach-use-inheritance t))

(use-package org-roam
  :straight t
  :init
  (defun my/org-roam-ripgrep ()
    (interactive)
    (consult-ripgrep org-roam-directory))

  :custom
  (org-roam-directory "~/org-roam")

  (org-roam-capture-templates
   `(("w" "work" plain "%?"
      :empty-lines-before 1
      :target (file+head
               "work/%<%Y%m%d%H%M%S>-${slug}.org"
               ,(concat "#+title: ${title}\n"
                        "#+filetags: :work:\n"
                        "#+startup: linkpreviews\n"))
      :unnarrowed t)

     ("n" "notes" plain "%?"
      :empty-lines-before 1
      :target (file+head
               "notes/%<%Y%m%d%H%M%S>-${slug}.org"
               ,(concat "#+title: ${title}\n"
                        "#+filetags: :note:\n"
                        "#+startup: linkpreviews\n"))
      :unnarrowed t)))

  ;; org-roam-node-find uses display template to find node
  (org-roam-node-display-template
   (concat "${title:*} "
           (propertize "${tags:40}" 'face 'org-tag)))

  :config
  (org-roam-db-autosync-mode t)

  :bind
  (;; Use for quick capture (closes capture window on save)
   ("C-c n c" . org-roam-capture)
   ("C-c n d" . org-roam-dailies-capture-today)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n /" . my/org-roam-ripgrep)))

;; - org-roam-ui-mode (launch org-roam-ui in browser)
(use-package org-roam-ui
  :straight t
  :after org-roam
  ;; Remove duplicate `org-roam-ui' mode names from the mode line
  ;; (one from org-roam-ui-mode, one from org-roam-ui-follow-mode)
  :delight org-roam-ui-follow-mode
  :custom
  (org-roam-ui-follow t)
  (org-roam-ui-open-on-start t)
  (org-roam-ui-sync-theme nil)
  (org-roam-ui-update-on-save t))

;; - "C-x p r" - project-query-replace-regexp (replace string in project)
(use-package project
  ;; Built-in package since Emacs 26
  ;; C-x p keymap is available since Emacs 28
  :straight (:type built-in)
  :custom
  (project-switch-commands #'consult-fd)

  :config
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Prefix-Keys.html
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Changing-Key-Bindings.html
  (keymap-set (current-global-map) "s-p" project-prefix-map))

(use-package project-tab-groups
  :straight t
  :config
  (project-tab-groups-mode 1))

(use-package rainbow-delimiters
  :straight t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :straight t
  :delight
  :hook
  (prog-mode . rainbow-mode))

;; Used by consult-project-buffer to list recent project files
(use-package recentf
  :straight (:type built-in)
  :config
  (recentf-mode 1))

;; For camel-case motions
(use-package subword
  :straight (:type built-in)
  :delight
  :config
  (global-subword-mode 1))

(use-package tab-bar
  :straight (:type built-in)
  :init
  ;; https://christiantietze.de/posts/2022/02/emacs-tab-bar-numbered-tabs/
  (defun my/tab-bar-tab-name-format-function (tab i)
    (propertize
     (concat " " (alist-get 'name tab) " ")
     'face (funcall tab-bar-tab-face-function tab)))

  (defun my/tab-bar-current-tab-group ()
    (let* ((tabs (funcall tab-bar-tabs-function))
           (current-tab (tab-bar--current-tab-find tabs))
           (current-group (funcall tab-bar-tab-group-function current-tab))
           (current-group (or current-group "--------"))
           (current-group (string-replace "dev-platform-" "" current-group)))
      (propertize
       (concat " " current-group " ")
       'face #'my/tab-bar-current-tab-group)))

  (defun my/tab-bar-move-tab-left ()
    (interactive)
    (tab-bar-move-tab -1))

  (defun my/tab-bar-move-tab-right ()
    (interactive)
    (tab-bar-move-tab 1))

  ;; When switching to another project with `project-switch-project',
  ;; tab group of current tab is updated by project-tab-groups package
  (defun my/tab-bar-update-tab-group ()
    (when (project-current)
      (tab-bar-change-tab-group (project-name (project-current)))))

  (defun my/jump-to-register-advice (register &optional _delete)
    (when (eq (car (get-register register)) 'file)
      (my/tab-bar-update-tab-group)))

  ;; Update tab group of current tab on Emacs startup
  (my/tab-bar-update-tab-group)

  :custom
  (tab-bar-auto-width-max '((170) 20))
  (tab-bar-close-button-show nil)
  (tab-bar-format '(tab-bar-format-tabs
                    tab-bar-separator
                    tab-bar-format-align-right
                    my/tab-bar-current-tab-group))
  (tab-bar-new-tab-choice t)
  ;; ZWSP is used to prevent last tab from filling all available space
  (tab-bar-separator "​")

  (tab-bar-tab-name-format-function #'my/tab-bar-tab-name-format-function)

  :custom-face
  (tab-bar ((t (:background "#F7F7FE"))))
  (tab-bar-tab ((t (:background "#D0D4D8" :box (:color "#A0A4A8" :style nil)))))
  (tab-bar-tab-inactive ((t (:background "#E4E4E8"))))

  (my/tab-bar-current-tab-group
   ((t (:background "#FCFCDF" :box (:color "#D5D58D" :style nil)))))

  :config
  ;; http://www.gonsie.com/blorg/tab-bar.html
  (tab-bar-mode 1)

  (advice-add 'jump-to-register :after #'my/jump-to-register-advice)

  :bind
  (("s-{" . tab-bar-switch-to-prev-tab)
   ("s-}" . tab-bar-switch-to-next-tab)
   ("C-s-{" . my/tab-bar-move-tab-left)
   ("C-s-}" . my/tab-bar-move-tab-right)
   ("s-t" . tab-bar-new-tab)
   ("s-w" . tab-bar-close-tab)
   ("C-<tab>" . tab-recent)
   ("C-<backspace>" . tab-bar-close-tab)))

(use-package typescript-mode
  :straight t)

(use-package ultra-scroll
  :straight t
  :custom
  (scroll-conservatively 3)
  ;; 0 is required for glitch-free scrolling
  (scroll-margin 0)

  :config
  (ultra-scroll-mode 1))

(use-package vertico
  :straight t
  :after consult
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
  (("C-x p b" . consult-project-buffer)
   ("M-g f" . consult-flymake)
   ("M-s d" . consult-fd)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line))

  (:map vertico-map
        ("C-s" . my/embark-split)
        ("C-v" . my/embark-vsplit)
        ("C-t" . my/embark-tab)))

;; - "C-x v =" - vc-diff (git diff for current file)
;; - "C-x v g" - vc-annotate (git blame)
;;   - "=" - view diff
;;   - "f" - view file at revision
;;   - "l" - view commit message
;; - "C-x v h" - vc-region-history (git log of current region with diff)
;; - "C-x v l" - vc-print-log (git log of current file)
;;
;; Common navigation keybindings:
;;   - "n" or "p" - move between commits
;;   - "f" - visit file as of commit at point
(use-package vc
  :straight (:type built-in))

(use-package whitespace
  :straight (:type built-in)
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
  ;; Use `:background unspecified' instead of `:background "white"'
  ;; to allow background of vertico-current to override it
  (whitespace-tab ((t (:background unspecified :foreground "#DDDDDD"))))
  (whitespace-trailing ((t (:background "#E3A8A8" :foreground "#C38888"))))
  (whitespace-missing-newline-at-eof ((t (:background "#E3A8A8"))))

  :config
  (global-whitespace-mode 1))

;; https://emacs.stackexchange.com/a/61387
;;
;; - "C-]" - xref-find-definitions
;; - "M-?" - xref-find-references
(use-package xref
  :straight (:type built-in)
  :custom
  ;; - xref-find-definitions jumps to the definition right away
  ;;   when only one definition is found
  ;; - xref-find-references always shows references list - even
  ;;   if only one reference is found (true for type definitions
  ;;   which are displayed with xref-find-references)
  (xref-show-definitions-function #'consult-xref)
  ;; In Emacs 27+ it will affect all xref-based commands
  ;; except for xref-find-definitions
  (xref-show-xrefs-function #'consult-xref))

(use-package yaml-mode
  :straight t)

;;-----------------------------------------------------------------------------
;;
;; Managed by customize-group
;;
;;-----------------------------------------------------------------------------

(put 'narrow-to-region 'disabled nil)
