;;-----------------------------------------------------------------------------
;;
;; Packages
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

(straight-use-package 'auto-package-update)
(straight-use-package 'avy)
(straight-use-package 'cider)
(straight-use-package 'company)
(straight-use-package 'counsel)
(straight-use-package 'delight)
(straight-use-package 'dired-subtree)
(straight-use-package 'dockerfile-mode)
(straight-use-package 'evil)
(straight-use-package 'evil-nerd-commenter)
(straight-use-package 'evil-surround)
(straight-use-package 'evil-visualstar)
(straight-use-package 'flycheck)
(straight-use-package 'iedit)
(straight-use-package 'json-mode)
(straight-use-package 'kotlin-mode)
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'magit)
(straight-use-package 'projectile)
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'yaml-mode)

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

(add-hook 'after-make-frame-functions 'startup)

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
(set-face-attribute 'default nil :font "Input-15")
(set-face-attribute 'default nil :font "Input-15")
(set-face-foreground 'fill-column-indicator "#DEE4EF")
(set-face-foreground 'vertical-border "#D8D8DE")

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
;; Folding
;;
;;-----------------------------------------------------------------------------

;; hs-minor-mode can be enabled not in all major modes
;; Use evil-toggle-fold to toggle folding
(add-hook 'prog-mode-hook 'hs-minor-mode)

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
;; evil
;;
;; NOTE: evil package should come first so that other packages can define
;;       their keybindings in evil state keymaps and use leader key
;;
;; For some reason <return> and RET keys are not the same: keybinding for
;; <return> key in evil-normal-state-map (insert newline below) is also
;; active in dired-mode but keybinding for RET is not.
;; Most likely RET key has lower precedence and can be overriden by other
;; modes while <return> can't be
;; => use RET and TAB where possible to allow other modes to override them
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
      ;; (evil-complete-next)
      (company-complete))))

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
  (iedit--quit)
  (keyboard-quit))

(define-key evil-normal-state-map (kbd "C-g") 'my/keyboard-quit)
(define-key evil-normal-state-map (kbd "C-.") 'execute-extended-command)

(define-key evil-normal-state-map (kbd "TAB") 'save-buffer)
(define-key evil-normal-state-map (kbd "C-o") 'evil-switch-to-windows-last-buffer)

(define-key evil-normal-state-map (kbd "H") 'evil-first-non-blank)
(define-key evil-normal-state-map (kbd "L") 'evil-last-non-blank)

(define-key evil-normal-state-map (kbd "RET") 'my/insert-newline-below)
;; S-RET translates to just RET
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

(define-key evil-normal-state-map (kbd "<backspace>") 'evil-toggle-fold)

(define-key evil-normal-state-map (kbd "<leader>t") 'dired-jump)

;; https://github.com/noctuid/evil-guide#binding-keys-to-keys-keyboard-macros
(evil-define-key 'normal 'global "gp" "`[v`]")

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
;;
;; - "C-c C-x j j" - cider-jack-in
;; - "C-c C-d C-c" - cider-clojuredocs
;; - "C-c C-d C-w" - cider-clojuredocs-web
;;-----------------------------------------------------------------------------

(setq eldoc-echo-area-use-multiline-p nil)

(add-hook 'cider-test-report-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))

;; For *cider-clojuredocs* buffer
(add-hook 'cider-popup-buffer-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))

(evil-define-key 'normal 'cider-repl-mode-map
  ;; Close *cider-error* window with q
	"q" 'cider-popup-buffer-quit-function)

;;-----------------------------------------------------------------------------
;; clojure-mode
;;
;; See also project level configuration in .dir-locals.el
;;-----------------------------------------------------------------------------

;; https://stackoverflow.com/a/4200242
;; Fix indentation for failjure library (indent like `let`)
(put 'f/attempt-all 'clojure-indent-function 1)
(put 'f/try-all 'clojure-indent-function 1)
(put 'f/when-failed 'clojure-indent-function 1)
(put 'f/when-let-failed? 'clojure-indent-function 1)

;;-----------------------------------------------------------------------------
;; company
;;-----------------------------------------------------------------------------

(add-hook 'after-init-hook 'global-company-mode)

;; http://company-mode.github.io/manual/Customization.html#Customization
;; Disable automatic completion
(setq company-idle-delay nil)
(setq company-selection-wrap-around t)
(setq company-require-match nil)

;; http://company-mode.github.io/manual/Frontends.html#Frontends
(setq company-tooltip-align-annotations t)
(setq company-tooltip-minimum 4)
(setq company-tooltip-limit 8)
(setq company-tooltip-width-grow-only t)
(setq company-tooltip-margin 1)
(setq company-format-margin-function 'company-vscode-light-icons-margin)

(with-eval-after-load 'company
  ;; Use <tab> instead of TAB to override other keybindings
  (define-key company-active-map (kbd "<tab>") 'company-complete-common))

;;-----------------------------------------------------------------------------
;; counsel (ivy / counsel / swiper)
;;-----------------------------------------------------------------------------

(ivy-mode 1)

;; https://github.com/junegunn/fzf#respecting-gitignore
;; For counsel-fzf
(setenv
 "FZF_DEFAULT_COMMAND"
 "fd --type f --strip-cwd-prefix -H -I \
    --exclude target \
    --exclude .git \
    --exclude .cpcache \
    --exclude .clj-kondo \
    --exclude .lsp \
    --exclude .gradle")

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
(setq ivy-on-del-error-function 'ignore)
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
;; delight
;;-----------------------------------------------------------------------------

(delight '((auto-revert-mode nil autorevert)
           (cider-mode " CIDER" cider)
           (company-mode nil company)
           (eldoc-mode nil t)
           (flycheck-mode nil flycheck)
           (hs-minor-mode nil hideshow)
           (ivy-mode nil ivy)
           (lsp-mode " LSP" lsp-mode)
           (projectile-mode nil projectile)))

;;-----------------------------------------------------------------------------
;; dired
;;
;; - "(" - dired-hide-details-mode
;;-----------------------------------------------------------------------------

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "p") 'dired-up-directory))

;;-----------------------------------------------------------------------------
;; dired-subtree
;;-----------------------------------------------------------------------------

(setq dired-subtree-use-backgrounds t)

(with-eval-after-load 'dired-subtree
  (set-face-background 'dired-subtree-depth-1-face "#F4F4F4")
  (set-face-background 'dired-subtree-depth-2-face "#E4E4E4")
  (set-face-background 'dired-subtree-depth-3-face "#D0D0D0")
  (set-face-background 'dired-subtree-depth-4-face "#D0D0D0")
  (set-face-background 'dired-subtree-depth-5-face "#D0D0D0")
  (set-face-background 'dired-subtree-depth-6-face "#D0D0D0"))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "TAB") 'dired-subtree-toggle))

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
;; iedit
;;-----------------------------------------------------------------------------

;; Use narrowing to apply editing to part of buffer:
;; - "C-x n d" - narrow-to-defun
;; - "C-x n n" - narrow-to-region
;; - "C-x n w" - widen
(define-key evil-normal-state-map (kbd "C-t") 'iedit-mode)

;;-----------------------------------------------------------------------------
;; kotlin-mode
;;-----------------------------------------------------------------------------

;; Unset default keybindings - REPL integration provided by this package
;; is not very useful
(with-eval-after-load 'kotlin-mode
  (define-key kotlin-mode-map (kbd "C-c C-z") nil)
  (define-key kotlin-mode-map (kbd "C-c C-n") nil)
  (define-key kotlin-mode-map (kbd "C-c C-r") nil)
  (define-key kotlin-mode-map (kbd "C-c C-c") nil)
  (define-key kotlin-mode-map (kbd "C-c C-b") nil))

;;-----------------------------------------------------------------------------
;; lsp-mode
;;
;; https://emacs-lsp.github.io/lsp-mode/tutorials/clojure-guide
;; https://clojure-lsp.io/settings
;;-----------------------------------------------------------------------------

;; https://github.com/emacs-lsp/lsp-mode/issues/2600
;;
;; clojure-lsp returns 'lsp-request: Internal error' when trying
;; to call `evil-indent` on the last line of namespace file
;; => Use formatting provided by Emacs by default
(setq lsp-enable-indentation nil)

;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
;; https://github.com/syl20bnr/spacemacs/issues/14292
(setq lsp-completion-show-detail t)
(setq lsp-completion-show-kind t)
;; Use ElDoc provided by CIDER
(setq lsp-eldoc-enable-hover nil)
(setq lsp-enable-symbol-highlighting nil)
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-lens-enable nil)
(setq lsp-modeline-code-actions-enable nil)
(setq lsp-modeline-diagnostics-enable nil)
(setq lsp-signature-auto-activate nil)
(setq lsp-signature-render-documentation nil)

;; https://emacs-lsp.github.io/lsp-mode/page/keybindings/
;; lsp-keymap-prefix is "s-l" by default
;;
;; - "s-l = =" - lsp-format-buffer
;; - "s-l g g" - lsp-find-definition
;; - "s-l g r" - lsp-find-references
;; - "s-l g i" - lsp-find-implementation
;; - "s-l g t" - lsp-find-type-definition
;; - "s-l g d" - lsp-find-declaration
;; - "s-l r o" - lsp-organize-imports
;; - "s-l r r" - lsp-rename
;; - "s-l a a" - lsp-execute-code-action
(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-]") 'lsp-find-definition))

;; -------------------- Clojure -----------------------------------------------

(add-hook 'clojure-mode-hook 'lsp)

(setq lsp-clojure-custom-server-command '("bash" "-c" "~/soft/clojure-lsp"))

(defun my/lsp-clojure-add-save-hooks ()
  ;; Calls cljfmt on current buffer
  (add-hook 'before-save-hook 'lsp-format-buffer))

(add-hook 'clojure-mode-hook 'my/lsp-clojure-add-save-hooks)

;; -------------------- Kotlin ------------------------------------------------

(add-hook 'kotlin-mode-hook 'lsp)

(setq lsp-clients-kotlin-server-executable
      "~/soft/kotlin-lsp/bin/kotlin-language-server")

(setq lsp-kotlin-completion-snippets-enabled nil)
(setq lsp-kotlin-debug-adapter-enabled nil)

;;-----------------------------------------------------------------------------
;; lsp-ui
;;-----------------------------------------------------------------------------

(setq lsp-ui-sideline-enable nil)
(setq lsp-ui-peek-enable nil)
(setq lsp-ui-menu-enable nil)

(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-position 'at-point)
(setq lsp-ui-doc-show-with-cursor nil)
(setq lsp-ui-doc-show-with-mouse nil)
(setq lsp-ui-doc-header t)
(setq lsp-ui-doc-include-signature t)
(setq lsp-ui-doc-max-width 80)
(setq lsp-ui-doc-max-height 16)
(setq lsp-ui-doc-enhanced-markdown nil)

(define-key evil-normal-state-map (kbd "C-n") 'lsp-ui-doc-toggle)

;; https://github.com/emacs-lsp/lsp-ui/issues/369
(with-eval-after-load 'lsp-ui-doc
  (set-face-background 'lsp-ui-doc-background "#FAF4EB")
  (set-face-background 'lsp-ui-doc-header "#C8DFEA"))

;;-----------------------------------------------------------------------------
;; markdown-mode
;;-----------------------------------------------------------------------------

(with-eval-after-load 'markdown-mode
  (set-face-attribute 'markdown-code-face nil :font "Input-15")
  (set-face-attribute 'markdown-inline-code-face nil :font "Input-15"))

;;-----------------------------------------------------------------------------
;; projectile
;;
;; It allows counsel-fzf to search from project root regardless of current file
;;-----------------------------------------------------------------------------

(projectile-mode 1)

(setq projectile-completion-system 'ivy)
(setq projectile-create-missing-test-files t)

(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)

(defun my/toggle-test-vsplit ()
  (interactive)
  (my/evil-window-vsplit)
  (projectile-toggle-between-implementation-and-test))

(define-key evil-normal-state-map (kbd "<leader>,")
  'projectile-toggle-between-implementation-and-test)
(define-key evil-normal-state-map (kbd "<leader>v")
  'my/toggle-test-vsplit)

;;-----------------------------------------------------------------------------
;; rainbow-delimiters
;;-----------------------------------------------------------------------------

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;-----------------------------------------------------------------------------
;; tab-bar
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

(setq tab-bar-tab-name-format-function 'my/tab-bar-tab-name-format-function)

(global-set-key (kbd "s-{") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "s-}") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "s-t") 'tab-bar-new-tab)
(global-set-key (kbd "s-w") 'tab-bar-close-tab)
(global-set-key (kbd "C-<backspace>") 'tab-bar-close-tab)

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
