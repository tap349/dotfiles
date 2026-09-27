;; -*- lexical-binding: t; -*-

(add-to-list 'default-frame-alist '(undecorated . t))

;; Use straight.el instead of package.el
(setq package-enable-at-startup nil)

;; https://emacsnotes.wordpress.com/2022/09/11/three-bonus-keys-c-i-c-m-and-c-for-your-gui-emacs-all-with-zero-headache/
(add-hook
 'after-make-frame-functions
 (defun setup-blah-keys (frame)
   (with-selected-frame frame
     (when (display-graphic-p)
       (define-key input-decode-map (kbd "C-m") [BLAH-m])))))

;; Set correct deployment target for native compilation
(when (eq system-type 'darwin)
  (setenv "MACOSX_DEPLOYMENT_TARGET"
          (string-trim (shell-command-to-string "sw_vers -productVersion"))))
