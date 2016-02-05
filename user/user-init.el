(let ((default-directory (concat user-emacs-directory "user/")))
  (normal-top-level-add-subdirs-to-load-path)
  (setq emacs-config/dir default-directory))

(defvar emacs-config/local-dir (concat emacs-config/dir "local/"))
(defvar emacs-config/local-file
  (concat emacs-config/local-dir "setup-local.org"))

(defun has-local-config ()
  "Returns non-nil if the file
`emacs-config/dir'/local/setup-local.org exists."
  (file-exists-p emacs-config/local-file))

(use-package emacs-config-loaddefs
  :ensure nil
  :load-path "user/emacs-config/"
  :config
  (emacs-config/init)
  :init
  ;; When in org mode, try to tangle the file if it is an emacs-config/file -
  ;; whether it is is checked by `emacs-config/tangle-file`
  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        (lambda ()
                          (emacs-config/tangle-file (buffer-file-name) t))
                        nil 'make-it-local))))

;; Very General Setup
(require-all '(general-functions
               general-settings))

;; Load packages that don't require any setup
(require-all '(setup-simple-packages))

;; Specific Setups
;; Bootstrap some stuff that every config file can rely on
(require-all '(bootstrap
               setup-look
               setup-PATH ;; No Packages
               setup-system ;; No Packages
               setup-keys ;; No Packages
               setup-buffers-windows ;; No Packages
               setup-movement
               setup-editing
               setup-misc
               ))

;; Mode-specific setups
(require-all '(setup-git
               setup-helm
               setup-projectile
               setup-text-mode
               setup-programming
               setup-org
               setup-deft
               setup-which-key
               setup-engine-mode
               ))

;; Load the machine-local setup if present
(when (has-local-config)
  (require 'setup-local))

(provide 'user-init)
