(let ((default-directory (concat user-emacs-directory "user/")))
  (normal-top-level-add-subdirs-to-load-path)
  (setq emacs-config/dir default-directory)
  (add-to-list 'load-path (concat default-directory "plugins/ob-session-async/lisp/")))

(use-package emacs-config
  :load-path "user/emacs-config/"
  :ensure nil
  :config
  :init
  (require 'emacs-config-autoloads)
  (emacs-config/init)
  ;; When in org mode, try to tangle the file if it is an emacs-config/file -
  ;; whether it is is checked by `emacs-config/tangle-file`
  (add-hook 'org-mode-hook
            (function
             (lambda ()
               (add-hook 'after-save-hook
                         #'emacs-config/tangle-current-file
                         nil 'make-it-local)))))

;; Very General Setup
(require-all '(general-functions
               general-settings))

;; Load packages that don't require any setup
;; (require-all '(setup-simple-packages))

;; Specific Setups
;; Bootstrap some stuff that every config file can rely on
(require-all '(bootstrap
               setup-evil-mode
               setup-look
               setup-PATH ;; No Packages
               setup-system ;; No Packages
               setup-keys ;; No Packages
               setup-buffers-windows ;; No Packages
               setup-movement
               setup-editing
               setup-projects
               setup-misc
               ))

;; Mode-specific setups
(require-all '(setup-git
               setup-helm
               setup-text-mode
               setup-programming
               setup-org
               setup-deft
               setup-which-key
               setup-engine-mode
               setup-eshell
               ))

;; Load the machine-local setup if present
(when (has-local-config)
  (require 'setup-local))


(provide 'user-init)
