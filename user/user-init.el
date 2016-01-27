(require 'emacs-config)

;; Very General Setup &
;; Bootstrap some stuff that every config file can rely on
(require-all '(bootstrap
               general-functions
               general-settings))

;; Load packages that don't require any setup
(require-all '(setup-simple-packages))

;; Specific Setups
(require-all '(setup-PATH ;; No Packages
               setup-system ;; No Packages
               setup-keys ;; No Packages
               setup-buffers-windows ;; No Packages
               setup-movement
               setup-editing
               setup-misc
               ))

;; Mode-specific setups
(require-all '(setup-evil-mode ;; TODO
               setup-git
               setup-helm
               setup-projectile
               setup-text-mode
               setup-programming
               setup-org
               setup-deft
               setup-which-key
               setup-engine-mode
               setup-look
               ))

;; Load the machine-local setup if present
(when (has-local-config)
  (require 'setup-local))

(provide 'user-init)
