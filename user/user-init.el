(let ((default-directory "~/.emacs.d/user/"))
  (normal-top-level-add-subdirs-to-load-path))
;;;;;; LOADING GENERAL ;;;;;;
(require 'ob-tangle)
(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))
;; load up all literate org-mode files in this directory
(mapc #'org-babel-load-file (directory-files dotfiles-dir t "\\.org$"))

;; Loading packages
(require 'loads)

;; Loading all the keybinds
(require 'keybinds)

;; Loading custom functions
(require 'functions)

;; Loading custom behaviors
(require 'behaviors)

;; Loading advices for built-in functions
(require 'advices)

;; Loading custom views
(require 'look)

;; Setting mode-specific stuff
(require 'mode-settings)

(delete-dups (add-PATH-to-exec-path))

(provide 'user-init)
