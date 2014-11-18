(let ((default-directory "~/.emacs.d/user/"))
  (normal-top-level-add-subdirs-to-load-path))
;;;;;; LOADING GENERAL ;;;;;;

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

(provide 'user-init)
