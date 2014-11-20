;; Bind the cmd key to be the super key
(setq mac-command-modifier 'super)
;; Bind the fn key to be the hyper key
(setq ns-function-modifier 'hyper)

;; Set up german umlaut support
(global-set-key (kbd "H-a") (lambda () (interactive) (insert "ä"))) ;; ä
(global-set-key (kbd "H-A") (lambda () (interactive) (insert "Ä"))) ;; Ä
(global-set-key (kbd "H-o") (lambda () (interactive) (insert "ö"))) ;; ö
(global-set-key (kbd "H-O") (lambda () (interactive) (insert "Ö"))) ;; Ö
(global-set-key (kbd "H-u") (lambda () (interactive) (insert "ü"))) ;; ü
(global-set-key (kbd "H-U") (lambda () (interactive) (insert "Ü"))) ;; Ü
(global-set-key (kbd "H-s") (lambda () (interactive) (insert "ß"))) ;; ß

;; Set up god mode
(global-set-key (kbd "s-,") 'god-mode-all)
(define-key god-local-mode-map (kbd "i") 'god-local-mode)
(define-key god-local-mode-map (kbd ".") 'repeat)

;; Set up evil mode
(global-set-key (kbd "s-<escape>") 'evil-mode)

;; Rebind the 'execute command' key to smex
(global-set-key (kbd "M-x") 'smex)

;; Make repeating easier
(global-set-key (kbd "C-.") 'repeat)

;; NERDTree for emacs: dirtree
(global-set-key (kbd "s-t") 'dirtree)

;; Org-mode agenda stuff
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c t") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;; Cycle through the mark ring more quickly
;; jump-to-mark defined in commands.el
(global-set-key (kbd "M-`") 'jump-to-mark)

;; Ace jump mode
(global-set-key (kbd "s-j") 'ace-jump-mode)
(global-set-key (kbd "s-J") 'ace-jump-char-mode)

;; Go to chars forward an backward more easily
(global-set-key (kbd "s-f") 'iy-go-to-char)
(global-set-key (kbd "s-b") 'iy-go-to-char-backward)

;; Go to line nr
(global-set-key (kbd "s-L") 'goto-line)

;; Expand the region one step further out
(global-set-key (kbd "s-e") 'er/expand-region)

;; Multiple cursors
(global-set-key (kbd "s-d") 'mc/mark-next-like-this)
(global-set-key (kbd "s-g") 'mc/mark-all-like-this)
;; (global-set-key (kbd "C-s-d") 'mc /mark-all-dwim)

;; Kill the current line if there is no active region
(global-set-key (kbd "M-w") 'save-region-or-current-line)

;; Projectile is awesome!
(global-unset-key (kbd "s-p"))
(setq projectile-keymap-prefix (kbd "s-p"))

;; Completion that uses many different methods to find options.
(global-set-key (kbd "M-/") 'hippie-expand)

;; Easier way to de-/increase the text size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Open the buffer menu, not the list
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; M-i for back-to-indentation
(global-set-key (kbd "M-i") 'back-to-indentation)

;; M-SPC is bound to iTerm already
(global-set-key (kbd "H-SPC") 'just-one-space)

;; C-h and M-h for deleting backwards
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "M-h") 'kill-region-or-backward-word)

;; Clean up the buffer
(global-set-key (kbd "s-c") 'cleanup-buffer)

(fset 'quick-switch-buffer [?\C-x ?b return])

(global-set-key (kbd "s-v") 'quick-switch-buffer)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Jump to a definition in the current file. (Protip: this is awesome.)
(global-set-key (kbd "s-i") 'imenu)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)

;; Window switching. (C-x o goes to the next window)
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Help should search more than just commands
(global-set-key (kbd "C-h a") 'apropos)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c e") 'eval-and-replace)
;; Eval and insert after s-exp
(global-set-key (kbd "C-c i") 'eval-and-insert)
;; Eval and insert after s-exp as comment
(global-set-key (kbd "C-c c") 'eval-and-insert-as-comment)

;; M-S-6 is awkward
(global-set-key (kbd "s-l") 'join-line)

;; So good!
(global-set-key (kbd "C-c g") 'magit-status)

(define-key Buffer-menu-mode-map (kbd "a") 'Buffer-menu-this-window)
(add-hook 'org-mode-hook (lambda ()
                           (local-set-key (kbd "M-n") 'outline-next-visible-heading)
                           (local-set-key (kbd "M-p") 'outline-previous-visible-heading)
                           ;; table
                           (local-set-key (kbd "H-i") 'org-table-insert-row)
                           (local-set-key (kbd "H-k") 'org-table-kill-row)
                           (local-set-key (kbd "C-M-y") 'org-table-paste-rectangle)
                           (local-set-key (kbd "C-M-l") 'org-table-sort-lines)
                           ;; display images
                           (local-set-key (kbd "M-I") 'org-toggle-iimage-in-org)
                           ;; fix tab
                           (local-set-key (kbd "C-y") 'yank)
                           ;; Make todos easier
                           (local-set-key (kbd "s-t") 'org-todo)))

(provide 'keybinds)
