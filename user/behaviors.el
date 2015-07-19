;; Start emacs as a server
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))

;; call function now
(set-exec-path-from-shell-PATH)

;; Use spotlight for the locate command
(setq locate-command "mdfind")

;; Start a scratch-org buffer
(org-scratch-buffer)

;; remember-notes saves the notes after emacs quits
(custom-set-variables '(initial-buffer-choice 'remember-notes))

;; Some general stuff
(setq default-directory "~"
      diff-switches "-u"
      ediff-window-setup-function 'ediff-setup-windows-plain
      inhibit-startup-message t
      initial-scratch-message nil
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-yank-at-point t
      oddmuse-directory "~/.emacs.d/data/oddmuse"
      save-place-file "~/.emacs.d/data/places"
      scroll-step 1
      sentence-end-double-space nil
      ;; set-mark-command-repeat-pop t
      shift-select-mode nil
      transient-mark-mode t
      uniquify-buffer-name-style 'forward
      whitespace-line-column 80
      whitespace-style '(face trailing lines-tail tabs))

;; Setup backups
(setq backup-by-copying t
      backup-directory-alist
      '(("." . "~/.emacs.d/backups"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Don't open a new buffer every time in dired when drilling down
(put 'dired-find-alternate-file 'disabled nil)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Set the defaults
(set-default 'indent-tabs-mode nil)
(set-default 'imenu-auto-rescan t)

;; Set some aliases for simpler commands
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)

(random t) ;; Seed the random-number generator

(provide 'behaviors)
