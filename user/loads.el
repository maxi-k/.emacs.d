;; All the packages that should be installed
;; Needs to be updated manually
(setq
 my/packages
 '(ace-jump-mode
   async
   auto-complete
   browse-kill-ring
   browse-url-dwim
   buffer-move
   cider
   clj-refactor
   clojure-mode
   clojure-snippets
   command-log-mode
   dash
   dash-at-point
   discover
   dirtree
   doremi
   drag-stuff
   elisp-slime-nav
   elm-mode
   emmet-mode
   epl
   evil
   evil-god-state
   evil-leader
   evil-org
   evil-paredit
   evil-surround
   evil-visualstar
   expand-region
   f
   fill-column-indicator
   flycheck
   fiplr
   git-commit-mode
   git-rebase-mode
   god-mode
   goto-chg
   haskell-mode
   helm
   highlight-indentation
   htmlize
   idle-highlight-mode
   ido-ubiquitous
   iy-go-to-char
   key-chord
   latex-preview-pane
   list-utils
   lorem-ipsum
   magit
   markdown-mode
   markdown-mode+
   multiple-cursors
   org
   pabbrev
   paredit
   php-mode
   pkg-info
   popup
   powerline
   projectile
   queue
   rainbow-delimiters
   rainbow-identifiers
   s
   simple-httpd
   slime
   smex
   smooth-scroll
   ssh
   string-utils
   tagedit
   tex-math-preview
   tree-mode
   twig-mode
   undo-tree
   visual-regexp
   windata
   writeroom-mode
   yasnippet))

(defun install-missing-packages ()
  (interactive)
  ;; Fetch a list of packages if available
  (unless package-archive-contents
    (package-refresh-contents))
  ;; install the missing packages
  (let ((fresh-installs '()))
    (dolist (package my/packages)
      (unless (package-installed-p package)
        (setq fresh-installs (cons (symbol-name package) fresh-installs))
        (package-install package)))
    (message "Installed the following packages: %s"
             (mapconcat 'identity fresh-installs ", "))))

;; Prompt the user to add the package to my/packages
(defadvice package-install (after add-to-pkg-list activate)
  (find-alternate-file-other-window (concat user-emacs-directory "user/loads.el")))

;; Prompt the user to remove the package from my/packages
(defadvice package-delete (after remove-from-pkg-list activate)
  (find-alternate-file-other-window (concat user-emacs-directory "user/loads.el")))

(mapc 'require
      my/packages)

(provide 'loads)
