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
   dirtree
   doremi
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
   powerline-evil
   projectile
   queue
   rainbow-delimiters
   rainbow-identifiers
   remember-theme
   s
   simple-httpd
   slime
   smex
   smooth-scroll
   ssh
   string-utils
   tex-math-preview
   tree-mode
   undo-tree
   visual-regexp
   windata
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
        (setq fresh-installs (append fresh-installs (symbol-name package)))
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
