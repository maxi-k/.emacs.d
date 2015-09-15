;; All the packages that should be installed
;; Needs to be updated manually
(setq
 my/packages
 '(ace-jump-mode
   async
   auto-complete
   browse-kill-ring
   browse-url-dwim
   cider
   clj-refactor
   clojure-mode
   clojure-snippets
   dash
   deft
   discover
   drag-stuff
   elisp-slime-nav
   emmet-mode
   engine-mode
   epl
   evil
   evil-god-state
   evil-leader
   evil-nerd-commenter
   evil-paredit
   evil-surround
   evil-visualstar
   expand-region
   f
   flycheck
   god-mode
   goto-chg
   haskell-mode
   helm
   helm-dash
   helm-itunes
   helm-projectile
   highlight-indentation
   htmlize
   idle-highlight-mode
   ido-ubiquitous
   impatient-mode
   inf-ruby
   iy-go-to-char
   js2-mode
   latex-preview-pane
   list-utils
   lorem-ipsum
   magit
   markdown-mode
   markdown-mode+
   multiple-cursors
   org
   org-bullets
   org-mac-iCal
   ox-gfm
   ox-reveal
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
   scala-mode
   simple-httpd
   skewer-mode
   slime
   smooth-scroll
   ssh
   string-utils
   tagedit
   tex-math-preview
   tree-mode
   twig-mode
   undo-tree
   visual-regexp
   web-mode
   windata
   writeroom-mode
   yaml-mode
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

(setq magit-last-seen-setup-instructions "1.4.0")

(mapc 'require
      my/packages)

(provide 'loads)
