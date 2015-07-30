;; Set the uniquify buffer name style to
;; filename|folder
(setq uniquify-buffer-name-style 'post-forward)

;; Configure helm and enable everywhere
;; (setq helm-mode-fuzzy-match t)
(helm-mode 1)
(setq helm-buffers-fuzzy-matching t)
(setq helm-recentf-fuzzy-match t)
(setq helm-locate-command "mdfind -name %s %s")

;; Enable projectile caching
(setq projectile-completion-system 'helm)
(setq projectile-enable-caching t)
(helm-projectile-on)

;; Configure helm-dash
(setq helm-dash-browser-func 'eww)

;; Activate engine mode and define some engines for
;; searching from within emacs
(engine/set-keymap-prefix (kbd "s-/"))
(engine-mode t)
(defengine google "https://google.com/#q=google" :keybinding "g")
(defengine github "https://github.com/search?ref=simplesearch&q=%s")
(defengine wikipedia "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s" :keybinding "w")
(defengine wikipedia-deutsch "http://www.wikipedia.org/search-redirect.php?language=de&go=Go&search=%s" :keybinding"d")
(defengine stack-overflow "https://stackoverflow.com/search?q=%s" :keybinding "s")

;; Set the markdown command to the homebrew install
(custom-set-variables
 '(markdown-command "/usr/local/bin/pandoc"))

;; Don't clutter the home dir
(setq remember-data-directory (concat user-emacs-save-directory "remember"))

;; Global yas mode
(yas-global-mode)

;; Set the writeroom width to the fillcolumn width
(setq writeroom-width (+ fill-column 10))

;; Projectile is awesome, so it'll be everywhere
(projectile-global-mode)
(setq projectile-require-project-root nil)

;; Make text-mode nicer
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

;; Add /usr/local/bin to eshell
(add-hook 'eshell-mode-hook
          (lambda () (setq eshell-path-env (concat "/usr/local/bin:" eshell-path-env))))

;; Remove trailing whitespace when saving
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

;; Autostart emmet mode on any markup modes
(add-hook 'sgml-mode-hook 'emmet-mode)
;; Enable emmet for css abbreviation
(add-hook 'css-mode-hook 'emmet-mode)
;; Enable emmet for web mode
(add-hook 'web-mode-hook 'emmet-mode)

;; Use the right ispell version
(setq ispell-program-name "/usr/local/bin/ispell")
;; Set flyspell to german
(setq ispell-dictionary "english")

;; Activate rainbow delimiters everywhere
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;; AC slows down writing too much
;; (add-hook 'prog-mode-hook 'auto-complete-mode)

;; Web mode for html files (embedded js/css is common)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; js2-mode for Javascript files
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Activate skewer mode for html, css and js
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

;;;;;; LOADING MODE-SETTIGS ;;;;;;

(require 'evil-settings)

(require 'org-settings)

(require 'clojure-settings)

(require 'lisp-settings)

(require 'haskell-settings)


(provide 'mode-settings)
