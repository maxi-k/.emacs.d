;; Set the uniquify buffer name style to
;; filename|folder
(setq uniquify-buffer-name-style 'post-forward)

;; Set the smex save items file
(setq smex-save-file (concat user-emacs-save-directory ".smex-items"))

;; Initialize smex
(smex-initialize)

;; Initialize ido mode
(ido-mode t)
(ido-ubiquitous t)
(setq ido-save-directory-list-file (concat user-emacs-save-directory ".ido.last")
      ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

;; Set the markdown command to the homebrew install
(custom-set-variables
 '(markdown-command "/usr/local/bin/pandoc"))

;; Don't clutter the home dir
(setq remember-data-directory (concat user-emacs-save-directory "remember"))

;; Global yas mode
(yas-global-mode)

;; Initialize key-chord mode
(key-chord-mode 1)

;; Projectile is awesome, so it'll be everywhere
(projectile-global-mode)
(setq projectile-require-project-root nil)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(add-hook 'eshell-mode-hook
          (lambda () (setq eshell-path-env (concat "/usr/local/bin:" eshell-path-env))))

;; Remove trailing whitespace when saving
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

;; Autostart emmet mode on any markup modes
(add-hook 'sgml-mode-hook 'emmet-mode)
;; Enable emmet for css abbreviation
(add-hook 'css-mode-hook 'emmet-mode)

;; Use the right ispell version
(setq ispell-program-name "/usr/local/bin/ispell")
;; Set flyspell to german
(setq ispell-dictionary "german8")

;; Activate rainbow delimiters everywhere
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;; AC slows down writing too much
;; (add-hook 'prog-mode-hook 'auto-complete-mode)
;;;;;; LOADING MODE-SETTIGS ;;;;;;

(require 'evil-settings)

(require 'org-settings)

(require 'clojure-settings)

(require 'lisp-settings)

(require 'haskell-settings)


(provide 'mode-settings)
