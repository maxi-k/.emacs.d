;; Default: 800000 bytes
;; Now: 1MB
;; Emacs shouldn't garbage-collect so quickly
;; which would also slow down initialization
(setq gc-cons-threshold 100000000)

(require 'package)
;; Add the package archives
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/") t)

;; Initialize the packages
(package-initialize)

(setq user-emacs-save-directory (concat user-emacs-directory "data/"))

;; Refresh it
(when (not package-archive-contents)
  (package-refresh-contents))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Uncomment this for debugging errors
;; in emacs configuration
;; (setq debug-on-error t)

(add-to-list 'load-path (concat user-emacs-directory "user/"))

;; Load the user init file
(require 'user-init)

;; Added by emacs
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
