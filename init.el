;; Default: 800000 bytes
;; Now: 10MB
;; Emacs shouldn't garbage-collect so quickly
;; which would also slow down initialization
(setq my/gc-cons-threshold 10000000)
(setq gc-cons-threshold my/gc-cons-threshold)
;; (setq garbage-collection-messages t)

(require 'cl)
(require 'package)
;; ;; Add the package archives
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defun list-network-connections ()
  "List the working network connections.
Returns nil if there is none, i.e no internet."
  (remove-if (lambda (el)
               (string-match-p "lo.*" (car el)))
             (network-interface-list)))

(defun has-network ()
  "Returns t if there is a network connection, nil otherwhise."
  (not (eq nil (list-network-connections))))

;; Initialize the package-management
(package-initialize)
(when (and (has-network) (not package-archive-contents))
  (package-refresh-contents))

;; Bootstrap `use-package' for loading packages easily
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
;; Always try to install the package from one of the repos
(setq use-package-always-ensure t)

;; Benchmark the init giving information on what takes how long
;; (when (fboundp 'benchmark-init/activate)
;;   (benchmark-init/activate))

(defun require-all (files)
  "Requires all the files in the provided list"
  (mapc 'require files))

(defmacro my|require-package-list (pkgs)
  `(mapc (lambda (pkg) `(use-package ,pkg :defer t)) ,pkgs))

(defmacro require-packages (packages &optional do-require)
  "Loads all the packages (if they need to be loaded)
and then requires them."
  (let ((pkgs (eval (quote packages))))
    (if (has-network)
        `(my|require-package-list ,pkgs)
      (if do-require
          (require-all pkgs)
        `(my|require-package-list ,pkgs)))))

(let ((cfile (expand-file-name "custom.el" user-emacs-directory)))
  ;; Create the custom file if it does not exist yet
  (unless (file-exists-p cfile)
    (write-region "" nil cfile))
  ;; Keep emacs Custom-settings in separate file
  (setq custom-file cfile))

(load custom-file)
(setq user-emacs-save-directory (concat user-emacs-directory "data/"))

;; Uncomment this for debugging errors
;; in emacs configuration
(setq debug-on-error t)

(add-to-list 'load-path (concat user-emacs-directory "user/"))

;; Load the user init file
(require 'user-init)

;; Added by emacs
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
