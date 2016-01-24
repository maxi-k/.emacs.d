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

;; Benchmark the init giving information on what takes how long
;; (benchmark-init/activate)

;; On-demand installation of packages
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install package `%s': %S" package err)
     nil)))

(defun autoload-package (p)
  (autoload p (concat (symbol-name p) ".elc") (symbol-name p) t))

(defun do-require-package (pkg &optional do-require assume-network-p )
  "Installes the package `pkg' if it is not already installed and
there is a network connection, then requires it.
If the second argument is nil, don't check for a network
"
  ;; "If I don't have to check the network OR there is one => download"
  (when (or assume-network-p (has-network))
    (require-package pkg))
  (if do-require
      (require pkg)
    (autoload-package pkg)))

(defun require-all (files)
  "Requires all the files in the provided list"
  (mapc 'require files))

(defun autoload-all (packages)
  (mapc #'autoload-package packages))

(defun require-packages (pkgs &optional do-require)
  "Loads all the packages (if they need to be loaded)
and then requires them."
  (if (has-network)
      (mapc (lambda (pkg) (do-require-package pkg do-require nil)) pkgs)
    (if do-require
        (require-all pkgs)
      (autoload-all pkgs))))

(let ((cfile (expand-file-name "custom.el" user-emacs-directory)))
  ;; Create the custom file if it does not exist yet
  (unless (file-exists-p cfile)
    (shell-command (concat "touch " custom-file)))
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
