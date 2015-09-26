;; Default: 800000 bytes
;; Now: 1MB
;; Emacs shouldn't garbage-collect so quickly
;; which would also slow down initialization
(setq gc-cons-threshold 100000000)

(require 'package)
;; Add the package archives
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Initialize the package-management
(package-initialize)

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

(defun do-require-package (pkg)
  (require-package pkg)
  (require pkg))

(defun require-all (files)
  "Requires all the files in the provided list"
  (mapc 'require files))

(defun require-packages (pkgs)
  "Loads all the packages (if they need to be loaded)
and then requires them."
  (mapc #'require-package pkgs)
  (require-all pkgs))

;; Refresh it
(when (not package-archive-contents)
  (package-refresh-contents))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
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
