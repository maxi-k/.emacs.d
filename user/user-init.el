(require 'ob-tangle)

;; Should be ~/.emacs.d/user
;; The base directory where all configuration takes place
(defconst emacs-config-dir (file-name-directory (or (buffer-file-name)
                                                    load-file-name)))
;; The directory where the actual elisp files are stored
(defconst emacs-config-elisp-dir (concat emacs-config-dir "elisp/"))
;; What org-mode property marks a file as emacs config
(defconst emacs-config-property '("my-file-type" . "emacs-config"))
;; A string representing the line that has to be inserted
;; into an org-file to be counted as emacs-config-file
(defconst emacs-config-marker
  (concat "#+PROPERTY: " (car emacs-config-property) " " (cdr emacs-config-property)))

(defun emacs-config-tangle-file (file &optional quiet-error-p quiet-success-p)
  "Tangles given org-file to an elisp file in `emacs-config-elisp-dir`
if it is marked as emacs-config-file (#+PROPERTIES my-file-type emacs-config)
Also byte-compiles it"
  (let ((visited-p (get-file-buffer (expand-file-name file)))
        to-be-removed)
    (prog1
        (save-window-excursion
          (find-file file)
          (setq to-be-removed (current-buffer))
          ;; When the file is visited, the org-file-properties
          ;; buffer-local-variable may not be up-to-date. Update it.
          (when visited-p (org-set-regexps-and-options))
          (let* ((config-file-p (equal (assoc (car emacs-config-property)
                                               org-file-properties)
                                        emacs-config-property)))
            (if config-file-p
                (let ((target-file (concat emacs-config-elisp-dir
                                           (file-name-base file) ".el")))
                  (org-babel-tangle nil target-file "emacs-lisp")
                  (byte-compile-file target-file 'load)
                  (unless quiet-success-p
                    (message (concat "Compiled and loaded " target-file))))
              (unless quiet-error-p
                (message (concat "File was not an emacs config file: " file))))))
      (unless visited-p
        (kill-buffer to-be-removed)))))

;; When in org mode, try to tangle the file
;; if it is an emacs-config-file - whether it is
;; is checked by `emacs-config-tangle-file`
(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook
                      (lambda ()
                        (emacs-config-tangle-file (buffer-file-name) t nil))
                      nil 'make-it-local)))

(defun emacs-config-compile ()
  "Tangle all org mode files in the `emacs-config-dir`
to compiled elisp-files in the `emacs-config-elisp-dir`
and load them."
  (interactive)
  (mapc #'emacs-config-tangle-file (directory-files emacs-config-dir t "\\.org$")))

;; Add all subdirectories of the config dir to the load-path
(setq default-directory emacs-config-dir)
(normal-top-level-add-subdirs-to-load-path)

;; Loading packages
(require 'loads)

;; Loading all the keybinds
(require 'keybinds)

;; Loading custom functions
(require 'functions)

;; Loading custom behaviors
(require 'behaviors)

;; Loading advices for built-in functions
(require 'advices)

;; Loading custom views
(require 'look)

;; Setting mode-specific stuff
(require 'mode-settings)

;; Clean up the exec-path created by PATH
;; throughout the initialization
(delete-dups (add-PATH-to-exec-path))

(provide 'user-init)
