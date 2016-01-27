(require 'org)
(require 'ob-tangle)

;; Should be ~/.emacs.d/user/
;; The base directory where all configuration takes place
(defconst emacs-config-dir (file-name-directory (or (buffer-file-name)
                                                    load-file-name)))
;; A non-version-controlled file in a subdirectory of the emacs config dir
;; For local setup. Gets loaded after the 'general' setup files in
;; emacs-config-dir
(defconst emacs-config-local-dir (concat emacs-config-dir "local/"))
(defconst emacs-config-local-file
  (concat emacs-config-local-dir "setup-local.org"))
;; The directory where the actual elisp files are stored
(defconst emacs-config-elisp-dir (concat emacs-config-dir "elisp/"))
;; What org-mode property marks a file as emacs config
(defconst emacs-config-property '("my-file-type" . "emacs-config"))
;; A string representing the line that has to be inserted
;; into an org-file to be counted as emacs-config-file
(defconst emacs-config-marker
  (concat "#+PROPERTY: " (car emacs-config-property) " " (cdr emacs-config-property)))

(defun has-local-config ()
  "Returns non-nil if the file
`emacs-config-dir'/local/setup-local.org exists."
  (file-exists-p emacs-config-local-file))

;; Add the folder with the tangled&compiled org-files
;; to the load-path
(add-to-list 'load-path emacs-config-elisp-dir)

(defun emacs-config-tangle-file (file &optional quiet-error-p quiet-success-p keep-el-p)
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
                  (org-babel-tangle nil target-file)
                  (byte-compile-file target-file nil)
                  ;; Delete the .el file, only keep the .elc file
                  (unless keep-el-p
                    (delete-file target-file))
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
                        (emacs-config-tangle-file (buffer-file-name) t))
                      nil 'make-it-local)))

(defun emacs-config-compile (&optional delete-old-p)
  "Tangle all org mode files in the `emacs-config-dir`
to compiled elisp-files in the `emacs-config-elisp-dir`
and load them.
If called with a prefix argument, deletes the content of
the `emacs-config-elisp-dir` first."
  (interactive "P")
  ;; If called with prefix argument, the old elisp
  ;; directory is deleted first
  (when delete-old-p
    (delete-directory emacs-config-elisp-dir t nil))
  ;; If the elisp-directory does not exist, create
  ;; it and any missing parent directories
  (unless (file-exists-p emacs-config-elisp-dir)
    (make-directory emacs-config-elisp-dir t))
  ;; Tangle all the files in the emacs-config dir
  (let* ((general-config (directory-files emacs-config-dir t "\\.org$"))
         (local-config (if (has-local-config)
                           (directory-files emacs-config-local-dir t "\\.org$")
                         '())))
    (mapc #'emacs-config-tangle-file (append general-config local-config))))

;; If the elsip directory does not exist
;; (e.g if it was just pulled from git)
;; create it and compile the whole config
(unless (file-exists-p emacs-config-elisp-dir)
  (emacs-config-compile))

;; Add all subdirectories of the config dir to the load-path
(setq default-directory emacs-config-dir)
(normal-top-level-add-subdirs-to-load-path)

;; Very General Setup &
;; Bootstrap some stuff that every config file can rely on
(require-all '(bootstrap
               general-functions
               general-settings))

;; Load packages that don't require any setup
(require-all '(setup-simple-packages))

;; Specific Setups
(require-all '(setup-PATH
               setup-system
               setup-keys
               setup-buffers-windows
               setup-movement
               setup-editing
               setup-misc
               ))

;; Mode-specific setups
(require-all '(setup-evil-mode
               setup-git
               setup-helm
               setup-projectile
               setup-text-mode
               setup-programming
               setup-org
               setup-deft
               setup-which-key
               setup-engine-mode
               setup-look
               ))

;; Load the machine-local setup if present
(when (has-local-config)
  (require 'setup-local))

(provide 'user-init)
