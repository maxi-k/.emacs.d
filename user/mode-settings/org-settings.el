;; Set the org settings
(setq org-directory "~/Documents/Org/"
      org-mobile-directory "~/Dropbox/Apps/MobileOrg"
      org-mobile-inbox-for-pull (concat org-directory "mobile/pulled.org")
      org-mobile-files (list org-directory)
      org-export-default-language "de"
      ;; Remove the annoying link in org mode exported html
      ;; org 8.0 (setq org-html-validation-link nil)
      org-html-validation-link nil
      ;; Allow for local bindings in org documents
      ;; e.g #+BIND: org-html-validation-link t
      org-export-allow-bind-keywords t
      org-export-async-debug nil
      org-startup-indented t
      org-footnote-auto-adjust t
      ;; fontify code in code blocks
      org-src-fontify-natively t
      ;; Add a :CLOSED tag with timestamp
      ;; to todo items when they are done
      org-log-done 'time)

;; Change per-file with #+BIND: org-confluence-src-block-theme "Emacs"
(setq org-confluence-src-block-theme "Default")

;; Set up quick note taking with deft
(require 'deft)
(setq deft-extension "org"
      deft-directory (concat org-directory "deft/")
      deft-text-mode 'org-mode)
(global-set-key (kbd "<f9>") 'deft)

;; Add the MacTeX programs to the PATH
(add-to-PATH "/Library/TeX/Distributions/Programs/texbin")

;; Use the MathToWeb jar for converting LaTeX to MathML/odf
(setq org-latex-to-mathml-convert-command
      "java -jar %j -unicode -force -df %o %I"
      org-latex-to-mathml-jar-file
      "~/Applications/MathToWeb/mathtoweb.jar")

(require 'ox-latex)
(setenv "PDFLATEX" "pdflatex -shell-escape")
(setq org-latex-pdf-process '("texi2dvi --pdf %f"))
;; Enable LaTeX (pdf) syntax highlighting
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

;; Used for extracting org-source-blocks into their own files
(require 'ob-tangle)

;; Load more languages for org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh         . t)
   (js         . t)
   (emacs-lisp . t)
   (perl       . t)
   (scala      . t)
   (clojure    . t)
   (python     . t)
   (ruby       . t)
   (dot        . t)
   (css        . t)
   (plantuml   . t)))

;; Don't ask before evaluating code snippets
(setq org-confirm-babel-evaluate nil)
;; Make tab in source blocks behave like it would in the language
(setq org-src-tab-acts-natively t)

;; Set the org-reveal settings (ox-reveal)
(setq org-reveal-root "file:///Users/Maxi/Applications/reveal-js")
;; Make a function to load the ox-reveal library
(defun activate-org-reveal ()
  (interactive)
  (load-library "ox-reveal"))

(define-minor-mode org-reveal-auto-export-mode
  "A minor mode for automatically exporting the org file
  you are working on to html whenever you save the file.
  This is only for using the org-reveal presentation tool."
  :lighter "org-reveal-auto-export"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "s-s") (lambda () (interactive)
                                          (save-buffer)
                                          (org-reveal-export-to-html)))
            map))

(define-minor-mode org-html-auto-export-mode
  "A minor mode for automatically exporting the org file
  you are working on to html whenever you save the file."
  :lighter "org-html-auto-export"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "s-s") (lambda () (interactive)
                                          (save-buffer)
                                          (org-html-export-to-html t)))
            map))

;; Define some local keybindings
(let ((bindings `((,(kbd "M-h") . kill-region-or-backward-word)
                  (,(kbd "M-n") . outline-next-visible-heading)
                  (,(kbd "M-p") . outline-previous-visible-heading)
                  (,(kbd "H-i") . org-table-insert-row)
                  (,(kbd "H-k") . org-table-kill-row)
                  (,(kbd "C-M-y") . org-table-paste-rectangle)
                  (,(kbd "C-M-l") . org-table-sort-lines)
                  (,(kbd "M-I") . org-toggle-iimage-in-org)
                  (,(kbd "C-y") . yank)
                  (,(kbd "s-t") . org-todo))))
  (mapc (lambda (arg)
          (define-key org-mode-map (car arg) (cdr arg)))
        bindings))

;; Make the bullets look nicer
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq org-agenda-include-diary t)
(setq diary-file (concat org-directory "diary/diary"))
(setq org-default-notes-file (concat org-directory "capture/notes.org"))

(defun markdown-file-to-org ()
  (interactive)
  (let* ((md-file-name (file-truename buffer-file-name))
         (file-base-name (file-name-sans-extension (file-truename buffer-file-name)))
         (command (concat "pandoc -f markdown -t org -o "
                          file-base-name ".org "
                          md-file-name)))
    (shell-command command)))

;; Beautify org mode
(ignore-errors(require 'org-beautify-theme)
              (enable-theme 'org-beautify-theme))

(provide 'org-settings)
