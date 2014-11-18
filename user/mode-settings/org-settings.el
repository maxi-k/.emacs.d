;; Set the org settings
(setq org-directory "~/Documents/Org/local/"
      org-mobile-directory "~/Dropbox/Apps/MobileOrg"
      org-mobile-inbox-for-pull "~/Documents/Org/mobile/pulled.org"
      org-mobile-files (list org-directory)
      org-export-default-language "de"
      ;; Remove the annoying link in org mode exported html
      ;; org 8.0 (setq org-html-validation-link nil)
      org-export-html-validation-link nil
      org-startup-indented t
      org-footnote-auto-adjust t)

;; Add the MacTeX programs to the exec-path & PATH
(add-to-PATH "/Library/TeX/Distributions/Programs/texbin")
(set-exec-path-to-PATH)

;; Use the MathToWeb jar for converting LaTeX to MathML/odf
(setq org-latex-to-mathml-convert-command
                "java -jar %j -unicode -force -df %o %I"
                org-latex-to-mathml-jar-file
                "~/Applications/MathToWeb/mathtoweb.jar")

;; Set the org-reveal settings (ox-reveal)
(setq org-reveal-root "file:///Users/Maxi/Applications/reveal-js")

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

(custom-set-variables
 '(org-agenda-files (quote ("~/Documents/Org/scratch.org"))))

(provide 'org-settings)
