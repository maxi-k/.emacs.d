(deftheme my-light
  "A custom light theme loosely based on the macvim theme.
  Unfinished.")

;; #f0f0f0
(let ((bg "#ffffff")
      (fg "#000000")
      (hl "#D4EDFF")
      (cursor "#000")
      (string "#036DD0")
      (light-gray "#E6E6E6")
      (medium-gray "#7F7F7F")
      (dark-gray "#303030")
      (blue "#3D2EF7")
      (medium-green "#00954F")
      (light-green "#239075")
      (medium-orange "#FF8200")
      (reddish "#BE2E76")
      (purple "#9D00E4"))
  (custom-theme-set-faces
   'my-light
   `(default ((t (:foreground ,fg :background ,bg))))
   `(cursor  ((t (:foreground ,bg :background ,cursor))))
   `(fringe ((t (:background ,bg))))
   `(region ((t (:background ,hl))))

   ;; fontlock
   `(font-lock-builtin-face ((t (:foreground ,fg))))
   `(font-lock-comment-face ((t (:foreground ,medium-gray))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,medium-gray))))
   `(font-lock-function-name-face ((t (:foreground ,fg))))
   `(font-lock-keyword-face ((t (:foreground ,reddish))))
   `(font-lock-string-face ((t (:foreground ,string))))
   `(font-lock-preprocessor-face ((t (:foreground ,medium-gray))))
   `(font-lock-type-face ((t (:foreground ,light-green))))
   `(font-lock-constant-face ((t (:foreground ,purple))))
   `(font-lock-warning-face ((t (:foreground "red"))))
   `(font-lock-variable-name-face ((t (:foreground ,medium-green))))
   `(font-lock-doc-face ((t (:foreground ,blue))))

   ;; line-numbers
   `(linum ((t (:background ,light-gray :foreground ,medium-gray))))

   ;; powerline
   `(powerline-active1 ((t (:background ,dark-gray))))
   `(powerline-active2 ((t (:background ,medium-gray))))
   `(mode-line-inactive ((t (:background ,medium-gray :foreground ,light-gray))))
   `(mode-line ((t (:background ,dark-gray :foreground ,light-gray))))))

(provide-theme 'my-light)
