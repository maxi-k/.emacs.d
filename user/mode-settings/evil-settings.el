(setq my/evil-states '(normal insert visual operator replace motion emacs god))

(defface powerline-evil-god-face
  '((t (:background "purple" :foreground "white" :inherit powerline-evil-base-face)))
  "Powerline face for evil GOD state."
  :group 'powerline)

;; Sets up my evil faces for powerline and cursor
;; faces :: evil-state -> (background, foreground)
(let ((faces '((normal   . ("#9BEA00" . "#006600"))
               (insert   . ("#0088B2" . "#70E1FF"))
               (visual   . ("#FF7C00" . "#920000"))
               (operator . ("#00FFFF" . "#000000"))
               (replace  . ("red"     . "black"))
               (motion   . ("#FF00FF" . "#000000"))
               (emacs    . ("violet"  . "purple" ))
               (god      . ("purple"  . "white")))))
  (mapc (lambda (arg)
          (let ((state (symbol-name (car arg)))
                (background (cadr arg))
                (foreground (cddr arg)))
            (message background)
            (custom-declare-face
             (intern (format "my/evil-%s-face" state))
             `((t (:background ,background :foreground ,foreground)))
             (format "Face for the evil %s state" state)
             :group 'my/evil)))
        faces))

(defun my/cur-evil-face ()
  "Returns the right my/evil-<state>-face for the current
  evil state"
  (let* ((face (intern (format "my/evil-%s-face" (symbol-name evil-state)))))
    (if (facep face) face nil)))

(defun reset-cursor-face ()
  (interactive)
  (unless evil-mode
    (my/set-face-from-attributes 'cursor my/emacs-cursor-face))
  (setq cursor-type 'box))

;; When exiting from evil mode when it's in
;; insert state, the cursor stays a line
;; This fixes that
(add-hook 'evil-mode-hook 'reset-cursor-face)

(mapc (lambda (arg) (set (intern (format "evil-%s-state-cursor" (symbol-name arg)))
                    (cons (face-background (intern (format "my/evil-%s-face" (symbol-name arg)))) '(box))))
      my/evil-states)

;; BASIC BINDINGS ;;
(define-key evil-normal-state-map (kbd "n") #'newline)
(define-key evil-normal-state-map (kbd "<return>") #'newline)
(define-key evil-normal-state-map (kbd "\\") #'evil-emacs-state)
(define-key evil-normal-state-map (kbd "C-,") #'evil-god-state)

(define-key evil-insert-state-map (kbd "M-n") #'evil-normal-state)
(define-key evil-insert-state-map (kbd "s-\\") #'evil-emacs-state)
(define-key evil-insert-state-map (kbd "C-,") #'evil-god-state)
(define-key evil-insert-state-map (kbd "C-;") #'evil-execute-in-normal-state)

(define-key evil-emacs-state-map (kbd "C-,") #'evil-god-state)
(define-key evil-emacs-state-map (kbd "M-n") #'evil-normal-state)
(define-key evil-emacs-state-map [escape] #'evil-normal-state)

(define-key evil-god-state-map [escape] #'evil-god-state-bail)
(define-key evil-god-state-map (kbd "g") #'evil-god-state-bail)
(define-key evil-god-state-map (kbd "M-n") #'evil-god-state-bail)
(define-key evil-god-state-map (kbd "C-,") #'evil-emacs-state)

;; ACE JUMP MODE ;;
(define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode)
(define-key evil-motion-state-map (kbd "C-SPC") #'evil-ace-jump-word-mode)

(define-key evil-operator-state-map (kbd "SPC") #'evil-ace-jump-char-mode)      ; similar to f
(define-key evil-operator-state-map (kbd "C-SPC") #'evil-ace-jump-char-to-mode) ; similar to t
(define-key evil-operator-state-map (kbd "M-SPC") #'evil-ace-jump-word-mode)

(define-key evil-normal-state-map (kbd "]f") #'projectile-find-file)

;; Different jumps for different visual modes
(lambda ()
  (defadvice evil-visual-line (before spc-for-line-jump activate)
    (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-line-mode))

  (defadvice evil-visual-char (before spc-for-char-jump activate)
    (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode))

  (defadvice evil-visual-block (before spc-for-char-jump activate)
    (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode)))

(global-evil-surround-mode t)

(progn
  (add-hook 'multiple-cursors-mode-enabled-hook 'evil-emacs-state)
  (add-hook 'multiple-cursors-mode-disabled-hook 'evil-normal-state))

(provide 'evil-settings)
