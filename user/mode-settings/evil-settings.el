(setq my/evil-states '(normal insert visual operator replace motion emacs god))

(defface powerline-evil-god-face
  '((t (:background "purple" :inherit powerline-evil-base-face)))
  "Powerline face for evil GOD state."
  :group 'powerline)

;; The evil powerline faces should be changing the foreground, not the background
(defun setup-powerline-evil-faces ()
  (set-face-foreground 'powerline-evil-base-face nil)
  (mapc (lambda (arg)
          (invert-face (intern (format "powerline-evil-%s-face" (symbol-name arg)))))
        my/evil-states))

(setup-powerline-evil-faces)

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
                    (cons (face-foreground (intern (format "powerline-evil-%s-face" (symbol-name arg)))) '(box))))
      my/evil-states)

;; BASIC BINDINGS ;;
(define-key evil-normal-state-map (kbd "n") #'newline)
(define-key evil-normal-state-map (kbd "<return>") #'newline)
(define-key evil-normal-state-map (kbd "\\") #'evil-emacs-state)

(define-key evil-insert-state-map (kbd "C-g") #'evil-normal-state)
(define-key evil-insert-state-map (kbd "M-n") #'evil-normal-state)


(define-key evil-emacs-state-map (kbd "C-,") #'evil-god-state)

;; ACE JUMP MODE ;;
(define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode)
(define-key evil-motion-state-map (kbd "C-SPC") #'evil-ace-jump-word-mode)

(define-key evil-operator-state-map (kbd "SPC") #'evil-ace-jump-char-mode)      ; similar to f
(define-key evil-operator-state-map (kbd "C-SPC") #'evil-ace-jump-char-to-mode) ; similar to t
(define-key evil-operator-state-map (kbd "M-SPC") #'evil-ace-jump-word-mode)

(define-key evil-normal-state-map (kbd "]f") #'projectile-find-file)

;; Different jumps for different visual modes
(defadvice evil-visual-line (before spc-for-line-jump activate)
  (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-line-mode))

(defadvice evil-visual-char (before spc-for-char-jump activate)
  (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode))

(defadvice evil-visual-block (before spc-for-char-jump activate)
  (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode))

(global-evil-surround-mode t)

;; EVIL GOD STATE ;;
(evil-define-key 'normal global-map (kbd "C-,") #'evil-god-state)
(evil-define-key 'god global-map [escape] #'evil-god-state-bail)
(evil-define-key 'god global-map (kbd "g") #'evil-god-state-bail)
(evil-define-key 'god global-map (kbd "C-,") #'evil-emacs-state)

(provide 'evil-settings)
