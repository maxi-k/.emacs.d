;; Add the themes folder and its subfolders to the load path
;; loading a theme is handled by the package 'remember-theme',
;; which loads the last theme used before quitting emacs
(let ((theme-path "~/.emacs.d/themes/"))
  (add-to-list 'custom-theme-load-path theme-path)
  (add-subfolders-to-theme-load-path theme-path))

;; Initialize the window
(if window-system
    (progn
      (setq frame-title-format '(buffer-file-name "%f" ("%b")))
      (tooltip-mode -1)
      (mouse-wheel-mode t)
      (blink-cursor-mode -1)
      (scroll-bar-mode 0)
      (setq-default fill-column 90)
      (setq-default indicate-empty-lines t)
      (add-hook 'after-init-hook (lambda () (load-theme-exclusively 'my-light))))
  (progn
    (menu-bar-mode -1)))

;; Increase the font size
(set-frame-font "Source Code Pro-16" nil t)

(custom-set-variables
 '(linum-format " %2d ")
 '(fringe-mode 4 nil (fringe)))

(unless (facep 'paren-face)
  (defface paren-face '()
    "Stop that annoying paren-face error"
    :group 'basic-faces))

;; Keep track of the default cursor color
(defun my/set-cursor-variable ()
  (setq my/emacs-cursor-face (face-all-attributes 'cursor (car (frame-list)))))
(defadvice load-theme (after set-cursor-variable activate) (my/set-cursor-variable))

;; Turn on fill column indicator mode
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

;; Make the fringe the same color as the background
(update-fringe-background)

;; Turn of the toolbar
(tool-bar-mode -1)

;; You can also set the initial frame parameters
(setq initial-frame-alist '((tool-bar-lines . 0)))

;; Set the initial window size
(setq initial-frame-alist
      '((top . 0) (left . 0) (width . 92) (height . 40)))

;; My ears!
(setq visible-bell t)

(defpowerline evil-mode-bar
  (if (and (boundp 'evil-mode) evil-mode)
      ;; evil-mode is active
      (format " %s " (upcase (symbol-name evil-state)))
    ;; evil-mode is not active
    " "))

(defpowerline god-mode-bar
  (if (and (boundp 'god-local-mode)
           (not evil-mode)
           god-local-mode)
      (format " GOD ")
    " "))

(defun setup-powerline-theme ()
  "Setup the default mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (face-evil (my/cur-evil-face))
                          (face-nil nil)
                          (separator-left (intern (format "powerline-%s-%s"
                                                          powerline-default-separator
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           powerline-default-separator
                                                           (cdr powerline-default-separator-dir))))

                          (lhs (list (powerline-raw "%*" face-nil 'l)
                                     (powerline-raw mode-line-mule-info face-nil 'l)
                                     (powerline-buffer-id face-nil 'l)
                                     (powerline-raw " " face-nil 'l)
                                     (funcall separator-right face-nil face-evil)
                                     (evil-mode-bar face-evil nil)
                                     (funcall separator-left face-evil face-nil)
                                     (god-mode-bar face-nil nil)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format face-nil 'l))
                                     (powerline-raw " ")
                                     ;; (funcall separator-left mode-line face1)
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode face-nil 'l)
                                     (powerline-raw " " face-nil)
                                     ;; (funcall separator-left face1 face2)
                                     ;; (powerline-minor-modes face-nil 'l)
                                     (powerline-process face-nil)
                                     (powerline-narrow face-nil 'l)
                                     (powerline-raw " " face-nil)
                                     ;; (funcall separator-left face1 face2)
                                     (powerline-vc face-nil 'r)
                                     ))
                          (rhs (list ;;(funcall separator-right face2 face1)
                                (powerline-raw "%3l:%2c " face-nil 'l)
                                ;; (funcall separator-right face1 mode-line)
                                (powerline-raw " ")
                                (powerline-raw "%7p" face-nil 'r)
                                (powerline-hud face2 face1)
                                ))
                          (center '()))
                     (concat (powerline-render lhs)
                             (powerline-fill mode-line (powerline-width rhs))
                             (powerline-render rhs)))))))

(setup-powerline-theme)

(provide 'look)
