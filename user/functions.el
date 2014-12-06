(defun add-to-PATH (s)
  "Adds given String (without :) to the PATH"
  (setenv "PATH" (concat (getenv "PATH") (concat ":" s))))

(defun set-exec-path-to-PATH ()
  "Sets the exec-path to the same value as PATH"
  (setq exec-path (split-string (getenv "PATH") path-separator)))

(defun set-exec-path-from-shell-PATH ()
  "Sets the exec-path to the same value used by the user shell"
  (let ((path-from-shell
         (replace-regexp-in-string
          "[[:space:]\n]*$" ""
          (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(defun add-subfolders-to-theme-load-path (parent-dir)
  "Add subfolders to theme load path"
  (dolist (f (directory-files parent-dir))
    (let ((name (concat parent-dir f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'custom-theme-load-path name)))))

(defun update-fringe-background ()
  (set-face-background 'fringe
                       (face-attribute 'default :background)))

(defun disable-all-themes ()
  "Disables all active themes."
  (interactive)
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defun load-theme-exclusively (theme)
  "Disables all active themes, then loads
  given theme."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name
                                     (custom-available-themes))))))
  (disable-all-themes)
  (load-theme theme))

(defun kill-other-buffers ()
  "Kill all buffers except the active one."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))

(defun org-scratch-buffer ()
  "Create a second scratch buffer in org mode."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch-org*"))
  (org-mode))

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defun kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun save-region-or-current-line (arg)
  (interactive "P")
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (copy-line arg)))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun eval-and-insert ()
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (insert (format "%S" value))))

(defun eval-and-insert-as-comment ()
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (insert (format " ;; %S" value))))

(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Google: "))))))

(defun open-in-browser ()
  "Open the buffer file in the standard browser."
  (interactive)
  (browse-url
   (concat "file://" (buffer-file-name))))

(defun org-create-link-in-org-dir ()
  "Creates a symbolic link of the currently
  visited file in the org-directory "
  (interactive)
  (shell-command (concat "ln -s " buffer-file-name " "
                         org-directory "/"
                         (file-name-nondirectory buffer-file-name)
                         " & disown")))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun time ()
  "Messages the current time in the format hh:mm."
  (interactive)
  (message (substring (current-time-string) 11 16)))

(defun my/set-face-from-attributes (face attrs)
  "Sets all the face attributes of given
  face from given attribute-value list"
  (mapc
   (lambda (attr) (set-face-attribute face nil (car attr) (cdr attr)))
   attrs))

(defun my/call-times (fn times arg)
  (let ((result arg))
    (dotimes (n times result)
      (setq result (funcall fn result)))))

(defun my/insert-alphabet (separator)
  (interactive "sSeparator: ")
  (message separator)
  (let ((alphabet "abcdefghijklmnopqrstuvwxyz"))
    (mapc (lambda (c) (insert c) (insert separator))
          (mapcar 'char-to-string (append alphabet nil)))))
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(provide 'functions)
