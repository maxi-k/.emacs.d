(mapc 'require
      '(ace-jump-mode
        buffer-move
        clj-refactor
        dirtree
        doremi
        evil
        evil-god-state
        evil-org
        evil-surround
        evil-visualstar
        expand-region
        fill-column-indicator
        god-mode
        idle-highlight-mode
        iy-go-to-char
        key-chord
        lorem-ipsum
        magit
        multiple-cursors
        ox-odt
        ox-reveal
        pabbrev
        paredit
        powerline
        powerline-evil
        rainbow-delimiters
        uniquify))

(when (require 'yasnippet nil 'noerror)
  (progn
    (yas/load-directory "~/.emacs.d/data/snippets")))

(provide 'loads)
