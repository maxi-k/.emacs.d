;; Update the fringe color after a theme changes
(defadvice load-theme (after update-fringe-color-load activate)
  (update-fringe-background))
(defadvice disable-theme (after update-fringe-color-disable activate)
  (update-fringe-background))

(provide 'advices)
