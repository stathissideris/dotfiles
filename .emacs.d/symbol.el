(setq global-mode-string
      (append global-mode-string
              '((:eval (concat "[☁ " (getenv "AM_PROFILE") "]")))))
