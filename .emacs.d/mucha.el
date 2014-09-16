;;mucha-specific options

(global-set-key [f8] `(lambda () (interactive) (find-file "s:/global/notes/notes.org")))

;;for debugging:
(setq mucha-options-loaded t)

(setq magit-git-executable "c:\\Program Files (x86)\\Git\\bin\\git.exe")

(provide 'mucha)
