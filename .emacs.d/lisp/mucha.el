;;mucha-specific options

(global-set-key [f8] `(lambda () (interactive) (find-file "s:/global/notes/notes.org")))

;;for debugging:
(setq mucha-options-loaded t)

(setq magit-git-executable "c:\\Program Files (x86)\\Git\\bin\\git.exe")
(setq magit-process-quote-curly-braces nil)

(defun w32-dired-open-explorer ()
  "Open a file in dired mode by explorer.exe as you double click it."
  (interactive)
  (let ((file-name (dired-get-file-for-visit)))
    (if (file-exists-p file-name)
	(w32-shell-execute "open" file-name nil 1))))

;; Bind it to `o' in dired mode.
(define-key dired-mode-map "o" 'w32-dired-open-explorer)

(provide 'mucha)
