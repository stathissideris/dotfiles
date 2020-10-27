;;mucha-specific options

(global-set-key [f8] `(lambda () (interactive) (find-file "s:/global/notes/notes.org")))

(setq default-directory "~")

;;for debugging:
(setq mucha-options-loaded t)

(setq magit-git-executable "c:\\Program Files\\Git\\bin\\git.exe")
(setq magit-process-quote-curly-braces nil)
(global-magit-file-mode -1)
(setq magit-auto-revert-mode nil)


(defun w32-dired-open-explorer ()
  "Open a file in dired mode by explorer.exe as you double click it."
  (interactive)
  (let ((file-name (dired-get-file-for-visit)))
    (if (file-exists-p file-name)
	(w32-shell-execute "open" file-name nil 1))))

;; Bind it to `o' in dired mode.
(define-key dired-mode-map "o" 'w32-dired-open-explorer)

(set-face-attribute 'default nil :family "Consolas")
(set-face-attribute 'default nil :height 105)
(set-face-attribute 'fixed-pitch nil :family "Consolas")

(provide 'mucha)
