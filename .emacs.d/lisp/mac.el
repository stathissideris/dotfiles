;;mac-specific options

(defun map-mac-keys ()
  (interactive)
  (setq mac-option-key-is-meta nil)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (setq ns-function-modifier 'hyper)
  (set-keyboard-coding-system nil))

(if window-system
    (map-mac-keys))

(defun mac-keys-for-pc-keyboard ()
  (interactive)
  (setq mac-option-key-is-meta t)
  (setq mac-command-key-is-meta nil)
  (setq mac-option-modifier 'meta))

(setq inferior-lisp-program "/Users/sideris/devel-tools/ccl/dx86cl64")
(global-set-key [f8] `(lambda () (interactive) (find-file "/Users/sideris/notes/notes.org")))

;;for debugging:
(setq mac-options-loaded t)

(set-face-attribute 'default nil :family "Monaco")
(set-face-attribute 'default nil :height 120)
(set-face-attribute 'fixed-pitch nil :family "Monaco")

(global-set-key [kp-delete] 'paredit-forward-delete)
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)

(defun finder ()
  "Opens file directory in Finder."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (shell-command
         (format "%s %s" (executable-find "open") (file-name-directory file)))
      (error "Buffer is not attached to any file"))))

(setq exec-path (append exec-path '("/usr/local/bin")))

(provide 'mac)
