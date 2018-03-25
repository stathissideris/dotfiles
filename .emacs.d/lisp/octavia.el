;;mac-specific options

(defun map-mac-keys ()
  (interactive)
  (setq mac-option-key-is-meta nil)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (set-keyboard-coding-system nil))

(if window-system
    (map-mac-keys))

(defun mac-keys-for-pc-keyboard ()
  (interactive)
  (setq mac-option-key-is-meta t)
  (setq mac-command-key-is-meta nil)
  (setq mac-option-modifier 'meta))

;;(setq inferior-lisp-program "/Users/sideris/devel-tools/ccl/dx86cl64")
(setq inferior-lisp-program "sbcl")
(global-set-key [f8] `(lambda () (interactive) (find-file "/Users/sideris/notes/notes.org")))

;;for debugging:
(setq mac-options-loaded t)

(set-face-attribute 'default nil :family "Monaco")
(set-face-attribute 'default nil :height 120)
(set-face-attribute 'fixed-pitch nil :family "Monaco")
(set-face-attribute 'font-lock-builtin-face nil :weight 'semi-bold)
(set-face-attribute 'font-lock-constant-face nil :weight 'semi-bold)
(set-face-attribute 'font-lock-keyword-face nil :weight 'semi-bold)

;;typeset 'Greek and Coptic' unicode block using Menlo font
(set-fontset-font
 "fontset-default"
 (cons (decode-char 'ucs #x0370)
       (decode-char 'ucs #x03FF))
 "Menlo")

(global-set-key [kp-delete] 'paredit-forward-delete)
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)

(defun finder ()
  "Opens file directory in Finder."
  (interactive)
  (let ((file (or (buffer-file-name)
                  (expand-file-name dired-directory))))
    (if file
        (shell-command
         (format "%s %s" (executable-find "open") (file-name-directory file)))
      (error "Buffer is not attached to any file"))))

(setq exec-path (append exec-path '("/usr/local/bin")))

(defun clean-AS-result (str)
  (substring str 1 -1))

(defun onetab ()
  "Inserts a list of Chrome tabs in buffer as an org bullet-point list"
  (interactive)
  (find-file "~/notes/tabs.org")
  (beginning-of-buffer)
  (insert "* Chrome tabs ")
  (org-time-stamp '(16) t)
  (insert "\n")
  (insert
   (clean-AS-result
    (do-applescript
     (get-string-from-file "~/.emacs.d/applescript/onetab-chrome.applescript"))))
  (beginning-of-buffer))

(provide 'octavia)
