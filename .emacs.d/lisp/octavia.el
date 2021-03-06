;;mac-specific options

(require 's)

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
(global-set-key [f8] `(lambda () (interactive) (find-file "/Users/sideris/notes/personal.org")))

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
  (forward-line 2)
  (insert "* ")
  (org-time-stamp '(16) t)
  (insert "\n")
  (insert
   (shell-command-to-string "osascript -l JavaScript ~/.emacs.d/js/onetab.js"))
  (beginning-of-buffer))

(defun ss/current-tab ()
  "Inserts an org link to the current active tab in Chrome"
  (interactive)
  (insert
   (s-trim
    (shell-command-to-string "osascript -l JavaScript ~/.emacs.d/js/current-tab.js"))))

(defun ss/extract-org-url (s)
  (when (string-match "\\[\\[\\(.+\\)\\]\\[.+\\]\\]" s)
    (match-string 1 s)))

(defun ss/split-lines (s)
  (split-string s "[\n\r]+"))

(defun ss/region-lines ()
  (ss/split-lines
   (buffer-substring-no-properties (region-beginning) (region-end))))

(defun ss/strip-properties (s)
  (set-text-properties 0 (length s) nil s)
  s)

(defun ss/browse-region-org-urls ()
  (interactive)
  (dolist (line (ss/region-lines))
    (let ((url (ss/extract-org-url line)))
      (when url
        (browse-url (ss/extract-org-url url))))))

(defun ss/open-context ()
  (interactive)
  (org-copy-subtree)
  (dolist (line (ss/split-lines (ss/strip-properties (current-kill 0))))
    (let ((url (ss/extract-org-url line)))
      (when url
        (browse-url url)))))

(provide 'octavia)
