;;packages to attempt to install at startup, comment this out if offline
(setq packages-to-bootstrap
      '(highlight-symbol
        clojure-mode
        clj-refactor
        align-cljlet
        cider
        ;;ac-nrepl
        ;;tuareg
        ;;merlin
        slime

        ;;scala:
        scala-mode2
        sbt-mode

        yasnippet
        magit
        powerline
        git-gutter
        markdown-mode
        puppet-mode
        solarized-theme
        zenburn-theme
        undo-tree
        diminish
        company
        emmet-mode
        ido-ubiquitous
        expand-region))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defun maybe-install-and-require (p)
  (when (not (package-installed-p p))
    (package-install p))
  (require p))

;;attempt to install packages at startup
(unless package-archive-contents
  (package-refresh-contents))
(dolist (p packages-to-bootstrap)
  (when (not (package-installed-p p))
    (package-install p))
  (require p))

(if (not (string-equal system-name "MUCHA"))
    (when (not (package-installed-p 'ag))
      (package-install 'ag)))

(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

(add-to-list 'load-path site-lisp-dir)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'bm)
(require 'setnu)
(require 'xmlgen)
(require 'tooltip-help)
(require 'framemove)
(require 'popup2)
(require 'imenu)
(require 'paredit)
(require 'pretty-mode)
(require 'uniquify)
(require 'highlight-symbol)
(require 'org)
(require 'cider)
(require 'tuareg)

(require 'powerline)
(powerline-default-theme)

(require 'diminish)
(diminish 'undo-tree-mode)
(diminish 'highlight-symbol-mode "hi")
(diminish 'paredit-mode "prdt")
;;(diminish 'auto-complete-mode "ac")
;;(diminish 'magit-auto-revert-mode)
(diminish 'git-gutter-mode)
;;(diminish 'clj-refactor-mode)

;(add-hook 'org-mode-hook 'turn-on-pretty-mode)

(global-set-key (kbd "C-z") nil)

(global-undo-tree-mode)

;;;;;;;;;;;; options ;;;;;;;;;;;;;;;;

(global-subword-mode 1)
(setq inhibit-startup-screen t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;mustache files are html-like
(setq auto-mode-alist
   (cons '("\\.mustache" . html-mode) auto-mode-alist))

(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)

(require 'paredit-steroids)
(global-set-key (kbd "s-/") 'cycle-symbol-at-point)
(global-set-key (kbd "s-\\") 'cycle-symbol-at-point)

(global-unset-key (kbd "C-d"))
(global-set-key (kbd "C-d") 'duplicate-sexp)
(define-key paredit-mode-map (kbd "C-d") 'duplicate-sexp)

(global-set-key [C-M-down] 'transpose-sexp-forward)
(global-set-key [C-M-up] 'transpose-sexp-backward)
(global-set-key (kbd "M-{") 'paredit-wrap-curly)
;;(global-set-key (kbd "M-[") 'paredit-wrap-square)
(global-set-key [M-f2] 'paredit-mode)
(global-set-key (kbd "<scroll>") 'paredit-mode)

(add-hook
  'emacs-lisp-mode-hook
  (function (lambda () (paredit-mode 1))))

(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

;;spaces instead of tabs
(setq clojure-mode-hook
	  (function (lambda ()
				  (setq indent-tabs-mode nil)
				  (setq c-indent-level 2))))
(setq lisp-mode-hook
	  (function (lambda ()
				  (setq indent-tabs-mode nil)
				  (setq c-indent-level 2))))

(setq-default cursor-type 'box)
(require 'stathis-blog)

;; (require 'color-theme)
;; (color-theme-initialize)
;; (setq color-theme-is-global t)

;(cua-mode t)
(setq ido-everywhere t)
(ido-mode t)

;;;; looks
(if window-system
    (progn
      (load-theme 'solarized-dark t)
      (scroll-bar-mode -1)
      (tool-bar-mode -1))
  (load-theme 'zenburn t)
  ;; (progn
  ;;   (setq scroll-bar-mode-explicit t)
  ;;   (set-scroll-bar-mode `right))
  )
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ;; one two lines at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)

;;cursor
(set-default 'cursor-type 'bar)
(set-cursor-color "yellow")

(column-number-mode t)
(setq inhibit-splash-screen t)

;; (setq CUA-mode-normal-cursor-color "red")
;; (setq CUA-mode-overwrite-cursor-color "blue")
;; (setq CUA-mode-read-only-cursor-color "green")


;;line numbers
(global-set-key [f11] `linum-mode)
(require 'linum)
(defun linum-update-window-scale-fix (win)
  "fix linum for scaled text"
  (set-window-margins win
		      (ceiling (* (if (boundp 'text-scale-mode-step)
				      (expt text-scale-mode-step
					    text-scale-mode-amount) 1)
				  (if (car (window-margins))
				      (car (window-margins)) 1)))))
(advice-add #'linum-update-window :after #'linum-update-window-scale-fix)


(defun new-scratch ()
  "open up a guaranteed new scratch buffer"
  (interactive)
  (switch-to-buffer (loop for num from 0
                          for name = (format "new-%03i" num)
                          while (get-buffer name)
                          finally return name)))

;;;; keys ;;;;

;; setting the PC keyboard's various keys to
;; Super or Hyper, for emacs running on Windows.

(setq w32-pass-lwindow-to-system nil 
      w32-pass-rwindow-to-system nil 
      w32-pass-apps-to-system nil 
      w32-lwindow-modifier 'super ; Left Windows key 
      w32-rwindow-modifier 'super ; Right Windows key 
      w32-apps-modifier 'hyper) ; Menu key

;; navigation
(global-set-key [C-prior] `previous-buffer)
(global-set-key [C-next] `next-buffer)
;;(global-set-key "\C-l" `goto-line)
(global-set-key [C-tab] `other-window)
(global-set-key [backtab] `switch-to-buffer)
(global-set-key (kbd "<M-f4>") `save-buffers-kill-emacs)

;;(require 'popup-buffer-switch)
;;(global-set-key (kbd "<f9>") 'popup-buffer-switch)

;;;; remember positions in files
;;(require 'saveplace)
;;(setq-default save-place t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; navigate imenu using popup2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun imenu-popup-jump ()
  (interactive)
  (let ((selection (popup-menu* (cdr (mapcar 'car (imenu--make-index-alist)))
                                :scroll-bar t
                                :isearch t
                                :margin-left 1)))
    (let ((index-item (assoc selection (imenu--make-index-alist))))
      (imenu-default-goto-function (car index-item) (cdr index-item) nil))))
(global-set-key (kbd "C-`") 'imenu-popup-jump)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; move between "windows" and frames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "<H-left>")  'windmove-left)
(global-set-key (kbd "<H-right>") 'windmove-right)
(global-set-key (kbd "<H-up>")    'windmove-up)
(global-set-key (kbd "<H-down>")  'windmove-down)

(setq framemove-hook-into-windmove t)

;;resize windows
(require 'resizewindows)

;;display buffers
(setq split-height-threshold 0)
(setq split-width-threshold nil)
(setq sensible-display-buffer-rows 15)

(defun my-split-window-sensibly (window)
  (or (and (window-splittable-p window)
		   ;; Split window vertically.
		   (with-selected-window window
			 (split-window-below (- sensible-display-buffer-rows))))
      (and (window-splittable-p window t)
		   ;; Split window horizontally.
		   (with-selected-window window
			 (split-window-right)))
      (and (eq window (frame-root-window (window-frame window)))
		   (not (window-minibuffer-p window))
		   ;; If WINDOW is the only window on its frame and is not the
		   ;; minibuffer window, try to split it vertically disregarding
		   ;; the value of `split-height-threshold'.
		   (let ((split-height-threshold 0))
			 (when (window-splittable-p window)
			   (with-selected-window window
				 (split-window-below (- sensible-display-buffer-rows))))))))
(setq split-window-preferred-function 'my-split-window-sensibly)

;; bookmarks
(global-set-key "\C-b"     'bm-toggle)
(global-set-key (kbd "<M-prior>") 'bm-previous)
(global-set-key (kbd "<M-next>")  'bm-next)

;; editing
(global-set-key "\C-n" `new-scratch)
(global-set-key "\M-i" `indent-region)
(global-set-key [f7] 'toggle-truncate-lines)

(setq-default indent-tabs-mode nil)

;;F1 stuff

(global-unset-key (kbd "<f1>"))

(global-set-key (kbd "<f1> <left>")  'windmove-left)
(global-set-key (kbd "<f1> <right>") 'windmove-right)
(global-set-key (kbd "<f1> <up>")    'windmove-up)
(global-set-key (kbd "<f1> <down>")  'windmove-down)

(global-set-key (kbd "<f1> SPC")  'mark-sexp)

(global-set-key (kbd "<f1> .") 'highlight-symbol-next)
(global-set-key (kbd "<f1> ,") 'highlight-symbol-prev)

;;change the shiftiness of the number keys
;;found here: http://stackoverflow.com/questions/6277813/unshifted-symbols-in-emacs/6280799#6280799
(define-minor-mode snoopy-mode
  "Toggle snoopy mode.
   With no argument, this command toggles the mode.
   Non-null prefix argument turns on the mode.
   Null prefix argument turns off the mode."
  ;;   The initial value.
  nil
  ;; The indicator for the mode line.
  " Snoopy"
  ;; The minor mode bindings.
  '(
    ;; ("1" . (lambda () (interactive) (insert-char ?! 1)))
    ;; ("2" . (lambda () (interactive) (insert-char ?@ 1)))
    ;; ("3" . (lambda () (interactive) (insert-char ?# 1)))
    ;; ("4" . (lambda () (interactive) (insert-char ?$ 1)))
    ;; ("5" . (lambda () (interactive) (insert-char ?% 1)))
    ;; ("6" . (lambda () (interactive) (insert-char ?^ 1)))
    ;; ("7" . (lambda () (interactive) (insert-char ?& 1)))
    ;; ("8" . (lambda () (interactive) (insert-char ?* 1)))
    ("9" . (lambda () (interactive) (paredit-open-round)))
    ("0" . (lambda () (interactive) (paredit-close-round)))

    ;; ("!" . (lambda () (interactive) (insert-char ?1 1)))
    ;; ("@" . (lambda () (interactive) (insert-char ?2 1)))
    ;; ("#" . (lambda () (interactive) (insert-char ?3 1)))
    ;; ("$" . (lambda () (interactive) (insert-char ?4 1)))
    ;; ("%" . (lambda () (interactive) (insert-char ?5 1)))
    ;; ("^" . (lambda () (interactive) (insert-char ?6 1)))
    ;; ("&" . (lambda () (interactive) (insert-char ?7 1)))
    ;; ("*" . (lambda () (interactive) (insert-char ?8 1)))
    ("(" . (lambda () (interactive) (insert-char ?9 1)))
    (")" . (lambda () (interactive) (insert-char ?0 1)))))

(global-set-key (kbd "<pause>") 'snoopy-mode)

;(fmakunbound 'snoopy-mode)
;(makunbound 'snoopy-mode-map)

;;doesn't work:
(defun detach-buffer ()
  (interactive)
  (let ((old-buffer (current-buffer)))
	(delete-window)
	(make-frame)
	(switch-to-buffer old-buffer)))  

;; (global-set-key "\C-k"
;;      `(lambda () (
;; 	  (push-mark)
;; 	  (pop-mark)
;;       )))
;;end-of-line
;;beginning-of-line
;;delete-region
;;push-mark

(defun refresh-file ()
  (interactive)
  (revert-buffer t t t)
  )
(global-set-key [f5] `refresh-file)
(global-set-key [f6] `mark-whole-buffer)

;; (setq cua-keep-region-after-copy t)
(delete-selection-mode 0)
(setq make-backup-files nil) ;;no backups!
(setq auto-save-default nil) ; stop creating those #autosave# files
(setq visible-bell t)

(global-unset-key [?\C-f])
(global-unset-key [?\M-f])

;; programming
(setq comment-empty-lines t)
(define-key global-map (kbd "RET") 'newline-and-indent)

;;ocaml
(add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))
(add-hook 'tuareg-mode-hook 'merlin-mode)
(setq merlin-use-auto-complete-mode t)
(setq merlin-error-after-save nil)

(global-set-key (kbd "C-c C-g") 'magit-status)

(add-hook 'emacs-lisp-mode-hook '(lambda () (highlight-symbol-mode 1)))

;;custom faces
(set-face-attribute 'org-hide nil :foreground "DarkSlateGray")

(set-face-attribute 'org-link nil :foreground "CornflowerBlue")
(set-face-attribute 'org-link nil :underline t)

;(set-face-attribute 'popup-isearch-match nil :background "orange3")

(set-face-attribute 'highlight-symbol-face nil :background "orange3")
(set-face-attribute 'highlight-symbol-face nil :foreground "gray100")

(require 'dired)
(if window-system
    (set-face-attribute 'dired-directory nil :foreground "#5fd7ff")
  (set-face-attribute 'dired-directory nil :foreground "#0020ff"))
(set-face-attribute 'dired-marked nil :foreground "#5fff00")

;;prevent dired from opening new buffers on dir visit

;;prevent dired from opening new buffers on parent
(add-hook
 'dired-mode-hook
 (lambda ()
   (define-key dired-mode-map (kbd "^")
     (lambda () (interactive) (find-alternate-file "..")))))

(defun hide-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; org mode
(setq org-ellipsis " >>")
(setq org-confirm-babel-evaluate nil)

(setq org-todo-keyword-faces
      '(("PROG" . "yellow")
        ("BLOK" . "IndianRed1")))

(add-hook
 'org-mode-hook
 '(lambda () 
    (define-key org-mode-map [S-insert] 'org-complete)
    (define-key org-mode-map [S-return] 'org-insert-subheading)
    (define-key org-mode-map [M-left] 'org-promote-subtree)
    (define-key org-mode-map [M-right] 'org-demote-subtree)

    ;;yasnippet/org-mode key conflict fix
    (org-set-local 'yas/trigger-key [tab])
    (define-key yas/keymap [tab] 'yas/next-field-group)))
 
(setq org-support-shift-select t)
(transient-mark-mode 1)

(defun org-headings-to-items (start end)
  (interactive "r")
  (org-toggle-region-headings start end)
  (org-toggle-region-items start end)
  (indent-region start end)
  )

(defun org-copy-subtree-unpromoted ()
  (interactive)
  (outline-mark-subtree)
  (x-select-text
   (buffer-substring-no-properties (mark) (point))))

;; (defun org-copy-subtree-as-html ()
;;   (interactive)
;;   (outline-mark-subtree)
;;   (x-select-text
;;    (org-export-region-as-html (mark) (point) t 'string)))

(defun org-clean-stars ()
  (beginning-of-buffer)
  (show-all)
  (let ((lc (count-lines (point-min) (point-max))))
	(dotimes (number lc)
	  (let ((level (- (org-outline-level) 1)))
		(when (and (not (org-in-item-p)) (> level 0))
		  (delete-char level)
		  (insert (make-string level (string-to-char " "))))
		(forward-line)))))
	
(defun org-get-subtree-internal (clean)
  (outline-mark-subtree)
  (setq temp-buffer (generate-new-buffer "temp"))
  (copy-to-buffer temp-buffer (mark) (point))
  (switch-to-buffer temp-buffer)
  (org-mode)
  (let ((level (- (org-outline-level) 1)))
	(dotimes (n level nil) (org-promote-subtree)))
  (when clean (org-clean-stars))
  (x-select-text
   (buffer-substring-no-properties (point-min) (point-max)))
  (kill-buffer temp-buffer))

(defun org-get-subtree ()
  (interactive)
  (org-get-subtree-internal nil)
  (message "Promoted subtree copied to clipboard"))

(defun org-get-subtree-clean ()
  (interactive)
  (org-get-subtree-internal t)
  (message "Clean promoted subtree copied to clipboard"))

(global-set-key [?\M-e] `eval-region)
;;(global-set-key [f12] `iimage-mode)
(global-set-key [f8] `(lambda () (interactive) (find-file "i:/notes/notes.org")))


;; move region
;; found here: http://groups.google.com/group/gnu.emacs.help/browse_thread/thread/75dd91fd45742d54

(defun move-text-internal (arg) 
  (cond 
   ((and mark-active transient-mark-mode) 
	(if (> (point) (mark)) 
        (exchange-point-and-mark)) 
	(let ((column (current-column)) 
          (text (delete-and-extract-region (point) (mark)))) 
	  (forward-line arg) 
	  (move-to-column column t) 
	  (set-mark (point)) 
	  (insert text) 
	  (exchange-point-and-mark) 
	  (setq deactivate-mark nil))) 
   (t 
	(beginning-of-line) 
	(when (or (> arg 0) (not (bobp))) 
	  (forward-line) 
	  (when (or (< arg 0) (not (eobp))) 
        (transpose-lines arg)) 
	  (forward-line -1)))))
(defun move-text-down (arg) 
  "Move region (transient-mark-mode active) or current line 
  arg lines down." 
  (interactive "*p") 
  (move-text-internal arg))
(defun move-text-up (arg) 
  "Move region (transient-mark-mode active) or current line 
  arg lines up." 
  (interactive "*p") 
  (move-text-internal (- arg)))
;;(global-set-key [\M-up] 'move-text-up)
;;(global-set-key [\M-down] 'move-text-down)

;; the following two functions are from
;; http://osdir.com/ml/help-gnu-emacs-gnu/2009-09/msg00668.html

(defun elisp-pop-found-function ()
  (interactive)
  (cond ((featurep 'xemacs) (pop-tag-mark nil))
		(t (pop-tag-mark))))

(defun elisp-push-point-marker ()
  (require 'etags)
  (cond ((featurep 'xemacs)
		 (push-tag-mark))
		(t (ring-insert find-tag-marker-ring (point-marker)))))

(defun elisp-find-definition (name)
  "Jump to the definition of the function (or variable) at point."
  (interactive (list (thing-at-point 'symbol)))
  (cond (name
         (let ((symbol (intern-soft name))
               (search (lambda (fun sym)
                         (let* ((r (save-excursion (funcall fun sym)))
                                (buffer (car r))
                                (point (cdr r)))
                           (cond ((not point)
                                  (error "Found no definition for %s in %s"
                                         name buffer))
                                 (t
                                  (switch-to-buffer buffer)
                                  (goto-char point)
                                  (recenter 1)))))))
           (cond ((fboundp symbol)
                  (elisp-push-point-marker)
                  (funcall search 'find-function-noselect symbol))
                 ((boundp symbol)
                  (elisp-push-point-marker)
                  (funcall search 'find-variable-noselect symbol))
                 (t
                  (message "Symbol not bound: %S" symbol)))))
        (t (message "No symbol at point"))))

(global-set-key (kbd "M-.") 'elisp-find-definition)
(global-set-key (kbd "M-,") 'elisp-pop-found-function)


;;;;; font size control per buffer
(global-set-key (kbd "C-=") (lambda () (interactive) (text-scale-increase 0.5)))
(global-set-key (kbd "C--") (lambda () (interactive) (text-scale-increase -0.5)))
(global-set-key (kbd "C-0") (lambda () (interactive) (text-scale-increase 0)))

;;frame title
(setq frame-title-format
	  '("" (:eval (if (buffer-file-name)
					  (abbreviate-file-name (buffer-file-name))
					"%b")) " - emacs"))

;;(condition-case () ;;prevent error message when in console
;;    (set-face-font 'default "-monotype-andale mono-medium-r-*-*-*-120-*-*-*-*-*-*")
;;(error nil))

;(push '(font-backend xft x) default-frame-alist)

;;(set-background-color "#ffffe1")
;;(set-background-color "#ffffff")

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
;; (require 'package)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (package-initialize)

;; clojure stuff

(require 'clojure-dev)

;; mac-specific config
(if (or (string-equal system-type "darwin")
        (string-equal system-name "devbox-stathis.development.agentsmutual.co.uk"))
    (require 'mac))
(if (string-equal system-name "MUCHA")
    (require 'mucha))
(if (not window-system)
    (require 'no-window))

(require 'scala)

(put 'erase-buffer 'disabled nil)

;;auto-complete mode
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/ac")
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/site-lisp/ac/ac-dict")
;; (ac-config-default)
;; (setq ac-delay 1)
;; (setq ac-use-quick-help t)
;; (setq ac-quick-help-delay 0.6)
;; (add-hook
;;  'auto-complete-mode-hook
;;  (lambda ()
;;    (local-set-key (kbd "<C-pause>") 'auto-complete)))
;; (defun ac () (interactive) (auto-complete-mode))

;;ibuffer

(defun ibuffer-visit-buffer-other-window-no-new (&optional noselect)
  "Visit the buffer on this line in another window."
  (interactive)
  (let ((buf (ibuffer-current-buffer t)))
    (bury-buffer (current-buffer))
    (other-window -1)
    (switch-to-buffer buf)))

;;TODO
(defun ibuffer-delete-buffer ()
  (interactive)
  (ibuffer-mark-for-delete)
  (ibuffer-do-kill-on-deletion-marks))

(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
   (dolist (buf (buffer-list))
     (with-current-buffer buf
       (if (eq mode major-mode)
           (add-to-list 'buffer-mode-matches buf))))
   buffer-mode-matches))

(defun ibuffer-update-all ()
  (save-excursion
    (dolist (buf (get-buffers-matching-mode 'ibuffer-mode))
      (set-buffer buf)
      (ibuffer-update nil t))))

(eval-after-load 'ibuffer
  '(progn
     (define-key ibuffer-mode-map (kbd "RET") 'ibuffer-visit-buffer-other-window-no-new)
     (define-key ibuffer-mode-map [space] 'ibuffer-mark-forward)
     (add-hook 'ibuffer-hook (lambda ()
                               (ibuffer-switch-to-saved-filter-groups "default")))
     (add-hook 'buffer-list-update-hook 'ibuffer-update-all)))

;;;;;;;;;;;;

(defun sgml-kill-tag-region ()
  "Kills the next tag and all its children"
  (interactive)
  (save-excursion
	(setq s (point))
	(sgml-skip-tag-forward 1)
	(setq e (point))
	(sgml-skip-tag-backward 1)
	(message "%d, %d" s e)
	(kill-region s e)
	(kill-line)
	(indent-according-to-mode)))

(require 'help-mode)


(defun un-camelcase-string (s &optional sep start)
  "Convert CamelCase string S to lower case with word separator SEP.
   Default for SEP is a hyphen \"-\".
   If third argument START is non-nil, convert words after that
   index in STRING."
  (let ((case-fold-search nil))
    (while (string-match "[A-Z]" s (or start 1))
      (setq s (replace-match (concat (or sep "-") 
                                     (downcase (match-string 0 s))) 
                             t nil s)))
    (downcase s)))

(defun un-camelcase-region ()
  (interactive)
  (let ((s (buffer-substring (region-beginning) (region-end))))
    (delete-region (region-beginning) (region-end))
    (insert (un-camelcase-string s))))

(defun un-camelcase-symbol ()
  (interactive)
  (save-excursion
    (let ((s (format "%s" (symbol-at-point)))
          (bounds (bounds-of-thing-at-point 'symbol)))
      (let ((replacement (un-camelcase-string s)))
        (when replacement
          (delete-region (car bounds) (cdr bounds))
          (insert replacement))))))

(global-set-key (kbd "C-M--") 'un-camelcase-symbol)

(defun hide-scrollbars ()
  (interactive)
  (set-window-scroll-bars (selected-window) 0 nil nil))

(setq haskell-program-name "C:\\dev\\hugs\\hugs.exe")

;;To make ediff operate on selected-frame use the following:
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;;To make ediff to be horizontally split use:
(setq ediff-split-window-function 'split-window-horizontally)


;;;; xml

(defun pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))

;;;; zone screen saver

;; (require 'zone)
;; (setq zone-timer
;;       (run-with-idle-timer
;;        120 t
;;        (lambda
;;          ()
;;          (let ((zone-programs (list 'zone-pgm-putz-with-case)))
;;            (zone)))))
;;to cancel
;;(cancel-timer zone-timer)


(global-company-mode)

(defun indent-or-complete ()
  (interactive)
  (if (looking-at "\\_>")
      (company-complete-common)
    (indent-according-to-mode)))

(global-set-key "\t" 'indent-or-complete)

(defun escape-double-quotes (start end)
  (interactive "r")
  (let ((text (buffer-substring start end)))
    (save-excursion
      (delete-region start end)
      (insert (replace-regexp-in-string "\"" "\\\\\"" text)))))

;;highlight the region you want to double-quote, turn off paredit and run this:
(defun double-quote (start end)
  (interactive "r")
  (let ((text (buffer-substring start end)))
    (save-excursion
      (delete-region start end)
      (insert
       (concat "\"" (replace-regexp-in-string "\"" "\\\\\"" text) "\"")))))

;; (setq yas-snippet-dirs
;;   '("~/.emacs.d/site-lisp/mysnippets"))
(require 'yasnippet)
(yas-global-mode 1)

(put 'dired-find-alternate-file 'disabled nil)

;;to not get international characters on alt- mac
(set-keyboard-coding-system nil)

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(setq initial-scratch-message (get-string-from-file "~/.emacs.d/logo"))


(require 'sql)
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))
(define-key sql-mode-map (kbd "C-x C-e") 'sql-send-paragraph)


(require 'expand-region)
(global-set-key (kbd "C-\\") 'er/expand-region)
;;(pending-delete-mode 'no)

(maybe-install-and-require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

(maybe-install-and-require 'multiple-cursors)
(global-set-key (kbd "C-c .") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c ,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c /") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(defun tabs->newlines ()
  (interactive)
  (replace-string "\t" "\n"))

(defun mce ()
  (interactive)
  (mc/edit-lines))

(require 'tiling)
(define-key global-map (kbd "C-\\") 'tiling-cycle)

(require 'ttl-mode)


;; Flash active buffer

(global-set-key (kbd "C-c ;") 'flash-active-buffer)
(make-face 'flash-active-buffer-face)
(set-face-attribute 'flash-active-buffer-face nil
                    :background "#808080"
                    :foreground "black")
(defun flash-active-buffer ()
  (interactive)
  (run-at-time "200 millisec" nil
               (lambda (remap-cookie)
                 (face-remap-remove-relative remap-cookie))
               (face-remap-add-relative 'default 'flash-active-buffer-face)))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-prompt-for-symbol nil)
 '(cljr-cljc-clojure-test-declaration
   "#?(:clj [clojure.test :refer :all]
:cljs [cljs.test :refer :all :include-macros true])")
 '(cljr-clojure-test-declaration "[clojure.test :refer :all]")
 '(custom-safe-themes
   (quote
    ("e80932ca56b0f109f8545576531d3fc79487ca35a9a9693b62bf30d6d08c9aaf" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "31a01668c84d03862a970c471edbd377b2430868eccf5e8a9aec6831f1a0908d" "1297a022df4228b81bc0436230f211bad168a117282c20ddcba2db8c6a200743" default)))
 '(frame-resize-pixelwise t)
 '(git-commit-summary-max-length 120)
 '(js-indent-level 2)
 '(ns-use-srgb-colorspace nil)
 '(org-time-clocksum-format
   (quote
    (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))
 '(package-selected-packages
   (quote
    (emacsd-tile ttl-mode browse-kill-ring sbt-mode scala-mode2 slime solarized-theme powerline clj-refactor zenburn-theme yasnippet undo-tree tuareg puppet-mode paredit multiple-cursors markdown-mode magit ido-ubiquitous highlight-symbol git-gutter emmet-mode diminish company cider align-cljlet ag)))
 '(powerline-default-separator nil)
 '(sbt:program-name "/usr/local/bin/sbt"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bold ((t (:foreground "white" :weight bold))))
 '(cider-repl-stdout-face ((t (:foreground "#c0c0c0"))))
 '(dired-directory ((t (:inherit font-lock-function-name-face :foreground "#55bbff"))))
 '(font-lock-string-face ((t (:foreground "#606060"))))
 '(hl-line ((t (:background "color-17"))))
 '(magit-blame-date ((t (:background "#404040" :foreground "#F2804F"))))
 '(magit-blame-heading ((t (:background "#404040" :foreground "#073642"))))
 '(magit-blame-name ((t (:inherit magit-blame-heading :background "#404040" :foreground "#F2804F"))))
 '(magit-blame-summary ((t (:background "#404040" :foreground "#F2804F" :weight bold))))
 '(magit-diff-hunk-heading ((t (:background "#009F00" :foreground "black"))))
 '(magit-diff-hunk-heading-highlight ((t (:background "#5FFF5F" :foreground "black"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.8))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.5))))
 '(org-clock-overlay ((t (:background "Black"))))
 '(org-level-1 ((t (:inherit fixed-pitch :foreground "#cb4b16" :height 1.3))))
 '(org-level-2 ((t (:inherit fixed-pitch :foreground "#859900" :height 1.2))))
 '(org-level-3 ((t (:inherit fixed-pitch :foreground "#268bd2" :height 1.15))))
 '(org-level-4 ((t (:inherit fixed-pitch :foreground "#b58900" :height 1.1))))
 '(org-level-5 ((t (:inherit fixed-pitch :foreground "#2aa198"))))
 '(show-paren-match ((t (:foreground "green" :background "Black" :weight bold))))
 '(smerge-refined-added ((t (:inherit smerge-refined-change :background "#225522")))))
