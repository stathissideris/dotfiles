;; ========================================
;; package
(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant


;; ========================================
;; Machine-specific config

(if (string-equal system-type "darwin")
    (require 'mac))

;; ========================================
;; Modes

(use-package clojure-mode
  :ensure t
  :pin melpa-stable
  :bind (("C-x t" . clojure-jump-to-test))
  :config

  (setq auto-mode-alist (cons '("\\.edn$" . clojure-mode) auto-mode-alist))

  (defun ss/string-join (sep s)
    (mapconcat 'identity s sep))

  (defun toggle-test-path (path)
    (ss/string-join
     "/"
     (mapcar
      (lambda (x)
        (cond ((string-equal x "test") "src")
              ((string-equal x "src") "test")
              ((string-match "\\(.+\\)_test\\.clj" x)
               (concat (match-string 1 x) ".clj"))
              ((string-match "\\(.+\\)\\.clj" x)
               (concat (match-string 1 x) "_test.clj"))
              (t x)))
      (split-string path "/"))))

  (defun clojure-jump-to-test ()
    "Jump to corresponding test buffer (or the corresponding src
  buffer if you're in a test."
    (interactive)
    (find-file (toggle-test-path buffer-file-name)))

  (setq safe-local-variable-values
	(quote
	 ((eval define-clojure-indent
		(snippet
		 (quote defun))
		(template
		 (quote defun)))))))

(use-package clj-refactor
  :ensure t
  :pin melpa-stable
  :diminish clj-refactor-mode
  :config
  (setq cljr-clojure-test-declaration "[clojure.test :refer :all]")
  (setq cljr-cljc-clojure-test-declaration
                                       "#?(:clj [clojure.test :refer :all] :cljs [cljs.test :refer :all :include-macros true])")
  (add-hook 'clojure-mode-hook (lambda ()
                                 (clj-refactor-mode 1)
                                 (cljr-add-keybindings-with-prefix "C-c C-v"))))

(use-package align-cljlet
  :ensure t
  :pin marmalade
  :config
  (add-hook 'clojure-mode-hook
          '(lambda ()
             (define-key clojure-mode-map "\C-c\C-a" 'align-cljlet))))

(use-package paredit
  :ensure t
  :pin melpa-stable
  :diminish (paredit-mode . "prdt")
  :bind (("C-d" . duplicate-sexp)
         ("M-{" . paredit-wrap-curly)
         ("M-[" . paredit-wrap-square)
         ("<C-M-up>" . transpose-sexp-backward)
         ("<C-M-down>" . transpose-sexp-forward))
  :config
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'scheme-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode)

  (defun duplicate-sexp ()
    "Duplicates the sexp at point."
    (interactive)
    (save-excursion
      (forward-sexp)
      (backward-sexp)
      (let ((bounds (bounds-of-thing-at-point 'sexp)))
        (insert (concat (buffer-substring (car bounds) (cdr bounds)) "\n"))
        (indent-for-tab-command))))

  (defun transpose-sexp-forward ()
    (interactive)
    (forward-sexp)
    (transpose-sexps 1)
    (backward-sexp))

  (defun transpose-sexp-backward ()
    (interactive)
    (forward-sexp)
    (transpose-sexps -1)
    (backward-sexp)))

(use-package show-paren-mode
  :init
  (show-paren-mode)
  (custom-set-faces
   '(show-paren-match ((t (:foreground "green" :background "Black" :weight bold))))))

(use-package cider
  :ensure t
  :defer t
  :pin melpa-stable
  :bind (("C-c M-o" . cider-repl-clear-buffer)
         ("<f2>" . clojure-quick-eval)
         ("<f12>" . apply-fix-macro)
         ;;("{" . 'paredit-open-curly);;TODO
         )
  :config
  (setq cider-promapt-for-symbol nil)
  (setq cider-repl-history-file "~/.emacs.d/cider-history")
  (setq cider-font-lock-dynamically '(macro core function var))
  (setq cider-repl-use-pretty-printing nil)
  (setq cider-repl-use-clojure-font-lock t)
  (setq cider-repl-result-prefix ";; => ")
  (setq cider-repl-wrap-history t)
  (setq cider-repl-history-size 3000)
  (setq cider-show-error-buffer 'except-in-repl)
  (setq cider-repl-display-help-banner nil)
  (setq cider-inject-dependencies-at-jack-in nil)
  (add-hook 'cider-mode-hook #'eldoc-mode)

  (setq clojure-quick-sexp
        '("(user/refresh)"
          "(use 'clojure.repl)"
          "(use 'clojure.tools.trace)"
          "(use 'clojure.pprint)"
          "(dev/start-cljs-figwheel)"))

  (defun clojure-quick-eval ()
    (interactive)
    (let ((selection (ido-completing-read
                      "Clojure eval: " clojure-quick-sexp nil t "")))
      (cider-interactive-eval selection)))

  (defun macroexpand-replace ()
    (interactive)
    (let ((exp
           (cider-sync-request:macroexpand
            "macroexpand-1"
            (cider-last-sexp))))
      (backward-sexp)
      (let ((bounds (bounds-of-thing-at-point 'sexp)))
        (delete-region (car bounds) (cdr bounds))
        (insert exp)
        (indent-for-tab-command))))

  (defun apply-fix-macro ()
    (interactive)
    (paredit-wrap-round)
    (insert "fix ")
    (forward-sexp)
    (forward-char 1)
    (macroexpand-replace)
    (backward-sexp))

  (defun replace-not-in-strings (start end match replacement)
    "Only tested on single characters"
    (set-mark nil)
    (let ((p (point)))
      (setq pos start)
      (while (< pos end)
        (goto-char pos)
        (let ((faces (face-at-point t t)))
          (princ faces)
          (princ "\n")
          (cond ((member 'font-lock-string-face faces)
                 (princ "case 1\n")
                 (setq pos (1+ pos)))

                ((string-equal match (buffer-substring pos (1+ pos)))
                 (princ "case 2\n")
                 (delete-char 1)
                 (insert replacement)
                 (setq pos (1+ pos)))

                (:else (setq pos (1+ pos))))))
      (goto-char p)))

  (defun format-map ()
    (interactive)
    (let ((p (point)))
      (mark-sexp)
      (replace-not-in-strings (region-beginning) (region-end) "," "\n")
      (goto-char p)

      (mark-sexp)
      (replace-not-in-strings (region-beginning) (region-end) "} {" "}\n{")
      (goto-char p)

      (mark-sexp)
      (indent-region (region-beginning) (region-end)))))

(use-package projectile
  :pin melpa-stable
  :config
  (setq projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name)))
    projectile-globally-ignored-files '("TAGS" ".nrepl-port")
    projectile-globally-ignored-file-suffixes '("pyc")
    projectile-globally-ignored-directories
    '(".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" "venv" "build"
      "vendor" "vendors" ".cabal-sandbox" "dist" ".vagrant" "node_modules"
      "bower_components" ".bundle" ".stack-work"))
  (projectile-global-mode nil))

(use-package dockerfile-mode
  :ensure t
  :defer t
  :pin melpa-stable)

(use-package company
  :ensure t
  :pin melpa-stable
  :diminish company-mode
  :config
  (global-company-mode))

(use-package ido-ubiquitous
  :ensure t
  :pin melpa-stable
  :config
  (ido-mode t)
  (ido-ubiquitous)
  (setq ido-enable-flex-matching t))

(use-package org
  :ensure t
  :pin melpa-stable
  :defer t
  :bind (("<S-insert>" . org-complete)
         ("<S-return>" . org-insert-subheading))
  :config
  (setq org-ellipsis " >>")
  (setq org-confirm-babel-evaluate nil)
  (setq org-todo-keyword-faces
        '(("PROG" . "yellow")
          ("BLOK" . "IndianRed1")))
  (setq org-support-shift-select t)
  (setq org-hide-leading-stars t)
  (setq org-time-clocksum-format
        (quote
         (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))
  (set-face-attribute 'org-hide nil :foreground "DarkSlateGray")
  (set-face-attribute 'org-link nil :foreground "CornflowerBlue")
  (set-face-attribute 'org-link nil :underline t)
  (custom-set-faces
   '(org-clock-overlay ((t (:background "Black"))))
   '(org-level-1 ((t (:inherit fixed-pitch :foreground "#cb4b16" :height 1.3))))
   '(org-level-2 ((t (:inherit fixed-pitch :foreground "#859900" :height 1.2))))
   '(org-level-3 ((t (:inherit fixed-pitch :foreground "#268bd2" :height 1.15))))
   '(org-level-4 ((t (:inherit fixed-pitch :foreground "#b58900" :height 1.1))))
   '(org-level-5 ((t (:inherit fixed-pitch :foreground "#2aa198"))))))

(use-package magit
  :ensure t
  :pin melpa-stable
  :config
  (global-set-key (kbd "C-c C-g") 'magit-status)

  (setq git-commit-fill-column 3000)
  (setq git-commit-finish-query-functions nil)
  (setq git-commit-summary-max-length 120)

  (custom-set-faces
   '(magit-blame-date ((t (:background "#404040" :foreground "#F2804F"))))
   '(magit-blame-heading ((t (:background "#404040" :foreground "#073642"))))
   '(magit-blame-name ((t (:inherit magit-blame-heading :background "#404040" :foreground "#F2804F"))))
   '(magit-blame-summary ((t (:background "#404040" :foreground "#F2804F" :weight bold))))
   '(magit-diff-hunk-heading ((t (:background "#009F00" :foreground "black"))))
   '(magit-diff-hunk-heading-highlight ((t (:background "#5FFF5F" :foreground "black"))))))

;; ========================================
;; Navigation

(use-package dired
  ;;:bind (("<^>" . (lambda () (find-alternate-file "..")))) ;;TODO
  :config
  (setq dired-dwim-target t)
  (put 'dired-find-alternate-file 'disabled nil)

  (custom-set-faces
   '(dired-directory ((t (:inherit font-lock-function-name-face :foreground "#55bbff")))))

  (defun kill-dired-buffers ()
    (interactive)
    (mapc (lambda (buffer)
	    (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
	      (kill-buffer buffer)))
	  (buffer-list)))
  (add-hook 'dired-mode-hook
	    '(lambda ()
               (set-face-attribute 'dired-marked nil :foreground "#5fff00")
               (set-face-attribute 'dired-directory nil :foreground "#55bbff")
               (if window-system
                   (set-face-attribute 'dired-directory nil :foreground "#5fd7ff")
                 (set-face-attribute 'dired-directory nil :foreground "#0020ff")))))

(use-package linum
  :config
  (if window-system
      (setq linum-format "%d")
    (setq linum-format "%d "))
  (global-set-key (kbd "<f11>") 'linum-mode))

(use-package highlight-symbol
  :diminish t
  :config
  (global-set-key (kbd "C-,") 'highlight-symbol-prev)
  (global-set-key (kbd "C-.") 'highlight-symbol-next)

  (set-face-attribute 'highlight-symbol-face nil :background "orange3")
  (set-face-attribute 'highlight-symbol-face nil :foreground "gray100")

  (add-hook 'lisp-mode-hook 'highlight-symbol-mode)
  (add-hook 'emacs-lisp-mode-hook 'highlight-symbol-mode)
  (add-hook 'scheme-mode-hook 'highlight-symbol-mode)
  (add-hook 'cider-repl-mode-hook 'highlight-symbol-mode)
  (add-hook 'clojure-mode-hook 'highlight-symbol-mode))

(use-package windmove
  :init
  (global-set-key (kbd "<H-left>")  'windmove-left)
  (global-set-key (kbd "<H-right>") 'windmove-right)
  (global-set-key (kbd "<H-up>")    'windmove-up)
  (global-set-key (kbd "<H-down>")  'windmove-down)
  (global-set-key (kbd "<f1> <left>")  'windmove-left)
  (global-set-key (kbd "<f1> <right>") 'windmove-right)
  (global-set-key (kbd "<f1> <up>")    'windmove-up)
  (global-set-key (kbd "<f1> <down>")  'windmove-down))

(use-package undo-tree
  :ensure t
  :pin marmalade
  :diminish (undo-tree-mode . "UT")
  :config
  (global-undo-tree-mode)

  (custom-set-faces
   '(undo-tree-visualizer-active-branch-face ((t (:background "#002b36" :foreground "gray95" :weight bold))))))

(use-package browse-kill-ring
  :ensure t
  :pin melpa-stable
  :config
  (browse-kill-ring-default-keybindings))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-x .") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-x ,") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-x /") 'mc/mark-all-dwim)
  (global-set-key (kbd "C-S-x C-S-x") 'mc/edit-lines)
  (defun mce ()
    (interactive)
    (mc/edit-lines)))

;; bookmarks
(use-package bm
  :ensure t
  :bind (("C-b" . bm-toggle)
         ("<M-prior>" . bm-previous)
         ("<M-next>" . bm-next)))

(use-package tiling
  :config
  (global-set-key (kbd "C-\\") 'tiling-cycle))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

;; Silver searcher
(use-package ag
  :ensure t
  :pin melpa-stable
  :bind ("C-x M-f" . mt-ag-search)
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t)
  (defun mt-ag-search (string file-regex directory)
    (interactive (list (read-from-minibuffer "Search string: " (ag/dwim-at-point))
		       (read-from-minibuffer "In filenames matching PCRE: " (ag/buffer-extension-regex))
		       (read-directory-name "Directory: " (ag/project-root default-directory))))
    (ag/search string directory :file-regex file-regex)))

;; ========================================
;; Colors and looks

(use-package zenburn-theme
  :ensure t)

(use-package solarized-theme
  :ensure t
  :config
  (if window-system
    (progn
      (load-theme 'solarized-dark t)
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (set-default 'cursor-type 'bar)
      (set-cursor-color "yellow")
      (setq x-underline-at-descent-line t))
    (load-theme 'zenburn t)))

;; ========================================
;; Misc - fake packages

(use-package global-custom-keys
  :init
  (global-set-key (kbd "C-=") (lambda () (interactive) (text-scale-increase 0.5)))
  (global-set-key (kbd "C--") (lambda () (interactive) (text-scale-increase -0.5)))
  (global-set-key (kbd "C-0") (lambda () (interactive) (text-scale-increase 0)))

  (global-set-key (kbd "<f1> SPC")  'mark-sexp)
  (global-set-key (kbd "<f1> .") 'highlight-symbol-next)
  (global-set-key (kbd "<f1> ,") 'highlight-symbol-prev)

  (global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
  (global-set-key [f7] 'toggle-truncate-lines)
  (global-set-key (kbd "RET") 'newline-and-indent)

  (defun refresh-file ()
    (interactive)
    (revert-buffer t t t))
  (global-set-key [f5] `refresh-file)
  (global-set-key [f6] `mark-whole-buffer))

(use-package super-slow-scroll
  :init
  (setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ;; one two lines at a time
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
  (setq scroll-step 1) ;; keyboard scroll one line at a time
  (setq scroll-conservatively 10000))

(use-package misc-custom-vars
  :init
  (setq frame-resize-pixelwise t)
  (setq inhibit-splash-screen t)
  (setq comment-empty-lines t)
  (setq visible-bell t)
  (setq make-backup-files nil) ;; no backups!
  (setq auto-save-default nil) ;; stop creating those #autosave# files
  (column-number-mode t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(use-package spaces-instead-of-tabs
  :init
  (setq-default indent-tabs-mode nil)
  (setq default-tab-width 2)
  (setq tab-width 2)
  (setq python-indent 3)
  (setq c-basic-offset 3)
  (setq c-indent-level 3)
  (setq c++-tab-always-indent nil)
  (setq js-indent-level 2)
  (setq lua-indent-level 2))

(use-package custom-scratch-message
  :init
  (defun get-string-from-file (filePath)
    "Return filePath's file content."
    (with-temp-buffer
      (insert-file-contents filePath)
      (buffer-string)))
  (setq initial-scratch-message (get-string-from-file "~/.emacs.d/logo")))
