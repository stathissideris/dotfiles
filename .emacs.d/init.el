;;early background to prevent white emacs blinding me
(custom-set-faces
 '(default ((t (:background "#022b35"))))
 '(bold ((t (:foreground "gold" :weight bold)))))

;; ========================================
;; package
(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
;;(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

(diminish 'eldoc-mode)

(setenv "bsq" "/Volumes/work/bsq/")
(setenv "osio" "~/devel/work/osio/")

;; ERC config

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'ss-erc)

;; ========================================
;; Modes

(use-package all-the-icons
  :ensure t)
;;then run (all-the-icons-install-fonts) once

(use-package log4j-mode
  :ensure t
  :disabled t
  :init
  (add-hook #'log4j-mode-hook #'view-mode)
  (add-hook #'log4j-mode-hook #'read-only-mode))

(use-package hideshow
  :bind (("C-c TAB" . hs-toggle-hiding)
         ("C-\\" . hs-toggle-hiding)
         ("M-+" . hs-show-all))
  :init (add-hook #'prog-mode-hook #'hs-minor-mode)
  :diminish hs-minor-mode
  :config
  (setq hs-special-modes-alist
        (mapcar 'purecopy
                '((c-mode "{" "}" "/[*/]" nil nil)
                  (c++-mode "{" "}" "/[*/]" nil nil)
                  (java-mode "{" "}" "/[*/]" nil nil)
                  (js-mode "{" "}" "/[*/]" nil)
                  (clojure-mode "[\(\[{]" "[\)\]}]" "#" nil nil)
                  (emacs-lisp-mode "\(" "\)" "#" nil nil)
                  (json-mode "{" "}" "/[*/]" nil)
                  (javascript-mode  "{" "}" "/[*/]" nil)))))

(use-package emacs-lisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq mode-name "elisp"))))

(use-package clojure-mode
  :ensure t
  :pin melpa-stable
  :diminish (clojure-mode . "clj")
  :defines clojure-mode-map
  :bind (("C-x t" . clojure-jump-to-test)
         ("C-c C-w" . cider-eval-last-sexp-and-replace)
         ("C-c M-e" . cider-eval-print-last-sexp))
  :mode (("\\.edn$" . clojure-mode))
  :config
  (add-hook 'clojure-mode-hook
          (lambda ()
            (setq mode-name "λ")))

  (custom-set-faces
   '(font-lock-doc-face ((t (:foreground "dark slate gray" :slant normal)))))

  (defun ss/string-join (sep s)
    (mapconcat 'identity s sep))

  (defun toggle-test-path (path)
    (ss/string-join
     "/"
     (mapcar
      (lambda (x)
        (cond ((string-equal x "test") "src")
              ((string-equal x "src") "test")

              ((string-equal x "src-cljs") "test-cljs")
              ((string-equal x "test-cljs") "src-cljs")

              ((string-match "\\(.+\\)_test\\.clj\\(.?\\)" x)
               (concat (match-string 1 x) ".clj" (match-string 2 x)))
              ((string-match "\\(.+\\)\\.clj\\(.?\\)" x)
               (concat (match-string 1 x) "_test.clj" (match-string 2 x)))

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
  :init
  (add-hook 'clojure-mode-hook (lambda ()
                                 (clj-refactor-mode 1)
                                 (cljr-add-keybindings-with-prefix "C-c C-v")))
  :config
  (setq cljr-clojure-test-declaration "[clojure.test :refer :all]")
  (setq cljr-cljc-clojure-test-declaration
        "#?(:clj [clojure.test :refer :all] :cljs [cljs.test :refer :all :include-macros true])")
  (setq cljr-warn-on-eval nil))

(use-package align-cljlet
  :ensure t
  :pin marmalade
  :init
  (add-hook 'clojure-mode-hook
          '(lambda ()
             (define-key clojure-mode-map "\C-c\C-a" 'align-cljlet))))

(use-package paredit
  :ensure t
  :pin melpa-stable
  :diminish (paredit-mode . " Ⓟ")
  :bind (("C-d" . duplicate-sexp)
         ("M-{" . paredit-wrap-curly)
         ("M-[" . paredit-wrap-square)
         ("<C-M-up>" . transpose-sexp-backward)
         ("<C-M-down>" . transpose-sexp-forward)
         ("<M-S-left>" . backward-sexp)
         ("<M-S-right>" . forward-sexp))
  :init
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
    (backward-sexp))

  :config
  (define-key paredit-mode-map "\C-d" 'duplicate-sexp))

(use-package sgml-mode
  :config
  :bind (:map sgml-mode-map
         ("<f1> SPC" . sgml-mark-tag))
  :mode (("\\.html$" . sgml-mode)
         ("\\.xml$" . sgml-mode))
  :config
  (defun sgml-mark-tag ()
    (interactive)
    (if (= 60 (char-after))
        (progn
          (sgml-skip-tag-forward 1)
          (set-mark-command nil)
          (sgml-skip-tag-backward 1))
      (mark-sexp))))

(use-package tagedit
  :ensure t
  :init
  (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))
  :config
  (tagedit-add-paredit-like-keybindings))

(use-package paren
  :init
  (add-hook 'lisp-mode-hook 'show-paren-mode)
  (add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
  (add-hook 'scheme-mode-hook 'show-paren-mode)
  (add-hook 'cider-repl-mode-hook 'show-paren-mode)
  (add-hook 'clojure-mode-hook 'show-paren-mode)
  (custom-set-faces
   '(show-paren-match ((t (:foreground "gray100" :background "#9c7618" :weight bold))))))

(use-package cider
  :ensure t
  :defer t
  :diminish (cider-mode . " 🍺")
  :pin melpa-stable
  :bind (("C-c M-o" . cider-repl-clear-buffer)
         ("<f2>" . clojure-quick-eval)
         ("<f12>" . apply-fix-macro))
  :init
  (add-hook 'cider-mode-hook #'eldoc-mode)
  :config
  (set-face-attribute 'cider-test-failure-face nil :background "#8c2020")
  (setq cider-prompt-for-symbol nil)
  (setq cider-repl-history-file "~/.emacs.d/cider-history")
  (setq cider-font-lock-dynamically '(macro core function var))
  (setq cider-repl-use-pretty-printing nil)
  (setq cider-repl-use-clojure-font-lock t)
  ;;(setq cider-repl-result-prefix ";; => ")
  (setq cider-repl-wrap-history t)
  (setq cider-repl-history-size 3000)
  (setq cider-show-error-buffer 'except-in-repl)
  (setq cider-repl-display-help-banner nil)
  (setq cider-inject-dependencies-at-jack-in nil)

  (bind-key "C-c M-o" 'cider-repl-clear-buffer cider-repl-mode-map)

  (setq clojure-quick-sexp
        '("(dev/refresh)"
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
      (indent-region (region-beginning) (region-end))

      (mark-sexp)
      (replace-not-in-strings (region-beginning) (region-end) "} {" "}\n{")
      (goto-char p)

      (mark-sexp)
      (indent-region (region-beginning) (region-end)))))

(use-package projectile
  :pin melpa-stable
  :diminish projectile-mode
  :config
  (setq projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name)))
    projectile-globally-ignored-files '("TAGS" ".nrepl-port")
    projectile-globally-ignored-file-suffixes '("pyc")
    projectile-globally-ignored-directories
    '(".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" "venv" "build"
      "vendor" "vendors" ".cabal-sandbox" "dist" ".vagrant" "node_modules"
      "bower_components" ".bundle" ".stack-work"))
  (projectile-global-mode nil))

(use-package yasnippet
  :ensure t
  :pin melpa-stable
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-load-directory "~/.emacs.d/snippets"))

(use-package dockerfile-mode
  :ensure t
  :defer t
  :pin melpa-stable)

(use-package company
  :ensure t
  :pin melpa-stable
  :diminish company-mode
  :bind (("<s-SPC>" . company-complete))
  :init
  (global-company-mode)
  (setq company-begin-commands
        '(self-insert-command org-self-insert-command orgtbl-self-insert-command c-scope-operator c-electric-colon c-electric-lt-gt c-electric-slash cljr-slash)))

(use-package ido-ubiquitous
  :ensure t
  :pin melpa-stable
  :init
  (ido-mode t)
  (ido-ubiquitous)
  (setq ido-enable-flex-matching t))

(defun org-clocktable-try-shift-left ()
  (interactive)
  (org-clocktable-try-shift 'left 1))

(defun org-clocktable-try-shift-right ()
  (interactive)
  (org-clocktable-try-shift 'right 1))

(use-package org
  :ensure t
  :pin org
  :defer t
  :bind (("<S-insert>" . org-complete)
         ("<S-return>" . org-insert-subheading)
         ("<s-return>" . org-insert-subheading)
         ("<S-left>" . org-clocktable-try-shift-left)
         ("<S-right>" . org-clocktable-try-shift-right)
         ("<f12>" . org-tree-slide-mode)
         ("<S-f12>" . org-tree-slide-skip-done-toggle))
  :init
  (font-lock-add-keywords 'org-mode
                          '(("^ +\\([-*]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "→"))))))
  :config
  (org-tree-slide-simple-profile)

  (defvar yt-iframe-format
    ;; You may want to change your width and height.
    (concat "<iframe width=\"440\""
            " height=\"335\""
            " src=\"https://www.youtube.com/embed/%s\""
            " frameborder=\"0\""
            " allowfullscreen>%s</iframe>"))

  (org-add-link-type
   "yt"
   (lambda (handle)
     (browse-url
      (concat "https://www.youtube.com/embed/"
              handle)))
   (lambda (path desc backend)
     (cl-case backend
       (html (format yt-iframe-format
                     path (or desc "")))
       (latex (format "\href{%s}{%s}"
                      path (or desc "video"))))))


  (setq org-ellipsis "…" ;;"↴"
        org-todo-keywords '((sequence "TODO" "PROG" "BLOK" "DONE"))
        org-todo-keyword-faces
        '(;;("PROG" . "yellow")
          ("BLOK" . "IndianRed1"))
        org-support-shift-select t
        org-hide-emphasis-markers t
        org-hide-leading-stars t

        org-confirm-babel-evaluate nil
        org-outline-path-complete-in-steps nil
        org-completion-use-ido t
        org-src-fontify-natively t
        org-src-tab-acts-natively nil
        org-babel-hash-show-time t
        org-src-preserve-indentation nil
        org-startup-with-inline-images t

        org-clock-display-default-range 'untilnow
        org-clock-into-drawer nil
        org-duration-format '(("h" . t) ("min" . t))

        org-export-babel-evaluate nil

        org-image-actual-width nil

        org-html-htmlize-output-type 'css)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((shell      . t)
                                 (js         . t)
                                 (emacs-lisp . t)
                                 (perl       . t)
                                 (scala      . t)
                                 (clojure    . t)
                                 (python     . t)
                                 (ruby       . t)
                                 (dot        . t)
                                 ;;(R          . t)
                                 (sql        . t)
                                 (css        . t)))

  ;; (defun sql-to-org-table ()
  ;;   (interactive)
  ;;   (mc/edit-lines)
  ;;   (org-force-self-insert "|")
  ;;   (multiple-cursors-mode))

  (set-face-attribute 'org-hide nil :foreground "DarkSlateGray")
  (set-face-attribute 'org-link nil :foreground "CornflowerBlue")
  (set-face-attribute 'org-link nil :underline t)
  (font-lock-add-keywords
   'org-mode `(("^\\*+ \\(TODO\\) " (1 (progn (compose-region (match-beginning 1) (match-end 1) "□") nil)))
               ("^\\*+ \\(PROG\\) " (1 (progn (compose-region (match-beginning 1) (match-end 1) "▶") nil)))
               ("^\\*+ \\(BLOK\\) " (1 (progn (compose-region (match-beginning 1) (match-end 1) "✘") nil)))
               ("^\\*+ \\(DONE\\) " (1 (progn (compose-region (match-beginning 1) (match-end 1) "✔") nil)))))
  (let* ((ss/variable-font-tuple (list :font "Monaco"))
         (ss/fixed-font-tuple    (list :font "Monaco" :height 1.0))
         (base-font-color        "grey65")
         (background-color       (face-background 'default nil 'default))
         (primary-color          (face-foreground 'mode-line nil))
         (secondary-color        (face-background 'secondary-selection nil 'region))
         (base-height            (face-attribute 'default :height))
         (headline               `(:inherit default :weight regular :foreground ,base-font-color)))

    (custom-set-faces `(org-agenda-structure ((t (:inherit default :height 2.0 :underline nil))))
                      `(org-verbatim ((t (:inherit 'fixed-pitched :foreground "#aef"))))
                      `(org-table ((t (:inherit 'fixed-pitched))))
                      `(org-block ((t (:inherit 'fixed-pitched))))
                      `(org-block-background ((t (:inherit 'fixed-pitched))))
                      `(org-block-begin-line ((t (:background "gray15" :foreground "gray22" :slant normal))))
                      `(org-block-end-line ((t (:background "gray15" :foreground "gray22" :slant normal))))

                      ;;levels
                      `(org-level-8 ((t (,@headline ,@ss/fixed-font-tuple))))
                      `(org-level-7 ((t (,@headline ,@ss/fixed-font-tuple))))
                      `(org-level-6 ((t (,@headline ,@ss/fixed-font-tuple))))
                      `(org-level-5 ((t (,@headline ,@ss/fixed-font-tuple))))
                      `(org-level-4 ((t (,@headline ,@ss/fixed-font-tuple))))
                      `(org-level-3 ((t (,@headline ,@ss/fixed-font-tuple))))
                      `(org-level-2 ((t (,@headline ,@ss/fixed-font-tuple))))
                      `(org-level-1 ((t (,@headline ,@ss/fixed-font-tuple))))

                      `(org-document-title ((t (,@headline ,@ss/variable-font-tuple :height 1.5 :underline nil)))))))

(use-package org-page
  :ensure t
  :init
  (setq op/repository-directory "/Users/sideris/devel/work/www.pixelated-noise.com/"
        op/site-domain "http://www.pixelated-noise.com/"
        op/repository-org-branch "blog-source"
        op/repository-html-branch "gh-pages"))

(use-package org-bullets
  :ensure t
  :init
  (setq org-bullets-bullet-list '("●"))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package magit
  :ensure t
  :pin melpa-stable
  :diminish auto-revert-mode
  :config
  (global-set-key (kbd "C-c C-g") 'magit-status)

  (setq git-commit-fill-column 3000
        git-commit-finish-query-functions nil
        git-commit-summary-max-length 120)

  (custom-set-faces
   '(magit-blame-date ((t (:background "#404040" :foreground "#F2804F"))))
   '(magit-blame-heading ((t (:background "#404040" :foreground "#073642"))))
   '(magit-diff-file-heading-highlight ((t (:background "#073642" :weight semi-bold))))
   '(magit-blame-name ((t (:inherit magit-blame-heading :background "#404040" :foreground "#F2804F"))))
   '(magit-blame-summary ((t (:background "#404040" :foreground "#F2804F" :weight bold))))
   '(magit-diff-hunk-heading ((t (:background "#009F00" :foreground "black"))))
   '(magit-diff-hunk-heading-highlight ((t (:background "#5FFF5F" :foreground "black"))))
   '(magit-popup-argument ((t (:foreground "white"))))
   '(smerge-refined-added ((t (:inherit smerge-refined-change :background "#227022"))))))

;; ========================================
;; Navigation

(use-package dired
  ;;:bind (("<^>" . (lambda () (find-alternate-file "..")))) ;;TODO
  :demand t
  :config
  (setq dired-dwim-target t)
  (put 'dired-find-alternate-file 'disabled nil)

  (set-face-attribute 'dired-marked nil :foreground "#5fff00")
  (if window-system
      (set-face-attribute 'dired-directory nil :foreground "#5fd7ff")
    (set-face-attribute 'dired-directory nil :foreground "#0020ff"))

  (defun kill-dired-buffers ()
    (interactive)
    (mapc (lambda (buffer)
	    (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
	      (kill-buffer buffer)))
	  (buffer-list))))

(use-package linum
  :init
  (if window-system
      (setq linum-format "%3d")
    (setq linum-format "%3d "))
  (global-set-key (kbd "<f11>") 'linum-mode))

(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :init
  (add-hook 'lisp-mode-hook 'highlight-symbol-mode)
  (add-hook 'emacs-lisp-mode-hook 'highlight-symbol-mode)
  (add-hook 'scheme-mode-hook 'highlight-symbol-mode)
  (add-hook 'cider-repl-mode-hook 'highlight-symbol-mode)
  (add-hook 'clojure-mode-hook 'highlight-symbol-mode)
  (global-set-key (kbd "C-,") 'highlight-symbol-prev)
  (global-set-key (kbd "<f1> ,") 'highlight-symbol-prev)
  (global-set-key (kbd "C-.") 'highlight-symbol-next)
  (global-set-key (kbd "<f1> .") 'highlight-symbol-next)
  (defun highlight-symbol-count (&optional symbol)
    "(Do not) Print the number of occurrences of symbol at point."
    (interactive))
  :config
  (setq highlight-symbol-idle-delay 1)
  (setq highlight-symbol-on-navigation-p 't)
  (custom-set-faces
   '(highlight-symbol-face ((t (:foreground "gray100" :background "#9c7618" :weight semi-bold))))))

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
  :diminish undo-tree-mode
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

(use-package expand-region
  :ensure t
  :pin melpa-stable
  :bind ("M-=" . er/expand-region))

;; bookmarks
(use-package bm
  :ensure t
  :bind (("C-b" . bm-toggle)
         ("<s-up>" . bm-previous)
         ("<s-down>" . bm-next))
  :init
  (custom-set-faces
   '(bm-face ((t (:background "#007994"))))))

(use-package tiling
  :init
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

(use-package open-github-from-here
  :bind (("M-g M-h" . open-github-from-here))
  :defer t
  :load-path "lisp/emacs-open-github-from-here")

(use-package sql
  :init
  (setq sql-connection-alist
        '((osio (sql-product 'postgres)
                (sql-server "localhost")
                (sql-user "osio_admin")
                (sql-database "opensensors"))
          (bsq-local (sql-product 'postgres)
                     (sql-server "localhost")
                     (sql-port 5430)
                     (sql-user "vittle")
                     (sql-database "bsq")
                     (sql-password "CL3ar--4n4lus1s@#"))
          (bsq (sql-product 'postgres)
               (sql-port 5432)
               (sql-user "vittle")
               (sql-database "bsq"))
          (bsq-personal (sql-product 'postgres)
                        (sql-port 5432)
                        (sql-user "stathis")
                        (sql-database "bsq"))))
  )

(use-package hydra
  :ensure t
  :pin melpa-stable
  :init
  (global-set-key (kbd "§") 'hydra-windows/body)

  (make-face 'move-window-buffer-face)
  (setq ss/window-move-remap-cookie nil)

  (defun move-splitter-left (arg)
    "Move window splitter left."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (shrink-window-horizontally arg)
      (enlarge-window-horizontally arg)))

  (defun move-splitter-right (arg)
    "Move window splitter right."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (enlarge-window-horizontally arg)
      (shrink-window-horizontally arg)))


  (defun move-splitter-up (arg)
    "Move window splitter up."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (enlarge-window arg)
      (shrink-window arg)))
  (defun move-splitter-down (arg)
    "Move window splitter down."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (shrink-window arg)
      (enlarge-window arg)))
  (set-face-attribute 'move-window-buffer-face nil
                      :background "#073642")

  (defun remove-window-move-indicator ()
    (if ss/window-move-remap-cookie
        (face-remap-remove-relative
         ss/window-move-remap-cookie)))

  (defun add-window-move-indicator ()
    (setq
     ss/window-move-remap-cookie
     (face-remap-add-relative 'default 'move-window-buffer-face)))

  (defun window-move (direction)
    (let ((fun (cond ((eq direction 'up) 'windmove-up)
                     ((eq direction 'down) 'windmove-down)
                     ((eq direction 'left) 'windmove-left)
                     ((eq direction 'right) 'windmove-right))))
      (remove-window-move-indicator)
      (funcall fun)
      (add-window-move-indicator)))

  (defun buffer-swap (direction)
    (let* ((other-window (windmove-find-other-window direction))
           (other-buffer (window-buffer other-window))
           (this-buffer (current-buffer))
           (this-window (selected-window)))
      (set-window-buffer other-window this-buffer)
      (set-window-buffer this-window other-buffer)
      (window-move direction)))

  (defhydra hydra-windows (global-map "C-M-s"
                                      :foreign-keys warn
                                      :pre  add-window-move-indicator
                                      :post remove-window-move-indicator)
    "windows"
    ("<up>" (progn (window-move 'up)))
    ("<down>" (progn (window-move 'down)))
    ("<left>" (progn (window-move 'left)))
    ("<right>" (progn (window-move 'right)))

    ("C-<up>" (progn (buffer-swap 'up)))
    ("C-<down>" (progn (buffer-swap 'down)))
    ("C-<left>" (progn (buffer-swap 'left)))
    ("C-<right>" (progn (buffer-swap 'right)))

    ("w" move-splitter-up)
    ("s" move-splitter-down)
    ("a" move-splitter-left)
    ("d" move-splitter-right)

    ("1" delete-other-windows "max")
    ("2" split-window-below "split below")
    ("-" split-window-below "split below")
    ("3" split-window-right "split right")
    ("|" split-window-right "split right")
    ("+" balance-windows "balance")

    ("C--" (progn (text-scale-increase -0.5)))
    ("C-=" (progn (text-scale-increase 0.5)))

    ("," beginning-of-buffer "home")
    ("." end-of-buffer "end")

    ("f" ido-find-file)
    ("b" (progn (remove-window-move-indicator)
                (ido-switch-buffer)
                (add-window-move-indicator)) "switch")
    ("k" (progn (remove-window-move-indicator)
                (kill-this-buffer)
                (add-window-move-indicator)) "kill")
    ("0" (progn (remove-window-move-indicator)
                (delete-window)
                (add-window-move-indicator)) "del")

    ("§" nil "exit")
    ("q" nil "exit")))

;; ========================================
;; Colors and looks

(use-package doom-themes
  :ensure t
  :config
  (require 'doom-themes)
  (load-theme 'doom-one t)
  (if window-system
    (progn
      (load-theme 'doom-one t)
      ;; (load-theme 'solarized-dark t)
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      ;;(fringe-mode '(8 . 8))
      ;; ;;(set-default 'cursor-type 'bar)
      ;; (set-cursor-color "#e3e2d6")
      ;;(set-face-foreground 'vertical-border "black")
      ;;(set-face-background 'vertical-border "black")
      ;;(setq x-underline-at-descent-line t)
      )
    (load-theme 'zenburn t))
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(defun justified-mode-line (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT
 aligned respectively."
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

(defun remove-vowels (string)
  (replace-regexp-in-string "a\\|e\\|i\\|o\\|u\\|" "" string))

(use-package all-the-icons
  :demand
  :init
  (progn (defun -custom-modeline-github-vc ()
           (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
             (concat
              (propertize (format "%s" (all-the-icons-octicon "git-branch"))
                          'face `(:height 1 :family ,(all-the-icons-octicon-family))
                          'display '(raise 0))
              (propertize (format " %s" branch)))))

         (defvar mode-line-my-vc
           '(:propertize
             (:eval (when vc-mode
                      (cond
                       ((string-match "Git[:-]" vc-mode) (-custom-modeline-github-vc))
                       (t (format "%s" vc-mode)))))
             face mode-line-directory)
           "Formats the current directory."))
  :config
  (progn (setq-default mode-line-format
                       '((:eval
                          (justified-mode-line
                           (format-mode-line
                            (list
                             ""
                             mode-line-mule-info
                             mode-line-modified
                             mode-line-frame-identification
                             mode-line-buffer-identification
                             " (%I) "
                             "prj:"
                             '(:eval (remove-vowels (projectile-project-name)))))
                           (format-mode-line
                            (list
                             mode-line-modes
                             ;;'(:eval (symbol-name major-mode))
                             "  "
                             mode-line-my-vc
                             "   "
                             "☰ %l ‖ %c "))))))))

(set-face-attribute 'mode-line nil
                    :box '(:line-width 5 :color "#1c1e24")
                    :overline nil
                    :underline nil)

(set-face-attribute 'mode-line-inactive nil
                    :box '(:line-width 5 :color "#1d2026")
                    :overline nil
                    :underline nil)

;; (use-package zenburn-theme
;;   :ensure t)

;; (use-package solarized-theme
;;   :ensure t)

;; (use-package powerline
;;   :ensure t
;;   :init
;;   (powerline-default-theme)
;;   (set-face-attribute 'mode-line nil
;;                       :foreground "grey"
;;                       :background "#34503e"
;;                       :box nil
;;                       :overline nil
;;                       :underline nil
;;                       :height 1)
;;   (set-face-attribute 'mode-line-inactive nil
;;                       :overline nil
;;                       :underline nil
;;                       :foreground "grey50"
;;                       :background "#282828"
;;                       :height 1)
;;   :config
;;   (set-face-attribute 'mode-line-buffer-id nil
;;                       :foreground "grey")
;;   (set-face-attribute 'mode-line-buffer-id-inactive nil
;;                       :foreground "grey50")
;;   (set-face-attribute 'powerline-active1 nil :background "#1a1a1a" :foreground "#667b7c")
;;   (set-face-attribute 'powerline-active2 nil :background "#0a3641" :foreground "#647b7c")
;;   (set-face-attribute 'powerline-inactive1 nil :background "#0f0f0f" :foreground "#494949")
;;   (set-face-attribute 'powerline-inactive2 nil :background "#161616" :foreground "#444444")
;;   (setq powerline-default-separator 'utf-8))

(use-package hl-line-mode
  :init
  (global-hl-line-mode)
  (set-face-attribute 'hl-line nil
                      :background "#032830"))

;; ========================================
;; Misc

(require 'simple-copy)

;;global-custom-keys
(global-set-key (kbd "C-=") (lambda () (interactive) (text-scale-increase 0.5)))
(global-set-key (kbd "C--") (lambda () (interactive) (text-scale-increase -0.5)))
(global-set-key (kbd "C-0") (lambda () (interactive) (text-scale-increase 0)))

(global-set-key (kbd "<f1> SPC") 'mark-sexp)

(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
(global-set-key [f7] 'toggle-truncate-lines)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(define-key lisp-interaction-mode-map (kbd "C-x M-e") 'eval-print-last-sexp)

(global-unset-key (kbd "C-x C-d"))
(global-unset-key (kbd "<f1> <f1>"))

(defun yank-without-moving ()
  (interactive)
  (let ((pos (point)))
    (yank)
    (set-window-point nil pos)))

(global-set-key (kbd "s-y") 'yank-without-moving)

;; special chars

(defun euro ()
  (interactive)
  (insert "€"))

(defun pound ()
  (interactive)
  (insert "£"))

;; camelcase

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

(defun camel->kebab ()
  (interactive)
  (un-camelcase-region))

;;

(defun refresh-file ()
  (interactive)
  (revert-buffer t t t))
(global-set-key [f5] `refresh-file)
(global-set-key [f6] `mark-whole-buffer)

(defun date (arg)
  (interactive "P")
  (insert (if arg
              (format-time-string "%d.%m.%Y")
            (format-time-string "%Y-%m-%d"))))

(defun new-scratch ()
  "open up a guaranteed new scratch buffer"
  (interactive)
  (switch-to-buffer (loop for num from 0
                          for name = (format "new-%03i" num)
                          while (get-buffer name)
                          finally return name)))

;;shell stuff
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;super-slow-scroll
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ;; one two lines at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)

;;misc-custom-vars

;;global-subword-mode is nice but it adds an annoying comma to modeline
;;(global-subword-mode 1)

(setq frame-resize-pixelwise t)
(setq inhibit-splash-screen t)
(setq comment-empty-lines t)
(setq visible-bell t)
(setq make-backup-files nil) ;; no backups!
(setq auto-save-default nil) ;; stop creating those #autosave# files
(setq custom-file "~/.emacs.d/custom.el")
(setq temporary-file-directory "/tmp") ;; necessary for tramp+babel
;;(load custom-file 'noerror)
(column-number-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(put 'narrow-to-region 'disabled nil)

;;spaces-instead-of-tabs
(setq-default indent-tabs-mode nil)
(setq default-tab-width 2)
(setq tab-width 2)
(setq python-indent 3)
(setq c-basic-offset 3)
(setq c-indent-level 3)
(setq c++-tab-always-indent nil)
(setq js-indent-level 2)
(setq lua-indent-level 2)
(setq css-indent-offset 2)

;;greek support
(setq default-input-method "greek")
(global-set-key (kbd "s-\\") 'toggle-input-method)

;;"Edit with Emacs" chrome plugin
(require 'edit-server)
(setq edit-server-new-frame nil)
(edit-server-start)
(require 'edit-server-htmlize)
(autoload 'edit-server-maybe-dehtmlize-buffer "edit-server-htmlize" "edit-server-htmlize" t)
(autoload 'edit-server-maybe-htmlize-buffer   "edit-server-htmlize" "edit-server-htmlize" t)
(add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
(add-hook 'edit-server-done-hook  'edit-server-maybe-htmlize-buffer)

;; ========================================
;; Machine-specific config

(if (string-equal system-type "darwin")
    (require 'octavia))
(if (string-equal system-type "darwin")
    (require 'octavia))
(if (string-equal system-name "MUCHA")
    (require 'mucha))
(if (not window-system)
    (require 'no-window))

(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))

;;custom-scratch-message
(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))
(setq initial-scratch-message (get-string-from-file "~/.emacs.d/logo"))


(load (expand-file-name "~/quicklisp/slime-helper.el"))

(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))
