(require 'inf-lisp)
(require 'clj-refactor)
;(require 'ac-nrepl)

(cljr-add-keybindings-with-prefix "C-c C-v")

(setq clojure-quick-sexp
	  '("(dev/reset)"
        "(use 'clojure.repl)"
        "(use 'clojure.tools.trace)"
        "(use 'clojure.pprint)"))

(defun clojure-quick-eval ()
  (interactive)
  (let ((selection (ido-completing-read
					"Clojure eval: " clojure-quick-sexp nil t "")))
	(cider-interactive-eval selection)))

(defun save-load-and-compile ()
  ""
  (interactive)
  (save-buffer)
  (cider-load-current-buffer))

(defun cloj-help-popup (msg)
  (if msg
   (let ((lines (cdr (split-string msg "\n"))))
	 (setq msg (mapconcat (lambda (x) x) lines "\n"))
	 (th-show-tooltip-for-point msg))
   (th-show-tooltip-for-point "No help found.")))

(global-set-key [f2] 'clojure-quick-eval)

(global-set-key (kbd "C-,") 'highlight-symbol-prev)
(global-set-key (kbd "C-.") 'highlight-symbol-next)

(defun jack-in () (interactive) (clojure-jack-in))
(defun repl () (interactive) (cider "127.0.0.1" 4005))

(defun load-clojure-file (filename)
  "Loads a clojure file in the new REPL. Assumes that the files are in the .elisp/ dir of home."
  (with-temp-message (concat "Loading Clojure file " filename)
    (cider-interactive-eval
     (concat "(try (load-file \"" (expand-file-name "~") "/.emacs.d/clojure/" filename "\") (catch Exception e (.getMessage e)))"))))

(defun clojuredocs-example ()
  (interactive)
  (cider-interactive-eval
   (concat "(emacs/clojuredocs \"" (cider-symbol-at-point) "\")")))

(global-set-key [M-f1] 'clojuredocs-example)
(global-set-key [M-f3] 'nrepl-pretty-toggle)
(global-set-key
 (kbd "<H-home>")
 (lambda ()
   (interactive)
   (switch-to-buffer "*GXS Scripts*")))

(setq auto-mode-alist (cons '("\\.edn$" . clojure-mode) auto-mode-alist))

;;hide dos new lines in cider help
(defadvice cider-popup-buffer-display
  (after cider-popup-hide-dos-eol (popup-buffer &optional select))
  (with-current-buffer popup-buffer
    (hide-dos-eol)))
(ad-activate 'cider-popup-buffer-display)

;;mode hooks

(defun cider-repl-setup ()
  "Some REPL setup."
  (interactive)
  (load-clojure-file "clojure-dev.clj")
  ;;(ac-nrepl-setup)
  ;;(auto-complete-mode)
  (hide-dos-eol))
(add-hook 'cider-repl-mode-hook 'cider-repl-setup)

(defun clojure-hook ()
  (paredit-mode 1)
  (highlight-symbol-mode 1)
  (auto-complete-mode)
  (clj-refactor-mode 1)
  ;(pretty-mode 1)
  )
(add-hook 'clojure-mode-hook 'clojure-hook)
(define-key clojure-mode-map (kbd "C-c C-a") 'align-cljlet)
(define-key clojure-mode-map (kbd "C-x t") 'clojure-jump-to-test)

;;cider
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq cider-auto-select-error-buffer t)
(setq cider-repl-wrap-history t)
(setq cider-repl-history-size 1000)
(setq cider-show-error-buffer 'except-in-repl)
(setq cider-repl-history-file "~/.emacs.d/cider-history")
(define-key cider-mode-map (kbd "C-c p") 'cider-repl-toggle-pretty-printing)
(define-key cider-repl-mode-map (kbd "C-c p") 'cider-repl-toggle-pretty-printing)

(defun string-join (sep s)
  (mapconcat 'identity s sep))

(defun toggle-test-path (path)
  "Calculate the filename of the corresponding test from the
  current buffer, or the src filename from a test filename. So, toggle between

  /devel/bare-square/monitor/src/monitor/extractor/site_catalyst/api.clj

  and

  /devel/bare-square/monitor/test/monitor/extractor/site_catalyst/api_test.clj

  Contains a bug for the (rare) cases where there is a \"test\"
  or \"src\" in your namespace hierarchy."
  (string-join
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

;;;-----

(defun chomp-end (str)
  "Chomp tailing whitespace from STR."
  (replace-regexp-in-string (rx (* (any " \t\n")) eos)
                            ""
                            str))

;;monkey patch!
(defun cider-eval-last-sexp-and-replace ()
  "Evaluate the expression preceding point and replace it with its result."
  (interactive)
  (let* ((last-sexp (concat "(with-out-str (clojure.pprint/pprint " (cider-last-sexp) "))"))
         (value     (chomp-end (cider-eval-and-get-value last-sexp))))
    (save-excursion
      (backward-kill-sexp)
      (insert value)
      (paredit-backward)
      (mark-sexp)
      (indent-for-tab-command))))

(provide 'clojure-dev)
