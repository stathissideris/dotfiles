(require 'inf-lisp)
(require 'clj-refactor)
(require 'align-cljlet)
(require 'cider-macroexpansion)

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

(setq auto-mode-alist (cons '("\\.edn$" . clojure-mode) auto-mode-alist))

;;hide dos new lines in cider help
(defadvice cider-popup-buffer-display
  (after cider-popup-hide-dos-eol (popup-buffer &optional select))
  (with-current-buffer popup-buffer
    (hide-dos-eol)))
(ad-activate 'cider-popup-buffer-display)

(defun clojure-hook ()
  (paredit-mode 1)
  (highlight-symbol-mode 1)

  (clj-refactor-mode 1)
  (yas-minor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-v")
  )

(add-hook 'clojure-mode-hook 'clojure-hook)
(define-key clojure-mode-map (kbd "C-c C-a") 'align-cljlet)
(define-key clojure-mode-map (kbd "C-x t") 'clojure-jump-to-test)
(define-key cider-repl-mode-map (kbd "C-c M-o") 'cider-repl-clear-buffer)
(define-key cider-repl-mode-map (kbd "C-c o") 'cider-repl-switch-to-other)
(define-key cider-repl-mode-map (kbd "{") 'paredit-open-curly)

(defun cider-repl-hook ()
  "Some REPL setup."
  (interactive)
  (paredit-mode)
  (set-face-attribute 'cider-repl-prompt-face nil :weight 'bold)
  ;;(load-clojure-file "clojure-dev.clj")
  ;;(hide-dos-eol)
  )
(add-hook 'cider-repl-mode-hook 'cider-repl-hook)

(defun cider-hook ()
  (cider-turn-on-eldoc-mode))
(add-hook 'cider-mode-hook 'cider-hook)
;;(add-hook 'after-init-hook 'global-company-mode) ;;auto-completion
(define-key cider-mode-map (kbd "C-c p") 'cider-repl-toggle-pretty-printing)
(define-key cider-repl-mode-map (kbd "C-c p") 'cider-repl-toggle-pretty-printing)

;;(setq cider-show-error-buffer 'except-in-repl)
;;(setq cider-auto-select-error-buffer t)
(setq cider-repl-wrap-history t)
(setq cider-repl-history-size 1000)
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;;jump to tests and back

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

;;eval and replace with pretty-printed code
(defun chomp-end (str)
  "Chomp tailing whitespace from STR."
  (replace-regexp-in-string (rx (* (any "\t\n")) eos)
                            ""
                            str))

(defun nrepl-eval (input)
 (nrepl-dict-get
  (cider-nrepl-sync-request:eval input)
  "value"))

;;monkey patch!
(defun cider-eval-last-sexp-and-replace ()
  "Evaluate the expression preceding point and replace it with its result."
  (interactive)
  (let ((last-sexp (concat "(with-out-str (clojure.pprint/pprint " (cider-last-sexp) "))")))
    ;; we have to be sure the evaluation won't result in an error
    (cider-nrepl-sync-request:eval last-sexp)
    ;; seems like the sexp is valid, so we can safely kill it
    (backward-kill-sexp)
    (insert (read (nrepl-dict-get (cider-nrepl-sync-request:eval last-sexp) "value")))
    (delete-backward-char 1)))

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

;; (fset 'apply-fix-macro
;;    (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217768 102 105 120 32 5 134217848 109 97 99 114 111 101 120 112 97 110 100 45 114 101 112 108 97 99 101 return] 0 "%d")) arg)))

(defun apply-fix-macro ()
  (interactive)
  (paredit-wrap-round)
  (insert "fix ")
  (forward-sexp)
  (forward-char 1)
  (macroexpand-replace)
  (backward-sexp))

(define-key clojure-mode-map (kbd "<f12>") 'apply-fix-macro)

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
    (indent-region (region-beginning) (region-end))))



(provide 'clojure-dev)
