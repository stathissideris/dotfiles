(require 'inf-lisp)
(require 'clj-refactor)
(require 'align-cljlet)
(require 'cider-macroexpansion)

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

;;hide dos new lines in cider help
(defadvice cider-popup-buffer-display
    (after cider-popup-hide-dos-eol (popup-buffer &optional select))
  (with-current-buffer popup-buffer
    (hide-dos-eol)))
(ad-activate 'cider-popup-buffer-display)

(defun cider-repl-hook ()
  "Some REPL setup."
  (interactive)
  (paredit-mode)
  (set-face-attribute 'cider-repl-prompt-face nil :weight 'bold)
  ;;(load-clojure-file "clojure-dev.clj")
  ;;(hide-dos-eol)
  )
(add-hook 'cider-repl-mode-hook 'cider-repl-hook)

;;(add-hook 'after-init-hook 'global-company-mode) ;;auto-completion
(define-key cider-mode-map (kbd "C-c p") 'cider-repl-toggle-pretty-printing)
(define-key cider-repl-mode-map (kbd "C-c p") 'cider-repl-toggle-pretty-printing)

;;(setq cider-show-error-buffer 'except-in-repl)
;;(setq cider-auto-select-error-buffer t)
(setq cider-repl-wrap-history t)
(setq cider-repl-history-size 1000)
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;;jump to tests and back

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

(defun graal ()
  (interactive)
  (setenv "JAVA_HOME" "/Users/sideris/devel/_tools/graalvm-ce-19.2.0/Contents/Home"))

(fset 'midje-to-test-assertion
   [?\C-s ?= ?> return backspace backspace backspace right C-M-up ?\M-\( C-right ?= ?  left left left ?\M-\( ?i ?s ?  left left left left f1 ?  ?\C-\M-\\])

(provide 'clojure-dev)
