;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Adds some fancy functionality to paredit
;;
;; proposed key bindings:
;;
;; (global-set-key (kbd "s-/") 'cycle-symbol-at-point)
;; (global-set-key (kbd "s-\\") 'cycle-symbol-at-point)
;; (global-set-key (kbd "C-e") 'duplicate-sexp)
;; (global-set-key [C-M-down] 'transpose-sexp-forward)
;; (global-set-key [C-M-up] 'transpose-sexp-backward)
;;

(setq symbol-cycle-replacements
      '(("+" . "-")
        ("-" . "+")
        ("*" . "/")
        ("/" . "*")
        ("true" . "false")
        ("false" . "true")
        ("TRUE" . "FALSE")
        ("FALSE" . "TRUE")
        (">" . "<")
        ("<" . ">")
        (">=" . "<=")
        ("<=" . ">=")
        ("=" . "not=")
        ("not=" . "=")
        ("remove" . "filter")
        ("filter" . "remove")
        ("even?" . "odd?")
        ("odd?" . "even?")
        ("and" . "or")
        ("or" . "and")
        ("inc" . "dec")
        ("dec" . "inc")
        ("when" . "when-not")
        ("when-not" . "when")
        ("if" . "if-not")
        ("if-not" . "if")
        ("empty?" . "not-empty")
        ("not-empty" . "empty?")
        ("every?" . "not-every?")
        ("not-every?" . "every?")
        ("any?" . "not-any?")
        ("not-any?" . "any?")
        ("true?" . "false?")
        ("false?" . "true?")
        ("neg?" . "zero?")
        ("zero?" . "pos?")
        ("pos?" . "neg?")))

(defun cycle-symbol-at-point ()
  "Replaces the symbol at point with the symbol in the
  symbol-cycle-replacements alist. Useful for quickly switching
  between true and false, + and -, < and > etc."
  (interactive)
  (save-excursion
    (let ((s (format "%s" (symbol-at-point)))
          (bounds (bounds-of-thing-at-point 'symbol)))
      (let ((replacement (cdr (assoc s symbol-cycle-replacements))))
        (when replacement
          (delete-region (car bounds) (cdr bounds))
          (insert replacement))))))

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

(provide 'paredit-steroids)
