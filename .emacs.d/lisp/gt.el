(defun gt/translate-key ()
  (interactive)
  (save-excursion
    (let* ((s (format "%s" (symbol-at-point)))
           (bounds (bounds-of-thing-at-point 'symbol))
           (exp (concat "(gt.ingest.translations/translate-key " s ")")))
      (let ((res (read (nrepl-dict-get (cider-nrepl-sync-request:eval exp) "value"))))
        (if (= 1 (length res))
            (progn
              (delete-region (car bounds) (cdr bounds))
              (insert (concat "::" (car res))))
          (let ((selected (ido-completing-read "Possible translations: " res nil t "")))
            (delete-region (car bounds) (cdr bounds))
            (insert (concat "::" selected))))))))
