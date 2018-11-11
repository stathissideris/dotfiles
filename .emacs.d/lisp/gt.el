(defun gt/replacement-text (original replacement)
  (let* ((olen (length original))
         (rlen (length replacement))
         (slen (max 0 (- olen rlen))))
    (concat replacement (make-string slen ?\s))))

(defun gt/translate-key ()
  (interactive)
  (save-excursion
    (let* ((original (format "%s" (symbol-at-point)))
           (bounds (bounds-of-thing-at-point 'symbol))
           (exp (concat "(gt.ingest.translations/translate-key " original ")")))
      (let* ((res (read (nrepl-dict-get (cider-nrepl-sync-request:eval exp) "value")))
             (selected (if (= 1 (length res))
                           (read-string "Translation: " (car res))
                         (read-string "Translation: "
                                      (ido-completing-read "Possible translations: " res nil nil ""))))
             (replacement (concat "::" selected)))
        (delete-region (car bounds) (cdr bounds))
        (insert (gt/replacement-text original replacement))
        (list replacement original)))))

(defun gt/preserve-translate-key ()
  (interactive)
  (save-excursion
    (let ((res (gt/translate-key)))
      (search-forward ":translations")
      (search-forward "}")
      (left-char)
      (insert "\n")
      (indent-for-tab-command)
      (insert (car res))
      (insert " ")
      (insert (nth 1 res))
      (align-cljlet))))

(global-set-key (kbd "<f12>") 'gt/preserve-translate-key)

(provide 'gt)
