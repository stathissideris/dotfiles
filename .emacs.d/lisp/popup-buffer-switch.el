;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; switch buffers using popup2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'popup2)

(defun acceptable-buffer-pred (x)
  (or
   (string-match "repl\\|REPL" x)
   (string-match "inferior-lisp" x)
   (string-match "scratch" x)
   (string-match "slime" x)
   (string-match "^\\*Messages" x)
   (string-match "^\\*GXS" x)
   (string-match "^\\*Customize" x)
   (and (not (string-match "^\\*" x))
        (not (string-match "^ \\*" x)))))

(defun acceptable-buffers ()
  (remove-if-not
   'acceptable-buffer-pred
   (sort* (mapcar (function buffer-name) (buffer-list))
          'string-lessp :key 'downcase)))

(setq popup-buffer-switch-previous-buffer nil)
(defun popup-buffer-switch ()
  (interactive)
  (let ((old-buffer popup-buffer-switch-previous-buffer)
        (buffers (acceptable-buffers)))
    (setq popup-buffer-switch-previous-buffer
          (buffer-name (current-buffer)))
    (switch-to-buffer (popup-menu2 buffers
                                   old-buffer
                                   :scroll-bar t
                                   :isearch t
                                   :margin-left 1))))

(provide 'popup-buffer-switch)
