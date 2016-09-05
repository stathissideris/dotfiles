(require 'erc)
(require 'erc-lang)
(require 'erc-dcc)
(require 'erc-ring)
(require 'erc-log)
(require 'erc-join)
(require 'erc-goodies)
(require 'erc-stamp)
(require 'erc-pcomplete)
(require 'erc-notify)
(require 'erc-track)
(require 'erc-autoaway)
(require 'erc-match)
(require 'erc-button)

(setq erc-beep-p nil
      erc-server-coding-system (cons 'utf-8-unix 'undecided)
      erc-save-buffer-on-part t
      erc-save-queries-on-quit t
      erc-save-buffer-on-quit t
      erc-log-write-after-send t
      erc-log-channels-directory "~/irclogs"
      erc-log-file-coding-system 'utf-8-unix
      erc-log-write-after-insert nil
      erc-server "irc.freenode.net"
      erc-auto-query t
      erc-user-full-name "iM iMopoulos"
      erc-nick "[iM"
      erc-email-userid "x"
      erc-user-mode "+i"
      erc-paranoid t
      erc-disable-ctcp-replies t
      erc-kill-queries-on-quit t
      erc-kill-buffer-on-part t
      erc-kill-server-buffer-on-quit t
      erc-minibuffer-notice nil)

(setq-default erc-enable-logging t)

(setq erc-generate-log-file-name-function
      (lambda (buffer target nick server port)
        target))

;; The `erc-hide-list' contents may be more useful if they are buffer
;; local, so that setting the hide list of one ERC channel window
;; doesn't affect all the active channels at the same time.
(make-variable-buffer-local 'erc-hide-list)

;; Override the default quit reason of ERC
(defun erc-quit-reason-normal (&optional s)
  "Normal quit message.

If S is non-nil, it will be used as the quit reason."
  (or s "Leaving"))

(defun erc-part-reason-normal (&optional s)
  "Normal part message.

If S is non-nil, it will be used as the quit reason."
  (or s ""))

(defun ss/erc-mode-hook ()
  (setq erc-timestamp-format "%H:%M "
        erc-fill-function 'erc-fill-static
        erc-fill-static-center 15
        erc-fill-column 100
        erc-track-shorten-function nil
        erc-insert-timestamp-function 'erc-insert-timestamp-left
        erc-insert-away-timestamp-function 'erc-insert-timestamp-left
        erc-timestamp-only-if-changed-flag nil
        erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477")
        erc-track-exclude-server-buffer t
        erc-keywords '()
        erc-button-buttonize-nicks nil
        erc-server-auto-reconnect t
        erc-autojoin-channels-alist '(("freenode.net" "#_penguins"))
        erc-prompt ">>>>>>>>>>>>>>>>>>>>"
        erc-prompt "                   ▶"
        erc-prompt "▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶"
        ;;erc-prompt "╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱"
        ;;erc-prompt " ▖ ▗ ▘ ▙ ▚ ▛ ▜ ▝ ▞ ▟"
        ;;erc-prompt "                   ➭"
        ;;erc-prompt "                ➤"
        )

  (define-key erc-mode-map (kbd "<C-up>") 'erc-previous-command)
  (define-key erc-mode-map (kbd "<C-down>") 'erc-next-command)
  ;;(define-key erc-mode-map (kbd "ESC 2") (lambda () (interactive) (switch-to-buffer "#_penguins")))

  (erc-log-enable)
  (erc-dcc-disable)
  (erc-ring-mode 1)
  (erc-completion-mode 1)
  (erc-autojoin-disable)
  (erc-notify-enable)
  (setq-default erc-notify-list nil)
  (erc-track-enable)
  (erc-autoaway-disable)
  (visual-line-mode 1))

;;shorten long links
(setq ss/erc-shorten-links-limit 50)
(setq ss/url-regexp
      (concat "\\(www\\.\\|\\(s?https?\\|"
              "ftp\\|file\\|gopher\\|news\\|telnet\\|wais\\|mailto\\):\\)"
              "\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?"
              "[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;.,()]+[-a-zA-Z0-9_=#$@~`%&*+\\/()]"))
(defun ss/erc-shorten-links ()
  (goto-char (point-min))
  (while (re-search-forward ss/url-regexp nil t)
    (let ((start (match-beginning 0))
          (end (match-end 0)))
      (when (> (- end start) ss/erc-shorten-links-limit)
        (let* ((url (buffer-substring start end))
               (replacement (concat (substring url 0 ss/erc-shorten-links-limit) "...")))
          ;;(message "replacing with URL: %s" replacement)
          (put-text-property start end 'display replacement)
          (put-text-property start end 'help-echo url))))))
(add-hook 'erc-insert-modify-hook 'ss/erc-shorten-links)
(add-hook 'erc-send-modify-hook 'ss/erc-shorten-links)

;;prevent long URL to be separated from the nickname
(defun ss/erc-fix-fill (fill-fun)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "> " (point-max) t)
      (replace-match ">➭")))
  (funcall fill-fun)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward ">➭" (point-max) t)
      (replace-match "> "))))

(advice-add 'erc-fill-regarding-timestamp :around #'ss/erc-fix-fill)

(custom-set-faces
 '(erc-input-face ((t (:foreground "grey40"))))
 '(erc-my-nick-face ((t (:foreground "grey40" :weight bold))))
 '(erc-nick-default-face ((t (:foreground "#839496" :weight bold))))
 '(erc-prompt-face ((t (:foreground "gray40" :weight bold))))
 '(erc-timestamp-face ((t (:foreground "gray30"))))
 '(erc-notice-face ((t (:foreground "#2b5a8d")))))

(add-hook 'erc-mode-hook 'ss/erc-mode-hook)

(provide 'ss-erc)
