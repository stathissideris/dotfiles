(global-unset-key (kbd "§"))

(global-set-key (kbd "§ <left>")  'windmove-left)
(global-set-key (kbd "§ <right>") 'windmove-right)
(global-set-key (kbd "§ <up>")    'windmove-up)
(global-set-key (kbd "§ <down>")  'windmove-down)

(global-set-key (kbd "§ §")  'mark-sexp)

(global-unset-key (kbd "≥"))
(global-unset-key (kbd "≤"))
(global-set-key (kbd "≥") 'highlight-symbol-next)
(global-set-key (kbd "≤") 'highlight-symbol-prev)

(menu-bar-mode -1)

(provide 'no-window)
