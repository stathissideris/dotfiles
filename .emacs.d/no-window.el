(set-face-attribute 'mode-line-inactive nil :background "#303030")

(menu-bar-mode -1)

(global-unset-key (kbd "<f1>"))

(global-set-key (kbd "<f1> <left>")  'windmove-left)
(global-set-key (kbd "<f1> <right>") 'windmove-right)
(global-set-key (kbd "<f1> <up>")    'windmove-up)
(global-set-key (kbd "<f1> <down>")  'windmove-down)

(global-set-key (kbd "<f1> <f1>")  'mark-sexp)

(global-set-key (kbd "<f1> .") 'highlight-symbol-next)
(global-set-key (kbd "<f1> ,") 'highlight-symbol-prev)


(provide 'no-window)
