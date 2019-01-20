;;; popup-menu* that allows selection

(defun* popup-menu2 (list
                     selection
                     &key
                     point
                     (around t)
                     (width (popup-preferred-width list))
                     (height 15)
                     margin
                     margin-left
                     margin-right
                     scroll-bar
                     symbol
                     parent
                     parent-offset
                     (keymap popup-menu-keymap)
                     (fallback 'popup-menu-fallback)
                     help-delay
                     prompt
                     isearch
                     (isearch-cursor-color popup-isearch-cursor-color)
                     (isearch-keymap popup-isearch-keymap)
                     isearch-callback
                     &aux menu event)
  (and (eq margin t) (setq margin 1))
  (or margin-left (setq margin-left margin))
  (or margin-right (setq margin-right margin))
  (if (and scroll-bar
           (integerp margin-right)
           (> margin-right 0))
      ;; Make scroll-bar space as margin-right
      (decf margin-right))
  (setq menu (popup-create point width height
                           :around around
                           :face 'popup-menu-face
                           :selection-face 'popup-menu-selection-face
                           :margin-left margin-left
                           :margin-right margin-right
                           :scroll-bar scroll-bar
                           :symbol symbol
                           :parent parent))
  (unwind-protect
      (progn
        (popup-set-list menu list)
        (let ((v 0)
              (i (position selection list :test 'equal)))
          (if (> (or i 0) 0)
              (dotimes (number i v)
                (popup-next menu))
            (popup-draw menu)))
        (popup-menu-event-loop menu keymap fallback prompt help-delay isearch
                               isearch-cursor-color isearch-keymap isearch-callback))
    (popup-delete menu)))

;(popup-menu2 '("lalaki" "llollos" "kaka" "dfew" "fe" "2222") "fe")
;(popup-menu2 '(4 7 5 6 3) 100)
;(position "lala" '("d" 2 3 "lala" 2 32 32 "t") :test 'equal)

(provide 'popup2)
