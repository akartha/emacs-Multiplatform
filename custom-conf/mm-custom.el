;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Mark-Multiple             

;; This extension allows you to quickly mark the next occurence of a
;; region and edit them all at once. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package mark-multiple
  :straight t
  :bind (:map ak-map
              ((">" . mark-next-like-this)
               ("<" . mark-previous-like-this)
               ("+" . mark-more-like-this-extended)
               ("=" . mark-all-like-this))))
;; ("C-c m" . mark-more-like-this-extended)
;; ("C-c a" . mark-all-like-this))

(provide 'mm-custom)
