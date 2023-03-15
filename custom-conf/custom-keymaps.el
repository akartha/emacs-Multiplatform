(define-prefix-command 'ak-map)
(global-set-key (kbd "`") 'ak-map)
(global-set-key (kbd "` `") 'self-insert-command)

(define-prefix-command 'avy-custom-keymap)
(global-set-key (kbd "` y") 'avy-custom-keymap)


;; Switch to scratch buffer
(define-key ak-map "z" (lambda ()
                         "Switch to scratch"
                         (interactive)
                         (switch-to-buffer "*scratch*")))

(provide 'custom-keymaps)
