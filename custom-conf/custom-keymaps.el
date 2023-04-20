(define-prefix-command 'ak-map)
(global-set-key (kbd "`") 'ak-map)
(global-set-key (kbd "` `") 'self-insert-command)



;; Switch to scratch buffer
(define-key ak-map "z" (lambda ()
                         "Switch to scratch"
                         (interactive)
                         (switch-to-buffer "*scratch*")))

;; Switch to scratch buffer
(define-key ak-map "Z" (lambda ()
                         "Create new scratch buffer to scratch"
                         (interactive)
                         (switch-to-buffer "*scratch*")))

(provide 'custom-keymaps)
