;;;;;;;;;;;;;;;;;;;;;;
;; ** CRUX 
;;;;;;;;;;;;;;;;;;;;;

(define-prefix-command 'ak-crux-map)
(global-set-key (kbd "` x") 'ak-crux-map)

(use-package crux
  :straight t
  :bind
  ("C-k" . crux-smart-kill-line)
  (:map ak-crux-map
        ;;     ("U" . crux-view-url)
        ;;("a" . crux-ispell-word-then-abbrev)
        ("." . crux-find-shell-init-file)
        ("1" . crux-find-user-init-file)
        ("a" . crux-move-beginning-of-line)
        ("o" . crux-smart-open-line)
        ("O" . crux-smart-open-line-above)
        ("d" . crux-duplicate-current-line-or-region)
        ("j" . crux-top-join-line)
        ("k" . crux-kill-line-backwards)
        ("C" . crux-cleanup-buffer-or-region)
        ("r" . crux-recentf-find-file)
        ("D" . crux-recentf-find-directory)
        ("U" . crux-upcase-region)
        ("L" . crux-downcase-region)
        ("i" . crux-insert-date)
        ("c" . crux-capitalize-region)
        ("w" . crux-other-window-or-switch-buffer)
        ("s" . crux-sudo-edit)
        ("<f2>" . crux-rename-buffer-and-file)
        ("<delete>" . crux-delete-file-and-buffer)
        (";" . crux-duplicate-and-comment-current-line-or-region)
        ("<f3>" . crux-kill-buffer-truename)
        ("<tab>" . crux-indent-defun)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; crux ships with some handy advises that can enhance the operation
;; of existing commands.
;; *** (crux-with-region-or-buffer) ;; ;;
;; You can use crux-with-region-or-buffer to make a command acting
;; normally on a region to operate on the entire buffer in the absence
;; of a region. Here are a few examples you can stuff in your config:
;; ;;

;; #+begin_example                                                                                                                                                                                     
;; (crux-with-region-or-buffer indent-region)                                                                                                                                                          
;; (crux-with-region-or-buffer untabify)                                                                                                                                                               
;; #+end_example                                                                                                                                                                                       
;; *** (crux-with-region-or-line)                                                                                                                                                                      

;; Likewise, you can use crux-with-region-or-line to make a command
;; alternately act on the current line if the mark is not active:

;; #+begin_example                                                                                                                                                                                     
;; (crux-with-region-or-line comment-or-uncomment-region)                                                                                                                                              
;; #+end_example                                                                                                                                                                                       
;; *** (crux-with-region-or-point-to-eol)                                                                                                                                                              

;; Sometimes you might want to act on the point until the end of the
;; current line, rather than the whole line, in the absence of a
;; region: ;;

;;#+begin_example
;; (crux-with-region-or-point-to-eol
;; kill-ring-save)
;; #+end_example ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'crux-custom)
