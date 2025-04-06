;;; -*- lexical-binding: t; -*-

(define-prefix-command 'avy-custom-keymap)
(global-set-key (kbd "` y") 'avy-custom-keymap)

(use-package avy
  :ensure t
  :bind
  (:map avy-custom-keymap
        ("l" . avy-goto-line)
        ;;    ("L" . avy-move-line)
        ("m" . avy-move-region)
        ;;        ("p" . avy-goto-line-above)
        ;;      ("n" . avy-goto-line-below)
        ("c" . avy-goto-char-timer)
        ("w" . avy-goto-word-0)
        ("t" . avy-transpose-lines-in-region)
        ;;  ("k" . avy-kill-ring-save-whole-line)
        ;;  ("K" . avy-kill-whole-line)
        ("r" . avy-kill-ring-save-region)
        ("R" . avy-kill-region)
        ("s" . avy-goto-symbol-1)
        ("h" . avy-org-goto-heading-timer))
  (:map ak-map
        ("<tab>" . avy-goto-line)
        ("'" . avy-goto-char-timer))
  :config 

  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))
  
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  (defun avy-action-define (pt)
    (cl-letf (((symbol-function 'keyboard-quit)
            #'abort-recursive-edit))
      (save-excursion
        (goto-char pt)
        (dictionary-search (thing-at-point 'word)))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (defun avy-action-exchange (pt)
  "Exchange sexp at PT with the one at point."
  (set-mark pt)
  (transpose-sexps 0))

  
  (defun avy-action-kill-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-line))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)
  
  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)
  
  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)
  
  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)
  
  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?l ?o
                   ?v ?b ?n ?, ?/ ?u ?p ?e
                   ?c ?q ?2 ?3 ?' ?\;))
  (setq avy-dispatch-alist '((?m . avy-action-mark)
                             (?i . avy-action-ispell)
                             (?z . avy-action-zap-to-char)
                             (?.  . avy-action-embark)
                             (?= . avy-action-define)
                             (?  . avy-action-mark-to-char)
                             ;; (67108925 . avy-action-tuxi)
                             ;; (?W . avy-action-tuxi)
                             ;; (?h . avy-action-helpful)
                             (?x . avy-action-exchange)
                             
                             (11 . avy-action-kill-line)
                             (25 . avy-action-yank-line)
                             
                             ;; (?w . avy-action-easy-copy)
                             ;; (134217847  . avy-action-easy-copy)
                             (?k . avy-action-kill-stay)
                             (?y . avy-action-yank)
                             (?t . avy-action-teleport)
                             
                             (?W . avy-action-copy-whole-line)
                             (?K . avy-action-kill-whole-line)
                             (?Y . avy-action-yank-whole-line)
                             (?T . avy-action-teleport-whole-line))))




;;;;;;;;;;;;;;;;;;;;;;
;; ** CRUX 
;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(define-prefix-command 'ak-crux-map)
(global-set-key (kbd "` x") 'ak-crux-map)

(use-package crux
  :ensure t
  :bind
  ("C-k" . crux-smart-kill-line)
  (:map ak-map 
        ("o" . crux-smart-open-line )
        ("O" . crux-smart-open-line-above ))
  (:map ak-crux-map
        ;;     ("U" . crux-view-url)
        ;;("a" . crux-ispell-word-then-abbrev)
        ("." . crux-find-shell-init-file)
        ("1" . crux-find-user-init-file)
        ("a" . crux-move-beginning-of-line)
        ;; ("o" . crux-smart-open-line)
        ;; ("O" . crux-smart-open-line-above)
        ("d" . crux-duplicate-current-line-or-region)
        ("j" . crux-top-join-line)
        ("k" . crux-kill-line-backwards)
        ("C" . crux-cleanup-buffer-or-region)
        ("r" . crux-recentf-find-file)
        ("D" . crux-recentf-find-directory)
        ;; ("U" . crux-upcase-region)
        ;; ("L" . crux-downcase-region)
        ("i" . crux-insert-date)
        ;; ("c" . crux-capitalize-region)
        ("w" . crux-other-window-or-switch-buffer)
        ("s" . crux-sudo-edit)
        ("<f2>" . crux-rename-buffer-and-file)
        ("0" . crux-delete-file-and-buffer)
        (";" . crux-duplicate-and-comment-current-line-or-region)
        ("<f3>" . crux-kill-buffer-truename)
        ("<tab>" . crux-indent-defun)))
  ;; (:map dired-mode-map
  ;;       ("<f6>" . crux-open-with)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Hungry deletion
;; Backspace or Delete will get rid of all whitespace until the next
;; non-whitespace character is encountered.
;; Convenient, and very very occasionally - irritating ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hungry-delete
  :ensure t
  :diminish
  :config
  (global-hungry-delete-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Mark-Multiple             
;; This extension allows you to quickly
;; mark the next occurence of a
;; region and edit them all at once. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package mark-multiple
  :ensure t
  ;; :straight t
  :bind (:map ak-map
              ((">" . mark-next-like-this)
               ("<" . mark-previous-like-this)
               ("+" . mark-more-like-this-extended)
               ("=" . mark-all-like-this))))


;; Better defaults for upcase/downcase
(use-package simple
  :config
  (global-set-key (kbd "M-u") 'upcase-dwim)
  (global-set-key (kbd "M-l") 'downcase-dwim)
  (global-set-key (kbd "M-c") 'capitalize-dwim)
  
  ;; Change case with impunity 
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil))

(use-package jinx
  :if (not ak/generic-windows-p)
  :ensure t
  :hook (text-mode . jinx-mode)
  :bind ([remap ispell-word] . jinx-correct))
  ;; :config (dolist
  ;;             (hook '(text-mode-hook org-mode-hook))
  ;;           (add-hook hook #'jinx-mode)))


(use-package easy-kill
  :ensure t
  :bind ("C-=" . easy-mark)
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))


;;from https://emacs.stackexchange.com/questions/3941/when-typing-automatically-transform-spc-spc-into-period-spc-spc
;;I don't want to double space after period, so just a single space after the period 
;;is fine.

(defun freaky-space ()
  (interactive)
  (cond ((looking-back "\\(?:^\\|\\.\\)  +")
         (insert " "))
        ((eq this-command
             last-command)
         (backward-delete-char 1)
         (insert ". "))
        (t
         (insert " "))))

(define-key text-mode-map " " 'freaky-space)    

(provide 'init-editing-functions)
