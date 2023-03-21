;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** avy                                                                                                                  

;; As you invoke one of avy's functions, you will be prompted for a
;; character that you'd like to jump to in the /visible portion of the
;; current buffer/.  ;; Afterwards you will notice how all instances
;; of said character have additional letter on top of them. Pressing
;; those letters, that are next to your desired character will move
;; your cursor over there. ;; ;; [[https://github.com/abo-abo/avy][Avy
;; github]] ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-prefix-command 'avy-custom-keymap)
(global-set-key (kbd "` y") 'avy-custom-keymap)


(use-package avy
  :straight t
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
        ("h" . avy-org-goto-heading-timer)))


(defun ak/avy-org-table-1-char ()
  "Avy navigation of cells in org-mode tables based on any char in the cell.
    `SPC` can be used to jump to any cell. "
  (interactive)
  ;; set some variables to limit candidates to the current table
  (let ((table-begin (save-excursion (goto-char (org-table-begin)) (forward-line -1) (point)))
        (table-end (save-excursion (goto-char (org-table-end)) (forward-line) (point))))
    ;; jump to the desired cell and re-align
    ;; (goto-char
    (avy-with avy-goto-word-0
      (avy-jump (concat "|\\{1\\}[^-\n|]+" (char-to-string (read-char "char: " t)))
                :window-flip nil
                :beg table-begin
                :end table-end )))
(org-table-end-of-field 1 ))
    
(define-key ak-map "%" 'ak/avy-org-table-1-char)

(provide 'avy-custom)
