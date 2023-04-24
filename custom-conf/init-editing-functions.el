;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Hungry deletion
;; Backspace or Delete will get rid of all whitespace until the next
;; non-whitespace character is encountered.
;; Convenient, and very very occasionally - irritating ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hungry-delete
  :diminish
  :config
  (global-hungry-delete-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Semantically cycle through selections ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package expand-region
  :bind
  ("C-=" . 'er/expand-region)
  ("C-+" . 'er/contract-region)
  ("C-c q" . 'er/mark-inside-quotes)
  ("C-c Q" . 'er/mark-outside-quotes))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Mark-Multiple             
;; This extension allows you to quickly
;; mark the next occurence of a
;; region and edit them all at once. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package mark-multiple
  ;; :straight t
  :bind (:map ak-map
              ((">" . mark-next-like-this)
               ("<" . mark-previous-like-this)
               ("+" . mark-more-like-this-extended)
               ("=" . mark-all-like-this))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Improved kill-word ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ak/kill-inner-word (n)
  "Kills the entire word your cursor is in. n words if prefix argument supplied."
  (interactive "*p")
  (forward-char 1)
  (backward-word)
  (kill-word (or n 1))
  (message (format "Killed %d word(s)" (or n 1))))

(define-key ak-map "W" 'ak/kill-inner-word)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Improved copy-word
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ak/copy-whole-word (n)
  "Copy current word, plus additional n words if prefix argument supplied"
  (interactive "*p")
  (save-excursion
    (forward-char 1)
    (backward-word)
    (kill-word (or n 1))
    (yank))
  (message (format "Copied %d word(s)" (or n 1))))

(define-key ak-map "w" 'ak/copy-whole-word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Copy a line                                                    ;;
;;  Regardless of where your cursor is, this quickly copies a line.  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ak/copy-whole-line ()
  "Copies a line without regard for cursor position."
  (interactive)
  (save-excursion
    (kill-new
     (buffer-substring
      ;; (point-at-bol)
      (pos-bol)
      (pos-eol))))
  (message "Copied current line"))

(define-key ak-map "l" 'ak/copy-whole-line)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Kill a line 
;; And this quickly deletes a line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  (global-set-key (kbd "C-c l k") 'kill-whole-line)
;; (define-key ak-map "L" (lambda ()
;;                           (interactive)
;;                           (kill-whole-line)
;;                           (message "Killed whole line")))
(define-key ak-map "L" 'kill-whole-line)


(provide 'init-editing-functions)
