;;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** avy
;; ;; https://github.com/abo-abo/avy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-prefix-command 'avy-custom-keymap)
(global-set-key (kbd "` y") 'avy-custom-keymap)


(use-package avy
  ;; :straight t
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

;;;;;;;;;;;;;;;;;;;;;;
;; ** CRUX 
;;;;;;;;;;;;;;;;;;;;;

(define-prefix-command 'ak-crux-map)
(global-set-key (kbd "` x") 'ak-crux-map)

(use-package crux
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
        ("<tab>" . crux-indent-defun))
  (:map dired-mode-map
        ("<f6>" . crux-open-with)))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Improved copy sexp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ak/copy-whole-sexp (n)
  "Copy current sexp, plus additional n words if prefix argument supplied"
  (interactive "*p")
  (save-excursion
    (forward-char 1)
    (backward-sexp)
    (kill-sexp (or n 1))
    (yank))
  (message (format "Copied %d sexp(s)" (or n 1))))

(define-key ak-map "s" 'ak/copy-whole-sexp)


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

;; (defun ak/copy-lines (n)
;;   "Copies a lines without regard for cursor position."
;;   (interactive "*p")
;;   (save-excursion
;;     (pos-bol)
;;     (kill-line (or n 1))))
    

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
  ;; :hook (emacs-startup . global-jinx-mode)
  :bind ([remap ispell-word] . jinx-correct)
  :config (dolist
              (hook '(text-mode-hook org-mode-hook))
            (add-hook hook #'jinx-mode)))


(defun ak/replace-garbage ()
"Replace non-rendering MS and other garbage characters with latin1 equivalents."
(interactive)
(save-excursion             ;save the current point
(replace-string "\221" "`" nil (point-min) (point-max))
(replace-string "\222" "'" nil (point-min) (point-max))
(replace-string "\226" "-" nil (point-min) (point-max))
(replace-string "\227" "--" nil (point-min) (point-max))
(replace-string "\223" "(" nil (point-min) (point-max))
(replace-string "\224" ")" nil (point-min) (point-max))
(replace-string "\205" "..." nil (point-min) (point-max))
(replace-string "\225" "-" nil (point-min) (point-max))
(replace-string "\344" "" nil (point-min) (point-max))
(replace-string "\374" "" nil (point-min) (point-max))
(replace-string "\337" "" nil (point-min) (point-max))
(replace-string "\366" "" nil (point-min) (point-max))
(replace-string "\247" "***" nil (point-min) (point-max))
(replace-string "\267" "****" nil (point-min) (point-max))
(replace-string "\351" "é" nil (point-min) (point-max))
(replace-string "\347" "ç" nil (point-min) (point-max))
(replace-string "\352" "ê" nil (point-min) (point-max))
(replace-string "\342" "â" nil (point-min) (point-max))
(replace-string "\307" "Ç" nil (point-min) (point-max))
(replace-string "\340" "à" nil (point-min) (point-max))
(replace-string "\340" "à" nil (point-min) (point-max))
(replace-string "\364" "ô" nil (point-min) (point-max))
(replace-string "\353" "ë" nil (point-min) (point-max))
(replace-string "\243" "£" nil (point-min) (point-max))
));end replace-garbage-characters
;bind-key replace-garbage-characters
(bind-key  "\C-cr"  'ak/replace-garbage)

(provide 'init-editing-functions)
