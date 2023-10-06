;;; -*- lexical-binding: t; -*-

(require 'xkcd)
(require 'url)
(require 'avy)

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

(define-key ak-map "W" '("Kill inner word" . ak/kill-inner-word))


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

(define-key ak-map "w" '("Copy word" . ak/copy-whole-word))


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

(define-key ak-map "s" '("Copy Sexp" . ak/copy-whole-sexp))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Copy a line                                                    ;;
;;  Regardless of where your cursor is, this quickly copies a line.  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ak/copy-whole-line ()
  "Copies a line without regard for cursor position."
  (interactive)
  (save-excursion
    (kill-new
     (filter-buffer-substring (pos-bol) (pos-eol))))
  (message "Copied current line"))

(define-key ak-map "l" '("Copy line" . ak/copy-whole-line))

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
(define-key ak-map "L" '("Kill line" . kill-whole-line))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert date at point. With prefix, insert time-stamp too  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key ak-map "i" '("Insert date/time stamp" . (lambda (prefix) 
                                                      (interactive "P")
                                                      (if prefix 
                                                          (insert (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))
                                                        (insert (format-time-string "%Y-%m-%d" (current-time)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;From https://www.reddit.com/r/orgmode/comments/13xs6bo/converting_a_web_page_to_org_mode_to_include_in/ ;;
;; converts selected text in clipboard to html, and then uses pandoc to convert it to org mode            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ak/insert-org-from-html-clipboard ()
  "Converts selected text in system clipboard to html, 
and then uses pandoc to convert it to org mode"
  (interactive)
  (if (not (executable-find "pandoc")) 
      (error "pandoc executable not found"))
  (let* ((pandoc-command 
          "pandoc -f html -t org --wrap=none")
       (linux-clip-as-html-command 
        "xclip -select clipboard -target text/html -o")
       (mac-clip-as-html-command 
        "osascript -e 'the clipboard as \"HTML\"' | perl -ne 'print chr foreach unpack(\"C*\",pack(\"H*\",substr($_,11,-3)))'")
       (windows-clip-as-html-command 
        "powershell -command Get-Clipboard -Format Text -TextFormatType Html"))
    (cond 
     (ak/generic-windows-p 
      (shell-command (concat windows-clip-as-html-command " | " pandoc-command) 1))
     (ak/generic-mac-p 
      (shell-command (concat mac-clip-as-html-command " | " pandoc-command) 1))
     (t 
      (shell-command (concat linux-clip-as-html-command " | " pandoc-command) 1)))))

;; (define-key ak-map (kbd "<f2>") 'ak/insert-org-from-html-clipboard)
(define-key ak-map (kbd "<f4>") '("Insert clipboard as org" . (lambda () 
                                                                     (interactive)
                                                                     (ak/insert-org-from-html-clipboard)
                                                                     (org-web-tools--clean-pandoc-output))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move lines up/down
;; Copied from this
;; [[https://stackoverflow.com/questions/2423834/move-line-region-up-and-down-in-emacs][stackoverflow
;; post]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; move the line(s) spanned by the active region up/down (line transposing)
(defun move-lines (n)
  (let ((beg) (end) (keep))
    (if mark-active
        (save-excursion
          (setq keep t)
          (setq beg (region-beginning)
                end (region-end))
          (goto-char beg)
          (setq beg (line-beginning-position))
          (goto-char end)
          (setq end (line-beginning-position 2)))
      (setq beg (line-beginning-position)
            end (line-beginning-position 2)))
    (let ((offset (if (and (mark t)
                           (and (>= (mark t) beg)
                                (< (mark t) end)))
                      (- (point) (mark t))))
          (rewind (- end (point))))
      (goto-char (if (< n 0) beg end))
      (forward-line n)
      (insert (delete-and-extract-region beg end))
      (backward-char rewind)
      (if offset (set-mark (- (point) offset))))
    (if keep
        (setq mark-active t
              deactivate-mark nil))))

(defun ak/move-lines-up (n)
  "move the line(s) spanned by the active region up by N lines."
  (interactive "*p")
  (move-lines (- (or n 1))))

(defun ak/move-lines-down (n)
  "move the line(s) spanned by the active region down by N lines."
  (interactive "*p")
  (move-lines (or n 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reload xkcd cartoon on dashboard  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when window-system
  (defun ak/reload-xkcd (arg)
    "Load a random xkcd cartoon on the dashboard.
With PREFIX - load the latest xkcd cartoon"
    (interactive "P")
    (let ((rand-id-xkcd nil)
          (rand-id-xkcd-url nil))

      (with-temp-buffer
        (if arg
            (setq rand-id-xkcd (string-to-number(xkcd)))
          (setq rand-id-xkcd (string-to-number( xkcd-rand))))
        (setq rand-id-xkcd-url (concat "http://xkcd.com/" (number-to-string rand-id-xkcd)))
        (xkcd-kill-buffer))
      
      (let ((last-xkcd-png (concat xkcd-cache-dir (number-to-string rand-id-xkcd) ".png")))
        (if (file-exists-p last-xkcd-png)
            (setq dashboard-startup-banner last-xkcd-png
                  dashboard-banner-logo-title rand-id-xkcd-url
                  dashboard-init-info xkcd-alt))))
    (revert-buffer))

  (define-key ak-map "X" '("Reload xkcd dashboard" . ak/reload-xkcd)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Download a random file            
;; Below is from https://stackoverflow.com/questions/4448055/download-a-file-with-emacs-lisp ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ak/download-file (&optional url download-dir download-name)
  (interactive)
  (message "Downloading...%s" url)
  (let ((url (or url
                 (read-string "Enter download URL: "))))
    (let ((download-buffer (url-retrieve-synchronously url)))
      ;; (save-excursion
        (set-buffer download-buffer)
        ;; we may have to trim the http response
        (goto-char (point-min))
        (re-search-forward "^$" nil 'move)
        (forward-char)
        (delete-region (point-min) (point))
        (write-file (concat (or download-dir
                                "~/downloads/")
                            (or download-name
                                (car (last (split-string url "/" t))))))
        (kill-buffer download-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Doing =C-x k= should kill the current buffer at all times ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun kill-current-buffer ()
  "Kills the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") '("Kill Current Buffer". kill-current-buffer))


;;;;;;;;;;;;;;;;;;;;;;;
;; Close All Buffers ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun close-all-buffers ()
  "Kill all buffers without regard for their origin."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(global-set-key (kbd "C-M-s-k") '("Kill all buffers" . close-all-buffers))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; *** Following window splits
;; Also opens the previous buffer in the
;; newly opened window ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun split-and-follow-horizontally (prefix)
  (interactive "P")
  (split-window-below)
  (balance-windows)
  (other-window 1 nil)
  (if  prefix 
      (switch-to-next-buffer)
    (switch-to-prev-buffer)))

(global-set-key (kbd "C-x 2") '("Split window horiz" . split-and-follow-horizontally))

(defun split-and-follow-vertically (prefix)
  (interactive "P")
  (split-window-right)
  (balance-windows)
  (other-window 1)
  (if prefix
      (switch-to-next-buffer)
    (switch-to-prev-buffer)))

(global-set-key (kbd "C-x 3") '("Split window vert" . split-and-follow-vertically))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom function to mark a field in an org table ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ak/org-table-mark-field ()
  "Mark the current table field."
  (interactive)
  ;; Do not try to jump to the beginning of field if the point is already there
  (when (not (looking-back "|\\s-?" nil))
    (org-table-beginning-of-field 1))
  (set-mark-command nil)
  (org-table-end-of-field 1))

(define-key ak-map "-" '("Mark table cell" . ak/org-table-mark-field))


;;;;;;;;;;;;;;;;;;;;;
;; Scratch buffers ;;
;;;;;;;;;;;;;;;;;;;;;

;; Switch to scratch buffer
(if (> emacs-major-version 28)
    (keymap-set ak-map "z" 'scratch-buffer)
  (define-key ak-map "z" '("Switch to scratch buffer" . (lambda ()
                                                          "Switch to scratch"
                                                          (interactive)
                                                          (switch-to-buffer "*scratch*")))))

;; Switch to text scratch buffer
(define-key ak-map "2" '("Switch to txt scratch" . (lambda ()
                                                      "Switch to text scratch"
                                                      (interactive)
                                                      (switch-to-buffer(get-buffer-create "*scratch-text*")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use avy to jump from cell to cell in orgmode table ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    
;; (define-key ak-map "%" 'ak/avy-org-table-1-char)
(define-key ak-map "%" '("Avy jump to table cell" . ak/avy-org-table-1-char))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Experimental features ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun ak/lookup-word(arg)
  "Lookup current word in the dictionaries. With
PREFIX, specify word to search"
  (interactive "P")
  (if arg
    (dictionary-search nil)
      (dictionary-lookup-definition)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Replace garbage in word and other copy and paste  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun ak/replace-garbage ()
;; "Replace non-rendering MS and other garbage characters with latin1 equivalents."
;; (interactive)
;; (save-excursion             ;save the current point
;; (replace-string "\221" "`" nil (point-min) (point-max))
;; (replace-string "\222" "'" nil (point-min) (point-max))
;; (replace-string "\226" "-" nil (point-min) (point-max))
;; (replace-string "\227" "--" nil (point-min) (point-max))
;; (replace-string "\223" "(" nil (point-min) (point-max))
;; (replace-string "\224" ")" nil (point-min) (point-max))
;; (replace-string "\205" "..." nil (point-min) (point-max))
;; (replace-string "\225" "-" nil (point-min) (point-max))
;; (replace-string "\344" "" nil (point-min) (point-max))
;; (replace-string "\374" "" nil (point-min) (point-max))
;; (replace-string "\337" "" nil (point-min) (point-max))
;; (replace-string "\366" "" nil (point-min) (point-max))
;; (replace-string "\247" "***" nil (point-min) (point-max))
;; (replace-string "\267" "****" nil (point-min) (point-max))
;; (replace-string "\351" "é" nil (point-min) (point-max))
;; (replace-string "\347" "ç" nil (point-min) (point-max))
;; (replace-string "\352" "ê" nil (point-min) (point-max))
;; (replace-string "\342" "â" nil (point-min) (point-max))
;; (replace-string "\307" "Ç" nil (point-min) (point-max))
;; (replace-string "\340" "à" nil (point-min) (point-max))
;; (replace-string "\340" "à" nil (point-min) (point-max))
;; (replace-string "\364" "ô" nil (point-min) (point-max))
;; (replace-string "\353" "ë" nil (point-min) (point-max))
;; (replace-string "\243" "£" nil (point-min) (point-max))
;; ));end replace-garbage-characters
;; ;bind-key replace-garbage-characters
;; (define-key  ak-map "R"  '("Replace text garbage" . ak/replace-garbage))

(provide 'init-nifty-utils)
