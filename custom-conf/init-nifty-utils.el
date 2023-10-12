;;; -*- lexical-binding: t; -*-

(require 'xkcd)
(require 'url)
(require 'avy)
(require 'hydra)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Improved kill-word ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun ak/move-lines-up (n)
  "move the line(s) spanned by the active region up by N lines."
  (interactive "*p")
  (move-lines (- (or n 1))))

;;;###autoload
(defun ak/move-lines-down (n)
  "move the line(s) spanned by the active region down by N lines."
  (interactive "*p")
  (move-lines (or n 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reload xkcd cartoon on dashboard  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun kill-current-buffer ()
  "Kills the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") '("Kill Current Buffer". kill-current-buffer))


;;;;;;;;;;;;;;;;;;;;;;;
;; Close All Buffers ;;
;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
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
;;;###autoload
(defun split-and-follow-horizontally (prefix)
  (interactive "P")
  (split-window-below)
  (balance-windows)
  (other-window 1 nil)
  (if  prefix 
      (switch-to-next-buffer)
    (switch-to-prev-buffer)))

(global-set-key (kbd "C-x 2") '("Split window horiz" . split-and-follow-horizontally))

;;;###autoload
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
;;;###autoload
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
;;;###autoload
(if (> emacs-major-version 28)
    (keymap-set ak-map "z" 'scratch-buffer)
  (define-key ak-map "z" '("Switch to scratch buffer" . (lambda ()
                                                          "Switch to scratch"
                                                          (interactive)
                                                          (switch-to-buffer "*scratch*")))))

;; Switch to text scratch buffer
;;;###autoload
(define-key ak-map "2" '("Switch to txt scratch" . (lambda ()
                                                      "Switch to text scratch"
                                                      (interactive)
                                                      (switch-to-buffer(get-buffer-create "*scratch-text*")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use avy to jump from cell to cell in orgmode table ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defhydra hydra-move-lines (:color amaranth
                                   :timeout 5
                                   :columns 2)
  "Move selected lines up/down"
  ("[" (ak/move-lines-up 1) "Move up")
  ("]" (ak/move-lines-down 1) "Move down")
  ("q" nil "Quit" :color blue))

;;;###autoload
(defhydra hydra-jump-cursor (:color amaranth
                                    :columns 3)
  "jump cursor"
  ("]" (forward-sexp) "Forward sexp")
  ;; ("<" (beginning-of-buffer) "Beginning of buffer")
  ("<" (goto-char (point-min)) "Beginning of buffer")
  ;; (">" (end-of-buffer) "End of buffer")
  (">" (goto-char (point-max)) "End of buffer")
  ("[" (backward-sexp) "Backward sexp")
  ;; ("w" (forward-to-word 1) "Forward word")
  ("w" (forward-word-strictly 1) "Forward word")
  ;; ("b" (backward-to-word 1) "Backward word")
  ("b" (backward-word-strictly 1) "Backward word")
  ("e" (forward-sentence) "Forward sentence")
  ("a" (backward-sentence) "Backward sentence")
  ("}" (forward-paragraph) "Forward para")
  ("{" (backward-paragraph) "Backward para")
  ("q" nil "Quit" :color blue))

(define-key ak-map "m" '("Move lines" . hydra-move-lines/body))

(define-key ak-map "." '("Jump to" . hydra-jump-cursor/body))

;;;###autoload
(if (or ak/my-framework-p ak/generic-windows-p)
    (defun ak/emacs-screenshot-svg ()
      "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring."
      (interactive)
      (let* ((filename (make-temp-file "emacs-screenshot" nil ".svg"))
             (data (x-export-frames nil 'svg)))
        (with-temp-file filename
          (insert data))
        (kill-new filename)
        (message filename))))

;;;###autoload
(defun xah-select-line ()
  "Select current line. If region is active, extend selection downward by line.
If `visual-line-mode' is on, consider line as visual line.

URL `http://xahlee.info/emacs/emacs/modernization_mark-word.html'
Version: 2017-11-01 2021-03-19 2023-07-16"
  (interactive)
  (if (region-active-p)
      (if visual-line-mode
          (let ((xp1 (point)))
            (end-of-visual-line 1)
            (when (eq xp1 (point))
              (end-of-visual-line 2)))
        (progn
          (forward-line 1)
          (end-of-line)))
    (if visual-line-mode
        (progn (beginning-of-visual-line)
               (push-mark (point) t t)
               (end-of-visual-line))
      (progn
        (push-mark (line-beginning-position) t t)
        (end-of-line)))))

;;;###autoload
(defun xah-select-block ()
  "Select the current/next block plus 1 blankline.
If region is active, extend selection downward by block.

URL `http://xahlee.info/emacs/emacs/modernization_mark-word.html'
Version: 2019-12-26 2021-04-04 2021-08-13"
  (interactive)
  (if (region-active-p)
      (re-search-forward "\n[ \t]*\n[ \t]*\n*" nil 1)
    (progn
      (skip-chars-forward " \n\t")
      (when (re-search-backward "\n[ \t]*\n" nil 1)
        (goto-char (match-end 0)))
      (push-mark (point) t t)
      (re-search-forward "\n[ \t]*\n" nil 1))))

;;;###autoload
(defun xah-extend-selection ()
  "Select the current word, bracket/quote expression, or expand selection.
Subsequent calls expands the selection.

when there is no selection,
• If cursor is on any type of bracket (including parenthesis, quotation mark)
  , select whole bracketed thing including bracket
• else, select current word.

when there is a selection, the selection extension behavior is still 
experimental. But when cursor is on a any type of bracket 
(parenthesis, quote), it extends selection to outer bracket.

URL `http://xahlee.info/emacs/emacs/modernization_mark-word.html'
Version: 2020-02-04 2023-07-22 2023-07-23"
  (interactive)
  (if (region-active-p)
      (progn
        (let ((xrb (region-beginning)) (xre (region-end)))
          (goto-char xrb)
          (cond
           ((looking-at "\\s(")
            (if (eq (nth 0 (syntax-ppss)) 0)
                (progn
                  ;; (message "left bracket, depth 0.")
                  (end-of-line) ; select current line
                  (push-mark (line-beginning-position) t t))
              (progn
                ;; (message "left bracket, depth not 0")
                (up-list -1 t t)
                (mark-sexp))))
           ((eq xrb (line-beginning-position))
            (progn
              (goto-char xrb)
              (let ((xfirstLineEndPos (line-end-position)))
                (cond
                 ((eq xre xfirstLineEndPos)
                  (progn
                    ;; (message "exactly 1 line. extend to next whole line." )
                    (forward-line 1)
                    (end-of-line)))
                 ((< xre xfirstLineEndPos)
                  (progn
                    ;; (message "less than 1 line. complete the line." )
                    (end-of-line)))
                 ((> xre xfirstLineEndPos)
                  (progn
                    ;; (message "beginning of line, but end is greater than 1st end of line" )
                    (goto-char xre)
                    (if (eq (point) (line-end-position))
                        (progn
                          ;; (message "exactly multiple lines" )
                          (forward-line 1)
                          (end-of-line))
                      (progn
                        ;; (message "multiple lines but end is not eol. make it so" )
                        (goto-char xre)
                        (end-of-line)))))
                 (t (error "%s: logic error 42946" real-this-command))))))
           ((and (> (point) (line-beginning-position)) (<= (point) (line-end-position)))
            (progn
              ;; (message "less than 1 line" )
              (end-of-line) ; select current line
              (push-mark (line-beginning-position) t t)))
           (t
            ;; (message "last resort" )
            nil))))
    (progn
      (cond
       ((looking-at "\\s(")
        ;; (message "left bracket")
        (mark-sexp)) ; left bracket
       ((looking-at "\\s)")
        ;; (message "right bracket")
        (backward-up-list) (mark-sexp))
       ((looking-at "\\s\"")
        ;; (message "string quote")
        (mark-sexp)) ; string quote
       ;; ((and (eq (point) (line-beginning-position)) (not (looking-at "\n")))
       ;;  (message "beginning of line and not empty")
       ;;  (end-of-line)
       ;;  (push-mark (line-beginning-position) t t))
       (
        ;; (prog2 (backward-char) (looking-at "[-_a-zA-Z0-9]") (forward-char))
        (looking-back "[-_a-zA-Z0-9]" (max (- (point) 1) (point-min)))
        ;; (message "left is word or symbol")
        (skip-chars-backward "-_a-zA-Z0-9")
        ;; (re-search-backward "^\\(\\sw\\|\\s_\\)" nil t)
        (push-mark)
        (skip-chars-forward "-_a-zA-Z0-9")
        (setq mark-active t)
        ;; (exchange-point-and-mark)
        )
       ((and (looking-at "[:blank:]")
             (prog2 (backward-char) (looking-at "[:blank:]") (forward-char)))
        ;; (message "left and right both space" )
        (skip-chars-backward "[:blank:]") (push-mark (point) t t)
        (skip-chars-forward "[:blank:]"))
       ((and (looking-at "\n")
             (eq (char-before) 10))
        ;; (message "left and right both newline")
        (skip-chars-forward "\n")
        (push-mark (point)  t t)
        (re-search-forward "\n[ \t]*\n")) ; between blank lines, select next block
       (t
        ;; (message "just mark sexp" )
        (mark-sexp)
        (exchange-point-and-mark))
       ;;
       ))))

(defvar xah-brackets '("“”" "()" "[]" "{}" "<>" "＜＞" "（）" "［］" "｛｝" "⦅⦆" "〚〛" "⦃⦄" "‹›" "«»" "「」" "〈〉" "《》" "【】" "〔〕" "⦗⦘" "『』" "〖〗" "〘〙" "｢｣" "⟦⟧" "⟨⟩" "⟪⟫" "⟮⟯" "⟬⟭" "⌈⌉" "⌊⌋" "⦇⦈" "⦉⦊" "❛❜" "❝❞" "❨❩" "❪❫" "❴❵" "❬❭" "❮❯" "❰❱" "❲❳" "〈〉" "⦑⦒" "⧼⧽" "﹙﹚" "﹛﹜" "﹝﹞" "⁽⁾" "₍₎" "⦋⦌" "⦍⦎" "⦏⦐" "⁅⁆" "⸢⸣" "⸤⸥" "⟅⟆" "⦓⦔" "⦕⦖" "⸦⸧" "⸨⸩" "｟｠")
 "A list of strings, each element is a string of 2 chars,
the left bracket and a matching right bracket.
Used by `xah-select-text-in-quote' and others.
Version 2023-07-31")

(defconst xah-left-brackets
  (mapcar (lambda (x) (substring x 0 1)) xah-brackets)
  "List of left bracket chars. Each element is a string.")

(defconst xah-right-brackets
  (mapcar (lambda (x) (substring x 1 2)) xah-brackets)
  "List of right bracket chars. Each element is a string.")

;;;###autoload
(defun xah-select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters here includes the following chars: \" ` 
and anything in `xah-brackets'.
This command ignores nesting. For example, if text is
    (a(b)c▮)
the selected char is “c”, not “a(b)c”.

URL `http://xahlee.info/emacs/emacs/modernization_mark-word.html'
Version: 2020-11-24 2023-07-16 2023-07-23"
  (interactive)
  (let ((xskipChars (concat "^\"`" (mapconcat #'identity xah-brackets ""))))
    (skip-chars-backward xskipChars)
    (push-mark (point) t t)
    (skip-chars-forward xskipChars)))


(define-key 'ak-map "9" 'xah-select-line)
(define-key 'ak-map (kbd "<right>") 'xah-select-block)
(define-key 'ak-map (kbd "<down>") 'xah-extend-selection)
(define-key 'ak-map "\"" 'xah-select-text-in-quote)


(provide 'init-nifty-utils)
