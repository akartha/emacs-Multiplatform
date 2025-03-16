;;; -*- lexical-binding: t; -*-

(require 'url)

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


;; (define-key ak-map (kbd "<f2>") 'ak/insert-org-from-html-clipboard)
(define-key ak-map (kbd "<f4>") '("Insert clipboard as org" . 
                                  (lambda () 
                                    (interactive)
                                    (ak/insert-org-from-html-clipboard)
                                    (org-web-tools--clean-pandoc-output))))

;; (define-key ak-map (kbd "<f5>") '("Clean pandoc vestiges from buffer" . 
;;                                   (lambda () 
;;                                     (interactive)
;;                                     (org-web-tools--clean-pandoc-output))))

;; (define-key ak-map (kbd "<f12>") '("Copy Org as Rich Text" . ak/export-org-to-clipboard-as-rtf))


(defun ak/search-replace-with-region (beg end prefix)
  "Replace selected region text from whole buffer with blanks.
With PREFIX, prompt for replacement string"
(interactive "r\nP")
    (let ((region (buffer-substring beg end))
          (replace-text (if prefix (read-string "Whats the replacement string?:")
                          "")))
      (save-excursion
        (goto-char beg)
        (deactivate-mark)
        (query-replace region replace-text))))

(define-key ak-map (kbd "5") '("Delete Selected region matches from buffer" . ak/search-replace-with-region))

;; from https://coredumped.dev/2019/02/08/using-org-mode-to-write-email-for-outlook/  ;;
;; (defun org-email-html-head ()                                                      ;;
;;   "Create the header with CSS for use with email"                                  ;;
;;   (concat                                                                          ;;
;;    "<style type=\"text/css\">\n"                                                   ;;
;;    "<!--/*--><![CDATA[/*><!--*/\n"                                                 ;;
;;    (with-temp-buffer                                                               ;;
;;      (insert-file-contents                                                         ;;
;;       "~/Dropbox/org-files/org-email-head.css")                                    ;;
;;      (buffer-string))                                                              ;;
;;    "/*]]>*/-->\n"                                                                  ;;
;;    "</style>\n"))                                                                  ;;
;;                                                                                    ;;
;; (defun export-org-email ()                                                         ;;
;;   "Export the current email org buffer and copy it to the                          ;;
;; clipboard"                                                                         ;;
;;   (interactive)                                                                    ;;
;;   (let ((org-export-show-temporary-export-buffer nil)                              ;;
;;         (org-html-head (org-email-html-head)))                                     ;;
;;     (org-html-export-as-html)                                                      ;;
;;     (with-current-buffer "*Org HTML Export*"                                       ;;
;;       (kill-new (buffer-string)))                                                  ;;
;;     (message "HTML copied to clipboard")))                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(use-package xkcd
  :if window-system 
  :ensure t
  :after dashboard 
  :bind (:map dashboard-mode-map
              ("X" . ak/reload-xkcd))
  :config 
;;;###autoload
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
    (revert-buffer)))

  ;; (define-key dashboard-mode-map "X" '("Reload xkcd dashboard" . ak/reload-xkcd)))


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

(define-key ak-map "#" '("Mark table cell" . ak/org-table-mark-field))


;;;;;;;;;;;;;;;;;;;;;
;; Scratch buffers ;;
;;;;;;;;;;;;;;;;;;;;;

;; Switch to scratch buffer
;;;###autoload
(if (> emacs-major-version 28)
    (keymap-set ak-map "z" 'scratch-buffer)
  (define-key ak-map "z" '("Switch to scratch buffer" . (lambda ()
                                                          "Switch to standard scratch"
                                                          (interactive)
                                                          (switch-to-buffer "*scratch*")))))

;; Switch to text scratch buffer
;;;###autoload
(define-key ak-map "2" '("Switch to txt scratch" . (lambda ()
                                                      "Switch to text scratch buffer"
                                                      (interactive)
                                                      (switch-to-buffer(get-buffer-create "*scratch-text*"))
                                                      (text-mode))))
;; Switch to org scratch buffer
(define-key ak-map "4" '("Switch to org scratch" . (lambda ()
                                                      "Switch to org-mode scratch buffer"
                                                      (interactive)
                                                      (switch-to-buffer(get-buffer-create "*scratch-org*"))
                                                      (org-mode))))
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

(use-package hydra
  :ensure t
  :config 
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

  (define-key ak-map "." '("Jump to" . hydra-jump-cursor/body)))

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

(defun xah-html-extract-url (*begin *end &optional *full-path-p)
  "Extract URLs in current block or region to `kill-ring'.

If `universal-argument' is called first, convert relative URL to full path.

This command extracts all text of the forms
 <‹letter› … href=\"…\" …>
 <‹letter› … src=\"…\" …>
that is on a a single line, by regex. The quote may be single quote.

When called in lisp code, *begin *end are region begin/end positions.
Returns a list.

URL `http://ergoemacs.org/emacs/elisp_extract_url_command.html'
Version 2016-07-28"
  (interactive
   (let (-p1 -p2)
     ;; set region boundary -p1 -p2
     (if (use-region-p)
         (progn (setq -p1 (region-beginning))
                (setq -p2 (region-end)))
       (save-excursion
         (if (re-search-backward "\n[ \t]*\n" nil "NOERROR")
             (progn (re-search-forward "\n[ \t]*\n")
                    (setq -p1 (point)))
           (setq -p1 (point)))
         (if (re-search-forward "\n[ \t]*\n" nil "NOERROR")
             (progn (re-search-backward "\n[ \t]*\n")
                    (setq -p2 (point)))
           (setq -p2 (point)))))
     (list -p1 -p2 current-prefix-arg)))

  (let ((-regionText (buffer-substring-no-properties *begin *end))
        (-urlList (list)))
    (with-temp-buffer
      (insert -regionText)

      (goto-char 1)
      (while (re-search-forward "<" nil t)
        (replace-match "\n<" "FIXEDCASE" "LITERAL"))

      (goto-char 1)
      (while (re-search-forward
              "<[A-Za-z]+.+?\\(href\\|src\\)[[:blank:]]*?=[[:blank:]]*?\\([\"']\\)\\([^\"']+?\\)\\2" nil t)
        (push (match-string 3) -urlList)))
    (setq -urlList (reverse -urlList))

    (when *full-path-p
      (setq -urlList
            (mapcar
             (lambda (-x)
               (if (string-match "^http:\\|^https:" -x )
                   (progn -x)
                 (progn
                   (expand-file-name -x (file-name-directory (buffer-file-name))))))
             -urlList)))

    (when (called-interactively-p 'any)
      (let ((-printedResult (mapconcat 'identity -urlList "\n")))
        (kill-new -printedResult)
        (message "%s" -printedResult)))
    -urlList ))


(define-key 'ak-map "9" 'xah-select-line)
(define-key 'ak-map (kbd "<down>") 'xah-select-block)
(define-key 'ak-map (kbd "SPC") 'xah-extend-selection)
(define-key 'ak-map "\"" 'xah-select-text-in-quote)

;;;###autoload
(defun ak/binary-health-check ()
  (interactive)
  (with-output-to-string
    (let* ((binaries '("git" "grep" "rg" "find" "locate" "ls" "xclip" "curl"
                       "jq" "pandoc" "gpg" "fortune"  "mmdc" "ffmpeg"
                       "gopls" "rustfmt" "pylsp" "flake8" "sbcl" "sqlformat" "xsv" "csvlens"))
           (loc)
           (binary-loc))
      (dolist (binary binaries)
        (setq loc (executable-find binary))
        (if loc 
            (progn 
              (push (cons binary loc) binary-loc)
              ;; (shell-eval-command (concat binary " --version")) 
              ;; (message "%s (version %s) available at %s" binary (shell-command (concat binary " --version | head -1")) loc))
              (message "%s available at %s" binary loc))
          (message "%s is not on path or is not installed" binary))))))

;;;###autoload
(defun ak/download-url-at-point ()
  "Place point inside a url and this will download the url 
to the current directory in the '_downloads' folder"
  (interactive)
  (let* ((url 
          (thing-at-point-url-at-point))
         (directory 
          "_downloads/")
         (local-file-name 
          (read-from-minibuffer "Enter filename: ")))
    (ak/download-file url directory local-file-name)))


(use-package easy-kill
  :ensure t
  :bind ("C-=" . easy-mark)
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))


(provide 'init-nifty-utils)
