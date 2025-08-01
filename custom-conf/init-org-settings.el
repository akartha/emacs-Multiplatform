;;; -*- lexical-binding: t; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Org Common settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)
(require 'url-parse)

(defface org-link-id
  '((t :foreground "#00af00"
       :weight bold
       :underline t))
  "Face for Org-Mode links starting with id:."
  :group 'org-faces)

(defface org-link-file
  '((t :foreground "#aa00ee"
       :weight bold
       :underline t))
  "Face for Org-Mode links starting with file:."
  :group 'org-faces)

(defface org-link-denote
  '((t :foreground "#ffffff"
       :weight bold
       :underline t))
  "Face for Org-Mode links starting with denote:."
  :group 'org-faces)

(defface org-link-roam
  '((t :foreground "#afaf00"
       :weight bold
       :underline t))
  "Face for Org-Mode links starting with roam:."
  :group 'org-faces)

(org-link-set-parameters
 "id"
 :face 'org-link-id)

;; (org-link-set-parameters
;;  "file"
;;  :face 'org-link-file)

(org-link-set-parameters
 "denote"
 :face 'org-link-denote)

(org-link-set-parameters
 "roam"
 :face 'org-link-roam)

;; (org-link-set-parameters
;;  "file"
;;  :face (lambda (path) (if (file-exists-p path) 'org-link-file 'org-warning)))

(org-link-set-parameters  
 "file"  
 :face (lambda (path) 
         (when (not (file-remote-p path))
           (if (file-exists-p path) 'org-link-file 'org-warning))))

(define-key global-map (kbd "C-c l") '("Org store link" . org-store-link))

(add-to-list 'org-modules 'org-habit t)

(setq org-babel-load-languages 
        '((emacs-lisp . t)
          (python . t)
          (R . t)
          (sql . t)
          ;; ;;    https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-sql.html ;;
          (sqlite . t)
          (C . t)
          (awk . t)
          (js . t)
          (passthrough . t)
          (shell . t)
          (lua . t)
          (latex . t)
          (dot . t)))

        
(defun ak/next-entry-or-next-visible-header ()
  (interactive)
  (condition-case err
      (org-next-item)
    (error (org-next-visible-heading 1))))

(defun ak/previous-entry-or-previous-visible-header ()
  (interactive)
  (condition-case err
      (org-previous-item)
    (error (org-previous-visible-heading 1))))
        
(define-key ak-map "p" '("Previous item/heading" . ak/previous-entry-or-previous-visible-header))
(define-key ak-map "n" '("Next item/heading" . ak/next-entry-or-next-visible-header))    
(define-key org-mode-map (kbd "M-]") '("Next org item" . org-forward-element))
(define-key org-mode-map (kbd "M-[") '("Previous org item" . org-backward-element))
                  
(setq org-ellipsis "⤵"
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-src-preserve-indentation t
      org-src-strip-leading-and-trailing-blank-lines t
      org-confirm-babel-evaluate nil
      org-image-actual-width nil
      org-agenda-start-with-log-mode t
      org-log-done 'time
      org-log-into-drawer t

      org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
        (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)"  "|" "COMPLETED(c)" "CANC(k@)"))

      org-src-window-setup 'current-window
      org-habit-show-all-today t)


(setq org-directory ak/my-org-file-location)

(setq org-agenda-files (mapcar #'(lambda(s) (expand-file-name s org-directory)) 
                               (list "agenda/Trips.org"
                                     "agenda/Tasks.org"
                                     "agenda/Schedule.org")))

(setq org-use-speed-commands t)

(add-to-list 'recentf-exclude org-agenda-files)

(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'abbrev-mode)


(add-hook 'org-mode-hook
          (lambda ()
            (visual-line-mode 1)))

(setq org-priority-faces '((?A . (:foreground "red" :background "yellow" :weight bold))
                           (?B . (:foreground "cyan" :background "black"))
                           (?C . (:foreground "green" :background "gray"))))

;; (add-to-list 'org-agenda-custom-commands
(setq org-agenda-custom-commands
             '(("w" "Custom agenda view - prioritized"
                ((tags-todo "PRIORITY={A}"
                            ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                             (org-agenda-overriding-header "High Priority - incomplete:")))
                 (tags-todo "PRIORITY={B}"
                            ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                             ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))
                             ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'notscheduled ))
                             (org-agenda-overriding-header "Medium Priority - incomplete:")))
                 (tags-todo "PRIORITY={C}"
                            ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                             (org-agenda-overriding-header "Low Priority - incomplete:")))
                 ;; (alltodo "")
                 (agenda "" ((org-agenda-span 1)
                             (org-deadline-warning-days 0)
                             (org-agenda-block-separator nil)
                             (org-scheduled-past-days 0)
                             (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                             (org-agenda-format-date "%A %-e %B %Y")
                             (org-agenda-overriding-header "\nToday's agenda\n")))
                 (agenda "" ((org-agenda-start-on-weekday nil)
                             (org-agenda-start-day "+1d")
                             (org-agenda-span 3)
                             (org-deadline-warning-days 0)
                             (org-agenda-block-separator nil)
                             (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                             (org-agenda-overriding-header "\nNext three days\n")))
                 (agenda "" ((org-agenda-time-grid nil)
                             (org-agenda-start-on-weekday nil)
                             ;; We don't want to replicate the previous section's
                             ;; three days, so we start counting from the day after.
                             (org-agenda-start-day "+4d")
                             (org-agenda-span 14)
                             (org-agenda-show-all-dates nil)
                             (org-deadline-warning-days 0)
                             (org-agenda-block-separator nil)
                             ;; (org-agenda-entry-types '(:deadline))
                             (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                             (org-agenda-overriding-header "\nUpcoming activities (+14d)\n")))
                         ))))

(add-to-list 'org-agenda-custom-commands
             '("p" "All Tasks (grouped by Priority)"
                ((tags-todo "PRIORITY={A}"
                           ((org-agenda-overriding-header "HIGH")))
                            ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline 'notscheduled 'todo '("WAITING" "DONE")))))
                (tags-todo "PRIORITY={B}"
                           ((org-agenda-overriding-header "MEDIUM")))
                            ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline 'notscheduled 'todo '("WAITING" "DONE")))))
                ;; (tags-todo "PRIORITY=\"\""
                ;;            ((org-agenda-overriding-header "NONE")))
                ;;             ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline 'notscheduled 'todo '("WAITING" "DONE")))))
                (tags-todo "PRIORITY={C}"
                           ((org-agenda-overriding-header "LOW")))
                            ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline 'notscheduled 'todo '("WAITING" "DONE")))))
                (todo "DONE|CANX"
                      ((org-agenda-overriding-header "COMPLETED")
                       (org-agenda-sorting-strategy '(priority-down)))))) t)



;;;;;;;;;;;;;;;;;;;;
;; ** Keybindings ;;
;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c '") 'org-edit-src-code)
(define-key global-map (kbd "C-c a") '("Org agenda" . org-agenda))

(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :custom 
  ;; (org-superstar-headline-bullets-list '("◉" ("🞛" ?◈) "○" "▷"))
  (org-superstar-headline-bullets-list '( "○" "❍" "⬭" "⬮" "▢" "⬨" "⬩"))
  (org-superstar-leading-fallback ?\s)
  (org-superstar-leading-bullet "․")
  (org-superstar-cycle-headline-bullets nil)
  (org-superstar-special-todo-items t)
  (org-superstar-todo-bullet-alist '(("TODO" . 9744) 
                                     ("NEXT" . 925)
                                     ("DONE" . 9745) 
                                     ("BACKLOG" . 128193)
                                     ("PLAN" . 128198)
                                     ("READY" . 8729)
                                     ("ACTIVE" . 9201)
                                     ("REVIEW" . 128064)
                                     ("WAIT" . 9203)
                                     ("HOLD" . 8987)
                                     ("CANC" . 9746)
                                     ("COMPLETED" . 9745))))

;; *** Reveal.js export

(use-package ox-reveal
  :ensure t
  ;; https://github.com/yjwen/org-reveal
  :config
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))


(require 'ob-passthrough)

(use-package ob-go
  :ensure t
  :demand t 
  ;; :after (go org)
  :config 
  (add-to-list 'org-babel-load-languages '(go . t)))

(use-package restclient
  :ensure t)
  ;; :after org)

(use-package ob-restclient
  :ensure t
  :demand t
  :mode ("\\.org\\'" . org-mode)
  :config 
  (add-to-list 'org-babel-load-languages '(restclient . t))
  (add-to-list 'org-babel-load-languages '(jq . t)))

(use-package plantuml-mode
  :ensure t
  ;; :demand t 
  ;; :after org 
  :mode ("\\.org\\'" . org-mode)
  :init
  (setq plantuml-jar-path "~/plantuml.jar")
  (setq org-plantuml-jar-path plantuml-jar-path)
  (setq plantuml-default-exec-mode 'jar)
  :config
  ;; (add-to-list 'org-babel-load-languages  '(plantuml . t)))
  (cl-pushnew '(plantuml . t) org-babel-load-languages))

(use-package verb
  :ensure t
  :demand t
  ;; :after org 
  :mode ("\\.org\\'" . org-mode)
  :config 
    ;; (cl-pushnew '(verb . t) org-babel-load-languages)
  (add-to-list 'org-babel-load-languages '(verb . t))
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(use-package mermaid-mode
  :ensure t)

(use-package ob-mermaid
  :ensure t
  :after mermaid-mode
  :demand t 
  :mode ("\\.org\\'" . org-mode)
  ;; :if ak/my-framework-p
  :init 
  (if ak/my-framework-p 
      (setq ob-mermaid-cli-path "~/.nvm/versions/node/v19.5.0/bin/mmdc"))
  :config 
  ;; (cl-pushnew '(mermaid . t) org-babel-load-languages))
  (add-to-list 'org-babel-load-languages '(mermaid . t)))


(use-package ob-duckdb
  :ensure (:type git :host github :repo "gggion/ob-duckdb")
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages '((duckdb . t)))))

(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

;;;;;;;;;;;;;;;;;
;; ** Org Roam ;;
;;;;;;;;;;;;;;;;;



(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      '(("t" "Todo" entry 
         (file+headline "agenda/Tasks.org" "Captured Tasks")
         "* TODO %?\n DEADLINE:%^t  %i\n  %a")
        ("s" "Schedule" entry 
         (file+headline  "agenda/Schedule.org" "Add to Schedule")
          ;; (file+headline  ,(file-truename "agenda/Schedule.org") "Add to Schedule")
         "* Appointment %?\n SCHEDULED:%^T\n%i")
        ;; ("j" "Journal")
        ("j" "General Journal" entry 
         (file+headline "agenda/jrnl.org" "General")
         "* %^{Capture}\n :PROPERTIES:\n :CAPTURED: %U\n :END: \n\n %?")
        ("w" "Work Journal" entry 
         (file+headline "agenda/jrnl.org" "Work")
         "* %^{Capture}\n :PROPERTIES:\n :CAPTURED: %U\n :END: \n\n %?")
        ("p" "Private-Journal" entry 
         (file+headline "agenda/jrnl.org" "Private")
         "* %^{Capture}\n :PROPERTIES:\n :CAPTURED: %U\n :END: \n\n%?")))

(use-package ox-odt 
  :ensure (:type git :host github :repo "kjambunathan/org-mode-ox-odt"
                 :files ("lisp/ox-odt.el"
                         "lisp/odt.el"
                         "etc"
                         "docs"
                         "contrib/odt/LibreOffice")))

(use-package ox-twbs
  :ensure t)

(use-package org-web-tools
  :load-path "custom-conf/third-party/org-web-tools/"
  :ensure t
  :after org 
  :commands (org-web-tools--get-url org-web-tools--get-first-url)
  :hook ((org-capture-before-finalize . ak/delete-image-base-64-data-lines)
         (org-capture-before-finalize . ak/org-extract-top-level-domain-into-property-drawer))
  :bind (:map ak-map
              ;; ("<f2>" . ak/get-url-title)
              ;; ("<f2>" . ak/clip-web-page-title-and-search-org-roam)
              ("<f3>" . org-web-tools-insert-web-page-as-entry)
              ;; ("<f6>" . org-web-tools-read-url-as-org)
              ("<f7>" . org-web-tools-insert-link-for-url)
              ("<f8>" . ak/delete-image-base-64-data-lines))
  :config 
  (setq org-web-tools-pandoc-sleep-time 0.7)
;;;###autoload
  (cl-defun ak/get-url-title(&optional (url (org-web-tools--get-first-url)))
    "Parses web page's title from url in clipboard."
    (interactive)
    (let* ((html (org-web-tools--get-url url))
           (title (org-web-tools--html-title html)))
      (if title (insert title) (insert url))))

;;;###autoload
  (defun ak/delete-image-base-64-data-lines ()
    "Deletes lines containing base-64 image data from the buffer."
    (interactive)
    (save-excursion
      (let ((kill-patterns '(;;"\\[\\[data:image/svg\\+xml;base64,[^]]*\\]\\]"
                             "^Advertisement$"
                             "\\[\\[data:image[^]]*\\]\\]"
                             "\\[\\[data:%20image[^]]*\\]\\]"
                             ;; "\\[\\[data:image/svg[^]]*\\]\\]"
                             ;; "\\[\\[data:image/png[^]]*\\]\\]"
                             ;; "\\[\\[data:image/gif;base64,[^]]*\\]\\]"
                             "^Copy link to cartoon$"
                             "^Link copied$"
                             "^Article continues after advertisement$"
                             "^Shop$")))
        (dolist (kill-pattern kill-patterns)
          (goto-char (point-min))
          (while (re-search-forward kill-pattern nil t)
            (beginning-of-line)
            (delete-line))))))

;;;###autoload
(defun ak/org-set-document-property (property value)
  "Set a document-level PROPERTY to VALUE inside the property drawer.
If the drawer or property does not exist, create them.
Does not overwrite existing properties."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^:PROPERTIES:" nil t)
        (let ((drawer-end (save-excursion (re-search-forward "^:END:" nil t))))
          ;; Check if property already exists
          (if (save-excursion
                (re-search-forward (format "^:%s: " (upcase property)) drawer-end t))
              (message "Property :%s: already exists, skipping." (upcase property))
            ;; Insert property before :END:
            (re-search-forward "^:END:" nil t)
            (beginning-of-line)
            (insert (format ":%s: %s\n" (upcase property) value))
            (message "Inserted :%s: property with value: %s" (upcase property) value)))
      ;; No property drawer — create one
      (goto-char (point-min))
      (forward-line)
      (insert ":PROPERTIES:\n")
      (insert (format ":%s: %s\n" (upcase property) value))
      (insert ":END:\n")
      (message "Created property drawer with :%s: %s" (upcase property) value))))


;;;###autoload
(defun ak/org-extract-top-level-domain-into-property-drawer ()
"Extract domain from the first top-level headline link and store it in the :SITE: document-level property."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\* .*?\\[\\[\\(https?://[^]]+\\)\\]\\[.*?\\]\\]" nil t)
      (let* ((url-str (match-string 1))
             (parsed-url (url-generic-parse-url url-str))
             (domain (url-host parsed-url)))
        (when domain
          ;; (org-set-property "SITE" domain)))))))
          (ak/org-set-document-property "SITE" domain)))))))

;; (add-hook 'org-capture-before-finalize-hook 'ak/delete-image-base-64-data-lines))
;; (add-hook 'org-roam-capture 'ak/delete-image-base-64-data-lines))

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance '("crypt"))


;;;###autoload
(defun ak/my-insert-clipboard-png ()
  "Paste image data in clipboard and save it to the 
(existing or new) '_media' directory in the current
 working directory. 

Works on Windows (using built-in powershell command), Mac 
(using pngpaste - install with brew) and Linux (requires xclip)
Image is saved as png and function inserts an org buffer block 
with image details."
  (interactive)
   (let* ((directory 
          "_media") ;;creates this directory in the current document's folder
         (default-file-or-caption-name 
          (concat (buffer-name) "_" (format-time-string "%Y%m%d_%H%M%S"))) ;;image defaults to this file/caption if none provided
         (user-filename 
          (read-from-minibuffer "Image File Name: "))
         (user-caption 
          (read-from-minibuffer "Image Caption: "))
         (filename 
          (if (string= "" user-filename)
              default-file-or-caption-name
            user-filename))
         (caption 
          (if (string= "" user-caption)
              default-file-or-caption-name
            user-caption))
         (linux-shell-clip-command 
          "xclip -selection clipboard -t image/png -o > %s.png")
         (mac-shell-clip-command 
          "pngpaste %s.png")
         (windows-shell-clip-command 
          "powershell -command \"Add-Type -AssemblyName System.Windows.Forms;if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {$image = [System.Windows.Forms.Clipboard]::GetImage();[System.Drawing.Bitmap]$image.Save('%s.png',[System.Drawing.Imaging.ImageFormat]::Png); Write-Output 'Clipboard Content Saved As File'} else {Write-Output 'Clipboard Does Not Contain Image Data'}\""))
    ;; (make-directory (concat default-directory directory) t)
    (make-directory (file-name-concat (directory-file-name default-directory) directory) t)
    (cond ((or ak/my-framework-p ak/my-pi-p)
           ;; (shell-command (format linux-shell-clip-command (shell-quote-argument (concat default-directory  directory "/" filename )))))
           (shell-command (format linux-shell-clip-command (shell-quote-argument (file-name-concat (directory-file-name default-directory) directory  filename )))))
          (ak/generic-windows-p
           ;; (shell-command (format windows-shell-clip-command (shell-quote-argument (concat default-directory  directory "/"  filename)))))
           (shell-command (format windows-shell-clip-command (shell-quote-argument (file-name-concat (directory-file-name default-directory) directory filename)))))
          (ak/my-mac-p
           ;; (shell-command (format mac-shell-clip-command (shell-quote-argument (concat default-directory  directory "/" filename))))))
           (shell-command (format mac-shell-clip-command (shell-quote-argument (file-name-concat (directory-file-name default-directory) directory filename))))))
    ;; Insert formatted link at point
    (save-excursion 
      (insert(format 
              "#+CAPTION: %s\n#+ATTR_HTML: :alt %s\n#+attr_html: :width 750px \n#+attr_latex: :width 0.4\\textwidth \n[[file:%s.png]]"
              ;; caption caption (concat directory "/" filename))))
              caption caption (file-name-concat (directory-file-name directory) filename))))
    ;; Message success to the minibuffer
    (message "saved to %s as %s.png" directory filename))
  (org-display-inline-images))

(define-key ak-map "v" '("Save clipboard image as org" . ak/my-insert-clipboard-png))

(use-package ox-pandoc
  :demand t
  :ensure t)

;; (use-package org-margin 
;;   :ensure (:type git :host github :repo "rougier/org-margin"))


(use-package ox-hugo
  :ensure t   
  :pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :after ox)


(defun ak/set-author-property()
  (interactive)
  (if (region-active-p) (call-interactively 'ak/set-author-property-with-region)
    (call-interactively 'ak/set-author-property-with-value)))

(defun ak/set-author-property-with-value(auth)
  (interactive "sEnter Author value: ")
  (save-excursion
    (goto-char (point-min))
    (org-set-property "AUTHOR" auth)))


;; this is from 
;; https://stackoverflow.com/questions/66574715/how-to-get-org-mode-file-title-and-other-file-level-properties-from-an-arbitra

  
(defun ak/get-keyword-key-value (kwd)
  (let ((data (cadr kwd)))
    (list (plist-get data :key)
          (plist-get data :value))))

(defun ak/org-current-buffer-get-title ()
  (nth 1
       (assoc "TITLE"
              (org-element-map (org-element-parse-buffer 'greater-element)
                  '(keyword)
                #'ak/get-keyword-key-value))))


(defun ak/org-current-buffer-get-author ()
  (nth 1
       (assoc "AUTHOR"
              (org-element-map (org-element-parse-buffer 'greater-element)
                  '(keyword)
                #'ak/get-keyword-key-value))))

(defun ak/org-file-get-title (file)
  (with-current-buffer (find-file-noselect file)
    (ak/org-current-buffer-get-title)))


(defun ak/org-file-get-author (file)
  (with-current-buffer (find-file-noselect file)
    (ak/org-current-buffer-get-author)))


;; Following is for a means to move separately generated org files
;; For e.g. nytimes downloaded articles, into orgroam
(defun ak/add-org-to-roam (f)
  (save-excursion 
    (find-file f)
    (goto-char (point-min))
    (org-id-get-create)
    ;; (org-roam-tag-add '("nytimes"))
    (org-set-property "AUTHOR" (ak/org-file-get-author f))
    (write-file f)
    (kill-buffer (current-buffer))))

(defun ak/process-input-org-dir-for-roam (dir) 
  (mapc 'ak/add-org-to-roam 
        (directory-files dir t ".org$")))
;; Then run 
;; (ak/process-input-org-dir-for-roam "~/Dropbox/org-files/nytimes/")
(define-key ak-map "7" '("Process nytimes articles into org-roam" . 
                         (lambda ()
                           (interactive)
                           (ak/process-input-org-dir-for-roam "~/Dropbox/org-files/nytimes/"))))
;; From https://emacs.stackexchange.com/questions/69924/count-words-under-subtree-ignoring-the-properties-drawer-and-the-subheading?newreg=292bf50260404217b2b4fd90952855a5
;; (require 'cl-lib)
;; (require 'org-element)

(defun org-element-parse-headline (&optional granularity visible-only)
  "Parse current headline.
GRANULARITY and VISIBLE-ONLY are like the args of `org-element-parse-buffer'."
  (let ((level (org-current-level)))
    (org-element-map
    (org-element-parse-buffer granularity visible-only)
    'headline
      (lambda (el)
    (and
     (eq (org-element-property :level el) level)
     (<= (org-element-property :begin el) (point))
     (<= (point) (org-element-property :end el))
     el))
      nil 'first-match 'no-recursion)))

(cl-defun org+-count-words-of-heading (&key (worthy '(paragraph bold italic underline code footnote-reference link strike-through subscript superscript table table-row table-cell))
                        (no-recursion nil))
  "Count words in the section of the current heading.
WORTHY is a list of things worthy to be counted. This list should at least include the symbols:
paragraph, bold, italic, underline and strike-through. 
If NO-RECURSION is non-nil don't count the words in subsections."
  (interactive (and current-prefix-arg
            (list :no-recursion t)))
  (let ((word-count 0))
    (org-element-map
    (org-element-contents (org-element-parse-headline))
    '(paragraph table)
      (lambda (par)
    (org-element-map
        par
        worthy
        (lambda (el)
          (cl-incf
           word-count
           (cl-loop
        for txt in (org-element-contents el)
        when (eq (org-element-type txt) 'plain-text)
        sum
        (with-temp-buffer
          (insert txt)
          (count-words (point-min) (point-max))))
           ))))
      nil nil (and no-recursion 'headline)
      )
      (when (called-interactively-p 'any)
      (message "Word count in section: %d" word-count))
    word-count))

(define-key ak-map "c" '("Count words in section" . org+-count-words-of-heading))


(defun ak/set-author-property-with-region(beg end)
  (interactive "r")
  (let ((region (buffer-substring beg end)))
  (save-excursion
    (goto-char (point-min))
    (org-set-property "AUTHOR" region))))

;;adapted from https://www.reddit.com/r/emacs/comments/ousics/date_conversion_error/
;;;###autoload
(defun ak/encode-time-string-intl-std (string)
  (let* ((dt (parse-time-string string))
     (dtt (if (car dt)
          dt
        (append '(0 0 0) (nthcdr 3 dt))))
     )
    (format-time-string "%Y-%m-%d %T" (apply #'encode-time dtt))))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEAD function
;; (defun ak/download-image-at-point-and-insert-org-link ()                                                                            ;;
;;   "Download the image in an org article to '_downloads' folder                                                                      ;;
;; and insert org image block for it"                                                                                                  ;;
;;   (interactive)                                                                                                                     ;;
;;   (let* ((url (thing-at-point-url-at-point))                                                                                        ;;
;;          (url-bnds (bounds-of-thing-at-point 'url))                                                                                 ;;
;;          (directory "_downloads/")                                                                                                  ;;
;;          (local-file-name (read-from-minibuffer "Enter File Name:" (buffer-name))))                                                 ;;
;;     (kill-region (car url-bnds) (cdr url-bnds))                                                                                     ;;
;;     (kill-whole-line 0)                                                                                                             ;;
;;     (insert(format                                                                                                                  ;;
;;             "#+CAPTION: %s\n#+ATTR_HTML: :alt %s\n#+ATTR_HTML: :width 750px \n#+ATTR_LATEX: :width 0.4\\textwidth \n[[file:%s]] \n" ;;
;;             ;; local-file-name url (concat directory local-file-name) ))                                                            ;;
;;             local-file-name url (file-name-concat (directory-file-name directory) local-file-name) ))                               ;;
;;     ;; Message success to the minibuffer                                                                                            ;;
;;     (org-display-inline-images)                                                                                                     ;;
;;     (ak/download-file url directory local-file-name)))                                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun ak/embark-download-image-at-point-and-insert-org-link (url )
  "Act on image at point, download  to '_downloads' folder
and insert org image block for it"
  ;; (interactive)
  (let* ((url-bnds (bounds-of-thing-at-point 'url))
         (buf (buffer-name))
         (directory (format "_downloads/%s/" (nth 1 (string-split buf "-" t)) "/"))
         (local-file-name (read-from-minibuffer "Enter File Name:" (last (string-split url "/" t) ))))
    (kill-region (car url-bnds) (cdr url-bnds))
    (kill-whole-line)
    (insert(format 
            "#+CAPTION: %s\n#+ATTR_HTML: :alt %s\n#+ATTR_HTML: :width 750px \n#+ATTR_LATEX: :width 0.4\\textwidth \n[[file:%s]]\n"
            ;; local-file-name url (concat directory local-file-name) ))
            local-file-name url (file-name-concat (directory-file-name directory) local-file-name) ))
    ;; Message success to the minibuffer
    (ak/download-file url directory local-file-name)
    (with-current-buffer buf
      (call-interactively 'org-redisplay-inline-images))))

(with-eval-after-load 'embark
  (define-key embark-org-link-map (kbd "<f12>") #'ak/embark-download-image-at-point-and-insert-org-link))


;;adapted from
;;https://emacs.stackexchange.com/questions/3981/how-to-copy-links-out-of-org-mode
(defun ak/extract-host-from-url-at-point (&optional arg)
  (interactive)
  (let* ((link (org-element-lineage (org-element-context) '(link) t))
          (type (org-element-property :type link))
          (url (org-element-property :path link))
          (url (concat type ":" url)) ;;need this, as otherwise the path retrieves the portion after https:// only
          (host (url-host (url-generic-parse-url url))))
         ;; (host (url-host (url-generic-parse-url url-at-point)))) ;;this would work if it was not an org link
    (message "%s" host)))


;;;###autoload
(defun ak/export-org-to-clipboard-as-rtf ()
  "Export org buffer to HTML, and copy it to the clipboard as rtf.
Requires pandoc"
  (interactive)
  (save-window-excursion
    (let* ((pandoc-command "pandoc --embed-resource --standalone --from=html --to=rtf")
           (rtf-clip-command nil)
           (org-export-show-temporary-export-buffer nil)
           (buf "*Org HTML Export*")
           (html nil))
      (org-html-export-as-html)
      (setq html (with-current-buffer buf (buffer-string)))
           ;;  (buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
           ;; (html (with-current-buffer buf (buffer-string))))
      (cond
       (ak/generic-mac-p 
        (if (not (executable-find "pandoc")) 
            (error "pandoc executable not found"))
        (setq rtf-clip-command "pbcopy --Prefer rtf")
        (with-current-buffer buf
          (shell-command-on-region
           (point-min)
           (point-max)
           (concat pandoc-command "|" rtf-clip-command))))
       (ak/generic-windows-p
        (if (not (executable-find "pandoc")) 
            (error "pandoc executable not found"))
        (setq rtf-clip-command "powershell -command Set-Clipboard -AsHtml")
        (with-current-buffer buf
          (shell-command-on-region
           (point-min)
           (point-max)
           (concat pandoc-command "|" rtf-clip-command))))
       (t ;;;generic linux station. requires xclip. 
        (if (not (executable-find "xclip")) 
            (error "xclip executable not found. Linux requires it!"))
        (setq rtf-clip-command "xclip -verbose -i \"%f\" -t text/html -selection clipboard")
        (let* ((tmpfile (make-temp-file "org-rtf-clip-" nil ".html" html))
               (proc (apply 
                      'start-process "org-rtf-clip" "*org-rtf-clip*" 
                      (split-string-and-unquote
                       (format-spec rtf-clip-command
                                    `((?f . ,tmpfile))) " "))))
          (set-process-query-on-exit-flag proc nil))))
      (kill-buffer buf))))

(defvar ak/org-roam-buffer-actions-alist '((?1 "Set Author\n" ak/set-author-property)
                                           (?2 "Create Org-roam entry\n" 
                                               (lambda() 
                                                 (interactive)
                                                 (org-id-get-create)
                                                 (call-interactively 'org-set-property)))
                                           (?3 "Copy Org as Rich Text\n"  ak/export-org-to-clipboard-as-rtf)
                                           (?4 "Insert clipboard as org\n" 
                                               (lambda () 
                                                 (interactive)
                                                 (ak/insert-org-from-html-clipboard)
                                                 (org-web-tools--clean-pandoc-output)))
                                           (?5 "Clean base64 artefacts\n" ak/delete-image-base-64-data-lines)
                                           (?6 "Clean pandoc vestiges from buffer\n" 
                                               (lambda () 
                                                 (interactive)
                                                 (org-web-tools--clean-pandoc-output)))
                                           (?7 "Assign parent url to site property\n" 
                                               (lambda () 
                                                 (interactive)
                                                 (ak/org-extract-top-level-domain-into-property-drawer)))
                                           (?9 "Add nytimes articles to org-roam" 
                                               (lambda ()
                                                 (interactive)
                                                 (ak/process-input-org-dir-for-roam "~/Dropbox/org-files/nytimes/"))))
  "List that associates numbers to common actions that can be taken on an org-roam buffer.")

(defun ak/org-roam-buffer-actions-choose ()
  "Choose action to take on org-roam buffer.
Returns whatever the action returns."
  (interactive)
  (let ((choice (read-char-choice (mapconcat (lambda (item) (format "%c: %s" (car item) (cadr item))) ak/org-roam-buffer-actions-alist "; ")
                  (mapcar #'car ak/org-roam-buffer-actions-alist))))
    (funcall (nth 2 (assoc choice ak/org-roam-buffer-actions-alist)))))

;; (define-key ak-map "3" '("Org roam entry creation from heading" . (lambda() 
;;                                                        (interactive)
;;                                                        (org-id-get-create)
;;                                                        (call-interactively 'org-set-property))))

(define-key ak-map "3" '("org-roam Buffer Actions" . ak/org-roam-buffer-actions-choose))

(provide 'init-org-settings)
