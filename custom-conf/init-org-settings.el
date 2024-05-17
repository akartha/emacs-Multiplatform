;;; -*- lexical-binding: t; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Org Common settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)

(defvar ak/my-org-file-location nil)

(add-to-list 'org-modules 'org-habit t)

(define-key global-map (kbd "C-c a") '("Org agenda" . org-agenda))
(define-key global-map (kbd "C-c l") '("Org store link" . org-store-link))
        



(defun ak/next-entry-or-next-visible-header ()
  (interactive)
  (condition-case err
      (org-next-item)
    (error (org-next-visible-heading 1))))
;; (define-key org-mode-map (kbd "C-c C-n") #'ag/next-entry-or-next-visible-header)

(defun ak/previous-entry-or-previous-visible-header ()
  (interactive)
  (condition-case err
      (org-previous-item)
    (error (org-previous-visible-heading 1))))
;; (define-key org-mode-map (kbd "C-c C-p") #'ag/previous-entry-or-previous-visible-header)


;; (define-key ak-map "p" '("Previous heading" . org-previous-visible-heading))
;; (define-key ak-map "P" '("Next heading" . org-next-visible-heading))
                  
(define-key ak-map "p" '("Previous item/heading" . ak/previous-entry-or-previous-visible-header))
(define-key ak-map "n" '("Next item/heading" . ak/next-entry-or-next-visible-header))    
                  
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


(setq org-directory ak/my-org-file-location
      org-agenda-files (mapcar #'(lambda(s) (expand-file-name s ak/my-org-file-location)) 
                               (list "agenda/Trips.org"
                                     "agenda/Tasks.org"
                                     "agenda/Schedule.org"))
      org-use-speed-commands t)

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
  ;;(setq org-reveal-hlevel 2)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ;;    Change variable org-reveal-hlevel’s value to set HLevel globally.                                                             
  ;; ;; Setting Org files local HLevel to option REVEAL_HLEVEL. #+REVEAL_HLEVEL: 2                                                        ;;
  ;; ;; org-reveal-title-slide nil)                                                                                                       ;;
  ;; ;;   To avoid a title slide, please set variable org-reveal-title-slide to nil, or add reveal_title_slide:nil to #+OPTIONS: line.    ;;
  ;; ;; To restore the default title slide, please set variable org-reveal-title-slide to ~’auto~                                         ;;
  ;; ;;   Customize the Title Slide                                                                                                       ;;
  ;;                                                                                                                                      ;;
  ;; ;; There are 3 ways to customize the title slide.                                                                                    ;;
  ;;                                                                                                                                      ;;
  ;; ;;     Set variable org-reveal-title-slide to a string of HTML markups.                                                              ;;
  ;; ;;     Set reveal_title_slide in the #+OPTIONS: line to a string of HTML markups.                                                    ;;
  ;; ;;     Use one or more option lines #+REVEAL_TITLE_SLIDE: to specify the HTML of the title slide.                                    ;;
  ;;                                                                                                                                      ;;
  ;; ;; The following escaping characters can be used to retrieve document information:                                                   ;;
  ;; ;; %t	Title                                                                                                                         ;;
  ;; ;; %s	Subtitle                                                                                                                      ;;
  ;; ;; %a	Author                                                                                                                        ;;
  ;; ;; %e	Email                                                                                                                         ;;
  ;; ;; %d	Date                                                                                                                          ;;
  ;; ;; %%	Literal %                                                                                                                     ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(require 'ob-passthrough)

(use-package ob-go
  :ensure t
  :after go)

(use-package restclient
  :ensure t)

(use-package ob-restclient
  :ensure t
  :after restclient)

(use-package plantuml-mode
  :ensure t)

(use-package verb
  :ensure t
  :demand t
  :mode ("\\.org\\'" . org-mode)
  :config 
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Org Babel languages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-babel
  :no-require
  :after (org ob-go restclient ob-restclient ob-mermaid verb plantuml-mode)
  :config 
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp .t)
     (python . t)
     (R . t)
     (restclient . t)
     (sql . t)
     ;; ;;    https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-sql.html ;;
     (sqlite . t)
     (C . t)
     (awk . t)
     (go . t)
     ;;https://github.com/ljos/jq-mode
     (jq . t)
     ;;https://github.com/arnm/ob-mermaid
     (mermaid . t)
     (verb . t)
     (js . t)
     (passthrough . t)
     (shell . t)
     (latex . t)
     (plantuml . t))))

(setq org-plantuml-jar-path "~/plantuml.jar")


;;;;;;;;;;;;;;;;;
;; ** Org Roam ;;
;;;;;;;;;;;;;;;;;

(use-package org-roam
  :ensure t 
  ;; :straight t
  :init
  (setq org-roam-v2-ack t)
  ;; org-roam-database-connector 'sqlite-module)
  :custom
  ;; (org-roam-database-connector 'sqlite-builtin)
  (org-roam-directory ak/my-org-file-location)
  (org-roam-completion-everywhere t)
  (org-roam-node-display-template (concat "${title:85} "
                                          (propertize "${tags:*}" 'face 'org-tag)))
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+SETUPFILE: custom-css/imagine-css.org\n#+filetags:")
      :unnarrowed t)

     ("w" "web" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+SETUPFILE: custom-css/org-email-head-css.org\n#+category:web article\n#+filetags: web\n")
      :unnarrowed t)

     ("f" "fiction" plain "%?"
      ;; :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: fiction\n#+filetags: fiction\n#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"~/.emacs.d/custom-css/org-email-head.css\" />\n#+OPTIONS: toc:nil num:nil")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+SETUPFILE: custom-css/imagine-css.org\n#+category: fiction\n#+filetags: fiction")
      :unnarrowed t)

     ("r" "recipe" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+SETUPFILE: custom-css/imagine-css.org\n#+category: recipe\n#+filetags: recipe")
      :unnarrowed t)

      ("b" "book notes" plain "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
       :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Book\n#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"~/.emacs.d/custom-css/org-email-head.css\" />\n#+OPTIONS: toc:nil num:nil")
       :unnarrowed t)

      ("p" "project" plain "\n* Goals\n\n%?\n\n* Tasks\n\n** TODO Add Initial Tasks\n\n* Dates\n\n"
       :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project\n#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"~/.emacs.d/custom-css/org-email-head.css\" />\n#+OPTIONS: toc:nil num:nil")
       :unnarrowed t)

      ("t" "random thoughts" plain "\n* Thought\n\n%?\n\n** Context\n\n** Prompted By\n\n"
       :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+SETUPFILE: custom-css/imagine-css.org\n#+filetags: Musings\n")
       :unnarrowed t)))

      ;; ("m" "Movie/Series notes" plain "\n* Source\n- Title: %^{Title}\n- Director: %^{Director}\n- Year: %^{Year}\n- Watched?: %^{Prompt|Watched|Want to watch|Want to avoid}\n** Summary\n%^C\n\n%?"
       ;; :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Movie Series\n#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"~/.emacs.d/custom-css/org-email-head.css\" />\n#+OPTIONS: toc:nil num:nil")
       ;; :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n a" . org-roam-alias-add)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n o" . org-id-get-create)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map ak-map
         ("<f1>" . org-roam-node-find)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
    :ensure t
    :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))


(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      '(("t" "Todo" entry 
         (file+headline "agenda/Tasks.org" "Captured Tasks")
         "* TODO %?\n DEADLINE:%^t  %i\n  %a")
        ("s" "Schedule" entry 
         (file+headline  "agenda/Schedule.org" "Add to Schedule")
          ;; (file+headline  ,(file-truename "agenda/Schedule.org") "Add to Schedule")
         "* Appointment %?\n SCHEDULED:%^T\n%i")
        ("j" "Journal")
        ("ja" "Journal - General" entry 
         (file+headline "agenda/jrnl.org" "General")
         "* %^{Capture}\n :PROPERTIES:\n :CAPTURED: %U\n :END: \n\n %?")
        ("jw" "Journal - Work" entry 
         (file+headline "agenda/jrnl.org" "Work")
         "* %^{Capture}\n :PROPERTIES:\n :CAPTURED: %U\n :END: \n\n %?")
        ("jp" "Journal - Private" entry 
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
  :bind (:map ak-map
              ;; ("<f2>" . ak/get-url-title)
              ("<f2>" . ak/clip-web-page-title-and-search-org-roam)
              ("<f3>" . org-web-tools-insert-web-page-as-entry)
              ("<f6>" . org-web-tools-read-url-as-org)
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

  (cl-defun ak/clip-web-page-title-and-search-org-roam(&optional (url (org-web-tools--get-first-url)))
    "Parses web page's title from url in clipboard and searches org-roam"
    (interactive)
    (let* ((html (org-web-tools--get-url url))
           (title (org-web-tools--html-title html)))
      (org-roam-node-find nil title)))

(defun ak/delete-image-base-64-data-lines ()
  "Deletes lines containing base-64 image data from the buffer."
 (interactive)
  (save-excursion
    (let ((kill-patterns '(;;"\\[\\[data:image/svg\\+xml;base64,[^]]*\\]\\]"
                           "\\[\\[data:image/svg[^]]*\\]\\]"
                           "\\[\\[data:image/png[^]]*\\]\\]"
                           "\\[\\[data:image/gif;base64,[^]]*\\]\\]")))
      (dolist (kill-pattern kill-patterns)

        (goto-char (point-min))
        (while (re-search-forward kill-pattern nil t)
          (beginning-of-line)
          (kill-line)))))))

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
    (make-directory (concat default-directory directory) t)
    (cond ((or ak/my-framework-p ak/my-pi-p)
           (shell-command (format linux-shell-clip-command (shell-quote-argument (concat default-directory  directory "/" filename )))))
          (ak/generic-windows-p
           (shell-command (format windows-shell-clip-command (shell-quote-argument (concat default-directory  directory "/"  filename)))))
          (ak/my-mac-p
           (shell-command (format mac-shell-clip-command (shell-quote-argument (concat default-directory  directory "/" filename))))))
    ;; Insert formatted link at point
    (save-excursion 
      (insert(format 
              "#+CAPTION: %s\n#+ATTR_HTML: :alt %s\n#+attr_html: :width 750px \n#+attr_latex: :width 0.4\\textwidth \n[[file:%s.png]]"
              caption caption (concat directory "/" filename))))
    ;; Message success to the minibuffer
    (message "saved to %s as %s.png" directory filename))
  (org-display-inline-images))

(define-key ak-map "v" '("Save clipboard image as org" . ak/my-insert-clipboard-png))

(use-package ox-pandoc
  :demand t
  :ensure t)

(use-package org-margin 
  :ensure (:type git :host github :repo "rougier/org-margin"))

(provide 'init-org-settings)
