;;; -*- lexical-binding: t; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Org Common settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

                          
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

      org-src-window-setup 'current-window)

(custom-set-variables
 '(org-directory ak/my-org-file-location)
 '(org-agenda-files (list (concat org-directory "/agenda"))))

(add-hook 'org-mode-hook 'org-indent-mode)
;; (add-hook 'org-mode-hook 'yas-minor-mode)
(add-hook 'org-mode-hook 'abbrev-mode)
;;(add-hook 'org-mode-hook #'org-modern-mode)

(add-hook 'org-mode-hook
          (lambda ()
            (visual-line-mode 1)))



;;;;;;;;;;;;;;;;;;;;
;; ** Keybindings ;;
;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c '") 'org-edit-src-code)

(use-package org-superstar
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

;; *** Reveal.js export

(use-package ox-reveal
  ;; https://github.com/yjwen/org-reveal
  :config
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
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
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Org Babel languages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ob-passthrough)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (restclient . t)
   (sql . t)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; ;;    https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-sql.html ;;
   ;;                                                                            ;;
   ;; ;; Header Arguments                                                        ;;
   ;; ;; The :colnames header argument defaults to "yes".                        ;;
   ;; ;; There are several SQL-specific header arguments:                        ;;
   ;; ;; :engine                                                                 ;;
   ;; ;;     one of "dbi", "monetdb", "msosql", "mysql", "postgresql";           ;;
   ;; ;; :cmdline                                                                ;;
   ;; ;;     extra command line arguments for the RDBMS executable;              ;;
   ;; ;; :dbhost                                                                 ;;
   ;; ;;     the host name;                                                      ;;
   ;; ;; :dbuser                                                                 ;;
   ;; ;;     the user name;                                                      ;;
   ;; ;; :dbpassword                                                             ;;
   ;; ;;     the user's password;                                                ;;
   ;; ;; :database                                                               ;;
   ;; ;;     the database name;                                                  ;;
   ;; ;; #+name: my-query                                                        ;;
   ;; ;; #+header: :engine mysql                                                 ;;
   ;; ;; #+header: :dbhost host                                                  ;;
   ;; ;; #+header: :dbuser user                                                  ;;
   ;; ;; #+header: :dbpassword pwd                                               ;;
   ;; ;; #+header: :database dbname                                              ;;
   ;; ;; #+begin_src sql                                                         ;;
   ;; ;;   SELECT * FROM mytable                                                 ;;
   ;; ;;   WHERE id > 500                                                        ;;
   ;; ;; #+end_src                                                               ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (sqlite . t)
   (C . t)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; ;;   Example code                        ;;
   ;; ;;   #+begin_src C++ :includes <stdio.h> ;;
   ;; ;;    int a=1;                           ;;
   ;; ;;    int b=1;                           ;;
   ;; ;;    printf("%d\n", a+b);               ;;
   ;; ;;   #+end_src                           ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (awk . t)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; ;; There are three AWK-specific header arguments.                                                                         ;;
   ;; ;; :cmd-line                                                                                                              ;;
   ;; ;;     takes command line arguments to pass to the AWK executable                                                         ;;
   ;; ;; :in-file                                                                                                               ;;
   ;; ;;     takes a path to a file of data to be processed by AWK                                                              ;;
   ;; ;; :stdin                                                                                                                 ;;
   ;; ;;     takes an Org-mode data or code block reference, the value of which will be passed to the AWK process through STDIN ;;
   ;;                                                                                                                           ;;
   ;;                                                                                                                           ;;
   ;; ;;    example code                                                                                                        ;;
   ;; ;;    #+begin_src awk :stdin inventory-shipped :exports results                                                           ;;
   ;; ;;     $1 ~ /J/                                                                                                           ;;
   ;; ;;    #+end_src                                                                                                           ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (go . t)

   ;;https://github.com/ljos/jq-mode
   (jq . t)
   ;;https://github.com/arnm/ob-mermaid
   (mermaid . t)

   (verb . t)
   (js . t)
   
   (passthrough . t)
   (shell . t)
   (plantuml . t)
   ))

(setq org-plantuml-jar-path "~/plantuml.jar")


;;;;;;;;;;;;;;;;;
;; ** Org Roam ;;
;;;;;;;;;;;;;;;;;

(use-package org-roam
  ;; :straight t
  :init
  (setq org-roam-v2-ack t)
  ;; org-roam-database-connector 'sqlite-module)
  :custom
  ;; (org-roam-database-connector 'sqlite-builtin)
  (org-roam-directory ak/my-org-file-location)
  (org-roam-completion-everywhere t)
  (org-roam-node-display-template "${title:55} ${tags:*}")
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags:")
      :unnarrowed t)

     ("w" "web" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category:web article\n#+filetags: web")
      :unnarrowed t)

     ("f" "fiction" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: fiction\n#+filetags: fiction")
      :unnarrowed t)

      ("b" "book notes" plain "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
       :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Book")
       :unnarrowed t)

      ("p" "project" plain "\n* Goals\n\n%?\n\n* Tasks\n\n** TODO Add Initial Tasks\n\n* Dates\n\n"
       :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
       :unnarrowed t)

      ("r" "random thoughts" plain "\n* Thought\n\n%?\n\n** Context\n\n** Prompted By\n\n"
       :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Musings")
       :unnarrowed t)

      ("m" "Movie/Series notes" plain "\n* Source\n- Title: %^{Title}\n- Director: %^{Director}\n- Year: %^{Year}\n- Watched?: %^{Prompt|Watched|Want to watch|Want to avoid}\n** Summary\n%^C\n\n%?"
       :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Movie Series")
       :unnarrowed t)
     ))
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

(defun ak/my-insert-clipboard-png ()
  "Paste image data in clipboard and save it to the (existing or new) '_media' directory
in the current working directory. 

Works on Windows (using built-in powershell command), Mac (using pngpaste - install with brew) and Linux (requires xclip)
Image is saved as png and function inserts an org buffer block with image details."
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

(define-key ak-map "v" '("Save clipboard image as org" ak/my-insert-clipboard-png))

(provide 'init-org-settings)
