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

      org-agenda-files ak/my-org-file-location

      org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
        (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)"  "|" "COMPLETED(c)" "CANC(k@)"))

      org-src-window-setup 'current-window)



(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'yas-minor-mode)
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
   ))



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
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode))

(provide 'init-org-settings)
