;;; -*- lexical-binding: t; -*-
(require 'init-org-settings)

(use-package org-roam
  :ensure t 
  ;; :after org-web-tools
  :init
  ;; (declare-function org-web-tools--get-url "org-web-tools")
  (setq org-roam-v2-ack t)
  ;; org-roam-database-connector 'sqlite-module)
  (setq org-roam-node-display-template
        (cond
         (ak/my-pi-p ;;not as wide a display
          (concat "${title:60} "
                  (propertize "${author:25}" 'face 'org-meta-line)
                  (propertize "${tags:20}" 'face 'org-tag)))
         (t 
          (concat "${title:85} "
                  (propertize "${author:25} " 'face 'org-meta-line)
                  ;; (propertize "${category:20}" 'face 'org-tag)
                  (propertize "${tags:30}" 'face 'org-tag)))))
  :custom
  ;; (org-roam-database-connector 'sqlite-builtin)
  (org-roam-directory ak/my-org-file-location)
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+SETUPFILE: custom-css/imagine-css.org\n#+filetags:\n#+DATE: %U\n")
      :unnarrowed t)

     ;; ("s" "test" plain "%?"
     ;;  :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+SETUPFILE: custom-css/org-email-head-css.org\n#+category:test\n#+filetags: web\n")
     ;;  :unnarrowed t)

     ("w" "articles" plain "%?"
      :target (file+head "articles/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+SETUPFILE: custom-css/org-email-head-css.org\n#+filetags: web\n#+DATE: %U\n")
      :unnarrowed t)

     ("f" "fiction" plain "%?"
      :target (file+head "lit/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+SETUPFILE: custom-css/subtle-elegance-css.org\n#+filetags: fiction\n#+DATE: %U\n")
      :unnarrowed t)

     ("r" "recipe" plain "%?"
      :target (file+head "recipes/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+SETUPFILE: custom-css/imagine-css.org\n#+filetags: recipe\n#+DATE: %U\n")
      :unnarrowed t)

      ("b" "book notes" plain "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
       :target (file+head "personal/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Book\n#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"~/.emacs.d/custom-css/org-email-head.css\" />\n#+OPTIONS: toc:nil num:nil")
       :unnarrowed t)

      ("t" "tech notes" plain "%?"
       :target (file+head "tech-notes/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+SETUPFILE: custom-css/imagine-css.org\n#+filetags: tech\n#+DATE: %U\n")
       :unnarrowed t)

      ("p" "project" plain "\n* Goals\n\n%?\n\n* Tasks\n\n** TODO Add Initial Tasks\n\n* Dates\n\n"
       :target (file+head "work/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project\n#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"~/.emacs.d/custom-css/org-email-head.css\" />\n#+OPTIONS: toc:nil num:nil")
       :unnarrowed t)

      ("n" "random notes" plain "\n* Thought\n\n%?\n\n** Context\n\n** Prompted By\n\n"
       :target (file+head "personal/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+SETUPFILE: custom-css/imagine-css.org\n#+filetags: Musings\n")
       :unnarrowed t)))

  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n a" . org-roam-alias-add)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n r" . org-roam-tag-remove)
         ("C-c n o" . org-id-get-create)
         ("C-c n r" . org-roam-tag-remove)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map ak-map
         ("<f1>" . org-roam-node-find)
         ("<f2>" . ak/clip-web-page-title-and-search-org-roam) 
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode)
  (cl-defmethod org-roam-node-author ((node org-roam-node))
    "Return the currently set author for the NODE."
    (cdr (assoc-string "AUTHOR" (org-roam-node-properties node)))))

;; (with-eval-after-load 'org-web-tools
  (defun ak/embark-clip-web-page-title-and-search-org-roam (url)
    "Parses web page's title from url and searches org-roam.
Provides an embark action to capture urls in org-roam from url/org-link at point"
    ;; (interactive )
    (let* ((html (org-web-tools--get-url url))
           (title (org-web-tools--html-title html)))
      (kill-new url)
      (org-roam-node-find nil title nil nil 
                          :templates 
                          '(("w" "web" plain "%?"
                             :target (file+head "articles/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+SETUPFILE: custom-css/org-email-head-css.org\n#+filetags: web\n#+DATE: %U\n")
                             :unnarrowed t)))))
;;;###autoload  
  (cl-defun ak/clip-web-page-title-and-search-org-roam(&optional (url (org-web-tools--get-first-url)))
      "Parses web page's title from url in clipboard and searches org-roam"
      (interactive)
      (require 'org-web-tools)
      (let* ((html (org-web-tools--get-url url))
             (title (org-web-tools--html-title html)))
        (org-roam-node-find nil title)))

(with-eval-after-load 'embark
  (define-key embark-url-map (kbd "<f2>") #'ak/embark-clip-web-page-title-and-search-org-roam)
  (define-key embark-org-link-map (kbd "<f2>") #'ak/embark-clip-web-page-title-and-search-org-roam))

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

(provide 'org-roam-settings)
