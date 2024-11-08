;;; -*- lexical-binding: t; -*-


;; https://flandrew.srht.site/listful/sw-emacs-xht.html
(use-package xht
  :ensure (:type git :host sourcehut :repo "flandrew/xht")
  :commands (global-xht-fontify-mode
             global-xht-do-mode
             xht-fontify-mode
             xht-do-mode
             xht-see-readme)
  :config
  (global-xht-fontify-mode)
  (global-xht-do-mode))


;;;###autoload
(defun ak/dired-id3vtag ()
  "For an audiobook directory in the format '~/downloads/author- title/', this code
will update the id3 tags associated with the audio files in it. 
TODO - No error checking implemented yet"
  (interactive)
  (let* ((directory dired-directory)
         (files (directory-files dired-directory t directory-files-no-dot-files-regexp))
         (directory-name (s-chop-suffix "/" (s-chop-prefix "~/downloads/" directory)))
         (author-title (split-string directory-name " - " ))
         (author (string-trim (nth  0 author-title) "\s+"))
         (title (string-trim (nth 1 author-title) "\s+" ))
         (number-of-files (length files))
         (command-string (format "id3tag -2 --artist=\"%s\" --album=\"%s\" --genre=\"audiobook\" --total=%d --track=" author title number-of-files))
         (command-string-track))

    (dolist (file files)
      (if (f-file? file)
          (progn (setq command-string-track (format "%s%s \"%s\"" command-string (f-base file) file))
                 (shell-command command-string-track)
                 ;; (message command-string-track)
                 )))))




(setq auto-save-default nil)
;; Auto-saving does not cooperate with org-crypt.el: so you need to
;; turn it off if you plan to use org-crypt.el quite often.  Otherwise,
;; you'll get an (annoying) message each time you start Org.

;; To turn it off only locally, you can insert this:
;;
;; # -*- buffer-auto-save-file-name: nil; -*-

(use-package nov
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  ;; (defun my-nov-font-setup ()
  ;; (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
  ;;                                          :height 1.5))
  ;; (add-hook 'nov-mode-hook 'my-nov-font-setup)
  ;; (setq nov-text-width 100)
  )

(use-package elfeed 
  :ensure t
  :bind (:map ak-map
              ("<f6>" . elfeed))
  :init 
  (setq-default elfeed-search-filter "@1-month-ago +unread ")
  :config
  (setq elfeed-feeds
        '(("http://nullprogram.com/feed/" blog tech) 
          ("https://planet.emacslife.com/atom.xml" tech emacs)
          ("https://rss.nytimes.com/services/xml/rss/nyt/HomePage.xml" news)
          ("https://rss.nytimes.com/services/xml/rss/nyt/World.xml" news)
          ("https://rss.nytimes.com/services/xml/rss/nyt/Technology.xml" news tech)
          ("http://feeds.bbci.co.uk/news/rss.xml" news world)
          ("http://feeds.bbci.co.uk/news/world/rss.xml" news world)
          ("https://sachachua.com/blog/category/emacs-news/feed/" tech emacs)
          ("https://www.thehindu.com/news/feeder/default.rss" news India)
          ("https://www.newindianexpress.com/States/Kerala/rssfeed/?id=178&getXmlFeed=true" news India Kerala)
          ("https://www.newindianexpress.com/Cities/Bengaluru/rssfeed/?id=182&getXmlFeed=true" news India Bangalore))))

(use-package dwim-shell-command
  :ensure t 
  :bind (([remap shell-command] . dwim-shell-command)
         :map dired-mode-map
         ([remap dired-do-async-shell-command] . dwim-shell-command)
         ([remap dired-do-shell-command] . dwim-shell-command)
         ([remap dired-smart-shell-command] . dwim-shell-command))
  :config
  (require 'dwim-shell-commands)

  (defun dwim-ak/dwim-shell-commands-download-clipboard-audio-url ()
    "Download clipboard URL's audio."
    (interactive)
    (cl-assert (string-match-p "^http[s]?://" (current-kill 0)) nil "Not a URL")
    (dwim-shell-command-on-marked-files
     "Downloading"
     "yt-dlp -N 5 --extract-audio --audio-format mp3 --audio-quality 0 --add-metadata --embed-thumbnail --newline -o \"~/audio/%(title)s.%(ext)s\" \"<<cb>>\""
     :utils "yt-dlp"
     :error-autofocus t
     :monitor-directory "~/audio"
     :silent-success t))

  (defun dwim-ak/dwim-shell-commands-download-clipboard-video-url ()
    "Download clipboard URL's video."
    (interactive)
    (cl-assert (string-match-p "^http[s]?://" (current-kill 0)) nil "Not a URL")
    (dwim-shell-command-on-marked-files
     "Downloading"
     "yt-dlp -N 5 --format \"bv*[ext=mp4]+ba[ext=m4a]/b[ext=mp4]\" --write-subs --embed-subs --sub-lang \"en*\" --newline -o \"~/Downloads/%(title)s.%(ext)s\" \"<<cb>>\""
     :utils "yt-dlp"
     :error-autofocus t
     :monitor-directory "~/Downloads"
     :silent-success t)))

(use-package ace-link
  :ensure t 
  :init
  (ace-link-setup-default)
  ;; :commands (ak/ace-link-org-jump)
  :bind(;; ("C-c o" . ace-link-addr)
        ;; ("C-c L" . ak/ace-link-addr)
        :map org-mode-map 
        ( "C-c o" . ace-link-org)
        ( "C-c L" . ak/ace-link-org-jump)
        :map avy-custom-keymap
        ("u" . ak/ace-link-org-jump)
        ("o" . ace-link-org))
  :config 
  (defun ak/ace-link-org-jump ()
    "Jump to a visible link in an `org-mode' buffer."
    (interactive)
    (require 'org)
    (let ((pt (avy-with ak/ace-link-org-jump
                (avy-process
                 (mapcar #'cdr (ace-link--org-collect))
                 (avy--style-fn avy-style)))))
      (ak/pt--jump pt)))

  (defun ak/ace-link-addr ()
    "Open a visible link in a goto-address buffer."
    (interactive)
    (let ((pt (avy-with ak/ace-link-addr
                (avy-process
                 (ace-link--addr-collect)
                 (avy--style-fn avy-style)))))
      (ak/pt--jump pt)))

  (defun ak/pt--jump (pt)
    (when (numberp pt)
      (goto-char pt))))


(defun my-org-link-as-url (link)
  "Return the final URL for LINK."
  (dom-attr
   (dom-by-tag
    (with-temp-buffer
      (insert (org-export-string-as link 'html t))
      (xml-parse-region (point-min) (point-max)))
    'a)
   'href))

(defun my-org-stored-link-as-url (&optional link insert)
  "Copy the stored link as a plain URL.
If LINK is specified, use that instead."
  (interactive (list nil current-prefix-arg))
  (setq link (or link (caar org-stored-links)))
  (let ((url (if link
                 (my-org-link-as-url link)
               (error "No stored link"))))
    (when (called-interactively-p 'any)
      (if url
          (if insert (insert url) (kill-new url))
        (error "Could not find URL.")))
    url))


(defun ak/my-org-link-qr (url)
  "Display a QR code for URL in a buffer."
  (let ((buf (save-window-excursion (qrencode--encode-to-buffer (my-org-stored-link-as-url url)))))
    (display-buffer-in-side-window buf '((side . right)))))

(use-package qrencode
  :ensure t
  :after embark-org 
  :config
  (with-eval-after-load 'embark
    (define-key embark-org-link-map (kbd "q") #'ak/my-org-link-qr)))

(define-prefix-command 'ak-denote-map)
(global-set-key (kbd "` t") '("Denote commands" . ak-denote-map))

(use-package denote
  :ensure t
  :hook
  (dired-mode . denote-dired-mode)
  :bind (:map ak-denote-map ("c" . 'denote)
              ("k" . 'denote-keywords-add)
              ("K" . 'denote-keywords-remove)
              ("r" . 'denote-rename-file)
              ("l" . 'denote-link)
              ("b" . 'denote-backlinks)
              ("j" . 'ak/denote-journal))
              ;; ("w" . 'ak/denote-clip-url))
  :custom
  (denote-known-keywords '("journal" "recipe" "article" "story" "musings" "work" "study"))
  :preface
  (defun ak/denote-journal ()
    "Create an entry tagged journal with the date as its title."
    (interactive)
    (denote
     (format-time-string "%A %e %B %Y") 
     '("journal")))
  :init
  (setq denote-directory "~/Dropbox/Documents/Denote")
  ;; (defun ak/denote-clip-url()
  ;;   "Create denote with title from url"
  ;;   (interactive)
  ;;   (let* ((web-title 'ak/get-url-title))
  ;;     (denote 
  ;;      'web-title
  ;;      '("article"))))
)


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
                           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+SETUPFILE: custom-css/org-email-head-css.org\n#+filetags: web\n")
                           :unnarrowed t)))))

(with-eval-after-load 'embark
  (define-key embark-url-map (kbd "<f2>") #'ak/embark-clip-web-page-title-and-search-org-roam)
  (define-key embark-org-link-map (kbd "<f2>") #'ak/embark-clip-web-page-title-and-search-org-roam))

;;;; Run commands in a popup frame
;;;;https://protesilaos.com/codelog/2024-09-19-emacs-command-popup-frame-emacsclient/

(defun prot-window-delete-popup-frame (&rest _)
  "Kill selected selected frame if it has parameter `prot-window-popup-frame'.
Use this function via a hook."
  (when (frame-parameter nil 'prot-window-popup-frame)
    (delete-frame)))

(defmacro prot-window-define-with-x-popup-frame (command)
  "Define interactive function which calls COMMAND in a new frame.
Make the new frame have the `prot-window-popup-frame' parameter."
  `(defun ,(intern (format "prot-window-popup-%s" command)) ()
     ,(format "Run `%s' in a popup frame with `prot-window-popup-frame' parameter.
Also see `prot-window-delete-popup-frame'." command)
     (interactive)
     (let ((frame (make-frame '((prot-window-popup-frame . t) (window-system . x)))))
       (select-frame frame)
       (switch-to-buffer " prot-window-hidden-buffer-for-popup-frame")
       (condition-case nil
           (call-interactively ',command)
         ((quit error user-error)
          (delete-frame frame))))))

(defmacro prot-window-define-with-nonx-popup-frame (command)
  "Define interactive function which calls COMMAND in a new frame.
Make the new frame have the `prot-window-popup-frame' parameter."
  `(defun ,(intern (format "prot-window-popup-%s" command)) ()
     ,(format "Run `%s' in a popup frame with `prot-window-popup-frame' parameter.
Also see `prot-window-delete-popup-frame'." command)
     (interactive)
     (let ((frame (make-frame '((prot-window-popup-frame . t) ))))
       (select-frame frame)
       (switch-to-buffer " prot-window-hidden-buffer-for-popup-frame")
       (condition-case nil
           (call-interactively ',command)
         ((quit error user-error)
          (delete-frame frame))))))


(declare-function org-capture "org-capture" (&optional goto keys))
(defvar org-capture-after-finalize-hook)

(add-hook 'org-capture-after-finalize-hook #'prot-window-delete-popup-frame)

(if ak/my-framework-p 
    (progn 
      (prot-window-define-with-x-popup-frame ak/clip-web-page-title-and-search-org-roam)
      (prot-window-define-with-x-popup-frame org-capture))
  (prot-window-define-with-nonx-popup-frame ak/clip-web-page-title-and-search-org-roam)
  (prot-window-define-with-nonx-popup-frame org-capture))
  

(provide 'non-core)




