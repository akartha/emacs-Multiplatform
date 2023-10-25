;;; -*- lexical-binding: t; -*-



;; (define-prefix-command 'ak-emms-map)
;; (global-set-key (kbd "` p") 'ak-emms-map)

;; (use-package emms-setup
;;   :when ak/my-framework-p
;;   :init
;;   (add-hook 'emms-player-started-hook 'emms-show)
;;   (setq emms-show-format "Playing: %s")
;;   :config
;;   (emms-all)
;;   (setq emms-player-list '(emms-player-mpv)
;;         emms-info-functions '(emms-info-native))

;;   (defun fg-emms-track-description (track)
;;     "Return a somewhat nice track description."
;;     (let ((artist (emms-track-get track 'info-artist))
;;           (year (emms-track-get track 'info-year))
;;           (album (emms-track-get track 'info-album))
;;           (tracknumber (emms-track-get track 'info-tracknumber))
;;           (title (emms-track-get track 'info-title)))
;;       (cond
;;        ((or artist title)
;;         (concat (if (> (length artist) 0) artist "Unknown artist") " - "
;;                 (if (> (length year) 0) year "XXXX") " - "
;;                 (if (> (length album) 0) album "Unknown album") " - "
;;                 (if (> (length tracknumber) 0)
;;                     (format "%02d" (string-to-number tracknumber))
;;                   "XX") " - "
;;                 (if (> (length title) 0) title "Unknown title")))
;;        (t (emms-track-simple-description track)))))

;;   (setq emms-track-description-function 'fg-emms-track-description)
;;   :bind
;;   (:map ak-emms-map
;;   ("p" . emms-pause)
;;   ("s" . emms-stop)
;;   ("b" . emms-browser)
;;   ("=" . emms-bookmarks-add)
;;   ("{" . emms-bookmarks-prev)
;;   ("}" . emms-bookmarks-next)
;;   ("<" . (lambda () (interactive) (emms-seek -30)))
;;   (">" . (lambda () (interactive) (emms-seek 30)))
;;   ("[" . emms-seek-backward)
;;   ("]" . emms-seek-forward)))

;; (eval-after-load 'emms '(emms-state-mode))


;; (require 'key-chord)
;; (key-chord-mode t)

;; (use-package key-chord
;;   :custom
;;   (key-chord-one-key-delay 0.05)
;;   (key-chord-two-keys-delay 0.10)
;;   ;;; A 2023 release caused problems and delays. See
;;   ;;; - https://github.com/emacsorphanage/key-chord/issues/6
;;   ;;; - https://github.com/emacsorphanage/key-chord/issues/7
;;   (key-chord-safety-interval-forward 0.1)
;;   (key-chord-safety-interval-backward 0)
;;   (key-chord-safety-interval-forward 0)
;;   :config 
;;   (key-chord-define-global "  " 'execute-extended-command)
;;   (key-chord-define-global "aa" 'move-beginning-of-line)
;;   (key-chord-define-global "''" 'move-end-of-line)

;;   :init
;;   ;; (use-package use-package-chords)

;;   (key-chord-mode t))

;; https://flandrew.srht.site/listful/sw-emacs-xht.html
(use-package xht
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
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  ;; (defun my-nov-font-setup ()
  ;; (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
  ;;                                          :height 1.5))
  ;; (add-hook 'nov-mode-hook 'my-nov-font-setup)
  ;; (setq nov-text-width 100)
  )

(use-package elfeed 
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

(provide 'non-core)

;; Graveyard

;; ** Line numbers 

;; (use-package linum-relative
;;   :diminish
;;   :straight t
;;   :config
;;   (setq linum-relative-current-symbol "")
;;   (add-hook 'prog-mode-hook 'linum-relative-mode)) ;;don't want it global

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Zapping to char
;; A nifty little package that kills all text between your cursor and
;; a selected character.  ;; If you wish to include the selected
;; character in the killed region, change =zzz-up-to-char= to
;; =zzz-to-char=. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   (use-package zzz-to-char
;;     :straight t
;;     :bind ("M-z" . zzz-up-to-char))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; * Editing with sudo      
;; ;; Pretty self-explanatory. 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  (use-package sudo-edit
;;    :straight t
;;    :bind
;;      ("s-e" . sudo-edit))



;;;;;;;;;;;;;;;;;;
;; Centaur Tabs ;;
;;;;;;;;;;;;;;;;;;

;; (use-package centaur-tabs
;;   :straight t
;;   :demand
;;   :init
;;   (setq centaur-tabs-enable-key-bindings t)
;;   :config
;;   (centaur-tabs-mode t)
;;   (setq centaur-tabs-set-modified-marker t
;;         centaur-tabs-height 24
;;         centaur-tab-style "slant"
;;         centaur-tabs-set-icons t
;;         centaur-tabs-gray-out-icons 'buffer
;;         uniquify-separator "/"
;;         uniquify-buffer-name-style 'forward
;;         centaur-tabs-show-count t ))
;;   ;; :bind
;;   ;; ("C-<prior>" . centaur-tabs-backward)
;;   ;; ("C-<next>" . centaur-tabs-forward))

;;;;;;;;;;;;
;; ** EKG ;;
;;;;;;;;;;;;

;; (define-prefix-command 'ekg-custom-keymap)
;; (global-set-key (kbd "` e") 'ekg-custom-keymap)

;; (use-package ekg
;;   :straight t
;;   :bind
;;   (:map ekg-custom-keymap
;;         ("c" . ekg-capture)
;;         ("a" . ekg-show-notes-with-tag)
;;         ("u" . ekg-capture-url)
;;         ("b" . ekg-browse-url)
;;         ("t" . ekg-show-notes-for-today)
;;         ("m" . ekg-show-notes-latest-modified)
;;         ("p" . ekg-show-notes-latest-captured)
;;         ("T" . ekg-show-notes-in-trash)
;;         :map ekg-notes-mode-map
;;         ("q" . kill-current-buffer))
  
;;   :config
;;   (require 'emacsql-sqlite))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Modus Theme customizations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq modus-themes-completions
;;       '((matches . (extrabold underline))
;;         (selection . (extrabold italic underline))))
;; (setq modus-themes-org-blocks 'tinted-background)
;; (setq modus-themes-prompts '(extrabold italic))

;; These overrides are common to all Modus themes.  We also provide
;; theme-specific options, such as `modus-operandi-palette-overrides'.
;;
;; In general, the theme-specific overrides are better for overriding
;; color values, such as redefining what `blue-faint' looks like.  The
;; common overrides are best used for changes to semantic color
;; mappings, as we show below.

;; Keep the background unspecified (like the default), but use a faint
;; foreground color.
;; (setq modus-themes-common-palette-overrides
;;       '((fg-prompt cyan-faint)
;;         (bg-prompt unspecified)))

;; ;; Add a nuanced background to prompts that complements their foreground.
;; (setq modus-themes-common-palette-overrides
;;       '((fg-prompt cyan)
;;         (bg-prompt bg-blue-nuanced)))

;; ;; Add a yellow background and adjust the foreground accordingly.
;; (setq modus-themes-common-palette-overrides
;;       '((fg-prompt fg-main)
;;         (bg-prompt bg-yellow-nuanced) ; try to replace "nuanced" or "subtle" with "intense"

;;         (comment yellow-faint)
;;         (string green-warmer)

;;         (fg-heading-1 blue-warmer)
;;         (bg-heading-1 bg-blue-nuanced)
;;         (overline-heading-1 blue)

;;         (fg-heading-2 fg-main)
;;         (bg-heading-2 bg-dim)
;;         (overline-heading-2 border)))


;;;;;;;;;;;;;;;;;;;;
;; ** Avy customs ;;
;;;;;;;;;;;;;;;;;;;;

;; (defun avy-goto-parens ()
;;   (interactive)
;;   (let ((avy-command this-command))   ; for look up in avy-orders-alist
;;     (avy-jump "(+")))
;; (add-to-list 'avy-orders-alist '(avy-goto-parens . avy-order-closest))
;; ;;(global-define-key (kbd "s-p") 'avy-goto-parens)
;; (define-key ak-map "(" 'avy-goto-parens)

;; (defun avy-org-same-level (&optional all)
;;   "Go to any org heading of the same level as the current one.
;; By default, choices are limited to headings under common
;; subheading, but if called with a prefix argument, will be
;; buffer-global."
;;   (interactive "P")
;;   (let ((org-level (org-current-level)))
;;     (avy--generic-jump
;;      (format "^%s "
;;              (regexp-quote
;;               (make-string org-level ?*)))
;;      nil
;;      'pre
;;      (unless (or all (= org-level 1))
;;        (save-excursion
;;          (outline-up-heading 1)
;;          (point)))
;;      (unless (or all (= org-level 1))
;;        (save-excursion
;;          (outline-up-heading 1)
;;          (org-end-of-subtree))))))

;; (defun avy-org-parent-level (&optional all)
;;   "Go to any org heading one level above the current one.

;; By default, choices are limited to headings under common
;; subheading, but if called with a prefix argument, will be
;; buffer-global."
;;   (interactive "P")
;;   (let ((org-level (org-current-level)))
;;     (if (= org-level 1)
;;         (message "Already at top level.")
;;       (avy--generic-jump (format "^%s " (regexp-quote (make-string (- org-level 1) ?*)))
;;                          nil 'pre (unless (or all (= org-level 2))
;;                                     (save-excursion
;;                                       (outline-up-heading 2)
;;                                       (point)))
;;                          (unless (or all (= org-level 2))
;;                            (save-excursion
;;                              (outline-up-heading 2)
;;                              (org-end-of-subtree)))))))

;; (defun avy-org-child-level (&optional all)
;;   "Go to any org heading one level below the current one.

;; By default, choices are limited to headings under common
;; subheading, but if called with a prefix argument, will be
;; buffer-global."
;;   (interactive "P")
;;   (if (save-excursion (org-goto-first-child))
;;       (let ((org-level (org-current-level)))
;;         (avy--generic-jump
;;          (format "^%s "
;;                  (regexp-quote
;;                   (make-string (+ org-level 1) ?*)))
;;          nil
;;          'pre
;;          (unless all
;;            (save-excursion
;;              (ignore-errors
;;                (outline-up-heading 0))
;;              (point)))
;;          (unless all
;;            (save-excursion
;;              (ignore-errors
;;                (outline-up-heading 0))
;;              (org-end-of-subtree)))))
;;     (message "Heading has no children.")))

;; (defun avy-org-goto-level (&optional num)
;;   "Prompt for an org level to go to, defaulting to the current one."
;;   (interactive (list
;;                 (read-number "Select heading level: " (org-current-level))))
;;   (avy--generic-jump
;;    (format "^%s " (regexp-quote (make-string num ?*)))
;;    nil
;;    'pre))

;; * Outdated/Not used/Phased out
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Helm                                                                                     ;;
;;                                                                                             ;;
;; [[https://github.com/emacs-helm/helm][Helm github]]                                         ;;
;;                                                                                             ;;
;; Replaced, as I like the functionality provided by embark - and this is just not compatible. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package helm
;;   :diminish
;;   :ensure t
;;   :preface (require 'helm-config)
;;   :bind
;;   ("C-x C-f" . 'helm-find-files)
;;   ("C-x C-b" . 'helm-buffers-list)
;;   ("M-x" . 'helm-M-x)
;;   :config
;;   (defun daedreth/helm-hide-minibuffer ()
;;     (when (with-helm-buffer helm-echo-input-in-header-line)
;; (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
;;   (overlay-put ov 'window (selected-window))
;;   (overlay-put ov 'face
;;         (let ((bg-color (face-background 'default nil)))
;;       `(:background ,bg-color :foreground ,bg-color)))
;;   (setq-local cursor-type nil))))
;;   (add-hook 'helm-minibuffer-set-up-hook 'daedreth/helm-hide-minibuffer)
;;   (setq helm-autoresize-max-height 0
;;   helm-autoresize-min-height 40
;;   helm-M-x-fuzzy-match t
;;   helm-buffers-fuzzy-matching t
;;   helm-recentf-fuzzy-match t
;;   helm-semantic-fuzzy-match t
;;   helm-imenu-fuzzy-match t
;;   helm-split-window-in-side-p nil
;;   helm-move-to-line-cycle-in-source nil
;;   helm-ff-search-library-in-sexp t
;;   helm-scroll-amount 8
;;   helm-echo-input-in-header-line t)
;;   :init
;;   (helm-mode 1))

;; ;;  (require 'helm-config)
;;   (helm-autoresize-mode 1)
;;   (define-key helm-find-files-map (kbd "C-b") 'helm-find-files-up-one-level)
;;   (define-key helm-find-files-map (kbd "C-f") 'helm-execute-persistent-action)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Swiper                ;;
;; Replaced by Consult now. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package swiper
;;   :ensure t
;;   :bind ("C-s" . 'swiper))




;;;;;;;;;;;;
;; ** Ivy ;;
;;;;;;;;;;;;

;  (use-package ivy
;   :ensure t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Electric                                                                                            ;;
;; If you write any code, you may enjoy this. I, personally, felt this was more of an annoyance than help ;;
;;                                                                                                        ;;
;; Typing the first character in a set of 2, completes the second one after your cursor.                  ;;
;; Opening a bracket? It's closed for you already. Quoting something? It's closed for you already.        ;;
;;                                                                                                        ;;
;; You can easily add and remove pairs yourself                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(setq electric-pair-pairs '(
;                           (?\{ . ?\})
;                           (?\( . ?\))
;                           (?\[ . ?\])
;                           (?\" . ?\")
;                           ))


;; And now to enable it

;(electric-pair-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Popup Kill Ring                                                                      ;;
;; With a simple M-y you can now browse your kill-ring like browsing autocompletion items. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; (use-package popup-kill-ring
  ;;   :straight t
  ;;   :bind ("M-y" . popup-kill-ring))


;; Function to check for internet being up
;; (defun internet-up-p (&optional host)
;;   (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1"
;;                      (if host host "www.google.com"))))

;; (message (if (internet-up-p) "Up" "Down"))

;; (use-package spaceline
;;   :straight t
;;   :config
;;   (require 'spaceline-config)
;;   (setq spaceline-buffer-encoding-abbrev-p nil
;;         ;; spaceline-line-column-p nil
;;         ;; spaceline-line-p nil
;;         powerline-default-separator (quote utf-8))
;;   (spaceline-spacemacs-theme))

;; ;;Spaceline is the mode line of choice. looks nice and you can set
;; ;;nice separators. Using the =all-the-icons= package gives you more
;; ;;eye-candy.
;; (use-package spaceline-all-the-icons
;;   :straight t
;;   :after spaceline
;;   :config
;;   (setq spaceline-all-the-icons-separator-type 'none)
;;   (spaceline-all-the-icons-theme))
;;   ;; (spaceline-all-the-icons--setup-neotree))

;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; ** company mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package company
;;   :straight t
;;   :config
;;   (setq company-idle-delay 0
;;         company-minimum-prefix-length 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Org capture stuff                                ;;
;; This stuff is kind of moot now that I have org-roam ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (global-set-key (kbd "C-c c") 'org-capture)

;; (setq org-capture-templates
;;       '(("j" "Journal" entry (file+datetree "~/Dropbox/org-files/journal.org")
;;          "* %?\nEntered on %U\n  %i\n  %a")
;;         ("t" "Todo" entry (file+headline "~/Dropbox/org-files/todo.org" "Tasks")
;;          "* TODO %?\n  %i\n  %a")
;;         ("n" "Note" entry (file+headline "~/Dropbox/org-files/notes.org" "Notes")
;;          "* Note %?\n%T")
;;         ("l" "Links" entry (file+headline "~/Dropbox/org-files/Links.org" "Links")
;;          "* %? %^L %^g \n%T" :prepend t)
;;         ))
;; (setq org-capture-templates
;;       '(("j" "Journal" entry (file+datetree (format "%s/%s" ak/my-org-file-location "journal.org"))
;;          "* %?\nEntered on %U\n  %i\n  %a")
;;         ("t" "Todo" entry (file+headline (concat ak/my-org-file-location "todo.org") "Tasks")
;;          "* TODO %?\n  %i\n  %a")
;;         ("n" "Note" entry (file+headline (concat ak/my-org-file-location "notes.org") "Notes")
;;          "* Note %?\n%T")
;;         ("l" "Links" entry (file+headline (concat ak/my-org-file-location "Links.org") "Links")
;;          "* %? %^L %^g \n%T" :prepend t)
;;         ))
;;        org-roam-node-display-template "${title:55} ${tags:*}")

;; *** Freemind

;; (use-package ox-freemind
;;   :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Exporting options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; *** latex

;; (when ak/my-framework-p
;;   (setenv "PATH" (concat (getenv "PATH") ":/usr/bin"))
;;  (when (file-directory-p "/usr/share/emacs/site-lisp/tex-utils")
;;    (add-to-list 'load-path "/usr/share/emacs/site-lisp/tex-utils")
;;    (require 'xdvi-search))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Org Bullets                ;;
;; Makes it all look a bit nicer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package org-bullets
;;   :ensure t
;;   :config
;;     (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;; ;; ** Syntax highlighting for documents exported to HTML ;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;   (use-package htmlize                                      ;;
;;     :straight t)                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package auto-package-update
;;   :config
;;   (setq auto-package-update-delete-old-versions t
;;         auto-package-update-interval 5
;;         auto-package-update-prompt-before-update t
;;         auto-package-update-hide-results t)
;;   (auto-package-update-maybe))


;; (use-package corfu-doc
;;   ;; NOTE 2022-02-05: At the time of writing, `corfu-doc' is not yet on melpa
;;   ;; :straight (corfu-doc :type git :host github :repo "galeo/corfu-doc")
;;   :after corfu
;;   :hook (corfu-mode . corfu-doc-mode)
;;   :bind 
;;   (:map corfu-map
;;             ;; This is a manual toggle for the documentation popup.
;;             ;; (:remap corfu-show-documentation . corfu-doc-toggle) ; Remap the default doc command
;;             ;; Scroll in the documentation window
;;             ("M-n" . corfu-doc-scroll-up)
;;             ("M-p" . corfu-doc-scroll-down))
;;   :custom
;;   (corfu-doc-delay 0.5)
;;   (corfu-doc-max-width 70)
;;   (corfu-doc-max-height 20)

;;   ;; NOTE 2022-02-05: I've also set this in the `corfu' use-package to be
;;   ;; extra-safe that this is set when corfu-doc is loaded. I do not want
;;   ;; documentation shown in both the echo area and in the `corfu-doc' popup.
;;   (corfu-echo-documentation nil))

;; (use-package all-the-icons-dired
;;   :hook (dired-mode . all-the-icons-dired-mode))

  ;; (defvar my/vertico-count-orig vertico-count)
  ;; (define-minor-mode my/vertico-grid-mode
  ;;   "Vertico-grid display with modified row count."
  ;;   :global t :group 'vertico
  ;;   (cond
  ;;    (my/vertico-grid-mode
  ;;     (setq my/vertico-count-orig vertico-count)
  ;;     (setq vertico-count 5)
  ;;     (vertico-grid-mode 1))
  ;;    (t (vertico-grid-mode 0)
  ;;       (setq vertico-count my/vertico-count-orig))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; *** Switch-window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (use-package switch-window
;;   :config
;;   (setq switch-window-input-style 'minibuffer
;;         switch-window-increase 4
;;         switch-window-threshold 2
;;         switch-window-shortcut-style 'qwerty
;;         switch-window-qwerty-shortcuts
;;         '("a" "s" "d" "f" "j" "k" "l" "i" "o"))
;;   :bind
;;   ([remap other-window] . switch-window))



;;;;;;;;;;;;;;;;;;;;;
;; ;; ** yasnippet ;;
;;;;;;;;;;;;;;;;;;;;;

;; (use-package yasnippet
;;   :commands yas-minor-mode
;;   :hook (go-mode . yas-minor-mode)
;;   :config
;;   (yas-reload-all))

;; ** Specific languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; *** lspmode settings                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package lsp-mode
;;   :init
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "C-c l")
;;   :commands lsp lsp-deferred
;;   :hook (
;;          (go-mode . lsp-deferred)
;;          (python-mode . lsp-deferred)
;;          (rustic-mode . lsp-deferred)
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   )

;; Optional - provides fancier overlays

;; (use-package lsp-ui
;;   :hook (lsp-mode . lsp-ui-mode)
;;   ;;  :config (setq lsp-ui-doc-enable t)
;;   :commands lsp-ui-mode
;;   )

;; optionally if you want to use debugger
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; (setq lsp-ui-doc-enable t
;;       lsp-ui-peek-enable t
;;       lsp-ui-sideline-enable t
;;       lsp-ui-imenu-enable t
;;       lsp-ui-flycheck-enable t)


;;;;;;;;;;;;;;;;;;;
;; ;; *** Golang ;;
;;;;;;;;;;;;;;;;;;;

;; (use-package go-mode
;;   ;; :after lsp-mode
;;   :config
;;   ;; (with-eval-after-load "lsp-mode"
;;     (add-to-list 'lsp-enabled-clients 'gopls))

;; ( setq lsp-gopls-staticcheck t
 ;;      lsp-eldoc-render-all t
 ;;      lsp-gopls-complete-unimported t)

;; set up before-save hooks to ensure buffer formatting and aa/delete imports
;; Make sure there are no other gofmt/goimports hooks enabled

;; (defun lsp-go-install-save-hooks ()
;;   (add-hook 'before-save-hook #'lsp-format-buffer t t)
;;   (add-hook 'before-save-hook #'lsp-organize-imports t t))

;; (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
;; (add-hook 'go-mode-hook 'yas-minor-mode)

;;;;;;;;;;;;;;;;;;
;; ;;  c/c++ ;;
;;;;;;;;;;;;;;;;;;

;; (add-hook 'c++-mode-hook 'yas-minor-mode)
;; (add-hook 'c-mode-hook 'yas-minor-mode)

;; (use-package flycheck-clang-analyzer
;;   :config
;;   (with-eval-after-load 'flycheck
;;     (require 'flycheck-clang-analyzer)
;;     (flycheck-clang-analyzer-setup)))

;; (use-package irony
;;   :diminish
;;   :config
;;   (add-hook 'c++-mode-hook 'irony-mode)
;;   (add-hook 'c-mode-hook 'irony-mode)
;;   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;;;;;;;;;;;;;;;;;;;
;; ;; *** python ;;
;;;;;;;;;;;;;;;;;;;

;; (use-package lsp-jedi
;;   :config
;;   (with-eval-after-load "lsp-mode"
;;     (add-to-list 'lsp-disabled-clients 'pyls)
;;     (add-to-list 'lsp-enabled-clients 'jedi)))

;; ;; (add-hook 'python-mode-hook 'yas-minor-mode)
;; (add-hook 'python-mode-hook 'flycheck-mode)


;; (defun ak/my-windows-org-screenshot ()
;;     "Take a screenshot into a time stamped unique-named file in the
;; same directory as the org-buffer and insert a link to this file."
;;     (interactive)
;;     (setq filename
;;           (concat
;;            (make-temp-name
;;             (concat (buffer-file-name)
;;                     "_"
;;                     (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
;;     (shell-command (concat "powershell -command \"Add-Type -AssemblyName System.Windows.Forms;if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {$image = [System.Windows.Forms.Clipboard]::GetImage();[System.Drawing.Bitmap]$image.Save('" filename "',[System.Drawing.Imaging.ImageFormat]::Png); Write-Output 'clipboard content saved as file'} else {Write-Output 'clipboard does not contain image data'}\""))
;;     (insert (concat "[[file:" filename "]]"))
;;     (org-display-inline-images))

;;   (defun ak/my-windows-sniptool ()
;;     (shell-command "snippingtool /clip")
;;     (ak/my-windows-org-screenshot))


;;   ;; (global-set-key "\C-cs" 'ak/my-windows-sniptool))

;; (defun ak/my-linux-take-screenshot ()
;;     (interactive)
;;     (let
;;         ;; Read Filename from Minibuffer
;;         ((filename (read-from-minibuffer "image file name: "))
;;         (directory "_media"))

;;         ;; Use maim to screenshot
;;         (shell-command (format "maim --select %s/%s/%s.png" default-directory directory filename ))

;;         ;; Insert formatted link at point
;;         (save-excursion (insert(format
;;         "#+attr_html: :width 400px \n #+attr_latex: :width 0.4\\textwidth \n [[file:%s/%s.png]]"
;;         directory filename)))

;;         ;; Message success to the minibuffer
;;         (message "saved to %s as %s.png" directory filename)
;;     )
;; )



