;;; -*- lexical-binding: t; -*-
(require 'xkcd)


;; Switch to scratch buffer
(if (> emacs-major-version 28)
    (keymap-set ak-map "z" 'scratch-buffer)
  (define-key ak-map "z" (lambda ()
                           "Switch to scratch"
                           (interactive)
                           (switch-to-buffer "*scratch*"))))

;; Switch to scratch buffer
(define-key ak-map "2" (lambda ()
                         "Switch to scratch"
                         (interactive)
                         (switch-to-buffer(get-buffer-create "*scratch-text*"))))

;; Switch to scratch buffer
;; (define-key ak-map "Z" (lambda ()
;;                          "Create new scratch buffer to scratch"
;;                          (interactive)
;;                          (switch-to-buffer "*scratch*")))

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

(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically (prefix)
  (interactive "P")
  (split-window-right)
  (balance-windows)
  (other-window 1)
  (if prefix
      (switch-to-next-buffer)
    (switch-to-prev-buffer)))

(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Doing =C-x k= should kill the current buffer at all times

(defun kill-current-buffer ()
  "Kills the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; *** Kill buffers without asking for confirmation

(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; *** close-all-buffers

(defun close-all-buffers ()
  "Kill all buffers without regard for their origin."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
(global-set-key (kbd "C-M-s-k") 'close-all-buffers)

(when ak/my-framework-p
  (defhydra hydra-jump-to-directory
    (:color amaranth
            :timeout 5)
"
^System^          ^Projects^            ^Personal^        
^^^^^^^^--------------------------------------------------
_h_: ~            _c_: emacs custom     _v_: Dropbox      
_d_: Documents    _s_: Scripts          _o_: orgs     
_e_: emacs        _p_: projects         _a_: Articles         
^ ^               ^ ^                   _x_: xkcd          _q_: quit 
"
    ("h" (find-file "~/"))
    ("d" (find-file "~/Documents"))
    ("v" (find-file "~/Dropbox"))
    ("a" (find-file "~/Dropbox/articles/"))
    ("e" (find-file "~/.emacs.d/") )
    ("c" (find-file "~/.emacs.d/custom-conf/") )
    ("s" (find-file "~/scripts/") )
    ("p" (find-file "~/projects/") )
    ("o" (find-file "~/Dropbox/org-files/") )
    ("x" (find-file "~/.emacs.d/xkcd/") )
    ("q" nil :color blue))
  (defhydra hydra-jump-to-config
    (:color amaranth
            :timeout 5)
    "Open Config files"
    ("b" (find-file "~/.bashrc") ".bashrc")
    ("p" (find-file "~/.bash_profile") ".bash_profile")
    ("e" (find-file "~/.emacs.d/init.el") "emacs init")
    ("i" (find-file "~/.i3/config") "i3 config")
    ("q" nil "Quit" :color blue))

  (define-key ak-map "d" 'hydra-jump-to-directory/body)
  (define-key ak-map "c" 'hydra-jump-to-config/body))

(when ak/my-win-framework-p
  (defhydra hydra-jump-to-directory
    (:color amaranth
            :timeout 5)
    "
^System^          ^Projects^            ^Personal^        
^^^^^^^^--------------------------------------------------
_h_: c:\          _c_: emacs custom     _v_: Dropbox      
_d_: Documents    ^ ^                   _o_: orgs     
_e_: emacs        ^ ^                   _a_: Articles         
_p_: Desktop      ^ ^                   _x_: xkcd          _q_: quit
"
    ("h" (find-file "c:/") )
    ("d" (find-file "c:/Users/Arun/Documents/") )
    ("p" (find-file "c:/Users/Arun/Desktop/") )
    ("v" (find-file "c:/Users/Arun/Dropbox/") )
    ("a" (find-file "c:/Users/Arun/Dropbox/Dropbox/articles/") )
    ("o" (find-file "c:/Users/Arun/Dropbox/org-files/")  )
    ("e" (find-file "~/.emacs.d/") )
    ("c" (find-file "~/.emacs.d/custom-conf/") )
    ("x" (find-file "~/.emacs.d/xkcd/") )
    ("q" nil :color blue))
  
  (define-key ak-map "d" 'hydra-jump-to-directory/body))
  
  (defhydra hydra-resize-window
    (:color amaranth
            :timeout 5)
    "
^Sideways^             ^Vertically^         ^Absolute^              ^Command and quit
^^^^^^^^--------------------------------------------------------------------------------
_[_: Shrink            _,_: Shrink          _-_: Shrink to fit      _o_: Other window
_]_: Enlarge           _._: Enlarge         _=_: balance            _s_: Ace-window
^ ^                    ^ ^                  ^ ^                     _q_: quit 
"
    ("[" (shrink-window-horizontally 2))
    ("]" (enlarge-window-horizontally 2))
    ("," (shrink-window 2))
    ("." (enlarge-window 2))
    ("-" (shrink-window-if-larger-than-buffer) :color blue)
    ("=" (balance-windows) :color blue)
    ("o" (other-window 1) :color blue)
    ("s" (ace-window 1) :color blue)
    ("q" nil :color blue))
  
  (define-key ak-map "]" 'hydra-resize-window/body)

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

(define-key ak-map "-" 'ak/org-table-mark-field)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Move lines up/down                                                                                                      ;;

;; Copied from this
;; [[https://stackoverflow.com/questions/2423834/move-line-region-up-and-down-in-emacs][stackoverflow
;; post]] ;;
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


(defhydra hydra-move-lines
  (:color amaranth
          :timeout 5)
  "Move selected lines up/down"
  ("[" (ak/move-lines-up 1) "Move up")
  ("]" (ak/move-lines-down 1) "Move down")
  ("q" nil "Quit" :color blue))

(defhydra hydra-launcher (:color blue)
  "Launch"
  ("h" man "man")
  ("n" (browse-url "http://www.nytimes.com/") "nytimes")
  ("w" (browse-url "http://www.emacswiki.org/") "emacswiki")
  ("g" (browse-url "http://www.github.com/") "github")
  ("c" (browse-url "https://chat.openai.com/") "ChatGPT")
  ("x" (browse-url "https://xkcd.com/") "xkcd browser")
  ("s" shell "shell")
  ("X" xkcd "xkcd - emacs")
  ("q" nil "cancel"))


(defhydra hydra-jump-cursor
  (:color amaranth)
  "jump cursor"
  ("]" (forward-sexp) "Forward sexp")
  ("<" (beginning-of-buffer) "Beginning of buffer")
  (">" (end-of-buffer) "End of buffer")
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

(global-set-key (kbd "C-c r") 'hydra-launcher/body)

(define-key ak-map "m" 'hydra-move-lines/body)

(define-key ak-map "." 'hydra-jump-cursor/body)

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

  (define-key ak-map "X" 'ak/reload-xkcd))

(require 'url)


;;Below is from https://stackoverflow.com/questions/4448055/download-a-file-with-emacs-lisp
(defun ak/download-file (&optional url download-dir download-name)
  (interactive)
  (let ((url (or url
                 (read-string "Enter download URL: "))))
    (let ((download-buffer (url-retrieve-synchronously url)))
      (save-excursion
        (set-buffer download-buffer)
        ;; we may have to trim the http response
        (goto-char (point-min))
        (re-search-forward "^$" nil 'move)
        (forward-char)
        (delete-region (point-min) (point))
        (write-file (concat (or download-dir
                                "~/downloads/")
                            (or download-name
                                (car (last (split-string url "/" t))))))))))

;;* Experimental features
(defun ak/lookup-word(arg)
  "Lookup current word in the dictionaries. With
PREFIX, specify word to search"
  (interactive "P")
  (if arg
    (dictionary-search nil)
      (dictionary-lookup-definition)))

;; (require 'emms-setup)
;; (emms-all)
;; (setq emms-player-list '(emms-player-mpv)
;;       emms-info-functions '(emms-info-native))

(define-prefix-command 'ak-emms-map)
(global-set-key (kbd "` p") 'ak-emms-map)

(use-package emms-setup
  :when ak/my-framework-p
  :init
  (add-hook 'emms-player-started-hook 'emms-show)
  (setq emms-show-format "Playing: %s")
  :config
  (emms-all)
  (setq emms-player-list '(emms-player-mpv)
        emms-info-functions '(emms-info-native))

  (defun fg-emms-track-description (track)
    "Return a somewhat nice track description."
    (let ((artist (emms-track-get track 'info-artist))
          (year (emms-track-get track 'info-year))
          (album (emms-track-get track 'info-album))
          (tracknumber (emms-track-get track 'info-tracknumber))
          (title (emms-track-get track 'info-title)))
      (cond
       ((or artist title)
        (concat (if (> (length artist) 0) artist "Unknown artist") " - "
                (if (> (length year) 0) year "XXXX") " - "
                (if (> (length album) 0) album "Unknown album") " - "
                (if (> (length tracknumber) 0)
                    (format "%02d" (string-to-number tracknumber))
                  "XX") " - "
                (if (> (length title) 0) title "Unknown title")))
       (t (emms-track-simple-description track)))))

  (setq emms-track-description-function 'fg-emms-track-description)
  :bind
  (:map ak-emms-map
  ("p" . emms-pause)
  ("s" . emms-stop)
  ("b" . emms-browser)
  ("=" . emms-bookmarks-add)
  ("{" . emms-bookmarks-prev)
  ("}" . emms-bookmarks-next)
  ("<" . (lambda () (interactive) (emms-seek -30)))
  (">" . (lambda () (interactive) (emms-seek 30)))
  ("[" . emms-seek-backward)
  ("]" . emms-seek-forward)))

(eval-after-load 'emms '(emms-state-mode))


(require 'key-chord)
(key-chord-mode t)

(key-chord-define-global "  " 'execute-extended-command)
(key-chord-define-global "aa" 'move-beginning-of-line)
(key-chord-define-global "''" 'move-end-of-line)

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

;;From https://www.reddit.com/r/orgmode/comments/13xs6bo/converting_a_web_page_to_org_mode_to_include_in/
;; converts selected text in clipboard to html, and then uses pandoc to convert it to org mode 
(defun ak/insert-org-from-html-clipboard ()
  (interactive)
  (if (not (executable-find "pandoc")) 
      (error "pandoc executable not found"))
  (let* 
      ((pandoc-command "pandoc -f html -t org --wrap=none")
       (linux-clip-as-html-command "xclip -select clipboard -target text/html -o")
       (mac-clip-as-html-command "osascript -e 'the clipboard as \"HTML\"' | perl -ne 'print chr foreach unpack(\"C*\",pack(\"H*\",substr($_,11,-3)))'")
       (windows-clip-as-html-command "powershell -command Get-Clipboard -Format Text -TextFormatType Html"))
    (cond 
     (ak/generic-windows-p (shell-command (concat windows-clip-as-html-command " | " pandoc-command) 1))
     (ak/generic-mac-p (shell-command (concat mac-clip-as-html-command " | " pandoc-command) 1))
     (t (shell-command (concat linux-clip-as-html-command " | " pandoc-command) 1)))))

(use-package org-web-tools
  :bind (:map ak-map
              ("<f4>" . ak/get-url-title)
              ("<f6>" . org-web-tools-insert-web-page-as-entry)
              ("<f5>" . org-web-tools-read-url-as-org)
              ("<f7>" . org-web-tools-insert-link-for-url))
  :config 
  (setq org-web-tools-pandoc-sleep-time 0.7)
  (cl-defun ak/get-url-title(&optional (url (org-web-tools--get-first-url)))
    (interactive)
    (let* ((html (org-web-tools--get-url url))
           (title (org-web-tools--html-title html)))
      (if title (insert title) (insert url)))))


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


(provide 'non-core)
