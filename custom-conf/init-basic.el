;;; -*- lexical-binding: t; -*-

;;(setq native-comp-async-report-warnings-errors nil)

(setq-default indent-tabs-mode nil
              tab-width 4
              tab-always-indent nil)

;; Can be activated with `display-line-numbers-mode'
(setq-default display-line-numbers-width 3)
(setq-default display-line-numbers-widen t)

(setq inhibit-startup-message t ;Remove the startup screen
      use-dialog-box nil
      ring-bell-function 'ignore
      make-backup-files nil
      ;; auto-save-default t
      global-auto-revert-non-file-buffers t ;; Revert Dired and other buffers
      ;; history-length t
      history-length 300
      ;; savehist-save-minibuffer-history t
      ;;Show the current line and column for your cursor.
      line-number-mode t
      column-number-mode t
      display-time-24hr-format t
      display-time-format "%H:%M - %d %B %Y"
      ;; scroll-conservatively 100
      scroll-conservatively 10
      scroll-step 1
      kill-ring-max 100
;;;    Indenting
      indent-line-function 'insert-tab
      electric-indent-mode nil ;;annoying when it tabs on text files. Maybe I should only enable for prog-mode
      sentence-end-double-space nil  ;; Sentence end need not be double spaced
      epg-pinentry-mode 'loopback
      package-install-upgrade-built-in t
      ;; No need to keep duplicates in prompt history.
      history-delete-duplicates t
      ;;this is needed so emacs doesnt do cute stuff when searching and replacing strings
      case-replace nil
      ;; Remove duplicates from the kill ring to reduce clutter
      kill-do-not-save-duplicates t
      ;; By default, Emacs "updates" its ui more often than it needs to
      idle-update-delay 1.0    
      )

(epa-file-enable)

(put 'narrow-to-region 'disabled nil)

(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate fontification immediately after scrolling.
(setq fast-but-imprecise-scrolling t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

(when (display-graphic-p)
   (tool-bar-mode -1))
;; (tool-bar-mode -1)
(menu-bar-mode -1)
(if window-system (scroll-bar-mode -1))
(global-auto-revert-mode 1)

;; `recentf' is an Emacs package that maintains a list of recently
;; accessed files, making it easier to reopen files you have worked on
(recentf-mode 1)
(setq recentf-max-saved-items 300) ; default is 20
(setq recentf-max-menu-items 20)
(setq recentf-auto-cleanup (if (daemonp) 300 'never))
(add-to-list 'recentf-exclude "~/\.emacs\.d/xkcd/*")


(save-place-mode t)     ;; Remember cursor position even after quitting file
(display-time-mode 1)
;; (global-subword-mode 1) 
(show-paren-mode 1) ;;Highlights matching parens when the cursor is just behind one of them.
(setq show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t) 
;; Turn on transient-mark-mode
(transient-mark-mode 1)
(goto-address-mode 1)

(blink-cursor-mode -1)
;; Reduce rendering/line scan work by not rendering cursors or regions in
;; non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Avoid automatic frame resizing when adjusting settings.
(setq global-text-scale-adjust-resizes-frames nil)

;; Disable wrapping by default due to its performance cost.
(setq-default truncate-lines t)

;;; Text mode and Auto Fill mode
; Set default Emacs mode to text-mode. In addition, turn on
; word-wrapping and either auto filling of text or longlines-mode,
; which auto fills in Emacs buffers but not when you copy the text.
(setq default-major-mode 'text-mode)
;;(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook 'toggle-word-wrap)

;; Continue wrapped lines at whitespace rather than breaking in the
;; middle of a word.
(setq-default word-wrap t)

;; Enhance `apropos' and related functions to perform more extensive searches
(setq apropos-do-all t)

;; Fixes #11: Prevents help command completion from triggering autoload.
;; (e.g., apropos-command, apropos-variable, apropos...)
;; Loading additional files for completion can slow down help commands
;; and may unintentionally execute initialization code from some libraries.
(setq help-enable-completion-autoload nil)
(setq help-enable-autoload nil)
(setq help-enable-symbol-autoload nil)
(setq help-window-select t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Scroll with cursor stationary ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [C-down] (kbd "C-u 1 C-v"))
(global-set-key [C-up] (kbd "C-u 1 M-v"))

;;Set locale to utf-8 in a myriad number of places
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(when window-system (add-hook 'prog-mode-hook 'hl-line-mode))


(add-hook 'prog-mode-hook 'subword-mode)


(add-hook 'prog-mode-hook 
          (defun ak/prog-mode-hook()
            (display-line-numbers-mode)
            (set-fill-column 88)
            (setq display-line-numbers 'relative)
            (display-fill-column-indicator-mode)))

(add-hook 'prog-mode-hook 'electric-pair-mode)
                            
(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default zoneinfo-style-world-list
   '(("Zulu" "Zulu")
     ("America/New_York" "New York/D.C.")
     ("America/Chicago" "Chicago/Minneapolis/Dallas")
     ("America/Denver" "Denver/Utah/El Paso")
     ("America/Phoenix" "Phoenix")
     ("America/Los_Angeles" "Los Angeles/Seattle")
     ("Asia/Calcutta" "Bangalore")
     ("Atlantic/Reykjavik" "Reykjavik")
     ("Europe/London" "London")
     ("Europe/Paris" "Paris/Berlin/Vatican/Zurich")
     ("Europe/Moscow" "Moscow/Kiev/Athens")
     ("Asia/Tehran" "Tehran")
     ("Asia/Shanghai" "Beijing/Shanghai")
     ("Asia/Singapore" "Singapore")
     ("Asia/Seoul" "Seoul")
     ("Asia/Tokyo" "Tokyo")
     ("Australia/Sydney" "Sydney/Melbourne")
     ("Pacific/Auckland" "Auckland")))



(use-package exec-path-from-shell
  :ensure t
  :if ak/my-mac-p
  :config
  (exec-path-from-shell-initialize))



(define-prefix-command 'ak-map)
(global-set-key (kbd "`") 'ak-map)
(global-set-key (kbd "` `") '("Literal \"`\"" . self-insert-command))

(setq epg-pinentry-mode 'loopback) ;;Allows gpg password entry through emacs, rather than external program.
;; added on Fri 28 Apr 2023 12:11:38 PM EDT

(define-key ak-map "$" '("Secrets" . (lambda() (interactive)
                                       (load(expand-file-name "~/.emacs.d/custom-conf/load-details.el.gpg")))))

(define-key ak-map "0" '("Repeat last command" . repeat))
(define-key ak-map (kbd "<left>")  '("Previous Buffer" . previous-buffer))
(define-key ak-map (kbd "<right>")  '("Next Buffer" . next-buffer))
(define-key ak-map (kbd "<up>")  '("Switch to Buffer" . consult-buffer))

(define-key ak-map (kbd "b")  '("Buffer Back" . mode-line-other-buffer))

(use-package savehist
  :defer t
  :init
  (savehist-mode 1)

  :config
  (if (> emacs-major-version 28)
      (progn (add-to-list 'savehist-additional-variables 'register-alist) ;This doesn't work, but I havent lost hope yet
             (add-to-list 'savehist-additional-variables '(search-ring . 100 ))
             (add-to-list 'savehist-additional-variables 'regexp-search-ring)
             (add-to-list 'savehist-additional-variables 'last-kbd-macro)
             (add-to-list 'savehist-additional-variables 'shell-command-history)
             (add-to-list 'savehist-additional-variables '(kill-ring . 100))) ;;dont want to go insane with the number of clipboard items saved.
    
    (add-to-list 'savehist-additional-variables 'register-alist)
    (add-to-list 'savehist-additional-variables 'search-ring )
    (add-to-list 'savehist-additional-variables 'regexp-search-ring)
    (add-to-list 'savehist-additional-variables 'last-kbd-macro)   
    (add-to-list 'savehist-additional-variables 'shell-command-history)    
    (add-to-list 'savehist-additional-variables 'kill-ring )))



;;;;;;;;;;;;;;;;;
;; ** PDF tool ;;
;;;;;;;;;;;;;;;;;

(when (and (or ak/my-framework-p ak/my-mac-p)
    (file-directory-p "/usr/share/emacs/site-lisp/tex-utils"))
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/tex-utils")
  (require 'xdvi-search))

;; See https://github.com/vedang/pdf-tools for more info
(use-package pdf-tools 
  :ensure t
  :init 
  (pdf-loader-install :no-query))

(add-hook 'text-mode-hook 'abbrev-mode)


(setq delete-by-moving-to-trash t)


;; added on Sat 10 Jun 2023 11:48:49 AM EDT from prots search video
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format "(%s/%s) ")

(when (> emacs-major-version 28)
  (pixel-scroll-precision-mode t))

;; *** Kill buffers without asking for confirmation

(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

(setq org-crypt-key "DA289147FB279C3D")
;; GPG key to use for encryption.
;; nil means  use symmetric encryption unconditionally.
;; "" means use symmetric encryption unless heading sets CRYPTKEY property.

(defun ak/isearch-with-region ()
  "Use region as the isearch text."
  (when mark-active
    (let ((region (funcall region-extract-function nil)))
      (goto-char (region-beginning))
      (deactivate-mark)
      ;; (isearch-push-state)
      (isearch-update)
      (isearch-yank-string region))))

(add-hook 'isearch-mode-hook #'ak/isearch-with-region)

;;Sometimes, magit requires the latest version 
;;of transient 
(use-package transient
    :ensure t)

(use-package magit
  :ensure t
  :after transient
  :config
  (setq magit-push-always-verify nil
        git-commit-summary-max-length 50)
  :bind (:map ak-map
              ("g" . magit-status)))

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package server
  :ensure nil
  :defer 1
  :config 
  (unless (server-running-p)
    (server-start)))


(provide 'init-basic)
