;;; -*- lexical-binding: t; -*-

(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)
;;(setq native-comp-async-report-warnings-errors nil)

(setq-default indent-tabs-mode nil
              tab-width 4)

(setq inhibit-startup-message t ;Remove the startup screen
      use-dialog-box nil
      ring-bell-function 'ignore
      make-backup-files nil
      auto-save-default t
      global-auto-revert-non-file-buffers t ;; Revert Dired and other buffers
      history-length 200
      ;;Show the current line and column for your cursor.
      line-number-mode t
      column-number-mode t
      display-time-24hr-format t
      display-time-format "%H:%M - %d %B %Y"
      scroll-conservatively 100
      kill-ring-max 100
;;;    Indenting
      indent-line-function 'insert-tab
      electric-indent-mode nil ;;annoying when it tabs on text files. Maybe I should only enable for prog-mode
      sentence-end-double-space nil)  ;; Sentence end need not be double spaced

(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate fontification immediately after scrolling.
(setq fast-but-imprecise-scrolling t)
(when (> emacs-major-version 27)
  (setq redisplay-skip-fontification-on-input t))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Remove command line options that aren't relevant to our current OS; that
;; means less to process at startup.
(unless ak/generic-mac-p   (setq command-line-ns-option-alist nil))
(unless ak/generic-linux-p (setq command-line-x-option-alist nil))

;; Performance on Windows is considerably worse than elsewhere.
(when ak/generic-windows-p
  ;; Reduce the workload when doing file IO
  (setq w32-get-true-file-attributes nil))

(savehist-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(if window-system (scroll-bar-mode -1))
(global-auto-revert-mode 1)
(recentf-mode 1)
(save-place-mode t)     ;; Remember cursor position even after quitting file
(display-time-mode 1)
;; (global-subword-mode 1) 
(show-paren-mode 1) ;;Highlights matching parens when the cursor is just behind one of them.
;; Turn on transient-mark-mode
(transient-mark-mode 1)

;;; Text mode and Auto Fill mode
; Set default Emacs mode to text-mode. In addition, turn on
; word-wrapping and either auto filling of text or longlines-mode,
; which auto fills in Emacs buffers but not when you copy the text.
(setq default-major-mode 'text-mode)
;;(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook 'toggle-word-wrap)

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

(defalias 'yes-or-no-p 'y-or-n-p)


(setq zoneinfo-style-world-list
   '(("America/New_York" "New York")
     ("America/Chicago" "Chicago")
     ("Asia/Calcutta" "Bangalore")
     ("America/Denver" "Denver")
     ("America/Los_Angeles" "Seattle")
     ("Europe/London" "London")
     ("Europe/Paris" "Paris")
     ("Asia/Tokyo" "Tokyo")))


;;below is from https://www.emacswiki.org/emacs/ExecPath
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.
Does not work with mac- so I have a package for that"
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$" "" (shell-command-to-string
					  "$SHELL --login -c 'echo $PATH'"
						    ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when
    (or ak/generic-linux-p
        ak/generic-mac-p)
        (set-exec-path-from-shell-PATH))

(use-package exec-path-from-shell
  :if ak/my-mac-p
  :config
  (exec-path-from-shell-initialize))

(add-to-list 'recentf-exclude "~/\.emacs\.d/xkcd/*")

(defvar ak/my-org-file-location nil)

(cond (ak/my-framework-p
       (setq ak/my-org-file-location (expand-file-name "~/Dropbox/org-files/")))
      (ak/my-win-framework-p
       (setq ak/my-org-file-location (expand-file-name "c:/Users/Arun/Dropbox/org-files/")))
      (ak/my-mac-p
       (setq ak/my-org-file-location (expand-file-name "/Volumes/Expansion/akartha/Dropbox/org-files/")))
      (ak/my-pi-p
       (setq ak/my-org-file-location (expand-file-name "~/Documents/org-docs/"))))

(define-prefix-command 'ak-map)
(global-set-key (kbd "`") 'ak-map)
(global-set-key (kbd "` `") 'self-insert-command)

(setq epg-pinentry-mode 'loopback) ;;Allows gpg password entry through emacs, rather than external program.
;; added on Fri 28 Apr 2023 12:11:38 PM EDT

(define-key ak-map "$" (lambda() (interactive)
        (load(expand-file-name "~/.emacs.d/custom-conf/load-details.el.gpg"))))

(define-key ak-map "0" 'repeat)

(use-package savehist
  :defer
  ;; :init
  ;; (savehist-mode 1)
  :config
  (if (> emacs-major-version 28)
      (progn (add-to-list 'savehist-additional-variables 'register-alist) ;This doesn't work, but I havent lost hope yet
             (add-to-list 'savehist-additional-variables '(search-ring . 100 ))
             (add-to-list 'savehist-additional-variables 'regexp-search-ring)
             (add-to-list 'savehist-additional-variables '(kill-ring . 100))) ;;dont want to go insane with the number of clipboard items saved.
    
    (add-to-list 'savehist-additional-variables 'register-alist)
    (add-to-list 'savehist-additional-variables 'search-ring )
    (add-to-list 'savehist-additional-variables 'regexp-search-ring)
    (add-to-list 'savehist-additional-variables 'kill-ring )))



;;;;;;;;;;;;;;;;;
;; ** PDF tool ;;
;;;;;;;;;;;;;;;;;

(when (and (or ak/my-framework-p ak/my-mac-p)
    (file-directory-p "/usr/share/emacs/site-lisp/tex-utils"))
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/tex-utils")
  (require 'xdvi-search))

;; See https://github.com/vedang/pdf-tools for more info
(pdf-loader-install)

(add-hook 'text-mode-hook 'abbrev-mode)

(setq gptel-default-mode 'org-mode) ;;added on Tue 09 May 2023 12:34:28 PM EDT

(setq delete-by-moving-to-trash t)

;; Below, with tweaks, is from https://www.masteringemacs.org/article/maximizing-emacs-startup
(defun ak/maximize-frame ()
  "Maximizes the active frame in Windows"
  (interactive)
  ;; Send a `WM_SYSCOMMAND' message to the active frame with the
  ;; `SC_MAXIMIZE' parameter.
  (if ak/generic-windows-p
      (w32-send-sys-command 61488)))

(add-hook 'window-setup-hook 'ak/maximize-frame t)

;; added on Sat 10 Jun 2023 11:48:49 AM EDT from prots search video
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format "(%s/%s) ")

(when (> emacs-major-version 28)
  (pixel-scroll-precision-mode t))

(provide 'init-basic)
