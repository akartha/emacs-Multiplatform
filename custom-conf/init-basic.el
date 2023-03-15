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
      history-length 25
      ;;Show the current line and column for your cursor.
      line-number-mode t
      column-number-mode t
      display-time-24hr-format t
      display-time-format "%H:%M - %d %B %Y"
      scroll-conservatively 100
      kill-ring-max 100
;;;    Indenting
      indent-line-function 'insert-tab)


(savehist-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-auto-revert-mode 1)
(recentf-mode 1)
(save-place-mode t)     ;; Remember cursor position even after quitting file
(display-time-mode 1)
(global-subword-mode 1) ;;Emacs treats camelCase strings as a single word by default, this changes said behaviour.
(show-paren-mode 1) ;;Highlights matching parens when the cursor is just behind one of them.

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
(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'init-basic)
