;;; -*- lexical-binding: t; -*-

(defconst ak/my-framework-p  (string=  (system-name) "arun-framework")
  "Framework 13 manjaro install")
(defconst ak/my-win-framework-p (string=  (system-name) "FRAMEWORKWIN")
  "Framework 13 windows dual boot")
(defconst ak/my-mac-p (string= (system-name) "Arun-MBP14.local")
  "Macbook Pro M1")
(defconst ak/my-pi-p (or (string= (system-name) "pi-o-mine") 
                         (string= (system-name) "pi-in-face"))
  "Either my Raspberry Pi 4 or the Clockworkpi uconsole")

(defconst ak/my-server-p (string= (system-name) "bunty")
  "Ubuntu server - non GUI")

(defconst ak/generic-windows-p (equal system-type 'windows-nt)
"Any windows machine")
(defconst ak/generic-linux-p (equal system-type 'gnu/linux)
"Any linux machine")
(defconst ak/generic-mac-p (equal system-type 'darwin)
"Any mac")


(defconst ak/my-org-file-location 
  (cond (ak/my-framework-p (expand-file-name "~/Dropbox/org-files/"))
        (ak/my-win-framework-p (expand-file-name "c:/Users/Arun/Dropbox/org-files/"))
        (ak/my-mac-p (expand-file-name "~/Dropbox/org-files/"))
        (ak/my-server-p (expand-file-name "~/Documents/org-files/"))
        (ak/my-pi-p (expand-file-name "~/Documents/org-docs/"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; need this for windows, as otherwise gpg doesnt understand
;; the path that gets generated

(when ak/my-win-framework-p
  (setq package-gnupghome-dir "~/.emacs.d/elpa/gnupg"))

(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(when (> emacs-major-version 27)
  (setq redisplay-skip-fontification-on-input t))

;; Remove command line options that aren't relevant to our current OS; that
;; means less to process at startup.
(unless ak/generic-mac-p
  (setq command-line-ns-option-alist nil))

(unless ak/generic-linux-p 
  (setq command-line-x-option-alist nil))


;; Performance on Windows is considerably worse than elsewhere.
;; (when ak/generic-windows-p
;; (when (symbolp 'w32-get-true-file-attributes)
;; ;;   ;; Reduce the workload when doing file IO
;;   (setq w32-get-true-file-attributes nil))


;;below is from https://www.emacswiki.org/emacs/ExecPath
;;;###autoload
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

;; ;; Below, with tweaks, is from https://www.masteringemacs.org/article/maximizing-emacs-startup
;; ;;;###autoload
;; (defun ak/maximize-frame ()
;;   "Maximizes the active frame in Windows"
;;   (interactive)
;;   ;; Send a `WM_SYSCOMMAND' message to the active frame with the
;;   ;; `SC_MAXIMIZE' parameter.
;;   (if ak/generic-windows-p
;;       (w32-send-sys-command 61488)))

;; (add-hook 'window-setup-hook 'ak/maximize-frame t)
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; (add-hook 'window-size-change-functions
;;             #'frame-hide-title-bar-when-maximized)


(provide 'init-env)
