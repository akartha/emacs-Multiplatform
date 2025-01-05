;;; -*- lexical-binding: t; -*-

(defvar ak/my-framework-p nil
"Framework 13 manjaro install")
(defvar ak/my-win-framework-p nil
"Framework 13 windows dual boot")
(defvar ak/my-mac-p nil
"Macbook Pro M1")
(defvar ak/my-pi-p nil
"Either my Raspberry Pi 4 or the Clockworkpi uconsole")
(defvar ak/my-package-list nil)

(cond ((string=  (system-name) "arun-framework")
       (setq ak/my-framework-p t))
      ((string=  (system-name) "FRAMEWORKWIN")
       (setq ak/my-win-framework-p t))
      ((string= (system-name) "Arun-MBP14.local")
       (setq ak/my-mac-p t))
      ((or (string= (system-name) "usbpi") (string= (system-name) "pi-o-mine") (string= (system-name) "pi-in-face"))
       (setq ak/my-pi-p t)))

(defvar ak/generic-windows-p (equal system-type 'windows-nt)
"Any windows machine")
(defvar ak/generic-linux-p (equal system-type 'gnu/linux)
"Any linux machine")
(defvar ak/generic-mac-p (equal system-type 'darwin)
"Any mac")
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

;; Below, with tweaks, is from https://www.masteringemacs.org/article/maximizing-emacs-startup
;;;###autoload
(defun ak/maximize-frame ()
  "Maximizes the active frame in Windows"
  (interactive)
  ;; Send a `WM_SYSCOMMAND' message to the active frame with the
  ;; `SC_MAXIMIZE' parameter.
  (if ak/generic-windows-p
      (w32-send-sys-command 61488)))

(add-hook 'window-setup-hook 'ak/maximize-frame t)

(cond (ak/my-framework-p
       (setq ak/my-org-file-location (expand-file-name "~/Dropbox/org-files/")))
      (ak/my-win-framework-p
       (setq ak/my-org-file-location (expand-file-name "c:/Users/Arun/Dropbox/org-files/")))
      (ak/my-mac-p
       (setq ak/my-org-file-location (expand-file-name "~/Dropbox/org-files/")))
      (ak/my-pi-p
       (setq ak/my-org-file-location (expand-file-name "~/Documents/org-docs/"))))


(provide 'init-env)
