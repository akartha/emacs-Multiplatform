;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; -*- lexical-binding: t; -*-

;; The default is 800 kilobytes.  Measured in bytes.
;; Set the garbage collection threshold to high (100 MB) since LSP client-server communication generates a lot of output/garbage
(setq gc-cons-threshold (* 100 1000 1000))

;; To increase the amount of data Emacs reads from a process
(setq read-process-output-max (* 1024 1024)) 

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d item(s) garbage collected."
                     (emacs-init-time "%.2f")
                     gcs-done)))

;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)

;; Set the right directory to store the native comp cache
;;(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

(defvar my-framework-linux-p (equal (system-name) "arun-framework"))

(defvar my-server-p (and (equal (system-name) "localhost") (equal user-login-name "akartha")))

;; (defvar my-phone-p (not (null (getenv "ANDROID_ROOT")))
;;   "If non-nil, GNU Emacs is running on Termux.")
;; (when my-phone-p (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
;; (global-auto-revert-mode)  ; simplifies syncing

(require 'package)
(setq package-enable-at-startup nil)

;;; remove SC if you are not using sunrise commander and org if you like outdated packages
(setq package-archives '(("ELPA"  . "http://tromey.com/elpa/")
			 ("gnu"   . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("org"   . "https://orgmode.org/elpa/")))
(package-initialize)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Bootstrapping use-package
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el" 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(straight-use-package 'org)

;;; This is the actual config file. It is omitted if it doesn't exist so emacs won't refuse to launch.
(when (file-readable-p "~/.emacs.d/config.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

;; (add-to-list 'load-path "~/.emacs.d/lisp/")
