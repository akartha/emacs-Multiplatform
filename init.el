;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- lexical-binding: t; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Garbage Collection
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


(add-to-list 'load-path (expand-file-name "custom-conf" user-emacs-directory))

;;temporary for emacs 30 dev build
(when (>= emacs-major-version 29)
(defvar native-comp-deferred-compilation-deny-list nil))

(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (progn
    (setq native-comp-async-report-warnings-errors nil)
    (setq comp-deferred-compilation t)
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
    (setq package-native-compile t)))




(defvar ak/my-framework-p nil)
(defvar ak/my-mac-p nil)

(cond ((string=  system-name "arun-framework")
       (setq ak/my-framework-p t))
      ((string= system-name "Arun-MBP14.local")
       (setq ak/my-mac-p t)))

(defvar ak/generic-windows-p (equal system-type 'windows-nt))
(defvar ak/generic-linux-p (equal system-type 'gnu/linux))
(defvar ak/generic-mac-p (equal system-type 'darwin))

;; (defvar my-server-p (and (equal (system-name) "localhost") (equal user-login-name "akartha")))

(require 'package)
;; (setq package-enable-at-startup nil)

(setq package-archives '(("ELPA"  . "http://tromey.com/elpa/")
			 ("gnu"   . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("org"   . "https://orgmode.org/elpa/")))
(package-initialize)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New approach to try out - loading custom settings from custom-conf directory

;; Below are settings that do not depend on packages and are built-in enhancements to the UI.󰀠󰀠󰀜
(require 'init-basic)

(require 'custom-keymaps)

(require 'init-looks)

(require 'init-editing-functions)

(require 'init-programming)

(require 'init-org-settings)

(require 'non-core)

(require 'init-system-utils)

;;; This is the actual config file. It is omitted if it doesn't exist so emacs won't refuse to launch.
;; (when (file-readable-p "~/.emacs.d/config.org")
;;   (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

