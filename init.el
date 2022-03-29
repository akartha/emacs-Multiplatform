;;; This fixed garbage collection, makes emacs start up faster ;;;;;;;
;;(setq gc-cons-threshold 402653184
;;      gc-cons-percentage 0.6)

;;(defvar startup/file-name-handler-alist file-name-handler-alist)
;;(setq file-name-handler-alist nil)

;;(defun startup/revert-file-name-handler-alist ()
 ;; (setq file-name-handler-alist startup/file-name-handler-alist))

;;(defun startup/reset-gc ()
;;  (setq gc-cons-threshold 16777216
;;	gc-cons-percentage 0.1))

;;(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)
;;(add-hook 'emacs-startup-hook 'startup/reset-gc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is all kinds of necessary
(require 'package)
(setq package-enable-at-startup nil)

;;; remove SC if you are not using sunrise commander and org if you like outdated packages
(setq package-archives '(("ELPA"  . "http://tromey.com/elpa/")
			 ("gnu"   . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")))
(package-initialize)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Bootstrapping use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; This is the actual config file. It is omitted if it doesn't exist so emacs won't refuse to launch.
(when (file-readable-p "~/.emacs.d/config.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-completion-style 'emacs)
 '(package-selected-packages
   '(general all-the-icons rustic dired-single elcord erc-hl-nicks crux org-roam json lsp-jedi xkcd expand-region nov olivetti writeroom-mode pretty-mode modus-themes restclient lsp-ui lsp-mode company-lsp pdf-tools sudo-edit org elfeed engine-mode org-present org-easy-img-insert xml-format json-reformatter-jq json-mode go-mode org-download use-package org-pdfview dash csv-mode))
 '(send-mail-function 'smtpmail-send-it))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "ADBO" :family "Source Code Pr")))))
