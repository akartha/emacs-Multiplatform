;; -*- lexical-binding: t; -*-

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d item(s) garbage collected."
                     (emacs-init-time "%.2f")
                     gcs-done)))

(add-to-list 'load-path (expand-file-name "custom-conf" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "custom-conf/third-party" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "custom-conf/third-party/nursery-main/lisp" user-emacs-directory))

(require 'init-env)

;; (unless ak/generic-windows-p
;;   (require 'elpaca-bootstrap)

(require 'elpaca-bootstrap)

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

  ;; (elpaca-wait)
;; (elpaca-process-queues))


(setq package-archives '(("ELPA"  . "http://tromey.com/elpa/")
			 ("gnu"   . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
			 ;; ("org"   . "https://orgmode.org/elpa/")))
(package-initialize)

(if ak/generic-windows-p
    (elpaca-no-symlink-mode))

(require 'init-basic)

(when (window-system)
  (require 'init-looks))

(require 'init-editing-functions)

(require 'init-system-utils)

(require 'init-org-settings)

(require 'org-roam-settings)

(require 'non-core)

(require 'init-nifty-utils)

(require 'init-programming)

;; (require 'epa-file)

;; (require 'sqlite-mode-extras)

(load-file "~/.emacs.d/custom-macros.el")
