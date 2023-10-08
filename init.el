;; -*- lexical-binding: t; -*-

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d item(s) garbage collected."
                     (emacs-init-time "%.2f")
                     gcs-done)))


(add-to-list 'load-path (expand-file-name "custom-conf" user-emacs-directory))


(require 'package)

(setq package-archives '(("ELPA"  . "http://tromey.com/elpa/")
			 ("gnu"   . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("org"   . "https://orgmode.org/elpa/")))
(package-initialize)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar ak/my-framework-p nil)
(defvar ak/my-win-framework-p nil)
(defvar ak/my-mac-p nil)
(defvar ak/my-pi-p nil)
(defvar ak/my-package-list nil)

(cond ((string=  (system-name) "arun-framework")
       (setq ak/my-framework-p t))
      ((string=  (system-name) "FRAMEWORKWIN")
       (setq ak/my-win-framework-p t))
      ((string= (system-name) "Arun-MBP14.local")
       (setq ak/my-mac-p t))
      ((string= (system-name) "usbpi")
       (setq ak/my-pi-p t)))

(defvar ak/generic-windows-p (equal system-type 'windows-nt))
(defvar ak/generic-linux-p (equal system-type 'gnu/linux))
(defvar ak/generic-mac-p (equal system-type 'darwin))
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
(eval-when-compile
  (eval-after-load 'advice
    `(setq ad-redefinition-action 'accept))
  (setq use-package-verbose nil
        use-package-compute-statistics nil
        ;use-package-ignore-unknown-keywords t
        use-package-minimum-reported-time 0.01
        ;; use-package-expand-minimally t
        use-package-enable-imenu-support t)
  (require 'use-package))

;; (defvar my-server-p (and (equal (system-name) "localhost") (equal user-login-name "akartha")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; need this for windows, as otherwise gpg doesnt understand
;; the path that gets generated

(when ak/my-win-framework-p
  (setq package-gnupghome-dir "~/.emacs.d/elpa/gnupg"))

;; (when ak/my-framework-p
;;   (server-start)) ;;added Tue 09 May 2023 11:55:19 PM EDT

(setq ak/my-package-list
      '(;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Custom options for below are defined in init-system-utils.el ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        avy
        crux
        marginalia
        orderless
        consult
        embark
        embark-consult
        all-the-icons-dired
        ;; dired-du
        dired-open
        dired-sidebar
        async
        dashboard
        which-key
        ;; auto-package-update
        corfu
        ;; switch-window ;; commented on Thu 27 Apr 2023 02:03:43 PM EDT
        savehist
        popper ;;added Mon 24 Apr 2023 02:47:04 PM EDT
        ace-window ;;added Thu 27 Apr 2023 11:40:37 AM EDT
        kind-icon ;; added on Sun 30 Apr 2023 12:21:21 PM EDT
        cape ;; added on Sun 30 Apr 2023 06:01:38 PM EDT
        nerd-icons-dired ;;added Fri 12 May 2023 01:36:19 PM EDT
        nov              ;;added Sun 21 May 2023 12:16:39 PM EDT
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; custom options defined in init-editing-functions.el ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        hungry-delete
        mark-multiple 
        expand-region
        jinx ;;added Mon 24 Apr 2023 06:15:40 PM EDT
        gptel ;;added on Tue 09 May 2023 12:06:00 PM EDT

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Custom options for below are set in the init-looks.el library ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        modus-themes 
        fontaine
        all-the-icons-completion
        beacon
        rainbow-mode
        rainbow-delimiters
        diminish
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; custom options for below are in init-org-settings.el ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        org-superstar
        ox-reveal
        org-roam
        pdf-tools
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; custom options for below are set in the init-programming.el library ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        exec-path-from-shell
        ;; yasnippet
        flycheck
        ;; lsp-mode
        ;; lsp-ui
        ;; lsp-jedi
        go-mode
        flycheck-clang-analyzer
        irony
        slime
        json-mode
        jq-mode
        json-reformat
        jq-format
        verb
        rustic
        ob-mermaid
        projectile
        magit
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; No custom options defined for the below ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        org-present
        ox-twbs
        dired-single
        hydra
        mermaid-mode
        sqlformat
        restclient
        ob-restclient
        request
        plz
        ob-go
        ;; yasnippet-snippets
        all-the-icons
        xkcd
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; non-core / experimental  ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        emms
        emms-state
        key-chord
        ;; org-web-tools
        htmlize
        plantuml-mode))

;; Install packages above
(dolist (package ak/my-package-list)
  (straight-use-package package))


;;custom options for vertico are defined in init-system-utils
(straight-use-package
 '(vertico :files (:defaults "extensions/*")))

(straight-use-package
 '(ox-odt :type git :host github :repo "kjambunathan/org-mode-ox-odt"
          :files ("lisp/ox-odt.el"
                           "lisp/odt.el"
                           "etc"
                           "docs"
                           "contrib/odt/LibreOffice")))

(straight-use-package
 '(lambda-line :type git :host github :repo "lambda-emacs/lambda-line"))

(straight-use-package
 '(xht :type git :host sourcehut :repo "flandrew/xht"))

(add-to-list 'load-path (expand-file-name "org-web-tools" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'init-basic)

(when (window-system)
  (require 'init-looks))

(require 'init-editing-functions)

(require 'init-org-settings)

;;added on Wed 27 Sep 2023 09:25:03 AM EDT because it wasnt being automatically loaded
(require 'org-web-tools) 

(require 'non-core)

(require 'init-nifty-utils)

;; (require 'init-hydra)

(require 'init-system-utils)

(require 'epa-file)
(epa-file-enable)

(require 'init-programming)


;; (when (file-readable-p "~/.emacs.d/config.org")
;;   (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

;; (setenv "GPG_AGENT_INFO" nil)

(setq epa-pinentry-mode 'loopback)

(put 'narrow-to-region 'disabled nil)
