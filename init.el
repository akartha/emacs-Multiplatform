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

(straight-use-package 'org)



(defvar ak/my-framework-p nil)
(defvar ak/my-mac-p nil)
(defvar ak/my-package-list nil)

(cond ((string=  (system-name) "arun-framework")
       (setq ak/my-framework-p t))
      ((string= (system-name) "Arun-MBP14.local")
       (setq ak/my-mac-p t)))

(defvar ak/generic-windows-p (equal system-type 'windows-nt))
(defvar ak/generic-linux-p (equal system-type 'gnu/linux))
(defvar ak/generic-mac-p (equal system-type 'darwin))

;; (defvar my-server-p (and (equal (system-name) "localhost") (equal user-login-name "akartha")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
        dired-du
        dired-open
        dired-sidebar
        async
        dashboard
        which-key
        auto-package-update
        corfu
        switch-window
        savehist
        popper ;;added Mon 24 Apr 2023 02:47:04 PM EDT
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; custom options defined in init-editing-functions.el ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        hungry-delete
        mark-multiple 
        expand-region
        jinx ;;added Mon 24 Apr 2023 06:15:40 PM EDT
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
        yasnippet
        flycheck
        lsp-mode
        lsp-ui
        lsp-jedi
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
        yasnippet-snippets
        all-the-icons
        xkcd))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'init-basic)

(require 'init-looks)

(require 'init-editing-functions)

(require 'init-programming)

(require 'init-org-settings)

(require 'non-core)

(require 'init-system-utils)

;; (when (file-readable-p "~/.emacs.d/config.org")
;;   (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

(put 'narrow-to-region 'disabled nil)
