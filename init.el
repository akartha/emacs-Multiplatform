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

;; (straight-use-package 'use-package)
;; (straight-use-package 'org)



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
      '(use-package
         org
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;; Custom options for below are defined in init-system-utils ;;
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
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; custom options defined in init-editing-functions ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        hungry-delete
        mark-multiple 
        expand-region
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Custom options for below are set in the init-looks library ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        modus-themes 
        fontaine
        all-the-icons-completion
        beacon
        rainbow-mode
        rainbow-delimiters
        diminish
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; custom options for below are in init-org-settings ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        org-superstar
        ox-reveal
        org-roam
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; custom options for below are set in the init-programming library ;;
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
