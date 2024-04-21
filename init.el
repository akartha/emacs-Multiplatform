;; -*- lexical-binding: t; -*-

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d item(s) garbage collected."
                     (emacs-init-time "%.2f")
                     gcs-done)))

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))



(add-to-list 'load-path (expand-file-name "custom-conf" user-emacs-directory))


(setq package-archives '(("ELPA"  . "http://tromey.com/elpa/")
			 ("gnu"   . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("org"   . "https://orgmode.org/elpa/")))
(package-initialize)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'init-env)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(if ak/generic-windows-p
    (elpaca-no-symlink-mode))

;; (setq ak/my-package-list
      ;; '(;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Custom options for below are defined in init-system-utils.el ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(elpaca       avy		     		)
(elpaca       crux			    	)
(elpaca       marginalia			)
(elpaca       orderless				)
(elpaca       consult				)
(elpaca       embark				)
(elpaca       embark-consult		)
(elpaca       all-the-icons-dired	)
(elpaca       dired-open			)
(elpaca       dired-sidebar			)
(elpaca       async				    )
(elpaca       dashboard				)
(elpaca       which-key				)
(elpaca       corfu				    )
(elpaca       popper 				)
(elpaca       ace-window 			)
(elpaca       kind-icon 			)
(elpaca       cape 				    )
(elpaca       nerd-icons-dired		)
(elpaca       nov  				    )
(elpaca       hungry-delete			)
(elpaca       mark-multiple 		)
(elpaca       jinx 				    )
(elpaca       gptel				    )
(elpaca       modus-themes 			)
(elpaca       fontaine				)
(elpaca       all-the-icons-completion		)
(elpaca       beacon				)
(elpaca       rainbow-mode			)
(elpaca       rainbow-delimiters	)
(elpaca       diminish				)
(elpaca       org-superstar			)
(elpaca       ox-reveal				)
(elpaca       org-roam				)
(elpaca       pdf-tools				)
(elpaca       ox-pandoc 			)
(elpaca       exec-path-from-shell	)
(elpaca       flycheck				)
(elpaca       go-mode				)
(elpaca       flycheck-clang-analyzer )
(elpaca       irony				    )
(elpaca       slime				    )
(elpaca       json-mode				)
(elpaca       jq-mode				)
(elpaca       json-reformat			)
(elpaca       jq-format				)
(elpaca       verb				    )
(elpaca       ob-mermaid			)
(elpaca       projectile			)
(elpaca       magit				    )
(elpaca       lsp-pyright			)
(elpaca       org-present			)
(elpaca       ox-twbs				)
(elpaca       dired-single			)
(elpaca       hydra				    )
(elpaca       mermaid-mode		 	)
(elpaca       sqlformat				)
(elpaca       restclient			)
(elpaca       ob-restclient			)
(elpaca       request				)
(elpaca       plz				    )
(elpaca       ob-go				    )
(elpaca       jedi				    )
(elpaca       all-the-icons			)
(elpaca       xkcd				    )
(elpaca       emms				    )
(elpaca       emms-state			)
(elpaca       key-chord				)
(elpaca       htmlize				)
(elpaca       plantuml-mode			)
(elpaca       elfeed				)
(elpaca       dwim-shell-command	)
(elpaca       link-hint				)
(elpaca       ace-link				)
(elpaca       easy-kill				)
(elpaca       denote				)
;; (elpaca       transient 		    )
(elpaca       qrencode				)
(elpaca       use-package-ensure-system-package )
(elpaca       ef-themes             )
(elpaca       logos                 )
(elpaca       spacious-padding      )
(elpaca       ess      )

;; ;;custom options for vertico are defined in init-system-utils
;; (straight-use-package
;;  '(vertico :files (:defaults "extensions/*")))

;;custom options for vertico are defined in init-system-utils
(elpaca (vertico :files (:defaults "extensions/*")))

(elpaca (ox-odt :type git :host github :repo "kjambunathan/org-mode-ox-odt"
          :files ("lisp/ox-odt.el"
                           "lisp/odt.el"
                           "etc"
                           "docs"
                           "contrib/odt/LibreOffice")))

(elpaca (lambda-line :type git :host github :repo "lambda-emacs/lambda-line"))

(elpaca (xht :type git :host sourcehut :repo "flandrew/xht"))

(elpaca (org-margin :type git :host github :repo "rougier/org-margin"))

(elpaca-wait)

(elpaca-process-queues)

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


(require 'epa-file)
(epa-file-enable)

(require 'init-programming)

(require 'init-system-utils)

(load-file "~/.emacs.d/custom-macros.el")

(require 'sqlite-mode-extras)


;; (when (file-readable-p "~/.emacs.d/config.org")
;;   (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

;; (setenv "GPG_AGENT_INFO" nil)

(setq epa-pinentry-mode 'loopback)

(put 'narrow-to-region 'disabled nil)
