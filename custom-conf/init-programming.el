;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; * Programming                                                        ;;
;; ;; Minor, non-completion related settings and plugins for writing code. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package exec-path-from-shell
  :straight t
  :if ak/my-mac-p
  :config
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;
;; ;; ** yasnippet ;;
;;;;;;;;;;;;;;;;;;;;;

(use-package yasnippet
  :straight t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode)
  :config
  (use-package yasnippet-snippets
    :straight t)
  (yas-reload-all))


;;;;;;;;;;;;;;;;;;;;
;; ;; ** flycheck ;;
;;;;;;;;;;;;;;;;;;;;

(use-package flycheck
  :diminish
  :straight t)


;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; ** company mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :straight t
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 1))


;; ** Specific languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; *** lspmode settings                         ;;
;; ;; Don't know enough about these to tweak much. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :straight t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :commands lsp lsp-deferred
  :hook (
         (go-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (rustic-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  )

;; Optional - provides fancier overlays

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  ;;  :config (setq lsp-ui-doc-enable t)
  :commands lsp-ui-mode
  )

;; optionally if you want to use debugger
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(setq lsp-ui-doc-enable t
      lsp-ui-peek-enable t
      lsp-ui-sideline-enable t
      lsp-ui-imenu-enable t
      lsp-ui-flycheck-enable t)


;;;;;;;;;;;;;;;;;;;
;; ;; *** Golang ;;
;;;;;;;;;;;;;;;;;;;

(use-package go-mode
  :straight t
  :after lsp-mode
  :config
  ;; (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-enabled-clients 'gopls))

(setq lsp-gopls-staticcheck t
      lsp-eldoc-render-all t
      lsp-gopls-complete-unimported t)

;; set up before-save hooks to ensure buffer formatting and aa/delete imports
;; Make sure there are no other gofmt/goimports hooks enabled

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(with-eval-after-load 'company
  (add-hook 'go-mode-hook 'company-mode))

(use-package ob-go
  :straight t)


;;;;;;;;;;;;;;;;;;
;; ;;  c/c++ ;;
;;;;;;;;;;;;;;;;;;

(add-hook 'c++-mode-hook 'yas-minor-mode)
(add-hook 'c-mode-hook 'yas-minor-mode)

(use-package flycheck-clang-analyzer
  :straight t
  :config
  (with-eval-after-load 'flycheck
    (require 'flycheck-clang-analyzer)
    (flycheck-clang-analyzer-setup)))

(with-eval-after-load 'company
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'c-mode-hook 'company-mode))

(use-package company-c-headers
  :straight t)

(use-package company-irony
  :straight t
  :config
  (setq company-backends '((company-c-headers
                            company-dabbrev-code
                            company-irony))))

(use-package irony
  :diminish
  :straight t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))


;;;;;;;;;;;;;;;;;;;
;; ;; *** python ;;
;;;;;;;;;;;;;;;;;;;

(use-package lsp-jedi
  :straight t
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'pyls)
    (add-to-list 'lsp-enabled-clients 'jedi)))

(add-hook 'python-mode-hook 'yas-minor-mode)
(add-hook 'python-mode-hook 'flycheck-mode)

(with-eval-after-load 'company
  (add-hook 'python-mode-hook 'company-mode))


(defun python-mode-company-init ()
  (setq-local company-backends '((company-jedi
                                  company-etags
                                  company-dabbrev-code))))

(use-package company-jedi
  :straight t
  :config
  (require 'company)
  (add-hook 'python-mode-hook 'python-mode-company-init))


;;;;;;;;;;;;;;;;;;;;;;;
;; ;;  emacs-lisp ;;
;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'yas-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'company-mode)

(use-package slime
  :straight t
  :if ak/my-framework-p
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

(use-package slime-company
  :straight t
  :if ak/my-framework-p
  :init
  (require 'company)
  (slime-setup '(slime-fancy slime-company)))


;; *** bash

(add-hook 'shell-mode-hook 'yas-minor-mode)
(add-hook 'shell-mode-hook 'flycheck-mode)
(add-hook 'shell-mode-hook 'company-mode)

(defun shell-mode-company-init ()
  (setq-local company-backends '((company-shell
                                  company-shell-env
                                  company-etags
                                  company-dabbrev-code))))

(use-package company-shell
  :straight t
  :config
  (require 'company)
  (add-hook 'shell-mode-hook 'shell-mode-company-init))


;;;;;;;;;;;;;;;;;
;; ;; *** json ;;
;;;;;;;;;;;;;;;;;


(use-package json-mode
  :straight t
  :mode (("\\.json\\'" . json-mode)
         ("\\.tmpl\\'" . json-mode)
         ("\\.eslintrc\\'" . json-mode))
  :config (setq-default js-indent-level 2))

(use-package jq-mode
  :straight t
  :mode (("\\.jq$" . jq-mode)))

(with-eval-after-load "json-mode"
  (define-key json-mode-map (kbd "C-c C-j") #'jq-interactively))


(use-package json-reformat
  :straight t
  :after json-mode
  :bind (("C-c f" . json-reformat-region)))

(use-package jq-format
  :straight t
  :demand t
  :after json-mode)    

(require 'json-snatcher)

(defun ak/jq-print-path ()
  "Print the jq-path to the JSON value under point, and save it in the kill ring."
  (interactive)
  (jsons-print-path-jq))

;; *** Restclient
(use-package restclient
  :straight t)
(use-package company-restclient
  :straight t)

(use-package ob-restclient
  :straight t)

(with-eval-after-load 'company
  (add-hook 'restclient-mode-hook 'company-mode))

(add-to-list 'company-backends 'company-restclient)


;;;;;;;;;;;;;;;;;
;; ;; *** Rust ;;
;;;;;;;;;;;;;;;;;

(use-package rustic
  :straight t
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-enabled-clients 'rust-analyzer))
  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; SQL                                                                               ;;
;; ;; Requires the installation of pip package =sqlparse= using =pip3 install sqlparse= ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sqlparse-region (beg end)
  (interactive "r")
  (shell-command-on-region
   beg end
   ;;   "python3 -c 'import sys, sqlparse; print(sqlparse.format(sys.stdin.read(), identifiers = \"upper\", reindent = True, comma_first = True, indent_columns = True))'"
   "sqlformat --keywords \"upper\" --reindent --indent_columns - "
   t t))


(use-package sqlformat
  :straight t)

;;;;;;;;;;;;
;; ;; awk ;;
;;;;;;;;;;;;

;; (add-hook 'awk-mode-hook (lambda()
;;                            (require 'live-awk)
;;                            (live-awk-mode 1)))
;; (require 'awk-it)


;;;;;;;;;;;;;;;;
;; ;; Mermaid ;;
;;;;;;;;;;;;;;;;
(use-package mermaid-mode
  :straight t)

(use-package ob-mermaid
  :straight t
  :if ak/my-framework-p
  :init (setq ob-mermaid-cli-path "~/.nvm/versions/node/v19.5.0/bin/mmdc"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; * Projectile                                                                       ;;
;; ;; Projectile is an awesome project manager, mostly because it recognizes directories ;;
;; ;; with a =.git= directory as projects and helps you manage them accordingly.         ;;
;;                                                                                       ;;
;; ;; ** Enable projectile globally                                                      ;;
;; ;; This makes sure that everything can be a project.                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :straight t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

;; ** Let projectile call make

(global-set-key (kbd "<f5>") 'projectile-compile-project)

;; * Magit 
;; magit is great. It's easy and intuitive to use, shows its options at a keypress and much more.

(use-package magit
  :straight t
  :config
  (setq magit-push-always-verify nil
        git-commit-summary-max-length 50)
  :bind (:map ak-map
              ("g" . magit-status)))

;;;;;;;;;;;;;
;; verb.el ;;
;;;;;;;;;;;;;

(use-package verb
  :straight t
  :mode ("\\.org\\'" . org-mode)
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))



(provide 'init-programming)
