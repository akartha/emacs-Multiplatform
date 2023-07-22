;;; -*- lexical-binding: t; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; * Programming                                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'project)

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)


;;;;;;;;;;;
;; ELDOC ;;
;;;;;;;;;;;

(use-package eldoc
  :hook (after-init . global-eldoc-mode))


;;;;;;;;;;;;;;;;;;
;; EGLOT config ;;
;;;;;;;;;;;;;;;;;;

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
  (setq-default eglot-workspace-configuration
                '((:pylsp . 
                                (:configurationSources ["flake8"] :plugins
                                                       (:pycodestyle (:enabled nil) :mccabe (:enabled nil) :flake8 (:enabled t) :yapf: (:enabled t))))))
                       ;; ((:gopls .
                       ;;          ((staticcheck . t)
                       ;;           (usePlaceholders .t)
                       ;;           (hoverKind . "FullDocumentation")
                       ;;           (matcher . "Fuzzy"))))))
  :hook
  ((python-mode . eglot-ensure)
   (go-mode . eglot-ensure)
   (rust-mode . eglot-ensure)))

(defun eglot-format-buffer-on-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
(add-hook 'go-mode-hook #'eglot-format-buffer-on-save)

;;;;;;;;;;;;;
;; GO-MODE ;;
;;;;;;;;;;;;;
(defun ak/eglot-organize-imports ()
  (call-interactively 'eglot-code-action-organize-imports))

(use-package go-mode
    :hook (go-mode . (lambda ()
                       ;; Using depth -10 will put this before eglot's
                       ;; willSave notification so that the notification
                       ;; reports the actual contents that will be
                       ;; saved.
                       (add-hook 'before-save-hook 'eglot-format-buffer -10 t)
                       (add-hook 'before-save-hook 'ak/eglot-organize-imports nil t))))



;;;;;;;;;;;;;;;;;;;;
;; ;; ** flycheck ;;
;;;;;;;;;;;;;;;;;;;;

(use-package flycheck
  :diminish)

;;;;;;;;;;;;;;;;;;;;;;;
;; ;;  emacs-lisp ;;
;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
;; (add-hook 'emacs-lisp-mode-hook 'yas-minor-mode)

(use-package slime
  :if ak/my-framework-p
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))


;;;;;;;;;;;;;;;;;;;;;;;
;; *** bash
;;;;;;;;;;;;;;;;;;;;;;;

;; (add-hook 'shell-mode-hook 'yas-minor-mode)
(add-hook 'shell-mode-hook 'flycheck-mode)

;;;;;;;;;;;;;;;;;
;; ;; *** json ;;
;;;;;;;;;;;;;;;;;

(define-prefix-command 'ak-json-manipulation-map)
(global-set-key (kbd "` j") 'ak-json-manipulation-map)

(use-package json-mode
  :mode (("\\.json\\'" . json-mode)
         ("\\.js\\'" . json-mode)
         ("\\.tmpl\\'" . json-mode)
         ("\\.eslintrc\\'" . json-mode))
  :config (setq-default js-indent-level 2)
  :bind
  (:map ak-json-manipulation-map
        ("b" . json-mode-beautify)
        ("s" . json-mode-show-path)
        ("w" . json-mode-kill-path)
        ("t" . json-toggle-boolean)
        ("n" . json-nullify-sexp)
        ("+" . json-increment-number-at-point)
        ("-" . json-decrement-number-at-point)))

(use-package jq-mode
  :mode (("\\.jq$" . jq-mode)))

(with-eval-after-load "json-mode"
  (define-key json-mode-map (kbd "C-c C-j") #'jq-interactively))


(use-package json-reformat
  :after json-mode
  :bind (("C-c f" . json-reformat-region)))

(use-package jq-format
  :demand t
  :after json-mode)

;;;;;;;;;;;;;;;;;
;; ;; *** Rust ;;
;;;;;;;;;;;;;;;;;

(use-package rustic
  ;; :bind (:map rustic-mode-map
  ;;             ("M-j" . lsp-ui-imenu)
  ;;             ("M-?" . lsp-find-references)
  ;;             ("C-c C-c l" . flycheck-list-errors)
  ;;             ("C-c C-c a" . lsp-execute-code-action)
  ;;             ("C-c C-c r" . lsp-rename)
  ;;             ("C-c C-c q" . lsp-workspace-restart)
  ;;             ("C-c C-c Q" . lsp-workspace-shutdown)
  ;;             ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)
  ;; (with-eval-after-load "lsp-mode"
  ;;   (add-to-list 'lsp-enabled-clients 'rust-analyzer))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; * Projectile                                                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

;; ** Let projectile call make

(global-set-key (kbd "<f5>") 'projectile-compile-project)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  * Magit                                        ;;
;; Magit is great. It's easy and intuitive to use, ;;
;; shows its options at a keypress and much more.  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :config
  (setq magit-push-always-verify nil
        git-commit-summary-max-length 50)
  :bind (:map ak-map
              ("g" . magit-status)))

;;;;;;;;;;;;;
;; verb.el ;;
;;;;;;;;;;;;;

(use-package verb
  ;; :straight t
  :mode ("\\.org\\'" . org-mode)
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom functions to make life a little easier ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'json-snatcher)

(defun ak/jq-print-path ()
  "Print the jq-path to the JSON value under point, and save it in the kill ring."
  (interactive)
  (jsons-print-path-jq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; SQL                                                                 
;; ;; Requires the installation of pip package
;; ;; =sqlparse= using =pip3 install sqlparse= ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sqlparse-region (beg end)
  (interactive "r")
  (shell-command-on-region
   beg end
   ;; "python3 -c 'import sys, sqlparse; print(sqlparse.format(sys.stdin.read(), identifiers = \"upper\", reindent = True, comma_first = True, indent_columns = True))'"
   "sqlformat --keywords \"upper\" --reindent --indent_columns - "
   t t))

(use-package ob-mermaid
  :if ak/my-framework-p
  :init (setq ob-mermaid-cli-path "~/.nvm/versions/node/v19.5.0/bin/mmdc"))

(provide 'init-programming)
