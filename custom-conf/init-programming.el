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


(use-package yasnippet
  :commands yas-minor-mode
  :hook ((go-ts-mode . yas-minor-mode)
         (python-ts-mode . yas-minor-mode)
         (rust-ts-mode . yas-minor-mode)
         (emacs-lisp-mode . yas-minor-mode)
         (sql-mode . yas-minor-mode)
         ;; (html-mode . yas-minor-mode)
         ;; (nxml-mode . yas-minor-mode)
         ;; (css-mode . yas-minor-mode)
         (org-mode . yas-minor-mode))
  :config (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet
  :ensure t)

(use-package eldoc
  :preface (add-to-list 'display-buffer-alist
               '("^\\*eldoc for" display-buffer-at-bottom
                 (window-height . 4)))
  :hook (after-init . global-eldoc-mode)
  :config 
  (setq eldoc-documentation-strategy
            'eldoc-documentation-compose-eagerly))

(use-package htmlize 
  :ensure t)
;;Treesitter modules

(setq treesit-language-source-alist
   '((rust "https://github.com/tree-sitter/tree-sitter-rust")
     (bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")
     (sql "https://github.com/DerekStride/tree-sitter-sql")))

;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(defun ak/treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
	      (treesit-install-language-grammar lang)
	      (message "`%s' parser was installed." lang)
	      (sit-for 0.75))))



(use-package jedi
  :ensure t)

;;;;;;;;;;;;;;;;;;
;; EGLOT config ;;
;;;;;;;;;;;;;;;;;;

(use-package eglot
  :defer t 
  :after (lsp-pyright)
  :commands (eglot eglot-ensure)
  :config
  (tooltip-mode 1)
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  ;; (add-to-list 'eglot-server-programs '(python-ts-mode . ("pylsp")))
  (add-to-list 'eglot-server-programs '(go-ts-mode . ("gopls")))
  (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rust-analyzer" )))

  (setq eglot-verbose t
        eglot-debug t)
  :custom (eglot-workspace-configuration
           '((:pyright . 
                       (:configurationSources ["flake8"] 
                                              :plugins (:pycodestyle (:enabled nil) 
                                                                     :mccabe (:enabled nil) 
                                                                     :pyflakes (:enabled :json-false) 
                                                                     :flake8 (:enabled t :maxLineLength 88)
                                                                     :ruff (:enabled t :lineLength 88)
                                                                     :pydocstyle (:enabled t :convention "numpy")
                                                                     :yapf: (:enabled t)
                                                                     :autopep8 (:enabled :json-false)
                                                                     :jedi_completion (:include_params t :fuzzy t)
                                                                     :pylint (:enabled :json-false))))
             (:gopls .
                     ((staticcheck . t)
                      (usePlaceholders . t)
                      (gofumpt . t)
                      (hoverKind . "FullDocumentation")
                      (importShortcut . "Both")
                      (symbolScope . "all")
                      (completeFunctionCalls . t)
                      (linksInHover . t)
                      (matcher . "Fuzzy")))
             (:rust-analyzer .
                             ( :procMacro ( :attributes ( :enable t)
                                            :enable t)
                               :cargo ( :buildScripts (:enable t)
                                        :features "all")
                               :diagnostics ( :disabled ["unresolved-proc-macro"
                                                         "unresolved-macro-call"])))))
  (read-process-output-max (* 1024 1024))
  (eglot-sync-connect 0)
  (eglot-autoshutdown t)
  :hook ((python-ts-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure))
  :bind (:map eglot-mode-map 
              ("<f6>" . eglot-format-buffer)
              ("C-c a" . eglot-code-actions)
              ("C-c r" . eglot-rename)
              ("C-c d" . eldoc)
              ("<f5>" . recompile )))

;;;###autoload
(defun ak/eglot-format-buffer-on-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
(add-hook 'go-ts-mode-hook #'ak/eglot-format-buffer-on-save)

;;Python pyright 
(use-package lsp-pyright
  :ensure t
  :hook (python-ts-mode . (lambda ()
                          (require 'lsp-pyright))))  ; or lsp

;;;;;;;;;;;;;
;; GO-MODE ;;
;;;;;;;;;;;;;
;;;###autoload
(defun ak/eglot-organize-imports ()
  (call-interactively 'eglot-code-action-organize-imports))

(use-package go-ts-mode
    :hook (go-ts-mode . (lambda ()
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
  :ensure t
  :diminish)

;;;;;;;;;;;;;;;;;;;;;;;
;; ;;  emacs-lisp ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
;; (add-hook 'emacs-lisp-mode-hook 'yas-minor-mode)

(use-package slime
  :if ak/my-framework-p
  :ensure t
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
(global-set-key (kbd "` j") '("JSON mode commands" . ak-json-manipulation-map))

(use-package json-mode
  :ensure t
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
  :ensure t
  :mode (("\\.jq$" . jq-mode)))

(with-eval-after-load "json-mode"
  (define-key json-mode-map (kbd "C-c C-j") #'jq-interactively))


(use-package json-reformat
  :ensure t 
  :after json-mode
  :bind (("C-c f" . json-reformat-region)))

(use-package jq-format
  :ensure t
  ;; :demand t
  :after json-mode)

;;;;;;;;;;;;;;;;;
;; ;; *** Rust ;;
;;;;;;;;;;;;;;;;;

;; (use-package rustic
;;   ;; :bind (:map rustic-mode-map
;;   ;;             ("M-j" . lsp-ui-imenu)
;;   ;;             ("M-?" . lsp-find-references)
;;   ;;             ("C-c C-c l" . flycheck-list-errors)
;;   ;;             ("C-c C-c a" . lsp-execute-code-action)
;;   ;;             ("C-c C-c r" . lsp-rename)
;;   ;;             ("C-c C-c q" . lsp-workspace-restart)
;;   ;;             ("C-c C-c Q" . lsp-workspace-shutdown)
;;   ;;             ("C-c C-c s" . lsp-rust-analyzer-status))
;;   :config
;;   ;; uncomment for less flashiness
;;   ;; (setq lsp-eldoc-hook nil)
;;   ;; (setq lsp-enable-symbol-highlighting nil)
;;   ;; (setq lsp-signature-auto-activate nil)
;;   ;; (with-eval-after-load "lsp-mode"
;;   ;;   (add-to-list 'lsp-enabled-clients 'rust-analyzer))
;;   ;; comment to disable rustfmt on save
;;   (setq rustic-format-on-save t)
;;   (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

;; ;;;###autoload
;; (defun rk/rustic-mode-hook ()
;;   ;; so that run C-c C-c C-r works without having to confirm, but don't try to
;;   ;; save rust buffers that are not file visiting. Once
;;   ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
;;   ;; no longer be necessary.
;;   (when buffer-file-name
;;     (setq-local buffer-save-without-query t)))

(use-package rust-ts-mode
  :mode ("\\.rs\\'" . rust-ts-mode))
  ;; :hook ((rust-ts-mode . company-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; * Projectile                                                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :ensure t 
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

;; ** Let projectile call make

(global-set-key (kbd "<f5>") 'projectile-compile-project)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom functions to make life a little easier ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package json-snatcher
;;   :load-path "custom-conf/third-party/"
;;   :config 
;;   (defun ak/jq-print-path ()
;;     "Print the jq-path to the JSON value under point, and save it in the kill ring."
;;     (interactive)
;;     (jsons-print-path-jq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; SQL                                                                 
;; ;; Requires the installation of pip package
;; ;; =sqlparse= using =pip3 install sqlparse= ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package sqlformat
  :ensure t
  :config 
  (setq sqlformat-command 'sqlformat
        sqlformat-args '("-r" "--indent_columns" "-k" "upper" "-i" "upper")))

;;;###autoload
(defun sqlparse-region (beg end)
  (interactive "r")
  (shell-command-on-region
   beg end
   ;; "python3 -c 'import sys, sqlparse; print(sqlparse.format(sys.stdin.read(), identifiers = \"upper\", reindent = True, comma_first = True, indent_columns = True))'"
   "sqlformat --keywords \"upper\" --reindent --indent_columns - "
   t t))

(use-package mermaid-mode
  :ensure t)

(use-package ob-mermaid
  :ensure t
  :after mermaid-mode
  ;; :if ak/my-framework-p
  :init (if ak/my-framework-p 
             (setq ob-mermaid-cli-path "~/.nvm/versions/node/v19.5.0/bin/mmdc")))


;; see https://xenodium.com/further-sqlite-mode-extensions/ for details
(use-package sqlite-mode-extras
  :load-path "custom-conf/third-party/"
  :bind (:map
         sqlite-mode-map
         ("n" . next-line)
         ("p" . previous-line)
         ("b" . sqlite-mode-extras-backtab-dwim)
         ("f" . sqlite-mode-extras-tab-dwim)
         ("+" . sqlite-mode-extras-add-row)
         ("D" . sqlite-mode-extras-delete-row-dwim)
         ("C" . sqlite-mode-extras-compose-and-execute)
         ("E" . sqlite-mode-extras-execute)
         ("S" . sqlite-mode-extras-execute-and-display-select-query)
         ("DEL" . sqlite-mode-extras-delete-row-dwim)
         ("g" . sqlite-mode-extras-refresh)
         ("<backtab>" . sqlite-mode-extras-backtab-dwim)
         ("<tab>" . sqlite-mode-extras-tab-dwim)
         ("RET" . sqlite-mode-extras-ret-dwim)))


(use-package eldoc
  :custom (eldoc-documentation-strategy #'eldoc-documentation-compose)
  :hook (after-init . global-eldoc-mode)
  :config (eldoc-add-command-completions "paredit-"))

(use-package emmet-mode
  :ensure t
  :hook ((sgml-mode . emmet-mode)
         (css-mode . emmet-mode)))

(provide 'init-programming)
