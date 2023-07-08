;;; -*- lexical-binding: t; -*-

(require 'projectile)


;;;;;;;;;;;;;;;;
;; ** Vertico ;;
;;;;;;;;;;;;;;;;

(use-package vertico
  :init
  (vertico-mode 1)
  ;; Different scroll margin
  (setq vertico-scroll-margin 1)
  ;; Show more candidates
  (setq vertico-count 10)
  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  :custom
  (setq vertico-cycle t)
  :commands
  vertico-mode
  :after minibuffer 
  :bind (:map vertico-map
              ("M-RET"   . nil)
              ("M-s"     . nil)
              ("<tab>"     . vertico-insert)
              ("C-M-n"   . vertico-next-group)
              ("C-M-p"   . vertico-previous-group)
              ("C-j"     . (lambda () (interactive)
	        	             (if minibuffer--require-match
	        	                 (minibuffer-complete-and-exit)
	        	               (exit-minibuffer))))
              ("C->"     . embark-become)
              (">"       . embark-become)
              ("C-*"     . embark-act-all)
              ("M-s o"   . embark-export)
              ("C-l"     . embark-export)))

;; A few more useful bits...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-multiform
  :commands vertico-multiform-mode 
  :after vertico-flat 
  :bind (:map vertico-map
              ("M-F" . vertico-multiform-flat)
              ("M-U" . vertico-multiform-unobtrusive)
              ("M-R" . vertico-multiform-reverse)
              ("M-G" . vertico-multiform-grid)
              ("C-M-l" . embark-export))
  :init (vertico-multiform-mode 1)
  :config
  (setq vertico-multiform-categories
         '((file reverse indexed)
           (project-file grid)
           (imenu buffer)
           (consult-location buffer indexed)
           (consult-grep buffer indexed)
           (consult-ripgrep buffer indexed)
           (minor-mode reverse)
           (xref-location reverse)
           (history reverse)
           (url reverse)
           (consult-info buffer)
           (kill-ring reverse indexed)
           (consult-compile-error reverse)
           (buffer reverse indexed)
           (org-roam-node reverse indexed)
           (t grid indexed)))
   (setq vertico-multiform-commands
         '((jinx-correct reverse indexed)
           (tab-bookmark-open reverse)
           (dired-goto-file grid indexed)
           (load-theme grid reverse)
           (org-refile reverse)
           (org-agenda-refile reverse)
           (org-capture-refile reverse)
           (execute-extended-command reverse indexed)
           (dired-goto-file grid)
           (consult-project-buffer grid)
           (consult-dir-maybe reverse)
           (consult-dir reverse)
           (consult-flymake reverse)
           (consult-history reverse)
           (consult-completion-in-region reverse)
           (consult-recoll reverse)
           (consult-line buffer)
           (completion-at-point reverse)
           (org-roam-node-find reverse)
           (embark-completing-read-prompter reverse)
           (embark-act-with-completing-read reverse)
           (embark-prefix-help-command reverse)
           (embark-bindings reverse)
           (consult-org-heading reverse)
           (embark-find-definition reverse)
           (xref-find-definitions reverse)
           (tmm-menubar reverse))))

(use-package vertico-unobtrusive
  :after vertico-flat)

(use-package vertico-grid
  :after vertico
  ;; :bind (:map vertico-map ("M-q" . vertico-grid-mode))
  :config
  (setq vertico-grid-separator "    ")
  (setq vertico-grid-lookahead 50))

(use-package vertico-quick
  :after vertico
  :bind (:map vertico-map
         ("M-i" . vertico-quick-insert)
         ("'" . vertico-quick-exit)
         ("C-'" . vertico-quick-embark))
  :config
  (defun vertico-quick-embark (&optional arg)
    "Embark on candidate using quick keys."
    (interactive)
    (when (vertico-quick-jump)
      (embark-act arg))))

(use-package vertico-repeat
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind (("C-x ." . vertico-repeat))
  :config
    (add-to-list 'savehist-additional-variables 'vertico-repeat-history))

(use-package vertico-reverse
  ;; :disabled
  :after vertico)

(use-package vertico-flat
  ;; :bind (:map vertico-map
  ;;             ("M-q" . vertico-flat-mode))
  :after vertico)

(use-package vertico-buffer
  :after vertico
  ;; :hook (vertico-buffer-mode . vertico-buffer-setup)
  :config
  (setq vertico-buffer-display-action 'display-buffer-reuse-window))

;;;;;;;;;;;;;;;;
;; ** Consult ;;
;;;;;;;;;;;;;;;;

(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root))))


;;;;;;;;;;;;;;;
;; ** Embark ;;
;;;;;;;;;;;;;;;
(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;;;;;;;;;;;;;;;;
;; ** Orderless ;;
;;;;;;;;;;;;;;;;;;

(use-package orderless
  :demand t
  :config
  ;; below is from https://github.com/minad/consult/wiki
   (defun +orderless--consult-suffix ()
    "Regexp which matches the end of string with Consult tofu support."
    (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
        (format "[%c-%c]*$"
                consult--tofu-char
                (+ consult--tofu-char consult--tofu-range -1))
      "$"))

  ;; Recognizes the following patterns:
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun +orderless-consult-dispatch (word _index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" word)
      `(orderless-regexp . ,(concat (substring word 0 -1) (+orderless--consult-suffix))))
     ;; File extensions
     ((and (or minibuffer-completing-file-name
               (derived-mode-p 'eshell-mode))
           (string-match-p "\\`\\.." word))
      `(orderless-regexp . ,(concat "\\." (substring word 1) (+orderless--consult-suffix))))))

  ;; Define orderless style with initialism by default
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))
  
  ;; :init
  ;; ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;; ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  ;; (setq completion-styles '(orderless basic)
  ;;       completion-category-defaults nil
  ;;       completion-category-overrides '((file (styles orderless basic partial-completion)))))
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        ;;; Enable partial-completion for files.
        ;;; Either give orderless precedence or partial-completion.
        ;;; Note that completion-category-overrides is not really an override,
        ;;; but rather prepended to the default completion-styles.
        completion-category-overrides '((file (styles orderless partial-completion)) ;; orderless is tried first
        ;; completion-category-overrides '((file (styles partial-completion)) ;; partial-completion is tried first
                                        ;; enable initialism by default for symbols
                                        (command (styles +orderless-with-initialism))
                                        (variable (styles +orderless-with-initialism))
                                        (symbol (styles +orderless-with-initialism)))
        orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
        orderless-style-dispatchers (list #'+orderless-consult-dispatch
                                          #'orderless-affix-dispatch)))
 
;;;;;;;;;;;;;;;;;;;
;; ** Marginalia ;;
;;;;;;;;;;;;;;;;;;;

(use-package marginalia
  :after vertico
  ;; :straight t
  :custom (marginalia-annotators '(marginalia-annottators-heavy marginalia-annottators-light nil))
  :init (marginalia-mode))

;;;;;;;;;;;
;; DIRED ;;
;;;;;;;;;;;

(use-package dired
  :init (if ak/my-mac-p 
            (setq insert-directory-program "gls" 
                  dired-use-ls-dired t))
  :custom  ((dired-listing-switches "-agho --group-directories-first --time-style=long-iso")
            (dired-recursive-copies 'always)
            (dired-recursive-deletes 'always)
            (dired-dwim-target t))
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window))
  :config
  ;;disabling this as I was seeing delays when autorevert mode is on
  (setq dired-free-space nil))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package dired-open
  :config
  (when ak/my-framework-p
    (setq dired-open-extensions '(("png" . "feh")
                                  ("m4a" . "mpv")
                                  ("mkv" . "vlc")
                                  ("mp4" . "vlc")
                                  ("mp3" . "mpv")
                                  ("avi" . "vlc")
                                  ("opus" . "mpv")))))

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'nerd)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))


;; Async
(use-package async
  :init (dired-async-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Dashboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dashboard
  :custom (visual-line-mode t)
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((bookmarks  . 10)
                          (recents  . 20)
                          (projects  . 10)))
  (setq dashboard-set-heading-icons t
        dashboard-center-content t
        dashboard-set-file-icons t
        dashboard-set-navigator nil)
  (if (or ak/my-framework-p ak/my-mac-p) (setq dashboard-footer-messages (list (shell-command-to-string "fortune"))))
  (add-hook 'dashboard-mode-hook (lambda ()
                                   (visual-line-mode 1)))
  :bind (:map ak-map
              ("1" . (lambda ()
                       (interactive)
                       (switch-to-buffer "*dashboard*")))))

 ;;;;;;;;;;;;;;;;;;
 ;; ** Which-Key ;;
 ;;;;;;;;;;;;;;;;;;

(use-package which-key
  :diminish
  :config
  (which-key-setup-side-window-right-bottom) ;;prefer right side - but will go for bottom if there is not enough space
  (which-key-mode))

;;;;;;;;;;;
;; CORFU ;;
;;;;;;;;;;;
(use-package corfu
  ;; Optional customizations
  :after eglot 
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-no-match 'separator) 
  (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  :hook ((prog-mode . corfu-mode)
         (org-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode)
         (eglot . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-exclude-modes'.
  ;; :config 
  ;; (global-corfu-mode))
  :bind
  ;; Another key binding can be used, such as S-SPC.
  (:map corfu-map 
        ("M-SPC" . corfu-insert-separator)
        ("<escape>" . corfu-quit)))

(use-package kind-icon 
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;;;;;;;;;;
;; CAPE ;;
;;;;;;;;;;
(use-package cape
  ;; Bind rededicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("M-p p" . completion-at-point) ;; capf
         ("M-p t" . complete-tag)        ;; etags
         ("M-p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("M-p h" . cape-history)
         ("M-p f" . cape-file)
         ("M-p k" . cape-keyword)
         ("M-p s" . cape-symbol) 
         ("M-p a" . cape-abbrev)
         ("M-p l" . cape-line)
         ("M-p w" . cape-dict)
         ("M-p \\" . cape-tex)
         ("M-p _" . cape-tex)
         ("M-p ^" . cape-tex)
         ("M-p &" . cape-sgml)
         ("M-p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; NOTE: The order matters!
  ;; (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-file) ;;;; Complete file name

  (add-to-list 'completion-at-point-functions #'cape-elisp-block) ;;;; `cape-elisp-block': Complete Elisp in Org or Markdown code block.

  ;; (add-to-list 'completion-at-point-functions #'cape-history) ;;;; `cape-history': Complete from Eshell, Comint or minibuffer history
  (add-to-list 'completion-at-point-functions #'cape-dabbrev) ;;Complete word from current buffers
  (add-to-list 'completion-at-point-functions #'cape-keyword) ;;;; `cape-keyword': Complete programming language keyword
  ;;(add-to-list 'completion-at-point-functions #'cape-tex) 
  ;; (add-to-list 'completion-at-point-functions #'cape-sgml)
  ;; (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)
;;;;;;;;;;;;
;; Popper ;;
;;;;;;;;;;;;
(use-package popper
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "[Oo]utput\\*$"
          "\\*Async Shell Command\\*"
          "^\\*Backtrace\\*"
          "\\*Completions\\*"
          "^\\*ielm\\*"
          "^Calc:"
          ("\\*Async Shell Command\\*" . hide)
          ("^\\*Warnings\\*$" . hide)
          ("^\\*Compile-Log\\*$" . hide)
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))


;;;;;;;;;;;;;;;;
;; ACE Window ;;
;;;;;;;;;;;;;;;;

(use-package ace-window
  :bind
  ("C-x o" . ace-window)
  ("M-o" . other-window)
  :config
  (setq aw-dispatch-always t
        aw-scope 'global
        aw-background nil
        aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-dispatch-alist
        '((?k aw-delete-window "Delete Window")
          (?m aw-swap-window "Swap Windows")
          (?M aw-move-window "Move Window")
          (?c aw-copy-window "Copy Window")
          (?j aw-switch-buffer-in-window "Select Buffer")
          (?\t aw-flip-window)
          (?b aw-switch-buffer-other-window "Switch Buffer Other Window")
          (?c aw-split-window-fair "Split Fair Window")
          (?s aw-split-window-vert "Split Vert Window")
          (?v aw-split-window-horz "Split Horz Window")
          (?o delete-other-windows "Delete Other Windows")
          (?? aw-show-dispatch-help)))
  (defun my/other-window-prev (&optional arg all-frames)
    (interactive "p")
    (other-window (if arg (- arg) -1) all-frames)))

(use-package nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  ;; (defun my-nov-font-setup ()
  ;; (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
  ;;                                          :height 1.5))
  ;; (add-hook 'nov-mode-hook 'my-nov-font-setup)
  ;; (setq nov-text-width 100)
  )



(provide 'init-system-utils)
