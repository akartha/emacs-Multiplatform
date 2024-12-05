;;; -*- lexical-binding: t; -*-

;; (require 'projectile)
(require 'crm)

;;;;;;;;;;;;;;;;
;; ** Vertico ;;
;;;;;;;;;;;;;;;;

(use-package vertico
  :ensure (:files (:defaults "extensions/*"))
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
  :bind (("C-x /" . vertico-suspend)
         (:map vertico-map
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
              ("C-l"     . embark-export))))

;; A few more useful bits...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  ;;;###autoload
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
         '(;;(jinx-correct reverse indexed)
           (jinx grid (vertico-grid-annotate . 20))
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
           (org-roam-node-find (vertico-sort-function . vertico-sort-history-alpha) reverse)
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
         ("'" . vertico-quick-jump)
         ("C-'" . vertico-quick-embark))
  :config
;;;###autoload
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
  :ensure t
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
         ;; ("M-s m" . consult-multi-occur)
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
  ;; (setq register-preview-delay 0.5
  ;;       register-preview-function #'consult-register-format)

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
  ;; (setq consult-preview-key (list :debounce 0.5 'any))
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme consult-imenu consult-imenu-multi :preview-key '(:debounce 0.2 any)
   consult-buffer consult-recent-file consult-mark consult-global-mark :preview-key '(:debounce 0.4 any)
   consult-ripgrep consult-git-grep consult-grep consult-bookmark consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.75 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root))))


;;;;;;;;;;;;;;;
;; ** Embark ;;
;;;;;;;;;;;;;;;
(use-package embark
  :ensure t
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
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;;;;;;;;;;;;;;;;
;; ** Orderless ;;
;;;;;;;;;;;;;;;;;;

(use-package orderless
  :ensure t
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
                                        (symbol (styles +orderless-with-initialism))
                                        (eglot (styles orderless))
                                        (eglot-capf (styles orderless)))
        orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
        orderless-style-dispatchers (list #'+orderless-consult-dispatch
                                          #'orderless-affix-dispatch)))
 
;;;;;;;;;;;;;;;;;;;
;; ** Marginalia ;;
;;;;;;;;;;;;;;;;;;;

(use-package marginalia
  :ensure t
  :after vertico
  ;; :straight t
  :custom (marginalia-annotators '(marginalia-annottators-heavy marginalia-annottators-light nil))
  :init (marginalia-mode))

;;;;;;;;;;;
;; DIRED ;;
;;;;;;;;;;;

(use-package dired
  :ensure nil
  :init (if ak/my-mac-p 
            (setq insert-directory-program "gls" 
                  dired-use-ls-dired t))
  :custom  ((dired-listing-switches "-agho --group-directories-first --time-style=long-iso")
            (dired-recursive-copies 'always)
            (dired-recursive-deletes 'always)
            (dired-dwim-target t)
            (dired-free-space nil))
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window)
         (:map dired-mode-map
               ("<f6>" . crux-open-with))))
  ;; :config
  ;; ;;disabling this as I was seeing delays when autorevert mode is on
  ;; (setq dired-free-space nil))

(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package dired-open
  :ensure t
  :config
  (when ak/my-framework-p
    (setq dired-open-extensions '(("png" . "feh")
                                  ("m4a" . "mpv")
                                  ("mkv" . "vlc")
                                  ("mp4" . "vlc")
                                  ("mp3" . "mpv")
                                  ("avi" . "vlc")
                                  ("opus" . "mpv")))))

;; (use-package dired-sidebar
;;   :ensure t
;;   :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
;;   :commands (dired-sidebar-toggle-sidebar)
;;   :init
;;   (add-hook 'dired-sidebar-mode-hook
;;             (lambda ()
;;               (unless (file-remote-p default-directory)
;;                 (auto-revert-mode))))
;;   :config
;;   (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
;;   (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

;;   (setq dired-sidebar-subtree-line-prefix "__")
;;   (setq dired-sidebar-theme 'nerd)
;;   (setq dired-sidebar-use-term-integration t)
;;   (setq dired-sidebar-use-custom-font t))


;; Async
(use-package async
  :ensure t
  :init (dired-async-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Dashboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dashboard
  :ensure t
  :custom (visual-line-mode t)
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 5)
                          (agenda . 10)
                          (bookmarks  . 10)
                          (registers . 5)
                          (projects  . 10)))
  (setq dashboard-set-heading-icons t
        dashboard-center-content t
        dashboard-set-file-icons t
        dashboard-set-navigator nil
        dashboard-week-agenda t)
  (if (executable-find "fortune")
      ;; (or ak/my-framework-p ak/my-mac-p) 
      (setq dashboard-footer-messages (list (shell-command-to-string "fortune"))))

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
  :ensure t
  :diminish
  :config
  (if ak/my-pi-p
      (which-key-setup-side-window-bottom)
    (which-key-setup-side-window-right-bottom)) ;;prefer right side - but will go for bottom if there is not enough space
  (which-key-mode))
  
;;;;;;;;;;;
;; CORFU ;;
;;;;;;;;;;;
(use-package corfu
  :ensure t
  ;; Optional customizations
  ;; :after eglot 
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto nil)                 ;; Disable  auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-no-match 'separator) 
  (corfu-scroll-margin 5)        ;; Use scroll margin
  (corfu-auto-delay 0.4)      ;; delay for a bit so that the popup menu isnt constantly flashing
  (corfu-min-width 70)        ;; have a comfortable display 
  (corfu-max-width corfu-min-width) ;; have that comfortable display be constant size
  (corfu-count 14)
  (corfu-popupinfo-delay '(1.25 . 0.5))
  :hook 
  (((prog-mode text-mode shell-mode eshell-mode minibuffer-setup) . corfu-mode)
  (prog-mode . corfu-popupinfo-mode)
  ((prog-mode minibuffer-setup) . corfu-history-mode))
  :bind
  ;; Another key binding can be used, such as S-SPC.
  (:map corfu-map 
        ("<tab>" . corfu-complete)
        ("M-n" . corfu-next)
        ("M-p" . corfu-previous)
        ("M-SPC" . corfu-insert-separator)
        ;; ("M-d" . corfu-show-documentation)
        ;; ("M-l" . corfu-show-location)
        ("<return>" . corfu-insert)
        ("<escape>" . corfu-quit))
  :config
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history))
      (corfu-popupinfo-mode 1))


  (use-package corfu-candidate-overlay
    :ensure t
    :after corfu
    :config
    ;; enable corfu-candidate-overlay mode globally
    ;; this relies on having corfu-auto set to nil
    (corfu-candidate-overlay-mode +1)
    ;; bind Ctrl + TAB to trigger the completion popup of corfu
    (global-set-key (kbd "C-<tab>") 'completion-at-point)
    ;; bind Ctrl + Shift + Tab to trigger completion of the first candidate
    ;; (keybing <iso-lefttab> may not work for your keyboard model)
    (global-set-key (kbd "<backtab>") 'corfu-candidate-overlay-complete-at-point))



(use-package kind-icon 
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;;;;;;;;;;
;; CAPE ;;
;;;;;;;;;;
(use-package cape
  :ensure t
  ;; Bind rededicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :after corfu 
  :bind (("M-p p" . completion-at-point) ;; capf
         ("M-p t" . complete-tag)        ;; etags
         ("M-p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("M-p e" . cape-elisp-block)
         ("M-p h" . cape-history)
         ("M-p f" . cape-file)
         ("M-p k" . cape-keyword)
         ;; ("M-p s" . cape-symbol) 
         ("M-p a" . cape-abbrev)
         ("M-p l" . cape-line)
         ("M-p w" . cape-dict)
         ("M-p \\" . cape-tex)
         ("M-p &" . cape-sgml)
         ("M-p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; NOTE: The order matters!
  ;; (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-keyword) ;;;; `cape-keyword': Complete programming language keyword  
  (add-to-list 'completion-at-point-functions #'cape-dabbrev) ;;Complete word from current buffers
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;; :hook (prog-mode . cape-mode))
  ;; (add-to-list 'completion-at-point-functions #'cape-file) ;;;; Complete file name
  (add-to-list 'completion-at-point-functions #'cape-elisp-block) ;;;; `cape-elisp-block': Complete Elisp in Org or Markdown code block.


  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))
  ;; (add-to-list 'completion-at-point-functions #'cape-history) ;;;; `cape-history': Complete from Eshell, Comint or minibuffer history
  ;;(add-to-list 'completion-at-point-functions #'cape-tex) 
  ;; (add-to-list 'completion-at-point-functions #'cape-sgml)
  ;; (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-symbol))
;;(add-to-list 'completion-at-point-functions #'cape-line)

;;;;;;;;;;;;
;; Popper ;;
;;;;;;;;;;;;
(use-package popper
  :ensure t
  :bind (("C-`"   . popper-toggle)
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
  (setq tab-always-indent 'complete
        tab-first-completion 'word-or-paren-or-punct)
:custom
  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil))


;;;;;;;;;;;;;;;;
;; ACE Window ;;
;;;;;;;;;;;;;;;;

(use-package ace-window
  :ensure t
  :bind
  ("C-x o" . ace-window)
  ("M-o" . other-window)
  :config
  (setq aw-dispatch-always t
        aw-scope 'global
        aw-background nil
        aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
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




(provide 'init-system-utils)
