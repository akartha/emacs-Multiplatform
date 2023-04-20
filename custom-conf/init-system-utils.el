;;;;;;;;;;;;;;;;
;; ** Vertico ;;
;;;;;;;;;;;;;;;;

(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  (setq vertico-count 15)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  :custom
  (setq vertico-cycle t))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

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

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

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



;;;;;;;;;;;;;;;;
;; ** Consult ;;
;;;;;;;;;;;;;;;;

(use-package consult
  :straight t
  ;; Replace bindings. Lazily loaded due by `use-package'.
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
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )


;;;;;;;;;;;;;;;
;; ** Embark ;;
;;;;;;;;;;;;;;;
    

(use-package embark
  :straight t
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
  :straight t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;;;;;;;;;;;;;;;;
;; ** Orderless ;;
;;;;;;;;;;;;;;;;;;

(use-package orderless
  :straight t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(basic orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic orderless)))))

 
;;;;;;;;;;;;;;;;;;;
;; ** Marginalia ;;
;;;;;;;;;;;;;;;;;;;

(use-package marginalia
  :after vertico
  :straight t
  :custom (marginalia-annotators '(marginalia-annottators-heavy marginalia-annottators-light nil))
  :init (marginalia-mode))

;; ** All the icons in completion

;; (use-package all-the-icons-completion
;;   :straight t 
;;   :after (marginalia all-the-icons)
;;   :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
;;   :init (all-the-icons-completion-mode))


;;;;;;;;;;;
;; DIRED ;;
;;;;;;;;;;;

(use-package dired
  :straight nil
  :custom ((dired-listing-switches "-agho --group-directories-first --time-style=long-iso")
           ;;  :custom ((dired-listing-switches "-agho --group-directories-first --time-style=\'+%Y%m%d %H:%M:%S\'")
           (dired-recursive-copies 'always)
           (dired-recursive-deletes 'always)
           (dired-dwim-target t))
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window)))


(use-package dired-single
  :straight t)
(use-package all-the-icons-dired
  :straight t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-du
  :straight t)
 ;; :hook (dired-mode . dired-du-mode))

(use-package dired-open
  :straight t
  :config
  (setq dired-open-extensions '(("png" . "feh")
                                ("m4a" . "mpv")
                                ("mkv" . "vlc")
                                ("mp4" . "vlc")
                                ("mp3" . "mpv")
                                ("avi" . "vlc")
                                ("opus" . "mpv"))))

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :straight t
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
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))


;; Async
;; Lets us use asynchronous processes wherever possible, pretty useful.

(use-package async
  :straight t
  :init (dired-async-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; This is your new startup screen, together with projectile it works in unison and ;;
;; ;; provides you with a quick look into your latest projects and files.              ;;
;; ;; Change the welcome message to whatever string you want and                       ;;
;; ;; change the numbers to suit your liking                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dashboard
  :straight t
  :custom (visual-line-mode t)
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((bookmarks  . 10)
                          (recents  . 20)
                          (projects  . 10)))
  ;; (agenda  . 5)))
  (setq dashboard-set-heading-icons t
        dashboard-center-content t
        dashboard-set-file-icons t
        dashboard-set-navigator nil
        dashboard-footer-messages (list (shell-command-to-string "fortune")))
  (add-hook 'dashboard-mode-hook (lambda ()
                                   (visual-line-mode 1)))
  :bind (:map ak-map
              ("1" . (lambda ()
                       (interactive)
                       (switch-to-buffer "*dashboard*")))))

;; ** Which-Key

(use-package which-key
  :diminish
  :straight t
  :config
  (which-key-setup-side-window-right-bottom) ;;prefer right side - but will go for bottom if there is not enough space
  (which-key-mode))

(use-package corfu
  :straight t
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-exclude-modes'.
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))


(use-package hydra
  :straight t )

(when ak/my-framework-p
  (defhydra hydra-jump-to-directory
    (:color amaranth
            :timeout 5)
    "Jump to directory"
    ("h" (find-file "~/") "Home")
    ("d" (find-file "~/Documents") "Documents")
    ("v" (find-file "~/Dropbox") "Dropbox")
    ("a" (find-file "~/Dropbox/articles/") "Articles")
    ("e" (find-file "~/.emacs.d/") "Emacs")
    ("c" (find-file "~/.emacs.d/custom-conf/") "Emacs custom config")
    ("s" (find-file "~/scripts/") "Scripts")
    ("p" (find-file "~/projects/") "Projects")
    ("o" (find-file "~/Dropbox/org-files/") "Org Folder")
    ("x" (find-file "~/.emacs.d/xkcd/") "xkcd folder")
    ("q" nil "Quit" :color blue))
  (defhydra hydra-jump-to-config
    (:color amaranth
            :timeout 5)
    "Open Config files"
    ("b" (find-file "~/.bashrc") ".bashrc")
    ("p" (find-file "~/.bash_profile") ".bash_profile")
    ("e" (find-file "~/.emacs.d/init.el") "emacs init")
    ("i" (find-file "~/.i3/config") "i3 config")
    ("q" nil "Quit" :color blue))

  (define-key ak-map "d" 'hydra-jump-to-directory/body)
  (define-key ak-map "c" 'hydra-jump-to-config/body))

(defhydra hydra-move-lines
  (:color amaranth
          :timeout 5)
  "Move selected lines up/down"
  ("[" (ak/move-lines-up 1) "Move up")
  ("]" (ak/move-lines-down 1) "Move down")
  ("q" nil "Quit" :color blue))

(defhydra hydra-launcher (:color blue)
  "Launch"
  ("h" man "man")
  ("n" (browse-url "http://www.nytimes.com/") "nytimes")
  ("w" (browse-url "http://www.emacswiki.org/") "emacswiki")
  ("g" (browse-url "http://www.github.com/") "github")
  ("c" (browse-url "https://chat.openai.com/") "ChatGPT")
  ("x" (browse-url "https://xkcd.com/") "xkcd browser")
  ("s" shell "shell")
  ("X" xkcd "xkcd - emacs")
  ("q" nil "cancel"))

(global-set-key (kbd "C-c r") 'hydra-launcher/body)

(define-key ak-map "m" 'hydra-move-lines/body)
  ;; (global-set-key (kbd "C-c 0") 'hydra-move-lines/body)

;; ;; (require 'rect)
;; (defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
;;                            :color pink
;;                            :post (deactivate-mark))
;;   "
;;   ^_k_^     _d_elete    _s_tring
;; _h_   _l_   _o_k        _y_ank
;;   ^_j_^     _n_ew-copy  _r_eset
;; ^^^^        _e_xchange  _u_ndo
;; ^^^^        ^ ^         _p_aste
;; "
;;   ("h" rectangle-backward-char nil)
;;   ("l" rectangle-forward-char nil)
;;   ("k" rectangle-previous-line nil)
;;   ("j" rectangle-next-line nil)
;;   ("e" hydra-ex-point-mark nil)
;;   ("n" copy-rectangle-as-kill nil)
;;   ("d" delete-rectangle nil)
;;   ("r" (if (region-active-p)
;;            (deactivate-mark)
;;          (rectangle-mark-mode 1)) nil)
;;   ("y" yank-rectangle nil)
;;   ("u" undo nil)
;;   ("s" string-rectangle nil)
;;   ("p" kill-rectangle nil)
;;   ("o" nil nil))

;; ;; Recommended binding:
;; ;; (global-set-key (kbd "C-x SPC") 'hydra-rectangle/body)
;; (define-key ak-map (kbd "<f2>") 'hydra-rectangle/body)

(use-package auto-package-update
  :straight t
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 5
        auto-package-update-prompt-before-update t
        auto-package-update-hide-results t)
  (auto-package-update-maybe))



(provide 'init-system-utils)
