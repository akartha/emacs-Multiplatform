(use-package modus-themes
  :straight t
  :custom
  (setq modus-themes-org-blocks 'tinted-background)
  :init
  ;; (setq modus-themes-common-palette-overrides
  ;;       '((fg-prompt fg-main)
  ;;         (bg-prompt bg-yellow-nuanced) ; try to replace "nuanced" or "subtle" with "intense"

  ;;         (comment yellow-faint)
  ;;         (string green-warmer)

  ;;         (fg-heading-1 blue-warmer)
  ;;         (bg-heading-1 bg-blue-nuanced)
  ;;         (overline-heading-1 blue)

  ;;         (fg-heading-2 green-warmer)
  ;;         (bg-heading-2 bg-green-nuanced)
  ;;         (overline-heading-2 border)
  
  ;;         (fg-heading-3 fg-main)
  ;;         (bg-heading-3 bg-dim)
  ;;         (overline-heading-3 border))
  
  ;;       modus-themes-completions
  ;;       '((matches . (extrabold underline))
  ;;         (selection . (extrabold italic underline))))

  ;; (load-theme 'modus-vivendi t))
  (load-theme 'modus-vivendi-tinted t)) ;;(load-theme 'modus-operandi-tinted/deuteranopia t))

(use-package fontaine
  :straight t)

(setq fontaine-latest-state-file
      (locate-user-emacs-file "fontaine-latest-state.eld"))


(setq fontaine-presets
      '((regular
         :default-weight light
         :default-height 110)
        (medium
         :default-weight semilight
         :default-height 140)
        (large
         :default-weight semilight
         :default-height 180
         :bold-weight extrabold)
        (t ; our shared fallback properties
         :default-family "Iosevka Comfy"
         :default-weight normal
         ;; :default-height 100
         ;; :fixed-pitch-family nil ; falls back to :default-family
         ;; :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family "FiraGO"
         :variable-pitch-weight normal

  :variable-pitch-height 1.05
         ;; :bold-family nil ; use whatever the underlying face has
         :bold-weight bold
         ;; :italic-family nil
         :italic-slant italic
         :line-spacing nil)))
    
(fontaine-restore-latest-preset)

;; Use `fontaine-recovered-preset' if available, else fall back to the
;; desired style from `fontaine-presets'.
;; (if-let ((state fontaine-recovered-preset))
;;     (fontaine-set-preset state)
;;   (fontaine-set-preset 'regular))
(fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

;; The other side of `fontaine-restore-latest-preset'.
(add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)

(add-hook 'modus-themes-after-load-theme-hook #'fontaine-apply-current-preset)

;; fontaine does not define any key bindings.  This is just a sample
;; that respects the key binding conventions.  Evaluate:
;;
;;     (info "(elisp) Key Binding Conventions")
(define-key global-map (kbd "C-c F") #'fontaine-set-preset)
(define-key global-map (kbd "C-c g") #'fontaine-set-face-font)

(use-package all-the-icons
  :straight t)

(use-package spaceline
  :straight t
  :config
  (require 'spaceline-config)
  (setq spaceline-buffer-encoding-abbrev-p nil
        ;; spaceline-line-column-p nil
        ;; spaceline-line-p nil
        powerline-default-separator (quote utf-8))
  (spaceline-spacemacs-theme))

;;Spaceline is the mode line of choice. looks nice and you can set
;;nice separators. Using the =all-the-icons= package gives you more
;;eye-candy.
(use-package spaceline-all-the-icons
  :straight t
  :after spaceline
  :config
  (setq spaceline-all-the-icons-separator-type 'none)
  (spaceline-all-the-icons-theme))
  ;; (spaceline-all-the-icons--setup-neotree))


(use-package all-the-icons-completion
  :straight t 
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init (all-the-icons-completion-mode))

;; While changing buffers or workspaces, the first thing you do is
;; look for your cursor.  Every time you change buffers, the current
;; position of your cursor will be briefly highlighted
(use-package beacon
  :diminish
  :straight t
  :config
  (beacon-mode 1))

;; Every time emacs encounters a hexadecimal code that resembles a
;; color, it will automatically highlight it in the appropriate color.

(use-package rainbow-mode
  :diminish
  :straight t
  :init
  (add-hook 'prog-mode-hook 'rainbow-mode))

;; Colors parentheses and other delimiters depending on their depth

(use-package rainbow-delimiters
  :diminish
  :straight t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Diminishing modes ;; The package =diminish= disables modes on the
;; mode line but keeps them running, it just prevents them from
;; showing up and taking up space. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package diminish
  :straight t
  :init
  (diminish 'visual-line-mode)
  (diminish 'subword-mode)
  (diminish 'page-break-lines-mode)
  (diminish 'auto-revert-mode)
  (diminish 'yas-minor-mode)
  (diminish 'org-indent-mode))


;; (setq fontaine-presets
;;       '((regular
;;          :default-family "Hack Nerd Font"
;;          :default-weight normal
;;          :default-height 110
;;          :fixed-pitch-family "Fira Code"
;;          :fixed-pitch-weight nil ; falls back to :default-weight
;;          :fixed-pitch-height 1.0
;;          :variable-pitch-family "Noto Sans"
;;          :variable-pitch-weight normal
;;          :variable-pitch-height 1.0
;;          :bold-family nil ; use whatever the underlying face has
;;          :bold-weight bold
;;          :italic-family "Source Code Pro"
;;          :italic-slant italic
;;          :line-spacing 1)
;;         (medium
;;          :default-family "Iosevka Comfy"
;;          :default-weight semilight
;;          :default-height 140
;;          :fixed-pitch-family nil ; falls back to :default-family
;;          :fixed-pitch-weight nil ; falls back to :default-weight
;;          :fixed-pitch-height 1.0
;;          :variable-pitch-family "FiraGO"
;;          :variable-pitch-weight normal
;;          :variable-pitch-height 1.05
;;          :bold-family nil ; use whatever the underlying face has
;;          :bold-weight bold
;;          :italic-family nil
;;          :italic-slant italic
;;          :line-spacing nil)
;;         (large
;;          :default-family "Hack Nerd Font Mono"
;;          ;;         :default-family "Iosevka"
;;          :default-weight semilight
;;          :default-height 180
;;          :fixed-pitch-family nil ; falls back to :default-family
;;          :fixed-pitch-weight nil ; falls back to :default-weight
;;          :fixed-pitch-height 1.0
;;          :variable-pitch-family "FiraGO"
;;          :variable-pitch-weight normal
;;          :variable-pitch-height 1.05
;;          :bold-family nil ; use whatever the underlying face has
;;          :bold-weight bold
;;          :italic-family nil ; use whatever the underlying face has
;;          :italic-slant italic
;;          :line-spacing 1)))

(provide 'init-looks)
