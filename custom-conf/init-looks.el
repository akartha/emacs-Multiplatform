;;; -*- lexical-binding: t; -*-

;; (use-package modus-themes
;;   ;; :straight t
;;   :custom
;;   (setq modus-themes-org-blocks 'tinted-background)
;;   :init
;;   (load-theme 'modus-vivendi-tinted t)) ;;other themes - modus-vivendi, modus-operandi, modus-operandi-tinted/deuteranopia, etc.

(use-package ef-themes
  :init
  (load-theme 'ef-symbiosis t))
;; Dark Themes I like - ef-autumn
;;                      ef-dark 
;;                      ef-duo-dark 
;;                      ef-deuteranopia-dark 
;;                      ef-elea-dark 
;;                      ef-maris-dark 
;;                      ef-night  
;;                      ef-symbiosis  

;; Light Themes -       ef-cyprus                                   
;;                      ef-deuteranopia-light  
;;                      ef-light  

;; (use-package ef-themes
;;   ;; :straight t
;;   ;; :custom
;;   ;; (setq modus-themes-org-blocks 'tinted-background)
;;   :init
;;   (load-theme 'ef-autumn t))


(setq fontaine-latest-state-file
      (locate-user-emacs-file "fontaine-latest-state.eld"))

;; Iosevka Comfy            == monospaced, supports ligatures
;; Iosevka Comfy Fixed      == monospaced, no ligatures
;; Iosevka Comfy Duo        == quasi-proportional, supports ligatures
;; Iosevka Comfy Wide       == like Iosevka Comfy, but wider
;; Iosevka Comfy Wide Fixed == like Iosevka Comfy Fixed, but wider
;; Iosevka Comfy Motion     == monospaced, supports ligatures, fancier glyphs
;; Iosevka Comfy Motion Duo == as above, but quasi-proportional
(when (or ak/my-framework-p ak/my-win-framework-p ak/my-pi-p)
  (setq fontaine-presets
        '((tiny
           :default-family "Iosevka Comfy Wide Fixed"
           :default-height 90)
          (small
           :default-family "Iosevka Comfy Fixed"
           :default-height 110)
          (regular
           :default-height 125)
          (medium
           :default-height 140)
          (large
           :default-weight semilight
           :default-height 150
           :bold-weight extrabold)
          (code-demo
           :default-weight semilight
           :default-height 170
           :bold-weight extrabold)
          (presentation
           :default-weight semilight
           :default-height 220
           :bold-weight extrabold)
          (reading
           :default-family "Iosevka Comfy Motion Duo" 
           :default-weight semilight
           :default-height 185
           :line-spacing 15)              ;this is in pixels
          (t
           ;; See the fontaine manual for the technicalities:
           ;; <https://protesilaos.com/emacs/fontaine>.
           :default-family "Iosevka Comfy"
           :default-weight regular
           :default-height 100
           :fixed-pitch-family nil ; falls back to :default-family
           :fixed-pitch-weight nil ; falls back to :default-weight
           :fixed-pitch-height 1.0
           :fixed-pitch-serif-family nil ; falls back to :default-family
           :fixed-pitch-serif-weight nil ; falls back to :default-weight
           :fixed-pitch-serif-height 1.0
           :variable-pitch-family "Iosevka Comfy Motion Duo"
           :variable-pitch-weight nil
           :variable-pitch-height 1.0
           :bold-family nil ; use whatever the underlying face has
           :bold-weight bold
           :italic-family "Iosevka Comfy Motion"
           :italic-slant italic
           :line-spacing 3))))

(when ak/my-mac-p 
  (setq fontaine-presets
        '((tiny
           :default-family "Iosevka Comfy Wide Fixed"
           :default-height 90)
          (small
           :default-family "Iosevka Comfy Fixed"
           :default-height 110)
          (regular
           :default-height 150)
          (medium
           :default-height 175)
          (large
           :default-weight semilight
           :default-height 195
           :bold-weight extrabold)
          (code-demo
           :default-weight semilight
           :default-height 175
           :bold-weight extrabold)
          (presentation
           :default-weight semilight
           :default-height 220
           :bold-weight extrabold)
          (reading
           :default-family "Noto Serif" 
           ;; :default-weight bold
           :default-height 205
           :line-spacing 8)              ;this is in pixels
          (t
           ;; See the fontaine manual for the technicalities:
           ;; <https://protesilaos.com/emacs/fontaine>.
           :default-family "Iosevka Comfy"
           :default-weight regular
           :default-height 100
           :fixed-pitch-family nil ; falls back to :default-family
           :fixed-pitch-weight nil ; falls back to :default-weight
           :fixed-pitch-height 1.0
           :fixed-pitch-serif-family nil ; falls back to :default-family
           :fixed-pitch-serif-weight nil ; falls back to :default-weight
           :fixed-pitch-serif-height 1.0
           :variable-pitch-family "Iosevka Comfy Motion Duo"
           :variable-pitch-weight nil
           :variable-pitch-height 1.0
           :bold-family nil ; use whatever the underlying face has
           :bold-weight bold
           :italic-family "Iosevka Comfy Motion"
           :italic-slant italic
           :line-spacing 5))))

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

;; (use-package all-the-icons
;;   :straight t)


(use-package lambda-line
  ;; :ensure (:type git :host github :repo "lambda-emacs/lambda-line")
  :custom
  (lambda-line-icon-time nil) ;; requires ClockFace font (see below)
  (lambda-line-clockface-update-fontset "ClockFaceFatHands") ;; set clock icon
  (lambda-line-position 'bottom) ;; Set position of status-line 
  (lambda-line-abbrev t) ;; abbreviate major modes
  (lambda-line-hspace "  ")  ;; add some cushion
  (lambda-line-prefix t) ;; use a prefix symbol
  (lambda-line-prefix-padding nil) ;; no extra space for prefix 
  (lambda-line-status-invert nil )  ;; no invert colors
  (lambda-line-gui-ro-symbol  "⨂") ;; symbols
  (lambda-line-gui-mod-symbol "⬤") 
  (lambda-line-gui-rw-symbol  "◯") 
  (lambda-line-space-top +.05)  ;; padding on top and bottom of line
  (lambda-line-space-bottom -.05)
  (lambda-line-symbol-position 0.1) ;; adjust the vertical placement of symbol
  (display-time-mode t)
  (lambda-line-visual-bell nil)
  :config
  ;; activate lambda-line 
  (lambda-line-mode)
  ;; (lambda-line-clockface-update-fontset "ClockFaceRectSolid")
  ;; set divider line in footer
  (when (eq lambda-line-position 'top)
    (setq-default mode-line-format (list "%_"))
    (setq mode-line-format (list "%_"))))


(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init (all-the-icons-completion-mode))

;; While changing buffers or workspaces, the first thing you do is
;; look for your cursor.  Every time you change buffers, the current
;; position of your cursor will be briefly highlighted
(use-package beacon
  :diminish
  :config
  (beacon-mode 1))

;; Every time emacs encounters a hexadecimal code that resembles a
;; color, it will automatically highlight it in the appropriate color.

(use-package rainbow-mode
  :diminish
  :init
  (add-hook 'prog-mode-hook 'rainbow-mode))

;; Colors parentheses and other delimiters depending on their depth

(use-package rainbow-delimiters
  :diminish
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Diminishing modes ;; The package =diminish= disables modes on the
;; mode line but keeps them running, it just prevents them from
;; showing up and taking up space. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package diminish
  :init
  (diminish 'visual-line-mode)
  (diminish 'subword-mode)
  (diminish 'page-break-lines-mode)
  (diminish 'auto-revert-mode)
  ;; (diminish 'yas-minor-mode)
  (diminish 'org-indent-mode))


(setq spacious-padding-widths
      '( :internal-border-width 45
         :header-line-width 4
         :mode-line-width 3
         :tab-width 4
         :right-divider-width 30
         :scroll-bar-width 8
         :fringe-width 8))

;; Read the doc string of `spacious-padding-subtle-mode-line' as it
;; is very flexible and provides several examples.
(setq spacious-padding-subtle-mode-line 
      '(:mode-line-active "#0000ff" 
                          :mode-line-inactive "#aaaa77"))

;; If you want to use outlines instead of page breaks (the ^L):
(setq logos-outlines-are-pages t)

;; This is the default value for the outlines:
(setq logos-outline-regexp-alist
      `((emacs-lisp-mode . "^;;;+ ")
        (org-mode . "^\\*+ +")
        (markdown-mode . "^\\#+ +")))

;; These apply when `logos-focus-mode' is enabled.  Their value is
;; buffer-local.
(setq-default logos-hide-cursor nil
              logos-hide-mode-line t
              logos-hide-header-line t
              logos-hide-buffer-boundaries t
              logos-hide-fringe t
              logos-variable-pitch nil
              logos-buffer-read-only nil
              logos-scroll-lock nil
              logos-olivetti nil)

;; Also check this manual for `logos-focus-mode-hook'.  It lets you
;; extend `logos-focus-mode'.

(let ((map global-map))
  (define-key map [remap narrow-to-region] #'logos-narrow-dwim)
  (define-key map [remap forward-page] #'logos-forward-page-dwim)
  (define-key map [remap backward-page] #'logos-backward-page-dwim)
  (define-key map (kbd "<f11>") #'logos-focus-mode))

;; Also consider adding keys to `logos-focus-mode-map'.  They will take
;; effect when `logos-focus-mode' is enabled.

(define-prefix-command 'ak-display-settings-keymap)
(global-set-key (kbd "` d") 'ak-display-settings-keymap)

(define-key ak-display-settings-keymap "f" '("Fontaine Presets" . fontaine-set-preset))
(define-key ak-display-settings-keymap (kbd "<f8>") '("Spacious padding mode" . spacious-padding-mode))
(define-key ak-display-settings-keymap (kbd "<f9>") '("Consult theme" . consult-theme))


(provide 'init-looks)
