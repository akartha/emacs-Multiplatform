;;; -*- lexical-binding: t; -*-

(use-package modus-themes
  ;; :straight t
  :custom
  (setq modus-themes-org-blocks 'tinted-background)
  :init
  (load-theme 'modus-vivendi-tinted t)) ;;other themes - modus-vivendi, modus-operandi, modus-operandi-tinted/deuteranopia, etc.

(setq fontaine-latest-state-file
      (locate-user-emacs-file "fontaine-latest-state.eld"))

;; Iosevka Comfy            == monospaced, supports ligatures
;; Iosevka Comfy Fixed      == monospaced, no ligatures
;; Iosevka Comfy Duo        == quasi-proportional, supports ligatures
;; Iosevka Comfy Wide       == like Iosevka Comfy, but wider
;; Iosevka Comfy Wide Fixed == like Iosevka Comfy Fixed, but wider
;; Iosevka Comfy Motion     == monospaced, supports ligatures, fancier glyphs
;; Iosevka Comfy Motion Duo == as above, but quasi-proportional
(when (or ak/my-framework-p ak/my-win-framework-p)
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
           :default-weight regular
           :default-height 185
           :line-spacing 10)              ;this is in pixels
          (t
           ;; I keep all properties for didactic purposes, but most can be
           ;; omitted.  See the fontaine manual for the technicalities:
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
           :line-spacing nil))))

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
           :default-height 170)
          (large
           :default-weight semilight
           :default-height 180
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
           :default-weight semilight
           :default-height 175
           :line-spacing 5)              ;this is in pixels
          (t
           ;; I keep all properties for didactic purposes, but most can be
           ;; omitted.  See the fontaine manual for the technicalities:
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
           :line-spacing 2))))

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
  :custom
  ;; (lambda-line-icon-time t) ;; requires ClockFace font (see below)
  ;; (lambda-line-clockface-update-fontset "ClockFaceRect") ;; set clock icon
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
  ;; :straight t 
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
  ;; :straight t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Diminishing modes ;; The package =diminish= disables modes on the
;; mode line but keeps them running, it just prevents them from
;; showing up and taking up space. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package diminish
  ;; :straight t
  :init
  (diminish 'visual-line-mode)
  (diminish 'subword-mode)
  (diminish 'page-break-lines-mode)
  (diminish 'auto-revert-mode)
  ;; (diminish 'yas-minor-mode)
  (diminish 'org-indent-mode))



(provide 'init-looks)
