;;; -*- lexical-binding: t; -*-


(add-to-list 'default-frame-alist '(mouse-color . "#FF8800"))
(add-to-list 'default-frame-alist '(cursor-color . "#FF0000"))

;; (use-package modus-themes
;;   ;; :straight t
;;   :custom
;;   (setq modus-themes-org-blocks 'tinted-background)
;;   :init
;;   (load-theme 'modus-vivendi-tinted t)) ;;other themes - modus-vivendi, modus-operandi, modus-operandi-tinted/deuteranopia, etc.
(use-package ef-themes
  :ensure t 
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

(use-package fontaine 
  :ensure t
  :config 


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
             :default-height 235
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
             :variable-pitch-height 1.3
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
             :bold-weight extrabold
             :line-spacing 10)
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
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset))

;; (add-hook 'modus-themes-after-load-theme-hook #'fontaine-apply-current-preset)

;; fontaine does not define any key bindings.  This is just a sample
;; that respects the key binding conventions.  Evaluate:
;;
;;     (info "(elisp) Key Binding Conventions")
(define-key global-map (kbd "C-c F") #'fontaine-set-preset)
(define-key global-map (kbd "C-c g") #'fontaine-set-face-font)

(use-package all-the-icons
  :ensure t)


(use-package lambda-line
  :ensure (:type git :host github :repo "lambda-emacs/lambda-line")
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
  :ensure t 
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init (all-the-icons-completion-mode))

;; While changing buffers or workspaces, the first thing you do is
;; look for your cursor.  Every time you change buffers, the current
;; position of your cursor will be briefly highlighted
(use-package beacon
  :ensure t
  :diminish
  :custom 
  (beacon-size 75)
  (beacon-blink-duration 0.75)
  (beacon-color "#666600")
  :config
  (beacon-mode 1))

;; Every time emacs encounters a hexadecimal code that resembles a
;; color, it will automatically highlight it in the appropriate color.

(use-package rainbow-mode
  :ensure t
  :diminish
  :init
  (add-hook 'prog-mode-hook 'rainbow-mode))

;; Colors parentheses and other delimiters depending on their depth

(use-package rainbow-delimiters
  :ensure t
  :diminish
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Diminishing modes ;; The package =diminish= disables modes on the
;; mode line but keeps them running, it just prevents them from
;; showing up and taking up space. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package diminish
  :ensure t 
  :init
  (diminish 'visual-line-mode)
  (diminish 'subword-mode)
  (diminish 'page-break-lines-mode)
  (diminish 'auto-revert-mode)
  ;; (diminish 'yas-minor-mode)
  (diminish 'org-indent-mode))

(use-package spacious-padding 
  :ensure t
  :config
(if ak/my-pi-p
    (setq spacious-padding-widths '( :internal-border-width 10
                                     :header-line-width 4
                                     :mode-line-width nil
                                     :tab-width 4
                                     :right-divider-width nil
                                     :scroll-bar-width nil
                                     :fringe-width 50))  
  (setq spacious-padding-widths '( :internal-border-width 45
                                   :header-line-width 4
                                   :mode-line-width nil
                                   :tab-width 4
                                   :right-divider-width 30
                                   :scroll-bar-width 8
                                   :fringe-width 200)))

  ;; Read the doc string of `spacious-padding-subtle-mode-line' as it
  ;; is very flexible and provides several examples.
  (setq spacious-padding-subtle-mode-line 
        '(:mode-line-active "#0000ff" 
                            :mode-line-inactive "#aaaa77")))

(use-package logos
  :ensure t
  :config 
;; If you want to use outlines instead of page breaks (the ^L):
  (setq logos-outlines-are-pages t
        ;; This is the default value for the outlines:
        logos-outline-regexp-alist `((emacs-lisp-mode . "^;;;+ ")
                                     (org-mode . "^\\*+ +")
                                     (markdown-mode . "^\\#+ +")))
  (setq-default logos-hide-cursor nil
                logos-hide-mode-line t
                logos-hide-header-line t
                logos-hide-buffer-boundaries t
                logos-hide-fringe t
                logos-variable-pitch nil
                logos-buffer-read-only nil
                logos-scroll-lock nil
                logos-olivetti nil))

;; (let ((map global-map))
(define-key global-map [remap narrow-to-region] #'logos-narrow-dwim)
(define-key global-map [remap forward-page] #'logos-forward-page-dwim)
(define-key global-map [remap backward-page] #'logos-backward-page-dwim)


;;Work in progress
(defvar-local my-minibuffer-font-remap-cookie nil
  "The current face remap of `my-minibuffer-set-font'.")

(defface my-minibuffer-default
  '((t :height 0.8))
  "Face for the minibuffer.")

(defun my-minibuffer-set-font ()
  (setq-local my-minibuffer-font-remap-cookie
              (face-remap-add-relative 'default 'my-minibuffer-default)))

(add-hook 'minibuffer-mode-hook #'my-minibuffer-set-font)
;;Work in progress 
(defun ak/toggle-immersive-mode()
  "Toggle Immersive Mode
Toggle Spacious padding mode, logos focus mode and 
toggle between presentation and regular fontaine presets"
  (interactive)
  (if (equal 
       (default-value 'spacious-padding-mode) 
       logos-focus-mode)
      (progn
        (logos-focus-mode 'toggle)
        (spacious-padding-mode 'toggle)
        ;; (fontaine-toggle-preset)
        (if (equal (default-value 'spacious-padding-mode) t)
            (fontaine-set-preset 'presentation)
          (fontaine-set-preset 'regular)))
    (fontaine-set-preset 'regular)
    (logos-focus-mode -1)
    (spacious-padding-mode -1)))


(define-prefix-command 'ak-display-settings-keymap)
(global-set-key (kbd "` d") 'ak-display-settings-keymap)

(define-key ak-display-settings-keymap "f" '("Fontaine Presets" . fontaine-set-preset))
(define-key ak-display-settings-keymap (kbd "<f9>") '("Consult theme" . consult-theme))

(define-key global-map (kbd "<f11>") '("Toggle Immersive Mode" . ak/toggle-immersive-mode))
(define-key ak-display-settings-keymap "i" '("Toggle Immersive Mode" . ak/toggle-immersive-mode))

(provide 'init-looks)
