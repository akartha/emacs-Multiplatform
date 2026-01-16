;;; -*- lexical-binding: t; -*-


(add-to-list 'default-frame-alist '(mouse-color . "#FF8800"))
(add-to-list 'default-frame-alist '(cursor-color . "#FF0000"))

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
             :default-weight semilight
             :default-height 235
             :line-spacing 15)              ;this is in pixels
            (immersive-writing
             :default-family "Iosevka Comfy Wide Fixed" 
             :default-weight semilight
             :default-height 235
             :line-spacing 13)              ;this is in pixels
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
  
(when ak/my-pi-p
    (setq fontaine-presets
          '((tiny
             :default-family "Iosevka Comfy Wide Fixed"
             :default-height 90)
            (small
             :default-family "Iosevka Comfy Fixed"
             :default-height 110)
            (regular
             :default-height 140)
            (medium
             :default-height 155)
            (large
             :default-weight semilight
             :default-height 170
             :bold-weight extrabold)
            (code-demo
             :default-weight semilight
             :default-height 185
             :bold-weight extrabold)
            (presentation
             :default-weight semilight
             :default-height 300
             :bold-weight extrabold)
            (reading
             :default-family "Iosevka Comfy Motion Duo" 
             :default-weight semilight
             :default-height 335
             :line-spacing 19)              ;this is in pixels
            (immersive-writing
             :default-family "Iosevka Comfy Wide Fixed" 
             :default-weight semilight
             :default-height 235
             :line-spacing 13)              ;this is in pixels
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
             :default-family "Iosevka Comfy Wide" 
             :default-weight semilight
             :default-height 195
             :bold-weight extrabold)
            (code-demo
             ;; :default-family "Iosevka Comfy Wide Fixed"
             :default-family "Fira Code Retina"
             :default-weight semilight
             :default-height 205
             :bold-weight extrabold)
            (coding
             ;; :default-family "Iosevka Comfy Wide Fixed"
             :default-family "Fira Code Retina"
             :default-weight semilight
             :default-height 175
             :bold-weight extrabold)
            (presentation
             :default-family "Iosevka Comfy Wide" 
             :default-weight semilight
             :default-height 220
             :bold-weight extrabold
             :line-spacing 10)
            (reading
             :default-family "ETBembo" 
             ;; :default-weight bold
             :default-height 225
             :line-spacing 8)              ;this is in pixels
            (immersive-writing
             :default-family "ETBembo" 
             ;; :default-weight bold
             :default-height 225
             :line-spacing 15)              ;this is in pixels
            (t
             ;; See the fontaine manual for the technicalities:
             ;; <https://protesilaos.com/emacs/fontaine>.
             ;; :default-family "Iosevka Comfy"
             :default-family "Iosevka Comfy Motion"
             :default-weight regular
             :default-height 100
             ;; :fixed-pitch-family nil ; falls back to :default-family
             :fixed-pitch-family "Fira Code Retina"
             :fixed-pitch-weight nil ; falls back to :default-weight
             :fixed-pitch-height 1.0
             :fixed-pitch-serif-family nil ; falls back to :default-family
             :fixed-pitch-serif-weight nil ; falls back to :default-weight
             :fixed-pitch-serif-height 1.0
             ;; :variable-pitch-family "Iosevka Comfy Motion Duo"
             :variable-pitch-family "ETBembo"
             :variable-pitch-weight nil
             :variable-pitch-height 1.0
             :bold-family nil ; use whatever the underlying face has
             :bold-weight bold
             ;; :italic-family "Iosevka Comfy Motion"
             :italic-family nil
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
;; (define-key global-map (kbd "C-c g") #'fontaine-set-face-font)

;; (use-package all-the-icons
;;   :ensure t)



;; (use-package all-the-icons-completion
;;   :ensure t 
;;   :after (marginalia all-the-icons)
;;   :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
;;   :init (all-the-icons-completion-mode))

;; While changing buffers or workspaces, the first thing you do is
;; look for your cursor.  Every time you change buffers, the current
;; position of your cursor will be briefly highlighted
(use-package beacon
  :ensure t
  :diminish
  :if (not ak/my-pi-p)
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
  :defer t
  :diminish
  :init
  (add-hook 'prog-mode-hook 'rainbow-mode))

;; Colors parentheses and other delimiters depending on their depth

(use-package rainbow-delimiters
  :ensure t
  :diminish
  :defer t
  :custom 
  (rainbow-delimiters-max-face-count 5)
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Diminishing modes ;; The package =diminish= disables modes on the
;; mode line but keeps them running, it just prevents them from
;; showing up and taking up space. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;Work in progress
(defvar-local my-minibuffer-font-remap-cookie nil
  "The current face remap of `my-minibuffer-set-font'.")

(defface my-minibuffer-miniature
  '((t :height 0.5))
  "Face for the minibuffer.")

(defface my-minibuffer-default
  '((t :height 1))
  "Face for the minibuffer.")

(defun my-minibuffer-set-font ()
  (if (equal fontaine-current-preset 'reading)
      (setq-local my-minibuffer-font-remap-cookie
                  (face-remap-add-relative 'default 'my-minibuffer-miniature))))

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
            (fontaine-set-preset 'reading)
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

(define-key ak-display-settings-keymap "z" '("Toggle full screen" . toggle-frame-fullscreen))

(provide 'init-looks)
