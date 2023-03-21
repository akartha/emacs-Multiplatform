;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * XKCD                                                ;;
;; For a bit of fun, add xkcd cartoons to your dashboard ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'init-looks)
;; (require 'custom-keymaps)
;; (require 'init-system-utils)


(add-to-list 'recentf-exclude "~/\.emacs\.d/xkcd/*")

(use-package xkcd
  :straight t)

;; Function to check for internet being up
;; (defun internet-up-p (&optional host)
;;   (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1"
;;                      (if host host "www.google.com"))))

;; (message (if (internet-up-p) "Up" "Down"))
(defun ak/reload-xkcd (arg)
  "Load a random xkcd cartoon on the dashboard
   With prefix - load the latest xkcd cartoon"
  (interactive "P")
  (let ((rand-id-xkcd nil)
        (rand-id-xkcd-url nil))

    (with-temp-buffer
      (if arg
          (setq rand-id-xkcd (string-to-number(xkcd)))
        (setq rand-id-xkcd (string-to-number( xkcd-rand))))
      (setq rand-id-xkcd-url (concat "http://xkcd.com/" (number-to-string rand-id-xkcd)))
      (xkcd-kill-buffer))
    
    (let ((last-xkcd-png (concat xkcd-cache-dir (number-to-string rand-id-xkcd) ".png")))
      (if (file-exists-p last-xkcd-png)
          (setq dashboard-startup-banner last-xkcd-png
                dashboard-banner-logo-title rand-id-xkcd-url
                dashboard-init-info xkcd-alt))))
  (revert-buffer))

(define-key ak-map "X" 'ak/reload-xkcd)

;;* Experimental features

;;;;;;;;;;;;
;; ** EKG ;;
;;;;;;;;;;;;

(define-prefix-command 'ekg-custom-keymap)
(global-set-key (kbd "` e") 'ekg-custom-keymap)

(use-package ekg
  :straight t
  :bind
  (:map ekg-custom-keymap
        ("c" . ekg-capture)
        ("a" . ekg-show-notes-with-tag)
        ("u" . ekg-capture-url)
        ("b" . ekg-browse-url)
        ("t" . ekg-show-notes-for-today)
        ("m" . ekg-show-notes-latest-modified)
        ("p" . ekg-show-notes-latest-captured)
        ("T" . ekg-show-notes-in-trash)
        :map ekg-notes-mode-map
        ("q" . kill-current-buffer))
  
  :config
  (require 'emacsql-sqlite))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Modus Theme customizations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq modus-themes-completions
;;       '((matches . (extrabold underline))
;;         (selection . (extrabold italic underline))))
;; (setq modus-themes-org-blocks 'tinted-background)
;; (setq modus-themes-prompts '(extrabold italic))

;; These overrides are common to all Modus themes.  We also provide
;; theme-specific options, such as `modus-operandi-palette-overrides'.
;;
;; In general, the theme-specific overrides are better for overriding
;; color values, such as redefining what `blue-faint' looks like.  The
;; common overrides are best used for changes to semantic color
;; mappings, as we show below.

;; Keep the background unspecified (like the default), but use a faint
;; foreground color.
;; (setq modus-themes-common-palette-overrides
;;       '((fg-prompt cyan-faint)
;;         (bg-prompt unspecified)))

;; ;; Add a nuanced background to prompts that complements their foreground.
;; (setq modus-themes-common-palette-overrides
;;       '((fg-prompt cyan)
;;         (bg-prompt bg-blue-nuanced)))

;; ;; Add a yellow background and adjust the foreground accordingly.
;; (setq modus-themes-common-palette-overrides
;;       '((fg-prompt fg-main)
;;         (bg-prompt bg-yellow-nuanced) ; try to replace "nuanced" or "subtle" with "intense"

;;         (comment yellow-faint)
;;         (string green-warmer)

;;         (fg-heading-1 blue-warmer)
;;         (bg-heading-1 bg-blue-nuanced)
;;         (overline-heading-1 blue)

;;         (fg-heading-2 fg-main)
;;         (bg-heading-2 bg-dim)
;;         (overline-heading-2 border)))


;;;;;;;;;;;;;;;;;;;;
;; ** Avy customs ;;
;;;;;;;;;;;;;;;;;;;;

;; (defun avy-goto-parens ()
;;   (interactive)
;;   (let ((avy-command this-command))   ; for look up in avy-orders-alist
;;     (avy-jump "(+")))
;; (add-to-list 'avy-orders-alist '(avy-goto-parens . avy-order-closest))
;; ;;(global-define-key (kbd "s-p") 'avy-goto-parens)
;; (define-key ak-map "(" 'avy-goto-parens)

;; (defun avy-org-same-level (&optional all)
;;   "Go to any org heading of the same level as the current one.
;; By default, choices are limited to headings under common
;; subheading, but if called with a prefix argument, will be
;; buffer-global."
;;   (interactive "P")
;;   (let ((org-level (org-current-level)))
;;     (avy--generic-jump
;;      (format "^%s "
;;              (regexp-quote
;;               (make-string org-level ?*)))
;;      nil
;;      'pre
;;      (unless (or all (= org-level 1))
;;        (save-excursion
;;          (outline-up-heading 1)
;;          (point)))
;;      (unless (or all (= org-level 1))
;;        (save-excursion
;;          (outline-up-heading 1)
;;          (org-end-of-subtree))))))

;; (defun avy-org-parent-level (&optional all)
;;   "Go to any org heading one level above the current one.

;; By default, choices are limited to headings under common
;; subheading, but if called with a prefix argument, will be
;; buffer-global."
;;   (interactive "P")
;;   (let ((org-level (org-current-level)))
;;     (if (= org-level 1)
;;         (message "Already at top level.")
;;       (avy--generic-jump (format "^%s " (regexp-quote (make-string (- org-level 1) ?*)))
;;                          nil 'pre (unless (or all (= org-level 2))
;;                                     (save-excursion
;;                                       (outline-up-heading 2)
;;                                       (point)))
;;                          (unless (or all (= org-level 2))
;;                            (save-excursion
;;                              (outline-up-heading 2)
;;                              (org-end-of-subtree)))))))

;; (defun avy-org-child-level (&optional all)
;;   "Go to any org heading one level below the current one.

;; By default, choices are limited to headings under common
;; subheading, but if called with a prefix argument, will be
;; buffer-global."
;;   (interactive "P")
;;   (if (save-excursion (org-goto-first-child))
;;       (let ((org-level (org-current-level)))
;;         (avy--generic-jump
;;          (format "^%s "
;;                  (regexp-quote
;;                   (make-string (+ org-level 1) ?*)))
;;          nil
;;          'pre
;;          (unless all
;;            (save-excursion
;;              (ignore-errors
;;                (outline-up-heading 0))
;;              (point)))
;;          (unless all
;;            (save-excursion
;;              (ignore-errors
;;                (outline-up-heading 0))
;;              (org-end-of-subtree)))))
;;     (message "Heading has no children.")))

;; (defun avy-org-goto-level (&optional num)
;;   "Prompt for an org level to go to, defaulting to the current one."
;;   (interactive (list
;;                 (read-number "Select heading level: " (org-current-level))))
;;   (avy--generic-jump
;;    (format "^%s " (regexp-quote (make-string num ?*)))
;;    nil
;;    'pre))

;; * Outdated/Not used/Phased out
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Helm                                                                                     ;;
;;                                                                                             ;;
;; [[https://github.com/emacs-helm/helm][Helm github]]                                         ;;
;;                                                                                             ;;
;; Replaced, as I like the functionality provided by embark - and this is just not compatible. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package helm
;;   :diminish
;;   :ensure t
;;   :preface (require 'helm-config)
;;   :bind
;;   ("C-x C-f" . 'helm-find-files)
;;   ("C-x C-b" . 'helm-buffers-list)
;;   ("M-x" . 'helm-M-x)
;;   :config
;;   (defun daedreth/helm-hide-minibuffer ()
;;     (when (with-helm-buffer helm-echo-input-in-header-line)
;; (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
;;   (overlay-put ov 'window (selected-window))
;;   (overlay-put ov 'face
;;         (let ((bg-color (face-background 'default nil)))
;;       `(:background ,bg-color :foreground ,bg-color)))
;;   (setq-local cursor-type nil))))
;;   (add-hook 'helm-minibuffer-set-up-hook 'daedreth/helm-hide-minibuffer)
;;   (setq helm-autoresize-max-height 0
;;   helm-autoresize-min-height 40
;;   helm-M-x-fuzzy-match t
;;   helm-buffers-fuzzy-matching t
;;   helm-recentf-fuzzy-match t
;;   helm-semantic-fuzzy-match t
;;   helm-imenu-fuzzy-match t
;;   helm-split-window-in-side-p nil
;;   helm-move-to-line-cycle-in-source nil
;;   helm-ff-search-library-in-sexp t
;;   helm-scroll-amount 8
;;   helm-echo-input-in-header-line t)
;;   :init
;;   (helm-mode 1))

;; ;;  (require 'helm-config)
;;   (helm-autoresize-mode 1)
;;   (define-key helm-find-files-map (kbd "C-b") 'helm-find-files-up-one-level)
;;   (define-key helm-find-files-map (kbd "C-f") 'helm-execute-persistent-action)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Swiper                ;;
;; Replaced by Consult now. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package swiper
;;   :ensure t
;;   :bind ("C-s" . 'swiper))




;;;;;;;;;;;;
;; ** Ivy ;;
;;;;;;;;;;;;

;  (use-package ivy
;   :ensure t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Electric                                                                                            ;;
;; If you write any code, you may enjoy this. I, personally, felt this was more of an annoyance than help ;;
;;                                                                                                        ;;
;; Typing the first character in a set of 2, completes the second one after your cursor.                  ;;
;; Opening a bracket? It's closed for you already. Quoting something? It's closed for you already.        ;;
;;                                                                                                        ;;
;; You can easily add and remove pairs yourself                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(setq electric-pair-pairs '(
;                           (?\{ . ?\})
;                           (?\( . ?\))
;                           (?\[ . ?\])
;                           (?\" . ?\")
;                           ))


;; And now to enable it

;(electric-pair-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Popup Kill Ring                                                                      ;;
;; With a simple M-y you can now browse your kill-ring like browsing autocompletion items. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; (use-package popup-kill-ring
  ;;   :straight t
  ;;   :bind ("M-y" . popup-kill-ring))



(provide 'non-core)
