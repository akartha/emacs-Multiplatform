;; ** Windows, panes                                                                                         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; *** Switch-window

;; ;;With 3 or more, upon pressing =C-x o= ,
;; ;; the buffers turn a solid color and each buffer is asigned a
;; ;; letter. Pressing a letter asigned to a window will take you to
;; ;; the window. ;; ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package switch-window
  :straight t
  :config
  (setq switch-window-input-style 'minibuffer
        switch-window-increase 4
        switch-window-threshold 2
        switch-window-shortcut-style 'qwerty
        switch-window-qwerty-shortcuts
        '("a" "s" "d" "f" "j" "k" "l" "i" "o"))
  :bind
  ([remap other-window] . switch-window))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; *** Following window splits                                                                         

;; After you split a window, your focus remains in the previous one -
;; unless the below is set up. Also opens the previous buffer in the
;; newly opened window ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun split-and-follow-horizontally (prefix)
  (interactive "P")
  (split-window-below)
  (balance-windows)
  (other-window 1 nil)
  (if  prefix 
      (switch-to-next-buffer)
    (switch-to-prev-buffer)))

(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically (prefix)
  (interactive "P")
  (split-window-right)
  (balance-windows)
  (other-window 1)
  (if prefix
      (switch-to-next-buffer)
    (switch-to-prev-buffer)))

(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)




;;;;;;;;;;;;;;;;;;
;; Centaur Tabs ;;
;;;;;;;;;;;;;;;;;;

(use-package centaur-tabs
  :straight t
  :demand
  :init
  (setq centaur-tabs-enable-key-bindings t)
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-set-modified-marker t
        centaur-tabs-height 24
        centaur-tab-style "slant"
        centaur-tabs-set-icons t
        centaur-tabs-gray-out-icons 'buffer
        uniquify-separator "/"
        uniquify-buffer-name-style 'forward
        centaur-tabs-show-count t ))
  ;; :bind
  ;; ("C-<prior>" . centaur-tabs-backward)
  ;; ("C-<next>" . centaur-tabs-forward))

(provide 'switch-window-custom)
