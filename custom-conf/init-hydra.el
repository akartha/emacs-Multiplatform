;;; -*- lexical-binding: t; -*-

(when ak/my-framework-p
  (defhydra hydra-jump-to-directory
    (:color amaranth
            :timeout 5)
"
^System^          ^Projects^            ^Personal^        
^^^^^^^^--------------------------------------------------
_h_: ~            _c_: emacs custom     _v_: Dropbox      
_d_: Documents    _s_: Scripts          _o_: orgs     
_e_: emacs        _p_: projects         _a_: Articles         
^ ^               ^ ^                   _x_: xkcd          _q_: quit 
"
    ("h" (find-file "~/"))
    ("d" (find-file "~/Documents"))
    ("v" (find-file "~/Dropbox"))
    ("a" (find-file "~/Dropbox/articles/"))
    ("e" (find-file "~/.emacs.d/") )
    ("c" (find-file "~/.emacs.d/custom-conf/") )
    ("s" (find-file "~/scripts/") )
    ("p" (find-file "~/projects/") )
    ("o" (find-file "~/Dropbox/org-files/") )
    ("x" (find-file "~/.emacs.d/xkcd/") )
    ("q" nil :color blue))
  (defhydra hydra-jump-to-config
    (:color amaranth
            :timeout 5)
    "Open Config files"
    ("b" (find-file "~/.bashrc") ".bashrc")
    ("p" (find-file "~/.bash_profile") ".bash_profile")
    ("e" (find-file "~/.emacs.d/init.el") "emacs init")
    ("i" (find-file "~/.i3/config") "i3 config")
    ("q" nil "Quit" :color blue))

  (define-key ak-map "d" '("Folders (hydra)" . hydra-jump-to-directory/body))
  (define-key ak-map "c" '("Config files (hydra)" . hydra-jump-to-config/body)))

(when ak/my-win-framework-p
  (defhydra hydra-jump-to-directory
    (:color amaranth
            :timeout 5)
    "
^System^          ^Projects^            ^Personal^        
^^^^^^^^--------------------------------------------------
_h_: c:\          _c_: emacs custom     _v_: Dropbox      
_d_: Documents    ^ ^                   _o_: orgs     
_e_: emacs        ^ ^                   _a_: Articles         
_p_: Desktop      ^ ^                   _x_: xkcd          _q_: quit
"
    ("h" (find-file "c:/") )
    ("d" (find-file "c:/Users/Arun/Documents/") )
    ("p" (find-file "c:/Users/Arun/Desktop/") )
    ("v" (find-file "c:/Users/Arun/Dropbox/") )
    ("a" (find-file "c:/Users/Arun/Dropbox/Dropbox/articles/") )
    ("o" (find-file "c:/Users/Arun/Dropbox/org-files/")  )
    ("e" (find-file "~/.emacs.d/") )
    ("c" (find-file "~/.emacs.d/custom-conf/") )
    ("x" (find-file "~/.emacs.d/xkcd/") )
    ("q" nil :color blue))
  
  (define-key ak-map "d" '("Folders (hydra)" . hydra-jump-to-directory/body)))
  
(defhydra hydra-resize-window
  (:color amaranth
          :timeout 5)
  "
^Sideways^             ^Vertically^         ^Absolute^              ^Command and quit
^^^^^^^^--------------------------------------------------------------------------------
_[_: Shrink            _,_: Shrink          _-_: Shrink to fit      _o_: Other window
_]_: Enlarge           _._: Enlarge         _=_: balance            _s_: Ace-window
^ ^                    ^ ^                  ^ ^                     _q_: quit 
"
  ("[" (shrink-window-horizontally 2))
  ("]" (enlarge-window-horizontally 2))
  ("," (shrink-window 2))
  ("." (enlarge-window 2))
  ("-" (shrink-window-if-larger-than-buffer) :color blue)
  ("=" (balance-windows) :color blue)
  ("o" (other-window 1) :color blue)
  ("s" (ace-window 1) :color blue)
  ("q" nil :color blue))

(define-key ak-map "]" '("Resize (hydra)" . hydra-resize-window/body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


(defhydra hydra-jump-cursor
  (:color amaranth)
  "jump cursor"
  ("]" (forward-sexp) "Forward sexp")
  ("<" (beginning-of-buffer) "Beginning of buffer")
  (">" (end-of-buffer) "End of buffer")
  ("[" (backward-sexp) "Backward sexp")
  ;; ("w" (forward-to-word 1) "Forward word")
  ("w" (forward-word-strictly 1) "Forward word")
  ;; ("b" (backward-to-word 1) "Backward word")
  ("b" (backward-word-strictly 1) "Backward word")
  ("e" (forward-sentence) "Forward sentence")
  ("a" (backward-sentence) "Backward sentence")
  ("}" (forward-paragraph) "Forward para")
  ("{" (backward-paragraph) "Backward para")
  ("q" nil "Quit" :color blue))

(global-set-key (kbd "C-c r") '("Launcher (hydra)" . hydra-launcher/body))

(define-key ak-map "m" '("Move lines (hydra)" . hydra-move-lines/body))

(define-key ak-map "." '("Jump to (hydra)" . hydra-jump-cursor/body))

(provide 'init-hydra)
