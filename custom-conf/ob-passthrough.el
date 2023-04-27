;;; ob-passthrough.el ---  passthrough evaluator          -*- lexical-binding: t; -*-
;; This is from https://emacs.stackexchange.com/questions/24247/org-mode-pipe-source-block-output-as-stdin-to-next-source-block
;; this ob evaluates the block as ifself, so it can be used as input for another block

;; add (passthrough . t) to org-babel-list-langauges, and here it is in action:

(require 'ob)

(defun org-babel-execute:passthrough (body params)
  body)

;; json output is json
(defalias 'org-babel-execute:json 'org-babel-execute:passthrough)

(provide 'ob-passthrough)
;;; ob-passthrough.el ends here
