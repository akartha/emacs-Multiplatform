;;; org-roam-epub.el --- Export org-roam nodes to EPUB via pandoc -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (org-roam "2.0.0"))
;; Keywords: org-roam, epub, export, pandoc

;;; Commentary:

;; This package provides commands to export org-roam nodes tagged with a
;; specific tag to EPUB format using pandoc.  It supports:
;;
;;   - Individual node export via `org-roam-epub-export-node-at-point'
;;   - Bulk export of all nodes matching a tag via `org-roam-epub-export-by-tag'
;;   - Interactive tag selection via `org-roam-epub-export-select-tag'
;;   - Post-export tagging of the source org file with a configurable tag
;;
;; Quick start:
;;
;;   (require 'org-roam-epub)
;;   (setq org-roam-epub-export-tag "epub-export")
;;   (setq org-roam-epub-output-dir "~/Documents/epubs/")
;;
;; Then call one of:
;;   M-x org-roam-epub-export-node-at-point
;;   M-x org-roam-epub-export-by-tag
;;   M-x org-roam-epub-export-select-tag
;;   M-x org-roam-epub-export-node-by-title

;;; Code:

(require 'org)
(require 'org-roam)
(require 'cl-lib)

;;;; Customisation

(defgroup org-roam-epub nil
  "Export org-roam nodes to EPUB via pandoc."
  :group 'org-roam
  :prefix "org-roam-epub-")

(defcustom org-roam-epub-export-tag "epub-export"
  "Tag used to identify org-roam nodes that should be exported to EPUB.
Nodes carrying this tag are included in bulk export operations."
  :type 'string
  :group 'org-roam-epub)

(defcustom org-roam-epub-exported-tag "epub-exported"
  "Tag added to a node after it has been successfully exported to EPUB.
Set to nil to disable post-export tagging."
  :type '(choice string (const nil))
  :group 'org-roam-epub)

(defcustom org-roam-epub-output-dir
  (expand-file-name "~/org-roam-epubs/")
  "Directory where exported EPUB files are written."
  :type 'directory
  :group 'org-roam-epub)

(defcustom org-roam-epub-pandoc-executable "pandoc"
  "Path to the pandoc executable."
  :type 'string
  :group 'org-roam-epub)

(defcustom org-roam-epub-pandoc-extra-args nil
  "List of additional arguments passed to pandoc on every export.
Example: (list \"--epub-cover-image\" \"/path/to/cover.png\")"
  :type '(repeat string)
  :group 'org-roam-epub)

(defcustom org-roam-epub-before-export-hook nil
  "Hook run before each individual node is exported.
The current buffer is the node's org file."
  :type 'hook
  :group 'org-roam-epub)

(defcustom org-roam-epub-after-export-hook nil
  "Hook run after each individual node is successfully exported.
The current buffer is the node's org file."
  :type 'hook
  :group 'org-roam-epub)

;;;; Keymap

(defvar org-roam-epub-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") #'org-roam-epub-export-node-at-point)
    (define-key map (kbd "b") #'org-roam-epub-export-by-tag)
    (define-key map (kbd "s") #'org-roam-epub-export-select-tag)
    (define-key map (kbd "n") #'org-roam-epub-export-node-by-title)
    map)
  "Keymap for org-roam-epub commands.
Bind this to a prefix key, e.g.:
  (global-set-key (kbd \"C-c r e\") org-roam-epub-command-map)")

;;;; Internal helpers

(defun org-roam-epub--ensure-output-dir ()
  "Create `org-roam-epub-output-dir' if it does not exist."
  (unless (file-directory-p org-roam-epub-output-dir)
    (make-directory org-roam-epub-output-dir :parents)
    (message "org-roam-epub: Created output directory %s"
             org-roam-epub-output-dir)))

(defun org-roam-epub--node-output-path (node)
  "Return the full EPUB output path for NODE."
  (let* ((title (org-roam-node-title node))
         (safe-title (replace-regexp-in-string
                      "[^[:alnum:]_\\-]" "_"
                      (string-trim title)))
         (filename (concat safe-title ".epub")))
    (expand-file-name filename org-roam-epub-output-dir)))

(defun org-roam-epub--build-pandoc-command (input-file output-file)
  "Build the pandoc shell command list for INPUT-FILE to OUTPUT-FILE."
  (append
   (list org-roam-epub-pandoc-executable
         input-file
         "--from" "org"
         "--to"   "epub"
         "--output" output-file)
   org-roam-epub-pandoc-extra-args))

(defun org-roam-epub--add-exported-tag (file)
  "Add `org-roam-epub-exported-tag' to the #+filetags line in FILE.
Creates the filetags line if absent.  Does nothing when
`org-roam-epub-exported-tag' is nil."
  (when org-roam-epub-exported-tag
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (let ((tag (concat ":" org-roam-epub-exported-tag ":")))
          (if (re-search-forward "^#\\+filetags:.*$" nil t)
              ;; Line exists - append tag if not already present
              (let ((line (match-string 0)))
                (unless (string-match-p (regexp-quote tag) line)
                  (goto-char (match-beginning 0))
                  (let* ((existing (match-string 0))
                         (current-tags
                          (if (string-match "^#\\+filetags:\\s-*\\(.*\\)" existing)
                              (string-trim (match-string 1 existing))
                            ""))
                         (new-tags
                          (if (string-empty-p current-tags)
                              tag
                            (if (string-match-p "^:.*:$" current-tags)
                                (concat (string-remove-suffix ":" current-tags)
                                        org-roam-epub-exported-tag ":")
                              (concat current-tags " " tag)))))
                    (delete-region (line-beginning-position)
                                   (line-end-position))
                    (insert (format "#+filetags: %s" new-tags)))))
            ;; No filetags line - insert one near the top
            (goto-char (point-min))
            (if (re-search-forward "^#\\+" nil t)
                (progn (end-of-line) (newline))
              (goto-char (point-min)))
            (insert (format "#+filetags: %s\n" tag)))))
      (save-buffer)
      (when (fboundp 'org-roam-db-update-file)
        (org-roam-db-update-file file)))))

(defun org-roam-epub--export-node (node &optional silent)
  "Export NODE to EPUB using pandoc.
Returns t on success, nil on failure.
When SILENT is non-nil, suppress progress messages."
  (let* ((file        (org-roam-node-file node))
         (title       (org-roam-node-title node))
         (output-file (org-roam-epub--node-output-path node))
         (cmd         (org-roam-epub--build-pandoc-command file output-file)))
    (unless silent
      (message "org-roam-epub: Exporting \"%s\" -> %s ..." title output-file))
    (with-current-buffer (find-file-noselect file)
      (run-hooks 'org-roam-epub-before-export-hook)
      (let ((exit-code (apply #'call-process (car cmd) nil nil nil (cdr cmd))))
        (if (zerop exit-code)
            (progn
              (unless silent
                (message "org-roam-epub: Exported \"%s\" successfully" title))
              (org-roam-epub--add-exported-tag file)
              (run-hooks 'org-roam-epub-after-export-hook)
              t)
          (message "org-roam-epub: pandoc failed (exit %d) for \"%s\""
                   exit-code title)
          nil)))))

(defun org-roam-epub--nodes-with-tag (tag)
  "Return a list of org-roam nodes that carry TAG."
  (cl-remove-if-not
   (lambda (node)
     (member tag (org-roam-node-tags node)))
   (org-roam-node-list)))

;;;; Interactive commands

;;;###autoload
(defun org-roam-epub-export-node-at-point ()
  "Export the org-roam node at point to EPUB.

The EPUB is written to `org-roam-epub-output-dir'.  After a
successful export, `org-roam-epub-exported-tag' is added to the
source file's #+filetags line."
  (interactive)
  (org-roam-epub--ensure-output-dir)
  (if-let ((node (org-roam-node-at-point 'assert)))
      (if (org-roam-epub--export-node node)
          (message "org-roam-epub: Export complete -> %s"
                   (org-roam-epub--node-output-path node))
        (user-error "org-roam-epub: Export failed - check *Messages* for details"))
    (user-error "org-roam-epub: No org-roam node found at point")))

;;;###autoload
(defun org-roam-epub-export-by-tag (&optional tag)
  "Bulk-export all org-roam nodes tagged with TAG to EPUB.

TAG defaults to `org-roam-epub-export-tag'.  When called
interactively with a prefix argument, prompts for a different tag.

Results are reported in a *org-roam-epub* summary buffer."
  (interactive
   (list (if current-prefix-arg
             (completing-read "Export nodes with tag: "
                              (org-roam-tag-completions)
                              nil nil nil nil
                              org-roam-epub-export-tag)
           org-roam-epub-export-tag)))
  (let* ((tag   (or tag org-roam-epub-export-tag))
         (nodes (org-roam-epub--nodes-with-tag tag)))
    (if (null nodes)
        (message "org-roam-epub: No nodes found with tag \"%s\"" tag)
      (org-roam-epub--ensure-output-dir)
      (let ((total   (length nodes))
            (success 0)
            (failure '()))
        (message "org-roam-epub: Exporting %d node(s) tagged \"%s\" ..."
                 total tag)
        (dolist (node nodes)
          (if (org-roam-epub--export-node node t)
              (cl-incf success)
            (push (org-roam-node-title node) failure)))
        (with-current-buffer (get-buffer-create "*org-roam-epub*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "org-roam-epub bulk export -- tag: %s\n" tag))
            (insert (make-string 60 ?-) "\n")
            (insert (format "Total:   %d\n" total))
            (insert (format "Success: %d\n" success))
            (insert (format "Failed:  %d\n\n" (length failure)))
            (when failure
              (insert "Failed nodes:\n")
              (dolist (title (nreverse failure))
                (insert (format "  - %s\n" title))))
            (insert "\nOutput directory: " org-roam-epub-output-dir "\n"))
          (special-mode))
        (display-buffer "*org-roam-epub*")
        (message "org-roam-epub: Done. %d/%d succeeded." success total)))))

;;;###autoload
(defun org-roam-epub-export-select-tag ()
  "Interactively choose a tag and bulk-export all matching org-roam nodes."
  (interactive)
  (let ((tag (completing-read "Export nodes with tag: "
                              (org-roam-tag-completions)
                              nil nil nil nil
                              org-roam-epub-export-tag)))
    (org-roam-epub-export-by-tag tag)))

;;;###autoload
(defun org-roam-epub-export-node-by-title ()
  "Select an org-roam node by title and export it to EPUB."
  (interactive)
  (org-roam-epub--ensure-output-dir)
  (let ((node (org-roam-node-read nil nil nil t)))
    (if (org-roam-epub--export-node node)
        (message "org-roam-epub: Export complete -> %s"
                 (org-roam-epub--node-output-path node))
      (user-error "org-roam-epub: Export failed - check *Messages* for details"))))

(provide 'org-roam-epub)

;;; org-roam-epub.el ends here
;;; org-roam-epub.el --- Export org-roam nodes to EPUB via pandoc -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (org-roam "2.0.0"))
;; Keywords: org-roam, epub, export, pandoc
;; URL: https://github.com/yourname/org-roam-epub

;;; Commentary:

;; This package provides commands to export org-roam nodes tagged with a
;; specific tag to EPUB format using pandoc.  It supports:
;;
;;   - Individual node export via `org-roam-epub-export-node-at-point'
;;   - Bulk export of all nodes matching a tag via `org-roam-epub-export-by-tag'
;;   - Interactive tag selection via `org-roam-epub-export-select-tag'
;;   - Post-export tagging of the source org file with a configurable tag
;;
;; Quick start:
;;
;;   (require 'org-roam-epub)
;;   (setq org-roam-epub-export-tag "epub-export")   ; tag to query
;;   (setq org-roam-epub-output-dir "~/Documents/epubs/")
;;
;; Then call one of:
;;   M-x org-roam-epub-export-node-at-point
;;   M-x org-roam-epub-export-by-tag
;;   M-x org-roam-epub-export-select-tag

;;; Code:

(require 'org)
(require 'org-roam)
(require 'cl-lib)

;;;; ─── Customisation ──────────────────────────────────────────────────────────

(defgroup org-roam-epub nil
  "Export org-roam nodes to EPUB via pandoc."
  :group 'org-roam
  :prefix "org-roam-epub-")

(defcustom org-roam-epub-export-tag "epub-export"
  "Tag used to identify org-roam nodes that should be exported to EPUB.
Nodes carrying this tag are included in bulk export operations."
  :type 'string
  :group 'org-roam-epub)

(defcustom org-roam-epub-exported-tag "epub-exported"
  "Tag added to a node after it has been successfully exported to EPUB.
Set to nil to disable post-export tagging."
  :type '(choice string (const nil))
  :group 'org-roam-epub)

(defcustom org-roam-epub-output-dir
  (expand-file-name "~/org-roam-epubs/")
  "Directory where exported EPUB files are written."
  :type 'directory
  :group 'org-roam-epub)

(defcustom org-roam-epub-pandoc-executable "pandoc"
  "Path to the pandoc executable."
  :type 'string
  :group 'org-roam-epub)

(defcustom org-roam-epub-pandoc-extra-args nil
  "List of additional arguments passed to pandoc on every export.
Example: \\='(\"--epub-cover-image\" \"/path/to/cover.png\")"
  :type '(repeat string)
  :group 'org-roam-epub)

(defcustom org-roam-epub-before-export-hook nil
  "Hook run before each individual node is exported.
The current buffer is the node's org file."
  :type 'hook
  :group 'org-roam-epub)

(defcustom org-roam-epub-after-export-hook nil
  "Hook run after each individual node is successfully exported.
The current buffer is the node's org file."
  :type 'hook
  :group 'org-roam-epub)

;;;; ─── Internal helpers ───────────────────────────────────────────────────────

(defun org-roam-epub--ensure-output-dir ()
  "Create `org-roam-epub-output-dir' if it does not exist."
  (unless (file-directory-p org-roam-epub-output-dir)
    (make-directory org-roam-epub-output-dir :parents)
    (message "org-roam-epub: Created output directory %s"
             org-roam-epub-output-dir)))

(defun org-roam-epub--node-output-path (node)
  "Return the full EPUB output path for NODE."
  (let* ((title (org-roam-node-title node))
         ;; Sanitise title for use as a filename
         (safe-title (replace-regexp-in-string
                      "[^[:alnum:]_\\-]" "_"
                      (string-trim title)))
         (filename (concat safe-title ".epub")))
    (expand-file-name filename org-roam-epub-output-dir)))

(defun org-roam-epub--build-pandoc-command (input-file output-file)
  "Build the pandoc shell command list for INPUT-FILE → OUTPUT-FILE."
  (append
   (list org-roam-epub-pandoc-executable
         input-file
         "--from" "org"
         "--to"   "epub"
         "--output" output-file)
   org-roam-epub-pandoc-extra-args))

(defun org-roam-epub--add-exported-tag (file)
  "Add `org-roam-epub-exported-tag' to the #+filetags: line in FILE.
Creates the filetags line if absent.  Does nothing when
`org-roam-epub-exported-tag' is nil."
  (when org-roam-epub-exported-tag
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (let ((tag (concat ":" org-roam-epub-exported-tag ":")))
          (if (re-search-forward "^#\\+filetags:.*$" nil t)
              ;; Line exists – append tag if not already present
              (let ((line (match-string 0)))
                (unless (string-match-p (regexp-quote tag) line)
                  (goto-char (match-beginning 0))
                  ;; Rewrite the filetags line, inserting the new tag
                  (let* ((existing (match-string 0))
                         (current-tags
                          (if (string-match "#\\+filetags:\\s-*\\(.*\\)" existing)
                              (string-trim (match-string 1 existing))
                            ""))
                         (new-tags
                          (if (string-empty-p current-tags)
                              tag
                            ;; Insert inside existing :tag1:tag2: block
                            (if (string-match-p "^:.*:$" current-tags)
                                (concat (string-remove-suffix ":" current-tags)
                                        org-roam-epub-exported-tag ":")
                              (concat current-tags " " tag)))))
                    (delete-region (line-beginning-position)
                                   (line-end-position))
                    (insert (format "#+filetags: %s" new-tags)))))
            ;; No filetags line – insert one after the last keyword block
            (goto-char (point-min))
            (if (re-search-forward "^#\\+" nil t)
                (progn (end-of-line) (newline))
              (goto-char (point-min)))
            (insert (format "#+filetags: %s\n" tag)))))
      (save-buffer)
      ;; Keep org-roam DB in sync
      (when (fboundp 'org-roam-db-update-file)
        (org-roam-db-update-file file)))))

(defun org-roam-epub--export-node (node &optional silent)
  "Export NODE to EPUB using pandoc.
Returns t on success, nil on failure.
When SILENT is non-nil, suppress progress messages."
  (let* ((file        (org-roam-node-file node))
         (title       (org-roam-node-title node))
         (output-file (org-roam-epub--node-output-path node))
         (cmd         (org-roam-epub--build-pandoc-command file output-file)))
    (unless silent
      (message "org-roam-epub: Exporting \"%s\" -> %s ..." title output-file))
    (with-current-buffer (find-file-noselect file)
      (run-hooks 'org-roam-epub-before-export-hook)
      (let ((exit-code (apply #'call-process (car cmd) nil nil nil (cdr cmd))))
        (if (zerop exit-code)
            (progn
              (unless silent
                (message "org-roam-epub: Exported \"%s\" successfully" title))
              (org-roam-epub--add-exported-tag file)
              (run-hooks 'org-roam-epub-after-export-hook)
              t)
          (message "org-roam-epub: pandoc failed (exit %d) for \"%s\""
                   exit-code title)
          nil)))))

(defun org-roam-epub--nodes-with-tag (tag)
  "Return a list of org-roam nodes that carry TAG."
  (cl-remove-if-not
   (lambda (node)
     (member tag (org-roam-node-tags node)))
   (org-roam-node-list)))

;;;; ─── Interactive commands ───────────────────────────────────────────────────

;;;###autoload
(defun org-roam-epub-export-node-at-point ()
  "Export the org-roam node at point (or current file) to EPUB.

The EPUB is written to `org-roam-epub-output-dir'.  After a
successful export, `org-roam-epub-exported-tag' is added to the
source file's #+filetags:."
  (interactive)
  (org-roam-epub--ensure-output-dir)
  (if-let ((node (org-roam-node-at-point 'assert)))
      (if (org-roam-epub--export-node node)
          (message "org-roam-epub: Export complete → %s"
                   (org-roam-epub--node-output-path node))
        (user-error "org-roam-epub: Export failed – check *Messages* for details"))
    (user-error "org-roam-epub: No org-roam node found at point")))

;;;###autoload
(defun org-roam-epub-export-by-tag (&optional tag)
  "Bulk-export all org-roam nodes tagged with TAG to EPUB.

TAG defaults to `org-roam-epub-export-tag'.  When called
interactively with a prefix argument (\\[universal-argument]),
prompts for a different tag.

Results are reported in a *org-roam-epub* summary buffer."
  (interactive
   (list (if current-prefix-arg
             (completing-read "Export nodes with tag: "
                              (org-roam-tag-completions)
                              nil nil nil nil
                              org-roam-epub-export-tag)
           org-roam-epub-export-tag)))
  (let* ((tag   (or tag org-roam-epub-export-tag))
         (nodes (org-roam-epub--nodes-with-tag tag)))
    (if (null nodes)
        (message "org-roam-epub: No nodes found with tag \"%s\"" tag)
      (org-roam-epub--ensure-output-dir)
      (let ((total   (length nodes))
            (success 0)
            (failure '()))
        (message "org-roam-epub: Exporting %d node(s) tagged \"%s\" ..."
                 total tag)
        (dolist (node nodes)
          (if (org-roam-epub--export-node node t)
              (cl-incf success)
            (push (org-roam-node-title node) failure)))
        ;; Summary buffer
        (with-current-buffer (get-buffer-create "*org-roam-epub*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "org-roam-epub bulk export — tag: %s\n" tag))
            (insert (make-string 60 ?─) "\n")
            (insert (format "Total:   %d\n" total))
            (insert (format "Success: %d\n" success))
            (insert (format "Failed:  %d\n\n" (length failure)))
            (when failure
              (insert "Failed nodes:\n")
              (dolist (title (nreverse failure))
                (insert (format "  • %s\n" title))))
            (insert "\nOutput directory: " org-roam-epub-output-dir "\n"))
          (special-mode))
        (display-buffer "*org-roam-epub*")
        (message "org-roam-epub: Done. %d/%d succeeded." success total)))))

;;;###autoload
(defun org-roam-epub-export-select-tag ()
  "Interactively choose a tag and bulk-export all matching org-roam nodes.

This is equivalent to calling `org-roam-epub-export-by-tag' with
a manually chosen tag, but more discoverable via M-x."
  (interactive)
  (let ((tag (completing-read "Export nodes with tag: "
                              (org-roam-tag-completions)
                              nil nil nil nil
                              org-roam-epub-export-tag)))
    (org-roam-epub-export-by-tag tag)))

;;;###autoload
(defun org-roam-epub-export-node-by-title ()
  "Select an org-roam node by title and export it to EPUB."
  (interactive)
  (org-roam-epub--ensure-output-dir)
  (let ((node (org-roam-node-read nil nil nil t)))
    (if (org-roam-epub--export-node node)
        (message "org-roam-epub: Export complete → %s"
                 (org-roam-epub--node-output-path node))
      (user-error "org-roam-epub: Export failed – check *Messages* for details"))))

;;;; ─── Minor mode & keymap (optional convenience) ────────────────────────────

;; (defvar org-roam-epub-command-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "e") #'org-roam-epub-export-node-at-point)
;;     (define-key map (kbd "b") #'org-roam-epub-export-by-tag)
;;     (define-key map (kbd "s") #'org-roam-epub-export-select-tag)
;;     (define-key map (kbd "n") #'org-roam-epub-export-node-by-title)
;;     map)
;;   "Keymap for `org-roam-epub' commands.
;; Bind this to a prefix key of your choice, e.g.:
;;   (global-set-key (kbd \"C-c r e\") org-roam-epub-command-map)")

(provide 'org-roam-epub)

;;; org-roam-epub.el ends here
