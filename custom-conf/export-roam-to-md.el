;; export-roam-to-md.el  -*- lexical-binding: t; -*-
;; Run headless: emacs --batch --load export-roam-to-md.el
;; Monitor:      tail -f /path/to/export.log
;; Kill cleanly: kill $(cat /path/to/export.pid)

(require 'org)
(require 'org-roam)
;;; ── Configuration ─────────────────────────────────────────────────────────────

(setq org-roam-directory (expand-file-name "~/Dropbox/org-files/"))
(setq *md-output-dir*    (expand-file-name "~/test-export-to-md/"))
(setq *log-file*         (expand-file-name "~/test-export-to-md/export.log"))
(setq *pid-file*         (expand-file-name "~/test-export-to-md/export.pid"))
(setq *pandoc-timeout*   30)
(setq *force-reexport*   nil)

;; Set to nil to export everything.
;; Set to a list of one or more tag strings to export only nodes that
;; carry ALL of the listed tags (AND semantics).
;; For OR semantics — any one of the tags is enough — set *tag-match* to 'any.
;;
;; Examples:
;;   (setq *export-tags* nil)                        ; export all
;;   (setq *export-tags* '("recipes"))               ; only recipes
;;   (setq *export-tags* '("project" "active"))      ; both tags required
;;   (setq *export-tags* '("recipes" "vegetarian"))  ; both tags required
;;   (setq *tag-match*   'any)                       ; either tag is enough
(setq *export-tags* nil)
(setq *tag-match*   'all)   ; 'all = AND,  'any = OR
;;; ── Logging ───────────────────────────────────────────────────────────────────

(defun my/log (level fmt &rest args)
  "Write a timestamped log line to *log-file* and to stderr.
LEVEL is a string: INFO, WARN, ERROR, DEBUG."
  (let* ((ts  (format-time-string "%Y-%m-%dT%H:%M:%S"))
         (msg (apply #'format fmt args))
         (line (format "[%s] %s %s\n" ts level msg)))
    (write-region line nil *log-file* 'append 'quiet)
    (message "%s" (string-trim-right line))))

;;; ── PID file ──────────────────────────────────────────────────────────────────

(defun my/write-pid ()
  (write-region (format "%d\n" (emacs-pid)) nil *pid-file* nil 'quiet)
  (my/log "INFO" "PID %d written to %s" (emacs-pid) *pid-file*))

(defun my/remove-pid ()
  (when (file-exists-p *pid-file*)
    (delete-file *pid-file*)))

;;; ── Tag filtering ─────────────────────────────────────────────────────────────

(defun my/node-matches-tags-p (node)
  "Return t if NODE satisfies the *export-tags* / *tag-match* filter.
Always returns t when *export-tags* is nil (export everything)."
  (if (null *export-tags*)
      t
    (let ((node-tags (org-roam-node-tags node)))
      (pcase *tag-match*
        ('all  (cl-every  (lambda (tag) (member tag node-tags)) *export-tags*))
        ('any  (cl-some   (lambda (tag) (member tag node-tags)) *export-tags*))
        (_     (error "Unknown *tag-match* value: %s (use 'all or 'any)"
                      *tag-match*))))))
;;; ── Helpers ───────────────────────────────────────────────────────────────────

(defun my/roam-id-to-title (id)
  "Resolve an org-roam node ID to its title, or nil if not found."
  (condition-case nil
      (when-let ((node (org-roam-node-from-id id)))
        (org-roam-node-title node))
    (error nil)))

(defun my/sanitize-filename (title)
  "Convert TITLE to a filesystem-safe string."
  (string-trim
   (replace-regexp-in-string
    "[/\\:*?\"<>|#]" "_"
    (replace-regexp-in-string "[ \t]+" " " title))))

(defun my/unique-out-path (safe-title node-id seen)
  "Return a unique .md output path; appends short UUID on collision."
  (let ((candidate safe-title))
    (when (gethash candidate seen)
      (setq candidate (format "%s_%s" safe-title (substring node-id 0 8))))
    (puthash candidate t seen)
    (expand-file-name (concat candidate ".md") *md-output-dir*)))

(defun my/node-org-content (node)
  "Extract the org source text for NODE (whole file or subtree)."
  (let ((file  (org-roam-node-file node))
        (point (org-roam-node-point node))
        (level (org-roam-node-level node)))
    (if (= level 0)
        (with-temp-buffer
          (insert-file-contents file)
          (buffer-string))
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)
        (goto-char point)
        (org-narrow-to-subtree)
        (buffer-string)))))

(defun my/clean-link-target (target)
  "Sanitize a link target string for pandoc's strict org reader.
Handles: newlines, backslashes, double-quotes, carets, equals, spaces.
Leaves id: links (org-roam UUIDs) completely untouched."
  (if (string-prefix-p "id:" target)
      target
    (let ((s target))
      ;; Newlines and surrounding whitespace → single space
      (setq s (replace-regexp-in-string "[ \t]*\n[ \t]*" " " s))
      (cond
       ;; http/https URLs: percent-encode chars pandoc can't handle
       ;; (= is valid in query strings but ^ and " are not)
       ((string-match-p "^https?://" s)
        (setq s (replace-regexp-in-string
                 "[\"\\^]"
                 (lambda (c) (pcase c
                               ("\"" "%22")
                               ("\\" "%5C")
                               ("^"  "%5E")
                               (_    c)))
                 s)))
       ;; file: paths: normalize backslashes to forward slashes
       ((string-match-p "^file:" s)
        (setq s (replace-regexp-in-string "\\\\" "/" s)))
       ;; Everything else: collapse whitespace, strip lone backslashes
       (t
        (setq s (replace-regexp-in-string "\\\\" "" s))
        (setq s (replace-regexp-in-string "[ \t]+" " " s))))
      s)))

(defun my/sanitize-all-links-in-buffer ()
  "Char-walk current buffer sanitizing every [[target]] and [[target][desc]].
Rewrites the target portion only; descriptions are left intact."
  (goto-char (point-min))
  (while (search-forward "[[" nil t)
    (let ((link-start   (- (point) 2))
          (target-start (point))
          target-end
          has-desc
          (depth 1))
      ;; Walk forward tracking nesting to find target boundary
      (while (and (not (eobp)) (> depth 0))
        (cond
         ;; Nested [[
         ((looking-at "\\[\\[")
          (setq depth (1+ depth))
          (forward-char 2))
         ;; ][ at top level = separator between target and description
         ((and (= depth 1) (looking-at "\\]\\["))
          (setq target-end (point)
                has-desc    t
                depth       0))
         ;; ]] at top level = end of bare link
         ((and (= depth 1) (looking-at "\\]\\]"))
          (setq target-end (point)
                has-desc    nil
                depth       0))
         ;; ]] closing a nested [[
         ((looking-at "\\]\\]")
          (setq depth (1- depth))
          (when (> depth 0) (forward-char 2)))
         (t (forward-char 1))))

      (when target-end
        (let* ((target (buffer-substring target-start target-end))
               (clean  (my/clean-link-target target)))
          (unless (string= target clean)
            (delete-region target-start target-end)
            (insert clean))
          ;; Always advance past the (possibly rewritten) target
          (goto-char (+ target-start (length clean))))))))

(defun my/preprocess-org-for-pandoc (org-text)
  "Multi-pass sanitization of org text targeting all known pandoc failures.
Passes:
  1. Link targets  — newlines, \\, \", ^, = inside [[ ]] brackets
  2. Org line breaks — \\\\ at end of line replaced with plain newline
  3. Keyword lines — #+INCLUDE stripped (unresolvable from temp paths)"
  (with-temp-buffer
    (insert org-text)
    ;; Pass 1
    (my/sanitize-all-links-in-buffer)
    ;; Pass 2: \\ line-break markers
    (goto-char (point-min))
    (while (re-search-forward "\\\\\\\\[ \t]*$" nil t)
      (replace-match ""))
    ;; Pass 3: #+INCLUDE (resolves relative to source dir, not /tmp)
    (goto-char (point-min))
    (while (re-search-forward "^#\\+INCLUDE:.*\n" nil t)
      (replace-match ""))
    (buffer-string)))

;;; ── ox-md fallback ────────────────────────────────────────────────────────────

(defun my/ox-md-convert (org-text)
  "Convert ORG-TEXT to markdown using Emacs ox-md as a second-tier fallback.
Applies the quote-block nil-contents guard that prevents the common
'Wrong type argument: arrayp, nil' crash."
  ;; Guard against the known ox-md quote-block bug
  (unless (advice-member-p 'my/ox-md-quote-block-guard 'org-md-quote-block)
    (advice-add 'org-md-quote-block :around
                (lambda (orig-fn block contents info)
                  (when contents
                    (funcall orig-fn block contents info)))
                '((name . my/ox-md-quote-block-guard))))
  (condition-case err
      (with-temp-buffer
        (insert org-text)
        (org-mode)
        (let ((org-export-with-toc nil)
              (org-export-with-properties t))
          (org-export-as 'md nil nil t)))
    (error
     (my/log "WARN" "ox-md also failed: %s" (error-message-string err))
     nil)))

(defun my/pandoc-org-to-gfm (org-text node-id source-file)
  "Convert ORG-TEXT to GFM with three tiers:
  1. pandoc  — fast, best output, fails on some edge cases
  2. ox-md   — slower, Emacs-native, handles most of what pandoc can't
  3. plaintext — last resort, always succeeds
Returns a plist with :status ('ok 'oxmd 'plaintext 'timeout) :output :stderr."
  (let* ((source-dir  (file-name-directory source-file))
         (clean-text  (my/preprocess-org-for-pandoc org-text))
         (tmp-in      (make-temp-file "roam-in-"     nil ".org"))
         (tmp-stderr  (make-temp-file "roam-stderr-" nil ".txt"))
         (stdout-buf  (generate-new-buffer " *pandoc-stdout*"))
         result)
    (unwind-protect
        (progn
          (write-region clean-text nil tmp-in nil 'quiet)
          (let* ((exit-code
                  (let ((default-directory source-dir))
                    (call-process "timeout" nil
                                  (list stdout-buf tmp-stderr)
                                  nil
                                  (number-to-string *pandoc-timeout*)
                                  "pandoc"
                                  "-f" "org"
                                  "-t" "gfm"
                                  "--wrap=none"
                                  (format "--resource-path=%s" source-dir)
                                  tmp-in)))
                 (stderr-str
                  (let ((s (with-temp-buffer
                             (when (file-exists-p tmp-stderr)
                               (insert-file-contents tmp-stderr))
                             (string-trim (buffer-string)))))
                    (when (> (length s) 0) s)))
                 (stdout-str
                  (with-current-buffer stdout-buf (buffer-string))))
            (setq result
                  (cond
                   ;; Tier 1: pandoc succeeded
                   ((= exit-code 0)
                    (list :status 'ok
                          :output stdout-str
                          :stderr stderr-str))
                   ;; Timeout: don't attempt ox-md (may also hang)
                   ((= exit-code 124)
                    (list :status 'timeout
                          :output (format "<!-- pandoc timed out after %ds for node %s -->"
                                          *pandoc-timeout* node-id)
                          :stderr stderr-str))
                   ;; Tier 2: pandoc failed — try ox-md
                   (t
                    (my/log "WARN" "pandoc exit %d for %s — trying ox-md fallback"
                            exit-code node-id)
                    (let ((oxmd (my/ox-md-convert org-text)))  ; use raw, not clean-text
                      (if oxmd
                          (list :status 'oxmd
                                :output oxmd
                                :stderr stderr-str)
                        ;; Tier 3: both failed — plaintext
                        (list :status 'plaintext
                              :output (my/org-text-to-plaintext-fallback
                                       org-text node-id)
                              :stderr stderr-str))))))))
      (when (file-exists-p tmp-in)     (delete-file tmp-in))
      (when (file-exists-p tmp-stderr) (delete-file tmp-stderr))
      (when (buffer-live-p stdout-buf) (kill-buffer stdout-buf)))
    result))


(defun my/rewrite-roam-links (text)
  "Replace [[id:UUID][desc]] and bare [[id:UUID]] with Obsidian wikilinks."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward
            "\\[\\[id:\\([^]\n]+\\)\\]\\[\\([^]\n]+\\)\\]\\]" nil t)
      (let* ((id    (match-string 1))
             (desc  (match-string 2))
             (title (my/roam-id-to-title id)))
        (replace-match (if title (format "[[%s|%s]]" title desc) desc)
                       t t)))
    (goto-char (point-min))
    (while (re-search-forward "\\[\\[id:\\([^]\n]+\\)\\]\\]" nil t)
      (let* ((id    (match-string 1))
             (title (my/roam-id-to-title id)))
        (replace-match (if title (format "[[%s]]" title) id)
                       t t)))
    (buffer-string)))

(defun my/build-frontmatter (node)
  "Return a YAML frontmatter string for NODE."
  (let* ((title   (org-roam-node-title node))
         (id      (org-roam-node-id node))
         (tags    (org-roam-node-tags node))
         (aliases (org-roam-node-aliases node))
         (level   (org-roam-node-level node))
         (file    (org-roam-node-file node))
         (fmt-list (lambda (lst)
                     (concat "["
                             (mapconcat (lambda (s) (format "\"%s\"" s))
                                        lst ", ")
                             "]"))))
    (format (concat "---\n"
                    "title: \"%s\"\n"
                    "roam_id: \"%s\"\n"
                    "roam_level: %d\n"
                    "source_file: \"%s\"\n"
                    "tags: %s\n"
                    "aliases: %s\n"
                    "---\n\n")
            (replace-regexp-in-string "\"" "\\\\\"" title)
            id
            level
            (file-relative-name file org-roam-directory)
            (funcall fmt-list tags)
            (funcall fmt-list aliases))))

;;; ── Per-node export ───────────────────────────────────────────────────────────
(defun my/export-node (node seen counters)
  "Export NODE to its own markdown file. SEEN prevents filename collisions.
COUNTERS is a plist with :ok :oxmd :plaintext :timeout :error keys."
  (let* ((id         (org-roam-node-id node))
         (title      (org-roam-node-title node))
         (level      (org-roam-node-level node))
         (src-file   (file-relative-name (org-roam-node-file node)
                                         org-roam-directory))
         (safe-title (my/sanitize-filename title))
         (out-path   (my/unique-out-path safe-title id seen)))

    (my/log "INFO" "START [L%d] \"%s\" (%s) → %s"
            level title src-file (file-name-nondirectory out-path))

    (cond
     ;; ── Already exported ──────────────────────────────────────────────────
     ((and (not *force-reexport*) (file-exists-p out-path))
      (my/log "INFO" "SKIP  already exists: %s" (file-name-nondirectory out-path))
      (plist-put counters :skipped (1+ (plist-get counters :skipped))))

     ;; ── Export ───────────────────────────────────────────────────────────
     (t
      (condition-case err
          (let* ((org-content   (my/node-org-content node))
                 (pandoc-result (my/pandoc-org-to-gfm org-content id
                                                       (org-roam-node-file node)))
                 (pan-status    (plist-get pandoc-result :status))
                 (pan-output    (plist-get pandoc-result :output))
                 (pan-stderr    (plist-get pandoc-result :stderr))
                 (final-md      (if (eq pan-status 'ok)
                                    (my/rewrite-roam-links pan-output)
                                  pan-output))
                 (frontmatter   (my/build-frontmatter node)))

            ;; Log any stderr pandoc produced
            (when pan-stderr
              (my/log (if (eq pan-status 'ok) "WARN" "ERROR")
                      "PANDOC_STDERR [%s] \"%s\" (%s):\n%s"
                      (upcase (symbol-name pan-status))
                      title src-file pan-stderr))

            ;; Update counters and log outcome — all inside the let* scope
            (pcase pan-status
              ('ok
               (my/log "INFO" "DONE  [OK] \"%s\"" title)
               (plist-put counters :ok (1+ (plist-get counters :ok))))
              ('oxmd
               (my/log "INFO" "DONE  [OXMD] \"%s\"" title)
               (plist-put counters :oxmd (1+ (plist-get counters :oxmd))))
              ('plaintext
               (my/log "WARN" "DONE  [PLAINTEXT] \"%s\"" title)
               (plist-put counters :plaintext (1+ (plist-get counters :plaintext))))
              ('timeout
               (my/log "WARN" "TIMEOUT after %ds for \"%s\"" *pandoc-timeout* title)
               (plist-put counters :timeout (1+ (plist-get counters :timeout)))))

            ;; Write the file regardless of which tier succeeded
            (write-region (concat frontmatter final-md)
                          nil out-path nil 'quiet))

        ;; ── Catch any Elisp-level exception ───────────────────────────────
        (error
         (my/log "ERROR" "EXCEPTION \"%s\" (%s): %s"
                 title src-file (error-message-string err))
         (plist-put counters :error (1+ (plist-get counters :error)))))))

    counters))

;;; ── Main ──────────────────────────────────────────────────────────────────────

(my/write-pid)
(write-region "" nil *log-file* nil 'quiet)
(my/log "INFO" "=== export-roam-to-md starting ===")
(my/log "INFO" "roam dir  : %s" org-roam-directory)
(my/log "INFO" "output    : %s" *md-output-dir*)
(my/log "INFO" "log file  : %s" *log-file*)
(my/log "INFO" "timeout   : %ds per pandoc call" *pandoc-timeout*)
(my/log "INFO" "force     : %s" (if *force-reexport* "yes" "no"))
(my/log "INFO" "tag filter: %s"
        (if *export-tags*
            (format "%s [match=%s]" *export-tags* *tag-match*)
          "none (export all)"))

(make-directory *md-output-dir* t)
(org-roam-db-sync)

(let* ((all-nodes (org-roam-node-list))
       (nodes     (if *export-tags*
                      (cl-remove-if-not #'my/node-matches-tags-p all-nodes)
                    all-nodes))
       (total-all (length all-nodes))
       (total     (length nodes))
       (seen      (make-hash-table :test 'equal))
       (counters  (list :ok 0 :oxmd 0 :plaintext 0 :skipped 0 :timeout 0 :error 0))
       (count     0))

  (my/log "INFO" "Total nodes in db : %d" total-all)
  (my/log "INFO" "Nodes to export   : %d" total)

  (when (and *export-tags* (= total 0))
    (my/log "WARN" "No nodes matched tags %s — nothing to export." *export-tags*))

  (dolist (node nodes)
    (setq count (1+ count))
    (when (= (mod count 50) 0)
      (my/log "INFO" "Progress %d / %d  (ok=%d oxmd=%d plain=%d timeout=%d err=%d)"
              count total
              (plist-get counters :ok)
              (plist-get counters :oxmd)
              (plist-get counters :plaintext)
              (plist-get counters :skipped)
              (plist-get counters :timeout)
              (plist-get counters :error)))
    (setq counters (my/export-node node seen counters)))

  (my/log "INFO" "=== Export complete ===")
  (my/log "INFO" "Matched / Total : %d / %d" total total-all)
  (my/log "INFO" "OK              : %d" (plist-get counters :ok))
  (my/log "INFO" "ox-md fallback  : %d" (plist-get counters :oxmd))
  (my/log "INFO" "Plaintext       : %d" (plist-get counters :plaintext))
  (my/log "INFO" "Skipped         : %d" (plist-get counters :skipped))
  (my/log "INFO" "Timeout         : %d" (plist-get counters :timeout))
  (my/log "INFO" "Error           : %d" (plist-get counters :error)))

(my/remove-pid)
