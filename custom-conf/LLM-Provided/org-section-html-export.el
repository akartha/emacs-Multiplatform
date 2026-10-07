;;; org-section-html-export.el --- Export Org top-level headings to HTML -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'org)
(require 'ox)
(require 'ox-html)
(require 'seq)
(require 'subr-x)
(require 'crm)

(defgroup ak-org-section-export nil
  "Export top-level Org sections to standalone files."
  :group 'org)

(defcustom ak/org-section-export-output-directory nil
  "Target directory for section exports.

If nil, empty, or pointing to a directory that does not exist, export
to the current Org file's directory instead."
  :type '(choice
          (const :tag "Use current Org file directory" nil)
          directory)
  :group 'ak-org-section-export)

(defcustom ak/org-section-export-copy-linked-assets t
  "Whether to copy local file links into the export location.

When non-nil, local Org file links such as:

  [[file:images/foo.png]]
  [[./docs/source.pdf][source]]

are copied into an asset directory next to the exported file, and the
links are rewritten to point to the copied files."
  :type 'boolean
  :group 'ak-org-section-export)

(defcustom ak/org-section-export-assets-directory-suffix "_files"
  "Suffix used for per-export asset directories.

For an output file named:

  my-article.html

linked assets are copied into:

  my-article_files/"
  :type 'string
  :group 'ak-org-section-export)

(defcustom ak/org-section-export-copied-keywords
  '("SETUPFILE"
    "HTML_HEAD"
    "HTML_HEAD_EXTRA"
    "HTML_DOCTYPE"
    "HTML_CONTAINER"
    "HTML_LINK_HOME"
    "HTML_LINK_UP"
    "LANGUAGE")
  "File-level Org keywords copied into the temporary export buffer.

TITLE, AUTHOR, and OPTIONS are handled separately per exported section."
  :type '(repeat string)
  :group 'ak-org-section-export)

(defcustom ak/org-section-export-html-include-default-style nil
  "Whether Org's default HTML CSS should be included.

When nil, the exported HTML relies primarily on CSS supplied through
SETUPFILE or copied HTML_HEAD / HTML_HEAD_EXTRA keywords."
  :type 'boolean
  :group 'ak-org-section-export)

(defcustom ak/org-section-export-html-include-scripts nil
  "Whether Org's default HTML JavaScript should be included."
  :type 'boolean
  :group 'ak-org-section-export)

(defcustom ak/org-section-export-html-toplevel-hlevel 1
  "HTML heading level used for top-level Org headings."
  :type 'integer
  :group 'ak-org-section-export)

(defun ak/org-section-export--escape-link-path (path)
  "Escape PATH for use inside an Org file link."
  (if (fboundp 'org-link-escape)
      (org-link-escape path)
    path))

(defun ak/org-section-export--local-file-link-target (path source-dir)
  "Resolve Org file link PATH relative to SOURCE-DIR.

Return absolute local path, or nil for remote/non-local paths."
  (let ((p (org-link-unescape (or path ""))))
    (setq p (string-trim p))
    (cond
     ((string-empty-p p)
      nil)

     ;; Skip URL-like paths accidentally appearing in file links.
     ((and (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" p)
           (not (file-name-absolute-p p)))
      nil)

     ;; Skip TRAMP/remote files.
     ((file-remote-p p)
      nil)

     (t
      (let ((expanded (expand-file-name p source-dir)))
        (unless (file-remote-p expanded)
          expanded))))))

(defun ak/org-section-export--unique-asset-destination (source-file asset-dir used-names)
  "Return a unique destination path for SOURCE-FILE inside ASSET-DIR.

USED-NAMES is a hash table tracking filenames already used during this
export."
  (let* ((base (file-name-nondirectory
                (directory-file-name source-file)))
         (base (if (string-empty-p base) "asset" base))
         (stem (file-name-sans-extension base))
         (ext (or (file-name-extension base t) ""))
         (candidate base)
         (n 1))
    (while (gethash candidate used-names)
      (setq candidate (format "%s-%d%s" stem n ext))
      (cl-incf n))
    (puthash candidate t used-names)
    (expand-file-name candidate asset-dir)))

(defun ak/org-section-export--make-file-link-replacement
    (relative-path description format search-option)
  "Return rewritten Org file link.

RELATIVE-PATH is the copied asset path relative to the exported HTML.
DESCRIPTION is the original link description, if any.
FORMAT is the original Org link format.
SEARCH-OPTION preserves file search suffixes such as ::heading."
  (let* ((escaped-path
          (ak/org-section-export--escape-link-path relative-path))
         (target
          (concat
           "file:"
           escaped-path
           (if (ak/org-section-export--nonblank search-option)
               (concat "::" search-option)
             ""))))
    (cond
     (description
      (org-link-make-string target description))

     ((eq format 'plain)
      target)

     ((eq format 'angle)
      (format "<%s>" target))

     (t
      (format "[[%s]]" target)))))

(defun ak/org-section-export--copy-local-linked-assets
    (org-text source-dir output-dir filename-base)
  "Copy local file links in ORG-TEXT into OUTPUT-DIR.

SOURCE-DIR is used to resolve relative Org links.
FILENAME-BASE is used to create the per-export asset directory.

Return a plist:

  :text    rewritten Org text
  :count   number of assets copied
  :missing list of local file paths that could not be copied"
  (let* ((asset-subdir
          (concat filename-base
                  ak/org-section-export-assets-directory-suffix))
         (asset-dir
          (expand-file-name asset-subdir output-dir))
         (source-to-relative
          (make-hash-table :test 'equal))
         (used-names
          (make-hash-table :test 'equal))
         (copied-count 0)
         missing)

    (cl-labels
        ((copy-one
          (source-file)
          (or (gethash source-file source-to-relative)
              (cond
               ((file-regular-p source-file)
                (make-directory asset-dir t)
                (let* ((dest
                        (ak/org-section-export--unique-asset-destination
                         source-file
                         asset-dir
                         used-names))
                       (relative
                        (file-relative-name dest output-dir)))
                  (copy-file source-file dest t)
                  (puthash source-file relative source-to-relative)
                  (cl-incf copied-count)
                  relative))

               ;; Keep this conservative: copy regular files only.
               ;; Directory-copying can be added later if desired.
               (t
                (push source-file missing)
                nil)))))

      (with-temp-buffer
        (insert org-text)
        (org-mode)

        (let (links)
          (setq links
                (org-element-map (org-element-parse-buffer) 'link
                  (lambda (link)
                    (when (string= (org-element-property :type link) "file")
                      (let* ((path
                              (org-element-property :path link))
                             (source-file
                              (ak/org-section-export--local-file-link-target
                               path
                               source-dir))
                             (relative
                              (and source-file
                                   (copy-one source-file))))
                        (when relative
                          (let ((contents-begin
                                 (org-element-property :contents-begin link))
                                (contents-end
                                 (org-element-property :contents-end link)))
                            (list
                             :begin (org-element-property :begin link)
                             :end (org-element-property :end link)
                             :format (org-element-property :format link)
                             :description
                             (when contents-begin
                               (buffer-substring-no-properties
                                contents-begin
                                contents-end))
                             :search-option
                             (org-element-property :search-option link)
                             :relative relative))))))))

          ;; Replace from end to beginning so buffer positions stay valid.
          (dolist (link (sort links
                              (lambda (a b)
                                (> (plist-get a :begin)
                                   (plist-get b :begin)))))
            (goto-char (plist-get link :begin))
            (delete-region (plist-get link :begin)
                           (plist-get link :end))
            (insert
             (ak/org-section-export--make-file-link-replacement
              (plist-get link :relative)
              (plist-get link :description)
              (plist-get link :format)
              (plist-get link :search-option)))))

        (list :text (buffer-string)
              :count copied-count
              :missing (nreverse missing))))))

(defun ak/org-section-export--nonblank (s)
  "Return trimmed S, or nil if S is empty."
  (when (stringp s)
    (let ((trimmed (string-trim s)))
      (unless (string-empty-p trimmed)
        trimmed))))

(defun ak/org-section-export--single-line (s)
  "Return S as a single trimmed line."
  (string-trim
   (replace-regexp-in-string
    "[[:space:]\n\r]+" " " (or s ""))))

(defun ak/org-section-export--current-directory ()
  "Return the current Org file directory, or `default-directory'."
  (file-name-as-directory
   (expand-file-name
    (or (and buffer-file-name
             (file-name-directory buffer-file-name))
        default-directory))))

(defun ak/org-section-export--output-directory ()
  "Return configured export directory, falling back when invalid."
  (let ((dir (ak/org-section-export--nonblank
              ak/org-section-export-output-directory)))
    (if (and dir
             (file-directory-p (expand-file-name dir)))
        (file-name-as-directory
         (expand-file-name dir))
      (ak/org-section-export--current-directory))))

(defun ak/org-section-export--safe-name (s)
  "Convert S into a filesystem-safe filename base."
  (let* ((trimmed (ak/org-section-export--single-line
                   (or s "untitled")))
         (safe
          (replace-regexp-in-string
           "[^[:alnum:]_-]+"
           "-"
           (downcase trimmed))))
    (setq safe (string-trim safe "-+" "-+"))
    (if (string-empty-p safe)
        "untitled"
      ;; Keep filenames usable if the title contains a long URL.
      (substring safe 0 (min (length safe) 120)))))

(defun ak/org-section-export--first-heading-position ()
  "Return position of the first Org heading, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward org-heading-regexp nil t)
      (match-beginning 0))))

(defun ak/org-section-export--file-keyword (key)
  "Return file-level Org keyword KEY, such as TITLE or AUTHOR."
  (ak/org-section-export--nonblank
   (car (cdr (assoc (upcase key)
                    (org-collect-keywords
                     (list (upcase key))))))))

(defun ak/org-section-export--file-property-drawer-value (key)
  "Return file-level property drawer value for KEY, or nil.

This checks for a property drawer before the first heading."
  (let ((case-fold-search t)
        (limit (or (ak/org-section-export--first-heading-position)
                   (point-max)))
        value)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^[ \t]*:PROPERTIES:[ \t]*$" limit t)
        (let ((drawer-start (point))
              (drawer-end
               (save-excursion
                 (when (re-search-forward "^[ \t]*:END:[ \t]*$" limit t)
                   (match-beginning 0)))))
          (when drawer-end
            (goto-char drawer-start)
            (when (re-search-forward
                   (format "^[ \t]*:%s:[ \t]*\\(.*\\)$"
                           (regexp-quote (upcase key)))
                   drawer-end t)
              (setq value
                    (ak/org-section-export--nonblank
                     (match-string-no-properties 1))))))))
    value))

(defun ak/org-section-export--file-property-keyword-value (key)
  "Return #+PROPERTY value for KEY, or nil.

For example:

  #+PROPERTY: AUTHOR Arun Kartha"
  (let ((props (cdr (assoc "PROPERTY"
                           (org-collect-keywords '("PROPERTY")))))
        value)
    (catch 'found
      (dolist (prop props)
        (when (string-match
               (format "\\`[ \t]*%s[ \t]+\\(.+\\)\\'"
                       (regexp-quote (upcase key)))
               prop)
          (setq value
                (ak/org-section-export--nonblank
                 (match-string 1 prop)))
          (throw 'found value))))
    value))

(defun ak/org-section-export--file-metadata (key)
  "Return file-level metadata KEY.

Checks, in order:

1. Standard Org keyword, e.g. #+TITLE or #+AUTHOR
2. File-level property drawer before the first heading
3. #+PROPERTY metadata"
  (or (ak/org-section-export--file-keyword key)
      (ak/org-section-export--file-property-drawer-value key)
      (ak/org-section-export--file-property-keyword-value key)))

(defun ak/org-section-export--heading-property (key)
  "Return current heading property KEY, or nil."
  (ak/org-section-export--nonblank
   (org-entry-get nil (upcase key))))

(defun ak/org-section-export--org-link-display (s &optional include-url)
  "Return display text for Org string S without destroying link meaning.

This is used for UI labels, metadata, and filenames only. The exported
HTML body itself keeps the original Org links intact.

When INCLUDE-URL is non-nil:

  [[https://example.com][Example]]

becomes:

  Example <https://example.com>

When INCLUDE-URL is nil, the same link becomes:

  Example

Undescribed links keep their raw URL."
  (let ((text (or s "")))
    (with-temp-buffer
      (insert text)
      (org-mode)
      (let (links)
        (setq links
              (org-element-map (org-element-parse-buffer) 'link
                (lambda (link)
                  (let ((type (org-element-property :type link)))
                    (when type
                      (list
                       :begin (org-element-property :begin link)
                       :end (org-element-property :end link)
                       :contents-begin (org-element-property :contents-begin link)
                       :contents-end (org-element-property :contents-end link)
                       :raw-link (org-element-property :raw-link link)))))))

        ;; Work backwards so replacements do not invalidate earlier positions.
        (dolist (link (sort links
                            (lambda (a b)
                              (> (plist-get a :begin)
                                 (plist-get b :begin)))))
          (let* ((beg (plist-get link :begin))
                 (end (plist-get link :end))
                 (contents-beg (plist-get link :contents-begin))
                 (contents-end (plist-get link :contents-end))
                 (raw-link (plist-get link :raw-link))
                 (desc
                  (when contents-beg
                    (ak/org-section-export--single-line
                     (buffer-substring-no-properties
                      contents-beg contents-end))))
                 (replacement
                  (cond
                   ((and desc include-url)
                    (format "%s <%s>" desc raw-link))
                   (desc desc)
                   (t raw-link))))
            (goto-char beg)
            (delete-region beg end)
            (insert replacement))))

      (ak/org-section-export--single-line
       (buffer-string)))))

(defun ak/org-section-export--top-level-headings ()
  "Return top-level headings in current Org buffer.

Each item is a plist containing:

  :index
  :raw-title
  :marker"
  (let ((index 0)
        items)
    (org-map-entries
     (lambda ()
       (when (= (org-outline-level) 1)
         (cl-incf index)
         (push
          (list :index index
                ;; Important: strip text properties from fontified/folded links.
                :raw-title (substring-no-properties
                            (org-get-heading t t t t))
                :marker (point-marker))
          items)))
     nil 'file)
    (nreverse items)))

(defun ak/org-section-export--raw-title-for-heading (heading one-heading-p)
  "Return raw Org title text for HEADING.

For one-heading files, prefer file-level TITLE.
For multi-heading files, prefer heading-level TITLE property.
Links are intentionally kept as Org markup here."
  (let* ((marker (plist-get heading :marker))
         (heading-title (plist-get heading :raw-title))
         (title
          (save-excursion
            (goto-char marker)
            (if one-heading-p
                (or (ak/org-section-export--file-metadata "TITLE")
                    (ak/org-section-export--heading-property "TITLE")
                    heading-title)
              (or (ak/org-section-export--heading-property "TITLE")
                  heading-title)))))
    (or (ak/org-section-export--nonblank
         (substring-no-properties title))
        "Untitled")))

(defun ak/org-section-export--metadata-for-heading (heading one-heading-p)
  "Return metadata plist for HEADING."
  (let* ((marker (plist-get heading :marker))
         (raw-title
          (ak/org-section-export--raw-title-for-heading
           heading one-heading-p))
         ;; Used for UI and HTML metadata.
         ;; Described links retain both display text and URL.
         (display-title
          (ak/org-section-export--org-link-display raw-title t))
         ;; Used for filenames.
         ;; Described links use only their visible descriptive text.
         (filename-title
          (ak/org-section-export--org-link-display raw-title nil))
         (author
          (save-excursion
            (goto-char marker)
            (if one-heading-p
                (or (ak/org-section-export--file-metadata "AUTHOR")
                    (ak/org-section-export--heading-property "AUTHOR")
                    user-full-name)
              (or (ak/org-section-export--heading-property "AUTHOR")
                  (ak/org-section-export--file-metadata "AUTHOR")
                  user-full-name)))))
    (list :raw-title raw-title
          :display-title display-title
          :filename-title filename-title
          :author author)))

(defun ak/org-section-export--normalize-setupfile-path (value source-dir)
  "Normalize SETUPFILE VALUE relative to SOURCE-DIR."
  (let ((path (string-trim (or value ""))))
    ;; Strip simple surrounding quotes.
    (when (string-match "\\`[\"']\\(.+\\)[\"']\\'" path)
      (setq path (match-string 1 path)))
    (cond
     ;; Leave URLs and explicit URI-like paths alone.
     ((string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" path)
      path)
     ((file-name-absolute-p path)
      (expand-file-name path))
     (t
      (expand-file-name path source-dir)))))

(defun ak/org-section-export--file-preamble-lines (source-dir)
  "Return export-related file-level preamble lines.

Copies selected keywords before the first heading, including SETUPFILE.
Relative SETUPFILE paths are expanded relative to SOURCE-DIR.

Also supports a file-level property drawer value:

  :SETUPFILE: path/to/setup.org"
  (let ((case-fold-search t)
        (limit (or (ak/org-section-export--first-heading-position)
                   (point-max)))
        lines)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^[ \t]*#\\+\\([A-Za-z0-9_]+\\):[ \t]*\\(.*\\)$"
              limit t)
        (let* ((key (upcase (match-string-no-properties 1)))
               (value (match-string-no-properties 2)))
          (when (member key ak/org-section-export-copied-keywords)
            (push
             (format "#+%s: %s"
                     key
                     (if (string= key "SETUPFILE")
                         (ak/org-section-export--normalize-setupfile-path
                          value source-dir)
                       value))
             lines)))))

    ;; Non-standard but convenient: support :SETUPFILE: in a top file-level
    ;; property drawer.
    (let ((drawer-setupfile
           (ak/org-section-export--file-property-drawer-value "SETUPFILE")))
      (when drawer-setupfile
        (push
         (format "#+SETUPFILE: %s"
                 (ak/org-section-export--normalize-setupfile-path
                  drawer-setupfile source-dir))
         lines)))

    (nreverse lines)))

(defun ak/org-section-export--subtree-string (marker)
  "Return the subtree at MARKER as a plain string.

MARKER may point into another buffer; this function switches to that
buffer before calling `org-narrow-to-subtree'."
  (let ((source-buffer (marker-buffer marker)))
    (unless (buffer-live-p source-buffer)
      (error "Source buffer for Org marker is no longer live"))
    (with-current-buffer source-buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char marker)
          (unless (org-at-heading-p)
            (org-back-to-heading t))
          (org-narrow-to-subtree)
          (buffer-substring-no-properties
           (point-min)
           (point-max)))))))

(defun ak/org-section-export--subtree-string-with-title (heading raw-title)
  "Return HEADING subtree with its first headline replaced by RAW-TITLE.

RAW-TITLE is kept as Org text, so links such as:

  [[https://example.com][Title]]

remain links in the exported HTML."
  (let ((subtree
         (ak/org-section-export--subtree-string
          (plist-get heading :marker))))
    (with-temp-buffer
      (insert subtree)
      (goto-char (point-min))
      (when (looking-at "^\\(\\*+\\)\\(?:[ \t]+.*\\)?$")
        (replace-match
         (concat (match-string 1)
                 " "
                 (ak/org-section-export--single-line raw-title))
         t
         t))
      (buffer-string))))

(defun ak/org-section-export--temporary-org (heading metadata source-dir)
  "Build temporary Org document for HEADING using METADATA."
  (let* ((preamble-lines
          (ak/org-section-export--file-preamble-lines source-dir))
         (raw-title (plist-get metadata :raw-title))
         (display-title
          (ak/org-section-export--single-line
           (plist-get metadata :display-title)))
         (author
          (ak/org-section-export--single-line
           (or (plist-get metadata :author) "")))
         (subtree
          (ak/org-section-export--subtree-string-with-title
           heading raw-title)))
    (string-join
     (append
      preamble-lines
      (list
       ;; Per-section metadata.
       (format "#+TITLE: %s" display-title)
       (format "#+AUTHOR: %s" author)

       ;; Keep exported section files clean by default.
       "#+OPTIONS: toc:nil num:nil"

       ""
       subtree
       ""))
     "\n")))

(defun ak/org-section-export--export-html (org-text metadata output-file source-dir)
  "Export ORG-TEXT to HTML OUTPUT-FILE.

METADATA is currently used for future backend symmetry.
SOURCE-DIR is used as `default-directory' while exporting."
  (let* ((org-export-show-temporary-export-buffer nil)
         (export-options
          `(:with-toc nil
            :section-numbers nil
            :with-title nil
            :html-postamble nil
            :html-head-include-default-style
            ,ak/org-section-export-html-include-default-style
            :html-head-include-scripts
            ,ak/org-section-export-html-include-scripts
            :html-toplevel-hlevel
            ,ak/org-section-export-html-toplevel-hlevel))
         html)
    (let ((default-directory source-dir))
      (setq html
            (org-export-string-as
             org-text
             'html
             nil
             export-options)))
    (let ((coding-system-for-write 'utf-8))
      (with-temp-file output-file
        (insert html)))
    output-file))

(defun ak/org-section-export--select-headings (headings)
  "Prompt user to select one or more HEADINGS.

The UI includes the applicable title and author. Links are not stripped;
described Org links are displayed with their URL."
  (let* ((candidates
          (mapcar
           (lambda (heading)
             (let* ((metadata
                     (ak/org-section-export--metadata-for-heading
                      heading nil))
                    (title
                     (plist-get metadata :display-title))
                    (author
                     (or (ak/org-section-export--nonblank
                          (plist-get metadata :author))
                         "Unknown Author")))
               (cons
                (format "%02d - %s — %s"
                        (plist-get heading :index)
                        title
                        author)
                heading)))
           headings))
         (crm-separator "[ \t]*,[ \t]*")
         (choices
          (completing-read-multiple
           "Export headings, comma-separated: "
           candidates
           nil
           t)))
    (mapcar
     (lambda (choice)
       (cdr (assoc choice candidates)))
     choices)))

(defun ak/org-section-export--export-backend (backend)
  "Export selected top-level headings using BACKEND.

BACKEND is a plist:

  :name      display name
  :extension output file extension
  :exporter  function called with
             ORG-TEXT METADATA OUTPUT-FILE SOURCE-DIR"
  (unless (derived-mode-p 'org-mode)
    (user-error "This command must be run in an Org buffer"))

  (let* ((headings (ak/org-section-export--top-level-headings))
         (one-heading-p (= (length headings) 1))
         (source-dir (ak/org-section-export--current-directory))
         (output-dir (ak/org-section-export--output-directory))
         (backend-name (plist-get backend :name))
         (extension (plist-get backend :extension))
         (exporter (plist-get backend :exporter))
         selected
         exported
         (copied-assets 0)
         missing-assets)

    (unless headings
      (user-error "No top-level Org headings found"))

    (setq selected
          (if one-heading-p
              headings
            (ak/org-section-export--select-headings headings)))

    (unless selected
      (user-error "No headings selected"))

    (setq exported
          (mapcar
           (lambda (heading)
             (let* ((metadata
                     (ak/org-section-export--metadata-for-heading
                      heading one-heading-p))
                    (filename-base
                     (ak/org-section-export--safe-name
                      (plist-get metadata :filename-title)))
                    (output-file
                     (expand-file-name
                      (concat filename-base "." extension)
                      output-dir))
                    (org-text
                     (ak/org-section-export--temporary-org
                      heading metadata source-dir)))

               ;; Copy and rewrite local file/image links before export.
               (when ak/org-section-export-copy-linked-assets
                 (let ((asset-result
                        (ak/org-section-export--copy-local-linked-assets
                         org-text
                         source-dir
                         output-dir
                         filename-base)))
                   (setq org-text
                         (plist-get asset-result :text))
                   (cl-incf copied-assets
                            (or (plist-get asset-result :count) 0))
                   (setq missing-assets
                         (append missing-assets
                                 (plist-get asset-result :missing)))))

               (funcall exporter
                        org-text
                        metadata
                        output-file
                        source-dir)))
           selected))

    (message "Exported %s file%s to %s: %s%s"
             backend-name
             (if (= (length exported) 1) "" "s")
             output-dir
             (string-join
              (mapcar #'file-name-nondirectory exported)
              ", ")
             (if (> copied-assets 0)
                 (format " — copied %d linked asset%s"
                         copied-assets
                         (if (= copied-assets 1) "" "s"))
               ""))

    (when missing-assets
      (message "Some linked local assets were not copied: %s"
               (string-join
                (mapcar #'abbreviate-file-name
                        (delete-dups missing-assets))
                ", ")))))

;;;###autoload
(defun ak/org-export-top-level-headings-to-html ()
  "Export one or more top-level Org headings to standalone HTML files.

If the Org file has one top-level heading, file-level TITLE and AUTHOR
metadata are preferred.

If the Org file has multiple top-level headings, prompt for one or more
headings. Each selected top-level heading becomes a separate HTML file.

Links are preserved in titles and body text.

File-level SETUPFILE / HTML_HEAD / HTML_HEAD_EXTRA settings before the
first heading are copied into the temporary export buffer, so HTML CSS
defined through SETUPFILE is applied to each exported file."
  (interactive)
  (ak/org-section-export--export-backend
   (list :name "HTML"
         :extension "html"
         :exporter #'ak/org-section-export--export-html)))

(provide 'org-section-html-export)
