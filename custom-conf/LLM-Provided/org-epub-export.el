;;; org-epub-export.el --- Export Org top-level headings to EPUB -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'org)
(require 'ox-md)
(require 'seq)
(require 'subr-x)
(require 'crm)

(defgroup ak-org-epub nil
  "Export Org sections to EPUB using Pandoc."
  :group 'org)

(defcustom ak/org-epub-output-directory nil
  "Target directory for Org EPUB exports.

If nil, empty, or pointing to a directory that does not exist, export
to the current Org file's directory instead."
  :type '(choice
          (const :tag "Use current Org file directory" nil)
          directory)
  :group 'ak-org-epub)

(defun ak/org-epub--nonblank (s)
  "Return trimmed S, or nil if S is empty."
  (when (stringp s)
    (let ((trimmed (string-trim s)))
      (unless (string-empty-p trimmed)
        trimmed))))

(defun ak/org-epub--current-directory ()
  "Return the current Org file directory, or `default-directory'."
  (file-name-as-directory
   (expand-file-name
    (or (and buffer-file-name
             (file-name-directory buffer-file-name))
        default-directory))))

(defun ak/org-epub--output-directory ()
  "Return EPUB output directory.

Use `ak/org-epub-output-directory' when it exists. Otherwise fall back
to the current Org file's directory."
  (let ((dir (ak/org-epub--nonblank ak/org-epub-output-directory)))
    (if (and dir
             (file-directory-p (expand-file-name dir)))
        (file-name-as-directory
         (expand-file-name dir))
      (ak/org-epub--current-directory))))

(defun ak/org-epub--safe-name (s)
  "Convert S into a filesystem-safe filename base."
  (let ((safe (replace-regexp-in-string
               "[^[:alnum:]_-]+"
               "-"
               (downcase (string-trim (or s "untitled"))))))
    (string-trim safe "-+" "-+")))

(defun ak/org-epub--file-keyword (key)
  "Return Org file keyword KEY, such as TITLE or AUTHOR."
  (ak/org-epub--nonblank
   (car (cdr (assoc (upcase key)
                    (org-collect-keywords (list (upcase key))))))))

(defun ak/org-epub--first-heading-position ()
  "Return position of the first Org heading, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward org-heading-regexp nil t)
      (match-beginning 0))))

(defun ak/org-epub--file-property-drawer-value (key)
  "Return file-level property drawer value for KEY, or nil.

This checks for a property drawer before the first heading."
  (let ((case-fold-search t)
        (limit (or (ak/org-epub--first-heading-position)
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
                    (ak/org-epub--nonblank
                     (match-string-no-properties 1))))))))
    value))

(defun ak/org-epub--file-property-keyword-value (key)
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
                (ak/org-epub--nonblank
                 (match-string 1 prop)))
          (throw 'found value))))
    value))

(defun ak/org-epub--file-metadata (key)
  "Return file-level metadata KEY.

Checks, in order:

1. Standard Org keyword, e.g. #+TITLE or #+AUTHOR
2. File-level property drawer before the first heading
3. #+PROPERTY metadata"
  (or (ak/org-epub--file-keyword key)
      (ak/org-epub--file-property-drawer-value key)
      (ak/org-epub--file-property-keyword-value key)))

(defun ak/org-epub--heading-property (key)
  "Return current heading property KEY, or nil."
  (ak/org-epub--nonblank
   (org-entry-get nil (upcase key))))

(defun ak/org-epub--top-level-headings ()
  "Return top-level headings in current Org buffer.

Each item is a plist containing:

  :index       numeric heading index
  :raw-title   original Org heading text
  :title       cleaned title used for UI/export fallback
  :marker      heading marker"
  (let ((index 0)
        items)
    (org-map-entries
     (lambda ()
       (when (= (org-outline-level) 1)
         (cl-incf index)
         (let* ((raw-title (org-get-heading t t t t))
                (clean-title
                 (or (ak/org-epub--nonblank
                      (ak/org-epub--clean-title-text raw-title))
                     "Untitled")))
           (push
            (list :index index
                  :raw-title raw-title
                  :title clean-title
                  :marker (point-marker))
            items))))
     nil 'file)
    (nreverse items)))

(defun ak/org-epub--clean-title-text (s)
  "Clean title text S for EPUB metadata, filenames, and export UI.

URL Org links are replaced with their description when available.
Undescribed URL links and bare URLs are removed.

Examples:

  [[https://example.com][My Story]] -> My Story
  [[https://example.com]]           -> empty
  My Story https://example.com      -> My Story"
  (let ((text (or s "")))
    (with-temp-buffer
      (insert text)
      (org-mode)

      ;; Replace Org URL links in the title.
      (let (links)
        (setq links
              (org-element-map (org-element-parse-buffer) 'link
                (lambda (link)
                  (when (member (org-element-property :type link)
                                '("http" "https"))
                    (list
                     :begin (org-element-property :begin link)
                     :end (org-element-property :end link)
                     :contents-begin (org-element-property :contents-begin link)
                     :contents-end (org-element-property :contents-end link))))))

        ;; Work backwards to avoid invalidating positions.
        (dolist (link (sort links
                            (lambda (a b)
                              (> (plist-get a :begin)
                                 (plist-get b :begin)))))
          (let* ((beg (plist-get link :begin))
                 (end (plist-get link :end))
                 (contents-beg (plist-get link :contents-begin))
                 (contents-end (plist-get link :contents-end))
                 (replacement
                  (if contents-beg
                      (buffer-substring-no-properties contents-beg contents-end)
                    "")))
            (goto-char beg)
            (delete-region beg end)
            (insert replacement))))

      (setq text (buffer-string)))

    ;; Remove bare URLs too.
    (setq text
          (replace-regexp-in-string
           "\\(?:https?://\\|www\\.\\)[^[:space:]]+"
           ""
           text))

    ;; Remove simple Org emphasis delimiters in titles.
    (setq text
          (replace-regexp-in-string
           "\\([*/_=~+]\\)\\([^[:space:]].*?[^[:space:]]\\)\\1"
           "\\2"
           text))

    ;; Normalize whitespace.
    (string-trim
     (replace-regexp-in-string
      "[[:space:]\n]+" " " text))))

(defun ak/org-epub--strip-url-links ()
  "Remove URL Org links while preserving descriptive text.

For example:

  [[https://example.com][Example]]

becomes:

  Example

Undescribed URL links are removed."
  (let (links)
    (setq links
          (org-element-map (org-element-parse-buffer) 'link
            (lambda (link)
              (when (member (org-element-property :type link)
                            '("http" "https"))
                (list
                 :begin (org-element-property :begin link)
                 :end (org-element-property :end link)
                 :contents-begin (org-element-property :contents-begin link)
                 :contents-end (org-element-property :contents-end link))))))

    ;; Work backwards so replacements do not invalidate earlier positions.
    (dolist (link (sort links
                        (lambda (a b)
                          (> (plist-get a :begin)
                             (plist-get b :begin)))))
      (let* ((beg (plist-get link :begin))
             (end (plist-get link :end))
             (contents-beg (plist-get link :contents-begin))
             (contents-end (plist-get link :contents-end))
             (replacement
              (if contents-beg
                  (buffer-substring-no-properties contents-beg contents-end)
                "")))
        (goto-char beg)
        (delete-region beg end)
        (insert replacement))))

  ;; Also remove bare URLs from exported content.
  (goto-char (point-min))
  (while (re-search-forward "\\(?:https?://\\|www\\.\\)[^[:space:]]+" nil t)
    (replace-match "")))

(defun ak/org-epub--subtree-string (marker)
  "Return the subtree at MARKER as a plain string."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char marker)
      (org-narrow-to-subtree)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun ak/org-epub--org-subtree-to-markdown (title org-text)
  "Convert ORG-TEXT to Markdown using TITLE as the EPUB heading."
  (with-temp-buffer
    (insert org-text)
    (org-mode)
    (goto-char (point-min))

    ;; Remove the exported top-level heading itself. Its value is used as
    ;; EPUB metadata and as the document title.
    (when (looking-at "^\\*+ .*$")
      (delete-region (line-beginning-position)
                     (min (point-max)
                          (1+ (line-end-position)))))

    ;; Remove URL targets while preserving link descriptions.
    (ak/org-epub--strip-url-links)

    ;; Let Org export remove property drawers, planning lines, comments,
    ;; and other Org-specific syntax while preserving useful formatting.
    (let ((markdown
           (org-export-string-as
            (buffer-string)
            'md
            t
            '(:with-toc nil
              :section-numbers nil
              :with-author nil
              :with-title nil
              :with-date nil))))
      (string-trim
       (concat "# " title "\n\n" markdown "\n")))))

(defun ak/org-epub--pandoc-export (markdown title author output-file)
  "Use Pandoc to convert MARKDOWN to OUTPUT-FILE EPUB.

TITLE and AUTHOR are passed to Pandoc as EPUB metadata."
  (let ((tmp-file (make-temp-file
                   (concat "org-epub-"
                           (ak/org-epub--safe-name title)
                           "-")
                   nil ".md"))
        (log-buffer (get-buffer-create "*Org EPUB Pandoc*"))
        exit-code)
    (unwind-protect
        (progn
          (with-temp-file tmp-file
            (insert markdown))

          (with-current-buffer log-buffer
            (erase-buffer))

          (setq exit-code
                (call-process
                 "pandoc"
                 nil
                 (list log-buffer t)
                 nil
                 tmp-file
                 "-f" "markdown"
                 "-t" "epub"
                 "-o" output-file

                 ;; Prevent Pandoc from adding a visible/generated TOC.
                 "--toc=false"
                 "--table-of-contents=false"

                 ;; Prevent Pandoc from adding a generated title page.
                 "--epub-title-page=false"

                 "--metadata" (concat "title=" title)
                 "--metadata" (concat "author=" author)))

          (unless (= exit-code 0)
            (display-buffer log-buffer)
            (error "Pandoc failed with exit code %s. See *Org EPUB Pandoc*"
                   exit-code))

          output-file)
      (when (file-exists-p tmp-file)
        (delete-file tmp-file)))))

(defun ak/org-epub--metadata-for-heading (heading one-heading-p)
  "Return metadata plist for HEADING.

When ONE-HEADING-P is non-nil, prefer file-level metadata.

When exporting multiple top-level headings, use the top-level heading
text as the EPUB title unless that heading has a TITLE property. Do not
use the file-level TITLE for individual heading exports."
  (let* ((marker (plist-get heading :marker))
         (heading-title (plist-get heading :title))
         title author)
    (save-excursion
      (goto-char marker)

      (setq title
            (if one-heading-p
                ;; Single-heading file:
                ;; Prefer file-level TITLE, then heading TITLE property,
                ;; then the heading text.
                (or (ak/org-epub--file-metadata "TITLE")
                    (ak/org-epub--heading-property "TITLE")
                    heading-title)

              ;; Multi-heading file:
              ;; Prefer this heading's TITLE property, then the heading text.
              ;; Intentionally do NOT use file-level TITLE here.
              (or (ak/org-epub--heading-property "TITLE")
                  heading-title)))

      (setq author
            (if one-heading-p
                ;; Single-heading file:
                ;; Prefer file-level AUTHOR, then heading AUTHOR property.
                (or (ak/org-epub--file-metadata "AUTHOR")
                    (ak/org-epub--heading-property "AUTHOR")
                    user-full-name)

              ;; Multi-heading file:
              ;; Prefer this heading's AUTHOR property, then file-level AUTHOR.
              (or (ak/org-epub--heading-property "AUTHOR")
                  (ak/org-epub--file-metadata "AUTHOR")
                  user-full-name))))

    (list :title title
          :author author)))

(defun ak/org-epub--export-heading (heading one-heading-p output-dir)
  "Export HEADING to an EPUB file in OUTPUT-DIR."
  (let* ((metadata (ak/org-epub--metadata-for-heading heading one-heading-p))
         (title (plist-get metadata :title))
         (author (plist-get metadata :author))
         (org-text (ak/org-epub--subtree-string
                    (plist-get heading :marker)))
         (markdown (ak/org-epub--org-subtree-to-markdown title org-text))
         (output-file
          (expand-file-name
           (concat (ak/org-epub--safe-name title) ".epub")
           output-dir)))

    (ak/org-epub--pandoc-export markdown title author output-file)))

(defun ak/org-epub--select-headings (headings)
  "Prompt user to select one or more HEADINGS.

Display cleaned heading titles and append the applicable author value.

For multi-heading exports, the displayed title follows the same logic
used for export: heading TITLE property if present, otherwise cleaned
top-level heading text."
  (let* ((candidates
          (mapcar
           (lambda (heading)
             (let* ((metadata (ak/org-epub--metadata-for-heading heading nil))
                    (title (plist-get metadata :title))
                    (author (or (ak/org-epub--nonblank
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

;;;###autoload
(defun ak/org-export-top-level-headings-to-epub ()
  "Export top-level Org headings to EPUB files using Pandoc.

Behavior:

- If the Org file has one top-level heading, export it directly.
  File-level TITLE and AUTHOR metadata are preferred.

- If the Org file has multiple top-level headings, prompt for one or
  more headings to export. Each selected top-level heading becomes a
  separate EPUB file. Top-level heading TITLE and AUTHOR properties are
  preferred.

- URL links are stripped while preserving their descriptive text.

- Org-specific formatting is cleaned through Org's Markdown exporter,
  while useful output formatting such as emphasis, lists, block quotes,
  and headings is preserved.

- Output files go to `ak/org-epub-output-directory' when it exists.
  Otherwise they go to the current Org file's directory."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command must be run in an Org buffer"))

  (unless (executable-find "pandoc")
    (user-error "Pandoc not found in PATH"))

  (let* ((headings (ak/org-epub--top-level-headings))
         (one-heading-p (= (length headings) 1))
         (output-dir (ak/org-epub--output-directory))
         selected
         exported)

    (unless headings
      (user-error "No top-level Org headings found"))

    (setq selected
          (if one-heading-p
              headings
            (ak/org-epub--select-headings headings)))

    (unless selected
      (user-error "No headings selected"))

    (setq exported
          (mapcar
           (lambda (heading)
             (ak/org-epub--export-heading
              heading
              one-heading-p
              output-dir))
           selected))

    (message "Exported EPUB%s to %s: %s"
             (if (= (length exported) 1) "" "s")
             output-dir
             (string-join
              (mapcar #'file-name-nondirectory exported)
              ", "))))

(provide 'org-epub-export)
