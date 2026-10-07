;; -*- lexical-binding: t; -*-

;;; org-roam-populate-created.el --- Populate CREATED from Org-roam filenames

(require 'org)
(require 'org-roam)

(defconst ak/org-roam-filename-timestamp-regexp
  "\\`\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)-"
  "Regexp matching the timestamp at the beginning of an Org-roam filename.

Expected filename format:

  YYYYMMDDHHMMSS-description.org")

(defun ak/org-roam-filename-created-time (filename)
  "Return creation time encoded in FILENAME.

FILENAME must begin with:

  YYYYMMDDHHMMSS-

Returns a time value, or nil if no timestamp is found."
  (let ((basename (file-name-nondirectory filename)))
    (when (string-match
           ak/org-roam-filename-timestamp-regexp
           basename)
      (encode-time
       0
       (string-to-number (match-string 6 basename))
       (string-to-number (match-string 5 basename))
       (string-to-number (match-string 4 basename))
       (string-to-number (match-string 3 basename))
       (string-to-number (match-string 2 basename))))))

(defun ak/org-roam-filename-created (filename)
  "Return CREATED timestamp extracted from FILENAME.

Expected filename format:

  YYYYMMDDHHMMSS-description.org

Returns a string in the form:

  YYYY-MM-DD HH:MM:SS

or nil if FILENAME does not contain a valid timestamp."
  (let ((basename (file-name-nondirectory filename)))
    (when (string-match
           "\\`\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)-"
           basename)
      (format "%s-%s-%s %s:%s:%s"
              (match-string 1 basename)
              (match-string 2 basename)
              (match-string 3 basename)
              (match-string 4 basename)
              (match-string 5 basename)
              (match-string 6 basename)))))

(defun ak/org-roam-populate-created-file (filename)
  "Populate CREATED on all Org-roam nodes in FILENAME.

Returns the number of nodes updated.

The timestamp is taken from the filename."
  (let ((created (ak/org-roam-filename-created filename))
        (count 0))

    ;; Ignore files whose names don't contain a timestamp.
    (when created
      (with-temp-buffer
        (insert-file-contents filename)
        (org-mode)

        (org-with-wide-buffer

         ;; ------------------------------------------------------------
         ;; File-level Org-roam node
         ;; ------------------------------------------------------------
         ;;
         ;; A file-level node is represented by an ID property before
         ;; the first headline.
         ;;
         (goto-char (point-min))
         (unless (org-at-heading-p)
           (when (org-entry-get nil "ID")
             (org-entry-put nil "CREATED" created)
             (setq count (1+ count))))

         ;; ------------------------------------------------------------
         ;; Headline-based Org-roam nodes
         ;; ------------------------------------------------------------
         ;;
         ;; Only headlines having an ID property are treated as
         ;; Org-roam nodes.
         ;;
         (goto-char (point-min))
         (while (re-search-forward
                 org-heading-regexp
                 nil
                 t)

           (when (org-entry-get nil "ID")
             (org-entry-put nil "CREATED" created)
             (setq count (1+ count))))))

      ;; Only write the file if we actually found nodes.
      (when (> count 0)
        (with-temp-buffer
          (insert-file-contents filename)
          (org-mode)

          (org-with-wide-buffer

           ;; File-level node
           (goto-char (point-min))
           (unless (org-at-heading-p)
             (when (org-entry-get nil "ID")
               (org-entry-put nil "CREATED" created)))

           ;; Headline nodes
           (goto-char (point-min))
           (while (re-search-forward
                   org-heading-regexp
                   nil
                   t)
             (when (org-entry-get nil "ID")
               (org-entry-put nil "CREATED" created))))

          (write-region
           (point-min)
           (point-max)
           filename
           nil
           'silent))))

    count))

(defun ak/org-roam-populate-created-recursively ()
  "Recursively populate CREATED properties throughout `org-roam-directory`.

Every .org file below `org-roam-directory` is inspected.

For files whose names begin with YYYYMMDDHHMMSS-, the timestamp is
written as the CREATED property on:

  1. the file-level Org-roam node, if present;
  2. every headline containing an ID property.

Existing CREATED properties are overwritten.

Files without a valid timestamp are skipped."
  (interactive)

  (unless (boundp 'org-roam-directory)
    (user-error "`org-roam-directory' is not defined"))

  (unless (file-directory-p org-roam-directory)
    (user-error
     "Org-roam directory does not exist: %s"
     org-roam-directory))

  (let ((files (directory-files-recursively
                org-roam-directory
                "\\.org\\'"))
        (files-processed 0)
        (files-skipped 0)
        (nodes-updated 0)
        (errors 0))

    (message "Scanning %d Org files..." (length files))

    (dolist (file files)

      (let ((created (ak/org-roam-filename-created file)))

        (if (not created)

            ;; Filename doesn't contain a timestamp.
            (setq files-skipped
                  (1+ files-skipped))

          (condition-case err

              (let ((count
                     (ak/org-roam-populate-created-file file)))

                (setq files-processed
                      (1+ files-processed))

                (setq nodes-updated
                      (+ nodes-updated count)))

            (error
             (setq errors (1+ errors))

             (message
              "ERROR processing %s: %s"
              file
              (error-message-string err)))))))

    (message
     (concat
      "Org-roam CREATED update complete: "
      "%d files processed, "
      "%d files skipped, "
      "%d nodes updated, "
      "%d errors")

     files-processed
     files-skipped
     nodes-updated
     errors)))

(provide 'org-roam-populate-created)

;;; org-roam-populate-created.el ends here

