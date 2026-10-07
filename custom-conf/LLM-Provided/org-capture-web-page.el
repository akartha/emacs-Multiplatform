;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'org-roam)
(require 'org-web-tools)

(defvar ak/web-page-cache (make-hash-table :test #'equal)
  "Cache of HTML returned by Chrome, keyed by URL.")

(defun ak/fetch-dom-with-chrome (url)
  "Return rendered HTML for URL using headless Chrome."
  (or (gethash url ak/web-page-cache)
      (puthash
       url
       (with-temp-buffer
         (let ((status
                (call-process
                 "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
                 nil
                 t
                 nil
                 "--headless=new"
                 "--disable-gpu"
                 "--dump-dom"
                 url)))
           (unless (zerop status)
             (error "Chrome exited with %d" status))
           (buffer-string)))
       ak/web-page-cache)))

(defun ak/org-roam-capture-web-page (&optional url)
  "Capture URL into Org-roam using a single Chrome DOM fetch.

If a node whose title matches the page title already exists,
visit it instead of creating a new one."
  (interactive)
  (setq url (or url (org-web-tools--get-first-url)))

  ;; Fetch exactly once.
  (let* ((html (ak/fetch-dom-with-chrome url))
         (title (org-web-tools--html-title html)))

    ;; Existing note?
    (if-let* ((node
              (seq-find
               (lambda (n)
                 (string= title (org-roam-node-title n)))
               (org-roam-node-list))))

        ;; Visit existing note.
        (org-roam-node-visit node)

      ;; Otherwise create a new note.
      (org-roam-capture- :node (org-roam-node-create :title title))

      ;; Pretend that org-web-tools downloaded the page.
      (cl-letf (((symbol-function #'org-web-tools--get-url)
                 (lambda (_url)
                   html)))
        (org-web-tools-insert-web-page-as-entry url))

      (save-buffer))))
