(in-package #:autolith)

;;;; -- Web search --
;;;; Fetch Bing result pages with the built-in web_extra.gist retriever and
;;;; parse them into title/URL/snippet rows.

(defun web-search--next-nonblank (lines)
  "Return the first nonblank line in LINES, or NIL."
  (find-if (lambda (line)
             (plusp (length (string-trim '(#\Space #\Tab) line))))
           lines))

(defun web-search--link (line)
  "Return values TITLE and URL from the first [TITLE](URL) in LINE."
  (let* ((open (position #\[ line))
         (middle (and open (search "](" line :start2 (1+ open))))
         (close (and middle (search ")" line :start2 (+ middle 2)))))
    (when (and open middle close)
      (values (subseq line (1+ open) middle)
              (subseq line (+ middle 2) close)))))

(defun web-search--trim-snippet (text)
  "Return TEXT without markdown decorations and the trailing read-more link."
  (let* ((markers '("[阅读更多](" "[Read more]("))
         (positions (remove nil (mapcar (lambda (m) (search m text)) markers)))
         (end (when positions (apply #'min positions)))
         (trimmed (if end (subseq text 0 end) text)))
    (string-trim '(#\Space #\Tab) (remove #\* trimmed))))

(defun web-search--real-url (url)
  "Return the destination inside a Bing ck/a redirect URL, or URL itself."
  (let ((start (search "u=a1" url)))
    (if start
        (let* ((begin (+ start 4))
               (stop (or (position #\& url :start begin) (length url)))
               (chars (substitute #\/ #\_ (substitute #\+ #\- (subseq url begin stop))))
               (remainder (mod (length chars) 4))
               (padded (concatenate 'string chars
                                    (if (zerop remainder) ""
                                        (make-string (- 4 remainder) :initial-element #\=)))))
          (handler-case (babel:octets-to-string (base64:base64-string-to-usb8-array padded))
            (error () url)))
        url)))

(defun web-search--result (line rest)
  "Return one result plist for header LINE followed by REST, or NIL."
  (multiple-value-bind (title url) (web-search--link line)
    (when (and title url)
      (list :title title
              :url (web-search--real-url url)
            :snippet (web-search--trim-snippet
                      (or (web-search--next-nonblank rest) ""))))))

(defun web-search--parse-bing (markdown)
  "Return a list of (:TITLE t :URL u :SNIPPET s) plists parsed from Bing result MARKDOWN."
  (let ((lines (uiop:split-string markdown :separator '(#\Newline))))
    (remove nil
            (loop for rest on lines
                  for line = (string-trim '(#\Space #\Tab) (first rest))
                  when (uiop:string-prefix-p "## [" line)
                    collect (web-search--result line (rest rest))))))

(defun web-search--page-url (query page)
  "Return the international Bing search URL for QUERY's zero-based PAGE."
  (format nil "https://www.bing.com/search?~A"
          (quri.encode:url-encode-params
           (list (cons "q" query)
                 (cons "ensearch" "1")
                 (cons "first" (1+ (* 10 page)))))))

(defun web-search--dedup-key (row)
  "Return a stable identity for ROW across repeated fetches."
  (let* ((url (getf row :url))
         (start (search "u=a1" url)))
    (if start
        (subseq url start (or (position #\& url :start start) (length url)))
        url)))

(defun web-search--fetch-rows (query pages)
  "Return deduplicated result rows from PAGES Bing result pages for QUERY."
  (unless (and (integerp pages) (plusp pages))
    (error "PAGES must be a positive integer, got ~S." pages))
  (delete-duplicates
   (loop for page from 0 below pages
         append (web-search--parse-bing
                 (web-gist--retrieve (web-search--page-url query page))))
   :key #'web-search--dedup-key
   :test #'string=))

(defclass web-search-tool (tool) ()
  (:documentation "A tool that searches Bing through the built-in page retriever."))

(defmethod tool-execute ((tool web-search-tool) (context tool-context) (arguments hash-table))
  "Search Bing for QUERY and return parsed title, URL and snippet rows from PAGES pages."
  (declare (ignore tool context))
  (let ((query (tool-argument arguments "query" :required t))
         (pages (or (tool-argument arguments "pages") 3)))
    (handler-case
        (let ((rows (web-search--fetch-rows query pages)))
          (tool-success
           (if rows
               (with-output-to-string (out)
                 (dolist (row rows)
                     (format out "## [~A](~A)~%~A~%~%"
                             (getf row :title) (getf row :url) (getf row :snippet))))
               (format nil "No results for ~S." query))))
      (error (condition)
        (tool-failure (princ-to-string condition))))))

(defun web-search-register-tools (registry)
  "Register the web search tool in REGISTRY and return REGISTRY."
  (tool-registry-register
   registry
   (make-instance 'web-search-tool
                  :namespace "web"
                  :name "search"
                    :description "Search the international edition of Bing. Returns title, URL and snippet for each result. Fetches 3 pages by default and removes duplicates."
                  :parameters (tool-object-schema
                               (json-object "query" (tool-string-property "Search query.")
                                            "pages" (tool-integer-property
                                                       "Number of result pages to fetch. Defaults to 3."))
                                '("query"))))
  registry)

(defvar *web-search-registry-wrapper* nil
  "The exact wrapper this file installed on MAKE-DEFAULT-TOOL-REGISTRY.")

(unless (eq (symbol-function 'make-default-tool-registry) *web-search-registry-wrapper*)
  (let ((original (symbol-function 'make-default-tool-registry)))
    (setf *web-search-registry-wrapper*
          (lambda (&rest arguments)
            (web-search-register-tools (apply original arguments)))
          (symbol-function 'make-default-tool-registry)
          *web-search-registry-wrapper*)))

(ignore-errors
  (when (boundp '*active-application*)
    (let ((application *active-application*))
      (when application
        (web-search-register-tools (application-tool-registry application))))))
