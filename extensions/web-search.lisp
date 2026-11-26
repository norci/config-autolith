(in-package #:autolith)

;;;; -- Web search --
;;;; Query Keenable's public search API, the keyless backend behind Lightpanda's
;;;; search tool. One POST returns clean title/URL/snippet rows, avoiding the
;;;; bot challenges that block HTML scraping of Bing and DuckDuckGo.

(defparameter *web-search-api-url* "https://api.keenable.ai/v1/search/public"
  "The Keenable public search endpoint.")

(defparameter *web-search-app-title* "autolith-web-search"
  "Attribution title sent to the keyless Keenable endpoint.")

(defun web-search--flatten (text)
  "Return TEXT as one trimmed line."
  (string-trim '(#\Space #\Tab #\Newline #\Return)
               (cl-ppcre:regex-replace-all "\\s+" (or text "") " ")))

(defun web-search--row (result)
  "Return a result plist from one Keenable RESULT object, or NIL."
  (let ((title (argo:json-get result "title"))
        (url (argo:json-get result "url")))
    (when (and title url)
      (list :title title
            :url url
            :snippet (web-search--flatten (argo:json-get result "snippet" ""))))))

(defun web-search--fetch-rows (query pages)
  "Return result rows for QUERY from one request of up to PAGES batches of ten."
  (unless (and (integerp pages) (plusp pages))
    (error "PAGES must be a positive integer, got ~S." pages))
  (let* ((body (argo:json-encode
                (argo:json-object
                 "query" query
                 "max_results" (min 50 (* 10 pages))
                 "snippet_max_length" 500)))
           (text (multiple-value-bind (response status)
                     (dexador:post *web-search-api-url*
                                   :headers (list (cons "Content-Type" "application/json")
                                                  (cons "X-Keenable-Title" *web-search-app-title*))
                                   :content body
                                   :read-timeout 25)
                 (unless (<= 200 status 299)
                   (error "Search API returned HTTP ~A." status))
                 response))
         (payload (argo:json-decode text))
         (results (or (argo:json-get payload "results") #())))
    (remove nil (map 'list #'web-search--row results))))

(defclass web-search-tool (tool) ()
  (:documentation "A tool that searches through Keenable's public search API."))

(defmethod tool-execute ((tool web-search-tool) (context tool-context) (arguments hash-table))
  "Search for QUERY and return parsed title, URL and snippet rows."
  (declare (ignore tool context))
  (let ((query (tool-argument arguments "query" :required t))
        (pages (or (tool-argument arguments "pages") 1)))
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
                  :description "Search the web through Keenable. Returns title, URL and snippet for each result. Fetches one batch of results by default."
                  :parameters (tool-object-schema
                               (json-object "query" (tool-string-property "Search query.")
                                            "pages" (tool-integer-property
                                                     "Number of result batches to fetch. Defaults to 1."))
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
