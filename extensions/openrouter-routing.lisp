(in-package #:autolith)

;;;; -- OpenRouter Endpoint Routing Plugin --

;;; Rank a model's serving endpoints by usage-weighted cost, drop unhealthy,
;;; banned, and low-precision ones, then pin the surviving order in the
;;; request's provider object. Ported from the pi openrouter extension.
;;;
;;; Loading this file installs the advice on PROVIDER-PREPARE-STREAM-REQUEST;
;;; there is no separate activation step because the method specializes on
;;; OPENROUTER-CHAT-COMPLETIONS-PROVIDER. Endpoint discovery reaches
;;; openrouter.ai through the egress proxy and must not bypass it.

(ql:quickload :cmu-infix :silent t)
(named-readtables:in-readtable cmu-infix:syntax)


;;;; -- Policy --

(defparameter *openrouter-routing-api-base* "https://openrouter.ai/api/v1"
  "OpenRouter API base used for endpoint discovery.")

(defparameter *openrouter-routing-timeout-seconds* 3
  "Deadline for one endpoint discovery request.")

(defparameter *openrouter-routing-cache-seconds* 300
  "Age after which a model's endpoint table is refreshed.")

(defparameter *openrouter-routing-minimum-uptime* 100
  "Smallest acceptable uptime percentage over the five and thirty minute windows.")

(defparameter *openrouter-routing-input-tokens-per-output-token* 60d0
  "Fallback prompt-to-completion token ratio for cost blending.")

(defparameter *openrouter-routing-cache-hit-rate* 0.9d0
  "Fallback prompt cache hit rate for cost blending.")

(defparameter *openrouter-routing-banned-vendors-path*
  (merge-pathnames "autolith/extensions/openrouter-banned-vendors.json" (uiop:xdg-config-home))
  "JSON array of vendor names, quantization and tag markers that routing never selects.")

(defvar *openrouter-routing-cache* (make-hash-table :test #'equal)
  "Endpoint tables per model, as (expiry . endpoints).")

(defvar *openrouter-routing-last-error* nil
  "The most recent endpoint discovery failure, for inspection.")


;;;; -- Endpoint table --

(defun openrouter-routing--number (value)
  "Return VALUE as a double float, or NIL when it does not carry a number."
  (cond
    ((numberp value)
     (coerce value 'double-float))
    ((stringp value)
     (ignore-errors
       (let ((*read-eval* nil))
         (coerce (read-from-string value) 'double-float))))
    (t
     nil)))

(defun openrouter-routing--vendor-key (name)
  "Return the comparison key for vendor NAME, ignoring case and punctuation."
  (string-downcase (remove-if-not #'alphanumericp name)))

(defun openrouter-routing--vendor (tag)
  "Return TAG's vendor segment."
  (subseq tag 0 (or (position #\/ tag) (length tag))))

(defun openrouter-routing--blocked-keys ()
  "Return the set of blocked comparison keys, empty when the list is unreadable."
  (let ((keys (make-hash-table :test #'equal)))
    (handler-case
        (when (probe-file *openrouter-routing-banned-vendors-path*)
          (let ((decoded (argo:json-decode
                          (uiop:read-file-string *openrouter-routing-banned-vendors-path*))))
            (when (vectorp decoded)
              (loop for name across decoded
                    when (stringp name)
                      do (setf (gethash (openrouter-routing--vendor-key name) keys) t)))))
      (error () nil))
    keys))

(defun openrouter-routing--fetch-endpoints (model api-key)
  "Return MODEL's endpoint vector from OpenRouter."
  (let* ((headers (if api-key
                      (list (cons "Authorization" (concatenate 'string "Bearer " api-key)))
                      nil))
         (body (dexador:get (format nil "~A/models/~A/endpoints"
                                    *openrouter-routing-api-base* model)
                            :headers                headers
                            :force-string           t
                            :keep-alive             nil
                            :connect-timeout        *openrouter-routing-timeout-seconds*
                            :read-timeout           *openrouter-routing-timeout-seconds*))
         (endpoints (argo:json-get (argo:json-get (argo:json-decode body) "data") "endpoints")))
    (if (vectorp endpoints) endpoints (make-array 0))))

(defun openrouter-routing--endpoints (model api-key)
  "Return MODEL's endpoint vector, refreshing it when the cached table expired.

  A failed refresh keeps the previous table, so one timeout never drops routing."
  (let* ((now   (get-universal-time))
         (entry (gethash model *openrouter-routing-cache*)))
    (if (and entry (< now (car entry)))
        (cdr entry)
        (handler-case
            (let ((endpoints (openrouter-routing--fetch-endpoints model api-key)))
              (setf (gethash model *openrouter-routing-cache*)
                    (cons (+ now *openrouter-routing-cache-seconds*) endpoints))
              endpoints)
          (error (condition)
            (setf *openrouter-routing-last-error* condition)
            (cond
              (entry
               (setf (car entry) (+ now *openrouter-routing-cache-seconds*))
               (cdr entry))
              (t
               (remhash model *openrouter-routing-cache*)
               nil)))))))


;;;; -- Endpoint selection --

(defun openrouter-routing--blocked-p (endpoint blocked-keys)
  "Return true when ENDPOINT's vendor, quantization, or a tag segment is blocked.

  BLOCKED-KEYS holds the banned list's comparison keys: vendor names and tag
  markers alike."
  (labels ((blocked (name)
             (and (stringp name)
                  (gethash (openrouter-routing--vendor-key name) blocked-keys))))
    (let ((tag (argo:json-get endpoint "tag")))
      (and (stringp tag)
           (or (blocked (openrouter-routing--vendor tag))
               (blocked (argo:json-get endpoint "quantization"))
               (loop for segment in (uiop:split-string tag :separator "/")
                     thereis (blocked segment)))))))

(defun openrouter-routing--healthy-p (endpoint)
  "Return true when ENDPOINT's recent uptime is acceptable or unreported."
  (loop for key in '("uptime_last_5m" "uptime_last_30m")
        always (let ((value (openrouter-routing--number (argo:json-get endpoint key))))
                 (or (null value) (>= value *openrouter-routing-minimum-uptime*)))))

(defun openrouter-routing--blended-cost (endpoint)
  "Return ENDPOINT's usage-weighted cost per million output tokens."
  (let* ((pricing    (argo:json-get endpoint "pricing"))
         (prompt     (openrouter-routing--number (and pricing (argo:json-get pricing "prompt"))))
         (cache      (openrouter-routing--number
                      (and pricing (argo:json-get pricing "input_cache_read"))))
         (completion (openrouter-routing--number
                      (and pricing (argo:json-get pricing "completion")))))
    (when (and prompt cache completion)
      (let ((h *openrouter-routing-cache-hit-rate*)
            (k *openrouter-routing-input-tokens-per-output-token*))
        #I(1d6 * (completion + k * (h * cache + (1d0 - h) * prompt)))))))

(defun openrouter-routing--cheaper (left right)
  "Order two ranked endpoints by cost, then by tag."
  (or (< (getf left :cost) (getf right :cost))
      (and (= (getf left :cost) (getf right :cost))
           (string< (getf left :tag) (getf right :tag)))))

(defun openrouter-routing--rank (endpoints)
  "Return useable ENDPOINTS grouped by vendor, cheapest first in each group.

  Vendors are ordered by their cheapest member so a fallback stays inside one
  vendor and keeps its prompt cache."
  (let ((groups (make-hash-table :test #'equal))
        (blocked-keys (openrouter-routing--blocked-keys)))
    (loop for endpoint across (or endpoints (make-array 0))
          for tag  = (argo:json-get endpoint "tag")
          for cost = (and (stringp tag) (openrouter-routing--blended-cost endpoint))
          when (and cost
                    (openrouter-routing--healthy-p endpoint)
                    (not (openrouter-routing--blocked-p endpoint blocked-keys)))
            do (push (list :tag                    tag
                           :cost                   cost
                           :max-completion-tokens (openrouter-routing--number
                                                   (argo:json-get endpoint
                                                                  "max_completion_tokens")))
                     (gethash (openrouter-routing--vendor tag) groups)))
    (loop for group in (sort (loop for entries being the hash-values of groups
                                   collect (sort entries #'openrouter-routing--cheaper))
                             (lambda (left right)
                               (openrouter-routing--cheaper (first left) (first right))))
          append group)))


;;;; -- Request preparation --

(defun openrouter-routing--preset-order-p (request)
  "Return true when REQUEST already carries a non-empty provider order."
  (let* ((provider (argo:json-get request "provider"))
         (order    (and (hash-table-p provider) (argo:json-get provider "order"))))
    (and (vectorp order) (plusp (length order)))))

(defun openrouter-routing--apply (request credentials)
  "Return REQUEST with an ordered provider object when routing applies."
  (let ((model (argo:json-get request "model")))
    (if (or (not (stringp model)) (openrouter-routing--preset-order-p request))
        request
        (let* ((api-key (or (and credentials
                                 (ignore-errors
                                   (cl-rfc8628:oauth-credentials-access-token credentials)))
                            (uiop:getenv "OPENROUTER_API_KEY")))
               (ranked  (openrouter-routing--rank
                         (openrouter-routing--endpoints model api-key))))
          (if (null ranked)
              request
              (let* ((copy     (argo:json-object-copy request))
                     (prepared (argo:json-get copy "provider"))
                     (provider (if (hash-table-p prepared)
                                   (argo:json-object-copy prepared)
                                   (argo:json-object)))
                     (order    (make-array (length ranked))))
                (loop for entry in ranked
                      for index from 0
                      do (setf (aref order index) (getf entry :tag)))
                (setf (gethash "order" provider) order)
                (setf (gethash "provider" copy) provider)
                (let ((ceiling   (getf (first ranked) :max-completion-tokens))
                      (requested (openrouter-routing--number
                                  (argo:json-get copy "max_tokens"))))
                  (when (and ceiling requested (> requested ceiling))
                    (setf (gethash "max_tokens" copy) (round ceiling))))
                copy))))))


;;;; -- Installation --

(defmethod provider-prepare-stream-request :around
    ((provider openrouter-chat-completions-provider) (request hash-table) credentials)
  "Pin OpenRouter's endpoint order before admission and transport."
  (let ((prepared (call-next-method)))
    (or (ignore-errors (openrouter-routing--apply prepared credentials))
        prepared)))
