(in-package #:autolith)

;;;; -- billion-context Plugin --

;;; billion-context (https://github.com/ranxianglei/billion-context) compresses
;;; conversation history inside an external HTTP proxy. Its pi extension
;;; registers the proxy's ACP tools natively and stamps cooperative-plugin
;;; headers so the proxy stops injecting the same tools at the wire level.
;;;
;;; This file is the Autolith counterpart: it registers a bili tool namespace
;;; and routes model traffic through the proxy, which is what lets compress
;;; fold the conversation. A dead proxy costs one failed attempt, never a turn.
;;;
;;; Start the proxy with `bili start`, then load this file from init.lisp:
;;;
;;;   (load (merge-pathnames "autolith/extensions/billion-context.lisp"
;;;                          (uiop:xdg-config-home)))

(defparameter *bili-proxy-origin* "http://127.0.0.1:8787"
  "The billion-context proxy origin serving this conversation.")

(defparameter *bili-plugin-agent* "autolith"
  "The x-bili-plugin identity Autolith announces to the proxy.")

(defparameter *bili-tool-timeout-seconds* 600
  "Deadline for one proxied tool call; compress may summarize for minutes.")

(defparameter *bili-route-provider-traffic-p* t
  "Whether activation points the configured provider at the proxy.

Routing is what lets the proxy see the conversation at all; without it the
proxy has no session and compress cannot fold anything.")

(defparameter *bili-proxy-retry-seconds* 60
  "How long traffic stays direct after the proxy fails mid-turn.")

(defparameter *bili-endpoint-environments*
  '((:codex      . "AUTOLITH_PROVIDER_ENDPOINT")
    (:grok       . "AUTOLITH_GROK_PROVIDER_ENDPOINT")
    (:nous       . "AUTOLITH_NOUS_PROVIDER_ENDPOINT")
    (:fireworks  . "AUTOLITH_FIREWORKS_PROVIDER_ENDPOINT")
    (:opencode   . "AUTOLITH_OPENCODE_PROVIDER_ENDPOINT")
    (:openrouter . "AUTOLITH_OPENROUTER_PROVIDER_ENDPOINT")
    (:mistral    . "AUTOLITH_MISTRAL_PROVIDER_ENDPOINT"))
  "Provider families whose endpoint an environment variable can route.")

(defvar *bili-active-p* nil
  "Whether a reachable proxy made this plugin register tools and route traffic.")

(defvar *bili-routing-suspended-until* nil
  "Universal time before which traffic stays direct after a proxy failure.")

(defvar *bili-fallback-attempted-p* nil
  "Whether the current request already retried without the proxy.")


;;;; -- Proxy transport --

(-> bili--routed-prefix (string) string)
(defun bili--routed-prefix (origin)
  "Return the path prefix ORIGIN prepends to a routed upstream URL."
  (format nil "~A/bili/" (string-right-trim "/" origin)))

(-> bili--routed-url-p (t) boolean)
(defun bili--routed-url-p (url)
  "Return true when URL addresses the compression proxy's /bili/ route."
  (and (stringp url)
       (uiop:string-prefix-p (bili--routed-prefix *bili-proxy-origin*) url)))

(-> bili--request (string &key (:content (option string))) json-object)
(defun bili--request (path &key content)
  "Send one request to the proxy at PATH and return the decoded JSON response.

CONTENT, when supplied, makes this a POST carrying that JSON text. A transport
or HTTP failure signals an error naming PATH so a tool call can report it."
  (handler-case
      (multiple-value-bind (payload status)
          (if content
              (dexador:post (format nil "~A~A" *bili-proxy-origin* path)
                            :headers         '(("content-type" . "application/json"))
                            :content         content
                            :force-string    t
                            :connect-timeout 5
                            :read-timeout    *bili-tool-timeout-seconds*
                              :proxy           nil)
              (dexador:get (format nil "~A~A" *bili-proxy-origin* path)
                           :force-string    t
                           :connect-timeout 5
                           :read-timeout    30
                             :proxy           nil))
        (unless (<= 200 status 299)
          (error "The billion-context proxy answered HTTP ~D at ~A." status path))
        (json-decode payload))
    (error (condition)
      (error "The billion-context proxy request ~A failed: ~A" path condition))))

(-> bili-proxy-reachable-p (&key (:timeout number)) boolean)
(defun bili-proxy-reachable-p (&key (timeout 2))
  "Return true when a proxy answers its plugin manifest at the configured origin."
  (handler-case
      (multiple-value-bind (payload status)
          (dexador:get (format nil "~A/__bili/plugin/manifest" *bili-proxy-origin*)
                       :force-string    t
                       :connect-timeout timeout
                       :read-timeout    timeout
                         :proxy           nil)
        (declare (ignore payload))
        (= status 200))
    (error () nil)))


;;;; -- Proxy tools --

(defclass bili-tool (tool)
  ((proxy-name
    :initarg       :proxy-name
    :reader        bili-tool-proxy-name
    :type          non-empty-string
    :documentation "The tool name the billion-context proxy executes."))
  (:documentation "One ACP tool served by the billion-context proxy."))

(defmethod tool-execution-policy ((tool bili-tool))
  "Serialize proxy calls: each one mutates or reads one conversation's state."
  (declare (ignore tool))
  ':exclusive)

(defmethod tool-execute ((tool bili-tool) (context tool-context) (arguments hash-table))
  "Forward one ACP tool call to the billion-context proxy."
  (let ((name (bili-tool-proxy-name tool)))
    (handler-case
        (let* ((conversation (tool-context-conversation context))
               (response     (bili--request
                              "/__bili/plugin/tool"
                              :content
                              (json-encode
                               (json-object
                                "conversationId" (conversation-identifier conversation)
                                "tool"           name
                                "args"           arguments)))))
          (if (json-get response "ok")
              (tool-success (or (json-get response "result") ""))
              (tool-failure
               (format nil "billion-context ~A refused the call: ~A"
                       name (or (json-get response "error") "no reason given")))))
      (error (condition)
        (tool-failure (princ-to-string condition))))))

(-> bili--tool-specifications () list)
(defun bili--tool-specifications ()
  "Return (NAME PROXY-NAME DESCRIPTION PARAMETERS) for every proxy ACP tool."
  (flet ((open-schema (properties required)
           (cl-llm-provider-api:provider-open-object-schema
            properties :required required))
         (string-property (description)
           (json-object "type" "string" "description" description))
         (number-property (description)
           (json-object "type" "number" "description" description))
         (boolean-property (description)
           (json-object "type" "boolean" "description" description)))
    (list
     (list "compress" "compress"
           "Replace consumed conversation ranges with self-contained summaries you write, identified by their refs. content = ONE plain string holding ALL ranges; each block starts with its 'm00150-m00220 optional topic' header line followed by that block's summary. Batch multiple ranges into ONE call. Use when content is genuinely consumed."
           (open-schema
            (json-object
             "content"
             (string-property
              "ONE plain string holding ALL ranges to compress. Each block starts with its 'm00150-m00220 optional topic' header line followed by that block's summary markdown. A JSON-encoded array as that string is also accepted."))
            nil))
     (list "decompress" "decompress"
           "Restores previously compressed content. Use when you need exact details lost in compression. By default restores one tier up. Use full:true for all the way to original messages. Use toFile to write to file instead of inflating context."
           (open-schema
            (json-object
             "blockId" (string-property "Block ID to decompress (e.g. b5).")
             "toFile"  (string-property "Optional: write content to file instead of context.")
             "full"    (boolean-property "Restore all the way to original messages."))
            '("blockId")))
     (list "search-context" "search_context"
           "Search through compressed block summaries by keyword. Use BEFORE decompressing to find the right block."
           (open-schema
            (json-object
             "query" (string-property "Search query.")
             "limit" (number-property "Max results (default 5)."))
            '("query")))
     (list "status" "acp_status"
           "Show context usage and compressible ranges. No args = overview; pass scope/view/tool/sort/limit/archiveDetail for targeted listings."
           (open-schema
            (json-object
             "scope"         (string-property "uncompressed: history outside active blocks; compressed: block ledger; omit = default overview.")
             "view"          (string-property "messages for a per-message listing; ranges is the default.")
             "tool"          (string-property "With view messages: only list results of this tool name.")
             "sort"          (string-property "With view messages: order by size, time, tool, or age.")
             "limit"         (number-property "Max entries to list.")
             "archiveDetail" (boolean-property "List pre-compaction archived blocks per-id instead of grouped by reason."))
            nil)))))

(-> bili-register-tools (tool-registry) tool-registry)
(defun bili-register-tools (registry)
  "Register the proxy's ACP tools in REGISTRY and return it.

Registration is skipped while no proxy answered, so an unconfigured or
proxy-less start offers no tools that could only fail."
  (when *bili-active-p*
    (tool-registry-describe-namespace
     registry "bili"
     "billion-context context compression through the local proxy.")
    (dolist (specification (bili--tool-specifications))
      (destructuring-bind (name proxy-name description parameters) specification
        (tool-registry-register
         registry
         (make-instance 'bili-tool
                        :namespace   "bili"
                        :name        name
                        :description description
                        :parameters  parameters
                        :proxy-name  proxy-name)))))
  registry)


;;;; -- Provider routing --

(-> bili--loopback-endpoint-p (model-provider) boolean)
(defun bili--loopback-endpoint-p (provider)
  "Return true when PROVIDER's configured endpoint targets the compression proxy."
  (bili--routed-url-p (config :provider-endpoint (provider-configuration provider))))

(-> bili-route-provider-endpoint
    (&key (:configuration (option configuration)) (:origin string))
    (option string))
(defun bili-route-provider-endpoint (&key (configuration *configuration*)
                                          (origin *bili-proxy-origin*))
  "Point CONFIGURATION's provider endpoint at ORIGIN and return the variable set.

Returns NIL when no configuration is available or the configured provider
family has no endpoint override."
  (unless configuration
    (return-from bili-route-provider-endpoint nil))
  (let* ((model      (config :model configuration))
         (provider   (config :model-provider configuration))
         (family     (provider-model-family model :provider provider))
         (variable   (cdr (assoc family *bili-endpoint-environments*)))
         (upstream   (configuration--provider-endpoint-for model provider))
         (prefix     (bili--routed-prefix origin)))
    (unless variable
      (return-from bili-route-provider-endpoint nil))
    (platform-setenv variable
                     (if (uiop:string-prefix-p prefix upstream)
                         upstream
                         (format nil "~A~A" prefix upstream)))
    variable))

(-> bili--strip-routing (&key (:origin string)) list)
(defun bili--strip-routing (&key (origin *bili-proxy-origin*))
  "Restore every routed endpoint variable to its direct upstream and return them."
  (let ((prefix   (bili--routed-prefix origin))
        (restored nil))
    (dolist (entry *bili-endpoint-environments*)
      (let* ((variable (cdr entry))
             (value    (uiop:getenv variable)))
        (when (and value (uiop:string-prefix-p prefix value))
          (platform-setenv variable (subseq value (length prefix)))
          (push variable restored))))
    restored))

(-> bili--suspend-routing () list)
(defun bili--suspend-routing ()
  "Send traffic direct for the cooldown window and return the restored variables."
  (setf *bili-routing-suspended-until*
        (+ (get-universal-time) *bili-proxy-retry-seconds*))
  (bili--strip-routing))

(-> bili--restore-routing-when-due () boolean)
(defun bili--restore-routing-when-due ()
  "Re-route once the cooldown elapsed and the proxy answers again.

The provider request path calls this, so a configuration or transport problem
must not escape: a failed restore leaves traffic direct for the next turn."
  (when (and *bili-routing-suspended-until*
             (<= *bili-routing-suspended-until* (get-universal-time)))
    (setf *bili-routing-suspended-until* nil)
    (ignore-errors
      (when (and *bili-route-provider-traffic-p*
                 (bili-proxy-reachable-p :timeout 1)
                 (boundp '*configuration*)
                 *configuration*)
        (setf *bili-active-p* t)
        (bili-route-provider-endpoint)
        t))))

(-> bili-status (&optional conversation) list)
(defun bili-status (&optional conversation)
  "Return the proxy reachability, activation state, and live plugin status.

CONVERSATION defaults to the active application's conversation; the proxy
requires its identifier to answer a status request."
  (let* ((reachable    (bili-proxy-reachable-p))
         (conversation (or conversation
                           (and (boundp '*active-application*)
                                *active-application*
                                (application-conversation *active-application*)))))
    (list :origin          *bili-proxy-origin*
          :reachable       reachable
          :active-p        *bili-active-p*
          :suspended-until *bili-routing-suspended-until*
          :status          (when (and reachable conversation)
                             (multiple-value-bind (status error)
                                 (bili--compression-status conversation)
                               (or status error))))))


;;;; -- Plugin headers --

(defvar *bili-request-conversation* nil
  "The conversation whose provider request is being prepared, or NIL.

Bound only for the dynamic extent of one provider request so that header
construction outside a request, such as provider reconfiguration, stays clean.")

(defvar *bili-current-request* nil)
(defvar *bili-current-provider* nil)

(defvar *bili-current-credentials* nil)
(defvar *bili-current-conversation* nil
  "The conversation of the current provider request.")
(-> bili-plugin-headers (model-provider) list)
(defun bili-plugin-headers (provider)
  "Return the cooperative-plugin headers naming the request's conversation.

The model header is deliberately omitted: the proxy compares it against the
wire model and ignores the reported window on any mismatch, so leaving it out
keeps the window header trusted."
  (let ((configuration (provider-configuration provider)))
    (list (cons "x-bili-plugin" *bili-plugin-agent*)
          (cons "x-bili-plugin-conversation"
                (conversation-identifier *bili-request-conversation*))
          (cons "x-bili-plugin-context-window"
                (princ-to-string (config :context-window configuration))))))

(defmethod openai-compatible-provider-headers :around ((provider openai-compatible-provider))
  "Announce this conversation to the proxy that carries the request."
  (let ((headers (call-next-method)))
    (if (and *bili-active-p* *bili-request-conversation*)
        (append headers (bili-plugin-headers provider))
        headers)))


;;;; -- Transport --

(defvar *bili-post-stream-wrapper* nil
  "The exact wrapper this file installed on PROVIDER-POST-EVENT-STREAM.")

(unless (eq (symbol-function 'cl-llm-provider-api:provider-post-event-stream)
            *bili-post-stream-wrapper*)
  (let ((original (symbol-function 'cl-llm-provider-api:provider-post-event-stream)))
    (setf *bili-post-stream-wrapper*
          (lambda (url body &rest arguments)
            (if (bili--routed-url-p url)
                (let ((dexador.util:*default-proxy* nil))
                  (apply original url body arguments))
                (apply original url body arguments)))
          (symbol-function 'cl-llm-provider-api:provider-post-event-stream)
          *bili-post-stream-wrapper*)))

(defmethod provider-open-response-stream :around
    ((provider openai-compatible-provider) request &key credentials conversation)
  (ignore-errors (bili--restore-routing-when-due))
  (let ((*bili-request-conversation* conversation)
        (*bili-current-provider*    provider)
        (*bili-current-request*     request)
        (*bili-current-conversation* conversation)
        (*bili-current-credentials*
          (or credentials
              (handler-case
                  (cl-rfc8628:credential-manager-credentials
                   (provider-credential-manager provider))
                (error () nil)))))
    (cond
      ((or (not (bili--loopback-endpoint-p provider))
           *bili-fallback-attempted-p*)
       (call-next-method))
      (t
       (handler-case
           (call-next-method)
         ((or stream-error usocket:socket-error sb-sys:deadline-timeout) (condition)
           (declare (ignore condition))
           (bili--suspend-routing)
           (let ((*bili-fallback-attempted-p* t))
             (provider-open-response-stream provider request
                                            :credentials  credentials
                                            :conversation conversation))))))))

;;;; -- Activation --

(defparameter *bili-proxy-start-p* t
  "Whether activation starts a proxy when none answers at the configured origin.

Without this every start would need `bili start` run by hand before Autolith
can offer bili.compress or route traffic.")
(defparameter *bili-proxy-start-timeout-seconds* 15
  "How long activation waits for a freshly started proxy to answer.")
(-> bili--origin-port (string) (option integer))
(defun bili--origin-port (origin)
  "Return the port ORIGIN names, or NIL when it names none."
  (let* ((scheme-end (search "://" origin))
         (host-start (if scheme-end (+ scheme-end 3) 0))
         (colon      (position #\: origin :start host-start)))
    (when colon
      (parse-integer origin :start (1+ colon) :junk-allowed t))))
(-> bili--start-proxy () t)
(defun bili--start-proxy ()
  "Start a detached proxy listening on the port of *BILI-PROXY-ORIGIN*.

LAUNCH-PROGRAM returns once the child is spawned; RUN-PROGRAM waits for the
child to exit, which for a server never happens."
  (let ((port (bili--origin-port *bili-proxy-origin*)))
    (uiop:launch-program
     (append (list "bili" "start" "--no-auto-update")
             (when port (list "--port" (princ-to-string port))))
     :output nil :error-output nil)))
(-> bili-ensure-proxy (&key (:timeout number)) boolean)
(defun bili-ensure-proxy (&key (timeout *bili-proxy-start-timeout-seconds*))
  "Return true when a proxy answers at the configured origin.

Starts one when none answers and *BILI-PROXY-START-P* is true, then polls
until TIMEOUT seconds have elapsed. Each poll uses a short timeout, so one
slow answer cannot stretch the wait past that deadline."
  (or (bili-proxy-reachable-p :timeout 1)
      (and *bili-proxy-start-p*
           (let ((deadline (+ (get-internal-real-time)
                              (* timeout internal-time-units-per-second))))
             (ignore-errors (bili--start-proxy))
             (loop
               (when (bili-proxy-reachable-p :timeout 0.25)
                 (return t))
               (when (>= (get-internal-real-time) deadline)
                 (return nil))
               (sleep 0.25))))))
(defvar *bili-registry-wrapper* nil
  "The exact wrapper this file installed on MAKE-DEFAULT-TOOL-REGISTRY.")

(unless (eq (symbol-function 'make-default-tool-registry) *bili-registry-wrapper*)
  (let ((original (symbol-function 'make-default-tool-registry)))
    (setf *bili-registry-wrapper*
          (lambda (&rest arguments)
            (bili-register-tools (apply original arguments)))
          (symbol-function 'make-default-tool-registry)
          *bili-registry-wrapper*)))

(when (bili-ensure-proxy)
  (setf *bili-active-p* t)
  (when (and *bili-route-provider-traffic-p* (boundp '*configuration*))
    (bili-route-provider-endpoint)))

;;;; -- Compression status --

(-> bili--compression-status (conversation) (values (option hash-table) (option string)))

(defun bili--compression-status (conversation)
  "Return the proxy's status for CONVERSATION and an error string, or NIL.

The second value names why no status came back, so a caller can report the
failure instead of silently showing nothing."
  (handler-case
      (multiple-value-bind (payload status)
          (dexador:get
           (format nil "~A/__bili/plugin/status?fallback=latest&conversationId=~A"
                   *bili-proxy-origin* (conversation-identifier conversation))
           :force-string t :connect-timeout 1 :read-timeout 1 :proxy nil)
        (if (<= 200 status 299)
            (values (json-decode payload) nil)
            (values nil (format nil "the proxy answered HTTP ~D for the status request." status))))
    (error (condition)
      (values nil (princ-to-string condition)))))
;;;; -- Compression prompt --

(defparameter *bili-compression-minimum-total-tokens* 64000
  "Compressible tokens required before the prompt reaches the agent.

A smaller total stays silent: reminding the agent about a few thousand
tokens costs more than the fold saves.")

(-> bili--compression-prompt (conversation) (option string))
(defun bili--compression-prompt (conversation)
  "Return the compression prompt for CONVERSATION, or NIL when little is due.

The proxy reports compressible ranges; the prompt appears only once their
total passes *bili-compression-minimum-total-tokens*, and then names every
range so the agent can fold them in one call."
  (when *bili-active-p*
    (let* ((status  (bili--compression-status conversation))
           (reported (and status (gethash "compressibleRanges" status)))
           (entries  (and reported (coerce reported 'list)))
           (total    (loop for range in entries
                           for tokens = (gethash "tokens" range)
                           when (integerp tokens)
                             sum tokens)))
      (when (and total (> total *bili-compression-minimum-total-tokens*))
        (with-output-to-string (stream)
          (format stream "Context compression is due: ~D compressible tokens across these ranges.~%"
                  total)
          (dolist (range entries)
            (format stream "- ~A-~A (~D tokens)~%"
                    (gethash "startRef" range)
                    (gethash "endRef" range)
                    (gethash "tokens" range)))
          (format stream "~%Write one summary per range and call the compress tool once, with content = one string holding every block: each block starts with its 'm00150-m00220 optional topic' header line followed by that block's summary. Keep exact file paths, symbol names, error strings, numbers, decisions and their reasons. Ranges the proxy did not list are not compressible."))))))

(-> bili--compression-contribution (conversation) (option t))
(defun bili--compression-contribution (conversation)
  "Return the compression contribution for CONVERSATION, or NIL when nothing is due."
  (let ((prompt (bili--compression-prompt conversation)))
    (when prompt
      (make-context-contribution
       :identifier        "billion-context-compression"
       :instruction       prompt
       :priority          40
       :lifetime          ':while-relevant
       :class             ':advice
       :deduplication-key "billion-context-compression"))))

(-> bili-context-contributor (request-context) t)
(defun bili-context-contributor (context)
  "Inject the compression prompt while a range is above the threshold."
  (unless (request-context-compaction-p context)
    (let ((conversation (request-context-conversation context)))
      (when conversation
        (bili--compression-contribution conversation)))))

(register-context-contributor "billion-context-compression"
                              #'bili-context-contributor)
