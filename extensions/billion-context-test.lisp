(in-package #:autolith)

(defparameter *billion-context-test-a*
  (make-instance 'conversation :identifier "billion-context-test-a"
                               :pathname #P"/tmp/billion-context-test-a.sexp"
                               :log-pathname #P"/tmp/billion-context-test-a.log"
                               :created-at (get-universal-time)
                               :next-sequence 1))

(defun billion-context-test--with-stubs (bindings thunk)
  (let ((saved (mapcar (lambda (binding) (cons (car binding) (symbol-function (car binding)))) bindings)))
    (unwind-protect
         (progn (dolist (binding bindings) (setf (symbol-function (car binding)) (cdr binding)))
                  (clrhash *bili-status-cache*)
                  (funcall thunk))
      (dolist (pair saved) (setf (symbol-function (car pair)) (cdr pair))))))

(defun billion-context-test--range (tokens start-ref end-ref)
  (let ((range (make-hash-table :test #'equal)))
    (setf (gethash "tokens" range) tokens
          (gethash "startRef" range) start-ref
          (gethash "endRef" range) end-ref)
    range))

(defun billion-context-test--status (ranges &optional (conversation *billion-context-test-a*))
  (let ((status (make-hash-table :test #'equal)))
    (setf (gethash "conversationId" status) (conversation-identifier conversation)
          (gethash "compressibleRanges" status) (coerce ranges 'vector)
          (gethash "contextTokensSource" status) "usage"
          (gethash "contextTokens" status)
          (loop for range in ranges sum (gethash "tokens" range))
          (gethash "contextLimit" status) 70000)
    status))

(defun billion-context-test--case-prompt-due ()
  "A total above the threshold names every range in the injected prompt."
  (let ((*bili-active-p* t))
    (billion-context-test--with-stubs
     (list (cons 'bili--compression-status
                 (lambda (conversation)
                   (declare (ignore conversation))
                   (billion-context-test--status
                    (list (billion-context-test--range 60000 "m1" "m2"))))))
     (lambda ()
       (let ((prompt (bili--compression-prompt *billion-context-test-a*)))
         (assert (stringp prompt))
         (assert (search "60000 of 70000 window tokens" prompt))
         (assert (search "m1-m2" prompt))
         (assert (search "compress tool" prompt))
         (assert (bili--compression-contribution *billion-context-test-a*)))))))

(defun billion-context-test--case-prompt-idle ()
  "A total below the threshold injects nothing."
  (let ((*bili-active-p* t))
    (billion-context-test--with-stubs
     (list (cons 'bili--compression-status
                 (lambda (conversation)
                   (declare (ignore conversation))
                   (billion-context-test--status
                    (list (billion-context-test--range 40000 "m1" "m2"))))))
     (lambda ()
       (assert (null (bili--compression-prompt *billion-context-test-a*)))
       (assert (null (bili--compression-contribution *billion-context-test-a*)))))))

(defun billion-context-test--case-prompt-many ()
  "Several small ranges add up past the threshold and are all named."
  (let ((*bili-active-p* t))
    (billion-context-test--with-stubs
     (list (cons 'bili--compression-status
                 (lambda (conversation)
                   (declare (ignore conversation))
                   (billion-context-test--status
                    (list (billion-context-test--range 20000 "m1" "m2")
                          (billion-context-test--range 20000 "m3" "m4")
                          (billion-context-test--range 20000 "m5" "m6"))))))
     (lambda ()
       (let ((prompt (bili--compression-prompt *billion-context-test-a*)))
         (assert (stringp prompt))
         (assert (search "60000 of 70000 window tokens" prompt))
         (assert (search "m1-m2" prompt))
         (assert (search "m3-m4" prompt))
         (assert (search "m5-m6" prompt)))))))
(defun billion-context-test--case-prompt-inactive ()
  "An inactive plugin injects nothing even when a range is due."
  (let ((*bili-active-p* nil))
    (billion-context-test--with-stubs
     (list (cons 'bili--compression-status
                 (lambda (conversation)
                   (declare (ignore conversation))
                   (billion-context-test--status
                    (list (billion-context-test--range 9000 "m1" "m2"))))))
     (lambda ()
       (assert (null (bili--compression-prompt *billion-context-test-a*)))))))

(defun billion-context-test--case-prompt-unreachable ()
  "A proxy that does not answer injects nothing."
  (let ((*bili-active-p* t))
    (billion-context-test--with-stubs
     (list (cons 'bili--compression-status
                 (lambda (conversation) (declare (ignore conversation)) nil)))
     (lambda ()
       (assert (null (bili--compression-prompt *billion-context-test-a*)))))))

(defun billion-context-test--case-ensure-proxy-attaches ()
  "A reachable proxy is used as is: no process is started."
  (let ((spawned nil))
    (billion-context-test--with-stubs
     (list (cons 'bili-proxy-reachable-p
                 (lambda (&rest arguments) (declare (ignore arguments)) t))
           (cons 'bili--start-proxy (lambda () (setf spawned t))))
     (lambda ()
       (assert (eq (bili-ensure-proxy) t))
       (assert (not spawned))))))

(defun billion-context-test--case-ensure-proxy-starts ()
  "An unreachable proxy is started once and then waited for."
  (let ((spawned 0)
        (probes  0))
    (billion-context-test--with-stubs
     (list (cons 'bili-proxy-reachable-p
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (incf probes)
                   (> probes 2)))
           (cons 'bili--start-proxy (lambda () (incf spawned))))
     (lambda ()
       (assert (eq (bili-ensure-proxy :timeout 2) t))
       (assert (= spawned 1))))))

(defun billion-context-test--case-ensure-proxy-disabled ()
  "With startup disabled an unreachable proxy leaves the plugin off."
  (let ((spawned nil))
    (billion-context-test--with-stubs
     (list (cons 'bili-proxy-reachable-p
                 (lambda (&rest arguments) (declare (ignore arguments)) nil))
           (cons 'bili--start-proxy (lambda () (setf spawned t))))
     (lambda ()
       (let ((*bili-proxy-start-p* nil))
         (assert (null (bili-ensure-proxy :timeout 1)))
         (assert (not spawned)))))))

(defun billion-context-test--run ()
  (billion-context-test--case-prompt-due)
  (billion-context-test--case-prompt-idle)
  (billion-context-test--case-prompt-many)
  (billion-context-test--case-prompt-inactive)
  (billion-context-test--case-prompt-unreachable)
  (billion-context-test--case-ensure-proxy-attaches)
  (billion-context-test--case-ensure-proxy-starts)
  (billion-context-test--case-ensure-proxy-disabled)
    (billion-context-test--case-replay-installs-reading)
    (billion-context-test--case-replay-skips-without-application)
  :passed)

(defun billion-context-test--case-replay-installs-reading ()
  (let* ((application (make-instance 'application))
         (*bili-active-p* t)
         (*active-application* application))
    (setf (application-provider application) nil
          (application-tool-registry application) (make-instance 'tool-registry))
    (setf (conversation-context-usage-sample *billion-context-test-a*) nil)
    (billion-context-test--with-stubs
     (list (cons 'application-provider
                 (lambda (application) (declare (ignore application)) :provider-stub))
           (cons 'application-tool-registry
                 (lambda (application) (declare (ignore application)) :registry-stub))
           (cons 'bili--request-fingerprints
                 (lambda (conversation provider registry)
                   (declare (ignore conversation))
                   (assert (eq provider :provider-stub))
                   (assert (eq registry :registry-stub))
                   (values nil :digest (list :policy :p :prompt :q :tool-header :t) :header)))
           (cons 'conversation--persisted-model-selection-p
                 (lambda (&rest arguments) (declare (ignore arguments)) t))
           (cons 'provider-context--sample-matches-p
                 (lambda (&rest arguments) (declare (ignore arguments)) nil))
           (cons 'bili--wire-usage
                 (lambda (conversation) (declare (ignore conversation)) (values 1234 200000)))
           (cons 'bili--context-usage-sample
                 (lambda (conversation provider registry tokens)
                   (declare (ignore conversation))
                   (assert (eq provider :provider-stub))
                   (assert (eq registry :registry-stub))
                   (list :format 2 :tokens tokens))))
     (lambda ()
       (conversation--project-record :configuration *billion-context-test-a*
                                     '(:model "stub-model" :reasoning-effort nil))
       (assert (eql 1234 (getf (conversation-context-usage-sample *billion-context-test-a*) :tokens)))))))

(defun billion-context-test--case-replay-skips-without-application ()
  (let ((*bili-active-p* t)
        (*active-application* nil)
        (sample-calls 0))
    (setf (conversation-context-usage-sample *billion-context-test-a*) nil)
    (billion-context-test--with-stubs
     (list (cons 'conversation--persisted-model-selection-p
                 (lambda (&rest arguments) (declare (ignore arguments)) t))
           (cons 'bili--context-usage-sample
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (incf sample-calls))))
     (lambda ()
       (conversation--project-record :configuration *billion-context-test-a*
                                     '(:model "stub-model" :reasoning-effort nil))
       (assert (zerop sample-calls))
       (assert (null (conversation-context-usage-sample *billion-context-test-a*)))))))
