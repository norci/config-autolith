(in-package #:autolith)

;;;; -- Tool call interval --

;;; Waits between consecutive tool waves of one conversation. Calls inside one
;;; wave still run concurrently; the interval is measured from the start of one
;;; wave to the start of the next.
;;;
;;; The gate wraps AGENT--RUN-TOOL-WAVE, which AGENT--EXECUTE-TOOL-CALLS calls
;;; exactly once per wave.

(defparameter *tool-call-minimum-interval-seconds* 10
  "Minimum seconds between the start of consecutive tool waves in one conversation.")

(defstruct tool-call-interval-entry
  "One conversation's tool wave gate state."
  (lock (make-lock "tool call interval") :read-only t)
  (last-send nil))

(defvar *tool-call-interval-table-lock* (make-lock "tool call interval table")
  "Protects the conversation-to-entry table during lookup and creation.")

(defvar *tool-call-interval-entries* (make-hash-table :test #'equal)
  "Maps a conversation identifier to its tool wave gate entry.")

(defvar *tool-call-interval-original-run-tool-wave* nil
  "The tool wave runner captured before this extension replaced it.")

(defun tool-call-interval--now-seconds ()
  "Return the current internal real time in seconds."
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun tool-call-interval--conversation-key (agent)
  "Return AGENT's durable conversation identifier, or NIL when it has none."
  (let ((conversation (and (typep agent 'agent) (agent-conversation agent))))
    (and (typep conversation 'conversation)
         (conversation-identifier conversation))))

(defun tool-call-interval--entry (key)
  "Return KEY's gate entry, creating it on first use."
  (with-lock-held (*tool-call-interval-table-lock*)
    (or (gethash key *tool-call-interval-entries*)
        (setf (gethash key *tool-call-interval-entries*)
              (make-tool-call-interval-entry)))))

(defun tool-call-interval--wait (entry)
  "Sleep until the configured interval has elapsed since ENTRY's last wave send."
  (with-lock-held ((tool-call-interval-entry-lock entry))
    (let ((last-send (tool-call-interval-entry-last-send entry)))
      (when last-send
        (let ((gap (- (tool-call-interval--now-seconds) last-send)))
          (when (< gap *tool-call-minimum-interval-seconds*)
            (sleep (- *tool-call-minimum-interval-seconds* gap))))))))

(defun tool-call-interval--stamp-send (entry)
  "Record that ENTRY's conversation sent a tool wave now."
  (with-lock-held ((tool-call-interval-entry-lock entry))
    (setf (tool-call-interval-entry-last-send entry)
          (tool-call-interval--now-seconds))))

(unless *tool-call-interval-original-run-tool-wave*
  (setf *tool-call-interval-original-run-tool-wave*
        (symbol-function 'agent--run-tool-wave)))

(defun agent--run-tool-wave (agent plans &rest options
                                    &key observer tool-round tool-restriction-p)
  "Wait for the conversation's interval, then run the whole wave concurrently."
  (declare (ignore observer tool-round tool-restriction-p))
  (let ((entry (tool-call-interval--entry
                 (tool-call-interval--conversation-key agent))))
    (tool-call-interval--wait entry)
    (tool-call-interval--stamp-send entry)
    (apply *tool-call-interval-original-run-tool-wave* agent plans options)))
