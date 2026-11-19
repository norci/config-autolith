(in-package #:autolith)

;;;; -- Session-initial skill --

(defparameter *session-skill-names* '("using-superpowers" "batch-processing" "correct-solution-first")
  "Skill names whose complete bodies enter the first request of each conversation.")

(defparameter *session-skill-injected* (make-hash-table :test #'equal)
  "Conversation identifiers that already received the session-initial skill body.")

(defun session-skill--metadata (configuration name)
  "Return catalog metadata named NAME under CONFIGURATION."
  (find name
        (skill-catalog-skills (skill-catalog-for-configuration configuration))
        :test #'string= :key #'skill-metadata-name))

(defun session-skill--instruction (configuration names)
  "Return the conversation-start instruction carrying the bodies of NAMES under CONFIGURATION."
  (with-output-to-string (stream)
    (dolist (name names)
      (let* ((metadata (session-skill--metadata configuration name))
             (body     (and metadata
                            (handler-case (skill-metadata-read metadata)
                              (error () nil)))))
        (cond
          (body
           (format stream "Skill ~A is loaded at this conversation start. Its complete current instruction body follows and governs the rest of the conversation.~2%~A~2%~%" name body))
          (metadata
           (format stream "Skill ~A is present in the current catalog but its instruction body could not be read.~2%~%" name))
          (t
           (format stream "Skill ~A is absent from the current catalog, so its session-initial body could not be injected.~2%~%" name)))))))

(define-context-contributor session-skill (request)
  "Return the session-initial skill body once per conversation."
  (unless (request-context-compaction-p request)
    (let ((identifier (ignore-errors
                        (conversation-identifier
                         (request-context-conversation request)))))
      (when (and identifier
                 (plusp (length identifier))
                 (not (gethash identifier *session-skill-injected*)))
        (let ((instruction (session-skill--instruction
                            (request-context-configuration request)
                            *session-skill-names*)))
          (setf (gethash identifier *session-skill-injected*) t)
          (make-context-contribution
           :identifier "session-skill"
           :instruction instruction
           :priority 915
           :lifetime :turn
           :class :mandatory
           :deduplication-key "session-skill"))))))
