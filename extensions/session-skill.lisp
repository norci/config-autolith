(in-package #:autolith)

;;;; -- Session-initial skill --

(defparameter *session-skill-name* "using-superpowers"
  "The skill whose complete body enters the first request of each conversation.")

(defparameter *session-skill-injected* (make-hash-table :test #'equal)
  "Conversation identifiers that already received the session-initial skill body.")

(defun session-skill--read-text (pathname)
  "Return PATHNAME complete text, or NIL when it cannot be read."
  (handler-case
      (with-open-file (stream pathname :element-type 'character)
        (let ((text (make-string (file-length stream))))
          (subseq text 0 (read-sequence text stream))))
    (error () nil)))

(defun session-skill--strip-frontmatter (text)
  "Return TEXT without its leading YAML frontmatter block."
  (let* ((newline (string (code-char 10)))
         (marker (concatenate 'string newline "---"))
         (opening (search "---" text)))
    (if (and opening (zerop opening))
        (let ((closing (search marker text :start2 (length marker))))
          (if closing
              (string-left-trim (list (code-char 10) (code-char 32))
                                (subseq text (+ closing (length marker))))
              text))
        text)))

(defun session-skill--metadata (configuration name)
  "Return catalog metadata named NAME under CONFIGURATION."
  (find name
        (skill-catalog-skills (skill-catalog-for-configuration configuration))
        :test #'string= :key #'skill-metadata-name))

(defun session-skill--body (configuration name)
  "Return NAME instruction body, or NIL when the catalog cannot supply it."
  (let ((metadata (session-skill--metadata configuration name)))
    (when metadata
      (let ((text (session-skill--read-text (skill-metadata-pathname metadata))))
        (when text
          (session-skill--strip-frontmatter text))))))

(defun session-skill--first-p (identifier)
  "Record IDENTIFIER and return true only for its first request in this process."
  (when (and identifier (plusp (length identifier)))
    (if (gethash identifier *session-skill-injected*)
        nil
        (progn
          (setf (gethash identifier *session-skill-injected*) t)
          t))))

(define-context-contributor session-skill (request)
  "Return the session-initial skill body once per conversation."
  (unless (request-context-compaction-p request)
    (let ((identifier (ignore-errors
                        (conversation-identifier
                         (request-context-conversation request)))))
      (when (session-skill--first-p identifier)
        (let* ((configuration (request-context-configuration request))
               (body (session-skill--body configuration *session-skill-name*)))
          (make-context-contribution
           :identifier "session-skill"
           :instruction
           (if body
               (format nil "Skill ~A is loaded at this conversation start. Its complete current instruction body follows and governs the rest of the conversation.~2%~A"
                       *session-skill-name* body)
               (format nil "Skill ~A is absent from the current catalog, so its session-initial body could not be injected."
                       *session-skill-name*))
           :priority 915
           :lifetime :turn
           :class :mandatory
           :deduplication-key "session-skill"))))))
