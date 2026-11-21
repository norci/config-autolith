(in-package #:autolith)

;;;; -- Always-selected skills --

(defparameter *always-selected-skill-names*
  '(
    "batch-processing"
    "correct-solution-first"
    )
  "Skill names whose bodies enter requests gated by *ALWAYS-SELECTED-SKILL-INJECTION-MODE*.")

(defparameter *always-selected-skill-injection-mode* ':user-prompt
  "When always-selected bodies enter requests: :TURN every request, :USER-PROMPT only on a real user prompt.")

(-> skill--always-selected-contributions (skill-catalog list) list)
(-> skill--always-selected-contribution (skill-metadata) context-contribution)
(-> skill--selected-instruction-characters (list) (integer 0))
(defun skill--always-selected-contribution (metadata)
  "Return the per-request contribution carrying METADATA's complete instructions.

A source that can no longer be read yields the module's read-failure warning
instead of signalling out of request assembly.

The contribution stays out of the anchored projection: anchored context is
placed inside durable history, where history rewriting drops it, while a turn
lifetime renders it in blocks every request rebuilds."
  (let ((identifier (skill--context-identifier
                     "skill-selected" (skill-metadata-name metadata))))
    (handler-case
        (make-context-contribution
         :identifier        identifier
         :instruction       (format nil
                                    "Skill ~A governs this whole session. Its complete current instruction body from ~A follows. Apply it to every request, including later turns, until the session ends.~2%~A"
                                    (skill-metadata-name metadata)
                                    (namestring (skill-metadata-pathname metadata))
                                    (skill-metadata-read metadata))
         :priority          920
         :lifetime          ':turn
         :class             ':mandatory
         :deduplication-key identifier)
      (skill-read-error (condition)
        (skill--warning-contribution
         metadata
         (skill--read-failure-instruction metadata condition))))))

(defun skill--selected-instruction-characters (contributions)
  "Return the characters of explicitly selected skill instructions in CONTRIBUTIONS.

The catalog and warning contributions are excluded, so the aggregate limit bound
by *SKILL-SELECTION-CHARACTER-LIMIT* counts selected skill instructions only."
  (let ((prefix "skill-selected-")
        (total  0))
    (dolist (contribution contributions total)
      (let ((identifier (context-contribution-identifier contribution)))
        (when (and (>= (length identifier) (length prefix))
                   (string= prefix identifier :end2 (length prefix)))
          (incf total
                (length (context-contribution-instruction contribution))))))))
(-> skill--place-selected-contribution (skill-metadata list (integer 0)) (values list (integer 0)))
(defun skill--place-selected-contribution (metadata contributions selected-characters)
  "Return CONTRIBUTIONS and the character total after placing METADATA's body."
  (let* ((contribution (skill--always-selected-contribution metadata))
         (identifier   (context-contribution-identifier contribution))
         (existing     (position identifier contributions
                                 :key  #'context-contribution-identifier
                                 :test #'string=))
         (replaced     (and existing
                            (context-contribution-instruction
                             (nth existing contributions))))
         (next-total   (+ selected-characters
                          (if replaced (- (length replaced)) 0)
                          (length (context-contribution-instruction contribution)))))
    (cond
      ((> next-total *skill-selection-character-limit*)
       (values (append contributions
                       (list
                        (skill--warning-contribution
                         metadata
                         (format nil
                                 "Skill ~A was selected but omitted because selected skill instructions exceed the ~D-character aggregate limit. Continue with the best fallback and report the omission."
                                 (skill-metadata-name metadata)
                                 *skill-selection-character-limit*))))
               selected-characters))
      (existing
       (values (append (subseq contributions 0 existing)
                       (list contribution)
                       (nthcdr (1+ existing) contributions))
               next-total))
      (t
       (values (append contributions (list contribution)) next-total)))))

(defun skill--always-selected-contributions (catalog contributions)
  "Return CONTRIBUTIONS extended with the always-selected skill bodies from CATALOG."
  (let ((selected-characters
          (skill--selected-instruction-characters contributions)))
    (dolist (name *always-selected-skill-names* contributions)
      (let ((metadata (skill-catalog-find catalog name)))
        (if metadata
            (multiple-value-bind (extended total)
                (skill--place-selected-contribution
                 metadata contributions selected-characters)
              (setf contributions extended
                    selected-characters total))
            (setf contributions
                  (append contributions
                          (list (skill--missing-selection-contribution name)))))))))

(-> skill--user-prompt-p (conversation) boolean)
(defun skill--user-prompt-p (conversation)
  "True when CONVERSATION's newest durable user message is a real prompt."
  (let ((record (first (conversation-records-newest conversation 1))))
    (and (consp record)
         (eq (first record) ':message)
         (eq (getf (rest record) ':role) ':user)
         (not (getf (rest record) ':automatic-p)))))

(-> skill-request-contributions (configuration conversation) list)
(defun skill-request-contributions (configuration conversation)
  "Return catalog and always-selected skill contributions for one provider request."
  (let* ((catalog  (skill-catalog-for-configuration configuration))
         (selected (skill--request-contributions-for-catalog catalog conversation)))
    (skill--bound-warning-contributions
     (if (and (eq *always-selected-skill-injection-mode* ':user-prompt)
              (not (skill--user-prompt-p conversation)))
         selected
          (skill--always-selected-contributions catalog selected)))))
