(in-package #:autolith)

;;;; -- Session-initial skill --

(defparameter *session-skill-names* '("using-superpowers" "batch-processing" "correct-solution-first")
  "Skill names whose complete bodies enter the conversation as one anchored history block.")


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
  "Return the session skill bodies as one anchored history block."
  (let* ((configuration (request-context-configuration request))
         (instruction   (session-skill--instruction configuration *session-skill-names*)))
    (change-class
     (make-context-contribution
      :identifier        "session-skill"
      :instruction       instruction
      :priority          915
      :class             ':mandatory
      :deduplication-key "session-skill")
     'anchored-context-contribution)))

(define-context-contributor execution-discipline (request)
  "Return the short execution rule that rides every request."
  (declare (ignore request))
  (make-context-contribution
   :identifier "execution-discipline"
   :instruction "技能里的流程是函数：加载后一次调用走完，不按文本步骤手工执行。编辑、验证、提交写进同一个程序；工具返回中间值时不为确认它再开一轮。"
   :priority 950
   :class ':mandatory
   :lifetime :turn
   :deduplication-key "execution-discipline"))
