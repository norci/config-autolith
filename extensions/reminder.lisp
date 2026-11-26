(in-package :autolith)

;;;; -- Prompt Reminder Plugin --

(defparameter *prompt-reminder-lines*
  (list "输出前逐句过闸。《金字塔原理》。写完整的短句。不写多重转折的长句。不用论文腔与装饰性文辞。"
	"must batch-processing. forbid split tool calls into multiple round trip"
	"correct-solution-first. forbid minimize diff."
	"施工走敏捷开发流程：小步迭代，每轮只落一个决定，交付最小可用增量，不攒大批改动。"
	))

(define-context-contributor reminder-context-contributor (request)
  (declare (ignore request))
  (make-context-contribution
   :identifier "reminder-context-contributor"
   :class ':mandatory
   :priority 100
   :lifetime ':turn
   :instruction
   (format nil "<reminder>~{~&- ~A~%~}</reminder>" *prompt-reminder-lines*)))
