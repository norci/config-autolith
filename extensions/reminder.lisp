(in-package :autolith)

;;;; -- Prompt Reminder Plugin --

(defparameter *prompt-reminder-lines*
  (list "输出前逐句过闸。《金字塔原理》。写完整的短句。不写多重转折的长句。不用论文腔与装饰性文辞。"
	"Must use agent-lsp tools for coding. skill.load agent-lsp-navigation in first turn. forbid read lots of code then wire by hand."
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
