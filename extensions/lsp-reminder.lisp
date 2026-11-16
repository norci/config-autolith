(in-package :autolith)

;;;; -- Prompt Reminder Plugin --

(defparameter *prompt-reminder-lines*
  (list
   "use agent-lsp-navigation skill for go/js/ts/py/rust coding task. 严禁加载代码库至 context 后手工分析"
   "use batch-processing skill in every turn: 一个响应块里放本轮能提前定参的全部调用；不要为填满一轮发凑数调用，没有可提前定参的第二个调用时就只发一个。"
   "MUST use correct-solution-first skill: 动手前写出不变量、所属层与完整解法。"
   "read file with lisp code instead of resource.read."
   "use helpers in file ~/.config/autolith/extensions/lisp-file-helpers.lisp, and optimize lisp-file-helpers.lisp as needed"))

(define-context-contributor lsp-reminder-context-contributor (request)
  (declare (ignore request))
  (make-context-contribution
   :identifier "lsp-reminder-context-contributor"
   :class ':mandatory
   :priority 200
   :lifetime ':turn
   :instruction
   (format nil "~{~&- ~A~%~}" *prompt-reminder-lines*)))
