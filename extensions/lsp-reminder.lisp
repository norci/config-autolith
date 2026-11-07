(in-package :autolith)

;;;; -- Prompt Reminder Plugin --

;;; Injects the per-turn operating rules as one mandatory turn-scoped context
;;; contribution. Rule bodies live in their SKILL.md files; this block names a
;;; rule and when to load it, never its text.

(defparameter *prompt-reminder-lines*
  (list
   "MUST use agent-lsp-navigation skill for go/js/ts/py/rust coding task. 严禁加载代码库至 context 后手工分析"
   "MUST use batch-processing skill in every turn: 一个响应块里放本轮能提前定参的全部调用；不要为填满一轮发凑数调用，没有可提前定参的第二个调用时就只发一个。"
   "后台作业完成会自己唤醒本轮；等它,不要找动作填空。"
   "MUST use correct-solution-first skill for making solution: 动手前写出不变量、所属层与完整解法。"
   "技能正文已在上下文里就不再 skill.load；被压缩掉之后才重新加载。"
   "read file with lisp code instead of resource.read."
   "用户最新一句话决定本轮目标；此前的目标不因还差一步而继续。")
  "Per-turn operating rules, injected as one mandatory context contribution.")

(define-context-contributor lsp-reminder-context-contributor (request)
  (declare (ignore request))
  (make-context-contribution
   :identifier "lsp-reminder-context-contributor"
   :class ':mandatory
   :priority 200
   :lifetime ':turn
   :instruction
   (format nil "<system>~{~&- ~A~%~}</system>" *prompt-reminder-lines*)))
