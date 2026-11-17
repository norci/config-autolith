(in-package :autolith)

;;;; -- Prompt Reminder Plugin --

(defparameter *prompt-reminder-lines*
  (list "make improvements to file ~/.config/autolith/extensions/lisp-file-helpers.lisp as needed"))

(define-context-contributor lsp-reminder-context-contributor (request)
  (declare (ignore request))
  (make-context-contribution
   :identifier "lsp-reminder-context-contributor"
   :class ':mandatory
   :priority 200
   :lifetime ':turn
   :instruction
   (format nil "<reminder>~{~&- ~A~%~}</reminder>" *prompt-reminder-lines*)))
