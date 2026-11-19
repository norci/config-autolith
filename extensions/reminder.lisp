(in-package :autolith)

;;;; -- Prompt Reminder Plugin --

(defparameter *prompt-reminder-lines*
  (list "use helper functions in ~/.config/autolith/extensions/lisp-file-helpers.lisp . make improvements to this file as needed"))

(define-context-contributor reminder-context-contributor (request)
  (declare (ignore request))
  (make-context-contribution
   :identifier "reminder-context-contributor"
   :class ':mandatory
   :priority 200
   :lifetime ':turn
   :instruction
   (format nil "<reminder>~{~&- ~A~%~}</reminder>" *prompt-reminder-lines*)))
