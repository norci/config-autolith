(in-package #:autolith)

;;;; -- Prompt Guard Plugin --

;;; Replaces prompt sentences that cast the agent as the acting party and the
;;; harness or the user as adjustable. Loaded from init.lisp after
;;; personal-instructions.lisp.


(DEFPARAMETER *PROMPT-GUARD-REPLACEMENTS*
  '(("Perform any additional steps you identify instead of handing them back\\s+as suggestions\\."
     "Report any additional steps you identify and wait for the user's authorization before performing them. The harness does not compensate for your own mistakes.")
    ("Self-modification is a routine way to remove Autolith-side friction or\\s+improve workflows\\."
     "Self-modification is a deliberate change the user authorizes; friction caused by your own output is not an Autolith defect.")
    ("Small reversible internal improvements within existing authority need\\s+no extra permission; report them afterward\\."
     "Small reversible internal improvements within existing authority still need the user's authorization; report them afterward.")
    ("Persist end-to-end whenever feasible, including through failed tool\\s+calls\\."
     "Persist toward the goal end-to-end. A failed tool call is a parameter error, not a reason to resend the same call: read the error, change the arguments, and continue.")
    ("After starting one,\\s+continue independent work or end your turn; never poll it with\\s+=job\\.get=\\s+or\\s+=job\\.wait=\\s*\\."
     "After =async: true=, do work that does not need the result, or end the turn and let the wakeup continue it. A =job.get= or =job.wait= on a job you started in this turn buys nothing and costs a full paid request, so one call is already too many: the call is forbidden.")
    ("Read =job\\.get= or the job's logs only when the\\s+preview is not enough\\."
     "Read =job.get= or the job's logs only when the user asks for a job's current state, or when the preview is not enough and the handoff result says the session has no automatic wakeup.")
    ("The handoff result says when a session has no\\s+automatic wakeup; there, use =job\\.wait= when you need the result\\."
     "Use =job.wait= only in that no-wakeup session, and only when the result is needed to continue.")
    )
  "Regex and replacement pairs applied to every rendered system prompt.")

(defvar *prompt-guard-original* nil
  "The system-prompt definition captured before this hook wrapped it.")

(unless *prompt-guard-original*
  (setf *prompt-guard-original* (symbol-function 'system-prompt)))

(defun system-prompt (configuration)
  "Return the rendered system prompt with the guarded sentences replaced."
  (let ((text (funcall *prompt-guard-original* configuration)))
    (if *system-prompt-override*
        text
        (loop for (pattern replacement) in *prompt-guard-replacements*
              do (setf text (cl-ppcre:regex-replace-all pattern text replacement))
              finally (return text)))))
