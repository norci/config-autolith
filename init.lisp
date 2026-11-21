(in-package #:autolith)

;;;; -- User plugins --

(mapc (lambda (name)
        (load (merge-pathnames (format nil "autolith/extensions/~A.lisp" name)
                               (uiop:xdg-config-home))))
      '(
	"openrouter-routing"
        "billion-context"
        "lisp-ast-tools"
        "lisp-file-helpers"
        "lisp-helper-autoload"
        "reminder"
        "personal-instructions"
        "always-selected-skills"
	))

(setf *conversation-title-generation-output-tokens* 4096)

(setf *permissions-model-token-budget* 32768)

(setf dexador.util:*default-proxy* nil)
