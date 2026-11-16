(in-package #:autolith)

;;;; -- User plugins --

(mapc (lambda (name)
        (load (merge-pathnames (format nil "autolith/extensions/~A.lisp" name)
                               (uiop:xdg-config-home))))
      '(
	"openrouter-routing"
        "billion-context"
        "lisp-ast-tools"
        "lsp-reminder"
        "mcp-loopback"
        "personal-instructions"
        "prompt-guard"
        "skill-catalog-compact"
        "always-selected-skills"
	))

(setf *conversation-title-generation-output-tokens* 4096)
