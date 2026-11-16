(in-package #:autolith)

;;;; -- User plugins --

(mapc (lambda (name)
        (load (merge-pathnames (format nil "autolith/extensions/~A.lisp" name)
                               (uiop:xdg-config-home))))
      '("openrouter-routing"
        ;; "billion-context"
        "personal-instructions"
        "mcp-loopback"
        "prompt-guard"
        "lisp-ast-tools"
        "lsp-reminder"))

(setf *conversation-title-generation-output-tokens* 4096)
