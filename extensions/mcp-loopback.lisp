(in-package #:autolith)

;;;; -- MCP loopback Plugin --

;;; Dexador defaults *default-proxy* to HTTPS_PROXY or HTTP_PROXY and
;;; ignores NO_PROXY, so mcparen sent loopback MCP requests through the
;;; user's HTTP proxy, which reset them. Autolith's own HTTP layer passes
;;; :proxy nil explicitly; clearing the Dexador default keeps third-party
;;; clients direct.
;;;
;;; Loaded from init.lisp.

(setf dexador:*default-proxy* nil)
