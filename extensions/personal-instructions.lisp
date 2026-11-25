(in-package #:autolith)

;;;; -- Personal instructions Plugin --

;;; Append the user's global coding rules to the system prompt. The harness
;;; prompt stays intact and the rules follow it. Inference frames bind
;;; *system-prompt-override* and keep their compact prompt.
;;;
;;; Load this file from init.lisp:
;;;
;;;   (load (merge-pathnames "autolith/extensions/personal-instructions.lisp"
;;;                          (uiop:xdg-config-home)))

(defparameter *personal-instructions-path*
  (merge-pathnames "autolith/personal-instructions.md" (uiop:xdg-config-home))
  "File whose text is appended to the system prompt.")

(defparameter *personal-instructions-limit* 16000
  "The maximum characters of personal instructions appended to the prompt.")

(defvar *personal-instructions-cache* nil
  "The (WRITE-DATE . TEXT) cache for the personal instructions file.")

(defun personal-instructions-text ()
  "Return the bounded personal instructions text, or NIL when the file is absent."
  (let ((path *personal-instructions-path*))
    (when (probe-file path)
      (let ((date (file-write-date path)))
        (unless (and *personal-instructions-cache*
                     (eql (first *personal-instructions-cache*) date))
          (setf *personal-instructions-cache*
                (cons date (uiop:read-file-string path))))
        (bounded-string (rest *personal-instructions-cache*)
                        :limit *personal-instructions-limit*)))))

;; Capture the harness definition once, so reloading this file does not wrap the wrapper.
(unless (fboundp 'system-prompt--without-personal-instructions)
  (setf (symbol-function 'system-prompt--without-personal-instructions)
        (symbol-function 'system-prompt)))

(setf (symbol-function 'system-prompt)
      (lambda (configuration)
        "Return the harness system prompt with the personal instructions appended."
        (let ((prompt (system-prompt--without-personal-instructions configuration))
              (extra  (and (null *system-prompt-override*)
                           (personal-instructions-text))))
          (if extra
              (format nil "~A~2%Personal instructions from ~A follow.~2%~A"
                      prompt
                      (namestring *personal-instructions-path*)
                      extra)
              prompt))))
