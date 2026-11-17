;;;; Load the user's Lisp helper file into every SBCL worker process.

(in-package #:autolith)

(defparameter *lisp-helper-file*
  (merge-pathnames "autolith/extensions/lisp-file-helpers.lisp"
                   (uiop:xdg-config-home))
  "The helper file loaded into each worker process when it starts.")

(defvar *lisp-helper-loading* nil
  "True while the helper file loads, so a nested start cannot recurse.")

(defun lisp-helper-autoload--load (worker)
  "Load the helper file into WORKER, reporting a failure without failing the start."
  (when (and (not *lisp-helper-loading*) (probe-file *lisp-helper-file*))
    (let ((*lisp-helper-loading* t))
      (handler-case
          (sbcl-workers:sbcl-worker-request
           worker :eval
           (list :forms
                 (list (format nil "(load ~S)" (namestring *lisp-helper-file*)))))
        (error (condition)
          (warn "Loading ~A into a worker failed: ~A"
                *lisp-helper-file* condition))))))

(defun lisp-helper-autoload--install ()
  "Make every SBCL-WORKERS start load the helper file into the new process."
  (when (get 'sbcl-workers:sbcl-worker-start :lisp-helper-autoload)
    (setf (symbol-function 'sbcl-workers:sbcl-worker-start)
          (get 'sbcl-workers:sbcl-worker-start :lisp-helper-autoload)))
  (setf (get 'sbcl-workers:sbcl-worker-start :lisp-helper-autoload)
        (symbol-function 'sbcl-workers:sbcl-worker-start))
  (setf (symbol-function 'sbcl-workers:sbcl-worker-start)
        (lambda (worker)
          (prog1 (funcall (get 'sbcl-workers:sbcl-worker-start
                               :lisp-helper-autoload)
                          worker)
            (lisp-helper-autoload--load worker))))
  (values))

(lisp-helper-autoload--install)
