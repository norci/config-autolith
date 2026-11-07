;;;; -- Lisp file helpers for disposable workers --
;;; Load by hand in each lisp.eval worker after it restarts
;;; This file stays out of init.lisp's plugin list on purpose: it is a working
;;; toolkit that grows with the work, not a startup plugin. Add helpers here
;;; instead of retyping them per session.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :autolith)
    (make-package :autolith :use '(:cl))))

(in-package #:autolith)

(defun split-lines (text)
  "Split TEXT at newlines, dropping the empty tail a final newline produces."
  (let ((lines '())
        (start 0))
    (loop for index from 0 below (length text)
          when (char= (char text index) #\Newline)
            do (push (subseq text start index) lines)
               (setf start (1+ index)))
    (push (subseq text start) lines)
    (let ((reversed (nreverse lines)))
      (if (and reversed (string= "" (first (last reversed))))
          (butlast reversed)
          reversed))))

(defun join-lines (lines)
  "Join LINES with newlines, without a trailing newline."
  (with-output-to-string (out)
    (loop for line in lines
          for first-p = t then nil
          do (unless first-p (write-char #\Newline out))
             (write-string line out))))

(defun write-lines (path lines)
  "Write LINES to PATH one per line with a trailing newline; return the count."
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
    (loop for line in (coerce lines 'list) do (write-line line out)))
  (length lines))

(defun line-indentation (line)
  "Return the number of leading spaces in LINE."
  (or (position-if (lambda (character) (not (char= character #\Space))) line) 0))

(defun count-substring (needle text)
  "Return how many non-overlapping times NEEDLE occurs in TEXT."
  (loop with start = 0
        for position = (search needle text :start2 start)
        while position
        do (setf start (+ position (length needle)))
        count 1))

(defun replace-all-text (text old new)
  "Replace every occurrence of OLD in TEXT with NEW. Argument order is TEXT first."
  (with-output-to-string (out)
    (let ((start 0)
          (width (length old)))
      (loop for position = (search old text :start2 start)
            while position
            do (write-string (subseq text start position) out)
               (write-string new out)
               (setf start (+ position width)))
      (write-string (subseq text start) out))))

(defun brief-error (condition)
  "Describe CONDITION by type and 160 characters, never by its whole payload."
  (let ((text (princ-to-string condition)))
    (list :failed (type-of condition) (subseq text 0 (min 160 (length text))))))

(defun form-line-range (path name)
  "Return the 1-based first and last line of the top-level definition named NAME."
  (let* ((lines (coerce (uiop:read-file-lines path) 'vector))
         (needle (format nil "(defun ~A " name))
         (start (position-if (lambda (line) (search needle line)) lines)))
    (unless start
      (error "No definition named ~A in ~A." name path))
    (let ((depth 0)
          (in-string nil)
          (escaped nil)
          (in-comment nil)
          (end nil))
      (loop for index from start below (length lines)
            while (null end)
            do (let ((line (aref lines index)))
                 (setf in-comment nil)
                 (loop for position from 0 below (length line)
                       for character = (char line position)
                       do (cond ((and (>= position 2)
                                      (char= (char line (1- position)) #\\
                                      )
                                      (char= (char line (- position 2)) #\#)))
                                 (in-comment)
                                 (escaped (setf escaped nil))
                                 ((and in-string (char= character #\\))
                                  (setf escaped t))
                                 (in-string
                                  (when (char= character #\")
                                    (setf in-string nil)))
                                 ((char= character #\") (setf in-string t))
                                 ((char= character #\;) (setf in-comment t))
                                 ((char= character #\() (incf depth))
                                 ((char= character #\))
                                  (decf depth)
                                  (when (and (zerop depth) (null end))
                                    (setf end index))))))
            finally (unless end
                      (error "Definition ~A in ~A never closes." name path)))
      (values (1+ start) (1+ end)))))

(defun splice-file-lines (path start end text)
  "Replace lines START through END (1-based, inclusive) in PATH with TEXT.
Return the new line count. An empty TEXT deletes the range."
  (let* ((lines (coerce (uiop:read-file-lines path) 'vector))
         (count (length lines)))
    (unless (<= 1 start end count)
      (error "Range ~D-~D is outside 1..~D in ~A." start end count path))
    (let ((updated (concatenate 'vector
                                (subseq lines 0 (1- start))
                                (coerce (split-lines text) 'vector)
                                (subseq lines end))))
      (write-lines path updated)
      (length updated))))

(defun apply-splices (path splices)
  "Apply (START END HEAD-ANCHOR TAIL-ANCHOR LINE...) ranges to PATH.
Every range must match its anchors before anything is written, so one bad
range leaves PATH untouched and reports its range and error type."
  (let ((lines (coerce (uiop:read-file-lines path) 'vector))
        (prepared '())
        (failures '()))
    (dolist (splice (sort (copy-list splices) #'> :key #'first))
      (handler-case
          (let ((start (first splice))
                (end (second splice))
                (head-anchor (third splice))
                (tail-anchor (fourth splice))
                (replacement (cddddr splice)))
            (unless (<= 1 start end (length lines))
              (error "Range ~D-~D is outside 1..~D." start end (length lines)))
            (unless (search head-anchor (aref lines (1- start)))
              (error "Start anchor ~S is absent at line ~D." head-anchor start))
            (unless (search tail-anchor (aref lines (1- end)))
              (error "Tail anchor ~S is absent at line ~D." tail-anchor end))
            (push (list start end replacement) prepared))
        (condition (condition)
          (push (list (first splice) (second splice) (brief-error condition))
                failures))))
    (if failures
        (list :applied 0 :failures (nreverse failures))
        (let ((updated lines))
          (dolist (entry (sort prepared #'> :key #'first))
            (destructuring-bind (start end replacement) entry
              (setf updated (concatenate 'vector
                                         (subseq updated 0 (1- start))
                                         (coerce replacement 'vector)
                                         (subseq updated end)))))
          (write-lines path updated)
          (list :applied (length prepared)
                :line-count (length updated)
                :failures nil)))))


(defun file-reads-completely-p (path)
  (handler-case
      (with-open-file (in path :direction :input)
        (loop for form = (read in nil :eof)
              until (eq form :eof)
              finally (return t)))
    (error () nil)))

(defun replace-range (path start end lines)
  (let* ((content (uiop:read-file-lines path))
         (updated (append (subseq content 0 (1- start))
                          lines
                          (subseq content end))))
    (write-lines path updated)))

(defun splice-range (path start end text)
  (let* ((lines (split-lines text)))
    (labels ((with-tail (n)
               (let ((tail (car (last lines)))
                     (head (butlast lines)))
                 (cond ((plusp n)
                        (append head
                                (list (concatenate 'string
                                                   tail
                                                   (make-string n :initial-element #\))))))
                       ((minusp n)
                        (append head
                                (list (subseq tail 0 (max 0 (- (length tail) (- n)))))))
                       (t lines))))
             (attempt (candidate)
               (replace-range path start end candidate)
               (file-reads-completely-p path)))
      (let ((original (uiop:read-file-lines path)))
        (cond ((attempt (with-tail 0))
               (values t (length (uiop:read-file-lines path)) 0))
              (t
               (loop for n in '(1 -1 2 -2 3 -3 4 5 6)
                     when (attempt (with-tail n))
                       do (return-from splice-range
                            (values t (length (uiop:read-file-lines path)) n)))
               (write-lines path original)
               (values nil nil nil)))))))
