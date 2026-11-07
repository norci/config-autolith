;;;; Lisp helpers for reading, slicing and splicing source files.
;;;;
;;;; init.lisp loads this file into the main image; lisp-helper-autoload.lisp
;;;; loads it into every SBCL worker when the process starts.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :autolith)
    (make-package :autolith :use '(:cl))))

(in-package #:autolith)
(declaim (ftype function slice-lines brief-text form-count form-count-or-nil
                     replace-range replaced-range))

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
  "Return the number of leading spaces in LINE, or its length when all of it is spaces."
  (or (position-if-not (lambda (character) (char= character #\Space)) line)
      (length line)))

(defun count-substring (needle text)
  "Return how many non-overlapping times NEEDLE occurs in TEXT."
  (when (zerop (length needle))
    (error "count-substring needs a non-empty NEEDLE."))
  (loop with start = 0
        for position = (search needle text :start2 start)
        while position
        do (setf start (+ position (length needle)))
        count 1))

(defun replace-all-text (text old new)
  "Replace every occurrence of OLD in TEXT with NEW. Argument order is TEXT first."
  (when (zerop (length old))
    (error "replace-all-text needs a non-empty OLD."))
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
         (needle (format nil "(defun ~A" name))
         (start (position-if (lambda (line)
                               (let ((at (search needle line)))
                                 (and at
                                      (or (= (length line) (+ at (length needle)))
                                          (find (char line (+ at (length needle))) " ()")))))
                             lines)))
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

(defun form-source (path name)
  "Return the complete source text of the top-level DEFUN named NAME in PATH.
Return NIL when PATH holds no such definition."
  (handler-case
      (multiple-value-bind (start end) (form-line-range path name)
        (slice-lines path start end))
    (error () nil)))

(defun splice-file-lines (path start end text)
  "Replace lines START through END (1-based, inclusive) in PATH with TEXT.
Return the new line count. An empty TEXT deletes the range."
  (replace-range path start end (split-lines text)))

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
                (setf updated (replaced-range updated start end replacement))))
          (write-lines path updated)
          (list :applied (length prepared)
                :line-count (length updated)
                :failures nil)))))


(defun file-reads-completely-p (path)
  "Return true when every form in PATH reads without error."
  (and (form-count-or-nil (uiop/stream:read-file-string path)) t))

(defun replaced-range (lines start end replacement)
  "Return LINES with its START..END (1-based, inclusive) replaced by REPLACEMENT."
  (concatenate 'vector
               (subseq lines 0 (1- start))
               replacement
               (subseq lines end)))

(defun replace-range (path start end lines)
  "Replace lines START through END of PATH with LINES; return the new line count."
  (let* ((content (uiop:read-file-lines path))
         (count (length content)))
    (unless (<= 1 start end count)
      (error "Range ~D-~D is outside 1..~D in ~A." start end count path))
    (when (and (member (pathname-type (pathname path)) '("lisp" "asd" "lsp" "cl") :test #'equalp)
               (null (form-count-or-nil (join-lines (coerce (replaced-range (split-lines (uiop/stream:read-file-string path)) start end lines) 'list)))))
      (error "Lines ~D-~D of ~A would leave it unreadable; nothing written." start end path))
    (write-lines path (replaced-range content start end lines))))

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
(defun slice-lines (path from to &key number)
  "Return lines FROM..TO of PATH, 1-based and inclusive, clamped to the file.
With NUMBER, prefix each line with its line number."
  (let* ((lines (coerce (uiop:read-file-lines path) 'vector))
         (count (length lines))
         (start (max 1 (min from count)))
         (end   (max start (min to count))))
    (with-output-to-string (out)
      (when (plusp count)
        (loop for index from start to end
              do (when number (format out "~D: " index))
                 (write-string (aref lines (1- index)) out)
                 (write-char #\Newline out))))))

(defun grep-lines (path needle)
  "Return (LINE-NUMBER . TEXT) for each line of PATH containing NEEDLE."
  (loop for line in (uiop:read-file-lines path)
        for index from 1
        when (search needle line)
          collect (cons index line)))



(defun edit-file (path pairs)
  "Replace each (OLD NEW) of PAIRS in PATH, each OLD occurring exactly once.

Nothing is written unless every OLD is unique, every NEW reads as complete
top-level forms once its delimiter balance matches its OLD's, and the result
holds as many top-level forms as the original plus the deltas the replacements
themselves imply, so callers never count forms.  A NEW whose delimiter balance
differs from its OLD's is adjusted at its end and reported as :REPAIRED."
    (labels ((paren-balance (text)
             "Return TEXT's net count of unclosed open delimiters."
             (let ((index 0) (limit (length text)) (balance 0))
               (flet ((skip-string ()
                        (incf index)
                        (loop while (< index limit)
                              for character = (char text index)
                              do (incf index) (cond
                                               ((char= character #\\)
                                                (incf index))
                                               ((char= character #\")
                                                (return)))))
                      (skip-block-comment ()
                        (incf index 2)
                        (loop while (< index limit)
                              do (if (and (char= (char text index) #\|)
                                          (< (1+ index) limit)
                                          (char= (char text (1+ index)) #\#))
                                     (progn (incf index 2) (return))
                                     (incf index)))))
                 (loop while (< index limit)
                       for character = (char text index)
                       do (cond
                           ((char= character #\;)
                            (setf index
                                    (or (position #\Newline text :start index)
                                        limit)))
                           ((char= character #\") (skip-string))
                           ((and (char= character #\#) (< (1+ index) limit)
                                 (char= (char text (1+ index)) #\|))
                            (skip-block-comment))
                           ((and (char= character #\#) (< (1+ index) limit)
                                 (char= (char text (1+ index)) #\\))
                            (incf index 2))
                           ((char= character #\() (incf balance) (incf index))
                           ((char= character #\)) (decf balance) (incf index))
                           (t (incf index)))))
               balance))
           (adjust-parens (text adjustment)
             "Return TEXT with ADJUSTMENT closing delimiters added or removed at its end, or NIL."
             (let ((body (string-right-trim '(#\Newline #\  #\Tab) text)))
               (cond
                ((plusp adjustment)
                 (concatenate 'string body
                              (make-string adjustment :initial-element #\))
                              (string #\Newline)))
                ((minusp adjustment)
                 (let ((end (length body)))
                   (loop repeat (- adjustment)
                         while (and (plusp end)
                                    (char= (char body (1- end)) #\)))
                         do (decf end))
                   (when (= end (- (length body) (- adjustment)))
                     (concatenate 'string (subseq body 0 end)
                                  (string #\Newline)))))
                (t text)))))
    (let* ((text (uiop/stream:read-file-string path))
           (original (form-count text))
           (repaired 0)
           (delta 0)
           (failures 'nil))
      (dolist (pair pairs)
        (destructuring-bind
            (old new)
            pair
          (let ((hits (count-substring old text)))
            (if (/= hits 1)
                (push (list :hits hits :needle (brief-text old)) failures)
                (let* ((old-forms (form-count-or-nil old))
                       (adjustment (- (paren-balance new) (paren-balance old)))
                       (fixed
                        (if (zerop adjustment)
                            new
                            (adjust-parens new adjustment)))
                       (new-forms (and fixed (form-count-or-nil fixed))))
                  (cond
                   ((null old-forms)
                    (push (list :incomplete-needle :needle (brief-text old))
                          failures))
                   ((null fixed)
                    (push
                     (list :unbalanced-replacement :adjustment adjustment
                           :needle (brief-text new))
                     failures))
                   ((null new-forms)
                    (push
                     (list :incomplete-replacement :needle (brief-text new))
                     failures))
                   (t (setf text (replace-all-text text old fixed))
                    (incf repaired adjustment)
                    (incf delta (- new-forms old-forms)))))))))
      (cond (failures (list :applied 0 :failures (nreverse failures)))
            ((eql (form-count-or-nil text) (+ original delta))
             (with-open-file
                 (out path :direction :output :if-exists :supersede)
               (write-string text out))
             (list :applied (length pairs) :failures nil :repaired repaired))
            (t
             (list :applied 0 :failures
                   (list
                    (list :unbalanced (form-count-or-nil text) :expected
                          (+ original delta)))))))))

(defun form-count-or-nil (text)
  "Return how many top-level forms TEXT holds, or NIL when they do not read."
  (handler-case (form-count text) (error () nil)))

(defun brief-text (text)
  "Return at most TEXT's first 60 characters, for a short report."
  (subseq text 0 (min 60 (length text))))
(defun form-count (text)
  "Return how many top-level forms TEXT holds; signals on unbalanced input."
  (with-input-from-string (in text)
    (let ((*read-eval* nil))
      (loop for form = (read in nil :eof)
            until (eq form :eof)
            count 1))))

(defun edit-file-lines (path pairs)
  "Replace each (OLD-LINES NEW-LINES) of PAIRS in PATH, each OLD occurring exactly once.
OLD-LINES and NEW-LINES are lists of lines joined with newlines, so multi-line
edits need no escape characters. Write nothing unless every OLD is unique."
  (edit-file path (mapcar (lambda (pair)
                            (list (join-lines (first pair))
                                  (join-lines (second pair))))
                          pairs)))


(defun form-printed-lines (form)
  (split-lines
   (string-right-trim '(#\Newline)
                      (with-output-to-string (stream)
                        (let ((*print-pretty* t)
                              (*print-case* :downcase)
                              (*print-right-margin* 100))
                          (pprint form stream))))))

(defun insert-form-file (path after form)
  (let* ((lines (split-lines (uiop/stream:read-file-string path)))
         (index (multiple-value-bind (first second)
                    (form-line-range path after)
                  (if (consp first) (cadr first) second)))
         (printed (append (form-printed-lines form) (list ""))))
    (unless index (error "Form ~A not found in ~A." after path))
    (replace-range path index index
                  (append (list (nth (1- index) lines)) printed))))

(defun replace-form-file (path name form)
  (multiple-value-bind (first second) (form-line-range path name)
    (let ((start (if (consp first) (car first) first))
          (end (if (consp first) (cadr first) second)))
      (unless (and start end) (error "Form ~A not found in ~A." name path))
      (replace-range path start end (form-printed-lines form)))))

(defun line-range (file start end)
  "Return FILE lines START through END with their numbers."
  (with-open-file (stream file)
    (loop for number from 1
          for line = (read-line stream nil)
          while line
          when (<= start number end)
            collect (format nil "~D| ~A" number line))))
