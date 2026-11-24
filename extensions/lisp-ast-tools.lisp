;;;; -- Lisp AST Tools --

;;; Read, search and edit Common Lisp and Hy source files as top-level forms
;;; instead of line windows. The scanner is a self-contained character-level
;;; reader that never interns or evaluates a form; it only locates balanced
;;; form spans so an edit can replace, insert before, or insert after one
;;; top-level form while the rest of the file stays byte for byte identical.

(in-package #:autolith)

(defvar *lisp-ast-maximum-file-bytes* (* 4 1024 1024)
  "Maximum UTF-8 bytes one lisp-ast tool call reads from a file.")

(defvar *lisp-ast-source-limit* 4000
  "Maximum characters of one form's source text reported in a result.")
(defvar *lisp-ast-result-budget* 5000
  "Maximum characters of form rows one read-file call reports.")

(defvar *lisp-ast-default-form-count* 200
  "Default maximum number of top-level forms read-file reports.")

(defvar *lisp-ast-candidate-limit* 32
  "Maximum number of candidate forms listed in a failure message.")


(defvar *lisp-ast-dialect* :common-lisp
  "Reader dialect the scanner applies: :COMMON-LISP or :HY.")

(defun lisp-ast--dialect-for-path (path)
  "Return the reader dialect for PATH's source type."
  (if (string-equal (pathname-type (pathname path)) "hy")
      :hy
      :common-lisp))

(defun lisp-ast--hy-p ()
  "Return T when the scanner is reading Hy source."
  (eq *lisp-ast-dialect* :hy))

(-> lisp-ast--hy-opener-p (character) boolean)
(defun lisp-ast--hy-opener-p (character)
  "Return T when CHARACTER opens a Hy-only collection form."
  (and (lisp-ast--hy-p) (not (null (find character "[{" :test #'char=)))))

(-> lisp-ast--hy-closer-p (character) boolean)
(defun lisp-ast--hy-closer-p (character)
  "Return T when CHARACTER closes a Hy-only collection form."
  (and (lisp-ast--hy-p) (not (null (find character "]}" :test #'char=)))))

(eval-when (:load-toplevel :execute)
  (pushnew "hy" parenchek:*lisp-source-balance-clojure-pathname-types*
           :test #'string=))

;;; -- Scanner --
(defmacro lisp-ast--defscan (name lambda-list &key doc with definitions call)
  "Define a character scanner with LIMIT and optional WITH bindings.
DEFINITIONS supplies the local recursive functions; CALL starts the scan."
  `(defun ,name ,lambda-list
     ,@(when doc (list doc))
     (let* ((limit (length ,(first lambda-list))) ,@with)
       (labels ,definitions
         ,call))))

(-> lisp-ast--whitespace-p (character) boolean)
(defun lisp-ast--whitespace-p (character)
  "Return T when CHARACTER is whitespace the scanner skips between elements."
  (and (find character #(#\space #\tab #\newline #\return #\page) :test #'char=)
       t))

(-> lisp-ast--skip-line-comment ((vector character) (integer 0)) (integer 0))
(defun lisp-ast--skip-line-comment (text index)
  "Return the index of the newline closing the comment starting at INDEX."
  (or (position #\newline text :start index) (length text)))

(-> lisp-ast--skip-block-comment ((vector character) (integer 0)) (integer 0))
(lisp-ast--defscan lisp-ast--skip-block-comment (text index)
  :doc "Skip one nested #| block comment; INDEX points at its #."
  :definitions
      ((scan (i depth)
         (cond
           ((>= i limit) limit)
           ((and (< (1+ i) limit)
                 (char= (char text i) #\|)
                 (char= (char text (1+ i)) #\#))
            (if (= depth 1)
                i
                (scan (+ i 2) (1- depth))))
           ((and (< (1+ i) limit)
                 (char= (char text i) #\#)
                 (char= (char text (1+ i)) #\|))
            (scan (+ i 2) (1+ depth)))
           (t (scan (1+ i) depth)))))
  :call (scan (+ index 2) 1))

(-> lisp-ast--skip-atmosphere ((vector character) (integer 0)) (integer 0))
(lisp-ast--defscan lisp-ast--skip-atmosphere (text index)
  :doc "Return the next non-whitespace, non-comment index at or after INDEX."
  :definitions
      ((scan (i)
         (cond
           ((>= i limit) i)
           ((lisp-ast--whitespace-p (char text i)) (scan (1+ i)))
           ((char= (char text i) #\;) (scan (lisp-ast--skip-line-comment text i)))
           ((and (< (1+ i) limit)
                 (char= (char text i) #\#)
                 (char= (char text (1+ i)) #\|))
            (scan (lisp-ast--skip-block-comment text i)))
           (t i))))
  :call (scan index))

  (-> lisp-ast--triple-quote-p ((vector character) (integer 0)) boolean)
  (defun lisp-ast--triple-quote-p (text index)
    "Return T when INDEX starts a Hy triple quote."
    (and (< (+ index 2) (length text))
         (char= (char text (1+ index)) #\")
         (char= (char text (+ index 2)) #\")))

(-> lisp-ast--quoted-end ((vector character) (integer 0) boolean boolean)
    (values (integer 0) boolean))
(lisp-ast--defscan lisp-ast--quoted-end (text index formatted-p triple-p)
  :doc "Return the end of the string whose quote is at INDEX."
  :definitions
      ((quoted (i depth)
         (cond
           ((>= i limit) (values limit nil))
           ((and (< (+ i 2) limit)
                 (char= (char text i) #\")
                 (char= (char text (1+ i)) #\")
                 (char= (char text (+ i 2)) #\"))
            (if (and triple-p (zerop depth))
                (values (+ i 3) t)
                (quoted (+ i 3) depth)))
           ((char= (char text i) #\\) (quoted (min limit (+ i 2)) depth))
           (formatted-p
            (cond
              ((char= (char text i) #\{) (field (1+ i) (1+ depth)))
              ((char= (char text i) #\}) (quoted (1+ i) depth))
              ((char= (char text i) #\") (values (1+ i) t))
              (t (quoted (1+ i) depth))))
           ((char= (char text i) #\") (values (1+ i) t))
           (t (quoted (1+ i) depth))))
       (field (i depth)
         (cond
           ((>= i limit) (values limit nil))
           ((char= (char text i) #\\) (field (min limit (+ i 2)) depth))
           ((char= (char text i) #\;) (field (lisp-ast--skip-line-comment text i) depth))
           ((char= (char text i) #\{) (field (1+ i) (1+ depth)))
           ((char= (char text i) #\})
            (if (= depth 1)
                (quoted (1+ i) 0)
                (field (1+ i) (1- depth))))
           ((char= (char text i) #\")
            (multiple-value-bind (end complete-p)
                (lisp-ast--quoted-end text i nil nil)
              (if complete-p (field end depth) (values end nil))))
           (t
            (multiple-value-bind (quote inner-formatted-p)
                (lisp-ast--string-prefix-end text i)
              (if quote
                  (multiple-value-bind (end complete-p)
                      (lisp-ast--quoted-end text quote inner-formatted-p
                                            (lisp-ast--triple-quote-p text quote))
                    (if complete-p (field end depth) (values end nil)))
                  (field (1+ i) depth)))))))
  :call (quoted (+ index (if triple-p 3 1)) 0))

  (-> lisp-ast--skip-string ((vector character) (integer 0)) (values (integer 0) boolean))
  (defun lisp-ast--skip-string (text index)
    "Return the index past the string starting at INDEX."
    (lisp-ast--quoted-end text index nil (lisp-ast--triple-quote-p text index)))

  (-> lisp-ast--string-prefix-end ((vector character) (integer 0))
      (values (option (integer 0)) boolean))
  (defun lisp-ast--string-prefix-end (text index)
    "Return the quote index and FORMATTED-P for a Hy string prefix at INDEX."
    (let ((limit (length text))
          (i index)
          (formatted-p nil))
      (when (and (< i limit) (find (char text i) "rbft" :test #'char=))
        (setf formatted-p (not (null (find (char text i) "ft" :test #'char=))))
        (incf i)
        (when (and (< i limit) (find (char text i) "rbft" :test #'char=))
          (setf formatted-p
                (or formatted-p (not (null (find (char text i) "ft" :test #'char=)))))
          (incf i))
        (when (and (< i limit) (char= (char text i) #\"))
          (values i formatted-p)))))

  (-> lisp-ast--skip-prefixed-string
      ((vector character) (integer 0) (integer 0) boolean) (values (integer 0) boolean))
  (defun lisp-ast--skip-prefixed-string (text index quote-index formatted-p)
    "Skip the prefixed Hy string whose prefix starts at INDEX."
    (declare (ignore index))
    (lisp-ast--quoted-end text quote-index formatted-p
                          (lisp-ast--triple-quote-p text quote-index)))

  (-> lisp-ast--prefixed-string-end ((vector character) (integer 0))
      (values (option (integer 0)) boolean))
  (defun lisp-ast--prefixed-string-end (text index)
    "Return the end of a Hy prefixed string starting at INDEX, if any."
    (when (lisp-ast--hy-p)
      (multiple-value-bind (quote formatted-p)
          (lisp-ast--string-prefix-end text index)
        (when quote
          (lisp-ast--skip-prefixed-string text index quote formatted-p)))))

(-> lisp-ast--skip-character-literal ((vector character) (integer 0)) (values (integer 0) boolean))
(lisp-ast--defscan lisp-ast--skip-character-literal (text index)
  :doc "Skip one #\\ character literal; INDEX points at its #."
  :with ((i (+ index 2)))
  :definitions
      ((scan (j)
         (if (and (< j limit) (alphanumericp (char text j)))
             (scan (1+ j))
             j)))
  :call (cond
          ((>= i limit) (values limit nil))
          ((alphanumericp (char text i)) (values (scan i) t))
          (t (values (min limit (1+ i)) t))))

(-> lisp-ast--token-end ((vector character) (integer 0)) (values (integer 0) boolean))
(lisp-ast--defscan lisp-ast--token-end (text index)
  :doc "Return the end of the token starting at INDEX."
  :with ((delimiters (if (lisp-ast--hy-p)
                           "()[]{}\";'`~,"
                           "()\";'`,`")))
  :definitions
      ((scan (i complete-p)
         (if (>= i limit)
             (values i complete-p)
             (let ((c (char text i)))
               (cond
                 ((char= c #\\)
                  (if (<= (+ i 2) limit)
                      (scan (+ i 2) complete-p)
                      (values limit nil)))
                 ((char= c #\|)
                  (let ((close (position #\| text :start (1+ i))))
                    (if close
                        (scan (1+ close) complete-p)
                        (values limit nil))))
                 ((or (lisp-ast--whitespace-p c)
                      (find c delimiters :test #'char=))
                  (values i complete-p))
                 (t (scan (1+ i) complete-p)))))))
  :call (scan index t))

  (defun lisp-ast--closing-delimiter (character)
    "Return the delimiter closing an opener CHARACTER."
    (case character
      (#\( #\))
      (#\[ #\])
      (#\{ #\})
      (otherwise #\))))

(-> lisp-ast--form-end ((vector character) (integer 0)) (values (integer 0) boolean))
(lisp-ast--defscan lisp-ast--form-end (text index)
  :doc "Return the index just past the balanced form starting at INDEX."
  :with ((closer (lisp-ast--closing-delimiter (char text index))))
  :definitions
      ((scan (i stack)
         (let ((i (lisp-ast--skip-atmosphere text i)))
           (cond
             ((>= i limit) (values limit nil))
             ((or (char= (char text i) #\()
                  (lisp-ast--hy-opener-p (char text i)))
              (scan (1+ i)
                    (cons (lisp-ast--closing-delimiter (char text i)) stack)))
             ((or (char= (char text i) #\))
                  (lisp-ast--hy-closer-p (char text i)))
              (if (char= (char text i) (car stack))
                  (if (null (cdr stack))
                      (values (1+ i) t)
                      (scan (1+ i) (cdr stack)))
                  (values i nil)))
             (t
              (multiple-value-bind (end complete-p)
                  (lisp-ast--element-end text i)
                (if (and complete-p (> end i))
                    (scan end stack)
                    (values end complete-p))))))))
  :call (scan (1+ index) (list closer)))

(-> lisp-ast--element-end ((vector character) (integer 0)) (values (integer 0) boolean))
(lisp-ast--defscan lisp-ast--element-end (text index)
  :doc "Return the end of one balanced form element starting at INDEX."
  :definitions
      ((tag-char-p (c)
         (or (alphanumericp c) (find c "_!?-+*" :test #'char=)))
       (scan-tag (i)
         (if (and (< i limit) (tag-char-p (char text i)))
             (scan-tag (1+ i))
             i))
       (scan-modifier (i)
         (if (and (< i limit)
                  (or (digit-char-p (char text i))
                      (find (char text i) "+-.:\"" :test #'char=)))
             (scan-modifier (1+ i))
             i))
       (sharp (i)
         (let ((next (1+ i)))
           (cond
             ((>= next limit) (values limit nil))
             ((char= (char text next) #\\)
              (lisp-ast--skip-character-literal text i))
             ((char= (char text next) #\|)
              (values (lisp-ast--skip-block-comment text i) t))
             ((char= (char text next) #\') (values (min limit (+ i 2)) t))
             ((find (char text next) "+-" :test #'char=)
              (let ((first (lisp-ast--skip-atmosphere text (+ i 2))))
                (if (>= first limit)
                    (values limit nil)
                    (multiple-value-bind (first-end first-ok) (scan first)
                      (if first-ok
                          (let ((second (lisp-ast--skip-atmosphere text first-end)))
                            (if (>= second limit)
                                (values limit nil)
                                (scan second)))
                          (values first-end nil))))))
             ((and (lisp-ast--hy-p) (tag-char-p (char text next)))
              (scan (scan-tag next)))
             (t (scan (scan-modifier next))))))
       (scan (i)
         (cond
           ((>= i limit) (values limit nil))
           (t
            (let ((c (char text i)))
              (cond
                ((and (lisp-ast--hy-p)
                      (lisp-ast--string-prefix-end text i))
                 (multiple-value-bind (quote formatted-p)
                     (lisp-ast--string-prefix-end text i)
                   (lisp-ast--skip-prefixed-string text i quote formatted-p)))
                ((or (char= c #\() (lisp-ast--hy-opener-p c))
                 (lisp-ast--form-end text i))
                ((char= c #\") (lisp-ast--skip-string text i))
                ((char= c #\;) (values (lisp-ast--skip-line-comment text i) t))
                ((char= c #\#) (sharp i))
                ((find c "'`," :test #'char=)
                 (scan (if (and (< (1+ i) limit)
                                (char= (char text (1+ i)) #\@))
                           (+ i 2)
                           (1+ i))))
                (t (lisp-ast--token-end text i))))))))
  :call (scan index))


;;; -- Located Forms --

(defstruct (lisp-ast-form
            (:constructor lisp-ast--make-form
                          (&key start end head name truncated-p)))
  "One located top-level source form."
  (start 0 :type (integer 0) :read-only t)
  (end 0 :type (integer 0) :read-only t)
  (head nil :type (option string) :read-only t)
  (name nil :type (option string) :read-only t)
  (truncated-p nil :type boolean :read-only t))

(-> lisp-ast--clean-name (string) string)
(defun lisp-ast--clean-name (token)
  "Strip reader and package prefixes from a name TOKEN."
  (let* ((text (string-left-trim "#'" token))
         (colon (position #\: text :from-end t)))
    (subseq text (if colon (1+ colon) 0))))

(defun lisp-ast--list-form-start (text start end)
  "Return the index of the list form that reader tags or conditionals guard.

  NIL means START is neither a list form nor a chain of prefixes ending in one."
  (let ((i start)
        (limit (min end (length text))))
    (loop
      (setf i (lisp-ast--skip-atmosphere text i))
      (when (>= i limit)
        (return nil))
      (let ((c (char text i)))
        (cond
          ((or (char= c #\()
                 (lisp-ast--hy-opener-p c))
           (return i))
          ((and (char= c #\#)
                (< (1+ i) limit)
                (find (char text (1+ i)) "+-" :test #'char=))
           (setf i (lisp-ast--skip-atmosphere text (+ i 2)))
           (when (>= i limit)
             (return nil))
           (setf i (lisp-ast--element-end text i)))
          ((and (lisp-ast--hy-p)
                (char= c #\#)
                (< (1+ i) limit)
                (or (alphanumericp (char text (1+ i)))
                    (find (char text (1+ i)) "_!?-+*" :test #'char=)))
           (incf i)
           (loop while (and (< i limit)
                            (or (alphanumericp (char text i))
                                (find (char text i) "_!?-+*" :test #'char=)))
                 do (incf i)))
          (t
           (return nil)))))))

(-> lisp-ast--form-head ((vector character) (integer 0) (integer 0)) (option string))
(defun lisp-ast--form-head (text start end)
  "Return the head symbol text of the list form spanning [START,END), or NIL.

A form guarded by #+/#- reader conditionals reports the head of the guarded
list form."
  (let ((list-start (lisp-ast--list-form-start text start end)))
    (when list-start
      (let ((i (lisp-ast--skip-atmosphere text (1+ list-start))))
        (when (< i end)
          (let ((c (char text i)))
            (when (and (char/= c #\")
                       (char/= c #\|)
                       (char/= c #\')
                       (char/= c #\#))
              (let ((token-end (min end (lisp-ast--token-end text i))))
                (when (> token-end i)
                  (subseq text i token-end))))))))))

(-> lisp-ast--form-name ((vector character) (integer 0) (integer 0)) (option string))
(defun lisp-ast--form-name (text start end)
  "Return the named subject of the list form spanning [START,END), or NIL.

The name is the second element; a second element that is itself a list
contributes its head, except for (setf ...) names, which contribute their
second element. A form guarded by #+/#- reader conditionals reports the name
of the guarded list form."
  (let ((list-start (lisp-ast--list-form-start text start end)))
    (when list-start
      (let ((i (lisp-ast--skip-atmosphere text (1+ list-start))))
        (when (< i end)
          (setf i (lisp-ast--element-end text i))
          (setf i (lisp-ast--skip-atmosphere text i))
          (when (< i end)
            (cond
                  ((or (char= (char text i) #\()
                       (lisp-ast--hy-opener-p (char text i)))
               (let* ((inner-end (lisp-ast--form-end text i))
                      (inner-head (lisp-ast--form-head text i inner-end)))
                 (cond
                   ((and inner-head (string-equal inner-head "setf"))
                    (lisp-ast--form-name text i inner-end))
                   (inner-head
                    (lisp-ast--clean-name inner-head)))))
              (t
               (let ((name-end (min end (lisp-ast--token-end text i))))
                 (when (> name-end i)
                   (lisp-ast--clean-name (subseq text i name-end))))))))))))

(-> lisp-ast--line-starts (string) simple-vector)
(defun lisp-ast--line-starts (text)
  "Return the vector of line start offsets in TEXT, line one at index zero."
  (let ((starts (list 0)))
    (loop for i from 0 below (length text)
          when (char= (char text i) #\newline)
            do (push (1+ i) starts))
    (coerce (nreverse starts) 'vector)))

(-> lisp-ast--line-of (simple-vector (integer 0)) (integer 1))
(defun lisp-ast--line-of (starts offset)
  "Return the one-based line holding OFFSET."
  (let ((lo 0)
        (hi (1- (length starts))))
    (loop while (< lo hi)
          do (let ((middle (ash (+ lo hi 1) -1)))
               (if (<= (aref starts middle) offset)
                   (setf lo middle)
                   (setf hi (1- middle)))))
    (1+ lo)))

(-> lisp-ast--top-level-forms (string) (values list (option (integer 0))))
(defun lisp-ast--top-level-forms (text)
  "Return the located top-level forms of TEXT in source order.

The second value is the character index where scanning stopped because no
element starts there (a stray close paren), or NIL when TEXT was scanned
to its end."
  (lisp-ast--forms-in text 0 (length text)))
(defun lisp-ast--bounded-source (text start end)
  "Return [START,END) of TEXT, clipped to *lisp-ast-source-limit*."
  (let* ((source (subseq text start end))
         (length (length source)))
    (if (> length *lisp-ast-source-limit*)
        (concatenate 'string (subseq source 0 *lisp-ast-source-limit*) "...")
        source)))

(-> lisp-ast--line-ending (string) string)
(defun lisp-ast--line-ending (text)
  "Return the dominant line ending of TEXT."
  (let ((crlf 0)
        (line-feed 0))
    (loop for index from 0 below (length text)
          when (char= (char text index) #\newline)
            do (if (and (plusp index)
                        (char= (char text (1- index)) #\return))
                   (incf crlf)
                   (incf line-feed)))
    (if (> crlf line-feed)
        (coerce #(#\return #\newline) 'string)
        (coerce #(#\newline) 'string))))

(-> lisp-ast--single-form-p (string) boolean)
(defun lisp-ast--single-form-p (new-form)
  "Return T when NEW-FORM holds exactly one complete top-level form."
  (let ((forms (lisp-ast--top-level-forms new-form)))
    (and (= (length forms) 1)
         (= (lisp-ast--skip-atmosphere new-form (lisp-ast-form-end (first forms)))
            (length new-form))
         (not (lisp-ast-form-truncated-p (first forms)))
         t)))

(-> lisp-ast--form-row (simple-vector lisp-ast-form (integer 1)) json-object)
(defun lisp-ast--form-row (starts form index)
  "Return the JSON row describing FORM at INDEX."
  (json-object
   "index" index
   "type" (or (lisp-ast-form-head form) "")
   "name" (or (lisp-ast-form-name form) "")
   "start-line" (lisp-ast--line-of starts (lisp-ast-form-start form))
   "end-line" (lisp-ast--line-of starts (1- (lisp-ast-form-end form)))
   "start-char" (lisp-ast-form-start form)
   "end-char" (lisp-ast-form-end form)))


;;; -- Read Execution --

(-> lisp-ast--read-matches-p
    ((vector character) lisp-ast-form (option string) (option string)) boolean)
(defun lisp-ast--read-matches-p (text form name-pattern content-pattern)
  "Return T when FORM passes READ-FILE's optional filters."
  (and (or (null name-pattern)
           (and (lisp-ast-form-name form)
                (search name-pattern (lisp-ast-form-name form) :test #'char-equal)))
       (or (null content-pattern)
           (search content-pattern text
                   :start2 (lisp-ast-form-start form)
                   :end2 (lisp-ast-form-end form)
                   :test #'char-equal))
       t))

(-> lisp-ast--page-notice ((integer 1) integer integer (option integer)) string)
(defun lisp-ast--page-notice (start shown total next-index)
  "Describe a page that already holds every form the caller asked for."
  (if (null next-index)
      (format nil "complete: all ~D matched forms." total)
      (format nil "complete: forms ~D-~D of ~D; pass start-index ~D for the next range."
              start (+ start shown -1) total next-index)))

(-> lisp-ast--check-page-fits (list (option integer) integer integer) t)
(defun lisp-ast--check-page-fits (shown next-index maximum total)
  "Signal TOOL-ERROR rather than return fewer forms than the caller asked for.

A budget-shortened page reads like a complete one, so refuse it and name the
largest page that fits."
  (when (and next-index (< (length shown) maximum))
    (error 'tool-error
           :message (if (null shown)
                        (format nil "The next matched form alone exceeds one response and ~D forms remain. Narrow the search with name-pattern or content-pattern."
                                total)
                        (format nil "One response holds ~D of the ~D forms requested and ~D of ~D matched forms remain. Pass max-forms ~D to receive that complete range."
                                (length shown) maximum (- total (length shown)) total (length shown)))
           :code      ':page-exceeds-response
           :tool-name "lisp-ast.read-file")))

(defun lisp-ast--read-rows (starts matches start maximum)
  "Return (VALUES ROWS NEXT-INDEX) for MATCHES from START.

ROWS stops at MAXIMUM rows or when the next row would push the encoded page
past *LISP-AST-RESULT-BUDGET* characters. NEXT-INDEX is the index of the
first matched form left out, or NIL when the page reached the end."
  (let ((rows nil)
        (size 0)
        (index start)
        (remaining (nthcdr (1- start) matches)))
    (loop while (and remaining (< (length rows) maximum))
          do (let* ((row (lisp-ast--form-row starts (first remaining) index))
                    (row-size (length (argo:json-encode row))))
               (when (and rows (> (+ size row-size) *lisp-ast-result-budget*))
                 (return))
               (push row rows)
               (incf size row-size)
               (incf index)
               (setf remaining (rest remaining))))
    (values (nreverse rows)
            (when remaining index))))

(-> lisp-ast--check-start-index ((integer 1) list) t)
(defun lisp-ast--check-start-index (start matches)
  "Signal TOOL-ERROR when START lies past the forms in MATCHES.

An index past the last match used to return an empty page that looked like
\"nothing matched\"; naming the range makes a wrong NEXT-INDEX obvious."
  (when (> start (max 1 (length matches)))
    (error 'tool-error
           :message
           (format nil "start-index ~D is past the ~D matched forms; use 1 through ~D."
                   start (length matches) (max 1 (length matches)))
           :code      ':start-index-out-of-range
           :tool-name "lisp-ast.read-file")))

(-> lisp-ast--read-range (json-object) cons)
(defun lisp-ast--read-range (arguments)
  "Return the caller's (START . MAXIMUM) range, both required.

One response holds only a bounded range, so the caller names it. A missing bound
is an error rather than a silent default that the reader mistakes for the whole
file."
  (let ((start (workspace-tool-integer-argument arguments "start-index" :fallback nil))
        (maximum (workspace-tool-integer-argument arguments "max-forms" :fallback nil)))
    (unless (and start maximum)
      (error 'tool-error
             :message "lisp-ast.read-file needs start-index and max-forms: one response holds a bounded range, so name the range you want."
             :code      ':range-required
             :tool-name "lisp-ast.read-file"))
    (unless (and (plusp start) (plusp maximum))
      (error 'tool-error
             :message (format nil "start-index and max-forms must be at least 1; got ~D and ~D." start maximum)
             :code      ':range-invalid
             :tool-name "lisp-ast.read-file"))
    (cons start maximum)))

(-> lisp-ast--execute-read (tool-context json-object) json-object)
(defun lisp-ast--execute-read (context arguments)
  "Answer READ-FILE's arguments with one bounded page of top-level forms.

START-INDEX selects the first matched form. NEXT-INDEX names the form after
the last reported one, or NIL when the page reached the end; the page is also
cut at *LISP-AST-RESULT-BUDGET* characters, so one call never returns a whole
large file. PARSE-ERROR reports where top-level scanning stopped because no
element starts at that character. FORMS-TRUNCATED reports that TEXT ended inside a form. PAGING names whether the page is complete or the next start-index."
  (let* ((path (tool-argument arguments "path" :required t))
         (resolved (workspace-tool-path context path :tool-name "lisp-ast.read-file"))
         (content (workspace-file--read-content
                   resolved
                   :maximum-bytes *lisp-ast-maximum-file-bytes*
                   :tool-name "lisp-ast.read-file"
                   :description "Lisp AST source snapshot"))
         (starts (lisp-ast--line-starts content)))
    (multiple-value-bind (forms stop) (lisp-ast--top-level-forms content)
      (let* ((name-pattern (tool-argument arguments "name-pattern"))
             (content-pattern (tool-argument arguments "content-pattern"))
             (range (lisp-ast--read-range arguments))
             (start (car range))
             (maximum (cdr range))
             (matches (remove-if-not
                       (lambda (form)
                         (lisp-ast--read-matches-p content form name-pattern content-pattern))
                       forms)))
        (lisp-ast--check-start-index start matches)
        (multiple-value-bind (shown next-index)
            (lisp-ast--read-rows starts matches start maximum)
           (lisp-ast--check-page-fits shown next-index maximum (length matches))
           (json-object
           "paging" (lisp-ast--page-notice start (length shown) (length matches) next-index)
           "path" path
           "form-count" (length forms)
           "matched-count" (length matches)
           "start-index" start
           "next-index" next-index
           "forms-truncated" (not (null (find-if #'lisp-ast-form-truncated-p forms)))
           "parse-error" (when stop
                           (json-object
                            "line" (lisp-ast--line-of starts stop)
                            "char" stop
                            "character" (string (char content stop))))
           "forms" (apply #'json-array shown)))))))



;;; -- Edit Execution --

(-> lisp-ast--check-operation ((option string)) t)
(defun lisp-ast--check-operation (operation)
  "Signal TOOL-ERROR unless OPERATION is one of the supported operations."
  (unless (member operation '("replace" "insert-before" "insert-after") :test #'equal)
    (error 'tool-error
           :message
           (format nil "Unknown lisp-ast.edit-form operation ~S; use replace, insert-before, or insert-after."
                   operation)
           :code      ':invalid-operation
           :tool-name "lisp-ast.edit-form")))

(-> lisp-ast--check-new-form (string) t)
(defun lisp-ast--check-new-form (new-form)
  "Signal TOOL-ERROR unless NEW-FORM is exactly one complete form.

The message names why: no top-level form, a second form, a form that never
closes, or text after the single form."
  (unless (lisp-ast--single-form-p new-form)
    (multiple-value-bind (forms stop) (lisp-ast--top-level-forms new-form)
      (error 'tool-error
             :message
             (cond
               ((null forms)
                (format nil "new-form holds no top-level form: it is empty, unbalanced, or only comments. Scanning stopped at char ~D of ~D."
                        (or stop 0) (length new-form)))
               ((rest forms)
                (format nil "new-form holds ~D top-level forms; the second starts at char ~D. Pass exactly one form."
                        (length forms) (lisp-ast-form-start (second forms))))
               ((lisp-ast-form-truncated-p (first forms))
                (format nil "new-form ends inside the form starting at char ~D; close every parenthesis."
                        (lisp-ast-form-start (first forms))))
               (t
                (format nil "new-form has text after its single form, starting at char ~D."
                        (lisp-ast--skip-atmosphere new-form
                                                   (lisp-ast-form-end (first forms))))))
             :code      ':invalid-form
             :tool-name "lisp-ast.edit-form"))))


(-> lisp-ast--form-matches-p (lisp-ast-form string string) boolean)
(defun lisp-ast--form-matches-p (form form-type form-name)
  "Return T when FORM's head matches TYPE and NAME matches or was omitted."
  (and (lisp-ast-form-head form)
       (string-equal (lisp-ast-form-head form) form-type)
       (or (null form-name)
           (let ((actual (lisp-ast-form-name form)))
             (and actual (string-equal actual form-name))))
       t))

(-> lisp-ast--candidate-summary (list) string)
(defun lisp-ast--candidate-summary (forms)
  "Return a bounded listing of FORMS for failure messages."
  (with-output-to-string (stream)
    (loop for form in forms
          for index from 1
          repeat *lisp-ast-candidate-limit*
          do (format stream "  ~D ~A ~A [chars ~D..~D]~%"
                     index
                     (or (lisp-ast-form-head form) "?")
                     (or (lisp-ast-form-name form) "?")
                     (lisp-ast-form-start form)
                     (lisp-ast-form-end form)))))

(-> lisp-ast--select-form (list (option integer) string string list) lisp-ast-form)
(defun lisp-ast--select-form (matches form-index form-type form-name forms)
  "Return the one targeted FORM or signal a typed TOOL-ERROR."
  (cond
    ((null matches)
     (error 'tool-error
            :message
            (format nil "No form matches type ~A and name ~A. Candidates:~%~A"
                    form-type form-name (lisp-ast--candidate-summary forms))
            :code      ':not-found
            :tool-name "lisp-ast.edit-form"))
    ((and (> (length matches) 1) (null form-index))
     (error 'tool-error
            :message
            (format nil "~D forms match type ~A and name ~A; pass form-index to choose:~%~A"
                    (length matches) form-type form-name (lisp-ast--candidate-summary matches))
            :code      ':ambiguous
            :tool-name "lisp-ast.edit-form"))
    ((null form-index)
     (first matches))
    ((and (integerp form-index) (>= form-index 1) (<= form-index (length matches)))
     (nth (1- form-index) matches))
    (t
     (error 'tool-error
            :message
            (format nil "form-index ~D is outside the ~D matching forms."
                    form-index (length matches))
            :code      ':not-found
            :tool-name "lisp-ast.edit-form"))))

(-> lisp-ast--splice
    ((vector character) simple-vector lisp-ast-form string string) string)
(defun lisp-ast--splice (content starts form operation new-form)
  "Return CONTENT with OPERATION applied around FORM using NEW-FORM."
  (let ((start (lisp-ast-form-start form))
        (end   (lisp-ast-form-end form))
        (eol   (lisp-ast--line-ending content)))
    (cond
      ((equal operation "replace")
       (concatenate 'string (subseq content 0 start) new-form (subseq content end)))
      ((equal operation "insert-before")
       (let ((line-start (aref starts (1- (lisp-ast--line-of starts start)))))
         (concatenate 'string
                      (subseq content 0 line-start)
                      new-form
                      eol
                      (subseq content line-start))))
      ((equal operation "insert-after")
       (concatenate 'string (subseq content 0 end) eol new-form (subseq content end)))
      (t
       (error 'tool-error
              :message   (format nil "Unknown lisp-ast.edit-form operation ~S." operation)
              :code      ':invalid-operation
              :tool-name "lisp-ast.edit-form")))))

(-> lisp-ast--apply-edit
    (tool-context t t simple-vector lisp-ast-form string string string)
    json-object)
(defun lisp-ast--apply-edit
    (context resource observation starts form path operation new-content)
  "Publish NEW-CONTENT as one revisioned workspace change."
  (let* ((state (resource-observation-state-ensure
                 (tool-context-conversation context) observation
                 :visible-ranges (list (list 1 (length starts)))))
         (change (cl-resources:make-resource-change
                  resource
                  :base-revision (resource-observation-state-alias state)
                  :operations
                  (list (make-instance 'workspace-content-operation
                                       :content new-content
                                       :permission-donor observation)))))
    (workspace-change-set-apply context (list change))
    (json-object
     "written" t
     "path" path
     "operation" operation
     "start-line" (lisp-ast--line-of starts (lisp-ast-form-start form))
     "end-line" (lisp-ast--line-of starts (1- (lisp-ast-form-end form)))
     "new-size" (length new-content))))

(-> lisp-ast--edit-preview
    (string string string string lisp-ast-form (vector character) simple-vector)
    json-object)
(defun lisp-ast--edit-preview (path operation form-type form-name form content starts)
  "Return the dry-run report for one pending edit."
  (json-object
   "dry-run" t
   "path" path
   "operation" operation
   "form-type" form-type
   "form-name" form-name
   "start-line" (lisp-ast--line-of starts (lisp-ast-form-start form))
   "end-line" (lisp-ast--line-of starts (1- (lisp-ast-form-end form)))
   "form-source" (lisp-ast--bounded-source content (lisp-ast-form-start form) (lisp-ast-form-end form))))

(-> lisp-ast--execute-edit (tool-context json-object) json-object)
(defun lisp-ast--edit-depth (arguments)
  "Return how deep EDIT-FORM searches; a negative value means every depth."
  (let ((value (tool-argument arguments "depth")))
    (if (numberp value) (round value) 0)))
(defun lisp-ast--execute-edit (context arguments)
  "Answer EDIT-FORM's arguments by splicing one form at the requested depth."
  (let* ((path (tool-argument arguments "path" :required t))
         (form-type (tool-argument arguments "form-type" :required t))
         (form-name (tool-argument arguments "form-name"))
         (operation (tool-argument arguments "operation" :required t))
         (new-form (tool-argument arguments "new-form" :required t))
         (form-index (workspace-tool-integer-argument
                      arguments "form-index" :fallback nil))
         (depth (lisp-ast--edit-depth arguments))
         (dry-run (tool-boolean-argument arguments "dry-run"
                                         :default nil
                                         :tool-name "lisp-ast.edit-form")))
    (lisp-ast--check-operation operation)
    (lisp-ast--check-new-form new-form)
    (let* ((resolved (workspace-tool-path context path
                                          :tool-name "lisp-ast.edit-form"))
           (uri (workspace-file--canonical-uri context resolved))
           (resource (resource-registry-resolve
                      (tool-registry-resource-registry
                       (tool-context-registry context))
                      uri context))
           (observation (resource-observe resource context))
           (content (resource-observation-content observation)))
      (unless (stringp content)
        (error 'tool-error
               :message (format nil
                                "Workspace file ~A does not exist or is not readable."
                                path)
               :code      ':not-found
               :tool-name "lisp-ast.edit-form"))
      (let* ((starts (lisp-ast--line-starts content))
             (roots (lisp-ast--top-level-forms content))
             (entries (lisp-ast--all-entries content roots
                                             (if (minusp depth) nil depth)))
             (candidates (mapcar #'first entries))
             (matches (remove-if-not
                       (lambda (form)
                         (lisp-ast--form-matches-p form form-type form-name))
                       candidates))
             (form (lisp-ast--select-form matches form-index
                                          form-type form-name candidates))
             (new-content (lisp-ast--splice content starts form
                                            operation new-form)))
        (if dry-run
            (lisp-ast--edit-preview path operation form-type form-name
                                    form content starts)
            (lisp-ast--apply-edit context resource observation starts form
                                  path operation new-content))))))


;;; -- Tool Surface --

(defclass lisp-ast-tool (tool)
  ()
  (:documentation "Base class of the Lisp AST read and edit tools."))

(defclass lisp-ast-search-tool (lisp-ast-tool)
  ()
  (:documentation "Search a Lisp-family source file by form structure."))

(defclass lisp-ast-read-tool (lisp-ast-tool)
  ()
  (:documentation "Report a Lisp-family source file's top-level forms."))

(defclass lisp-ast-edit-tool (lisp-ast-tool)
  ()
  (:documentation "Edit one top-level form of a Lisp-family source file."))

(defmethod tool-execution-policy ((tool lisp-ast-edit-tool))
  "Serialize edits so concurrent reads observe one committed result."
  ':exclusive)

(defmethod tool-execute ((tool lisp-ast-read-tool) (context tool-context) (arguments hash-table))
  "Return READ-FILE's bounded top-level form report."
  (declare (ignore tool))
  (handler-case
      (let ((*lisp-ast-dialect*
              (lisp-ast--dialect-for-path (tool-argument arguments "path" :required t))))
        (tool-success (json-encode (lisp-ast--execute-read context arguments))))
    (tool-authority-denied (condition)
      (tool-failure (princ-to-string condition) :denied-p t))
    (file-error (condition)
      (tool-failure (princ-to-string condition)))))

(defmethod tool-execute ((tool lisp-ast-edit-tool) (context tool-context) (arguments hash-table))
  "Return EDIT-FORM's result after splicing one top-level form."
  (declare (ignore tool))
  (handler-case
      (let ((*lisp-ast-dialect*
              (lisp-ast--dialect-for-path (tool-argument arguments "path" :required t))))
        (tool-success (json-encode (lisp-ast--execute-edit context arguments))))
    (tool-authority-denied (condition)
      (tool-failure (princ-to-string condition) :denied-p t))
    (resource-revision-stale (condition)
      (tool-failure (princ-to-string condition)))))

(-> lisp-ast--read-tool () lisp-ast-tool)
(defun lisp-ast--forms-in (text start end)
  "Return the located forms directly between START and END of TEXT in source order.

The second value is the character index where scanning stopped because no
element starts there (a stray close paren), or NIL when scanning reached END."
  (let ((forms nil)
        (limit end)
        (i start))
    (loop
      (setf i (lisp-ast--skip-atmosphere text i))
      (when (>= i limit)
        (return (values (nreverse forms) nil)))
      (let ((form-start i))
        (multiple-value-bind (form-end complete-p)
            (lisp-ast--element-end text i)
          (when (or (<= form-end form-start) (> form-end limit))
            (return (values (nreverse forms) i)))
          (push (lisp-ast--make-form
                 :start form-start
                 :end form-end
                 :head (lisp-ast--form-head text form-start form-end)
                 :name (lisp-ast--form-name text form-start form-end)
                 :truncated-p (not complete-p))
                forms)
          (setf i form-end))))))

(defun lisp-ast--children (text form)
  "Return the forms directly inside FORM."
  (let ((start (1+ (lisp-ast-form-start form)))
        (end (1- (lisp-ast-form-end form))))
    (when (< start end)
      (lisp-ast--forms-in text start end))))

(defun lisp-ast--search-matches-p (form head name-pattern)
  "True when FORM's head equals HEAD and its name contains NAME-PATTERN."
  (and (or (null head)
           (let ((actual (lisp-ast-form-head form)))
             (and actual (string-equal actual head))))
       (or (null name-pattern)
           (let ((actual (lisp-ast-form-name form)))
             (and actual
                  (search (string-upcase name-pattern)
                          (string-upcase actual)))))))

(defun lisp-ast--collect-forms (text form depth max-depth parent acc)
  "Push FORM and its descendants onto ACC in source order as (FORM DEPTH PARENT)."
  (let ((acc (cons (list form depth parent) acc)))
    (when (or (null max-depth) (< depth max-depth))
      (dolist (child (lisp-ast--children text form))
        (setf acc (lisp-ast--collect-forms text child (1+ depth) max-depth
                                          (lisp-ast-form-head form) acc))))
    acc))

(defun lisp-ast--all-entries (text roots max-depth)
  "Return every (FORM DEPTH PARENT) entry of ROOTS in source order."
  (let ((entries nil))
    (dolist (root roots)
      (setf entries
            (nconc entries
                   (nreverse
                    (lisp-ast--collect-forms text root 0 max-depth nil nil)))))
    entries))
(defun lisp-ast--search-entries (text roots head name-pattern max-depth)
  "Return matching (FORM DEPTH PARENT) entries of ROOTS in source order."
  (remove-if-not
   (lambda (entry)
     (lisp-ast--search-matches-p (first entry) head name-pattern))
   (lisp-ast--all-entries text roots max-depth)))

(defun lisp-ast--search-depth (arguments)
  "Return how deep SEARCH descends; a negative value means no limit."
  (let ((value (tool-argument arguments "depth")))
    (if (numberp value) (round value) 0)))

(defun lisp-ast--search-start (arguments)
  "Return the one-based index of the first matched form SEARCH reports."
  (let ((value (tool-argument arguments "start-index")))
    (if (and (numberp value) (plusp value)) (round value) 1)))

(defun lisp-ast--search-maximum (arguments)
  "Return how many matched forms one SEARCH page reports."
  (let ((value (tool-argument arguments "max-forms")))
    (if (and (numberp value) (plusp value)) (round value) 100)))

(defun lisp-ast--search-row (line-starts entry index)
  "Return the JSON row describing ENTRY at INDEX."
  (destructuring-bind (form depth parent) entry
    (json-object
     "index" index
     "type" (or (lisp-ast-form-head form) "")
     "name" (or (lisp-ast-form-name form) "")
     "depth" depth
     "parent" (or parent "")
     "start-line" (lisp-ast--line-of line-starts (lisp-ast-form-start form))
     "end-line" (lisp-ast--line-of line-starts (1- (lisp-ast-form-end form)))
     "start-char" (lisp-ast-form-start form)
     "end-char" (lisp-ast-form-end form))))

(defun lisp-ast--execute-search (context arguments)
  "Answer SEARCH's arguments with one bounded page of matching forms."
  (let* ((path (tool-argument arguments "path" :required t))
         (resolved (workspace-tool-path context path :tool-name "lisp-ast.search"))
         (content (workspace-file--read-content
                   resolved
                   :maximum-bytes *lisp-ast-maximum-file-bytes*
                   :tool-name "lisp-ast.search"
                   :description "Lisp AST source snapshot"))
         (line-starts (lisp-ast--line-starts content))
         (head (tool-argument arguments "head"))
         (name-pattern (tool-argument arguments "name-pattern"))
         (depth (lisp-ast--search-depth arguments))
         (start (lisp-ast--search-start arguments))
         (maximum (lisp-ast--search-maximum arguments)))
    (multiple-value-bind (roots stop) (lisp-ast--top-level-forms content)
      (let* ((entries (lisp-ast--search-entries content roots head name-pattern
                                                (if (minusp depth) nil depth)))
             (total (length entries))
             (first (min (1- start) total))
             (shown (subseq entries first (min total (+ first maximum))))
             (rows (loop for entry in shown
                         for index from start
                         collect (lisp-ast--search-row line-starts entry index)))
             (next (when (< (+ start (length shown)) total)
                     (+ start (length shown)))))
        (json-object
         "path" path
         "form-count" (length roots)
         "matched-count" total
         "start-index" start
         "next-index" next
         "forms-truncated" (not (null (find-if #'lisp-ast-form-truncated-p
                                               (mapcar #'first entries))))
         "parse-error" (when stop
                         (json-object
                          "line" (lisp-ast--line-of line-starts stop)
                          "char" stop
                          "character" (string (char content stop))))
         "forms" (apply #'json-array rows))))))

(defmethod tool-execute ((tool lisp-ast-search-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Answer one SEARCH call with one bounded page of matching forms."
  (declare (ignore tool))
  (let ((*lisp-ast-dialect*
          (lisp-ast--dialect-for-path (tool-argument arguments "path" :required t))))
    (tool-success (json-encode (lisp-ast--execute-search context arguments)))))

(defun lisp-ast--search-tool ()
  "Return the configured SEARCH tool instance."
  (make-instance 'lisp-ast-search-tool
                 :namespace "lisp-ast"
                 :name "search"
                 :description
                  "Search a Lisp-family source file by form structure: match a head symbol and an optional name substring, at a given nesting depth. One call reports a bounded page; when NEXT-INDEX is non-nil, pass it as START-INDEX to read the next page."
                 :parameters
                 (tool-object-schema
                  (json-object
                   "path" (tool-string-property "File path to scan, workspace-relative or absolute.")
                   "head" (tool-string-property "Exact head symbol name to match, case-insensitive; omit to match any head.")
                   "name-pattern" (tool-string-property "Optional case-insensitive substring filter on form names.")
                   "depth" (tool-integer-property "Nesting depth to descend: 0 reports top-level forms, -1 descends fully; the default is 0.")
                   "start-index" (tool-integer-property "One-based index of the first matched form to report.")
                   "max-forms" (tool-integer-property "Maximum number of matched forms to return; it must fit one response."))
                  '("path" "start-index" "max-forms"))))

(defun lisp-ast--read-tool ()
  "Return the configured READ-FILE tool instance."
  (make-instance 'lisp-ast-read-tool
                 :namespace "lisp-ast"
                 :name "read-file"
                 :description
                  "Read a Lisp-family source file as top-level forms: each form's head symbol, name, line span, and character offsets. One call reports a bounded page; when NEXT-INDEX is non-nil, pass it as START-INDEX to read the next page."
                 :parameters
                 (tool-object-schema
                  (json-object
                   "path" (tool-string-property "File path to scan, workspace-relative or absolute.")
                   "name-pattern" (tool-string-property "Optional case-insensitive substring filter on form names.")
                   "content-pattern" (tool-string-property "Optional case-insensitive substring filter on form source text.")
                   "start-index" (tool-integer-property "One-based index of the first matched form to report; pass NEXT-INDEX from a previous result to continue.")
                   "max-forms" (tool-integer-property "Maximum number of forms to return; it must fit one response."))
                  '("path" "start-index" "max-forms"))))

(-> lisp-ast--edit-tool () lisp-ast-tool)
(defun lisp-ast--edit-tool ()
  "Return the configured EDIT-FORM tool instance."
  (make-instance 'lisp-ast-edit-tool
                 :namespace "lisp-ast"
                 :name "edit-form"
                 :description
                  "Replace one form of a Lisp-family source file, or insert a form before or after it, matching by head symbol and optional name at a given nesting depth; the rest of the file stays byte for byte identical."
                 :parameters
                 (tool-object-schema
                  (json-object
                   "path" (tool-string-property "File path to edit, workspace-relative or absolute.")
                   "form-type" (tool-string-property "Head symbol to match, for example defun, defclass, setf, or let; case-insensitive.")
                   "form-name" (tool-string-property "Optional name to match, for example the function or class name; case-insensitive. Omit to match by head alone.")
                   "operation" (tool-string-property "One of replace, insert-before, or insert-after.")
                     "new-form" (tool-string-property "Exactly one complete Lisp-family form to write.")
                   "depth" (tool-integer-property "Nesting depth to search: 0 matches top-level forms, N matches forms at depth N, -1 searches every depth; the default is 0.")
                   "form-index" (tool-integer-property "Optional one-based index among matching forms when several share the type and name.")
                   "dry-run" (tool-boolean-property "True to preview the targeted form without writing the file."))
                  '("path" "form-type" "operation" "new-form"))))

(-> lisp-ast-register-tools (tool-registry) tool-registry)
(defun lisp-ast-register-tools (registry)
  "Register the AST tools into REGISTRY and return REGISTRY."
  (tool-registry-describe-namespace
   registry "lisp-ast"
    "Read and edit Lisp-family source files as balanced top-level forms. lisp-ast.read-file reports form spans; lisp-ast.search finds forms by head symbol, name substring and nesting depth; lisp-ast.edit-form replaces, inserts before, or inserts after one form matched by head symbol and name.")
  (tool-registry-register registry (lisp-ast--read-tool))
  (tool-registry-register registry (lisp-ast--search-tool))
  (tool-registry-register registry (lisp-ast--edit-tool))
  registry)

(defvar *lisp-ast-registry-wrapper* nil
  "The exact wrapper this file installed on MAKE-DEFAULT-TOOL-REGISTRY.")

(defun lisp-ast--install-registry-wrapper ()
  "Wrap MAKE-DEFAULT-TOOL-REGISTRY once so every registry carries the tools."
  (unless (eq (symbol-function 'make-default-tool-registry)
              *lisp-ast-registry-wrapper*)
    (let ((original (symbol-function 'make-default-tool-registry)))
      (setf *lisp-ast-registry-wrapper*
            (lambda (&rest arguments)
              (lisp-ast-register-tools (apply original arguments)))
            (symbol-function 'make-default-tool-registry)
            *lisp-ast-registry-wrapper*))))

(lisp-ast--install-registry-wrapper)

;; mallet:suppress no-ignore-errors -- best-effort registration into a running application
(ignore-errors
  (when (boundp '*active-application*)
    (let ((application *active-application*))
      (when application
        (lisp-ast-register-tools (application-tool-registry application))))))
