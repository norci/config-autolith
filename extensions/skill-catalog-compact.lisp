(in-package #:autolith)

;;;; -- Skill catalog selection lines --

(defvar *skill-catalog-render-original* nil
  "The tracked catalog renderer, retained so this plugin reloads cleanly.")

(unless *skill-catalog-render-original*
  (setf *skill-catalog-render-original* #'skill-catalog-render))

(defvar *skill-catalog-prefix-original* nil
  "The tracked catalog introduction, retained so this plugin reloads cleanly.")

(unless *skill-catalog-prefix-original*
  (setf *skill-catalog-prefix-original* #'skill--catalog-prefix))

;; Descriptions are the selection signal; 30 skills with them render at 3719 characters.
(setf *skill-catalog-character-budget* 4000)

(defun skill--catalog-prefix ()
  "Return the model-visible catalog introduction for name-and-description entries."
  (format nil "## Skills~2%An Autolith skill is a reusable instruction set stored in SKILL.sexp or standard SKILL.md. Each entry gives the skill name and the description that states when to use it.~2%### Available skills~%"))

(defun skill--catalog-selection-line (metadata)
  "Render one catalog line carrying the description that drives selection."
  (let ((description (cl-skills:skill-metadata-description metadata)))
    (if (and description (plusp (length description)))
        (format nil "- ~A: ~A" (cl-skills:skill-metadata-name metadata) description)
        (format nil "- ~A" (cl-skills:skill-metadata-name metadata)))))

(defun skill-catalog-render (catalog &key (character-budget *skill-catalog-character-budget*))
  "Render CATALOG as names and descriptions; paths resolve when a skill is selected."
  (let* ((skills   (cl-skills:skill-catalog-skills catalog))
         (prefix   (skill--catalog-prefix))
         (guidance (skill--catalog-guidance)))
    (labels ((compose (lines omitted)
               (cl-skills::skill--catalog-compose lines omitted
                                                  :prefix   prefix
                                                  :guidance guidance)))
      (if (null skills)
          (values (compose nil 0) 0 0)
          (let ((lines nil)
                (selected 0))
            (dolist (metadata skills)
              (let* ((candidate (append lines (list (skill--catalog-selection-line metadata))))
                     (omitted   (- (length skills) (length candidate))))
                (when (<= (length (compose candidate omitted)) character-budget)
                  (setf lines candidate)
                  (incf selected))))
            (let ((omitted (- (length skills) selected)))
              (values (compose lines omitted) selected omitted)))))))
