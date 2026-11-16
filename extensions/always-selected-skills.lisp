(in-package #:autolith)

;;;; -- Always-selected skills --

(defparameter *always-selected-skill-names* '("batch-processing" "correct-solution-first")
  "Skill names whose complete instruction bodies enter every provider request.")

(-> skill--always-selected-contributions (skill-catalog list) list)
(defun skill--always-selected-contributions (catalog contributions)
  "Return CONTRIBUTIONS extended with the always-selected skill bodies from CATALOG."
  (let ((rendered            (mapcar #'context-contribution-identifier contributions))
        (selected-characters 0))
    (dolist (name *always-selected-skill-names* contributions)
      (let ((identifier (skill--context-identifier "skill-selected" name)))
        (unless (member identifier rendered :test #'string=)
          (multiple-value-setq (contributions selected-characters)
            (skill--append-selected-contribution
             catalog
             name
             :contributions       contributions
             :selected-characters selected-characters))
          (push identifier rendered))))))

(-> skill-request-contributions (configuration conversation) list)
(defun skill-request-contributions (configuration conversation)
  "Return catalog and always-selected skill contributions for one provider request."
  (let* ((catalog  (skill-catalog-for-configuration configuration))
         (selected (skill--request-contributions-for-catalog catalog conversation)))
    (skill--bound-warning-contributions
     (skill--always-selected-contributions catalog selected))))
