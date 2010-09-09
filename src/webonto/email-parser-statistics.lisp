;;; Mode: Lisp; Package: web-onto

;;; Author: John Domingue

;;; The Open University

(in-package "WEB-ONTO")


(defstruct (email-parser-query (:type list))
  research-area strings n1 n2 n3)

(defun email-parser-statistics-results (stream upcase-string)
  (ocml::select-ontology 'ocml::akt-kmi-planet-kb)
  (let ((*package* (find-package "OCML"))
        (results nil))
    (with-input-from-string (istream upcase-string)
      ;;the EMAIL-PARSER-STATISTICS-RESULTS bit
      (read istream)
      (dolist (query (read istream))
        (when (and (> (email-parser-query-n2 query) 0)
                   (> (email-parser-query-n3 query) 30))
          (let ((result
                 (ocml::ocml-eval-gen
                  `(ocml::setofall
                    (?kmi-member1 ?kmi-member2)
                    (and (ocml::project-generic-area ?kmi-project
                                               ,(email-parser-query-research-area query))
                         (ocml::kmi-project ?kmi-project)
                         (ocml::has-project-leader ?kmi-project ?kmi-member1)
                         (ocml::kmi-member ?kmi-member1)
                         (ocml::has-project-member ?kmi-project ?kmi-member2)
                         (ocml::kmi-member ?kmi-member2))))))
            (push result results))))
      (http::princ-to-binary-stream
       (format nil "web-onto server acknowledges parsing statistics ~a~%" results)
       stream))))
