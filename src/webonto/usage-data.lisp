;;; Mode: Lisp; Package: web-onto

;;; Author: John Domingue

;;; The Open University

(in-package "WEB-ONTO")

(defvar *kmi-group-string* "kmi")

(defun kmi-group (group)
  (string= group *kmi-group-string*))
                  

(defun collect-users (&optional include-kmi-p)
  (let ((collection nil))
    (mapc #'(lambda (ontology-spec)
              (let* ((author (ocml::ontology-author (cdr ontology-spec)))
                     (user-group (user-groups author)))
                (unless (or (not author)
                            (and (not include-kmi-p)
                                 (kmi-group user-group)))
                  (let ((existing-item (assoc author collection :test #'string=)))
                    (cond (existing-item
                           (setf (third existing-item)
                                 (cons (car ontology-spec) (third existing-item)))
                           (setf (fourth existing-item)
                                 (+ (ocml::count-items-in-ontology
                                     (cdr ontology-spec)) (fourth existing-item))))
                          (t (setf collection
                                   (cons (list author user-group (list (car ontology-spec))
                                               (ocml::count-items-in-ontology
                                                (cdr ontology-spec)))
                                         collection))))))))
                        ocml::*all-ontologies*)
  (format t "~{~{~a ~a (~(~{~a ~}~)) ~a~%~}~}"
          (sort  collection
                #'>
                :key #'fourth))))

(defun count-users (&optional include-kmi-p)
  (length (collect-users include-kmi-p)))

(defun ontologies-owned-by (author)
  (mapcan #'(lambda (ontology-spec)
              (when (string= author (ocml::ontology-author (cdr ontology-spec)))
                (list (car ontology-spec))))
          ocml::*all-ontologies*))

(defun all-ontologies-under (root)
  (mapcar #'ocml::name 
          (internal-all-ontologies-under 
           (ocml::get-ontology root))))

(defun internal-all-ontologies-under (root-ontology &optional collected-ontologies)
  (let* ((collected-ontologies (cons root-ontology 
                                     collected-ontologies))
         (children
          (careful-ontology-children root-ontology
                                     collected-ontologies)))
    (if children
        (remove-duplicates
         (append
         collected-ontologies
         (mapcan #'(lambda (ontology)
                     (let ((descendants 
                            (internal-all-ontologies-under ontology 
                                                  collected-ontologies)))
                       (setf collected-ontologies
                             (remove-duplicates 
                              (append collected-ontologies descendants)))
                       (copy-list descendants)))
                 children)))
      (remove-duplicates collected-ontologies))))
    
