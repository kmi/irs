;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")


(defun kmi-people (&optional (ontology 'kmi-planet-kb))
  (select-ontology ontology)
  (mapcar #'string-capitalize (sort (ocml-eval (setofall ?x (kmi-member ?x))) #'string<)))

(defun kmi-technologies (&optional (ontology 'kmi-planet-kb))
  (select-ontology ontology)
  (mapcar #'string-capitalize (sort (ocml-eval (setofall ?x (kmi-technology ?x))) #'string<)))

(defun kmi-projects (&optional (ontology 'kmi-planet-kb))
  (select-ontology ontology)
  (mapcar #'string-capitalize (sort (ocml-eval (setofall ?x (kmi-project ?x)))  #'string<)))

(defun kmi-planet-story-types (&optional (ontology 'kmi-planet-kb))
  (select-ontology ontology)
  (mapcar #'(lambda (x) (string-capitalize (web-onto::ocml-name x)))
          (web-onto::ocml-class-children (get-ocml-class 'kmi-planet-story))))

(defun news-items-from-story-name (story-name &optional (ontology 'kmi-planet-kb))
  (select-ontology ontology)
  (let ((headline
         (car (ocml-eval-gen
               `(setofall ?x (kmi-planet-story ,story-name
                                               has-headline ?x))))))
    (when headline
      (cl-user::news-item-for-headline headline))))

(defun check-story-database (&optional (ontology 'kmi-planet-kb))
  (select-ontology ontology)
  (let ((kmi-planet-story-headlines
         (ocml-eval-gen
          `(setofall ?x (kmi-planet-story ?story
                                          has-headline ?x)))))
    (mapcan
     #'(lambda (headline)
        (let ((story (cl-user::news-item-for-headline headline)))
          (unless story
            (list headline))))
     kmi-planet-story-headlines)))

(defun stories-related-to (story-type person technology project &optional (ontology 'kmi-planet-kb))
  (select-ontology ontology)
  (ocml-eval-gen `(setofall ?x
                            (and (kmi-planet-story ?x relates-events ?e)
                                 ,@(unless (or (not story-type) (eq story-type 'anystorytype))
				     `((,story-type ?x)))
                                 ,@(unless (or (not person) (eq person 'anyperson))
				     `((stories-about-agent ?x ,person)))
                                 ,@(unless (or (not project)(eq project 'anyproject))
                                     `((event-involves-kmi-project ?e ,project)))
                                 ,@(unless (or (not technology) (eq  technology 'anytechnology))
                                     `((event-involves-kmi-technology ?e ,technology)))))))

(defvar *html-class-colour*
  "GREEN")

(defvar *html-instance-colour*
  "#1063B5")

(defvar *html-relation-colour*
  "#6E8c9c")

(defvar *html-function-colour*
  "#BB0004")

(defvar *html-procedure-colour*
  "#BB0009")

(defvar *html-rule-colour*
  "RED")

(defun ocml-lookup (name &optional (ontology 'kmi-planet-kb)
                         check-relation-defined-from-def-relation-p)
  (select-ontology ontology)
  (let ((result nil) (type nil) (class nil) (colour nil))
    (cond ((setf result (get-ocml-class name))
           (setf colour *html-class-colour*
                 type :class))
          ((setf result (find-current-instance name))
           (setf colour *html-instance-colour*
                 type :instance
                 class 
                 (name (class-of result))))
          ((setf result (find-bc-rule name))
           (setf colour *html-rule-colour*
                 type :rule))
          ((and (setf result (get-ocml-relation name))
                (or (not check-relation-defined-from-def-relation-p)
                    (ocml::defined-from-def-relation result)))
           (setf colour *html-relation-colour*
                 type :relation))
          ((setf result (get-ocml-function name))
           (if (procedure-p name)
               (setf colour *html-procedure-colour*
                     type :procedure)
             (setf colour *html-function-colour*
                   type :function)))
          ((setf result (get-ontology name))
           (setf colour *html-ontology-colour*
                 type :ontology)))
    (if (eq type :ontology)
        (values result
                (when result (eq ontology (name result)))
                colour
                (when result (name result))
                type
                class)
      (values result
              (when result (and (home-ontology result)
                                (eq ontology (name (home-ontology result)))))
              colour
              (when result (name (home-ontology result)))
              type
              class))))

(defun ocml-definition (name &optional (ontology 'kmi-planet-kb) instance-class)
  (select-ontology ontology)
  (cond ((get-ocml-class name)
         (web-onto::get-source-string (list 'def-class name ontology)))
        ((find-current-instance name instance-class)
         (web-onto::get-source-string (list 'def-instance name instance-class ontology)))
        ((find-bc-rule name)
         (web-onto::get-source-string (list 'def-rule name ontology)))
        ((get-ocml-relation name)
         (web-onto::get-source-string (list 'def-relation name ontology)))
        ((and (get-ocml-function name) (not (procedure-p (get-ocml-function name))))
         (web-onto::get-source-string (list 'def-function name ontology)))
        ((get-ocml-function name)
         (web-onto::get-source-string (list 'def-procedure name ontology)))))

(defun html-ocml-ask (expression)
  (internal-ocml-eval expression  #'(lambda (expression)
                                     (ocml::ask-top-level expression :all t :query-mode t)))) 

(defun html-ocml-eval (expression)
  (internal-ocml-eval expression #'(lambda (expression)
                                     (ocml::ocml-eval-gen expression nil))))

(defun internal-ocml-eval (expression function &optional (ontology 'kmi-planet-kb))
  (select-ontology ontology)
  (let ((*package* (find-package "OCML"))
        output (result nil))
    (setf output
	  (handler-case
	      (with-output-to-string (*standard-output*)
                (let ((input (read-from-string (string-upcase expression))))
		  (setf result
                        (if input
                            (format nil "~a" (funcall function input))
                            "No Input"))))
            ;;use serious-condition to catch stack overflows
	    (serious-condition (c) (format nil "When evaluating ~a got an error of type ~a" expression c))))
    
    (http::font
     (http::preformatted (if result
                             (concatenate 'string output "<p>" result )
                             output))
     :size 10)))