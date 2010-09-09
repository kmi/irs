;;; Mode: Lisp; Package: web-onto

;;; Author: John Domingue

;;; The Open University

(in-package "WEB-ONTO")

(defclass tree-mixin ()
  ((parent-link :accessor parent-link :initform nil)
   (left-outline :accessor left-outline :initform nil)
   (right-outline :accessor right-outline :initform nil)
   (offset :accessor offset :initform 0)
   (level-positions :initform nil :accessor level-positions)
   (level :accessor level)))

(defclass web-onto-shape ()
  ((x :initarg :x :accessor x-position)
   (y :initarg :y :accessor y-position)
   (width :initarg :width :accessor width)
   (height :initarg :height :accessor height)))

(defmethod new-position ((object web-onto-shape) x y)
  (setf (x-position object) x
        (y-position object) y))

(defclass web-onto-named-shape (web-onto-shape)
  ((web-onto-name :initarg :web-onto-name :accessor web-onto-name :initform nil)))

(defclass ocml-class (web-onto-named-shape tree-mixin 
                                           ) ())

(defmethod ocml-name ((class ocml-class))
  (if (web-onto-name class)
      (web-onto-name class)
      (setf (web-onto-name class)
            (string-downcase (format nil "~a" (ocml::maybe-ocml-name class))))))

(defclass ocml-ontology (web-onto-named-shape tree-mixin) ())

(defmethod ocml-name ((ontology ocml-ontology))
  (if (web-onto-name ontology)
      (web-onto-name ontology)
      (setf (web-onto-name ontology)
            (string-downcase (format nil "~a" (ocml::name ontology))))))

#+:irs-lispworks
(defun roots ()
  (slot-value (find-class 'ocml::basic-domain-class)
                      'clos::direct-subclasses))

#+:irs-lispworks
(defun ocml-class-children (class)
  (with-slots (clos::direct-subclasses) class
    (ocml::filter-active-classes clos::direct-subclasses)))

;;;only returns children available from the current ontology
;;;that is children within the current ontology or one of its ancestors
(defun currently-available-ocml-class-children (class)
  (let ((children (ocml-class-children class)))
    (mapcan #'(lambda (child)
                (when (ocml::get-ocml-class (ocml::maybe-ocml-name child))
                  (list child)))
            children)))

#+:irs-lispworks
(defun ocml-class-parents (class)
  (with-slots (clos::direct-superclasses) class
    (ocml::filter-active-classes
     (remove (find-class 'ocml::basic-domain-class)
             clos::direct-superclasses))))

(defun ocml-class-slots (class)
  (ocml::domain-slots class))

(defun careful-ocml-class-children (class classes)
  (set-difference (currently-available-ocml-class-children class) classes))

(defun ontology-children (ontology)
  (ocml::ontology-included-by ontology))

(defun careful-ontology-children (ontology ontologies)
  (set-difference (ocml::ontology-included-by ontology) ontologies))

(defun careful-task-or-psm-children (name names)
  (set-difference (ocml::task-or-psm-children name) names))

#+:irs-lispworks
(defun draw-all-classes ()
  (let ((root (make-instance 'ocml::ocml-metaclass
                             :name "root")))
    (setf (slot-value root 'clos::direct-subclasses)
          (slot-value (find-class 'ocml::basic-domain-class)
                      'clos::direct-subclasses))
    (tree-draw-classes root :horizontal)))

(defun tree-draw-classes (root existing-classes existing-instances orientation)
  (multiple-value-bind (classes left top)
      (produce-tree root orientation)
    (mapcar #'(lambda (class)
                (append (list (web-onto-name class)
                              (class-extra-info (ocml::name class) class
                                                (remove (ocml::name class)
                                                        existing-classes)
                                                existing-instances))
                        (let ((class-structure-info
                               (ocml::list-class-parents-and-children class)))
                          (if (eq class root)
                              ;;root has no parents
                              (cons "nil%" (cdr class-structure-info))
                              class-structure-info))
                        (if (eq orientation :horizontal)
                            (list (x-position class) 
                                  (+ (y-position class) (- top))
                                  (tree-width class)
                                  (tree-height class))
                            (list (+ (x-position class) (- left))
                                  (y-position class)
                                  (tree-width class)
                                  (tree-height class)))))
            classes)))

(defun produce-tree (root orientation)
  (clear-tree root 'ocml-class-children)
  (let (classes collected-classes)
    (multiple-value-bind (left top)
    (pretty-draw-tree
      root
      #'(lambda (class stream)
          (declare (ignore stream))
          (push class classes))
      #'(lambda (class)
          (push class collected-classes)
          (unless (and (ocml::limited-p ocml::*list-length-limit*)
                       (> *number-of-tree-items* ocml::*list-length-limit*))
            (let ((children (careful-ocml-class-children class collected-classes)))
              (cond ((and (ocml::limited-p ocml::*list-length-limit*)
                          (> (+ *number-of-tree-items*
                                (length children))
                             ocml::*list-length-limit*))
                     (setf children
                           (subseq children 0
                                   (- ocml::*list-length-limit*
                                      *number-of-tree-items*)))
                     (incf *number-of-tree-items* (- ocml::*list-length-limit*
                                                     *number-of-tree-items*)))
                    (t (incf *number-of-tree-items* (length children))))
              (setf collected-classes
                    (append children collected-classes))
              children)))
      :orientation orientation
      :clear-inferior-producer #'(lambda () (setf collected-classes nil)))
      (values classes left top))))

(defun parse-java-mangling (str)
  (let* ((str1 (substitute #\space #\% str))
         (lst (http::read-into-list str1)))
    (format nil "[~{~(\"~A\"~),~}]" lst)))

(defun js-tree-draw-classes (root existing-classes existing-instances orientation)
  (declare (ignore existing-instances existing-classes))
  (multiple-value-bind (classes left top)
      (produce-tree root orientation)
    (mapcar #'(lambda (class)
                (format nil "{name: \"~A\", x: ~A, y: ~A, superclasses: ~A}"
                        (first class) (third class) (fifth class)  (second class)
                        ))
            (mapcar #'(lambda (class)
                 (let ((class-structure-info
                        (ocml::list-class-parents-and-children class)))
                   (append
                    ;; this class's name
                    (list (web-onto-name class))
                    ;; superclasses
                    (list (if (eq class root)
                              "[]"
                              (parse-java-mangling (car class-structure-info))))
                    (if (eq orientation :horizontal)
                        ;; XXX I'm just curious: we passed ORIENTATION to
                        ;; PRODUCE-TREE about ten lines back, so why are
                        ;; we now worrying about it again?
                        (list (x-position class) 
                              (+ (y-position class) (- top))
                              (tree-width class)
                              (tree-height class))
                        (list (+ (x-position class) (- left))
                              (y-position class)
                              (tree-width class)
                              (tree-height class))))))
             classes))))

(defun tree-draw-tasks (root orientation)
  (let ((task-psms nil) (collected-task-psms nil))
    (clear-tree root 'ocml::task-or-psm-children)
    (multiple-value-bind (left top)
        (pretty-draw-tree root
			  #'(lambda (task-psm stream)
			      (declare (ignore stream))
			      (push task-psm task-psms))
			  #'(lambda (task-psm)
			      (push task-psm collected-task-psms)
			      (let ((children
                                     (careful-task-or-psm-children task-psm collected-task-psms)))
			        (setf collected-task-psms (append children collected-task-psms))
			        children))
                          :orientation orientation
			  :clear-inferior-producer
			  #'(lambda () (setf collected-task-psms nil)))
      (mapcar #'(lambda (task-psm)
                  (let ((task-p (task-p (ocml::name task-psm))))
                    (append (list (if task-p
                                      "task"
                                      "problem_solving_method")
                                  (web-onto-name task-psm)
                                  (class-extra-info (ocml::name task-psm) task-psm
                                                    nil nil) ;;
                                  ;;"nil"
                                  )
                            (let ((task-psm-structure-info
                                   (ocml::list-task-psm-parents-and-children task-psm)))
                              (if (eq task-psm root)
                                  ;;root has no parents
                                  (cons "nil%" (cdr task-psm-structure-info))
                                  task-psm-structure-info))
                            (if (eq orientation :horizontal)
                                (list (x-position task-psm) 
                                      (+ (y-position task-psm) (- top))
                                      (tree-width task-psm)
                                      (tree-height task-psm))
                                (list (+ (x-position task-psm) (- left))
                                      (y-position task-psm)
                                      (tree-width task-psm)
                                      (tree-height task-psm)))
                            (list (if task-p
                                      *task-colour*
                                      *problem-solving-method-colour*)))))
              task-psms))))

(defun tree-draw-problem-solving-methods (root orientation)
  (tree-draw-tasks root orientation))

(defun ontology-colour (ontology)
  (or (cdr (assoc (ocml::ontology-type ontology) *ontology-type-colour-mapping*))
      *ontology-colour*))

(defun tree-draw-ontology (root orientation)
  (let ((ontologies nil) (collected-ontologies nil) (left-most-node-name-width) (min-x nil))
    (clear-tree root #'(lambda (ontology)
			 (push ontology collected-ontologies)
			 (careful-ontology-children ontology
						    collected-ontologies)))
    (setf collected-ontologies nil)
    (multiple-value-bind (left top) ;; top right)
      (pretty-draw-tree root
			#'(lambda (ontology stream)
			    (declare (ignore stream))
			    (push ontology ontologies))
			#'(lambda (ontology)
                            (push ontology collected-ontologies)
                            (let ((children (careful-ontology-children ontology
                                                                       collected-ontologies)))
                              (setf collected-ontologies (append children collected-ontologies))
                              children))
                        :orientation orientation
                        :clear-inferior-producer
                        #'(lambda () (setf collected-ontologies nil)))
      (mapc #'(lambda (ontology)
		(when (or (not min-x) (< (x-position ontology) min-x))
		  (setf min-x (x-position ontology) left-most-node-name-width
			(ceiling (/ (tree-width ontology) 2)))))
	    ontologies)
      (mapcar #'(lambda (ontology)
                  (append (list "ontology" (web-onto-name ontology)
                                "nil")
			  (let ((ontology-structure-info
				 (ocml::list-ontology-structure-info
				  ontology)))
			    (if (eq ontology root)
				;;root has no parents
				(cons "nil%" (cdr ontology-structure-info))
				ontology-structure-info))
                          (if (eq orientation :horizontal)
                              (list (+ (x-position ontology)
                                       (round (/ (tree-width root) 2)))
                                    (+ (y-position ontology) (- top))
                                    (tree-width ontology)
                                    (tree-height ontology))
			      (list (+ (x-position ontology)
				       left-most-node-name-width
				       (- left))
				    (y-position ontology)
				    (tree-width ontology)
				    (tree-height ontology)))
                          (list (ontology-colour ontology))))
              ontologies))))

(defun class-extra-info (name class existing-classes
                              existing-instances)
  (let ((parents (intersection existing-classes
                               (mapcar #'ocml::name (ocml-class-parents class))))
        (children (intersection existing-classes
                        (mapcar #'ocml::name (ocml-class-children class))))
        (instances (intersection
           existing-instances
           (mapcar #'ocml::name (ocml::all-current-direct-instances name))))
        (type-of (ocml::type-of-class existing-classes name))
        (class-types (ocml::class-types existing-classes name))
        (class-default-values (ocml::class-default-values existing-instances name)))
  (format nil "~(~:[~*null-value~;~{~a}~}~]%~:[~*null-value~;~{~a}~}~]%~:[~*null-value~;~{~a}~}~]%~:[~*null-value~;~{~a}~}~]%~:[~*null-value~;~{~a}~}~]%~:[~*null-value~;~{~a}~}~]%~)"
          parents parents children children instances instances type-of
          type-of class-types class-types
          class-default-values class-default-values)))

(defun instance-extra-info (instance-name class-name existing-classes
                                          existing-instances)
  (format nil "~(~a%~{~a}~}%~)" class-name
          (ocml::default-value-of-class existing-classes instance-name)))

(defun tree-draw-instance (name class-name existing-classes existing-instances)
  (let* ((ocml-name (string-downcase name))
         (width (tree-width ocml-name)))
    (list (list ocml-name (instance-extra-info name class-name
                                               existing-classes existing-instances)
                "nil%" "nil%"
                (+ *default-tree-start-x* (ceiling (/ width 2)))
                *default-tree-start-y*
                (+ *default-tree-start-x* width)
		(+ *default-tree-start-y* *class-node-height*)))))

(defun tree-draw-class (name class existing-classes existing-instances)
  (let* ((ocml-name (string-downcase name))
         (width (tree-width ocml-name)))
    (list (list ocml-name (class-extra-info name class
                                            existing-classes existing-instances)
                "nil%" "nil%"
                (+ *default-tree-start-x* (ceiling (/ width 2)))
                *default-tree-start-y*
                (+ *default-tree-start-x* width)
		(+ *default-tree-start-y* *class-node-height*)))))

(defun tree-draw-ocml-structure (name structure)
  (declare (ignore structure))
  (let* ((ocml-name (string-downcase name))
        (width (tree-width ocml-name)))
    (list (list ocml-name "nil"
                "nil%" "nil%"
                (+ *default-tree-start-x* (ceiling (/ width 2)))
                *default-tree-start-y*
                (+ *default-tree-start-x* width)
		(+ *default-tree-start-y* *class-node-height*)))))

(defun ontologies-hierarchy (ontology &optional collected-ontologies (include-number-of-children t))
  (let ((children (careful-ontology-children ontology collected-ontologies)))
    (setf collected-ontologies (append children collected-ontologies))
    (values
     (if include-number-of-children
         (cons (length children)
               (cons (ocml-name ontology)
                     (mapcar #'(lambda (ontology)
                                 (multiple-value-bind (hierarchy new-collected-ontologies)
				     (ontologies-hierarchy ontology collected-ontologies
							   include-number-of-children)
				   (setf collected-ontologies new-collected-ontologies)
				   hierarchy))
		             children)))
         (cons (ocml-name ontology)
	       (mapcar #'(lambda (ontology) (multiple-value-bind (hierarchy new-collected-ontologies)
						(ontologies-hierarchy ontology collected-ontologies
								      include-number-of-children)
					      (setf collected-ontologies new-collected-ontologies)
					      hierarchy))
		       children)))
     collected-ontologies)))

(defun complete-ontologies-hierarchy (&optional (include-number-of-children t))
  (ontologies-hierarchy (get-ontology 'ocml::base-ontology) nil include-number-of-children))

(defun complete-ontologies-string-hierarchy ()
  (ontologies-string-hierarchy (get-ontology 'ocml::base-ontology)))

(defun planet-ontologies-string-hierarchy ()
  (string-downcase (format nil "~{~a ~}"
                           (cons (length *planet-ontologies*)
				 (cons 'base-ontology (mapcan #'(lambda (x) (list 0 x))
							      *planet-ontologies*))))))

(defun ontologies-string-hierarchy (ontology)
  (with-output-to-string (output-stream)
    (internal-ontologies-string-hierarchy ontology output-stream nil)))

(defun internal-ontologies-string-hierarchy (ontology output-stream &optional collected-ontologies)
  (let ((children (careful-ontology-children ontology collected-ontologies)))
    (setf collected-ontologies (append children collected-ontologies))
    (format output-stream "~d ~a " (length children) (ocml-name ontology))
    (mapc #'(lambda (ontology) (setf collected-ontologies
                                     (internal-ontologies-string-hierarchy ontology output-stream
                                                                           collected-ontologies)))
	    children)
    collected-ontologies))

                             
    
