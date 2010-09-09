;;;orchestration constructs

(in-package wsmo-protocol)


(defun get-mediator-needed-by-goal (goal)
  ;;(ocml::findany '?mediator-to-use `(ocml::goal-associated-mediator ,goal ?mediator-to-use)))
  (car (all-mediators-associated-with-goal goal)))

(defun all-mediators-associated-with-goal (goal-type last-goal)
  (let* ((mediators (ocml::all-class-slot-values goal-type 'ocml::used-mediator))
         (home-ontology (ocml::home-ontology (ocml::get-ocml-class goal-type)))
         (relevant-ontologies (cons home-ontology
                                    (ocml::dependent-ontologies 
                                     home-ontology))))
    (loop for ontology in (ocml::remove-subsumed-ontologies relevant-ontologies)
          do
          (ocml::switch-to-ontology ontology)
          (let ((ontology-mediators
                 ;;make sure the class exists
                 (mapcan
                  #'(lambda (x) (when (ocml::get-domain-class x) (list x)))
                  (web-onto::findany 
                   '?x '(= ?x (ocml::all-subclasses ocml::mediator))))))
            (setf mediators
                  (append mediators 
                          (mapcan 
                           #'(lambda (mediator)
                               (when (and (find goal-type
                                                (ocml::all-class-slot-values 
                                                 mediator 
                                                 'ocml::has-target-component))
                                          (or (null last-goal)
                                              (find last-goal
                                                (ocml::all-class-slot-values 
                                                 mediator 
                                                 'ocml::has-source-component))))
                                 (list mediator)))
                                  ontology-mediators)))))
    (remove-duplicates mediators)))

(defun suitable-mediator? (goal-input-role mediator-l)
  (do ((mediator mediator-l (cdr mediator)) (mediation-service) (result nil) (found nil))
      ((or (null mediator) found) result)
    (setf mediation-service (ocml::findany '?x
                                           `(ocml::wsmo-mediator-mediation-service ,(car mediator) ?x)))
    ;(format t "mediation service goal ~a~%" mediation-service)
    (when (eql goal-input-role (output-role mediation-service))
      ;(format t "the mediator ~a looks good!! ~%" (car mediator))
      (setf result (car mediator))
      (setf found t))))
    

(defun set-result (goal-instance output-role output-value)
  (ocml::ocml-eval-gen `(ocml::set-goal-value ,goal-instance ,output-role ',output-value)))


(defun find-input-role-value (goal-input ws goal goal-l)
  (let ((ws-input-role (input-roles ws))
        (result))
    (if (find goal-input ws-input-role)
        (setf result (ocml::findany '?x 
                       `(= ?x (ocml::wsmo-role-value 
                               ,ws
                               ,goal-input))))
      (do ((goal-tmp goal-l (cdr goal-tmp)) (value nil) (value-found nil))
          ((or (null goal-tmp) value-found) result)
        (when (not (eq goal (car goal-tmp)))
          ;(format t "current goal ~a current goal of the list ~s~%" goal (car goal-tmp))
          ;(format t "output list of the goal ~a~%" (output-role (car goal-tmp)))
          
          ;;n.b output-role does not return a list as there mostly one output, need to listify it
          (when (find goal-input (list (output-role (car goal-tmp))))
            (setf value-found t)
            (let ((goal-instance (cdr (assoc (car goal-tmp) wp::*internal-goal-instances*))))
              ;(format t "goal ~a instance ~s~%" goal-tmp goal-instance)
              (when goal-instance
                (setf result (ocml::findany '?x 
                                      `(= ?x (ocml::wsmo-role-value 
                                              ,goal-instance
                                              ,goal-input))))
                ;(format t "result in do ~a~%" result)
                ))))))
    result))

(defun find-input-role-value-through-mediator (ontology mediator)
  (let* ((mediation-service (ocml::findany '?x
                                           `(ocml::wsmo-mediator-mediation-service ,mediator ?x)))
         (mediation-source (ocml::findany '?x 
                                          `(ocml::wsmo-mediator-source ,mediator ?x)))
         (input-list (mapcar #'(lambda (input-mediation-service) 
                                 (list input-mediation-service
                                       (ocml::findany '?x
                                                      `(= ?x (ocml::wsmo-role-value 
                                                              ,(cdr (assoc mediation-source wp::*internal-goal-instances*))
                                                              ,input-mediation-service)))))
                             ;;;the output-role is a single value need to listify it 
                             (list (output-role mediation-source))))
         (result nil))
    (format t "fi: ~a ~a ~a~%" mediation-service mediation-source input-list)
    (setf result (ip::internal-solve-goal ontology mediation-service input-list))
    result))

(defvar *control-constructs* '(ocml::sequence-execution 
                               ocml::conditional-execution ocml::while-execution))

(defun control-construct-p (x)
  (and (listp x) (find (car x) *control-constructs*)))

(defmethod execute-control-construct (type args)
  (error "Unknown control construct ~a" type))

(defmethod execute-control-construct ((type (eql 'ocml::sequence-execution)) args)
  (funcall 'internal-sequence-execution (car args) (second args) (cddr args)))

(defmethod execute-control-construct ((type (eql 'ocml::conditional-execution)) args)
  (apply #'ocml::conditional-execution args))

(defmethod execute-control-construct ((type (eql 'ocml::while-execution)) args)
  (apply #'ocml::while-execution args))

(defun ocml::internal-run-orchestration (orchestration-form)
  ;;(setf aa orchestration-form)
  (eval orchestration-form))

(defmacro ocml::sequence-execution (ontology web-service &rest goal-list)
  `(internal-sequence-execution ',ontology ',web-service ',goal-list))

(defun ocml::internal-sequence-execution (goal-list)
  (internal-sequence-execution wsmo-protocol::*current-orchestration-web-service* goal-list))

(defun goal-class-p (x)
  (ocml::holds? 'ocml::subclass-of x 'ocml::goal))

(defun ocml::internal-achieve-goal (goal-type goal-values)
  (let ((ontology (ocml::name ocml::*current-ontology*))
        (result)
        (goal-input-roles (input-roles goal-type))
        (goal-output-role (output-role goal-type))
        role-value-pairs)
    (setf role-value-pairs
          (mapcar #'(lambda (goal-input-role value)
                      (list goal-input-role value))
                  goal-input-roles goal-values))
    ;;(setf ll (list ontology goal-type role-value-pairs))
    (setf result (ip::internal-solve-goal ontology goal-type role-value-pairs))
    ;;(format t "goal ~a gives result ~s~%" goal-type result)
    (set-result (cdr (assoc goal-type wp::*internal-goal-instances*)) goal-output-role result)
    result))

(defun internal-sequence-execution (web-service goal-list)
  ;;(format t "goal list ~a~%" goal-list)
  (let ((ontology (ocml::name ocml::*current-ontology*))
        (result))
    (loop for goal-type in goal-list
          do
          ;;supposing all goals are in the same ontology of the web service
          (cond ((and (listp goal-type)
                      (ocml::function-or-procedure? (car goal-type)))
                 (ocml::ocml-eval-gen goal-type))                  
                ((and (atom goal-type) (goal-class-p goal-type))
                 (let (ontology-for-invocation 
                       (goal-input-roles (input-roles goal-type))
                       (goal-output-role (output-role goal-type))
                       (mediator-needed 
                        (remove-duplicates
                         (mapcan #'(lambda (previous-goal)
                                     (copy-list (all-mediators-associated-with-goal goal-type previous-goal)))
                                 (mapcar #'car *internal-goal-instances*))))
                       (input-role-values nil))
                   (when (or (null mediator-needed) (ocml:nothing? mediator-needed))
                     (ocml::select-ontology ontology)
                     ;case1: no mediation at all
                     ;all input-role output-role names are different
                     ;;(format t "No mediator needed for goal ~a~%" goal-type)
                     (setf input-role-values 
                           (mapcar #'(lambda (goal-input-role)
                                       (list goal-input-role 
                                             (find-input-role-value goal-input-role web-service goal-type goal-list)))
                                   goal-input-roles)))
                   (when (and mediator-needed (not (ocml:nothing? mediator-needed)))
                     ;;(format t "Mediator needed ~a for goal ~s~%" mediator-needed goal-type)
                     (ocml::select-ontology ontology)
                     (setf input-role-values
                           (mapcar #'(lambda (goal-input-role)
                                       (list goal-input-role
                                             (let ((mediator-to-use (suitable-mediator? goal-input-role mediator-needed)))
                                               (if mediator-to-use
                                                   (find-input-role-value-through-mediator ontology mediator-to-use)
                                                 (find-input-role-value goal-input-role web-service 
                                                                        goal-type goal-list)))))
                                   goal-input-roles)))
                   ;;(format t "Input list ~a for goal ~s~%" input-role-values goal-type)
                   (multiple-value-setq (result ontology-for-invocation)
                       (ip::internal-solve-goal ontology goal-type input-role-values))
                   (maybe-save-sequence-result result)
                   ;;(format t "goal ~a gives result ~s~%" goal-type result)
                   (ocml::select-ontology ontology-for-invocation)
                   (set-result (cdr (assoc goal-type *internal-goal-instances*)) goal-output-role result)
                   (ocml::select-ontology ontology)))
                ;;if its not a procedure or a goal then we just return it as the ocml intepreter
                ;;has already evaluated the argument
                (t (setf result goal-type)
                   (maybe-save-sequence-result result))))
    result))

(defun maybe-save-sequence-result (result)
  (when (boundp 'ocml::*sequence-results*)
    (push result ocml::*sequence-results*)))

(defun ocml::get-last-achieve-goal-value (goal-name)
  (second (assoc (list (ocml::name ocml::*current-ontology*)  goal-name) *achieve-goal-results* :test #'equal)))

(defun ocml::get-all-achieve-goal-values (goal-name)
  (let ((result nil)
        (current-ontology (ocml::name ocml::*current-ontology*)))
    ;;(setf op current-ontology gg goal-name aaa *achieve-goal-results*)
    (mapc #'(lambda (achieve-goal-result-description)
              (when (and (eq current-ontology (caar achieve-goal-result-description))
                         (eq goal-name (second (car achieve-goal-result-description))))
                (push (second achieve-goal-result-description) result)))
          *achieve-goal-results*)
    result))

(defun ocml::conditional-execution (ontology web-service condition-goal true-goal false-goal)
  (declare (ignore ontology web-service condition-goal true-goal false-goal)))

(defun ocml::while-execution (ontology web-service condition-goal true-goal false-goal)
  (declare (ignore ontology web-service condition-goal true-goal false-goal)))
