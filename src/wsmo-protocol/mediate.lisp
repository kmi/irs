(in-package wsmo-protocol)

(defun subclass-of (x y)
  (ocml::holds? 'ocml::subclass-of x y))

(defun wg-mediator-p (x)
  (subclass-of x 'ocml::wg-mediator))

(defun oo-mediator-p (x)
  (subclass-of x 'ocml::oo-mediator))

(defun get-mapping-rule-spec-relation (mapping-rule-spec)
  (caar (second mapping-rule-spec)))

(defun create-oo-mediator-ontology (source-ontology target-ontology)
  (let ((new-ontology-name (intern (format nil "~a-TO-~a-MEDIATOR-ONTOLOGY" source-ontology target-ontology)
                                   (find-package "OCML"))))
    (eval `(ocml::def-ontology-internal ',new-ontology-name
                                        ,(format nil  "Ontology to mediate from ~:(~a~) to ~:(~a~)" source-ontology target-ontology)
                                        '(:includes (,source-ontology ,target-ontology)
                                          :type :web-service
                                          :author "john")))
    new-ontology-name))

(defun create-combined-oo-mediator-ontologies (web-service ontologies)
  (let ((new-ontology-name (intern (format nil "~a-COMBINED-MEDIATOR-ONTOLOGY" web-service)
                                   (find-package "OCML"))))
    (eval `(ocml::def-ontology-internal ',new-ontology-name
                                        ,(format nil  "Combined Ontology for ~:(~a~)" web-service)
                                        '(:includes ,ontologies
                                          :type :web-service
                                          :author "john")))
    new-ontology-name))

(defun create-mapping-rules (mapping-rule-specs)
  (mapc #'(lambda (mapping-rule-spec)
            (format t "def-rule ~a ~a~%" (car mapping-rule-spec) (cdr mapping-rule-spec))
            (ocml::def-rule-internal (car mapping-rule-spec) "" (cdr mapping-rule-spec)))
        mapping-rule-specs))

(defun run-mapping-rules (mapping-rule-specs target-goal input-roles input-values)
  "TARGET-GOAL is the owner of the input-values, not the mediation
goal invoking this function."
  (dolist (mapping-rule-spec mapping-rule-specs)
    (setf input-values
          (mapcar #'(lambda (input-role input-value)
                      (let* ((form `(,(get-mapping-rule-spec-relation mapping-rule-spec) ,target-goal ,input-role ,input-value ?x))
                             (mediated-value (ocml::findany '?x form)))
                        (if (ocml:nothing? mediated-value)
                            input-value
                          mediated-value)))
                  input-roles input-values)))
  input-values)

(defun run-oo-mediator (oo-mediator goal input-roles input-values)
  (let ((source
         (web-onto::findany 
          '?x 
          `(ocml::wsmo-mediator-source-component
            ,oo-mediator ?x)))
        (target
         (web-onto::findany 
          '?x 
          `(ocml::wsmo-mediator-target-component
            ,oo-mediator ?x)))
        (target-goal
         (web-onto::findany
          '?x
          `(ocml::has-target-goal ,goal ?x)))
        (mapping-rules 
         (web-onto::findany 
          '?x 
          `(ocml::wsmo-mediator-mapping-rules
            ,oo-mediator ?x))))
    (when (and source target mapping-rules)
      (let ((new-ontology (create-oo-mediator-ontology source target)))
        (ocml::select-ontology new-ontology)
        (create-mapping-rules (apply #'append mapping-rules))
        (values new-ontology
                (run-mapping-rules (car mapping-rules) target-goal input-roles input-values))))))



(defun ocml::irs-input-role-values-mediator (ontology web-service)
  ;;(setf xoo ontology xxw web-service)
  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology ontology)
    (let ((instance (ocml::find-current-instance web-service)))
      (when instance
        (ocml::select-ontology (ocml::name (ocml::home-ontology instance)))
        (let* ((goal-instance
                (web-onto::findany 
                 '?x 
                 `(ocml::suitable-web-service ?x ,web-service)))
               (goal-to-mediate
                (web-onto::findany 
                 '?x 
                 `(ocml::has-goal ,goal-instance ?x)))
               (ontology-for-mediation
                (web-onto::findany 
                 '?x 
                 `(ocml::has-mediation-ontology ,goal-instance ?x)))
               (web-service-to-mediate
                (web-onto::findany 
                 '?x 
                 `(ocml::has-web-service ,goal-instance ?x)))
               mediators wg-mediators oo-mediators ;;used-mediator mediation-service
               (new-oo-mediator-ontologies nil)
               new-oo-mediator-ontology
               (input-values
                (web-onto::findany 
                 '?x 
                 `(ocml::has-input-role-values ,goal-instance ?x)))
               (input-roles                 (web-onto::findany 
                 '?x 
                 `(ocml::has-input-roles ,goal-instance ?x))))
          (ocml::select-ontology ontology-for-mediation)
          (setf mediators
                (web-onto::findany 
                 '?x 
                 `(ocml::wsmo-web-service-all-used-mediators-for-goal
                   ,web-service-to-mediate ,goal-to-mediate ?x))
                wg-mediators
                (mapcan #'(lambda (x) (when (wg-mediator-p x)
                                        (list x)))
                        mediators)
                oo-mediators
                (mapcan #'(lambda (x) (when (oo-mediator-p x)
                                        (list x)))
                        mediators))
          (mapc #'(lambda (oo-mediator)
                    (multiple-value-setq (new-oo-mediator-ontology input-values)
                      (run-oo-mediator oo-mediator goal-instance input-roles input-values))
                      (setf input-values input-values)
                      (push new-oo-mediator-ontology new-oo-mediator-ontologies))
                oo-mediators)
          (mapc #'(lambda (wg-mediator)
                    (setf input-values 
                          (run-wg-mediator ontology-for-mediation ;;ontology
                                           goal-to-mediate web-service
                                           instance
                                           wg-mediator input-values)))
                wg-mediators)
          (list input-values 
                  (if new-oo-mediator-ontologies
                      (create-combined-oo-mediator-ontologies 
                       web-service-to-mediate 
                       (remove-duplicates
                        (cons ontology-for-mediation new-oo-mediator-ontologies)))
                  ontology-for-mediation)))))))

(defun run-wg-mediator (ontology goal-to-mediate web-service web-service-instance
                                 wg-mediator input-role-values)
  (let ((mediation-service
         (web-onto::findany 
          '?x 
          `(and (ocml::wsmo-mediator-mediation-service
                 ,wg-mediator ?x)))))
    (if mediation-service
        (let* ((web-service-input-roles
                (and (ocml::get-domain-class web-service)
                     (local-input-roles web-service)))
               (input-roles
                (input-roles goal-to-mediate))
               (mediator-output-role
                (output-role mediation-service))
               (mediator-input-roles
                (input-roles mediation-service))
               (input-roles-to-mediate
                (intersection input-roles mediator-input-roles))
               mediated-values)            
          ;;(format t "mediator ~s~%" goal-to-mediate)
          (setf mediated-values                          
                (ip::internal-solve-goal 
                 ontology mediation-service 
                 (mapcar 
                  #'(lambda (input-role-to-mediate)
                      (list input-role-to-mediate 
                            (elt input-role-values 
                                 (position input-role-to-mediate input-roles))))
                  input-roles-to-mediate)))
          (when (find mediator-output-role web-service-input-roles)
            (ocml::tell1 `(,mediator-output-role ,web-service-instance ,mediated-values)))
          (when (= (length input-roles-to-mediate) 1)
            ;;if only one input role is mediated then
            ;;only a single value is returned 
            ;;otherwise a list
            (setf mediated-values (list mediated-values)))
           (mapcar 
            #'(lambda (input-role)
                (if (find input-role input-roles-to-mediate)
                    (elt mediated-values (position input-role input-roles-to-mediate))
                  (elt input-role-values (position input-role input-roles))))
            input-roles))
      input-role-values)))

;;;this is not used currently
(defun ocml::identity-input-role-values-mediator (ontology web-service)
  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology ontology)
    (let ((instance (ocml::find-current-instance web-service)))
      (when instance
        (ocml::select-ontology (ocml::name (ocml::home-ontology instance)))
        (let ((input-role-values
               (web-onto::findany 
                '?x 
                `(and (ocml::suitable-web-service ?goal-inst ,web-service)
                      (ocml::has-input-role-values ?goal-inst ?x)))))
          input-role-values)))))
