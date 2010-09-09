(in-package wsmo-protocol)


(defvar *mediator-component-keyword-to-relation*
  '((:source ocml::wsmo-mediator-source-component) 
    (:target ocml::wsmo-mediator-target-component)
    (:service ocml::wsmo-mediator-mediation-service)
    (:non-functional-properties ocml::wsmo-mediator-non-functional-properties)
    (ocml::source ocml::wsmo-mediator-source-component) 
    (ocml::target ocml::wsmo-mediator-target-component)
    (ocml::service ocml::wsmo-mediator-mediation-service)
    (ocml::non-functional-properties ocml::wsmo-mediator-non-functional-properties)))

(defun get-mediator-component (component-type mediator)
  (let ((mediator-component-relation (second (assoc component-type *mediator-component-keyword-to-relation*))))
    (web-onto::findany '?x `(,mediator-component-relation ,mediator ?x))))

(defun satisfies-mediator-filters-p (mediator mediator-filters)
  (do ((current-mediator-filters mediator-filters (cdr current-mediator-filters)))
      ((or (null current-mediator-filters) 
           (not (satisfies-mediator-filter-p mediator (car current-mediator-filters))))
       (not current-mediator-filters))))

(defun satisfies-mediator-filter-p (mediator mediator-filter)
  (destructuring-bind (mediator-component-type relation additional-arguments) mediator-filter
    (let ((component (get-mediator-component mediator-component-type mediator)))
      (or (null relation)
          (and component
               (apply 'ocml::holds? relation component additional-arguments))))))

(defun find-mediators (ontology &optional (mediator-type 'ocml::mediator) mediator-filters)
  (ocml::select-ontology ontology)
  (mapcan #'(lambda (mediator)
              (when (satisfies-mediator-filters-p mediator mediator-filters)
                  (list mediator)))
          (ocml::setofall '?mediator `(ocml::subclass-of ?mediator ,mediator-type))))

;;;(find-mediators 'ocml::wsmo-use-case 'ocml::mediator '((:source nil nil)))

;;(find-mediators 'ocml::wsmo-use-case 'ocml::mediator '((:source (ocml::kappa (?x)   (and (ocml::has-wsmo-input-role ?x ?y)         (ocml::class-slot-type ?x ?y ocml::person))) nil)))

;;(find-mediators 'ocml::wsmo-use-case 'ocml::mediator '((:source (ocml::kappa (?x ?z)   (and (ocml::has-wsmo-input-role ?x ?y)         (ocml::class-slot-type ?x ?y ocml::person))) '(3))))

;;(find-mediators 'ocml::wsmo-use-case 'ocml::mediator '((:non-functional-properties (ocml::kappa (?x)   (=  "john domingue" (ocml::the-class-slot-value ?x ocml::has-owner))) nil)))

;;(ip::handle-post-soap-service 'user::find-mediator *standard-output* "" '(ocml::wsmo-use-case ocml::mediator ((:non-functional-properties (ocml::kappa (?x)   (=  "john domingue" (ocml::the-class-slot-value ?x ocml::has-owner))) nil))))

;;(ip::handle-post-soap-service 'user::find-mediator *standard-output* "" '(ocml::wsmo-use-case ocml::mediator ((:source nil nil))))

;;(ip::handle-post-soap-service 'user::find-mediator *standard-output* "" '(ocml::wsmo-use-case ocml::mediator ((:source (ocml::kappa (?x)   (and (ocml::has-wsmo-input-role ?x ?y)         (ocml::class-slot-type ?x ?y ocml::person))) nil))))

;;(ip::handle-post-soap-service 'user::find-mediator *standard-output* "" '(ocml::wsmo-use-case ocml::mediator ((:source (ocml::kappa (?x ?z)   (and (ocml::has-wsmo-input-role ?x ?y)         (ocml::class-slot-type ?x ?y ocml::person))) '(3)))))

