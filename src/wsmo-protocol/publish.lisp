(in-package wsmo-protocol)

(defun web-service-host (web-service)
  (web-onto::findany '?x `(ocml::wsmo-web-service-host ,web-service ?x)))

(defun web-service-port (web-service)
  (web-onto::findany '?x `(ocml::wsmo-web-service-port ,web-service ?x)))                       

(defun ip::internal-publish-wsmo-web-service (user-name ontology web-service-name
                                                        web-service-host web-service-port 
                                                        web-service-location)
  (ocml::with-ocml-thread-safety ()
    (ocml::select-ontology ontology)
    (if (ocml::get-domain-class web-service-name)
        (if (ocml::holds? 'ocml::subclass-of web-service-name 'ocml::web-service)
            (let ((input-roles (ip::input-roles-with-soap-bindings web-service-name))
                  (output (second (output-role-with-soap-binding web-service-name))))
              (when (and output (string output))
                (setf output (intern (string-upcase output) (find-package "OCML"))))                
              (let ((associated-interface 
                     (ocml::findany '?x 
                                    `(ocml::associated-interface ,web-service-name ?x))))
                (if associated-interface
                    (let ((associated-publisher-information-class-name
                           (ocml::findany 
                            '?x 
                            `(ocml::has-associated-web-service-interface ?x ,associated-interface))))
                      (when (ocml:nothing? associated-publisher-information-class-name)
                        (setf associated-publisher-information-class-name
                              (generate-publisher-information-name-for-web-service web-service-name))
                        (update-publisher-information user-name ontology 
                                                      associated-publisher-information-class-name
                                                      associated-interface
                                                      web-service-host 
                                                      web-service-port 
                                                      web-service-location))
                      (visualiser:refresh))
                  (values t (format nil "Couldn't find an inteface for the web service ~a" web-service-name)))))
          (values t (format nil "~a not a web service" web-service-name)))
      (values t (format nil "~a not a class" web-service-name)))))


(defun generate-publisher-information-name-for-web-service (web-service)
  (generate-name-for-web-service web-service "-PUBLISHER-INFORMATION"))

(defun generate-interface-name-for-web-service (web-service)
  (generate-name-for-web-service web-service "-INTERFACE"))

(defun generate-choreography-name-for-web-service (web-service)
  (generate-name-for-web-service web-service "-CHOREOGRAPHY"))

(defun generate-orchestration-name-for-web-service (web-service)
  (generate-name-for-web-service web-service "-ORCHESTRATION"))

(defun generate-problem-solving-pattern-name-for-orchestration (orchestration)
  (generate-name-for-web-service orchestration "-PROBLEM-SOLVING-PATTERN"))

#| 

let's always generate the name with the root to make things cleaner
(defun generate-name-for-web-service (web-service root)
  (let ((name (intern (concatenate 'string (symbol-name web-service) root)
                      (find-package "OCML"))))
    (if (ocml::get-domain-class name)
        (intern (symbol-name (gensym (concatenate 'string (symbol-name web-service) root)))
                (find-package "OCML"))
      name)))
|#

(defun generate-name-for-web-service (web-service root)
  (intern (concatenate 'string (symbol-name web-service) root)
                      (find-package "OCML")))

(defun publish-wsmo-web-service-with-new-interface (user-name 
                                                    ontology web-service-name 
                                                    web-service-host web-service-port 
                                                    web-service-location)
  (let ((interface
         (generate-interface-name-for-web-service web-service-name))
        (publisher-information
         (generate-publisher-information-name-for-web-service web-service-name)))
    (new-interface ontology interface publisher-information)
    (new-publisher-information user-name ontology publisher-information 
                      web-service-host 
                      web-service-port 
                      web-service-location)
    (add-interface-to-web-service ontology web-service-name interface)))



(defun publish-wsmo-web-service-with-new-publisher-information (user-name ontology web-service-name
                                                                 interface web-service-host 
                                                                 web-service-port 
                                                                 web-service-location)
  (let ((publisher-information
         (generate-publisher-information-name-for-web-service web-service-name)))
    (new-publisher-information user-name ontology publisher-information 
                      web-service-host 
                      web-service-port 
                      web-service-location)
    (add-publisher-information-to-interface ontology interface publisher-information)))

(defun get-parents (x)
  (ocml::setofall '?x `(ocml::direct-superclass-of ?x ,x)))

(defun add-publisher-information-to-interface (ontology interface publisher-information)
  (add-slot-value-to-class ontology interface '(ocml::interface)
                           'ocml::has-publisher-information publisher-information))

(defun add-interface-to-web-service (ontology web-service interface)
  (add-slot-value-to-class ontology web-service '(ocml::web-service)
                           'ocml::has-interface interface))

(defun add-slot-value-to-class (ontology class-name class-parents-default-value 
                                         slot-name value)
  (let* ((*package* (find-package "OCML"))
         (class-parents 
         (or (get-parents class-name)
             class-parents-default-value))
        (all-local-slots 
         (remove slot-name
                 (web-onto::findany '?x `(= ?x (ocml::local-class-slots ,class-name)))))
        (all-local-slots-with-values
         (mapcar #'(lambda (slot)
                     (cons slot (mapcan 
                                 #'(lambda (slot-value)
                                     (list :value slot-value))
                                 (web-onto::findany 
                                   '?x 
                                   `(= ?x (ocml::all-class-slot-values ,class-name ,slot))))))
                 all-local-slots)))
    (ip::add-irs-knowledge-level-definition 
     ontology
     class-name
     (format nil 
             "(def-class ~(~a~) ~(~a~)~%  (~(~{~s~%   ~}(~a :value ~a~))))" 
             class-name class-parents 
             all-local-slots-with-values
             slot-name value))))
          

(defun new-publisher-information (user-name ontology publisher-information
                                   web-service-host 
                                   web-service-port 
                                   web-service-location)
  (update-publisher-information user-name ontology publisher-information 
                       web-service-host 
                       web-service-port 
                       web-service-location))

;;;currently update assumes we can overwrite publisher-information because 
;;;it only uses the slots host, port and location
(defun update-publisher-information (user-name ontology publisher-information 
                                               interface
                                     web-service-host 
                                     web-service-port 
                                     web-service-location)
  (let ((publisher-information-parents 
         (or (car (get-parents publisher-information)) ;;assume one parent
             *default-publisher-information-parent*)))
    (save-standard-wsmo-description 
     *standard-output* (list user-name publisher-information ontology 
                             publisher-information-parents interface
                             web-service-host 
                             web-service-port 
                             web-service-location)
     *publisher-information-properties*
     'publisher-information
     *default-publisher-information-parent*)))


(defun new-interface (ontology interface publisher-information)
  (ip::add-irs-knowledge-level-definition 
   ontology
   interface
  (format nil 
          "(def-class ~(~a~) (interface)~%  ((has-publisher-information :value ~(~a~))))"
          interface publisher-information)))