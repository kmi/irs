(in-package wsmo-protocol)

(defvar *choreography-result* nil)

(defstruct (wsmo-grounding (:type list))
  type operation-mappings)

(defstruct (operation-mappings (:type list))
  name implementation-info input-soap-bindings output-soap-binding)

(defun get-operation-grounding-type (grounding operation)
  (wsmo-grounding-type (get-operation-grounding grounding operation)))

(defun get-operation-grounding (web-service-grounding operation)
  (find operation web-service-grounding 
        :key 
        #'(lambda (x) (operation-mappings-name (wsmo-grounding-operation-mappings x)))))

(defun get-operation-external-method-name (web-service-grounding operation)
  (let ((operation-grounding
         (get-operation-grounding web-service-grounding operation)))
    (operation-mappings-implementation-info (wsmo-grounding-operation-mappings operation-grounding))))
;;  (second (assoc operation operation-mappings)))

(defun get-operation-input-mapping (web-service-grounding operation)
  (let ((operation-grounding
         (get-operation-grounding web-service-grounding operation)))
    (operation-mappings-input-soap-bindings (wsmo-grounding-operation-mappings operation-grounding))))
;;  (third (assoc operation operation-mappings)))

(defun get-operation-output-mapping (web-service-grounding operation)
  (let ((operation-grounding
         (get-operation-grounding web-service-grounding operation)))
    (operation-mappings-output-soap-binding (wsmo-grounding-operation-mappings operation-grounding))))
;;  (fourth (assoc operation operation-mappings)))

(defvar *java-class-method-name-separator* "_")

(defun generate-invocation-item-string (x)
  (cond ((stringp x)
         x)
        ((symbolp x)
         (string-downcase (symbol-name x)))))

#|
(defun make-invocation-name-for-java-operation (implementation-info)
  (concatenate 'string (generate-invocation-item-string (car implementation-info))
               *java-class-method-name-separator*
               (generate-invocation-item-string (second implementation-info))))
|#

(defun make-invocation-name-for-java-operation (implementation-info)
  (declare (ignore implementation-info))
  "invoke_java_class")


(defun make-invocation-name-for-web-service-operation (web-service-grounding operation)
  ;; 2007/02/26 Dave: Changed from a case on TYPE to a call to
  ;; make-invocation-name-for-web-service-operation2 methods
  ;; specialised on TYPE.  This means we can put the methods in
  ;; grounding-specific files, although only the XML-RPC one currently
  ;; does so.
  (let* ((operation-grounding (get-operation-grounding web-service-grounding operation))
         (type (wsmo-grounding-type operation-grounding))
         (implementation-info (operation-mappings-implementation-info 
                               (wsmo-grounding-operation-mappings operation-grounding))))
    (make-invocation-name-for-web-service-operation2 type implementation-info)))

(defgeneric make-invocation-name-for-web-service-operation2 (type implementation-info)
  (:documentation "Return string denoting the method name or
equivalent for the grounding TYPE."))

(defmethod make-invocation-name-for-web-service-operation2 ((type (eql 'ocml::grounded-to-lisp))
                                                            implementation-info)
  (string-downcase (symbol-name implementation-info)))

(defmethod make-invocation-name-for-web-service-operation2 ((type (eql 'ocml::grounded-to-http))
                                                            implementation-info)
  nil)

(defmethod make-invocation-name-for-web-service-operation2 ((type (eql 'ocml::grounded-to-java))
                                                            implementation-info)
  (make-invocation-name-for-java-operation implementation-info))

(defmethod make-invocation-name-for-web-service-operation2 ((type (eql 'ocml::grounded-to-wsdl))
                                                            implementation-info)
  (if (symbolp (second implementation-info))
      (string-downcase (symbol-name (second implementation-info)))
    (second implementation-info)))

(defun get-implementation-info-for-web-service-operation (web-service-grounding operation)
  (let ((operation-grounding
         (get-operation-grounding web-service-grounding operation)))
    (operation-mappings-implementation-info 
     (wsmo-grounding-operation-mappings operation-grounding))))

(defun ocml::internal-end-choreography ()
  (throw 'ocml::%%halt%% :halt)) ;;;from HALT-FC

(defun combine-input-role-value-pairs (operation-mappings existing-role-value-pairs new-role-value-pairs)
  (mapcar #'(lambda (role-name)
              (list role-name
                    (if (assoc role-name new-role-value-pairs)
                        (second (assoc role-name new-role-value-pairs))
                      (second (assoc role-name existing-role-value-pairs)))))
          (mapcar #'car operation-mappings)))

(defun ocml::internal-invoke-web-service-from-rule-and-assert
       (choreography-state &optional 
                           operation
                           new-input-role-value-pairs
                           web-service
                           grounding
                           host port location)
  (let* ((web-service 
          (or web-service
              (ocml::findany '?x `(ocml::has-web-service-class ,choreography-state ?x))))
         (host 
          (or host
              (ocml::findany '?host `(ocml::has-web-service-host ,choreography-state ?host))))
         (port 
          (or port 
              (ocml::findany '?port `(ocml::has-web-service-port ,choreography-state ?port))))
         (location 
          (or location (ocml::findany '?location 
                                      `(ocml::has-web-service-location ,choreography-state ?location))))
         (grounding
          (or grounding
              (ocml::findany '?grounding
                       `(ocml::has-grounding ,choreography-state
                                             ?grounding))))
         (input-role-value-pairs
          (combine-input-role-value-pairs 
           (get-operation-input-mapping grounding operation)
           (ocml::findany '?input-role-value-pairs
                    `(ocml::has-input-role-value-pairs ,choreography-state ?input-role-value-pairs))
           new-input-role-value-pairs)))
    ;;(format t "internal-invoke-web-service-from-rule-and-assert ~a~%" (ocml::name ocml::*current-ontology*))
    (ocml::describe-instance choreography-state)
    ;;(format t "~a~%" (list web-service host port (ocml::findany '?port `(ocml::has-port ,choreography-state ?port)) location (ocml::findany '?x `(ocml::has-location ,choreography-state ?x)) grounding ))
    (setf *choreography-result*
          (invoke-soap-client-with-visualization
           web-service
           input-role-value-pairs
           (get-operation-input-mapping grounding operation)
           (mapcar #'(lambda (x) (car (last x))) input-role-value-pairs)
           host port location (get-operation-output-mapping grounding operation)
           grounding operation
           ))
    ;;(setf d *choreography-result*)
    (ocml::unassert1 `(ocml::has-choreography-result ,choreography-state ?x))
    ;;(format t "asserting in ontology ~a~%" (name *current-ontology*))
    (assert-all-operation-inputs operation input-role-value-pairs)
    (assert-result choreography-state operation *choreography-result*)
    ;;(format t "all output ~a~%" (setofall '?x `(operation-output ,operation ?x)))
    *choreography-result*))

(defun assert-all-operation-inputs (operation input-role-value-pairs)
  (ocml::unassert1 `(ocml::sent-message-input ,operation ?x))
  (ocml::unassert1 `(ocml::operation-input ,operation ?x ?y))
  (ocml::tell1
     `(ocml::sent-message-input ,operation ,input-role-value-pairs))
  (mapc #'(lambda (input-role-value-pair)
            (ocml::tell1
             `(ocml::operation-input ,operation ,(car input-role-value-pair) 
                                     ,(second input-role-value-pair))))
        input-role-value-pairs))

(defvar *irs-error-strings*
  '(("error: " error) ("irs error: " irs-error)))

(defun error-result-p (result)
  (setf result (string-downcase result))
  (dolist (error-string-info *irs-error-strings*)
    (let ((error-string (car error-string-info)))
      (when (and (>= (length result)
                    (length error-string))
                 (string= error-string (subseq result 0 (length error-string))))
        (return-from error-result-p error-string-info)))))
  
(defun get-error-type (error-info result)
  (handler-case 
      (let ((*package* (find-package "OCML")))
        (read-from-string (subseq result (length (car error-info)))))
    (error
     (c)
     (second error-info))
    (serious-condition 
     (c)
     (second error-info))))
    

(defun assert-result (choreography-state operation result)
  (let ((error-info (error-result-p result)))
    (if error-info
        (assert-error-result choreography-state operation result (get-error-type error-info result))
      (assert-normal-result choreography-state operation result))))

(defun assert-normal-result (choreography-state operation result)
  (let ((lifted-result (ocml::findany '?x `(ocml::lift ,result ?x))))
    (ocml::tell1
     `(ocml::has-choreography-result ,choreography-state 
                               ,lifted-result))
    (ocml::tell1
     `(ocml::received-message ,operation ,lifted-result))))


(defun assert-error-result (choreography-state operation result error-type)
  (ocml::tell1
     `(ocml::has-choreography-result ,choreography-state ,result))
  (ocml::tell1
   `(ocml::received-error ,operation ,result ,error-type)))