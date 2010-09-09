;;; Mode: Lisp; Package: cl-user

(in-package cl-user)

(defun select-elms-goals-ontology ()
  (ocml::select-ontology 'ocml::elms-goals))


(defun catalogue-entry-by-weight-interface-out (input)
  ;;(setf ii input)
  (ocml::with-ocml-thread-safety ()
    (setf input (make-ocml-symbol input))
    (select-elms-goals-ontology)
    'catalogue-entry-by-weight-interface-out))

(defun redirect-equipment-to-new-address-interface-out (input)
  (ocml::with-ocml-thread-safety ()
    (setf input (make-ocml-symbol input))
    (select-elms-goals-ontology)
    'redirect-equipment-to-new-address-interface-out))

(defun return-equipment-interface-out (input)
  (ocml::with-ocml-thread-safety ()
    (setf input (make-ocml-symbol input))
    (select-elms-goals-ontology)
    'return-equipment-interface-out))


(eval-when (eval load)
  (irs-wsmo-web-service-registration 
   elms-goals catalogue-entry-by-weight-web-service)
  (irs-wsmo-web-service-registration 
   elms-goals 
   redirect-equipment-to-new-address-web-service)
  (irs-wsmo-web-service-registration 
   elms-goals 
   return-equipment-web-service))

