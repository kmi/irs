;;; Mode: Lisp; Package: cl-user

(in-package cl-user)


(defun multiply (x y)
  (* x y))

(defun greater-than (x y)
  (> x y))

(defun orchestration-mediation (x)
  x)

(eval-when (eval load)
  (irs-wsmo-web-service-registration if-test-ontology ;;this is the ontology name
                                     multiply-web-service  
                                     )
    (irs-wsmo-web-service-registration if-test-ontology ;;this is the ontology name
                                     greater-than-web-service  
                                     )
  
                                     
  )

