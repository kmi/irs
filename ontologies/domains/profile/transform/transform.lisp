
(in-package ocml)

;;;;IMPORTANT, JOHN'S SOLUTION FOR MAPPING
;;;; (def-rule family-name-mapping-rule 
;;;;   ((has-family-name ?inst ?x) if 
;;;;    (haslastname ?inst ?x)))

;;;; (def-rule full-name-mapping-rule 
;;;;   ((has-full-name ?inst ?full-name) if 
;;;;    (haslastname ?inst ?x)
;;;;    (hasfirstname ?inst ?y)
;;;;    (= ?full-name))


(import 'ocml::*md-db*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; metadata database access functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun *md-db*-access-get (rll &optional (db *md-db*))
  "basic access"
  (let ((res db)
        (get-f #'(lambda (k l) (second (assoc k l)))))
    (dolist (r rll res)
    ;(format t "~a~%" l)
      (setf res (funcall get-f r res)))
    ))

(defun *md-db*-access-get-value (rll  &optional (db *md-db*))
  "basic access get interface"
  (let ((res  (*md-db*-access-get rll db)))
    (when (or (listp (caar res))
        (< (length (car res)) 2 )
        (not (cadadr res))) (first res))
    
  ))



(defun *md-db*-access-ask (query rll &optional (db *md-db*))
  "possible queries are (list-nodes: list all nodes from a rll) (get-all: gets subtree from an rll)"
  (case query
    ('list-nodes (let ((res (*md-db*-access-get rll db)))
                   (mapcar 'first res)))
    ('get-all (*md-db*-access-get rll db))
    ))

;(*md-db*-access-get '(transform ocml-xsd struc classes profile-fam))
;(*md-db*-access-get '(transform ocml-xsd struc classes profile-fam Sex enumeration)) ((f (m |Male|)))
;(*md-db*-access-get-value '(transform ocml-xsd))
;(*md-db*-access-get-value '(transform ocml-xsd struc classes profile-fam Sex enumeration))
;(*md-db*-access-get-value '(transform ocml-xsd struc types string))
;(*md-db*-access-get-value '(transform ocml-xsd struc classes profile-fam FirstName maxLength))
;(*md-db*-access-get-value '(transform ocml-xsd struc classes profile-fam))
;(*md-db*-access-ask 'list-nodes '(transform ocml-xsd struc))
;(*md-db*-access-ask'list-nodes '(classes) (*md-db*-access-ask 'get-all '(transform ocml-xsd struc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; metadata database interface access
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *md-access-interface* 
  '((get *md-db*-access-get-value)
    (ask *md-db*-access-ask)
    (db  *md-db*)))

(defun *mdai*-get (key )
  (second (assoc key *md-access-interface*)))

(defun md-get-value (rll &optional (db (*mdai*-get 'db)))
  "generic function to access a md value, to be specialized over particular md types"
  (funcall (*mdai*-get 'get) rll (eval db))
)

(defun md-get-par-value (rll &optional (db (*mdai*-get 'db)))
  "generic function to access a md value, from a parametric path"
  (funcall (*mdai*-get 'get) (second rll) (eval db))
)

(defun md-ask-value (query rll &optional (db (*mdai*-get 'db)))
  "generic function to access a md value, to be specialized over particular md types"
  (funcall (*mdai*-get 'ask) query rll (eval db))
)

;(md-ask-value 'list-nodes '(transform ocml-xsd struc classes profile-fam))
;(md-ask-value 'get-all '(transform ocml-xsd struc classes profile-fam))
;(setf stree (md-ask-value 'get-all '(transform ocml-xsd)))
;(setf stree-2 (md-ask-value 'get-all '(struc classes profile-fam) 'stree))
;(md-get-value '(FirstName maxLength) 'stree-2)


