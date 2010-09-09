(in-package :kmi.vt.xpath-mini)

(defmacro lower ((&key instance) &body exprs)
  (let* ((output (gensym))
         (expr (gensym)))
    (error "XXX xpm::lower not implemented.")))

(defun build-lower (instance expr)
  (cond ((listp instance) ;;vlad 25/06/06 to handle e.g. (LOWER-USER '(INSTANCE3121 INSTANCE3129))
         (loop for i in instance do (build-lower i expr)))
        ;;vlad 01/06/06 to allow arbitrary functions introduced by eval in lowering
        ((eql (first expr) 'eval)
         `(lower (:instance ',instance) ',(list expr)))
        (t
         `(lower (:instance ',instance) ',(recure-expr instance expr)))))

(defun recure-expr (instance exprs) 
;;  (print instance)
;;  (print exprs)
  (let ((res nil))
    (loop for ex in exprs
          ;;simple case, tag -> slot value
          when (and (stringp (first ex)) (stringp (second ex))) do (setf res (append res (list ex)))

          ;;complex case , tag -> other tags          
          when (and (stringp (first ex)) (listp (second ex))) do (setf res (append res (list (list (first ex) (build-lower instance (second ex))))))
          ;;e.g. ("hasAddres" (lower-new-address "has-new-address"))
          when (symbolp (first ex)) do (let* ((fname (first ex))
                                              (arg (second ex))
                                              (inst (first (o-xp (concatenate 'string "/" arg) (list instance))))) ;; here is a list of instances
                                         (setf res (append res (cond ((listp inst) (loop for i in inst
                                                                     collect (first (list `(,fname ',i)))))
                                                                     (t (list (first (list `(,fname ',inst))))))));;iterated in order to capture the possible multiple instances (Alessio 17/02/2006)

                                         ));;(print fname)) )
   ;(print res)
    res))


(defmacro ocml::define-lower (name class (&key services) &body exprs)
  `(progn
     ,(if services
          `(dolist (ws ',services)
             do (ocml::tell1 (list 'ocml::lower-defined ',class ws ',name)))
          `(ocml::tell1 '(ocml::lower-defined-all ,class ,name)))
     ,(if (or (functionp (car exprs))
              (and (listp (car exprs)) (eq (caar exprs) 'lambda)))
          `(setf (symbol-function ',name) ,(car exprs))
          `(defun ,name (inst)
             (eval (build-lower inst ,(first exprs)))))))

(defmacro ocml::deflower (name class &body exprs)
  `(ocml::define-lower ,name ,class () ,@exprs))

(defun ocml::lower-instance-name ()
  "vlad 01/03/06 to give an unique 
   name to spatial objects, reused in the client"
  (symbol-name (car *context*)))

(defun ocml::lower-instance-class-name ()
  "vlad 07/03/06 "  
  (symbol-name (eval `(ocml::ocml-eval (ocml::the-parent ,(car *context*))))))

;;http://irs.open.ac.uk/emergencyGisDomain#restCentre

(defun deOCMLize (s)
  "vlad 04/09/06 
  makes a string adapted to wsmo representation
  e.g. TOTO-TATA-TITI -> totoTataTiti"
    (string-downcase (remove #\- (string-capitalize  s :start 0)) :end 1))

;; (deOCMLize "TOTO-TATA-TITI")

(defun ocml::lower-instance-name-as-wsml-id (&optional instance)
  "vlad 04/09/06 "  
  (let* ((inst (or instance *context*))
         (instance (if (listp inst) (car inst) inst))
         ;;(cl (eval `(ocml::ocml-eval (ocml::the-parent ,instance))))
         (ontology-name (deOCMLize (ocml::name (ocml::home-ontology (ocml::find-current-instance instance))))))
    (concatenate 'string "http://irs.open.ac.uk/" ontology-name "#" 
                 (deOCMLize (symbol-name instance)))))

(defun ocml::lower-instance-class-name-as-wsml-id (&optional instance)
  "vlad 04/09/06 "  
  (let* ((inst (if instance instance *context*))
         (instance (if (listp inst) (car inst) inst))
         (cl (eval `(ocml::ocml-eval (ocml::the-parent ,instance))))
         (ontology-name (deOCMLize (ocml::name (ocml::home-ontology (ocml::get-ocml-class cl))))))
    (concatenate 'string "http://irs.open.ac.uk/" ontology-name "#" 
                 (deOCMLize (symbol-name cl)))))

#+:lispworks 
(editor::setup-indent 'deflower 0 2)

#|
(defun upto (max)
  (let ((result nil))
    (dotimes (i max)
      (push i result))
    (nreverse result)))

(setf l '(a b c d e f g h i))

(defun modif (l)
  (let ((res nil))
    (do ((i 1 (1+ i)))
        ((> i (length l)))
      (let ((el (elt  l (- i 1))))
        (push el res)
        (if (= 0 (mod i 3))
            (push el res))))
    (nreverse res)))

(modif l)

|#


