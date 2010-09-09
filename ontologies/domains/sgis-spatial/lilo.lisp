;; Mode: Lisp; Package: ocml

;;; Vlad
;;; this file contains all lifting and lowering functions
 
(in-package "OCML")

(in-ontology sgis-spatial)

#|
(def-function get-spatial-object-attributes (?spatial-object)
  :lisp-fun
  #'(lambda (spatial-object)
      (when (holds? 'instance-of spatial-object 'spatial-object)
        (let ((slots (set-difference 
                      (findany '?x `(and (instance-of ,spatial-object ?class)
                                         (= ?x (all-class-slots ?class))))
                      (findany '?x '(= ?x (all-class-slots spatial-object))))))
          (mapcan #'(lambda (slot)
                      (let ((values
                             (findany '?x `(= ?x (all-slot-values ,spatial-object ,slot)))))
                        (when values
                          (list (cons slot values)))))
                  slots)))))
|#

(def-function get-spatial-object-attributes (?spatial-object)
  :lisp-fun
  #'(lambda (spatial-object)
      (when (holds? 'instance-of spatial-object 'spatial-object)
        (let* ((obj-attributes (findany '?x `(and (instance-of ,spatial-object ?class)
                                         (= ?x (all-class-slots ?class)))))
               (so-attributes (findany '?x '(= ?x (all-class-slots spatial-object))))           
               (slots (cons 'HAS-AFFORDANCES (set-difference obj-attributes so-attributes)))) ;;vlad 25/09 added has-affordances
          (mapcan #'(lambda (slot)
                      (let ((values
                             ;;(findany '?x `(= ?x (all-slot-values ,spatial-object ,slot)))))
                             (setofall '?x `(= ?x (all-slot-values ,spatial-object ,slot)))))
                        (when values
                          (list (cons slot values)))))
                  slots)))))

;;(def-class spatial-object-attribute ()
;;  ((has-name :type string)
;;   (has-value :type string)))

;move to sgis space

(def-function get-spatial-object-archetype-list (?spatial-object)
  :lisp-fun
  #'(lambda (spatial-object)
      (when (and (holds? 'instance-of spatial-object 'spatial-object)
                 (holds? 'instance-of spatial-object 'archetype))
        ;;(findany '?x `(= ?x (the-parent-new ,spatial-object 'archetype))))))
        (let ((arch-classes
               (intersection
                (findany '?x `(= ?x (all-superclasses (the-parent-new ,spatial-object 'archetype))))
                (findany '?x `(= ?x (all-subclasses 'archetype))))))
          (reverse arch-classes)))))

(def-class hci () ())

(def-class google-maps-hci (hci) 
  ((has-hover-content :type string)
   (has-hidden-attributes :type list)))

#|
(lower-hci 'v2)
|#

(def-function get-only-slots-value-of-parent (?the-instance ?the-parent)
  :documentation "gets the slots of the specified parent"
  :lisp-fun
  #'(lambda (the-instance the-parent)
      (let ((slots (intersection
                    (findany '?x `(and (instance-of ,the-instance ?class)
                                       (= ?x (all-class-slots ?class))))
                    (findany '?x `(= ?x (all-class-slots ,the-parent))))))
        (mapcan #'(lambda (slot)
                      (let ((values
                             (findany '?x `(= ?x (all-slot-values ,the-instance ,slot)))))
                        (when values
                          (list (cons slot values)))))
                  slots))))



(defun sgis-lift-point-location-from-string (s) ;;e.g. lat=51.876&amp;long=0.137
  "vlad 03/03/06 lifts to a puntual location"
  (declare (special OCML-CTX-INSTANCE))
  (xpm::with-ocml-ctx (:ontology (ocml::name ocml::*current-ontology*)
                       :class 'point-location
                       :instance (xpm::o-instance))
    (let ((lat (second (xpath-mini::split (first (xpath-mini::split s #\&)) #\=)))
          (long (second (xpath-mini::split (second (xpath-mini::split s #\&)) #\=))))
      ;;(princ lat) (princ long)
      (when (and lat long)
          (progn
            (xpm::o-set-slot 
             'HAS-POINTS 
             (list (xpm::with-ocml-ctx (:class 'longitudinal-point
                                        :instance (xpm::o-instance))
                     (xpm::o-set-slot 'HAS-LATITUDE lat)
                     (xpm::o-set-slot 'HAS-LONGITUDE long)
                     xpm::OCML-CTX-INSTANCE)))
            xpm::OCML-CTX-INSTANCE))
      )))

(defun sgis-lift-point-location (&key lat-xpath long-xpath)
  "vlad 03/03/06 lifts to a puntual location"
  (declare (special OCML-CTX-INSTANCE))
  (xpm::with-ocml-ctx (:ontology (ocml::name ocml::*current-ontology*)
                       :class 'point-location
                       :instance (xpm::o-instance))
    (let ((lat (xpm::x-xp lat-xpath))
          (long (xpm::x-xp long-xpath)))
      (xpm::o-set-slot 
       'HAS-POINTS 
       (list (xpm::with-ocml-ctx (:class 'longitudinal-point
                                  :instance (xpm::o-instance))
               (xpm::o-set-slot 'HAS-LATITUDE lat)
               (xpm::o-set-slot 'HAS-LONGITUDE long)
               xpm::OCML-CTX-INSTANCE))))
    xpm::OCML-CTX-INSTANCE))

(defun sgis-lower-spatial-object-attributes ()
  (declare (special xpm::CONTEXT))
  (let ((attr-list (eval `(ocml-eval (get-spatial-object-attributes ,(car  xpm::CONTEXT)))))
        (sres nil))
    (loop for attr in attr-list do
          (let ((attr-string (let ((out-name (make-string-output-stream))
                                    (out-val (make-string-output-stream))
                                    (val-list (second attr)))
                               ;;(if (and (not (atom val-list)) (> (length val-list) 1))
                                   ;;multiple values
                                   (let* ((out-name-str (progn (princ (first attr) out-name)(get-output-stream-string out-name)))
                                         );;(out-val-str (progn (princ (second attr) out-val)(get-output-stream-string out-val))))
                                     (format nil "<name>~a</name>~%<values>~{~a~}~%</values>" 
                                             ;;(print "twelkjfsdlkfjsdlkfjsdlkfjsd")
                                             (xpath-mini::inner-xml-encode out-name-str) 
                                             (loop for cns in val-list
                                                   do (progn (princ cns out-val))
                                                   collect (format nil "<value>~a</value>" 
                                                                   (cond ((string-equal out-name-str "HAS-AFFORDANCES")
                                                                          (ocml::lower-instance-name-as-wsml-id cns))
                                                                         (t
                                                                          (xpath-mini::inner-xml-encode (get-output-stream-string out-val) )))
                                                                                       ))))
                                   ;;only one value
                                 ;;  (let ((out-name-str (progn (princ (first attr) out-name)(get-output-stream-string out-name)))
                                 ;;        (out-val-str (progn (princ (if (atom (second attr)) (second attr) (car (second attr))) out-val)(get-output-stream-string out-val))))
                                ;;     (format nil "<name>~a</name>~%<value>~a</value>" 
                                ;;             (xpath-mini::inner-xml-encode out-name-str) 
                                ;;             (xpath-mini::inner-xml-encode out-val-str)
;;))
)));;)
            (setf sres (concatenate 'string sres
                                    (format nil "<attribute>~a</attribute>~%" attr-string)))
                         
            ))
    (setf sres (format nil "<attributes>~%~a</attributes>" sres ))
    sres))

;;(concatenate 'string "toto" (format nil "toto~atata" "tutu"))

(defun lower-longitudinal-point (inst)
  ;;(princ inst)
  (let ((lat (car (xpm::o-xp "has-latitude" inst)))
        (long  (car (xpm::o-xp "has-longitude" inst))))
    ;;(princ lat)(princ long)
    (cond ((and lat long)
           (format nil "<georss:where xmlns:georss=\"http://www.georss.org/georss\" xmlns:gml=\"http://www.opengis.net/gml\"><gml:Point><gml:pos>~a ~a</gml:pos></gml:Point></georss:where>" (car lat) (car long)))
          (t nil))))

(defun lower-monotone-polygon (inst-list)
  (format nil "<georss:where xmlns:georss=\"http://www.georss.org/georss\" xmlns:gml=\"http://www.opengis.net/gml\">
      <gml:Polygon>
         <gml:exterior>
            <gml:LinearRing>
               <gml:posList>
            ~{~a ~}
               </gml:posList>
            </gml:LinearRing>
         </gml:exterior>
      </gml:Polygon>
   </georss:where>" (let ((res ()))
                      (print inst-list)
                      (loop for inst in (first inst-list)
                            ;;do (describe-instance inst)
                            do (setf res (append res (list (car (xpm::o-xp "has-latitude/text()" inst))
                                                (car (xpm::o-xp "has-longitude/text()" inst))))))
                      (print res)
                      res)))
                         



(defun sgis-lower-spatial-object-location (&optional inst)
  (declare (special xpm::CONTEXT))
  (when inst (setf xpm::CONTEXT inst))
  (let ((loc (caar (xpm::o-xp "has-location"))))
    (print "loc")(print loc)
    (if loc
        (let* ((the-loc-class (findany '?x `(= ?x (the-parent-new ,loc nil))))
              (loc-classes  (cons the-loc-class 
                                  (findany '?x `(and
                                                 (= ?y (the-parent-new ,loc nil))
                                                 (= ?x (union ?y (all-superclasses ?y)))))))) ;;(ocml::findall '?c `(= ?c (ocml::the-parent ,loc)))))
          (PRINT 'LOC-CLASSES-ARE-US) (print loc-classes)
          (cond ((member 'POINT-LOCATION loc-classes) 
                 (LET ((lgres (lower-longitudinal-point (xpm::o-xp "HAS-POINTS[1]" loc)))) (princ lgres)))
                ((member 'MONOTONE-POLYGON loc-classes) 
                 (lower-monotone-polygon (first (xpm::o-xp "HAS-POINTS" loc))))
                (t nil)))
          ;;no location
          nil)))

(defun sgis-lower-spatial-object (&optional inst)
  (declare (special xpm::CONTEXT))
  (when inst (setf xpm::CONTEXT (if (listp inst) inst (list inst))))
  (let ((sres "")
        (i (first xpm::CONTEXT)))
    (let ((sres ""))
      (setf sres (concatenate 'string sres (format nil "<id>~a</id>~%"  (lower-instance-name))))
      (setf sres (concatenate 'string sres (format nil "<type>~%~a~%</type>~%"  (lower-instance-class-name-as-wsml-id))))
      (setf sres (concatenate 'string sres (format nil "<archetypes>~%~a~%</archetypes>~%"  (lower-instance-archetype-list))))
      ;;(setf sres (concatenate 'string sres (format nil "<hci>~%~a~%</hci>~%"  (lower-hci))))
      (setf sres (concatenate 'string sres (sgis-lower-spatial-object-attributes)))
      (setf sres (concatenate 'string sres (sgis-lower-spatial-object-location)))
      (format nil "<object>~%~a~%</object>" sres))))

(deflower lower-spatial-object spatial-object
  (lambda (instance)
    (sgis-lower-spatial-object instance)))

(deflower lower-object-field object-field
  (lambda (instance)
    (sgis-lower-object-field instance)))



;;; changed to dynamically get the new (situation) affordances

(defun sgis-lower-object-field (&optional inst)
  (declare (special xpm::CONTEXT))
  (when inst (setf xpm::CONTEXT (if (listp inst) inst (list inst))))
  (let* ((sres "")
         (i (first xpm::CONTEXT))
         (objects (car (xpm::o-xp "has-objects" )))
         (next-sit (get-next-situation inst)))
    ;;(princ objects)
  (format nil "<sgis>~%<object-field>~%~a~%~a~%<objects>~%~a~%</objects>~%</object-field></sgis>"
          (format nil "<ofid>~a</ofid>~%"  (lower-instance-name))
          (if next-sit (format nil "<becomes>~%~a~%</becomes>" (sgis-lower-spatial-object next-sit)) "<becomes/>")
          (format nil "~{~a~}" 
                  (loop for obj in objects
                        collect (format nil "~%~a~%" (sgis-lower-spatial-object obj)))))))

      

(defun lower-instance-archetype-list ()
  "vlad 07/03/06 "  
  (declare (special xpm::CONTEXT))
  (let ((sres ""))
    (loop for arch in (eval `(ocml-eval (get-spatial-object-archetype-list ,(car xpm::CONTEXT)))) do
           (setf sres
                 (concatenate 'string sres 
                              "<archetype>"
                              (symbol-name arch)
                              "</archetype>")))
    sres))

(defun lower-hci (&optional inst)
  (declare (special xpm::CONTEXT))
  (when inst (setf xpm::CONTEXT inst))
  (let* ((i (if (listp xpm::CONTEXT) (car xpm::CONTEXT) xpm::CONTEXT))
         (hci-inst (eval `(ocml-eval (the-parent-new ,i 'hci)))))
    (unless (eql hci-inst ':NOTHING)
      (funcall (xpm::get-lower-function 'google-maps-hci) xpm::CONTEXT))))


#|
(eval `(ocml-eval (get-only-slots-value-of-parent 'v1 'hci)))
(eval `(ocml-eval (get-only-slots-value-of-parent 'v1 'google-maps-hci)))
(eval `(ocml-eval (get-only-slots-value-of-parent 'v1 (ocml-eval (get-direct-parent-of-class-for-instance 'v1 'hci)))))
|#                    


(deflower lower-google-maps-hci google-maps-hci
  (lambda (instance)
    (%lower-google-maps-hci instance)))

(defun %lower-google-maps-hci (&optional inst)
  (declare (special xpm::CONTEXT))
  (when inst (setf xpm::CONTEXT (if (listp inst) inst (list inst))))
  (let ((sres "")
        (i (first xpm::CONTEXT)))
    (setf sres
      (concatenate 'string
                   ;;"<visual>"
        (concatenate 'string 
                     "<hoverContent>"
                     ;;(xpm::o-xp "has-hover-content/text()" )
                     (format nil "~{~a~^ ~}" (setofall '?x `(has-hover-content ,i ?x)))
                     "</hoverContent>")
        (concatenate 'string 
                     "<hiddenAttributes>"
                     (format nil "~{~a~^ ~}" (loop for attr in (car (xpm::o-xp "has-hidden-attributes"))
                           collect (symbol-name attr)))
                     "</hiddenAttributes>")
                    ;;"</visual>"
                    ))))


#|


(def-instance v2 google-maps-hci
  ((has-hover-content "Hello hello!")
   (has-hidden-attributes (HAS-LATITUDE HAS-LONGITUDE))))
(describe-instance 'v2)
(lower-google-maps-hci 'v2)
|#




;;(lower-hospital 'INSTANCE1970)


        

#| 
(let ((slots (set-difference 
                      (findany '?x `(and (instance-of ,spatial-object ?class)
                                         (= ?x (all-class-slots ?class))))
                      (findany '?x '(= ?x (all-class-slots spatial-object))))))
          (mapcan #'(lambda (slot)
                      (let ((values
                             (findany '?x `(= ?x (all-slot-values ,spatial-object ,slot)))))
                        (when values
                          (list (cons slot values)))))
                  slots)))))
|#

;;((HAS-LATITUDE 0.0638580322265625) (HAS-LONGITUDE 52.01193653675363) (HAS-ADDRESS "Great Chishill") (HAS-BEDS 40) (HAS-NAME "Chelmsford & Essex Hospital") (HAS-POSTCODE "CM1 7LF") (HAS-TELEPHONE "01245 91149"))

#|
(def-class field (function)
  ())

(def-class object-field (Field)
  ((has-type :type ocml-class)))
|#
#|
(def-instance essex-space Space)
(def-instance mk-space Space)
(def-instance p1 Point
  ((has-space essex-space)))
(def-instance p2 Point
  ((has-space mk-space)))
(def-instance bad-loc Location
  ((has-space essex-space)
   (has-points p1 p2)))
(def-instance good-loc Location
  ((has-space essex-space)
   (has-points p1 )))
(describe-instance 'p1)
(describe-instance 'essex-space)
(ask (has-space p1 ?s))
(ask (element-of ?p essex-space))
(ask (has-space bad-loc ?x))
|#