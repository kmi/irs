(in-package #:lhdl)

(defun start-web-interface ()
  (web:register-plugin
   :lhdl :application
   "Living Human Digital Library"
   (nconc
    (mapcar (lambda (args)
	      (apply #'hunchentoot:create-regex-dispatcher args))
	    `(("/lhdl$" draw-top-page)
              ("/lhdl/gui$" draw-gui-page)
              ("/lhdl/javascript"
               ,(lambda () (web:send-url/file "/lhdl/javascript" "irs:apps;lhdl;javascript")))
              ("/lhdl/www"
               ,(lambda () (web:send-url/file "/lhdl/www" "irs:apps;lhdl;www")))
              ("/lhdl/debase64" debase64))))))

;;; {{{ Service info
(defstruct (service (:type list))
  (name)
  (operation))

(defun service-webpage (service)
  (let ((name (service-name service)))
    (format nil "http://lhdl.cis.beds.ac.uk/execws/~A/~AWS.asmx"
            name name)))

(defun service-help-human (service)
  (format nil "http://lhdl.cis.beds.ac.uk/execws/help/~AWS.htm"
          (service-name service)))

(defun service-help-autogen (service)
  (format nil "http://lhdl.cis.beds.ac.uk/~AService/~AService.asmx?op=~A"
          (service-operation service)
          (service-operation service)
          (service-operation service)))

(defparameter *services*
  (mapcar #'(lambda (service)
              (make-service :name (first service) :operation (second service)))
          '(("mmoSTLImporter" "importSTL")
            ("mmoVTKImporter" "importVTK"))))
;;; }}}

(defun draw-top-page ()
  (web:standard-page "Living Human Digital Library"
    (:p "The Living Human Digital Library")
    (:ul (:li ((:a :href "http://www.livinghuman.org/")
	       "Living Human homepage"))
	 (:li ((:a :href "http://www.kmi.open.ac.uk/projects/lhdl/")
	       "Living Human Digital Library page at the Open University"))
         (:li ((:a :href "lhdl/gui") "Semantic Services Ajax demonstrator")))))
