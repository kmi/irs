;;; Mode: Lisp; Package: web-onto

;;; Author: John Domingue

;;; The Open University

(in-package "WEB-ONTO")

;(defvar *ontologies* '(ocml::truck-cabin ocml::phys-mech
;		      ocml::cost-ontology ocml::health-rel-payoff
;                      ocml::medical-ontology 
;		      ocml::sis1-as-gen-design ocml::sis1-as-a-star-design
;		      ocml::sis1-as-hc-design ocml::kmi-as-pardes
;                      ocml::kmi-as-p-r-i
;		      ocml::kmi-as-p-and-i
;                      ocml::kmi-as-p-and-r
;		      ocml::vt-as-p-and-r
;                      ;;ocml::bae-workbook
;                      ocml::kmi-planet-ontology
;                      ocml::johns-work))


(defvar *planet-ontologies* '(ocml::event-ontology ocml::kmi-new ocml::kmi-planet-kb
                                                   ocml::kmi-planet-ontology
                                                   ocml::news-ontology
                                                   ocml::persons-and-organizations)) 

(defvar *page-colour*
  #+:irs-use-lispweb
  (http::color-string :red 255 :blue 255 :green 255))

(http::define-page ("Web Onto Browser" :func-name web-onto-page
			         :class :user :bgcolor *page-colour*
                              ) 
    (&rest ignore)
  (declare (ignore ignore))
   (http::html-out (http::center (header 1 "The Web Ontology Browser")))
   (http::html-out (http::italic "<p>Welcome to the Web Ontology Browser. This system was implemented by ~a.</p>")
		   (http::anchor "http://kmi.open.ac.uk/~john/john.html"
		                 "John Domingue"))
   (http::html-out (make-empty-onto-page)))

;;;height=600 -> 450 for wwdl paper width=680 -> 780 for wwdl paper
(defun make-empty-onto-page (&optional (java-file "johnd.webonto.web_onto.class"))
  (html::applet java-file
	        :width 680
		:height 600
		:param-list
                (list (list "ontologies"
                            (string-downcase (format nil "~{~a ~}" *ontologies*)))
                      (list "ocml_types"
                            (string-downcase (format nil "~{~a ~}"
						     '(classes relations functions))))
                      (list "query_string"
                            (string-downcase
                             (format nil "~{~a ~}"
				     '(classes relations functions
                                               rules instances)))))))

  ;(format nil
;	  "<title>Graph Layout</title>
;<!-- Changed by: John Domingue,  3-Jul-1996 -->
;<hr>
;<applet code=\"~a\" width=680 height=600>
;<param name=ontologies value=\"~a\">
;<param name=ocml_types value=\"~a\">
;<param name=query_string value=\"\">
;</applet>
;<hr>"
;	  java-file
;          
;          (string-downcase (format nil "~{~a ~}"
;                                   '(classes relations functions rules instances))))) ;;;tasks relations psms)))))
