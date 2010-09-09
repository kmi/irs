(in-package web-onto)

(defvar *root-published-ontolingua-ontology-directory*
  "ocml:ontolingua-web;")

(defvar *published-ontolingua-ontology-root-url*
  (concatenate 'string 
               web-onto::*default-host*
               "/ontolingua/"))

(defvar *plain-default-ontolingua-sub-directory*
  "ontolingua")


(defun publish-ontolingua-ontology (ontology 
                                    ontology-type
                                    documentation
                                    ocml-source-directory
                                    load-file
                                    files
                                    &optional 
                                    (root-directory
                                     *root-published-ontolingua-ontology-directory*)
                                    (root-url 
                                     *published-ontolingua-ontology-root-url*))
  (ocml::translate-ocml-ontology-to-ontolingua ontology)
  (let* ((web-directory 
          (translate-logical-pathname
           (ontology-directory-name-from-type 
            ontology-type ontology
            root-directory)))
         (source-directory 
          (make-pathname :directory
                         (append 
                          (pathname-directory ocml-source-directory)
                          (list 
                           *plain-default-ontolingua-sub-directory*))
                         :host (pathname-host ocml-source-directory))))
    (ensure-directories-exist web-directory)
    (generate-published-ontology-index-page 
     :ontolingua
     web-directory
     ontology 
     ontology-type 
     documentation (cons load-file files)
     root-url)
    (mapc 
     #'(lambda (file)
         (generate-published-ontology-page
          ontology :ontolingua
          documentation web-directory
          source-directory file
          ocml::*ontolingua-suffix*
          root-url))
     (cons load-file files))))