;;; Copyright © 2008 The Open University

;;; Composite services for the LHDL project review 2008.

;;; XXX The mix of lift/lower might hurt us here.

;;; get-input-role and orch-get-input-role are from Alessio (cleaned
;;; up by me).

(in-package #:ocml)

(in-ontology lhdl-goals)

(defun get-input-role (goal-input)
  (let ((ws wp::*current-orchestration-web-service*))
    (ocml::findany '?x                  ; XXX This might be in webonto now...
                   `(= ?x (ocml::wsmo-role-value
                           ,ws
                           ,goal-input)))))

(def-function orch-get-input-role (?goal-input)
  :lisp-fun #'get-input-role)

(def-function orch-get-goal-value2 (?goal-name)
  :lisp-fun
  #'(lambda (goal-name)
      (let* ((instance-name (get-last-achieve-goal-value goal-name))
             (instance (first (find-all-current-instances-named-x instance-name))))
        instance)))

(def-function orch-get-goal-value3 (?goal-name)
  :lisp-fun
  #'(lambda (goal-name)
      (let* ((instance-name (get-last-achieve-goal-value goal-name))
             (instance (first (find-all-current-instances-named-x instance-name)))
             (values (get-slot-values instance 'ocml::has-content)))
        (first values))))

;;; XXX These are hacks.  They should be done either by aligning the
;;; types in the definitions, or by mediation.  Once again, I don't
;;; have time to do it properly.

(def-function binary-counterpart (uri-result)
  :lisp-fun
  #'(lambda (uri-result)
      (let ((uri (first (get-slot-values uri-result 'ocml::has-uri))))
        (ocml::name
         (ocml::new-instance 'lhdl-url `((has-value ,uri)))))))

;;; Compute the XML file name
;;; for the file identified by BINARY-PART.
(def-function xml-counterpart (binary-part)
  :lisp-fun
  #'(lambda (uri-result)
      (let ((uri (first (get-slot-values uri-result 'ocml::has-uri))))
        (ocml::name
         (ocml::new-instance 'lhdl-xml
                             `((has-value ,(format nil "~A.xml" uri))))))))

;;; {{{ move-file-s3-to-bt-goal
(def-class move-file-s3-to-bt-goal (lhdl-goal) ?goal
    ((has-input-role :value has-amazon-account
                     :value has-amazon-bucket
                     :value has-amazon-key
                     :value #_hasBiomedTownAccount
                     :value has-filename)
     (has-input-soap-binding :value (has-amazon-account "string")
                             :value (has-amazon-bucket "string")
                             :value (has-amazon-key "string")
                             :value (#_hasBiomedTownAccount "string")
                             :value (has-filename "string"))
     (has-amazon-account :type #_s3:amazon-account)
     (has-amazon-bucket :type #_s3:amazon-bucket)
     (has-amazon-key :type #_s3:amazon-object-key)
     (#_hasBiomedTownAccount :type #_domain:BiomedTownAccount)
     (has-filename :type lhdl-filename)
     (has-non-functional-properties :value move-file-s3-to-bt-goal-non-functional-properties)))

(def-class move-file-s3-to-bt-goal-non-functional-properties (non-functional-properties)
    ((has-description "Move a file from Amazon S3 to BiomedTown storage.")))
;;; }}}
;;; {{{ move-file-s3-to-bt-web-service
(def-class move-file-s3-to-bt-mediator-non-functional-properties (non-functional-properties)
    nil)

(def-class move-file-s3-to-bt-mediator (wg-mediator) ?mediator
    ((has-source-component :value move-file-s3-to-bt-goal)
     (has-non-functional-properties
      :value move-file-s3-to-bt-mediator-non-functional-properties)))

(def-class move-file-s3-to-bt-web-service (web-service) ?web-service
    ((has-capability :value move-file-s3-to-bt-web-service-capability)
     (has-interface :value move-file-s3-to-bt-web-service-interface)
     (has-non-functional-properties
      :value move-file-s3-to-bt-web-service-non-functional-properties)))

(def-class move-file-s3-to-bt-web-service-non-functional-properties
    (non-functional-properties)
    nil)

(def-class move-file-s3-to-bt-web-service-capability-non-functional-properties
           (non-functional-properties)
           nil)

(def-class move-file-s3-to-bt-web-service-capability
           (capability)
           ?capability
           ((used-mediator :value move-file-s3-to-bt-mediator)
            (has-non-functional-properties
             :value
             move-file-s3-to-bt-web-service-capability-non-functional-properties)))

(def-class move-file-s3-to-bt-web-service-interface-non-functional-properties
           (non-functional-properties)
           nil)

(def-class move-file-s3-to-bt-web-service-interface-choreography
           (choreography)
           ((has-grounding :value ((normal dummy-function)))))

(def-class move-file-s3-to-bt-web-service-interface-orchestration-problem-solving-pattern
    (problem-solving-pattern)
    ((has-body :value ((orch-seq
                        (achieve-goal 'ocml::get-object-goal
                                      (orch-get-input-role 'ocml::has-amazon-account)
                                      (orch-get-input-role 'ocml::has-amazon-bucket)
                                      (orch-get-input-role 'ocml::has-amazon-key)))
                       (orch-seq
                        (achieve-goal 'ocml::upload-file-goal
                                      (orch-get-input-role 'ocml::has-filename)
                                      ""
                                      ""
                                      (orch-get-goal-value 'ocml::get-object-goal)
                                      (orch-get-input-role '#_hasBiomedTownAccount)))))))

(def-class move-file-s3-to-bt-web-service-interface-orchestration
           (orchestration)
           ((has-problem-solving-pattern
             :value
             move-file-s3-to-bt-web-service-interface-orchestration-problem-solving-pattern)))

(def-class move-file-s3-to-bt-web-service-interface (interface) ?interface
    ((has-choreography :value move-file-s3-to-bt-web-service-interface-choreography)
     (has-orchestration :value move-file-s3-to-bt-web-service-interface-orchestration)
     (has-non-functional-properties
      :value
      move-file-s3-to-bt-web-service-interface-non-functional-properties)))
;;; }}}

;;; {{{ move-file-bt-to-s3-goal
(def-class move-file-bt-to-s3-goal (lhdl-goal) ?goal
    ((has-input-role :value has-amazon-account
                     :value #_hasBiomedTownAccount
                     :value has-amazon-bucket
                     :value has-amazon-key
                     :value has-filename)
     (has-input-soap-binding :value (has-amazon-account "string")
                             :value (#_hasBiomedTownAccount "string")
                             :value (has-amazon-bucket "string")
                             :value (has-amazon-key "string")
                             :value (has-filename "string"))
     (has-amazon-account :type #_s3:amazon-account)
     (#_hasBiomedTownAccount :type #_domain:BiomedTownAccount)
     (has-amazon-bucket :type #_s3:amazon-bucket)
     (has-amazon-key :type #_s3:amazon-object-key)
     (has-filename :type lhdl-filename)
     (has-non-functional-properties :value move-file-bt-to-s3-goal-non-functional-properties)))

(def-class move-file-bt-to-s3-goal-non-functional-properties (non-functional-properties)
    ((has-description "Move a file from Amazon S3 to BiomedTown storage.")))
;;; }}}
;;; {{{ move-file-bt-to-s3-web-service
(def-class move-file-bt-to-s3-mediator-non-functional-properties (non-functional-properties)
    nil)

(def-class move-file-bt-to-s3-mediator (wg-mediator) ?mediator
    ((has-source-component :value move-file-bt-to-s3-goal)
     (has-non-functional-properties
      :value move-file-bt-to-s3-mediator-non-functional-properties)))

(def-class move-file-bt-to-s3-web-service (web-service) ?web-service
    ((has-capability :value move-file-bt-to-s3-web-service-capability)
     (has-interface :value move-file-bt-to-s3-web-service-interface)
     (has-non-functional-properties
      :value move-file-bt-to-s3-web-service-non-functional-properties)))

(def-class move-file-bt-to-s3-web-service-non-functional-properties
    (non-functional-properties)
    nil)

(def-class move-file-bt-to-s3-web-service-capability-non-functional-properties
           (non-functional-properties)
           nil)

(def-class move-file-bt-to-s3-web-service-capability
           (capability)
           ?capability
           ((used-mediator :value move-file-bt-to-s3-mediator)
            (has-non-functional-properties
             :value
             move-file-bt-to-s3-web-service-capability-non-functional-properties)))

(def-class move-file-bt-to-s3-web-service-interface-non-functional-properties
           (non-functional-properties)
           nil)

(def-class move-file-bt-to-s3-web-service-interface-choreography
           (choreography)
           ((has-grounding :value ((normal dummy-function)))))

(def-class move-file-bt-to-s3-web-service-interface-orchestration-problem-solving-pattern
    (problem-solving-pattern)
    ((has-body :value ((orch-seq
                        (achieve-goal 'ocml::download-file-goal
                                      (orch-get-input-role 'ocml::has-filename)
                                      (orch-get-input-role '#_hasBiomedTownAccount)))
                       (orch-seq
                        (achieve-goal 'ocml::put-object-goal
                                      (orch-get-input-role 'ocml::has-amazon-account)
                                      (orch-get-input-role 'ocml::has-amazon-bucket)
                                      (orch-get-input-role 'ocml::has-amazon-key)
                                      (orch-get-goal-value3 'ocml::download-file-goal)))))))

(def-class move-file-bt-to-s3-web-service-interface-orchestration
           (orchestration)
           ((has-problem-solving-pattern
             :value
             move-file-bt-to-s3-web-service-interface-orchestration-problem-solving-pattern)))

(def-class move-file-bt-to-s3-web-service-interface (interface) ?interface
    ((has-choreography :value move-file-bt-to-s3-web-service-interface-choreography)
     (has-orchestration :value move-file-bt-to-s3-web-service-interface-orchestration)
     (has-non-functional-properties
      :value
      move-file-bt-to-s3-web-service-interface-non-functional-properties)))
;;; }}}

;;; {{{ import-decimate-render-goal
(def-class import-decimate-render-goal (lhdl-goal) ?goal
    ((has-input-role :value has-filename
                     :value has-preserve-topology
                     :value has-target-reduction
                     :value has-background-colour
                     :value has-zoom-factor
                     :value has-camera-azimuth
                     :value has-camera-roll
                     :value has-image-width
                     :value has-image-height
                     :value #_hasAccount)
     (has-input-soap-binding :value (has-filename "string")
                             :value (has-preserve-topology "string")
                             :value (has-target-reduction "string")
                             :value (has-background-colour "string")
                             :value (has-zoom-factor "string")
                             :value (has-camera-azimuth "string")
                             :value (has-camera-roll "string")
                             :value (has-image-width "string")
                             :value (has-image-height "string")
                             :value (#_hasAccount "string"))
     (has-output-role :value has-base64-image)
     (has-output-soap-binding :value (has-base64-image "string"))
     (has-filename :type lhdl-filename)
     (has-preserve-topology :type preserve-topology)
     (has-target-reduction :type target-reduction)
     (has-background-colour :type background-colour)
     (has-zoom-factor :type zoom-factor)
     (has-camera-azimuth :type camera-azimuth)
     (has-camera-roll :type camera-roll)
     (has-image-width :type image-width)
     (has-image-height :type image-height)
     (has-base64-image :type base64-image)
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (has-non-functional-properties :value import-decimate-render-goal-non-functional-properties)))

(def-class import-decimate-render-goal-non-functional-properties (non-functional-properties)
    ((has-description "Import an STL file, decimate and render it.")))
;;; }}}
;;; {{{ import-decimate-render-web-service
(def-class import-decimate-render-mediator-non-functional-properties (non-functional-properties)
    nil)

(def-class import-decimate-render-mediator (wg-mediator) ?mediator
    ((has-source-component :value import-decimate-render-goal)
     (has-non-functional-properties
      :value import-decimate-render-mediator-non-functional-properties)))

(def-class import-decimate-render-web-service (web-service) ?web-service
    ((has-capability :value import-decimate-render-web-service-capability)
     (has-interface :value import-decimate-render-web-service-interface)
     (has-non-functional-properties
      :value import-decimate-render-web-service-non-functional-properties)))

(def-class import-decimate-render-web-service-non-functional-properties
    (non-functional-properties)
    nil)

(def-class import-decimate-render-web-service-capability-non-functional-properties
           (non-functional-properties)
           nil)

(def-class import-decimate-render-web-service-capability
           (capability)
           ?capability
           ((used-mediator :value import-decimate-render-mediator)
            (has-non-functional-properties
             :value
             import-decimate-render-web-service-capability-non-functional-properties)))

(def-class import-decimate-render-web-service-interface-non-functional-properties
           (non-functional-properties)
           nil)

(def-class import-decimate-render-web-service-interface-choreography
           (choreography)
           ((has-grounding :value ((normal dummy-function)))))

(def-class import-decimate-render-web-service-interface-orchestration-problem-solving-pattern
    (problem-solving-pattern)
    ((has-body
      :value
      ((orch-seq
        (achieve-goal 'ocml::import-stl-goal
                      (orch-get-input-role 'ocml::has-filename)
                      (orch-get-input-role '#_hasAccount)))
       (orch-seq
        (achieve-goal 'ocml::decimate-surface-goal
                      (binary-counterpart (orch-get-goal-value2 'ocml::import-stl-goal))
                      (xml-counterpart (orch-get-goal-value2 'ocml::import-stl-goal))
                      (orch-get-input-role 'ocml::has-preserve-topology)
                      (orch-get-input-role 'ocml::has-target-reduction)
                      (orch-get-input-role '#_hasAccount)))
       (orch-seq
        (achieve-goal 'ocml::render-surface-goal
                      (binary-counterpart (orch-get-goal-value2 'ocml::decimate-surface-goal))
                      (xml-counterpart (orch-get-goal-value2 'ocml::decimate-surface-goal))
                      (orch-get-input-role 'ocml::has-background-colour)
                      (orch-get-input-role 'ocml::has-zoom-factor)
                      (orch-get-input-role 'ocml::has-camera-azimuth)
                      (orch-get-input-role 'ocml::has-camera-roll)
                      (orch-get-input-role 'ocml::has-image-width)
                      (orch-get-input-role 'ocml::has-image-height)
                      (orch-get-input-role '#_hasAccount)))))))

(def-class import-decimate-render-web-service-interface-orchestration
           (orchestration)
           ((has-problem-solving-pattern
             :value
             import-decimate-render-web-service-interface-orchestration-problem-solving-pattern)))

(def-class import-decimate-render-web-service-interface (interface) ?interface
    ((has-choreography :value import-decimate-render-web-service-interface-choreography)
     (has-orchestration :value import-decimate-render-web-service-interface-orchestration)
     (has-non-functional-properties
      :value
      import-decimate-render-web-service-interface-non-functional-properties)))
;;; }}}
