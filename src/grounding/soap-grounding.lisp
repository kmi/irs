;;; Copyright (C) Open University 2007, 2008.

;;; This is a grounding from IRS to SOAP 1.1.

;;; It is currently a (huge) kludge for two reasons:
;;;
;;; 1. Shortcomings in IRS's  generic grounding support.
;;; 2. It was written quickly and only to support LHDL services.
;;;
;;; Neither is acceptable long-term.
;;;
;;; We use a new grounding signature.  The choreography's
;;; has-grounding slot takes the sexp '((grounded-to-soap1.1)), but
;;; everything goes through the new slot has-earthing.  That takes an
;;; instance of soap-grounding.  See the WSMO ontology for details.

(in-package :irs.grounding.soap)

(defun soap-fault? (xml)
  (xpm::with-xml-ctx (:sequence (xpm::document-parser xml))
    (xpm::x-xp "Envelope/Body/Fault")))

(defmethod wsmo-protocol:invoke-web-service-operation
    ((grounding-type (eql 'ocml::grounded-to-soap1.1)) grounding
     operation proc output-type input-roles-and-soap-bindings values
     host port location-ignored service-instance)
  (declare (ignore location-ignored))
  (let* ((service (ocml::name (ocml::parent-class service-instance)))
         (earthing (web-onto::findany
                    'ocml::?m `(ocml::associated-earthing ,service ocml::?m)))
         (xml (make-xml-message values))
         (url (ocml::get-role-value earthing 'ocml::has-url))
         (soapaction (ocml::get-role-value earthing 'ocml::has-soap-action))
         (method (ocml::get-role-value earthing 'ocml::has-soap-method))
         (namespace (ocml::get-role-value earthing 'ocml::has-target-namespace))
         (connection-read-timeout
          (let ((val (ocml::get-role-value earthing 'ocml::has-connection-read-timeout)))
            (if (ocml:nothing? val)
                irs:*connection-read-timeout*
                val)))
         (content (format nil "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">
  <soap:Body>
    <~A xmlns=\"~A\">
~A
    </~A>
  </soap:Body>
</soap:Envelope>
" method namespace xml method)))
    #-:lispworks (declare (ignore connection-read-timeout))
    (when irs.grounding:*debug-stream*
      (format irs.grounding:*debug-stream* "sending:~%~A~%" content))
     ;;;;;;
    (irs.api.javascript:event :rpc-call service content)
    (let ((response (funcall #'utilities:http-request url
			     :method :post
			     :additional-headers `(("SOAPAction" . ,soapaction))
			     :content-type "text/xml; charset=utf-8"
			     #+:lispworks :read-timeout #+:lispworks connection-read-timeout
			     :content content)))
      (if (soap-fault? response)
          (signal 'irs.grounding::<grounding-fault>
                  :service service :service-response response)
          response))))



(defun make-xml-message ( values)
  "Wrap up the VALUES, which should be in the form <string>blah</string> or <i4>42</i4> or whatever, with <params> and <param><value>."
  (format nil "~{~A~}" values))

(defmethod wsmo-protocol::make-invocation-name-for-web-service-operation2
    ((type (eql 'ocml::grounded-to-soap1.1)) implementation-info)
  implementation-info)
