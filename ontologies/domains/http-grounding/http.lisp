;;; Copyright © 2008 The Open University

(in-package #:ocml)

(in-ontology http-grounding)

(def-relation downer-defined (?service-class ?relation))

(def-rule #_grnd:ocmlify-http-response
    ((#_ocmlify-http-response ?response-code ?response-body ?response) if
     (= ?response (#_rfc2616:new-instance #_rfc2616:http-response))
     (#_rfc2616:set-content ?response ?response-body)
     (#_rfc2616:set-content ?response ?response-body)
     (exec (set-slot-value ?response #_rfc2616:has-response-code ?response-code))))

;;; {{{ Time and date

(def-function #_current-time ()
  "The current time in the Unix epoch.  Returns a #_time instance."
  :lisp-fun (lambda ()
              (let* ((time (get-universal-time))
                     (instance (new-instance '#_rfc2616:time
                                             `((#_rfc2616:has-value ,time)))))
                (name instance))))

;;; }}}

(def-rule #_listAsURLQuery
    "Turn a list of (parameter value) pairs into a string that can be
used as part of the URL."
  ((#_listAsURLQuery () ""))
  ((#_listAsURLQuery ((?name ?value)) ?str) if
   (#_rfc2616:url-encoding ?name ?encoded-name)
   (#_rfc2616:url-encoding ?value ?encoded-value)
   (= ?str (make-string "~A=~A" ?encoded-name ?encoded-value)))
  ((#_listAsURLQuery ((?name ?value) . ?rest) ?str) if
   (not (null ?rest))
   (#_rfc2616:url-encoding ?name ?encoded-name)
   (#_rfc2616:url-encoding ?value ?encoded-value)
   (#_listAsURLQuery ?rest ?reststr)
   (= ?str (make-string "~A=~A&~A" ?encoded-name ?encoded-value ?reststr))))
