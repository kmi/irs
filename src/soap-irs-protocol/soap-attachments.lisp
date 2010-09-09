(in-package irs-protocol)


(defvar *soap-attachment-type*
  "xml")

(defun soap-attachment-type-p (x)
  (string= x *soap-attachment-type*))

;; Added by Carlos to ensure CRLF and not just LF are sent
(defvar *crlf* 
  (format nil "~C~C" #.(code-char 13) #.(code-char 10)))

;; Modified by Carlos 18-1-2007
;; Include Content-ID to avoid incompatibilities with Axis
;; New lines are also replaces by CRLF as dictated by SOAP specification
(defun send-soap-attachment (task-name-string x output-type 
                                               &optional (stream *standard-output*))
  ;;(setf l11 (list task-name-string x output-type))
  (let* ((stm stream)
	 (response (iu::gen-soap-response task-name-string (list "")
                                         output-type nil))

         (separator (format nil "------=_Part_0_1124227.1061915908980"))
         (content-type-header (format nil "Content-Type: text/xml; charset=UTF-8"))
         (xml-header (format nil "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"))
         (content-type-header2 (format nil "Content-Type: text/plain"))
         (content-id (format nil "achievedGoal~d@irs.kmi.open.ac.uk" (get-universal-time)))
         (content-id-soap (format nil "Content-ID: soapPart"))
         (content-id-result (format nil "Content-ID: <~a>" content-id))

         ;; This is how the response should be. To be done later on (requires adapting Java API)
         ;;(response (format nil "<RESULT type=\"xml\">cid:result=~a</RESULT>" content-id))

	 (reslen (+ (length iu:*soap-header2*) 
		    (length response) 
		    (length iu:*soap-end2*)
                    (* 3 (length separator))
                    (length content-type-header)
                    (length xml-header)
                    (length content-type-header2)
                    (length content-id-result)
                    (length content-id-soap)
                    (length x)
                    ;; We also need to take into account the CRLFs
                    26 ;; For the CRLF's
                    2  ;; For the latest separator
                    )))
    (format stm "HTTP/1.1 200 OK")
    (format stm *crlf*)
    (format stm "Content-Type: multipart/related; type=\"text/xml\"; boundary=\"----=_Part_0_1124227.1061915908980\";start=soapPart")
    (format stm *crlf*)
    (format stm "Content-Length: ~d" reslen)
    (format stm *crlf*)
    (format stm "SOAPAction: \"\"")
    (format stm *crlf*)(format stm *crlf*)
    
    ;; Contents starts here - take into the CRLF for the content length
    (format stm "~a" separator)
    (format stm *crlf*)
    (format stm "~a" content-type-header)
    (format stm *crlf*)
    (format stm "~a" content-id-soap)
    (format stm *crlf*)(format stm *crlf*)
    (format stm xml-header)
    (format stm *crlf*)
    (format stm "~a" iu:*soap-header2*)
    (format stm "~a" response)
    (format stm "~a" *soap-end2*)
    (format stm *crlf*) (format stm *crlf*)
    (format stm "~a" separator)
    (format stm *crlf*)
    (format stm "~a" content-type-header2)
    (format stm *crlf*)
    (format stm "~a" content-id-result)
    (format stm *crlf*)(format stm *crlf*)
    (format stm "~a" x)
    (format stm *crlf*)(format stm *crlf*)
    ;; The last separator requires a trailing '--'
    (format stm "~a--" separator)
    (force-output stm)
    ))