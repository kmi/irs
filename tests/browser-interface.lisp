;;; Dave Lambert, 2007

;; The IRS browser makes several HTTP requests to the IRS server.

;; Within the web-onto HTTP "method" there are several actions.
;; Check webonto::internal-handle-web-onto for details.

(in-package :irs.tests)

(def-suite browser-interface-suite
  :description "Tests for the IRS browser interface.")

(in-suite browser-interface-suite)

(defun query (method uri)
  "Because the IRS web server is crippled, various HTTP requests get
  shovelled through ad-hoc HTTP methods because the dispatch mechanism
  there is simpler.  Since this breaks any self-respecting HTTP client
  library, we have to write our own client for the purpose of this
  test."
  (let ((host "localhost")
        (port 3000))
    ;; Open socket
    (with-open-stream (connection (comm:open-tcp-stream host port :errorp t))
      ;; Ram HTTP request down socket
      (format connection "~A ~A HTTP/1.0~A~A~A~A" (symbol-name method) uri
              #\return #\linefeed #\return #\linefeed)
      (finish-output connection)
      ;; Of course, the *entire* output happens to be in the first line.
      ;; The HTTP header is non-existant.
      (let ((header (read-line connection nil nil)))
        (when (null header)
          (error "no response from server"))
        header))))

(defun extract-ontologies (string)
  (http::read-into-list (substitute #\space #\[ string)))

;; These tests are less than ideal, but they have to be this way
;; because sometimes the underlying method "succeeds" but returns a
;; string that says something like "we failed".

(test mnm-method-test
  (is-true (string< "<html><body><h2>"
		    (query :mnm "html_info common-concepts common-concepts :ontology")))
  (is-true (string< "<html><body>Sorry"
		    (query :mnm "html_info wsmo not-an-ontology :ontology")))
  (is-true (search "Superclass" (query :mnm "html_info wsmo assumption :class")))
  (is-true (search "Ontology" (query :mnm "html_info wsmo wsmo :ontology")))
  (is-true (search "Binary-Relation"(query :mnm "html_info wsmo has-arity :relation"))))

(test webonto-method-test
  (is-true
   (string= "OK" (query :web-onto "login \"dave\" \"dave\"")))
  (is-true
   (member 'ocml::base-ontology 
	   (extract-ontologies (query :web-onto "get_all_ontologies john"))))
  (is-true 
   (string= "nil" (query :web-onto "get_all_ontologies_of_type () john"))))

(test irs-method-test
  (is-true (search "has-effect"
		   (query :irs "goal-details wsmo find-web-services-for-goal")))
  (is-true (search "has-target-component"
		   (query :irs "mediator-details wsmo bridge"))))
