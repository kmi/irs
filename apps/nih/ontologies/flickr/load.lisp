;;; Copyright © 2009 The Open University

;;; File created in GNU Emacs

(in-package #:ocml)

(def-ontology flickr
    "Goals and services for accessing Flickr."
    :includes (cryptography http-grounding mime rfc2616 xml xml-rpc wsmo)
    :type :goal
    :namespace-uri "http://www.kmi.open.ac.uk/projects/irs/flickr#"
    :namespaces (("flickr" flickr)
		 ("crypt" cryptography)
                 ("grnd" http-grounding)
                 ("mime" mime)
                 ("rfc2616" rfc2616)
		 ("xml" xml)
                 ("xmlrpc" xml-rpc))
    :author "dave" :allowed-editors ("dave" "john")
    :files ("elevation" "flickr" "rest" "sundry" "xmlrpc"))
