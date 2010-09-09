;;; Copyright © 2008 The Open University

;;; File created in GNU Emacs.

(in-package #:ocml)

(def-ontology xml-rpc
    "XML-RPC message formats."
    :includes (rfc2616 xml)
    :type :domain
    :author "dave" :allowed-editors ("dave")
    :namespaces (("http" rfc2616)
                 ("xml" xml)
                 ("xmlrpc" xml-rpc))
    :namespace-uri "http://www.kmi.open.ac.uk/projects/irs/ns/xml-rpc#"
    :files ("xmlrpc" "serialisation"))
