;;; Copyright © 2008 The Open University

;;; ATTENTION!  DO NOT REFORMAT THE STRINGS IN HERE!
;;;
;;; They are sensitive to white space changes which are a pain to
;;; debug.  You break it, you fix it.

(in-package #:irs.tests)

(def-suite grounding-suite
    :description "Tests for the groundings.")

(in-suite grounding-suite)

(test http-goal-invocation
  (is (string=
       (with-output-to-string (str)
         (ip::raw-irs-achieve-goal 'ocml::irs-tests
                                   'ocml::ocml-query-goal
                                   `((ocml::has-ontology "IRS-TESTS")
                                     (ocml::has-query ,(hunchentoot:url-encode "(setofall ?x (member ?x (1 2 3)))"))
                                     (ocml::has-format "xml"))
                                   str nil t))
       "<?xml version='1.0' encoding='UTF-8'?>
<tns:ResultSet xmlns:tns='http://kmi.open.ac.uk/irs/query/20080215/result'
    xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'
    xsi:schemaLocation='http://kmi.open.ac.uk/irs/query/20080215/result QueryResultSet.xsd'>
    <tns:Result>
        <tns:Binding Variable='?X' Value='1'/>
    </tns:Result>
    <tns:Result>
        <tns:Binding Variable='?X' Value='2'/>
    </tns:Result>
    <tns:Result>
        <tns:Binding Variable='?X' Value='3'/>
    </tns:Result>
</tns:ResultSet>")))
