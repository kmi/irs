(in-package :irs.tests)


;;;; Testing from a web browser:

;;;; Unit tests

(in-package :irs.tests)

(def-suite luisa-suite
  :description "Tests for the LUISA project.")

(in-suite luisa-suite)

(test luisa-stefan-test
  (finishes
    (with-output-to-string (str)
      (ip::raw-irs-achieve-goal 'ocml::luisa-first-prototype-services
                                'ocml::luisa-get-lo-goal
                                '((ocml::has-method "getURL")
                                  (ocml::has-comp "evolution")
                                  (ocml::has-field "computerScience")
                                  (ocml::has-lang "english")
                                  (ocml::has-interact ""))
                                str nil t)))
  ;; XXX Hmm. This one almost works, but ends with a complaint that 
  ;; map-lpmo-provider-object_is_not_a_defined_lisp_function
  (finishes
    (with-output-to-string (str)
      (ip::raw-irs-achieve-goal 'ocml::luisa-first-prototype-services
                                'ocml::MAP-LPMO-ID-GOAL
                                '((ocml::has-lpmo-id "instance3URL"))
                                str nil t)))

  (finishes
    (with-output-to-string (str)
      (ip::raw-irs-achieve-goal 'ocml::luisa-first-prototype-services
                                'ocml::LUISA-get-lo-GOAL
                                '((ocml::has-method "getURL")
                                  (ocml::has-comp "ethicalIssues")
                                  (ocml::has-field "rightsAndLaws")
                                  (ocml::has-lang "")
                                  (ocml::has-interact "mixed"))
                                str nil t)))
)

#|
  ;; XXX below this line, the tests fail...
  '(finishes
    (with-output-to-string (str)
      (ip::raw-irs-achieve-goal 'ocml::luisa-first-prototype-services
                                'ocml::luisa-core-get-profile-goal
                                '((ocml::has-user "liana"))
                                str nil t)))

  (finishes
    (with-output-to-string (str)
      (ip::raw-irs-achieve-goal 'ocml::luisa-first-prototype-services
                                'ocml::get-lpmo-object-id-goal
                                '((ocml::has-method "getURL")
                                  (has-scorm-objective "English4Runaways"))
                                str nil t)))


  (finishes
    (with-output-to-string (str)
      (ip::raw-irs-achieve-goal 'ocml::luisa-first-prototype-services
                                'ocml::Dynamic-ACHIEVE-IMSLD-OBJECTIVE-Meta-GOAL
                                '((ocml::has-method "getURL")
                                  (ocml::has-imsld-objective "LearningEnglish")
                                  (ocml::has-language "German"))
                                str nil t)))



  (finishes
    (with-output-to-string (str)
      (ip::raw-irs-achieve-goal 'ocml::luisa-first-prototype-services
                                'ocml::ACHIEVE-SCORM-OBJECTIVE-GOAL
                                '((ocml::has-method "getURL")
                                  (ocml::has-geturl "Learn Java"))
                                str nil t))))
|#
