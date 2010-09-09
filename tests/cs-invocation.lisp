(in-package :irs.tests)

;;;; Unit tests

(in-package :irs.tests)

(def-suite cs-invocation
  :description "Tests for the context sensitive invocation.")

(in-suite cs-invocation)

(test cs-invocation-test
 (finishes
    (with-output-to-string (str)
      (ip::cs-irs-select-ws 'ocml::booking-request
                                'ocml::get4-booking-request-Goal
                                '(ocml::departing-toulouse ocml::br-euro)
                                '((ocml::has-method "getRequest")
                                  (ocml::has-departure-city "Toulouse")
                                  (ocml::has-departure-country "France")
                                  (ocml::has-arrival-city "Madrid")
                                  (ocml::has-arrival-country "Spain")
                                  (ocml::has-outbound-date "03-02-2008")
                                  (ocml::has-return-date "04-12-2008"))
                                str nil))))

;;;; Testing from a web browser:

;;; http://localhost:8080/cs-invocation/achieve-cs-goal-web?ontology=booking-request&goal=get4-booking-request-Goal&has-method=getRequest&has-departure-city=Toulouse&has-departure-country=France&has-arrival-city=Madrid&has-arrival-country=Spain&has-outbound-date=03-02-2008&has-return-date=04-12-2008&assumption=(%22departing-toulouse%22%20%22Euro%22)      

