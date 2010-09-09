(in-package cl-user)


(eval-when (eval load)
  (irs-wsmo-web-service-registration wsmo-use-case
                                     english-buy-train-ticket-service)
;;  (irs-wsmo-web-service-registration wsmo-use-case
;;                                     german-buy-train-ticket-service)
  (irs-wsmo-web-service-registration wsmo-use-case
                                     french-buy-train-ticket-service)
  (irs-wsmo-web-service-registration wsmo-use-case
                                     austrian-buy-train-ticket-service)
  (irs-wsmo-web-service-registration wsmo-use-case
                                     europe-student-buy-train-ticket-service)
  (irs-wsmo-web-service-registration wsmo-use-case
                                     europe-business-buy-train-ticket-service)
  (irs-wsmo-web-service-registration wsmo-use-case
                                     get-train-timetable-service)
;;  (irs-wsmo-web-service-registration wsmo-use-case
;;                                     universal-time-buy-train-ticket-mediation-service)
  )

#|
(progn 
  (clear-all-services)
(irs-wsmo-web-service-registration wsmo-use-case
                                     german-buy-train-ticket-service)
  (irs-wsmo-web-service-registration wsmo-use-case
                                     austrian-buy-train-ticket-service)
  (irs-wsmo-web-service-registration wsmo-use-case
                                     universal-time-buy-train-ticket-mediation-service)
  (publish-all-wsmo-services "john")
)
|#

#|
(eval-when (eval load)
  (irs-wsmo-web-service-registration wsmo-use-case
                                     english-buy-train-ticket-service
                                     book-english-train-journey)
  (irs-wsmo-web-service-registration wsmo-use-case
                                     german-buy-train-ticket-service
                                     ((book-standard-german-train-journey
                                       normal)
                                      (book-professor-german-train-journey
                                       first-class-upgrade)))
  (irs-wsmo-web-service-registration wsmo-use-case
                                     french-buy-train-ticket-service
                                     book-french-train-journey)
  (irs-wsmo-web-service-registration wsmo-use-case
                                     austrian-buy-train-ticket-service
                                     book-austrian-train-journey)
  (irs-wsmo-web-service-registration wsmo-use-case
                                     europe-student-buy-train-ticket-service
                                     book-student-european-train-journey)
  (irs-wsmo-web-service-registration wsmo-use-case
                                     europe-business-buy-train-ticket-service
                                     book-business-european-train-journey)
  (irs-wsmo-web-service-registration wsmo-use-case
                                     get-train-timetable-service
                                     get-train-times)
  (irs-wsmo-web-service-registration wsmo-use-case
                                     universal-time-buy-train-ticket-mediation-service
                                     mediate-time)
  )
|#

#|
(achieve-goal-through-irs 'wsmo-use-case 'get-train-timetable-goal
                          '(ocml::has-departure-station paris)
                          '(ocml::has-destination-station vienna)
                          '(ocml::has-date-and-time (18 8 2004)))


(achieve-goal-through-irs 'wsmo-use-case 'buy-train-ticket-goal
                          '(ocml::has-person john)
                          '(ocml::has-departure-station paris)
                          '(ocml::has-destination-station vienna)
                          '(ocml::has-date-and-time (3 4 6 18 8 2004)))

(achieve-goal-through-irs 'wsmo-use-case 'buy-train-ticket-goal
                          '(ocml::has-person christoph)
                          '(ocml::has-departure-station frankfurt)
                          '(ocml::has-destination-station berlin)
                          '(ocml::has-date-and-time (3 4 6 18 8 2004)))

(achieve-goal-through-irs 'wsmo-use-case 'buy-train-ticket-goal
                          '(ocml::has-person liliana)
                          '(ocml::has-departure-station frankfurt)
                          '(ocml::has-destination-station berlin)
                          '(ocml::has-date-and-time (3 4 6 18 8 2004)))

(achieve-goal-through-irs 'wsmo-use-case 'buy-train-ticket-goal
                          '(ocml::has-person john)
                          '(ocml::has-departure-station london)
                          '(ocml::has-destination-station milton-keynes)
                          '(ocml::has-date-and-time (3 4 6 18 8 2004)))

(achieve-goal-through-irs 'wsmo-use-case 'buy-train-ticket-goal
                          '(ocml::has-person fred)
                          '(ocml::has-departure-station frankfurt)
                          '(ocml::has-destination-station berlin)
                          '(ocml::has-date-and-time (3 4 6 18 8 2004)))
|#