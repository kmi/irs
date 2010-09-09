(in-package #:travel)

(defun publish ()
  (publisher:clear-all-services)
  ;; Register services with publisher
  (publisher:irs-wsmo-web-service-registration 
    wsmo-use-case english-buy-train-ticket-service)
  (publisher:irs-wsmo-web-service-registration 
    wsmo-use-case german-buy-train-ticket-service)
  (publisher:irs-wsmo-web-service-registration
    wsmo-use-case french-buy-train-ticket-service)
  (publisher:irs-wsmo-web-service-registration
    wsmo-use-case austrian-buy-train-ticket-service)
  (publisher:irs-wsmo-web-service-registration 
    wsmo-use-case europe-student-buy-train-ticket-service)
  (publisher:irs-wsmo-web-service-registration 
    wsmo-use-case europe-business-buy-train-ticket-service)
  (publisher:irs-wsmo-web-service-registration 
    wsmo-use-case get-train-timetable-service)
  (publisher:irs-wsmo-web-service-registration 
    wsmo-use-case universal-time-buy-train-ticket-mediation-service)
  ;; Publish!
  (publisher:publish-all-wsmo-services "john"))
