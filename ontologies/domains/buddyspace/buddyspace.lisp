;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology buddyspace)



(def-instance PRESENCE-GOAL-ADAPTER input-roles-adapter

  ((HAS-MAPPING "HAS-COMMAND presences")))

 

(def-instance PRESENCE-AFFORDANCE spatial-affordance

  ((has-goal PRESENCE-GOAL)

   (has-input-roles-adapter PRESENCE-GOAL-ADAPTER)))

 

(def-class SESSION-INFO (object-field)

  (;;(has-caller-situation :default-value BUDDYSPACE-CONNECT) 

   ;;(producing-observable :default-value ((is-connected check-is-connected)))

   (has-objects :type buddyspace-SESSION-ID)))

 

(defun check-is-connected (inst)

  (let ((obj (ocml::setofall '?x `(ocml::has-slot-value ,inst 'ocml::has-objects ?x)))

        (is-connected t))

    is-connected))

 

  #|    (sum 0)

      (count 0))

     (loop for o in obj

           do (setf plume-level (ocml::THE-SLOT-VALUE o 'ocml::has-conc))

              (setf sum (+ sum plume-level))

              (setf count (+ count 1)))

     (if (> count 0) (/ sum count) 0)))

|#

 

(def-instance LOGIN-GOAL-ADAPTER input-roles-adapter

  ((HAS-MAPPING "HAS-COMMAND connect" "has-user-name \"irs-iii\"" "has-password \"irs-iii\"")))

 

(def-instance LOGIN-AFFORDANCE spatial-affordance

  ((has-goal LOGIN-GOAL)

   (has-input-roles-adapter LOGIN-GOAL-ADAPTER)))

 

(def-class BUDDYSPACE-CONNECT (spatial-object point-location-marker classifiable-situation)

  ((has-affordances 

    :DEFAULT-VALUE LOGIN-AFFORDANCE)

   ;;(has-requirements :default-value is-really-connected)

   ;;(is-really-connected :default-value not)

   ;;(has-observables :default-value is-connected)

   ;;(is-connected :type boolean :default-value nil)

   (has-following-situation 

         :default-value BUDDYSPACE-SESSION-ID

         :default-value BUDDYSPACE-CONNECT)))

 

(def-class BUDDYSPACE-SESSION-ID (spatial-object point-location-marker)

  ((has-affordances 

    :DEFAULT-VALUE PRESENCE-AFFORDANCE)

   (has-session-id :type string)

   (has-session-number :type integer)

   (has-instance-name :type string)))

 

(def-class PRESENCE-INFO (object-field)

  ((has-objects :type buddyspace-user-connection)))

 

(def-class BUDDYSPACE-USER-CONNECTION (spatial-object point-location-marker person-archetype)

  ((has-user-id :type string)

   (has-status :type string)

   (has-contact-url :type string)

   (has-pretty-name :type string)))

 

(def-class BUDDYSPACE-USER-ONLINE (BUDDYSPACE-USER-CONNECTION) ?x

  :sufficient (has-status ?x "online"))

 

(def-class BUDDYSPACE-USER-AWAY (BUDDYSPACE-USER-CONNECTION) ?x

  :sufficient (has-status ?x "away"))

