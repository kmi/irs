;;; Copyright © 2008 The Open University

(in-package #:ocml)

(in-ontology rfc2616)

(def-class #_http-message ()
    ((#_has-header :type #_http-header)
     (#_has-content :type string :max-cardinality 1)))

(def-class #_http-request (#_http-message)
    ((#_has-url :type string)
     (#_has-method :type string)))

(def-class #_http-response (#_http-message)
    ((#_has-response-code :cardinality 1)))

(def-class #_http-header ()
    ((#_field-name :type string)
     (#_field-value :type string)))

(def-relation #_header-value (?message ?field-name ?field-value)
  :iff-def (and (#_get-header ?message ?field-name ?header)
                (#_field-value ?header ?field-value)))

(def-rule #_set-header
    "Change the value of the header ?field-name to ?field-value."
  ((#_set-header ?message ?field-name ?field-value) if
   ;; Preconditions
   (#_http-message ?message)
   (string ?field-name)
   (string ?field-value)
   ;; Side-effects
   (#_ensure-header ?message ?field-name ?header)
   (exec (set-slot-value ?header #_field-value ?field-value))))

(def-rule #_ensure-header
    "Get the header with ?field-name from ?http-message, or add it."
  ;; Check if it's already there.  if it is, just unify.  if not, add
  ;; it and unify.
  ((#_ensure-header ?http-message ?field-name ?header) if
   ;; Preconditions
   (#_http-message ?http-message)
   (string ?field-name)
   ;;
   (or (#_get-header ?http-message ?field-name ?header)
       (#_add-header ?http-message ?field-name ?header))))

(def-rule #_add-header
    ((#_add-header ?message ?field-name ?header) if
     ;; Precondition
     (#_http-message ?message)
     (string ?field-name)
     (not (#_get-header ?message ?field-name ?header))
     ;; Side-effects
     (= ?header (#_new-instance #_http-header))
     (exec (set-slot-value ?header #_field-name ?field-name))
     (exec (add-slot-value ?message #_has-header ?header))))

(def-rule #_get-header
    ((#_get-header ?message ?field-name ?header) if
     ;; Type check
     (#_http-message ?message)
     (#_is-field-name ?field-name)
     ;;
     (#_has-header ?message ?header)
     (#_field-name ?header ?field-name)))

(def-relation #_is-field-name (?thing)
  "Check if ?thing is a valid #_field-type."
  ;; XXX Needs to be more specific wrt RFC2616.
  :iff-def (string ?thing))

(def-relation #_get-content (?message ?content)
  :iff-def (#_has-content ?message ?content))

(def-rule #_set-content
    ((#_set-content ?message ?content) if
     (#_http-message ?message)
     (exec (set-slot-value ?message #_has-content ?content))))

(def-rule #_set-method
    ((#_set-method ?request ?method) if
     (#_http-request ?request)
     (exec (set-slot-value ?request #_has-method ?method))))

(def-rule #_set-url
    ((#_set-url ?request ?url) if
     (#_http-request ?request)
     (exec (set-slot-value ?request #_has-url ?url))))

;;; {{{ Time and date

(def-class #_time ()
    "Just a type tag for Unix epoch time."
    ((#_has-value :type integer :cardinality 1)))

(def-function #_format-http-time (?time)
  "Produce an HTTP-style time string showing ?TIME."
  :lisp-fun (lambda (time)
              (let ((inst (first (find-all-current-instances-named-x time))))
                (%http-date (the-instance-slot-value inst '#_has-value)))))

;;; }}}

;;; {{{ Stuff to move to a yet more general ontology
(def-function #_new-instance (?class)
  :lisp-fun (lambda (class)
              (let ((i (new-instance class)))
                (name i))))
;;; }}}

;;; Differs from set-slot-value in that it does not retract the
;;; previous slot value.
(def-procedure add-SLOT-VALUE (?i ?s ?v)
  :constraint (and (instance-of ?i ?c)
                   (slot-of ?s ?c))
  :body (do
          (tell (list-of ?s ?i ?v))))
