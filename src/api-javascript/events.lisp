;;; Copyright © 2007-2009 The Open University

(in-package #:irs.api.javascript)

(define-constant +event-types+
  '(:goal-call :goal-return :register-service :rpc-call :rpc-fault :rpc-return))

(defparameter *keep-alive-interval* 15)

(defvar *event-streams* nil)

(defvar *events* nil)

(defvar *events-length* 25)

(defparameter *event-comment-length* 500)

(defun map-event-streams (function)
  (dolist (stream *event-streams*)
    (handler-case
        (funcall function stream)
      (t (condition)
        (setf *event-streams* (remove stream *event-streams*))))))

(defun current-time-string ()
  (multiple-value-bind (sec min hour)
        (get-decoded-time)
    (format nil "~A:~2,'0D:~2,'0D" hour min sec)))

(defun firstn (list n)
  (subseq list 0 (min n (length list ))))

(defun event (event-type method-instance comment)
  ;; XXX Very bad!  I think the reason is that the writes to the event
  ;; streams are so quick that the browser misses them.  Not sure if
  ;; that's a problem fixable on the client side or if we have to
  ;; solve it here.
  (sleep 0.25)
  (setf *events* (firstn (cons (list (current-time-string) event-type
                                     (string-downcase (iu:get-service-name method-instance))
                                     comment)
                               *events*) *events-length*))
  (send-events (list (first *events*))))

(defun json-of-event (event)
  (format nil "{time: \"~A\", operation: \"~A\", name: \"~A\", content: \"~A\"}"
          (first event) (second event) (third event)
          (js-escape (let ((fullout (format nil "~A" (fourth event))))
                       (subseq fullout 0 (min (length fullout) *event-comment-length*))))))

(defun json-of-events (events)
  (format nil "[~{~A,~}]~%" (mapcar #'json-of-event events)))

(defun send-events (events)
  (let ((json (json-of-events events)))
    (map-event-streams
     #'(lambda (stream) (irs.web:send-multipart stream "text/javascript" json)))))

(defun services-info ()
  ;; now we're going to send a json representation of the initial set of services.
  (let ((content (services-json)))
    (irs.web:write-http-stream "text/javascript" content)))

;;; XXX Refactor with visualiser::add-visualizers.  And get both of
;;; them to avoid double-counting services that appear in several
;;; ontologies.
(defun services-json ()
  "Write a json representation of current services."
  (let ((res nil))
    (dolist (ontology (mapcar #'car ocml::*all-ontologies*))
      (ocml:with-ontology (ontology)
        (dolist (ws (ocml::setofall '?x '(ocml::subclass-of ?x ocml::web-service)))
          (let ((host (wp::web-service-host ws))
                (port (wp::web-service-port ws)))
            (when host
              (push (format nil "~A{service: \"~(~A~)\", host: ~S, port: ~A}" 
                            (if res ", " "")ws host port)
                    res))))))
    (format nil "[~{~A~}]" (reverse res))))

(defun events-info ()
  ;; Get the request stream, set up a callback on the events
   (let ((stream (irs.web:setup-multipart)))
    (irs.web:send-multipart stream "text/javascript" (json-of-events (reverse *events*)))
    (force-output stream)
    (register-for-events stream)))

(defun send-no-op (stream)
  (irs.web:send-multipart stream "text/javascript" "[]~%"))

(defun register-for-events (stream)
  ;; This is for the keep-alive...
  (push stream *event-streams*)
  (send-no-op stream))

;;; We need to periodically send null events to all listening clients,
;;; just to keep the sockets alive.
(defun run-keep-alive-thread ()
  ;; Every five seconds, send an empty JavaScript event array.
  #+:irs-lispworks
  (mp:process-run-function
   "JavaScript event socket keep-alive" nil
   #'(lambda ()
       (loop do
            (sleep *keep-alive-interval*)
            (map-event-streams #'send-no-op)))))
