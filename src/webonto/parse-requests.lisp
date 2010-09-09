;;; Mode: Lisp; Package: web-onto

;;; Author: John Domingue

;;; The Open University

(in-package "WEB-ONTO")

(defun get-receiver (request)
  (with-input-from-string (stream request)
    (dotimes (i 2)
      (read stream))
    (let* ((port (read stream))
           (host (read stream))
           (host-string (format nil "~a" host)))
      (values (subseq host-string 0 (position #\/ host-string))
              port
              (subseq host-string (1+ (position #\/ host-string)))))))

(defun broadcast-message (request)
  (with-input-from-string (stream request)
    (read stream)
    (let ((action (read stream))
          (rest nil))
      (do ((el (read stream nil nil) (read stream nil nil)))
          ((null el))
        (push el rest))
      (string-downcase (format nil "~a ~{~a ~}" action (reverse rest))))))

(defun get-stop-receiver (request)
  (with-input-from-string (stream request)
    (let (port host host-string)
      (read stream)
      (setf port (read stream))
      (read stream)
      (setf host (read stream)
            host-string (format nil "~a" host))
      (values (subseq host-string 0 (position #\/ host-string))
	      port))))

(defun get-stop-receiver-port (request)
  (with-input-from-string (stream request)
    (read stream)
    (read stream)))

(defun start-string (x y)
  (and (>= (length y) (length x))
       (string= x (subseq y 0 (length x)))))

(defun multiple-line-broadcast (string)
  (start-string "current_text" string))

(defun broadcast (request)
  (let ((bad-receivers nil) (string (format nil "~a~%" request)))
    (when (multiple-line-broadcast string)
      (setf string (read-in-multiple-lines string)))
    ;;(add-frame string)
    (dolist (receiver http::*receivers*)
      (unless (eq http::*http-stream* (http::receiver-stream receiver))
        ;;don't broadcast to yourself
        (let ((princ-ok-p
               (http::princ-to-binary-stream string
                                       (http::receiver-stream receiver))))
          (if princ-ok-p
              (finish-output (http::receiver-stream receiver))
              (push receiver bad-receivers)))))
    (http::clean-up-bad-receivers bad-receivers)))

(defvar *string-segment-terminator* "$$$")

(defun read-in-multiple-lines (start-string)
  (do ((result start-string
               (concatenate 'string result
			    (format nil "~a~%" latest)))
       (latest (http::read-from-http-stream *http-stream*)
               (http::read-from-http-stream *http-stream*)))
      ((string= latest *string-segment-terminator*)
       (format nil "~a~a~%" result *string-segment-terminator*))))
