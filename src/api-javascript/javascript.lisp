;;; Copyright © 2007,2008 The Open University

(in-package #:irs.api.javascript)

(defstruct json-object
  (fields))

(defstruct json-function
  (string))

(defun js-object (&rest name-value-pairs)
  (make-json-object :fields name-value-pairs))

(defun js-function (function-string)
  (make-json-function :string function-string))

(defun js-escape (string)
  "Escape STRING so that it is a valid Javascript string."
  (with-output-to-string (str)
    (loop for c across string
       do (cond ((char= c #\")
                 (format str "\\\""))
                ((char= c #\newline)
                 (format str "\\n"))
                ((char= c #\return)
                 (format str "\\r"))
                (t
                 (format str "~A" c))))))

(defun value-mapper (value)
  "Turn Lisp value VALUE into a JSON equivalent."
  (cond
    ((symbolp value) (extern-ocml-symbol value))
    ((utilities:xsd-date-p value)
     (let ((ymd (utilities::year/month/day value)))
       (js-object
        "type" "xsd-date"
        "toString" (js-function
                    (format nil "function(){return \"~A\";}"
                            (js-escape (format nil "~A-~A-~A" (first ymd)
                                               (second ymd) (third ymd))))))))
    (t value)))

(defun serialise-json (thing)
  "Convert the THING of JSON to a string."
  (with-output-to-string (str)
    (labels
        ((do-thing (thing)
           (cond ((null thing)
                  (format str "null"))
                 ((json-function-p thing)
                  (do-json-function thing))
                 ((json-object-p thing)
                  (do-json-object (json-object-fields thing)))
                 ((listp thing)
                  (do-sexp thing))
                 ((stringp thing)
                  (do-string thing))
                 ((vectorp thing)
                  (do-array thing))
                 ((numberp thing)
                  (do-number thing))
                 ((symbolp thing)
                  (do-symbol thing))
                 (t
                  (error "Don't know what to do with ~A." thing))))
         (do-sexp (sexp)
           (format str "{\"type\": \"sexp\",\"toString\": function(){return \"~A\";}}"
                   (js-escape (format nil "~S" sexp))))
         (do-symbol (symbol)
           (format str "\"~A\"" (symbol-name symbol)))
         (do-json-object (name-value-pairs)
           (format str "{")
           (loop while name-value-pairs
              do
              (let ((name (first name-value-pairs))
                    (value (second name-value-pairs)))
                (setf name-value-pairs (subseq name-value-pairs 2))
                (format str "\"~A\":" name)
                (do-thing value)
                (when name-value-pairs
                  (format str ","))))
           (format str "}"))
         (do-json-function (function)
           (format str (json-function-string function)))
         (do-array (values)
           (cond ((and (> (length values) 0)
                       (eq 'ocml::kappa (aref values 0)))
                  (format str "\"~S\"" values))
                 (t
                  (format str "[")
                  (dotimes (i (length values))
                    (do-thing (aref values i))
                    (when (< i (- (length values) 1))
                      (format str ",")))
                  (format str "]"))))
         (do-string (string)
           (format str "\"")
           (write-string (js-escape string) str)
           (format str "\""))
         (do-number (number)
           (format str "~A" number)))
      (do-thing thing))))
