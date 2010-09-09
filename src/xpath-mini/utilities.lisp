(in-package #:xpath-mini)

(defun children (node)
  (api-soap-old::dom-children node))

(defun document-parser (string)
  (api-soap-old:soap-xml-parse string))

;;;; Lists

(defun list-filter-nil (l)
  "suppress all nil in a list"
  (let ((res '())) (dolist (el l (reverse res)) (when el (push el res)))))

;(list-filter-nil '(:// "ontology" :/ "text()" NIL NIL NIL))

(defun list-flatten (l)
  "flattens a hierarchical list: '(a (b c (d))) becomes '(a b c d)"
  (cond ((atom l) (list l))
        ((listp l) (append (list-filter-nil (list-flatten (car l))) (list-filter-nil (list-flatten (cdr l)))))
        (t)))

(defun list-flatten-and-conc (sl)
  "flattens nested lists of strings and concatenates the result"
  (reduce #'(lambda (a b) (concatenate 'string a b)) (list-flatten sl)))

;;;; Strings

(defun gen-str (char num) 
  "generates a string of num times char char"
  (let ((res)) (coerce (dotimes (i num res) (push char res)) 'string)))

(defun ind (num)
  "produces indentation space"
  (gen-str #\Space num))

(defun wl (&rest sl ) 
  "specialises write-line for lists of strings"
  (if (loop for el in sl when el collect el) (write-line  (list-flatten-and-conc sl)) ""))

(defun cr () 
  "generates a carriage return"
  (format nil "~%"))

(defun ws (&rest sl ) 
  "specialises write-string for lists of strings"
  (if (loop for el in sl when el collect el) (write-string  (list-flatten-and-conc sl)) ""))

(defun es (&rest sl ) 
  "emits string from lists, concatenating them and avoiding nils (through loop)"
  (if (loop for el in sl when el collect el) (format nil "~a" (list-flatten-and-conc sl)) ""))


(defun split (string &optional (char #\Space))
    "Returns a list of substrings of string
divided by separators."
    (loop for i = 0 then (1+ j)
          as j = (position char string :start i)
          as res = (subseq string i j)
          when (> (length res) 0)
          collect res
          while j))

;(split "toto  toa tost tot")
;(split "toto  toa tost tot" #\t)

(defun string-on-one-line (string)
  "(string-on-one-line \"foo
          WSML to RDF
               \") -> \"foo WSML to RDF\" "
  (let ((i-s (make-string-input-stream string)))
    (do ((s (read-line i-s nil) (read-line i-s nil))
         (res ""))
        ((not s) res)
      (when (> (length (string-trim " " s)) 0)
        (progn 
          (setf res (string-trim " " (concatenate 'string res " " (string-trim " " s)))))))))

;;(string-on-one-line "foo
;;          WSML to RDF
;;")

(defun url-get-local-name (url)
  (subseq url (+ 1 (search "#" url :from-end t))))

(defun url-encode (str)
  (let ((sres (make-string-output-stream))
        (char nil))
    (dotimes (x (length str))
      (setf char (char str x))
      (case char
        (#\% (princ "%25" sres))     
        (t (princ  char sres))))
    (get-output-stream-string sres)))

;;(url-encode "http://uptonpark.open.ac.uk/bswebclient?command=chat&amp;jid=p.alexander%open.ac.uk@buddyspace.org")

(defun inner-xml-encode (str)
  (let ((sres (make-string-output-stream))
        (char nil))
    (dotimes (x (length str))
      (setf char (char str x))
      (case char
        (#\& (princ "&amp;" sres))  
        (#\< (princ "&lt;" sres))  
        (#\> (princ "&gt;" sres)) 
        (t (princ  char sres))))
    (get-output-stream-string sres)))
