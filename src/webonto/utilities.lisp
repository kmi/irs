;;; Mode: Lisp; Package: web-onto

;;; Author: John Domingue

;;; The Open University

(in-package "WEB-ONTO")

(defvar *string-height* 12)

(defvar *large-characters-table*
  '((#\a . 10) (#\b . 10) (#\c . 9) (#\d . 10) (#\e . 9) (#\f . 6) (#\g . 10)
    (#\h . 10) (#\i . 5) (#\j . 6) (#\k . 11) (#\l . 5) (#\m . 15) (#\n . 10)
    (#\o . 10) (#\p . 10) (#\q . 10) (#\r . 8) (#\s . 8) (#\t . 6) (#\u . 10) 
    (#\v . 9) (#\w . 14) (#\x . 9) (#\y . 9) (#\z . 8) (#\A . 13) (#\B . 13) 
    (#\C . 14) (#\D . 14) (#\E . 13) (#\F . 12) (#\G . 15) (#\H . 14) (#\I . 7) 
    (#\J . 10) (#\K . 14) (#\L . 12) (#\M . 18) (#\N . 14) (#\O . 15) (#\P . 12)
    (#\Q . 15) (#\R . 14) (#\S . 11) (#\T . 13) (#\U . 13) (#\V . 13) (#\W . 19) 
    (#\X . 13) (#\Y . 13) (#\Z . 12) (#\1 . 10) (#\2 . 10) (#\3 . 10) (#\4 . 10) 
    (#\5 . 10) (#\6 . 10) (#\7 . 10) (#\8 . 10) (#\9 . 10) (#\0 . 10) (#\` . 6) 
    (#\¬ . 11) (#\\ . 5) (#\| . 3) (#\, . 5) (#\< . 11) (#\. . 5) (#\> . 11) 
    (#\/ . 5) (#\? . 10) (#\' . 5) (#\@ . 18) (#\; . 5) (#\: . 5) (#\# . 10) 
    (#\~ . 10) (#\( . 6) (#\{ . 7) (#\[ . 6) (#\) . 6) (#\} . 7) (#\] . 6) 
    (#\- . 6) (#\_ . 10) (#\= . 11) (#\+ . 11) (#\* . 10) (#\& . 16) (#\^ . 11)
    (#\% . 18) (#\$ . 10) (#\£ . 10) (#\" . 11) (#\! . 5) (#\Tab . 15) (#\Space . 5) 
    (#\Return . 15) (#\Newline . 30) (#\ä . 10) (#\Ä . 13) (#\ö . 10) (#\Ö . 15) 
    (#\å . 10) (#\Å . 13) (#\ó . 10) (#\Ó . 15))
  "Character table for times bold 14 to use for webonto presentations")

(defvar *original-characters-table*
  '((#\a . 5) (#\b . 5) (#\c . 4) (#\d . 5) (#\e . 4) (#\f . 3) (#\g . 5)
    (#\h . 6) (#\i . 3) (#\j . 3) (#\k . 5) (#\l . 3) (#\m . 8) (#\n . 6)
    (#\o . 5) (#\p . 5) (#\q . 5) (#\r . 4) (#\s . 4) (#\t . 3) (#\u . 6)
    (#\v . 4) (#\w . 7) (#\x . 5) (#\y . 4) (#\z . 4) (#\A . 7) (#\B . 7)
    (#\C . 7) (#\D . 7) (#\E . 7) (#\F . 7) (#\G . 7) (#\H . 8) (#\I . 4)
    (#\J . 5) (#\K . 7) (#\L . 6) (#\M . 9) (#\N . 7) (#\O . 7) (#\P . 6)
    (#\Q . 7) (#\R . 7) (#\S . 6) (#\T . 6) (#\U . 7) (#\V . 7) (#\W . 9)
    (#\X . 7) (#\Y . 6) (#\Z . 6) (#\1 . 5) (#\2 . 5) (#\3 . 5) (#\4 . 5)
    (#\5 . 5) (#\6 . 5) (#\7 . 5) (#\8 . 5) (#\9 . 5) (#\0 . 5) (#\` . 3)
    (#\¬ . 6) (#\\ . 3) (#\| . 2) (#\, . 3) (#\< . 6) (#\. . 3) (#\> . 6)
    (#\/ . 3) (#\? . 5) (#\' . 3) (#\@ . 8) (#\; . 3) (#\: . 3) (#\# . 5)
    (#\~ . 6) (#\( . 3) (#\{ . 3) (#\[ . 3) (#\) . 3) (#\} . 3) (#\] . 3)
    (#\- . 6) (#\_ . 5) (#\= . 6) (#\+ . 6) (#\* . 5) (#\& . 8) (#\^ . 6)
    (#\% . 8) (#\$ . 5) (#\£ . 5) (#\" . 6) (#\! . 4) (#\Tab . 3) (#\Space . 3)
    (#\Return . 3) (#\Newline . 3) (#\ä . 5) (#\Ä . 7) (#\ö . 5) (#\Ö . 7)
    (#\å . 5) (#\Å . 7) (#\ó . 5) (#\Ó . 7)))


(defvar *characters-table* *original-characters-table*)

(defun use-large-characters ()
  (setf *characters-table* *large-characters-table*
        *web-onto-font-size* 14))

(defun use-normal-characters ()
  (setf *characters-table* *original-characters-table*
        *web-onto-font-size* 10))

(defun lookup-char-width (char)
  (cdr (assoc char *characters-table*)))

(defun string-width (string font)
  (declare (ignore font))
  (let ((count 0))
    (dotimes (i (length string))
      (incf count (lookup-char-width (elt string i))))
    count))

(defun string-height (string font)
  (declare (ignore font))
  *string-height*)

(defun find-system (name)
  #+lispworks
  (scm::find-system name nil)
  #+allegro
  (excl:find-system name))

(defun setup-colours ()
  (setf *class-colour*
        ;;(colour-name-to-rgb-string :green)
        "0.0000000000%1.0000000000%0.0000000000"
        *axiom-colour*
        ;;(colour-name-to-rgb-string :green)
        "0.0000000000%1.0000000000%0.0000000000"
        *instance-colour*
        ;;(colour-name-to-rgb-string :Turquoise)
        "0.2509800000%0.8784200000%0.8156700000"
        *relation-colour*
        ;;(colour-name-to-rgb-string :lightblue)
        "0.6784000000%0.8470500000%0.9019500000"
        *relation-instance-colour*
        ;;(colour-name-to-rgb-string :powderblue)
        "0.6902000000%0.8784200000%0.9019500000"
        *function-colour*
        ;;(colour-name-to-rgb-string :violet)
        "0.9333200000%0.5098000000%0.9333200000"
        *procedure-colour*
        ;;(colour-name-to-rgb-string :palevioletred)
        "0.8588000000%0.4392100000%0.5764500000"
        *rule-colour*
        ;;(colour-name-to-rgb-string :bisque)
        "1.0000000000%0.8941000000%0.7686200000"
        *ontology-colour* 
        ;;(make-rgb-string .7 .7 .7)
        "0.7000000000%0.7000000000%0.7000000000"
        *problem-solving-method-colour*
        ;;(colour-name-to-rgb-string :skyblue)
        "0.5294000000%0.8078300000%0.9215400000"
        *task-colour*
        ;;(colour-name-to-rgb-string :yellow)
        "1.0000000000%1.0000000000%0.0000000000"
        *task-ontology-colour* *task-colour*
        *domain-ontology-colour*
        ;;(colour-name-to-rgb-string :lightgoldenrod1)
        "1.0000000000%0.9254800000%0.5450700000"
        *application-ontology-colour*
        ;;(colour-name-to-rgb-string :indianred1)
        "1.0000000000%0.4156800000%0.4156800000"
        *method-ontology-colour* *problem-solving-method-colour*
        *base-ontology-colour*
        ;;(colour-name-to-rgb-string :wheat1)
        "1.0000000000%0.9058500000%0.7294000000"
        *class-text-colour*
        ;;(colour-name-to-rgb-string :darkgreen) ;;;blue
        "0.0000000000%0.3921500000%0.0000000000"
        *axiom-text-colour*
        ;;(colour-name-to-rgb-string :darkseagreen)
        "0.5607600000%0.7372400000%0.5607600000"
        *instance-text-colour*
        ;;(colour-name-to-rgb-string :darkTurquoise) ;;green
        "0.0000000000%0.8078300000%0.8195800000"
        *relation-text-colour*
        ;;;(colour-name-to-rgb-string :blue) ;;lilac (make-rgb-string .47 .37 .51)
        "0.0000000000%0.0000000000%1.0000000000"
        *function-text-colour*
        ;;(colour-name-to-rgb-string :darkviolet) ;;magenta
        "0.5803800000%0.0000000000%0.8274200000"
        *procedure-text-colour*
        ;;(colour-name-to-rgb-string :violetred3) 
        "0.8039000000%0.1960750000%0.4705800000"
        *rule-text-colour*
        ;;(colour-name-to-rgb-string :bisque4) ;;(make-rgb-string .74 .46 0)) ;; yellow
        "0.5450700000%0.4901900000%0.4196000000"
        *ontology-text-colour*
        ;;(colour-name-to-rgb-string :darkgoldenrod)  ;;(make-rgb-string .3 .3 .3) ;;grey
        "0.7215600000%0.5254800000%0.0431370000"
        *task-text-colour*
        ;;(colour-name-to-rgb-string :yellow4)
        "0.5450700000%0.5450700000%0.0000000000"
	*problem-solving-method-text-colour*
        ;;(colour-name-to-rgb-string :navyblue)
        "0.0000000000%0.0000000000%0.5019500000")
  (setf *ontology-type-colour-mapping*
        `((:application . ,*application-ontology-colour*)
          (:domain . ,*domain-ontology-colour* )
          (:method  . ,*method-ontology-colour*)
          (:task . ,*task-ontology-colour*)
          (:basic . ,*base-ontology-colour*))))

;;;apply complains if the number of arguments is too long
;;;so this doesn't use apply
(defun concatenate* (strings)
  (let ((result ""))
    (dolist (string strings)
      (setf result (concatenate 'string result string)))
    result))
