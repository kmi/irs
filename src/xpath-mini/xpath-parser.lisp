
(in-package :kmi.vt.xpath-mini)

(defparser xpath-parser
  ((start PathExpr) $1)

  ;PathExpr ::= ("/" RelativePathExpr?)
  ;| ("//" RelativePathExpr)
  ;| RelativePathExpr

  ((PathExpr :/) `(apply-leading-/ :/))
  ((PathExpr :/ RelativePathExpr) `(apply-leading-/ ,$2))
  
  ((PathExpr ://) `(apply-leading-//))
  ((PathExpr :// RelativePathExpr) `(apply-leading-// ,$2))
  
  ((PathExpr RelativePathExpr) $1)

  ;RelativePathExpr ::= StepExpr (("/" | "//") StepExpr)*
  ((RelativePathExpr StepExpr) $1)
  ((RelativePathExpr StepExpr :/ RelativePathExpr) `(,$1 (apply-inner-/ ,$3)))
  ((RelativePathExpr StepExpr :// RelativePathExpr) `(,$1 (apply-inner-// ,$3)))

  ;StepExpr ::= AxisStep | FilterExpr
  ((StepExpr AxisStep) $1)
  ((StepExpr FilterExpr) $1)

  ;AxisStep ::= (ForwardStep | ReverseStep) PredicateList
  ((AxisStep ForwardStep) $1) ;..
  ;((AxisStep ReverseStep) $1) ;..

  ;ForwardStep ::= (ForwardAxis NodeTest) | AbbrevForwardStep
  ((ForwardStep ForwardAxis NodeTest) `(,$1 ,$2))
  ((ForwardStep AbbrevForwardStep) $1)

  ;ForwardAxis ::= <"child" "::">
  ;| <"descendant" "::">
  ;| <"attribute" "::">
  ;| <"self" "::">
  ;| <"descendant-or-self" "::">
  ;| <"following-sibling" "::">
  ;| <"following" "::">
  ;| <"namespace" "::">
  ((ForwardAxis :child) 'apply-child)
  ((ForwardAxis :attribute) 'apply-attribute) ;..

  ;AbbrevForwardStep ::= "@"? NodeTest
  ((AbbrevForwardStep :@ NodeTest) `(apply-attribute ,$2))
  ((AbbrevForwardStep NodeTest) `(apply-child ,$1)) ;..

  ;ReverseStep ::= (ReverseAxis NodeTest) | AbbrevReverseStep
  

  ;NodeTest ::= KindTest | NameTest
  ((NodeTest NameTest) $1)
  ((NodeTest KindTest) $1)

  ;NameTest ::= QName | Wildcard
  ;QName ::= [http://www.w3.org/TR/REC-xml-names/#NT-QName]  

  ((NameTest :qname) $1)
  ((NameTest Wildcard) $1)

  ;Wildcard ::= "*"
  ;| <NCName ":" "*">
  ;| <"*" ":" NCName>

  ((Wildcard :*) "*") ;..

  ;KindTest ::= DocumentTest
  ;| ElementTest
  ;| AttributeTest
  ;| SchemaElementTest
  ;| SchemaAttributeTest
  ;| PITest
  ;| CommentTest
  ;| TextTest
  ;| AnyKindTest

  ((KindTest TextTest) $1) ;..

  ;TextTest ::= <"text" "("> ")"
  
  ((TextTest :text) $1)

  ;FilterExpr ::= PrimaryExpr PredicateList !!!!!!!!!!!!!!1Modified!!!
  ((FilterExpr StepExpr PredicateList) `(,$1 ,$2))

  ;PredicateList ::= Predicate*
  ((PredicateList Predicate) $1)
  ((PredicateList Predicate PredicateList) `(,$1 ,$2))

  ;Predicate ::= "[" Expr "]" !!!!!!!!!!!11for now only numbers!!!!
  ((Predicate :[ :num :]) `(apply-predicate-begin ,$2 apply-predicate-end))

)

;(parse-xpath "//child::toto")
;(parse-xpath "//@toto")
;(parse-xpath "//toto")
;(parse-xpath "//child::toto")
;(parse-xpath "//child::toto/@tata")
;(parse-xpath "/child::toto/tata")
;(parse-xpath "//toto/tata")
;(parse-xpath "//ontology")
;(parse-xpath "//ontology/text()")

(defclass scanner ()  
  ((ch
    :accessor ch
    :initform nil
    :documentation "the character being parsed")
   (oldch
    :accessor oldch
    :initform nil
    :documentation "the last parsed character")
   (chars
    :accessor chars
    :initform ""
    :documentation "the representation of the lexem (if more than one possible, ex:variables)")
   (terminals
    :initform nil))
  (:documentation "generic scanner (lexer) class"))

(defclass xpath-scanner (scanner)
  ((position
    :accessor pos
    :initform 0
    :documentation "position in the string being parsed, 0 based")
   (in
    :accessor in
    :initarg :expression
    :initform ""
    :documentation "string which contains the xpath")
   (terminals
    :initform '(;PathExpr
                ("/" :/) ("//" ://)
                ;ForwardAxis
                ("child::" :child) ("attribute::" :attribute) ;...
                ;AbbrevForwardStep
                ("@" :@)
                ;ReverseAxis
                ("parent::" :parent) ;...
                ;AbbrevReverseStep
                (".." :..)
                ;Wildcard
                ("*" :*)
                ;Predicate
                ("[" :[) ("]" :])
                )))
  (:documentation "scanner (lexer) for xpath"))

;(setf xp/ (make-instance 'xpath-scanner :expression "/"))
;(setf xp// (make-instance 'xpath-scanner :expression "//"))
;(describe xp/)
;(describe xp//)
;(slot-value  xp/ 'ch)

(defgeneric scan-get-terminal (scanner s)
  (:documentation "gets the terminal from the terminals association list")
  (:method ((sc scanner) s)
           (with-slots (terminals) sc
             (or (cadr (assoc s terminals :test #'string-equal))
                 :unknown))))

;(scan-get-terminal xp// "/")
;(scan-get-terminal xp// "child::")

(defgeneric next-token (scanner)
  (:documentation "gets the next token and avoids whitespace and comments")
  (:method ((sc xpath-scanner))
           (if (next-ch sc)
               (read-token sc)
             :eoi)))

(defparameter *alphanum-chars*
  (append
   (loop for char across "abcdefghijklmnopqrstuvwxyz" 
         collecting char into lc 
         collecting (char-upcase char) into uc 
         finally (return (append lc uc)))
   (loop for char across "0123456789" collecting char)
   '(#\: #\( #\) #\-)))

(defgeneric read-token (scanner)
  (:documentation "reads next token without whitespace")
  (:method ((sc xpath-scanner))
   (with-slots (ch chars) sc
     (setf chars "")
     (cond               
       
       ((eql ch #\/)
        (if (and (peek-next-ch sc) (char-equal #\/ (peek-next-ch sc)))
            (progn 
              (next-ch sc)
              (setf chars "//")
              (scan-get-terminal sc chars))
          (progn
            (setf chars "/")
            (scan-get-terminal sc chars))))
       
       ((member ch *alphanum-chars*)
        (progn 
          (setf chars (make-array 0 ;in order to use #'vector-push-extend
                                  :element-type 'character
                                  :fill-pointer 0
                                  :adjustable t))
          (vector-push-extend ch chars)
          (do ((n-ch (peek-next-ch sc) 
                     (progn 
                       (vector-push-extend n-ch chars)
                       (next-ch sc)
                       (peek-next-ch sc))))
              ((or (not n-ch) (not (member n-ch *alphanum-chars*)) (char-equal n-ch #\:)) )) 
          ;(print chars)
          (cond
           ((and (peek-next-ch sc) (char-equal (peek-next-ch sc) #\:)) ;test if "::" at the end
            (progn
              (next-ch sc)
              (next-ch sc)
              (vector-push-extend #\: chars)
              (vector-push-extend #\: chars)
              ;(print chars)
              (scan-get-terminal sc chars)))
           ((string-equal chars "text()") :text)
           ((parse-integer chars :junk-allowed t) :num )
           (t :qname))))

       ((member ch '(#\* #\@ #\[ #\]))
        (progn
          (setf chars ch)
          (scan-get-terminal sc chars)))
       
       (t :unknown)))))


;(parse-xpath "//child::*")
;(parse-xpath "//child::*")
;(parse-xpath "//child::toto[1]")
;(parse-xpath "//@toto")
;(reverse (subseq (reverse "//toto::") 2))

;(setf xp/ (make-instance 'xpath-scanner :expression "/"))
;(next-token xp/)
;(setf xp// (make-instance 'xpath-scanner :expression "//"))
;(next-token xp//)

(defgeneric next-ch (scanner)
  (:documentation "puts next char in ch and updates position")
  (:method ((sc xpath-scanner))
           (with-slots (ch oldch position in) sc
             (setf oldch ch)
             (if (< position (length in)) 
                 (progn
                   (setf ch (char in position))
                   (incf position)
                   ch)
               nil))))

;(setf xp/ (make-instance 'xpath-scanner :expression "/"))
;(next-ch xp/)
;(setf xp// (make-instance 'xpath-scanner :expression "//"))
;(next-ch xp//)
;(describe xp//)

(defgeneric peek-next-ch (scanner)
  (:documentation "just looks at next char and returns it")
  (:method ((sc xpath-scanner))
   (with-slots (position in) sc
     (when (< position (length in))
       (char in position)))))

;(peek-next-ch xp/)
;(peek-next-ch xp//)

;;; The main function -- note bindings of globals (these
;;; are exported from the parsergen package).

(defun lex-xpath (sc)
  (let ((token (next-token sc)))
    (values token (chars sc))))

;(setf xp/ (make-instance 'xpath-scanner :expression "/"))
;(lex-xpath xp/)
;(setf xp// (make-instance 'xpath-scanner :expression "//"))
;(lex-xpath xp//)

(defun parse-xpath (input)
  "parses an xpath string and returns a list of actions (yeah, kind of minimal AST)"
  (list-flatten
   (let ((sc (make-instance 'xpath-scanner :expression input)))
     (xpath-parser #'(lambda () (lex-xpath sc))))))

(defun apply-xpath (xpath sequence)
  "executes the xpath instructions"
  (let ((op1 (first xpath))
        (rest-path (rest xpath)))
    ;; XXX This should not be here!
    ;; (declare (special APPLY-LEADING-/ APPLY-CHILD APPLY-INNER-/ APPLY-ATTRIBUTE ))
    (if (not op1) sequence ;if path expression empty return sequence
      (ccase op1
        
        (APPLY-LEADING-/
         ;(when (xqdm:parent sequence) (error "not top level, cannot apply lading /"))
         (apply-xpath rest-path (apply *APPLY-LEADING-/* (list sequence))))
        
        (APPLY-CHILD
         (let ((nodetest (first rest-path)))
           (unless nodetest (error "forward step without specifier"))
           (apply-xpath (cdr rest-path) (apply *APPLY-CHILD* nodetest (list sequence)))))

        (APPLY-INNER-/
         (apply-xpath rest-path sequence))

        (APPLY-ATTRIBUTE
         (let ((attrtest (first rest-path)))
           (unless attrtest (error "attribute test without specifier"))
           (apply-xpath (cdr rest-path) (apply *APPLY-ATTRIBUTE* attrtest (list sequence)))))

        (APPLY-PREDICATE-BEGIN ;for now only numerical prediactes ;-)
         (let ((predicate  (1- (parse-integer (first rest-path)))))
           (when (< predicate 0 ) (error "index below 0!!"))
           (when (>= predicate (length sequence)) (error "index too large!!"))
           (apply-xpath (cddr rest-path) (elt sequence predicate))))))))
      
(defun xp (xpath sequence)
  "parses and executes xpath expressions"
  (apply-xpath (parse-xpath xpath) sequence))



