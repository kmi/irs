;;; Mode: Lisp; Package: ocml

; A Parser for the Cashew Surface Language which generates an OCML Cashew Ontology.

(in-package #:cashew)

(defvar *ocml-definitions* nil)

(defparser cashew-parser 
           ((cw workflow) $1)
           
           ((workflow :workflow :name :lparen acceptors :rparen :lparen offerors :rparen pattern) 
            (let ((w (read-from-string $2)))
              (progn 
                ; (print '(definining workflow))
                ; Call the function to generate the acceptors related to this workflow
                (funcall $4 w)
                ; Call the function to generate the offerors
                (funcall $7 w)
                ; Call the function to generate the workflow pattern
                (funcall $9 w)
                w)))

           ((acceptors acceptor :comma acceptors) (lambda (x) (cons (funcall $1 x) (funcall $2 x))))
           ((acceptors acceptor) (lambda (x) (funcall $1 x)))
           ((acceptor :accept :name :dot :name :name :dot :name :dot :name) 
            (lambda (w)
              (let ((n (concatenate 'string "pf-acc-" $5 "_" $9)))
                (progn
                  ; Generate a pf mediator for this acceptor
                  (push `(,n 
                          (ocml::def-class ,(read-from-string n) (ocml::pf-mediator)
                            ((ocml::has-source-component :value ,w)
                             (ocml::has-target-component :value ,(read-from-string $5))
                             (ocml::has-mediation-service :value 
                                                          ,(read-from-string (concatenate 'string "goal-" n)))))
                                               ) *ocml-definitions*)
                  ; Generate a mediation goal for the acceptor [INCOMPLETE]
                  (push `(,(concatenate 'string "goal-" n)
                          (ocml::def-class ,(read-from-string (concatenate 'string "goal-" n)) (ocml::goal))) *ocml-definitions*)
                  ))))
              
           ((offerors offeror :comma offerors) (lambda (x) (cons (funcall $1 x) (funcall $2 x))))
           ((offerors offeror) (lambda (x) (funcall $1 x)))
           ((offeror :offer :name :dot :name :dot :name :name :dot :name)
            (lambda (w)
              (let ((n (concatenate 'string "pf-off-" $2 "_" $6)))
                (progn                  
                  ; Generate a pf mediator for this offeror
                  (push `(,n (ocml::def-class ,(read-from-string n) (ocml::pf-mediator)
                               ((ocml::has-source-component :value ,(read-from-string $2))
                                (ocml::has-target-component :value ,w)
                                (ocml::has-mediation-service :value 
                                                             ,(read-from-string (concatenate 'string "goal-" n))))
                               )
                             ) *ocml-definitions*)
                  ; Generate a mediation goal [INCOMPLETE]
                  (push `(,(concatenate 'string "goal-" n)
                          (ocml::def-class ,(read-from-string (concatenate 'string "goal-" n)) (ocml::goal))) *ocml-definitions*)
                  ))))
           
           ; Sequential Pattern
           ((pattern :sequential :lparen performance-list :rparen) 
            (lambda (n) 
              (progn 
                (print $3)
                (push `(,(prin1-to-string n) (ocml::def-class ,n (ocml::sequential)
                            ((ocml::has-perform-list 
                              :value ,$3)))) *ocml-definitions*)
                (print (append '(defining sequence) `(,n) $3)))))

           ; Concurrent Pattern
           ((pattern :concurrent :lparen performance-list :rparen) 
            (lambda (n) 
              (progn 
                (push `(,(prin1-to-string n) (ocml::def-class ,n (ocml::concurrent)
                         ((ocml::has-perform-list
                           :value ,$3)))) *ocml-definitions*)
                (print (append '(defining concurrent) `(,n) $3)))))

           ; Interleaved Pattern
           ((pattern :interleaved :lparen performance-list :rparen) 
            (lambda (n) 
              (progn 
                (push `(,(prin1-to-string n) (ocml::def-class ,n (ocml::interleaved)
                         ((ocml::has-perform-list
                           :value ,$3)))) *ocml-definitions*)
                (print (append '(defining interleaved) `(,n) $3)))))

           ; XOR Pattern [CONDITION MISSING]
           ((pattern :xor perform perform) 
            (lambda (n) 
              (progn (print (append '(defining xor) `(,n) `(,$2) `(,$3))))))

           ; Deferred Choice [NEEDS UPDATING INLINE WITH CASHEW-O FOR ATOMIC DEFERRED CHOICE]
           ((pattern :deferred-choice :lparen performance-list :rparen)            
            (lambda (n) 
              (progn 
                (push `(,(prin1-to-string n) (ocml::def-class ,n (ocml::deferred-choice)
                          ((ocml::has-perform-list
                            :value ,$3)))) *ocml-definitions*))))

           ; Deferred While [CONDITION MISSING]
           ((pattern :deferred-while perform perform) 
            (lambda (n) 
              (progn 
                (push `(,(prin1-to-string n) (ocml::def-class ,n (ocml::deferred-while) 
                          ((break :value $2)
                           (body :value $3)))) *ocml-definitions*)
                (print (append '(defining deferred-while) `(,n) `(,$2) `(,$3))))))

           ; Loop Until [INCOMPLETE]
           ((pattern :until condition perform) (lambda (n) (progn (print (append '(defining until) `(,n) `(,$2))))))

           ; While Loop [INCOMPLETE]
           ((pattern :while condition perform) (lambda (n) (progn (print (append '(defining while) `(,n) `(,$2))))))

           ((connection :connect :name :dot :name :dot :name :name :dot :name :dot :name)  
            (let ((n (concatenate 'string "pp-con-" $7 "_" $11)))
              (progn
                ; Generate the pp mediator for this connection
                (push `(,n (ocml::def-class ,(read-from-string n) (ocml::pp-mediator) 
                             ((ocml::has-source-component :value ,(read-from-string $2)) 
                              (ocml::has-target-component :value ,(read-from-string $7))
                              (ocml::has-mediation-service :value ,(read-from-string (concatenate 'string "goal-" n)))))) *ocml-definitions*)
                ; Generate the mediation goal [INCOMPLETE]
                (push `(,(concatenate 'string "goal-" n)
                        (ocml::def-class ,(read-from-string (concatenate 'string "goal-" n)) (ocml::goal))) *ocml-definitions*))))
           ; Gather together performance and connection names into a list for inclusion into a pattern.
           ((performance-list performance :scolon performance-list) (cons $1 $3))
           ((performance-list connection :scolon performance-list) $3)
           ((performance-list performance) `(,$1))

           ((performance :perform :name workflow) 
            (progn
              ; Generate workflow performance
              (push `(,$2 (ocml::def-class ,(read-from-string $2) (ocml::perform-workflow)
                        ((has-workflow :value ,$3)))) *ocml-definitions*)
              ; Return performance name
              (read-from-string $2)))

           ((performance :perform :name :send :name :lparen output-list :rparen)
            (progn
              ; Generate send
              (push `(,$2 (ocml::def-class ,(read-from-string $2) (ocml::send)
                            (,(append '(ocml::has-input-role) (first $6))))) *ocml-definitions*)
              ; Return performance name
              (read-from-string $2)))

           ((output-message-list output-message :comma output-message-list) (cons $1 $3))
           ((output-message-list output-message) `(,$1))
           
           ((output-message :name :lparen output-message-parts :rparen)
            (progn
              ; Define a class for each of the output messages, as goals have a single output role.
              (let ((ps (mapcar (lambda (x) `(,(read-from-string (first x)) :type ,(read-from-string (second x)))) $3)))
                (push `(,$1 (ocml::def-class ,(read-from-string $1) () ,ps)) *ocml-definitions*))
              `(,$1 ,$3)))

           ; Just gather the output parts into a list as we need to do lots of different things with them.
           ((output-message-parts output-message-part :comma output-message-parts) (cons $1 $3))
           ((output-message-parts output-message-part) `(,$1))

           ((output-message-part :name :oftype :name) `(,$1 ,$3))

           ; Generate a goal performance
           ((performance :perform :name :goal :name :lparen input-list :rparen :lparen output-message-list :rparen)  
            ; Intersperse the message names with -or- to make the disjunctio name
            (let ((orole (read-from-string (format nil "~{~A~^-or-~}" 
                                                   (mapcar (lambda (x) (first x)) $9)))))
              (progn
                ; Define the performance class
                (push `(,$2 (ocml::def-class ,(read-from-string $2)  (ocml::perform-goal)
                          ((ocml::has-goal :value ,(read-from-string $4))))) *ocml-definitions*)
                ; Define the union of outputs class if needed for the goal output role
                (if (> (length $9) 1) 
                    (push `(,(prin1-to-string orole) (ocml::def-class ,orole () ocml::?ms
                              :sufficient
                              ,(cons 'or (mapcar (lambda (x) `(,(read-from-string (first x)) ocml::?ms)) $9)))) *ocml-definitions*))

                ; Create an extractor pp-mediator for each of the output messages
                (mapcar 
                 (lambda (x) 
                   (let ((ppn (concatenate 'string "pp-ext-" $2 "_" (first x))))
                     (let ((goals (mapcon (lambda (y) `(:value ,(read-from-string (concatenate 'string "goal-" ppn "_" (first (first y)))))) (second x)))
                           (goals2 (mapcon (lambda (y) `(,(read-from-string (concatenate 'string "goal-" ppn "_" (first (first y)))))) (second x))))
                       (progn
                         (print goals2)
                         ; Generate the pp mediator
                         (push `(,ppn (ocml::def-class ,(read-from-string ppn) (ocml::pp-mediator)
                                        ((ocml::has-source-component :value ,(read-from-string $2)) 
                                         ,(cons 'ocml::has-mediation-service goals)))) *ocml-definitions*)
                         
                         ; Generate the mediation goal
                         (mapcar (lambda (g)
                                   (push `(,(prin1-to-string g) 
                                           (ocml::def-class ,g (ocml::goal))) *ocml-definitions*)) goals2)
                       )))) $9)
                 
                ; Define the actual goal, with multiple input roles and the union class as the output role
                (push `(,$4 (ocml::def-class ,(read-from-string $4) (ocml::goal)
                          ocml::?goal                        
                          (,(append '(ocml::has-input-role) (first $6))
                           ,(append '(ocml::has-input-soap-binding) (second $6))
                           (ocml::has-output-role ,orole)
                           )
                          )) *ocml-definitions*)

                ; Return the performance name
                (read-from-string $2))
              ))

           ((performance :perform :name :receive :name :lparen input-list :rparen) 
            (progn
              ; Generate receive
              (push `(,$2 (ocml::def-class ,(read-from-string $2) (ocml::receive)
                       (,(append '(ocml::has-output-role) (first $6))))) *ocml-definitions*)
              ; Return the performance name
              (read-from-string $2)))
           
           ; Process the input list into :value lists, as I think we'll only need them like this
           ((input-list input) $1)
           ((input-list input :comma input-list) `(,(append (first $1) (first $3)) ,(append (second $1) (second $3))))
           ((input :name :oftype :name) `((:value ,(read-from-string $1)) (:value (,(read-from-string $1) ,$3))))


           ((output-list output) $1)
           ((output-list output :comma output-list) `(,(append (first $1) (first $3)) ,(append (second $1) (second $3))))
           ((output :name :oftype :name) `((:value ,(read-from-string $1)) (:value (,(read-from-string $1) ,$3))))
 

           ; Conditions are assumed for the time being to be lists of atoms contained in braces.
           ; This rule reconnects the list of atoms into a single string.
           ((condition :lbrace condition-tail) $2)
           ((condition-tail :name condition-tail) (concatenate 'string $1 " " $2))
           ((condition-tail :rbrace) "")

           )


; Use the definitions found in *ocml-definitions* to generate OCML class definitions.
(defun ocml-define ()
       (mapcar 
        (lambda (x) 
          (ip::add-irs-knowledge-level-definition
           (ocml::name ocml::*current-ontology*) (first x)
           (prin1-to-string (second x)))) *ocml-definitions*))

; Keyword lexical tokens
(defparameter *keywords* 
  '(("workflow" :workflow) ("sequential" :sequential) ("concurrent" :concurrent) ("interleaved" :interleaved) ("xor" :xor) ("deferred-choice" :deferred-choice) ("while" :while) ("deferred-while" :deferred-while) ("until" :until) ("deferred-until" :deferred-until) ("connect" :connect) ("perform" :perform) ("send" :send) ("receive" :receive) ("goal" :goal) ("workflow" :workflow) ("accept" :accept) ("offer" :offer)))

; Get the lexical class of a string
(defun get-lex-class (word)
   (values
    (or (cadr (assoc-if #'(lambda (x) (string-equal word x)) *keywords*))
        (if (not (eq (type-of word) (type-of ""))) word nil)
        :name)
    word))

; The token input stream
(defvar *input*)

; Lex Cashew pops the input stream, and returns the lexical class of the token.
(defun lex-cashew () 
  (let ((symbol (pop *input*))) 
    (if symbol (get-lex-class symbol)   
      nil)))

; Invoke the Cashew Parser with the given input.
(defun parse-cashew (input)
  "Parser for the Cashew surface syntax"
  (let ((*input* (pre-lex-cashew input)))
     (cashew-parser #'lex-cashew)))

; Lexical symbols
(defparameter *keysymbols*
  '((#\( :lparen) (#\) :rparen) (#\; :scolon) (#\, :comma) (#\: :oftype) (#\" :quote) (#\{ :lbrace) (#\} :rbrace) (#\. :dot))) 

; Remove whitespace, significant symbols and convert from CamelCase.
(defun pre-lex-cashew (string)
    "Remove whitespace and lex brackets, commas, colons and semi-colons"
    (loop for i = 0 then (1+ j)
          as j = (position-if #'(lambda (x) (or (eq x #\Space) (assoc x *keysymbols*))) string :start i)
          as res = (subseq string i j)
          when (> (length res) 0)
          collect (decamel res)
          when (and j (assoc (elt string j) *keysymbols*))
          collect (cadr (assoc (elt string j) *keysymbols*))
          while j))

(defun decamel (string) 
  "Convert a string in CamelCase to a dash-seperated-string"
  (format nil "~{~A~^-~}" 
          (loop for i = 1 then (1+ j)
                as j = (position-if #'upper-case-p string :start i)
                collect (string-downcase (subseq string (- i 1) j))
                while j)))

