;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: OCML;   -*-

(in-package "OCML")

(in-ontology news-ontology)



(def-class news ()
  "News is a collection of news-item"
  ((has-news-items :type news-item
                   :min-cardinality 1)))

(def-class news-item (temporal-thing)
  ((has-author :type person)
   (has-headline :type string)
   (has-contents :type string)
   (expressed-in-medium :type publishing-medium)
   (published-date :type calendar-date)
   (relates-events :min-cardinality 1 :type event))
  :slot-renaming ((published-date start-time)))


(def-relation has-contents (?x ?c)
  :constraint (and (news-item ?X)
                   (string ?c)))
  :lisp-fun #'(Lambda (x y env)
                 (let ((conts (unbound-variable? y env)))
                   (if conts
                       (list (cons (cons conts (get-contents (the-slot-value x 'headline)))env))
                       (let ((new-env (unify y (get-contents (the-slot-value x 'headline))env)))
                         (if (eq new-env :fail)
                             :fail
                             (list new-env)))))))



 #+:lispworks
(defun get-contents (headline)
  (dolist (item (cl-user::news-items))
    (when (string= headline (cl-user::news-item-headline item))
     (return (cl-user::news-item-contents item)))))


