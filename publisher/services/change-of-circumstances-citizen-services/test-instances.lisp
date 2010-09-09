
(in-package ocml)


;;;test instances for change of circumstances 

;;;citizen-part

(def-anonymous-instance address
  ((has-street "Marina Drive")
   (has-postal-code "MK12 5DT")
   (has-number-house 33)
   (has-municipal-unit   milton-keynes)
   (has-county buckinghamshire )
   (has-country  england)))

(def-anonymous-instance address
  ((has-street "Eaton Crescent")
   (has-postal-code "NE3 5DF")
   (has-number-house 34)
   (has-municipal-unit  london)
   (has-country  england)))

(def-anonymous-instance citizen
  ((full-name "Bill Bloggs")
   (family-name "Bloggs")
   (given-name "Bill")
   (has-gender  male-gender)
   (date-of-birth  date-of-birth1)
   (has-address  address1)
   (has-impairment  hearing-impairment)
   (has-ethnicity white-british)))

(def-anonymous-instance citizen
  ((full-name "Claire Houston")
   (family-name "Houston")
   (given-name "Claire")
   (has-gender  female-gender)
   (date-of-birth  date-of-birth2)
   (has-address  address2)
   (has-impairment  speech-impairment)
   (has-ethnicity other-white)))

(def-anonymous-instance citizen
  ((full-name "Samantha Phillips")
   (Family-Name "Samantha")
   (given-name "Phillips")
   (has-gender  female-gender)
   (date-of-birth  date-of-birth2)
   (has-address  address2)
   (has-impairment  visual-impairment)
   (has-ethnicity black-british)))

(def-anonymous-instance calendar-date
  ((day-of 12)
   (month-of 4)
   (year-of 2005)))

;;;test two instances together. put both instances below as a single input role value
(instances
(def-instance simons-address address
  ((has-postal-code "MK12 3ED")
   (has-street "Victoria Street")
   (has-number-house 21)
   (has-municipal-unit  milton-keynes)
   (has-country england)))

(def-anonymous-instance citizen
  ((full-name "Simon White")
   (family-name "White")
   (given-name "Simon")
   (has-gender  female-gender)
   (date-of-birth  date-of-birth2)
   (has-address  simons-address)
   (has-impairment  visual-impairment)
   (has-ethnicity black-british))))