;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")

(in-ontology aktive-portal-ontology2)

;;;This file deals with publications. I have reused some stuff from a 'documents'
;;;ontology which can be found on the ontolingua server




(def-class INFORMATION-BEARING-OBJECT (tangible-thing)
  "This notion comes from Cyc.  It is useful to group together all
   information bearing entities, including video, audio and documents.
   An information bearing object may have an author (a generic agent)
   and may be owned by a legal agent. It is a tangible object"
  ((has-author :type generic-agent)
   (owned-by :type legal-agent)))


(def-class ABSTRACT-INFORMATION (intangible-thing)
  "Information in general, independent of an object in which it is encoded. 
   Whatever is transferred in an information-transfer event.
   It is clearly an intangible thing")


(def-class RECORDED-AUDIO (Information-Bearing-Object)
  "Any kind of recorded audio, which is tangible.  This also includes a  
   audio file on a machine")

(def-class RECORDED-VIDEO (Information-Bearing-Object)
  "Any kind of recorded video, which is tangible.  This also includes (e.g.,) a mpeg 
   video file on a machine")





(def-class PUBLICATION (Information-Bearing-Object)
  "A publication is something which has one or more publication references.
   A publication can be both an article in a journal or a journal itself.
   The distinction between publication and publication-reference makes it possible
   to distinguish between multiple occurrences of the sam publication, for instance in
   different media"
  ((has-publication-reference :min-cardinality 1
                              :type publication-reference)
   (cites-publication-reference :type publication-reference)))


(def-class COMPOSITE-PUBLICATION (publication)
  "A publication which contains items which cane be themselves referenced through a 
   publication reference.  Composite publications include newspapers, magazines and journals.
   A book which is a collection of articles is a composite publication, a monograph is not"
  ((contains-publication :min-cardinality 1
                         :type publication)
   ))


(def-class SERIAL-PUBLICATION (Publication) ?x
  "This used to be called periodical publication.  However, many periodicals
do not appear at fixed intervals, which is why librarians refer to them as serials.
So, we now use the concept of serial publication and the has-periodicity slot has been 
removed"
  )


(def-class PERIODICAL-PUBLICATION (serial-publication) ?x 
  "This comes from the ontolingua library.
   A periodical-publication is published regularly, such as once
   every week.  Strictly speaking, the noun 'periodical' is used
   by librarians to refer to things published at intervals of greater
   than a day.  We use the phase periodical-publication to include
   newspapers and other daily publications, since they share many
   bibliographic features.
   The periodicity indicates how often the publication comes out. Note that this is
   a duration, rather than a time interval. A time interval indicates a specific time interval
   on the time continuum, so we need to model periodicity as a time quantity"
  ((has-periodicity :cardinality 1 :type duration)))



(def-class ARTICLE-IN-A-COMPOSITE-PUBLICATION (publication)
  ((included-in-publication :type composite-publication)))

(def-axiom CONSISTENCY-BETWEEN-COMPOSITE-PUBLICATIONS-AND-THEIR-CONTENTS
  (<=> (included-in-publication ?a ?p)
       (contains-publication ?p ?a)))


(def-class JOURNAL (serial-publication composite-publication)
   ((contains-article :type publication))
   :slot-renaming ((contains-article contains-publication)
                  ))

(def-class MAGAZINE (serial-publication composite-publication)
  ((contains-article :type publication))
  :slot-renaming ((contains-article contains-publication)
                  ))

(def-class NEWSPAPER (periodical-publication composite-publication)
  ((contains-news-item :type news-item))
  :slot-renaming ((contains-news-item contains-publication)
                  ))

(def-class DAILY-NEWSPAPER (newspaper)
  ((has-periodicity :value 24-hour-duration)))

(def-class NEWS-ITEM (article-in-a-composite-publication))

(def-axiom MUTUALLY-EXCLUSIVE-SERIAL-PUBLICATIONS
  (subclass-partition  Serial-Publication 
                                  (Setof Journal Magazine Newspaper)))

(def-class BOOK (publication)
  ((has-publication-reference :min-cardinality 1
                              :type book-reference)))

(def-class EDITED-BOOK (publication)
  ((has-publication-reference :min-cardinality 1
                              :type edited-book-reference)))

 
(def-class PUBLICATION-REFERENCE (abstract-information)
  "we have decided that a publication reference is an intangible, abstract information"
  ((has-title :type string)
   (has-author :type generic-agent)
   (has-date :type calendar-date)
   (has-place-of-publication :type location)))


(def-class WEB-REFERENCE (publication-reference)
  ((has-URL :type URL)))

(def-class URL (string) ?x
  "A URL is a particular type of string"
  :sufficient-for-type-checking (string ?x))
  

(def-class BOOK-REFERENCE (Publication-Reference)
  ((published-by :type publishing-house)
   (has-ISBN-number :type ISBN-Number)))

(def-class EDITED-BOOK-REFERENCE (Book-Reference)
  ((edited-by :type person)
   )
  :slot-renaming ((edited-by has-author)))

(def-class CONFERENCE-PROCEEDINGS-REFERENCE (Publication-Reference)
  ((edited-by :type person)
   (has-page-numbers :type string)
   (published-in-conference :type conference))
   :slot-renaming ((edited-by has-author)))

(def-class WORKSHOP-PROCEEDINGS-REFERENCE (Publication-Reference)
  ((edited-by :type person)
   (has-page-numbers :type string)
   (published-in-workshop :type workshop))
   :slot-renaming ((edited-by has-author)))


(def-class ISBN-NUMBER (string))
   

(def-class BOOK-SECTION-REFERENCE (Publication-Reference)
  ((has-page-numbers :type string)
   (section-of-book :type book-reference)))


(def-class  ARTICLE-REFERENCE (Publication-Reference)
  ((has-page-numbers :type string)
   (article-of-journal :type journal)
   (issue-number :type integer)
   (issue-volume :type integer)))


(def-class PROCEEDINGS-PAPER-REFERENCE (Publication-Reference)
  ((has-page-numbers :type string)
   (paper-in-proceedings :type conference-Proceedings-Reference)))


(def-class THESIS-REFERENCE (Publication-Reference)
  ((degree-of-thesis :type academic-degree)
   (institute-of-thesis :type academic-unit)))

(def-class TECHNICAL-REPORT-REFERENCE (Publication-Reference)
  ((published-by :type organization)
   (has-tech-report-number :type integer)))