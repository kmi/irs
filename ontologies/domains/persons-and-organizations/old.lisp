;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

(in-ontology persons-and-organizations)

(def-class giving-a-demo (event)
  ((demonstrated-technology :type technology) (demonstrator))
:slot-renaming ((demonstrated-technology object-acted-on) (demonstrator main-agent)))

(def-class research-institute (non-profit-organization))

(def-class senior-lecturer (academic))

(def-class drawing-technology (multimedia-technology))

(def-class business-analyst (employee))

(def-class senior-manager (manager))

(def-class operating-system (software-technology))

(def-class software-status ())

(def-class minister (politician)
  ((portfolio) (areas-of-interest)))

(def-class url ()
  ((prefix) (host-name) (location)))

(def-class consultant (affiliated-person))

(def-class head-of-organisation (manager))

(def-class statistical-software-technology (software-technology))

(def-class presentation ()
  ((presentation-title :type string)
   (presentation-topic)
   (speaker)
   (presentation-abstract :type string)))

(def-class course ()
 ((course-name) (host-educational-organization) (host-department)))

(def-class bayesian-software-technology (statistical-software-technology))

(def-class higher-educational-course (course)
  ((host-faculty)))

(def-class open-university-course (higher-educational-course)
  ((host-educational-organization :default-value open-university)))

(def-instance course-t171 open-university-course
  ((course-name "you, your computer and the net")
   (host-department department-of-telematics)
   (host-faculty faculty-of-technology)))

(def-instance department-of-telematics academic-unit
  ((part-of faculty-of-technology)))

(def-instance howells minister
  ((portfolio lifelong-learning)
   (constituency pontypridd)
   (areas-of-interest learning-age university-for-industry)
   (biography "Dr Howells entered parliament in 1989 and is MP for Pontypridd.  In
opposition, he has spoken for Development and Cooperation, Foreign and
Commonwealth Affairs, Home Affairs and Trade and Industry. His current
government position in Lifelong Learning at the DfEE has seen the
publication of a Green Paper on the Learning Age and the planning
prospectus for the University for Industry.")))

(def-instance daniel head-of-organisation
  ((full-name "Sir John Daniel")
   (works-at open-university)
   (has-affiliation open-university)
   (biography "Sir John Daniel has been Vice-Chancellor since 1990. He holds dual UK and
Canadian nationality, having spent most of his career in Canada after being
born and educated in the UK. Secondary education at Christ's Hospital led
him to study at Oxford where he obtained a First in Metallurgy. He then
spent four years at the University of Paris completing a doctorate on the
mechanical behaviour of uranium crystals. During this period he spent two
summers working in industrial laboratories in the USA, where he met his
wife.

His first academic appointment was at the Ecole Polytechnique of the
Universite de Montreal in 1969. From there successive moves took him to the
Tele-université of the Université du Quebec 1973-77 (Director of Studies);
Athabasca University 1978-80 (Vice-President); Concordia University 1980-84
(Vice Rector); and Laurentian University 1984-90 (President). He returned to
Britain in 1990 after spending his last year in Canada at the National
Defence College.

In 1971 he embarked on part-time study for an MA in Educational Technology
at what is now Concordia University, Montreal. The internship required by
this programme, which he spent at the Open University in 1972, inspired him
to re-orient his career towards distance education. He finally completed the
MA programme in 1990 after preparing a thesis on the implications of new
technologies for world's ten largest open universities. In the intervening
period he had authored nearly 150 publications on a variety of scientific,
educational and management subjects.

Sir John was president of the International Council for Distance Education
from 1982-85 and Chairman of the Planning Committee for the Commonwealth of
Learning in 1987-88. He holds honorary degrees from universities on four
continents and was knighted for services to higher education in 1994.
")
   (email-address "j.s.daniel@open.ac.uk")
   (web-address "http://www.open.ac.uk/OU/Admin/VCO/vc.htm")))

(def-instance TecInno corporation
  ((location germany)
  (affiliated-people wess traphoner)
  (alternate-names )
  (part-of )))

(def-instance technical-university-of-kosice higher-educational-organization
  ((support-units )
  (academic-units )
  (location slovakia)
  (affiliated-people hatala)
  (alternate-names )
  (part-of )))

(def-instance german-national-ai-institute research-institute
  ((part-of )
  (alternate-names )
  (affiliated-people hinkelman bernardi)
  (location )))

(def-instance university-of-pavia higher-educational-organization
  ((support-units )
  (academic-units )
  (location Pavia)
  (affiliated-people riva)
  (alternate-names )
  (part-of )))

(def-instance alpha-status software-status)

(def-instance beta-status software-status)

(def-instance finished-status software-status)

(def-instance microsoft corporation
  ((location seattle)))

(def-instance Boulder-colorado location)

(def-instance University-of-Colorado higher-educational-organization
  ((web-address university-of-colorado-address)
   (affiliated-people lewis)
   (location Boulder-colorado)))

(def-instance lewis professor
  ((works-at University-of-Colorado)))
