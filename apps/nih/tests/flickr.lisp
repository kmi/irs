;;; Copyright © 2009,2010 The Open University

(in-package #:irs.applications.nih.tests)

(defvar *photos-recently-updated*)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (ocml:register-namespace
   "nih" "http://kmi.open.ac.uk/irs/apps/nih#")
  (ocml:register-namespace
   "flickr" "http://www.kmi.open.ac.uk/projects/irs/flickr#"))

(in-suite flickr-suite)

(test flickr-auth-frob
  (finishes
    (wp:achieve-goal-slotted
     'ocml::nih-application '#_flickr:authGetFrobRestGoal
     `((#_flickr:hasAccount "#_nih:theFlickrAccount")))))

(test flickr-photos-recently-updated
  (finishes
    (setf *photos-recently-updated*
	  (wp:achieve-goal-slotted
	   'ocml::nih-application '#_flickr:photosRecentlyUpdatedRestGoal
	   `((#_flickr:hasAccount "#_nih:theFlickrAccount")
	     (#_flickr:hasToken "#_nih:theFlickrToken")
	     (#_flickr:hasMinimumDate 1))))))

(test flickr-auth-checktoken
  (finishes
    (wp:achieve-goal-slotted
     'ocml::nih-application '#_flickr:authCheckTokenRestGoal
     `((#_flickr:hasAccount "#_nih:theFlickrAccount")
       (#_flickr:hasToken "#_nih:theFlickrToken")))))

(test flickr-test-login
  (finishes
    (wp:achieve-goal-slotted
     'ocml::nih-application '#_flickr:testLoginRestGoal
     `((#_flickr:hasAccount "#_nih:theFlickrAccount")
       (#_flickr:hasToken "#_nih:theFlickrToken")))))

(test flickr-test-echo
  (finishes
    (wp:achieve-goal-slotted
     'ocml::nih-application '#_flickr:testEchoRestGoal
     `((#_flickr:hasAccount "#_nih:theFlickrAccount")))))

(test (flickr-get-sizes :depends-on flickr-photos-recently-updated)
  (is (listp (wp:achieve-goal-slotted
	       'ocml::nih-application '#_flickr:photosGetSizesRestGoal
	       `((#_flickr:hasAccount "#_nih:theFlickrAccount")
		 (#_flickr:hasToken "#_nih:theFlickrToken")
		 (#_flickr:hasPhoto ,(first *photos-recently-updated*))))))
  (is (listp (read-from-string
	      (wp:achieve-goal-slotted
	       'ocml::nih-application '#_flickr:photosGetSizesXmlrpcGoal
	       `((#_flickr:hasAccount "#_nih:theFlickrAccount")
		 (#_flickr:hasToken "#_nih:theFlickrToken")
		 (#_flickr:hasPhoto ,(first *photos-recently-updated*))))))))
