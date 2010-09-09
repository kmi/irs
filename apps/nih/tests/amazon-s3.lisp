;;; Copyright © 2009 The Open University

(in-package :irs.applications.nih.tests)

(def-suite amazon-s3-suite
  :description "Tests for the Amazon S3 services."
  :in nih-suite)

(in-suite amazon-s3-suite)

(test amazons3-file-upload-test
  (finishes
    (with-output-to-string (str)
      (ip::raw-irs-achieve-goal
       'ocml::nih-application 'ocml::put-object-goal
       `((ocml::has-account "#_nih:theS3Account")
         (ocml::has-bucket "lhdl")
         (ocml::has-key "test")
         (ocml::has-data ,nih-tests::*random-string*))
       str nil t))))

(test (amazons3-file-download-test
       :depends-on (and amazons3-file-upload-test))
  (is (string=
       (with-output-to-string (str)
         (ip::raw-irs-achieve-goal
          'ocml::nih-application 'ocml::get-object-goal
          '((ocml::has-account "#_nih:theS3Account")
            (ocml::has-bucket "lhdl")
            (ocml::has-key "test"))
          str nil t))
       nih-tests::*random-string*)))

(test amazons3-bucket-test
  (finishes
   (with-output-to-string (str)
     (ip::raw-irs-achieve-goal
      'ocml::nih-application 'ocml::get-bucket-goal
      '((ocml::has-account "#_nih:theS3Account")
        (ocml::has-bucket "lhdl"))
      str nil t))))
