;;; Copyright © 2007, 2008 The Open University

;;;; Unit tests

(in-package #:irs.tests)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (ocml:register-namespace
   "lhdlA" "http://kmi.open.ac.uk/projects/lhdl/ns/application#")
  (ocml:register-namespace
   "lhdlD" "http://kmi.open.ac.uk/projects/lhdl/ns/domain#")
  (ocml:register-namespace
   "lhdlG" "http://kmi.open.ac.uk/projects/lhdl/ns/goals#"))

(def-suite lhdl-suite
  :description "All tests for the LHDL project.")

;;; Most of our tests depend on the LHDL execution services provided
;;; by the University of Bedfordshire, but they are unreliable and
;;; even when they are up, the complete test suite takes around five
;;; mintues to run.  So, we split lhdl-suite into two subsuites, those
;;; with and those without dependencies on execution services.

(def-suite lhdl-sans-execution-suite
    :description "Tests for LHDL, except the Bedfordshire execution
    services."
    :in lhdl-suite)

(def-suite lhdl-with-execution-suite
    :description "Tests for LHDL that use the Bedfordshire execution
    services."
    :in lhdl-suite)

(def-suite lhdl-amazon-suite
  :description "Tests for the LHDL Amazon S3 services."
  :in lhdl-sans-execution-suite)

(def-suite lhdl-community-suite
  :description "Tests for the LHDL community services."
  :in lhdl-sans-execution-suite)

(def-suite lhdl-execution-suite
  :description "Tests for the LHDL execution services."
  :in lhdl-with-execution-suite)

(def-suite lhdl-execution-import-suite
  :description "LHDL import services."
  :in lhdl-execution-suite)

(def-suite lhdl-execution-export-suite
  :description "LHDL export services."
  :in lhdl-execution-suite)

(def-suite lhdl-execution-other-suite
  :description "LHDL other execution services."
  :in lhdl-execution-suite)

(def-suite lhdl-review-suite
  :description "Tests for the LHDL 2008 review."
  :in lhdl-with-execution-suite)

;;; {{{ preliminaries

(defparameter *vme-in-base64* "KHJlcXVpcmUgOnMteG1sLXJwYykKKHJlcXVpcmUgOnMtYmFzZTY0KQoKKHVzZS1wYWNrYWdlIDpzLWJhc2U2NCkKKHVzZS1wYWNrYWdlIDpzLXhtbC1ycGMpCgo7OyBzIG5vdyBhdmFpbGFibGUgaW4gcHJvZHVjdGlvbiBMaHBSZXBvc2l0b3J5IGVudmlyb25tZW50IGJvdGgKOzsgd2Vic2VydmljZXMgZm9yIFVwbG9hZCBhbmQgRG93bmxvYWQgWk1TRiBmaWxlcy4KCjs7IFVSTDogCgo7OyAxLiBVcGxvYWQgKFVwbG9hZCBhIHptc2YgZmlsZSB0byBhdXRoZW50aWNhdGVkIHVzZXIgc2FuZGJveCk6Cjs7IGlucHV0czoKOzsgIC0gbWV0aG9kIG5hbWU6IFhNTFVwbG9hZEZpbGUKOzsgIC0gcGFyYW1ldGVyczogYmFzZTY0LWVuY29kZWQgem1zZiBmaWxlLCBmaWxlbmFtZSwgY29tbWVudCAob3B0aW9uYWwpCjs7IG91dHB1dHM6Cjs7ICAtIG9wZXJhdGlvbiByZXN1bHQgKG5pbCBpZiBzdWNjZXNzZnVsLCAwIGlmIGZpbGUgcmVqZWN0LCBlcnJvciBtZXNzYWdlIGlmCjs7ICAgYW5vdGhlciBlcnJvcikKCjs7IDIuIERvd25sb2FkIChEb3dubG9hZCBhIHptc2YgZmlsZSBhbHJlYWR5IHN0b3JlZCBvbiB1c2VyIHNhbmRib3ggcmVwb3NpdG9yeSk6Cjs7IGlucHV0czoKOzsgIC0gbWV0aG9kIG5hbWU6IFhNTERvd25sb2FkRmlsZQo7OyAgLSBwYXJhbWV0ZXJzOiByZW1vdGUgZmlsZW5hbWUKOzsgb3V0cHV0czoKOzsgIC0gZmlsZW5hbWUgYW5kIGJhc2U2NC1lbmNvZGVkIHptc2YgZmlsZSAoY2xpZW50IGhhcyB0byBkZWNvZGUgaXQgYW5kIHNhdmUgaXQgdG8KOzsgICBkaXNrKSBpZiBzdWNjZXNzZnVsCjs7ICAtIG9yIGVycm9yCgo7OyBUaGFuayB5b3UuCgooZGVmdW4gY3JlYXRlLWF1dGhvcml6YXRpb24tc3RyaW5nICh1c2VybmFtZSBwYXNzd29yZCkKICAiQ3JlYXRlIHRoZSBYTUwtUlBDIGF1dGhvcml6YXRpb24gaGVhZGVyIHZhbHVlIGZyb20gVVNFUk5BTUUgYW5kClBBU1NXT1JELiIKICAobGV0ICgoYnl0ZXMgKHN0cmluZy10by1vY3RldHMgKGZvcm1hdCBuaWwgIn5BOn5BIiB1c2VybmFtZSBwYXNzd29yZCkpKSkKICAgIChmb3JtYXQgbmlsICJCYXNpYyB+QSIgKHdpdGgtb3V0cHV0LXRvLXN0cmluZyAob3V0KQoJCQkgICAgIChlbmNvZGUtYmFzZTY0LWJ5dGVzIGJ5dGVzIG91dCkpKSkpCgooZGVmdmFyICp1cmwqCiAgImh0dHA6Ly93d3cuYmlvbWVkdG93bi5vcmcvYmlvbWVkX3Rvd24vTEhETC91c2Vycy9yZXBvc2l0b3J5L2xocHJlcG9zaXRvcnkiKQoKKGRlZnZhciAqeG1scnBjLWxvY2F0aW9uKiAiL2Jpb21lZF90b3duL0xIREwvdXNlcnMvcmVwb3NpdG9yeS9saHByZXBvc2l0b3J5IikKCihkZWZ2YXIgKmhvc3QqICJ3d3cuYmlvbWVkdG93bi5vcmciKQoKKGRlZnZhciAqZnVkZ2VkLWV4dHJhLXN0dWZmKgogIChsaXN0IDpwcm94eS1ob3N0ICIxOTQuNjYuMTQ2LjIyIiA6cHJveHktcG9ydCA4MCA6YXV0aG9yaXphdGlvbiAKICAgICAgICAoY3JlYXRlLWF1dGhvcml6YXRpb24tc3RyaW5nICJkbGFtYmVydCIgInBva2V5aGF0IikpKQoKOzsgICAgICAgICAoY3JlYXRlLWF1dGhvcml6YXRpb24tc3RyaW5nICJ0ZXN0dXNlciIgIjNKTTlIZiIpCgooZGVmdW4gdXBsb2FkIChiYXNlNjQgZmlsZW5hbWUgJm9wdGlvbmFsIChjb21tZW50ICIiKQoJICAgICAgIChwb3J0IDgwKSAoaG9zdCAqaG9zdCopKQogIChhcHBseSAjJ3htbC1ycGMtY2FsbAoJIChlbmNvZGUteG1sLXJwYy1jYWxsICJYTUxVcGxvYWRGaWxlIgoJCQkgICAgICBiYXNlNjQgZmlsZW5hbWUgY29tbWVudCkKCSA6cG9ydCBwb3J0IDpob3N0IGhvc3QgOnVybCAqeG1scnBjLWxvY2F0aW9uKgoJICpmdWRnZWQtZXh0cmEtc3R1ZmYqKSkKCihkZWZ1biBkb3dubG9hZCAoZmlsZW5hbWUgJm9wdGlvbmFsIChwb3J0IDgwKSAoaG9zdCAqaG9zdCopKQogIChhcHBseSAjJ3htbC1ycGMtY2FsbAoJIChlbmNvZGUteG1sLXJwYy1jYWxsICJYTUxEb3dubG9hZEZpbGUiIGZpbGVuYW1lKQoJIDpwb3J0IHBvcnQgOmhvc3QgaG9zdCA6dXJsICp4bWxycGMtbG9jYXRpb24qCgkgKmZ1ZGdlZC1leHRyYS1zdHVmZiopKQoKCgooZGVmdW4gc2l4dHktZm91ciAoc3RyaW5nKQogICJDcmVhdGUgdGhlIFhNTC1SUEMgYXV0aG9yaXphdGlvbiBoZWFkZXIgdmFsdWUgZnJvbSBVU0VSTkFNRSBhbmQKUEFTU1dPUkQuIgogICh3aXRoLW91dHB1dC10by1zdHJpbmcgKG91dCkKICAgIChlbmNvZGUtYmFzZTY0LWJ5dGVzIChzdHJpbmctdG8tb2N0ZXRzIHN0cmluZykgIG91dCkpKQo7OyAodXBsb2FkICAoc2l4dHktZm91ciAiaGVsbG8gd29ybGQiKSAibXktcGVsdmlzLWhvbGlkYXktcGljdHVyZXMuenNtZiIgImhpIG1vbSEiKQoKCg==")

;;; }}}

;;; {{{ lhdl-review-suite
(in-suite lhdl-review-suite)

(test move-file-test
  (finishes
    (with-output-to-string (str)
      (ip::raw-irs-achieve-goal 'ocml::lhdl-application
				'ocml::upload-file-goal
				`((ocml::has-filename "irs-test.zmsf")
                                  (ocml::has-id "irs-test")
                                  (ocml::has-comment "a test file")
                                  (ocml::has-content ,tests::*vme-in-base64*)
                                  (#_lhdlG:hasAccount "#_defaultBiomedTownAccount"))
                                str nil t)))
  (finishes
    (with-output-to-string (str)
      (ip::raw-irs-achieve-goal 'ocml::lhdl-application
				'ocml::move-file-bt-to-s3-goal
				'((ocml::has-filename "irs-test.zmsf")
                                  (ocml::has-amazon-account "lhdl-amazons3-account")
                                  (#_lhdlG:hasBiomedTownAccount "#_defaultBiomedTownAccount")
                                  (ocml::has-amazon-bucket "lhdl")
                                  (ocml::has-amazon-key "irs-test")) str nil t)))
  (finishes
    (with-output-to-string (str)
      (ip::raw-irs-achieve-goal 'ocml::lhdl-application
				'ocml::move-file-s3-to-bt-goal
				'((ocml::has-amazon-account "lhdl-amazons3-account")
                                  (#_lhdlG:hasBiomedTownAccount "#_defaultBiomedTownAccount")
                                  (ocml::has-amazon-bucket "lhdl")
                                  (ocml::has-amazon-key "irs-test")
                                  (ocml::has-filename "irs-test-2.zmsf")) str nil t)))

  (finishes
    (with-output-to-string (str)
      (ip::raw-irs-achieve-goal
       'ocml::lhdl-application
       'ocml::delete-file-goal
       `((ocml::has-filename "irs-test-2.zmsf")
         (#_lhdlG:hasAccount "#_defaultBiomedTownAccount"))
       str nil t)))
  (finishes
    (with-output-to-string (str)
      (ip::raw-irs-achieve-goal
       'ocml::lhdl-application
       'ocml::delete-file-goal
       `((ocml::has-filename "irs-test.zmsf")
         (#_lhdlG:hasAccount "#_defaultBiomedTownAccount"))
       str nil t))))

(test import-decimate-render-test
  (finishes
   (with-output-to-string (str)
     (ip::raw-irs-achieve-goal
               'ocml::lhdl-application 'ocml::import-decimate-render-goal
               '((#_lhdlG:hasAccount "#_defaultBiomedTownAccount")
                 (ocml::has-filename "sample/1091_Femur_Right.stl")
                 (ocml::has-preserve-topology "true")
                 (ocml::has-target-reduction "1")
                 (ocml::has-background-colour "100/100/100")
                 (ocml::has-zoom-factor "2.0")
                 (ocml::has-camera-azimuth "90")
                 (ocml::has-camera-roll "225")
                 (ocml::has-image-width "300")
                 (ocml::has-image-height "300"))
               str	; this is the html stream
               nil t))))
;;; }}}
;;; {{{ lhdl-community-suite
(in-suite lhdl-community-suite)

(test test-listsandbox
  (finishes
    (with-output-to-string (str)
      (ip::raw-irs-achieve-goal
       'ocml::lhdl-application '#_lhdlG:listSandboxGoal
       `((#_lhdlG:hasAccount "#_defaultBiomedTownAccount"))
       str nil t))))

(test test-simple-metadata-search
  (finishes
    (with-output-to-string (str)
      (ip::raw-irs-achieve-goal
       'ocml::lhdl-application '#_lhdlG:simpleSearchGoal
       `((#_lhdlG:hasAccount "#_defaultBiomedTownAccount")
         (#_lhdlG:hasQuery "female AND surface"))
       str nil t))))

(test upload-file-test
  (finishes
   (with-output-to-string (str)
      (ip::raw-irs-achieve-goal 'ocml::lhdl-application
				'ocml::upload-file-goal
				`((ocml::has-filename "irs-test.zmsf")
                                  (ocml::has-id "irs-test")
                                  (ocml::has-comment "a test file")
                                  (ocml::has-content ,tests::*vme-in-base64*)
                                  (#_lhdlG:hasAccount "#_defaultBiomedTownAccount"))
                                str nil t))))

(test (download-file-test :depends-on (and upload-file-test))
  ;; Can't download if we haven't uploaded first.
  (is (string=
       (ocml:with-ontology ('ocml::lhdl-application)
         (let* ((instance-string
                 (with-output-to-string (str)
                   (ip::raw-irs-achieve-goal
                    'ocml::lhdl-application
                    'ocml::download-file-goal
                    `((ocml::has-filename "irs-test.zmsf")
                      (#_lhdlG:hasAccount "#_defaultBiomedTownAccount"))
                    str nil t)))
                (instance (first (ocml::find-all-current-instances-named-x
                                  (intern instance-string :ocml))))
                (has-content (first (ocml::get-slot-values
                                     instance 'ocml::has-content))))
           has-content))
       tests::*vme-in-base64*)))

(test (delete-file-test :depends-on (and download-file-test))
  ;; And we can't be deleting the file before we've checked we can
  ;; download it, can we now?
  (finishes (with-output-to-string (str)
              (ip::raw-irs-achieve-goal
              'ocml::lhdl-application
              'ocml::delete-file-goal
              `((ocml::has-filename "irs-test.zmsf")
                (#_lhdlG:hasAccount "#_defaultBiomedTownAccount"))
              str nil t))))

;;; }}}
;;; {{{ lhdl-execution-suite
(defvar *vtk-as-vme*)
(defvar *stl-as-vme*)
(defvar *surface-vme*)

(in-suite lhdl-execution-import-suite)

(test import-vtk-test
  (finishes
    (setf *vtk-as-vme*
          (with-output-to-string (str)
            (ip::raw-irs-achieve-goal
             'ocml::lhdl-application
             'ocml::import-vtk-goal
             `((ocml::has-filename "sample/head.vtk")
               (#_lhdlG:hasAccount "#_defaultBiomedTownAccount"))
             str nil t)))))

(test import-stl-test
  (finishes
    (setf *stl-as-vme*
          (with-output-to-string (str)
            (ip::raw-irs-achieve-goal
             'ocml::lhdl-application
             'ocml::import-stl-goal
             `((ocml::has-filename "sample/1091_Femur_Right.stl")
               (#_lhdlG:hasAccount "#_defaultBiomedTownAccount")) str nil t))))
  ;; This one tries to convert a non-existant file, so it should fail.
  (signals irs.grounding::<grounding-fault>
    (with-output-to-string (str)
      (ip::raw-irs-achieve-goal
       'ocml::lhdl-application
       'ocml::import-stl-goal
       `((ocml::has-filename "sample/091_Femur_Right.stl")
         (#_lhdlG:hasAccount "#_defaultBiomedTownAccount"))
       str nil t))))

(test import-dicom-test
  (finishes
   (with-output-to-string (str)
     (ip::raw-irs-achieve-goal
      'ocml::lhdl-application 'ocml::import-dicom-goal
      `((ocml::has-dictionary "sample/dicom3.dic")
	(ocml::has-dicom-zipfile "sample/DICOM-CT.zip")
	(ocml::has-dicom-type "0")
	(ocml::has-build-step "4")
        (#_lhdlG:hasAccount "#_defaultBiomedTownAccount"))
      str nil t))))

(test import-image-sequence-test
  (finishes
    (with-output-to-string (str)
      (ip::raw-irs-achieve-goal
       'ocml::lhdl-application 'ocml::import-image-sequence-goal
       `((ocml::has-filenames ("sample/img1.jpg" "sample/img2.jpg"
                               "sample/img3.jpg"))
         (#_lhdlG:hasAccount "#_defaultBiomedTownAccount"))
       str nil t))))

(test import-raw-volume-test
  (finishes
    (with-output-to-string (str)
      (ip::raw-irs-achieve-goal
       'ocml::lhdl-application 'ocml::import-raw-volume-goal
       `((#_lhdlG:hasAccount "#_defaultBiomedTownAccount")
         (ocml::has-filename "sample/deb_1_15x12x2.raw")
         (ocml::has-scalar-type "1")
         (ocml::has-endianess "big")
         (ocml::has-scalar-signed "true")
         (ocml::has-data-dimension "(15 12 2)")
         (ocml::has-slice-voi "(0 1)")
         (ocml::has-data-spacing "(1 1 1)")
         (ocml::has-header-skip-size "0")
         (ocml::has-z-coord-file "sample/Z_coordinates_deb_1.txt")
         (ocml::has-current-slice "0"))
       str nil t))))

(test import-landmark-cloud-test
  (finishes
    (with-output-to-string (str)
      (ip::raw-irs-achieve-goal
       'ocml::lhdl-application 'ocml::import-landmark-cloud-goal
       `((ocml::has-filename "sample/landmarkCloud.txt")
         (ocml::has-tagged-file "true")
         (#_lhdlG:hasAccount "#_defaultBiomedTownAccount"))
       str nil t))))

(test import-vrml-test
  (finishes (with-output-to-string (str)
              (ip::raw-irs-achieve-goal
               'ocml::lhdl-application
               'ocml::import-vrml-goal
               `((ocml::has-filename "sample/fran.wrl")
                 (#_lhdlG:hasAccount "#_defaultBiomedTownAccount"))
               str nil t))))


(in-suite lhdl-execution-export-suite)

(test (export-vtk-test :depends-on import-vtk-test)
  (finishes
    (with-output-to-string (str)
      (ip::raw-irs-achieve-goal
       'ocml::lhdl-application 'ocml::export-vtk-goal
       `((ocml::has-url ,*vtk-as-vme*)
         (ocml::has-xml ,(format nil "~A.xml" *vtk-as-vme*))
         (ocml::has-abs-matrix "true")
         (ocml::has-vtk-file-format "ascii")
         (#_lhdlG:hasAccount "#_defaultBiomedTownAccount"))
       str nil t))))

(test (export-stl-test :depends-on import-stl-test)
  (is (string=
       (with-output-to-string (str)
         (ip::raw-irs-achieve-goal
          'ocml::lhdl-application 'ocml::export-stl-goal
          `((ocml::has-url ,*stl-as-vme*)
            (ocml::has-xml ,(format nil "~A.xml" *stl-as-vme*))
            (ocml::has-abs-matrix "true")
            (ocml::has-stl-file-format "ascii")
            (#_lhdlG:hasAccount "#_defaultBiomedTownAccount"))
          str nil t))
       "http://lhdl.cis.beds.ac.uk/export/1091_Femur_Right.stl")))

(test (export-bmp-test :depends-on extract-iso-surface-test)
  (is (string=
       (with-output-to-string (str)
         (ip::raw-irs-achieve-goal
          'ocml::lhdl-application 'ocml::export-bmp-goal
          `((ocml::has-url ,*surface-vme*)
            (ocml::has-xml ,(format nil "~A.xml" *surface-vme*))
            (#_lhdlG:hasAccount "#_defaultBiomedTownAccount"))
          str nil t))
       "http://lhdl.cis.beds.ac.uk/export/head00.zip")))

(test (export-vrml-test :depends-on import-stl-test)
  (is (string=
       (with-output-to-string (str)
         (ip::raw-irs-achieve-goal
          'ocml::lhdl-application 'ocml::export-vrml-goal
          `((ocml::has-url ,*stl-as-vme*)
            (ocml::has-xml ,(format nil "~A.xml" *stl-as-vme*))
            (#_lhdlG:hasAccount "#_defaultBiomedTownAccount"))
          str nil t))
       "http://lhdl.cis.beds.ac.uk/export/1091_Femur_Right.wrl")))

(in-suite lhdl-execution-other-suite)

(test (measure-volume-test :depends-on import-stl-test)
  (is (string=
       (with-output-to-string (str)
         (ip::raw-irs-achieve-goal
          'ocml::lhdl-application 'ocml::measure-volume-goal
          `((ocml::has-url ,*stl-as-vme*)
            (ocml::has-xml ,(format nil "~A.xml" *stl-as-vme*))
            (#_lhdlG:hasAccount "#_defaultBiomedTownAccount"))
          str nil t))
       "<<volume-measurement> nsi=626203.25 area=70173.77 volume=1.4080138>")))

(test create-ref-sys-test
  (is (stringp
       (with-output-to-string (str)
         (ip::raw-irs-achieve-goal
          'ocml::lhdl-application 'ocml::create-ref-sys-goal
          '((ocml::has-name "foobar")
            (ocml::has-origin-landmark "(0 0 0)")
            (ocml::has-point-1-landmark "(10 0 10)")
            (ocml::has-point-2-landmark "(5 0 15)")
            (ocml::has-scale "1")
            (#_lhdlG:hasAccount "#_defaultBiomedTownAccount"))
          str nil t)))))

(test create-parametric-cone-surface-test
  (is (stringp
       (with-output-to-string (str)
         (ip::raw-irs-achieve-goal
          'ocml::lhdl-application 'ocml::create-cone-surface-parametric-goal
          '((ocml::has-height "1.0")
            (ocml::has-radius "3.0")
            (ocml::has-resolution "30")
            (ocml::has-cap "true")
            (#_lhdlG:hasAccount "#_defaultBiomedTownAccount"))
          str nil t)))))

(test create-parametric-cube-surface-test
  (is (stringp
       (with-output-to-string (str)
         (ip::raw-irs-achieve-goal
          'ocml::lhdl-application 'ocml::create-cube-surface-parametric-goal
          '((ocml::has-x-length "1.0")
            (ocml::has-y-length "3.0")
            (ocml::has-z-length "2.0")
            (#_lhdlG:hasAccount "#_defaultBiomedTownAccount"))
          str nil t)))))

(test create-cylinder-surface-parametric-test
  (is (stringp
       (with-output-to-string (str)
         (ip::raw-irs-achieve-goal
          'ocml::lhdl-application 'ocml::create-cylinder-surface-parametric-goal
          '((ocml::has-radius "1.0")
            (ocml::has-resolution "3.0")
            (ocml::has-height "2.0")
            (#_lhdlG:hasAccount "#_defaultBiomedTownAccount"))
          str nil t)))))

(test create-plane-surface-parametric-test
  (is (stringp
       (with-output-to-string (str)
         (ip::raw-irs-achieve-goal
          'ocml::lhdl-application 'ocml::create-plane-surface-parametric-goal
          '((ocml::has-x-resolution "2")
            (ocml::has-y-resolution "2")
            (ocml::has-origin "(0 0 0)")
            (ocml::has-point-1 "(2 0 0)")
            (ocml::has-point-2 "(0 3 0)")
            (#_lhdlG:hasAccount "#_defaultBiomedTownAccount"))
          str nil t)))))

(test create-sphere-surface-test
  (is (stringp
       (with-output-to-string (str)
         (ip::raw-irs-achieve-goal
          'ocml::lhdl-application 'ocml::create-sphere-surface-parametric-goal
          '((ocml::has-radius "2")
            (ocml::has-theta-resolution "2")
            (ocml::has-phi-resolution "4")
            (#_lhdlG:hasAccount "#_defaultBiomedTownAccount"))
          str nil t)))))

(test (clean-filter-surface-test :depends-on extract-iso-surface-test)
  (is (stringp
       (with-output-to-string (str)
         (ip::raw-irs-achieve-goal
          'ocml::lhdl-application 'ocml::clean-filter-surface-goal
          `((ocml::has-binary ,*surface-vme*)
            (ocml::has-xml ,(format nil "~A.xml" *surface-vme*))
            (#_lhdlG:hasAccount "#_defaultBiomedTownAccount"))
          str nil t)))))

(test (connectivity-filter-surface-test :depends-on extract-iso-surface-test)
  (is (stringp
       (with-output-to-string (str)
         (ip::raw-irs-achieve-goal
          'ocml::lhdl-application 'ocml::connectivity-filter-surface-goal
          `((ocml::has-binary ,*surface-vme*)
            (ocml::has-xml ,(format nil "~A.xml" *surface-vme*))
            (#_lhdlG:hasAccount "#_defaultBiomedTownAccount")) str nil t)))))

(test (generate-normal-filter-surface-test :depends-on extract-iso-surface-test)
  (is (stringp
       (with-output-to-string (str)
         (ip::raw-irs-achieve-goal
          'ocml::lhdl-application 'ocml::generate-normal-filter-surface-goal
          `((ocml::has-filename ,*surface-vme*)
            (ocml::has-xml ,(format nil "~A.xml" *surface-vme*))
            (#_lhdlG:hasAccount "#_defaultBiomedTownAccount")
            (ocml::has-angle "2")
            (ocml::has-flip-normals "false")
            (ocml::has-edge-split "true"))
          str nil t)))))

(test (smooth-filter-surface-test :depends-on extract-iso-surface-test)
  (is (stringp
       (with-output-to-string (str)
         (ip::raw-irs-achieve-goal
          'ocml::lhdl-application 'ocml::smooth-filter-surface-goal
          `((ocml::has-filename ,*surface-vme*)
            (ocml::has-xml ,(format nil "~A.xml" *surface-vme*))
            (#_lhdlG:hasAccount "#_defaultBiomedTownAccount")
            (ocml::has-iterations "2")) str nil t)))))

(test (strip-filter-surface-test :depends-on extract-iso-surface-test)
  (is (stringp
       (with-output-to-string (str)
         (ip::raw-irs-achieve-goal
          'ocml::lhdl-application 'ocml::strip-filter-surface-goal
          `((ocml::has-filename ,*surface-vme*)
            (ocml::has-xml ,(format nil "~A.xml" *surface-vme*))
            (#_lhdlG:hasAccount "#_defaultBiomedTownAccount")) str nil t)))))

(test (triangulate-filter-surface-test :depends-on extract-iso-surface-test)
  (is (stringp
       (with-output-to-string (str)
         (ip::raw-irs-achieve-goal
          'ocml::lhdl-application 'ocml::triangulate-filter-surface-goal
          `((ocml::has-filename ,*surface-vme*)
            (ocml::has-xml ,(format nil "~A.xml" *surface-vme*))
            (#_lhdlG:hasAccount "#_defaultBiomedTownAccount"))
          str nil t)))))

(test (extract-iso-surface-test :depends-on import-vtk-test)
  (finishes
    (setf *surface-vme*
          (with-output-to-string (str)
            (ip::raw-irs-achieve-goal
             'ocml::lhdl-application 'ocml::extract-iso-surface-goal
             `((ocml::has-url ,*vtk-as-vme*)
               (ocml::has-xml ,(format nil "~A.xml" *vtk-as-vme*))
               (ocml::has-contour "100")
               (ocml::has-auto-lod "true")
               (ocml::has-optimise "true")
               (#_lhdlG:hasAccount "#_defaultBiomedTownAccount"))
             str nil t)))))

(test (decimate-surface-test :depends-on extract-iso-surface-test)
  (finishes
    (with-output-to-string (str)
      (ip::raw-irs-achieve-goal
       'ocml::lhdl-application 'ocml::decimate-surface-goal
       `((ocml::has-file-url ,*surface-vme*)
         (ocml::has-xml ,(format nil "~A.xml" *surface-vme*))
         (ocml::has-preserve-topology "true")
         (ocml::has-target-reduction "1")
         (#_lhdlG:hasAccount "#_defaultBiomedTownAccount"))
       str nil t))))

(test (render-surface-test :depends-on extract-iso-surface-test)
  (finishes
    (with-output-to-string (str)
      (ip::raw-irs-achieve-goal
       'ocml::lhdl-application 'ocml::render-surface-goal
       `((ocml::has-file-url ,*surface-vme*)
         (ocml::has-xml ,(format nil "~A.xml" *surface-vme*))
         (ocml::has-background-colour "100/100/100")
         (ocml::has-zoom-factor "160.0")
         (ocml::has-camera-azimuth "90")
         (ocml::has-camera-roll "0")
         (ocml::has-image-width "100")
         (ocml::has-image-height "100")
         (#_lhdlG:hasAccount "#_defaultBiomedTownAccount"))
       str nil t))))

(test soap-goal-invocation-test
  (finishes (utilities:http-request "http://localhost:3000/soap"
                                 :method :post
                                 :additional-headers '(("SOAPAction" .  ""))
                                 :content-type "text/xml; charset=utf-8"
                                 :content "
<SOAP-ENV:Envelope SOAP-ENV:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\"
    xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\"
    xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"
    xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
    <SOAP-ENV:Header/>
    <SOAP-ENV:Body>
        <achieve-goal>
            <ONTOLOGY type=\"sexpr\">lhdl-application</ONTOLOGY>
            <GOAL-TYPE type=\"sexpr\">import-vtk-goal</GOAL-TYPE>
            <NUMBER-OF-INPUT-ROLE-VALUE-PAIRS type=\"int\">2</NUMBER-OF-INPUT-ROLE-VALUE-PAIRS>
            <INPUT-NAME type=\"sexpr\">has-filename</INPUT-NAME><INPUT-VALUE type=\"string\">sample/head.vtk</INPUT-VALUE>
            <INPUT-NAME type=\"sexpr\">has-compress</INPUT-NAME><INPUT-VALUE type=\"string\">true</INPUT-VALUE>
        </achieve-goal>
    </SOAP-ENV:Body>
</SOAP-ENV:Envelope>"
                                 :connection-timeout nil)))
;;; }}}
