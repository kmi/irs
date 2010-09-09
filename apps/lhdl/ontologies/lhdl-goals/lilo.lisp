;;; Mode: Lisp; Package: ocml

;;; This file contains elevation functions.

(in-package "OCML")

(in-ontology lhdl-goals)

;;; {{{ mmo-vtk-importer-web-service
(deflower lower-lhdl-filename lhdl-filename
  (lambda (instance)
    (format nil "<fileName>~A</fileName>" (get-role-value instance 'has-value))))

(defun lhdl-boolean->string (instance)
  (if (get-role-value instance 'has-value)
      "true"
      "false"))

(deflower lower-lhdl-boolean lhdl-boolean
  (lambda (instance)
    (format nil "<isZip>~A</isZip>" (lhdl-boolean->string instance))))

;;; }}}

(deflower lower-lhdl-xml lhdl-xml
  (lambda (instance)
    (format nil "<xmlpart>~A</xmlpart>" (get-role-value instance 'has-value))))

(define-skyhook skyhook-lhdl-xml lhdl-xml
  (lambda (value)
    (xpm::with-ocml-ctx (:class 'lhdl-xml)
      (let ((i (xpm::o-instance)))
        (xpm::o-set-slot 'ocml::has-value value i)
        i))))

;;; {{{ for extract-iso-surface
(deflower lower-lhdl-url lhdl-url
  (lambda (instance)
    (format nil "<binaryURI>~A</binaryURI>" (get-role-value instance 'has-value))))

(define-skyhook skyhook-lhdl-url lhdl-url
  (lambda (value)
    (xpm::with-ocml-ctx (:class 'lhdl-url)
      (let ((i (xpm::o-instance)))
        (xpm::o-set-slot 'ocml::has-value value i)
        i))))

(deflower lower-autolod autolod
  (lambda (instance)
    (format nil "<autoLOD>~A</autoLOD>"
            (if (get-role-value instance 'has-value)
                "1"
                "0"))))

(define-skyhook skyhook-autolod autolod
  (lambda (value)
    (xpm::with-ocml-ctx (:class 'autolod)
      (let ((i (xpm::o-instance)))
        (xpm::o-set-slot 'ocml::has-value (string= "true" value) i)
        i))))

(define-skyhook skyhook-optimise optimise
  (lambda (value)
    (xpm::with-ocml-ctx (:class 'optimise)
      (let ((i (xpm::o-instance)))
        (xpm::o-set-slot 'ocml::has-value (string= "true" value) i)
        i))))

(define-skyhook skyhook-contour contour
  (lambda (value)
    (xpm::with-ocml-ctx (:class 'contour)
      (let ((i (xpm::o-instance))
            (val (read-from-string value)))
        (if (typep val 'number)
            (setf val (coerce val 'float))
            (error "Cannot elevate ~S to an lhdl-double." value))
        (xpm::o-set-slot 'ocml::has-value val i)
        i))))

(deflower lower-contour contour
  (lambda (instance)
    (format nil "<contour>~A</contour>" (get-role-value instance 'has-value))))

(deflower lower-optimise optimise
  (lambda (instance)
    (format nil "<optimise>~A</optimise>" 
            (if (get-role-value instance 'has-value)
                "1"
                "0"))))

;;; }}} 

(define-skyhook skyhook-camera-perspective camera-perspective
  (lambda (value)
    (let ((val (assoc value '(("bottom" . camera-perspective-bottom)
                              ("top" . camera-perspective-top)
                              ("back" . camera-perspective-back)
                              ("front" . camera-perspective-front)
                              ("left" . camera-perspective-left)
                              ("right" . camera-perspective-right))
                      :test #'string=)))
      (if val
          (cdr val)
          (error "Cannot lift camera-perspective ~S." value)))))

(define-skyhook skyhook-background-colour background-colour
  (lambda (value)
    (labels ((as-float (lispval)
               (if (typep lispval 'number)
                   (coerce lispval 'float)
                   (error "Cannot elevate ~S to an lhdl-double." value))))
      (xpm::with-ocml-ctx (:class 'background-colour)
        (let ((i (xpm::o-instance))
              (numbers (mapcar #'read-from-string
                               (cl-ppcre:all-matches-as-strings "[0-9.]+" value))))
          (xpm::o-set-slot 'ocml::has-red-value (as-float (first numbers)) i)
          (xpm::o-set-slot 'ocml::has-green-value (as-float (second numbers)) i)
          (xpm::o-set-slot 'ocml::has-blue-value (as-float (third numbers)) i)
          i)))))

(deflower lower-camera-perspective camera-perspective
  (lambda (instance)
    (format nil "<cameraPerspective>~A</cameraPerspective>" 
            (or (position instance '(camera-perspective-bottom
                                     camera-perspective-top
                                     camera-perspective-back
                                     camera-perspective-front
                                     camera-perspective-left
                                     camera-perspective-right))
                (error "Cannot lower camera-perspective ~S." instance)))))

(deflower lower-background-colour background-colour
  (lambda (instance)
    (format nil "<bgColor0>~A</bgColor0><bgColor1>~A</bgColor1><bgColor2>~A</bgColor2>"
            (get-role-value instance 'has-red-value)
            (get-role-value instance 'has-green-value)
            (get-role-value instance 'has-blue-value))))

(define-skyhook skyhook-zoom-factor zoom-factor
  (lambda (value)
    (xpm::with-ocml-ctx (:class 'zoom-factor)
      (let ((i (xpm::o-instance))
            (val (read-from-string value)))
        (if (typep val 'number)
            (setf val (coerce val 'float))
            (error "Cannot elevate ~S to an lhdl-double." value))
        (xpm::o-set-slot 'ocml::has-value val i)
        i))))

(define-skyhook skyhook-camera-azimuth camera-azimuth
  (lambda (value)
    (xpm::with-ocml-ctx (:class 'camera-azimuth)
      (let ((i (xpm::o-instance))
            (val (read-from-string value)))
        (if (typep val 'number)
            (setf val (coerce val 'float))
            (error "Cannot elevate ~S to an lhdl-double." value))
        (xpm::o-set-slot 'ocml::has-value val i)
        i))))

(define-skyhook skyhook-camera-roll camera-roll
  (lambda (value)
    (xpm::with-ocml-ctx (:class 'camera-roll)
      (let ((i (xpm::o-instance))
            (val (read-from-string value)))
        (if (typep val 'number)
            (setf val (coerce val 'float))
            (error "Cannot elevate ~S to an lhdl-double." value))
        (xpm::o-set-slot 'ocml::has-value val i)
        i))))

(define-skyhook skyhook-image-width image-width
  (lambda (value)
    (xpm::with-ocml-ctx (:class 'image-width)
      (let ((i (xpm::o-instance))
            (val (read-from-string value)))
        (if (typep val 'number)
            (setf val (coerce val 'float))
            (error "Cannot elevate ~S to an lhdl-double." value))
        (xpm::o-set-slot 'ocml::has-value val i)
        i))))

(define-skyhook skyhook-image-height image-height
  (lambda (value)
    (xpm::with-ocml-ctx (:class 'image-height)
      (let ((i (xpm::o-instance))
            (val (read-from-string value)))
        (if (typep val 'number)
            (setf val (coerce val 'float))
            (error "Cannot elevate ~S to an lhdl-double." value))
        (xpm::o-set-slot 'ocml::has-value val i)
        i))))


(deflower lower-image-height image-height
  (lambda (instance)
    (format nil "<imageHeight>~A</imageHeight>"
            (get-role-value instance 'has-value))))

(deflower lower-image-width image-width
  (lambda (instance)
    (format nil "<imageWidth>~A</imageWidth>"
            (get-role-value instance 'has-value))))

(deflower lower-camera-roll camera-roll
  (lambda (instance)
    (format nil "<camRoll>~A</camRoll>"
            (get-role-value instance 'has-value))))

(deflower lower-camera-azimuth camera-azimuth
  (lambda (instance)
    (format nil "<camAzimuth>~A</camAzimuth>"
            (get-role-value instance 'has-value))))

(deflower lower-zoom-factor zoom-factor
  (lambda (instance)
    (format nil "<zoomFactor>~A</zoomFactor>"
            (get-role-value instance 'has-value))))

(deflift lift-base64-image base64-image ()
  (lambda (xml)
    (xpm::with-ocml-ctx (:class 'base64-image)
      (let* ((i (xpm::o-instance))
             (x (xpm::document-parser xml))
             (text (dom:node-value (first (xpm::children (first (xpm::children (first (xpm::children (first (xpm::children (first (xpm::children x)))))))))))))
        (xpm::o-set-slot 'ocml::has-value text i)
        i))))

;;  (("has-value" "/Envelope/Body/renderSurfaceResponse/renderSurfaceResult/text()")))

;; (deflift lift-base64-image base64-image ()
;;   (("has-value" "Envelope/Body/renderSurfaceResponse string/text()")))

(deflower lower-base64-image base64-image
  (lambda (instance)
    (format nil "~A" (get-role-value instance 'has-value))))

(deflower lower-preserve-topology preserve-topology
  (lambda (instance)
    (format nil "<preserveTopology>~A</preserveTopology>"
            (if (get-role-value instance 'has-value)
                "true"
                "false"))))

(deflower lower-target-reduction target-reduction
  (lambda (instance)
    (format nil "<targetReduction>~A</targetReduction>"
            (get-role-value instance 'has-value))))

(deflift lift-uri-result uri-result ()
  (lambda (xml)
    (xpm::with-ocml-ctx (:class 'uri-result)
      (let* ((i (xpm::o-instance))
             (x (xpm::document-parser xml))
	     (soap-body (aref (dom:get-elements-by-tag-name-ns x #"*" "Body") 0))
             (text (irs.api.rest::get-first-text-child (first (xpm::children (first (xpm::children soap-body)))))))
        (xpm::o-set-slot 'ocml::has-uri text i)
        i))))

(deflower lower-uri-result uri-result
  (lambda (instance)
    (get-role-value instance 'has-uri)))

(define-skyhook skyhook-vtk-file-format vtk-file-format
  (lambda (value)
    (cond ((string= "ascii" value)
           (find-current-instance 'ocml::vtk-file-format-ascii))
          ((string= "binary" value)
           (find-current-instance 'ocml::vtk-file-format-binary))
          (t
           (error "Cannot elevate ~S to a vtk-file-format." value)))))

(deflower lower-vtk-file-format vtk-file-format
  (lambda (instance)
    (format nil "<format>~A</format>"
            (ecase (name instance)
              ((vtk-file-format-ascii) 0)
              ((vtk-file-format-binary) 1)))))

(deflower lower-abs-matrix abs-matrix
  (lambda (instance)
    (format nil "<ABSMatrix>~A</ABSMatrix>"
            (lhdl-boolean->string instance))))

;;; {{{ DICOM dictionary
(deflower lower-dicom-dictionary dicom-dictionary
  (lambda (instance)
    (format nil "<dictionary>~A</dictionary>"
            (get-role-value instance 'has-value))))

(define-skyhook skyhook-dicom-dictionary dicom-dictionary
  (lambda (value)
    (%skyhook-string value 'dicom-dictionary)))
;;; }}}

;;; {{{ DICOM Zip file

;;; WTF kind of argument is this anyway?
(define-skyhook skyhook-folder dicom-zipfile
  (lambda (value)
    (%skyhook-string value 'dicom-zipfile)))

(deflower lower-folder dicom-zipfile
  (lambda (instance)
    (format nil "<dicomZipFile>~A</dicomZipFile>"
            (get-role-value instance 'has-value))))
;;; }}}

;;; {{{ DICOM type
(deflower lower-dicom-type dicom-type
  (lambda (instance)
    (format nil "<DICOMType>~A</DICOMType>"
            (get-role-value instance 'has-value))))

(define-skyhook skyhook-dicom-type dicom-type
  (lambda (value)
    (%skyhook-integer value 'dicom-type)))
;;; }}}

;;; {{{ DICOM build step
(deflower lower-dicom-build-step dicom-build-step
  (lambda (instance)
    (format nil "<buildStepValue>~A</buildStepValue>"
            (get-role-value instance 'has-value))))

(define-skyhook skyhook-dicom-build-step dicom-build-step
  (lambda (value)
    (%skyhook-integer value 'dicom-build-step)))
;;; }}}

;;; {{{ image file sequences
(deflower lower-image-file-sequence image-file-sequence
  (lambda (instance)
    (with-output-to-string (str)
      (format str "<files>")
      (dolist (file (get-role-value instance 'has-value))
        (format str "<string>~A</string>" (get-role-value file 'has-filename)))
      (format str "</files>")
      str)))

(define-skyhook skyhook-image-file-sequence image-file-sequence
  (lambda (value)
    (let ((files (mapcar (lambda (str)
                           (xpm::with-ocml-ctx (:class 'image-file)
                             (let ((file (xpm::o-instance)))
                               (xpm::o-set-slot 'ocml::has-filename str file)
                               file)))
                         value)))
      (xpm::with-ocml-ctx (:class 'image-file-sequence)
        (let ((seq (xpm::o-instance)))
          ;; XXX Have to wrap SEQ in another list, because o-set-slot
          ;; likes to turn list values into a set of values.  It
          ;; shouldn't.  There's at least two better ways to do
          ;; multiple values.
          (xpm::o-set-slot 'ocml::has-value (list files) seq)
          seq)))))

;;; }}}

;;; {{{ boolean lowering for import landmarks service
(define-lower lower-lhdl-boolean lhdl-boolean
    (:services (import-landmark-cloud-web-service))
  (lambda (instance)
    (format nil "<taggedFile>~A</taggedFile>"
            (if (get-role-value instance 'has-value)
                "1"
                "0"))))
;;; }}}

;;; {{{ endianess
(define-skyhook skyhook-endianess endianess
  (lambda (value)
    (cond ((string= "big" value)
           'ocml::big-endianess)
          ((string= "little" value)
           'ocml::little-endianess)
          (t
           (error "Cannot elevate ~S to an endianess." value)))))

(define-lower lower-endianess endianess ()
  (lambda (instance)
    (format nil "<endian>~A</endian>"
            (ecase instance
              ((big-endianess) 0)
              ((little-endianess) 1)))))
;;; }}}

;;; {{{ scalar-type
(define-skyhook skyhook-scalar-type scalar-type
  (lambda (value)
    (%skyhook-integer value 'dicom-type)))

(define-lower lower-scalar-type scalar-type ()
  (lambda (instance)
    (format nil "<scalarType>~A</scalarType>"
            (get-role-value instance 'has-value))))
;;; }}}
;;; {{{ scalar signed
(define-lower lower-scalar-signed scalar-sign
    (:services (import-raw-volume-web-service))
  (lambda (instance)
    (format nil "<scalarSigned>~A</scalarSigned>"
            (if (get-role-value instance 'has-value)
                0
                1))))
;;; }}}
;;; {{{ 3d-integer
(define-skyhook skyhook-3d-integer 3d-integer
  (lambda (value)
    (let ((lst (read-from-string value)))
      (xpm::with-ocml-ctx (:class '3d-integer)
        (let ((3d (xpm::o-instance)))
          (xpm::o-set-slot 'has-x (first lst) 3d)
          (xpm::o-set-slot 'has-y (second lst) 3d)
          (xpm::o-set-slot 'has-z (third lst) 3d)
          3d)))))

(define-lower lower-3d-integer 3d-integer ()
  (lambda (instance)
    (with-output-to-string (str)
      (format str "<dimensionX>~A</dimensionX>"
              (get-role-value instance 'has-x))
      (format str "<dimensionY>~A</dimensionY>"
              (get-role-value instance 'has-y))
      (format str "<dimensionZ>~A</dimensionZ>"
              (get-role-value instance 'has-z))
      str)))
;;; }}}

;;; {{{ 3d-double
(define-skyhook skyhook-3d-double 3d-double
  (lambda (value)
    (%skyhook-3d-double value '3d-double)))

(define-lower lower-3d-double 3d-double ()
  (lambda (instance)
    (with-output-to-string (str)
      (format str "<dataSpacing>")
      (dolist (slot '(has-x has-y has-z))
        (format str "<double>~A</double>" (get-role-value instance slot)))
      (format str "</dataSpacing>")
      str)))
;;; }}}

(define-lower lower-file-header header-skip-size ()
  (lambda (instance)
    (format nil "<fileHeader>~A</fileHeader>"
            (get-role-value instance 'has-value))))

(define-lower lower-z-coord-file z-coord-filename
    (:services (import-raw-volume-web-service))
  (lambda (instance)
    (format nil "<zCoordFile>~A</zCoordFile>"
            (get-role-value instance 'has-value))))

(define-lower lower-current-slice slice ()
  (lambda (instance)
    (format nil "<currentSlice>~A</currentSlice>"
            (get-role-value instance 'has-value))))

;;; {{{ 3d-integer
(define-skyhook skyhook-slice-voi slice-voi
  (lambda (value)
    (let ((lst (read-from-string value)))
      (xpm::with-ocml-ctx (:class 'slice-voi)
        (let ((slice (xpm::o-instance)))
          (xpm::o-set-slot 'has-value-one (first lst) slice)
          (xpm::o-set-slot 'has-value-two (second lst) slice)
          slice)))))

(define-lower lower-slice-voi slice-voi ()
  (lambda (instance)
    (with-output-to-string (str)
      (format str "<sliceVOI0>~A</sliceVOI0>"
              (get-role-value instance 'has-value-one))
      (format str "<sliceVOI1>~A</sliceVOI1>"
              (get-role-value instance 'has-value-two))
      str)))
;;; }}}


(define-skyhook skyhook-header-skip-size header-skip-size
  (lambda (value)
    (%skyhook-integer value 'header-skip-size)))

(define-skyhook skyhook-slice slice
  (lambda (value)
    (%skyhook-integer value 'slice)))

;;; {{{ export-stl-web-service
(define-lower lower-abs-matrix abs-matrix
    (:services (export-stl-web-service))
  (lambda (instance)
    (format nil "<ABSMatrixFlag>~A</ABSMatrixFlag>"
            (if (get-role-value instance 'has-value)
                "1"
                "0"))))

(define-skyhook skyhook-stl-file-format stl-file-format
  (lambda (value)
    (cond ((string= "ascii" value)
           (find-current-instance 'ocml::stl-file-format-ascii))
          ((string= "binary" value)
           (find-current-instance 'ocml::stl-file-format-binary))
          (t
           (error "Cannot elevate ~S to a stl-file-format." value)))))

(deflower lower-stl-file-format stl-file-format
  (lambda (instance)
    (format nil "<format>~A</format>"
            (ecase (name instance)
              ((stl-file-format-ascii) 0)
              ((stl-file-format-binary) 1)))))
;;; }}}

(define-skyhook skyhook-colour/grayscale colour/grayscale
  (lambda (value)
    (cond ((string= "colour" value)
           (find-current-instance 'ocml::colour/grayscale-colour))
          ((string= "grayscale" value)
           (find-current-instance 'ocml::colour/grayscale-grayscale))
          (t
           (error "Cannot elevate ~S to a colour/grayscale." value)))))

;;; {{{ export-bmp
(define-lower lower-colour/grayscale colour/grayscale
    (:services (export-bmp-web-service))
  (lambda (instance)
    (format nil "<format>~A</format>"
            (ecase (name instance)
              ((colour/grayscale-colour) 0)
              ((colour/grayscale-grayscale) 1)))))

(define-lower lower-current-slice slice
    (:services (export-bmp-web-service))
  (lambda (instance)
    (format nil "<offset>~A</offset>"
            (get-role-value instance 'has-value))))
;;; }}}

;; volume-measurement

(deflift lift-volume-measurement volume-measurement ()
  (lambda (xml)
    (xpm::with-ocml-ctx (:class 'volume-measurement)
      (let* ((i (xpm::o-instance))
             (x (xpm::document-parser xml))
             (kids (xpm::children (first (xpm::children (first (xpm::children (first (xpm::children (first (xpm::children x)))))))))))
        (xpm::o-set-slot 'ocml::has-nsi (read-from-string (dom:node-value (first (xpm::children (first kids))))) i)
        (xpm::o-set-slot 'ocml::has-surface-area (read-from-string (dom:node-value (first (xpm::children (second kids))))) i)
        (xpm::o-set-slot 'ocml::has-surface-volume (read-from-string (dom:node-value (first (xpm::children (third kids))))) i)
        i))))

(define-lower lower-lhdl-url-for-measure-volume lhdl-url
    (:services (measure-volume-web-service))
  (lambda (instance)
    (format nil "<binaryURI>~A</binaryURI>"
            (get-role-value instance 'has-value))))

;;; This is mainly so we get a nice output from calls. 
(define-lower lower-volume-measurement volume-measurement ()
  (lambda (instance)
    (format nil "<<volume-measurement> nsi=~A area=~A volume=~A>"
            (get-role-value instance 'has-nsi)
            (get-role-value instance 'has-surface-area)
            (get-role-value instance 'has-surface-volume))))

;;; {{{ parametric surfaces
(define-skyhook skyhook-height height
  (lambda (value)
    (%skyhook-double value 'height)))

(define-lower lower-height height
    (:services (create-cone-surface-parametric-web-service
                create-cylinder-surface-parametric-web-service))
  (lambda (instance)
    (format nil "<height>~A</height>"
            (get-role-value instance 'has-value))))

(define-skyhook skyhook-radius radius
  (lambda (value)
    (%skyhook-double value 'radius)))

(define-lower lower-radius radius
    (:services (create-sphere-surface-parametric-web-service
                create-cone-surface-parametric-web-service
                create-cylinder-surface-parametric-web-service))
  (lambda (instance)
    (format nil "<radius>~A</radius>"
            (get-role-value instance 'has-value))))

(define-skyhook skyhook-resolution resolution
  (lambda (value)
    (%skyhook-double value 'resolution)))

(define-lower lower-resolution resolution
    (:services (create-cone-surface-parametric-web-service
                create-cylinder-surface-parametric-web-service))
  (lambda (instance)
    (format nil "<resolution>~A</resolution>"
            (get-role-value instance 'has-value))))

(define-skyhook skyhook-cap
  (lambda (value)
    (%skyhook-boolean value 'lhdl-boolean)))

(define-lower lower-cap lhdl-boolean
    (:services (create-cone-surface-parametric-web-service))
  (lambda (instance)
    (format nil "<cap>~A</cap>"
            (if (get-role-value instance 'has-value)
                "1"
                "0"))))

(define-skyhook skyhook-x-length x-length
  (lambda (value)
    (%skyhook-double value 'x-length)))

(define-lower lower-x-length x-length
    (:services (create-cube-surface-parametric-web-service))
  (lambda (instance)
    (format nil "<xLength>~A</xLength>"
            (get-role-value instance 'has-value))))

(define-skyhook skyhook-y-length y-length
  (lambda (value)
    (%skyhook-double value 'y-length)))

(define-lower lower-y-length y-length
    (:services (create-cube-surface-parametric-web-service))
  (lambda (instance)
    (format nil "<yLength>~A</yLength>"
            (get-role-value instance 'has-value))))

(define-skyhook skyhook-z-length z-length
  (lambda (value)
    (%skyhook-double value 'z-length)))

(define-lower lower-z-length z-length
    (:services (create-cube-surface-parametric-web-service))
  (lambda (instance)
    (format nil "<zLength>~A</zLength>"
            (get-role-value instance 'has-value))))

;;; }}}

(define-skyhook skyhook-origin origin
  (lambda (value)
    (%skyhook-3d-double value 'origin)))

(define-skyhook skyhook-point-1 point-1
  (lambda (value)
    (%skyhook-3d-double value 'point-1)))

(define-skyhook skyhook-origin point-2
  (lambda (value)
    (%skyhook-3d-double value 'point-2)))

(define-skyhook skyhook-bt-account #_domain:BiomedTownAccount
  (lambda (value)
    (%skyhook-instance-name value '#_domain:BiomedTownAccount)))

(define-lower lower-bt-account1 #_domain:BiomedTownAccount
    (:services (export-bmp-web-service
                export-stl-web-service
                export-vrml-web-service
                export-vtk-web-service
                render-surface-web-service))
  (lambda (instance)
    (let* ((username (get-role-value
                      (get-role-value instance '#_domain:hasUsername) 'has-value))
           (password (get-role-value
                      (get-role-value instance '#_domain:hasPassword) 'has-value)))
      (format nil "<user>~A</user><password>~A</password>" username password))))

(define-lower lower-bt-account2 #_domain:BiomedTownAccount
    (:services (clean-filter-surface-web-service
                connectivity-filter-surface-web-service
                create-cone-surface-parametric-web-service
                create-cube-surface-parametric-web-service
                create-cylinder-surface-parametric-web-service
                create-plane-surface-parametric-web-service
                create-ref-sys-web-service
                create-sphere-surface-parametric-web-service
                decimate-surface-web-service
                extract-iso-surface-web-service
                generate-normal-filter-surface-web-service
                measure-volume-web-service
                smooth-filter-surface-web-service
                strip-filter-surface-web-service))
  (lambda (instance)
    (let* ((username (get-role-value
                      (get-role-value instance '#_domain:hasUsername) 'has-value))
           (password (get-role-value
                      (get-role-value instance '#_domain:hasPassword) 'has-value)))
      (format nil "<userName>~A</userName><password>~A</password>" username password))))

(define-lower lower-bt-account4 #_domain:BiomedTownAccount
    (:services (import-dicom-web-service
                import-image-sequence-web-service
                import-landmark-cloud-web-service
                import-raw-volume-web-service
                import-stl-web-service
                import-vrml-web-service
                import-vtk-web-service))
  (lambda (account)
    (let* ((username (get-role-value
                      (get-role-value account '#_domain:hasUsername) 'has-value))
           (password (get-role-value
                      (get-role-value account '#_domain:hasPassword) 'has-value)))
      (format nil "<user>~A</user><psw>~A</psw>" username password))))

(define-lower lower-origin-for-create-plane-surface-parametric origin
    (:services (create-plane-surface-parametric-web-service))
  (lambda (instance)
    (format nil "<origin><double>~A</double><double>~A</double><double>~A</double></origin>"
            (get-role-value instance 'has-x)
            (get-role-value instance 'has-y)
            (get-role-value instance 'has-z))))

(define-lower lower-point-1-for-create-plane-surface-parametric point-1
    (:services (create-plane-surface-parametric-web-service))
  (lambda (instance)
    (format nil "<point1><double>~A</double><double>~A</double><double>~A</double></point1>"
            (get-role-value instance 'has-x)
            (get-role-value instance 'has-y)
            (get-role-value instance 'has-z))))

(define-lower lower-point-2-for-create-plane-surface-parametric point-2
    (:services (create-plane-surface-parametric-web-service))
  (lambda (instance)
    (format nil "<point2><double>~A</double><double>~A</double><double>~A</double></point2>"
            (get-role-value instance 'has-x)
            (get-role-value instance 'has-y)
            (get-role-value instance 'has-z))))

(define-skyhook skyhook-x-resolution x-resolution
  (lambda (value)
    (%skyhook-double value 'x-resolution)))

(define-skyhook skyhook-y-resolution y-resolution
  (lambda (value)
    (%skyhook-double value 'y-resolution)))

(define-lower lower-x-resolution x-resolution
    (:services (create-plane-surface-parametric-web-service))
  (lambda (instance)
    (format nil "<xResolution>~A</xResolution>"
            (get-role-value instance 'has-value))))

(define-lower lower-y-resolution y-resolution
    (:services (create-plane-surface-parametric-web-service))
  (lambda (instance)
    (format nil "<yResolution>~A</yResolution>"
            (get-role-value instance 'has-value))))

(define-lower lower-phi-resolution phi-resolution
    (:services (create-sphere-surface-parametric-web-service))
  (lambda (instance)
    (format nil "<phiResolution>~A</phiResolution>"
            (get-role-value instance 'has-value))))

(define-lower lower-theta-resolution theta-resolution
    (:services (create-sphere-surface-parametric-web-service))
  (lambda (instance)
    (format nil "<thetaResolution>~A</thetaResolution>"
            (get-role-value instance 'has-value))))

(define-skyhook skyhook-phi-resolution phi-resolution
  (lambda (value)
    (%skyhook-integer value 'phi-resolution)))

(define-skyhook skyhook-theta-resolution theta-resolution
  (lambda (value)
    (%skyhook-integer value 'theta-resolution)))

(define-skyhook skyhook-lhdl-name lhdl-name
  (lambda (value)
    (%skyhook-string value 'lhdl-name)))

(define-skyhook skyhook-scale scale
  (lambda (value)
    (%skyhook-double value 'scale)))

;;; {{{ create-ref-sys
(define-lower lower-origin-for-create-ref-sys origin
    (:services (create-ref-sys-web-service))
  (lambda (instance)
    (format nil "<originLandmark><double>~A</double><double>~A</double><double>~A</double></originLandmark>"
            (get-role-value instance 'has-x)
            (get-role-value instance 'has-y)
            (get-role-value instance 'has-z))))

(define-lower lower-point-1-for-create-ref-sys point-1
    (:services (create-ref-sys-web-service))
  (lambda (instance)
    (format nil "<point1Landmark><double>~A</double><double>~A</double><double>~A</double></point1Landmark>"
            (get-role-value instance 'has-x)
            (get-role-value instance 'has-y)
            (get-role-value instance 'has-z))))

(define-lower lower-point-2-for-create-ref-sys point-2
    (:services (create-ref-sys-web-service))
  (lambda (instance)
    (format nil "<point2Landmark><double>~A</double><double>~A</double><double>~A</double></point2Landmark>"
            (get-role-value instance 'has-x)
            (get-role-value instance 'has-y)
            (get-role-value instance 'has-z))))

(define-lower lower-lhdl-name lhdl-name
    (:services (create-ref-sys-web-service))
  (lambda (instance)
    (format nil "<name>~A</name>" (get-role-value instance 'has-value))))

(define-lower lower-scale scale
    (:services (create-ref-sys-web-service))
  (lambda (instance)
    (format nil "<scale>~A</scale>" (get-role-value instance 'has-value))))

;;; }}}

(define-skyhook skyhook-iterations iterations
  (lambda (value)
    (%skyhook-integer value 'iterations)))

(define-lower lower-iterations iterations
    (:services (smooth-filter-surface-web-service))
  (lambda (instance)
    (format nil "<numberOfIterations>~A</numberOfIterations>"
            (get-role-value instance 'has-value))))

(define-skyhook skyhook-angle angle
  (lambda (value)
    (%skyhook-integer value 'angle)))

(define-skyhook skyhook-flip-normal flip-normals
  (lambda (value)
    (%skyhook-boolean value 'flip-normals)))

(define-skyhook skyhook-edge-split edge-split
  (lambda (value)
    (%skyhook-boolean value 'edge-split)))

(define-lower lower-edge-split edge-split ()
  (lambda (instance)
    (format nil "<edgeSplit>~A</edgeSplit>"
            (if (get-role-value instance 'has-value) "0" "1"))))

(define-lower lower-flip-normals flip-normals ()
  (lambda (instance)
    (format nil "<flipNormals>~A</flipNormals>"
            (if (get-role-value instance 'has-value) "0" "1"))))

(define-lower lower-angle angle ()
  (lambda (instance)
    (format nil "<angle>~A</angle>"
            (get-role-value instance 'has-value))))

;;; {{{ Execution orchestration

(define-skyhook skyhook-source source
  (lambda (string)
    (xpm::with-ocml-ctx (:class 'source)
      (let ((vals (read-from-string string)))
        (xpm::with-ocml-ctx (:class (first vals))
          (let ((i (xpm::o-instance)))
            (cond ((or (equalp (first vals) 'ocml::VTKsource)
                       (equalp (first vals) 'ocml::STLsource))
                   (xpm::o-set-slot 'ocml::has-filename (string-downcase (symbol-name (second vals))) i))
                  ((equalp (first vals) 'ocml::CreateCone)
                   (xpm::o-set-slot 'ocml::has-height (write-to-string (second vals)) i)
                   (xpm::o-set-slot 'ocml::has-radius (write-to-string (third vals)) i)
                   (xpm::o-set-slot 'ocml::has-resolution (write-to-string (fourth vals)) i)
                   (xpm::o-set-slot 'ocml::has-cap (string-downcase (symbol-name (fifth vals))) i)
                   )
                  ((equalp (first vals) 'ocml::CreateCube)
                   (xpm::o-set-slot 'ocml::has-x (write-to-string (second vals)) i)
                   (xpm::o-set-slot 'ocml::has-y (write-to-string (third vals)) i)
                   (xpm::o-set-slot 'ocml::has-z (write-to-string (fourth vals)) i)
                   )
                  
                  )
            i)
          )))))

(define-skyhook skyhook-operation operation
  (lambda (string)
    (xpm::with-ocml-ctx (:class 'operation)
      (let ((vals (read-from-string string)))
        (xpm::with-ocml-ctx (:class (first vals))
          (let ((i (xpm::o-instance)))
            (cond ((equalp (first vals) 'ocml::SmoothSurface)
                   (xpm::o-set-slot 'ocml::has-iterations (write-to-string (second vals)) i))                  
                  )
            i)
          )))))

(define-skyhook skyhook-sink sink
  (lambda (string)
    (xpm::with-ocml-ctx (:class 'sink)
      (let ((vals (read-from-string string)))
        (xpm::with-ocml-ctx (:class (first vals))
          (let ((i (xpm::o-instance)))
            (cond ((or (equalp (first vals) 'ocml::ExportVTK)
                       (equalp (first vals) 'ocml::ExportSTL))
                   (xpm::o-set-slot 'ocml::has-ABSMatrixFlag (string-downcase (symbol-name (second vals))) i)
                   (xpm::o-set-slot 'ocml::has-format (string-downcase (symbol-name (third vals))) i)
                   )
                  ((equalp (first vals) 'ocml::RenderSurface)
                   (xpm::o-set-slot 'ocml::has-bgColour (string-downcase (symbol-name (second vals))) i)
                   (xpm::o-set-slot 'ocml::has-zoom (write-to-string (third vals)) i)
                   (xpm::o-set-slot 'ocml::has-camAzimuth (write-to-string (fourth vals)) i)
                   (xpm::o-set-slot 'ocml::has-camRoll (write-to-string (fifth vals)) i)
                   (xpm::o-set-slot 'ocml::has-imageWidth (write-to-string (elt vals 5)) i)
                   (xpm::o-set-slot 'ocml::has-imageHeight (write-to-string (elt vals 6)) i)
                   )                                    
                  )
            i)
          )))))

;;; }}}
