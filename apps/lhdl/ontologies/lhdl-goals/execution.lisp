;;; File created in Dave Lambert's head and recorded in Emacs.

;;; The stuff in here is about the LHDL execution web services at
;;; Bedfordshire.

(in-package #:ocml)

(in-ontology lhdl-goals)

(def-class import-goal (execution-goal))

(def-class export-goal (execution-goal))

(def-class modify-goal (execution-goal))

(def-class display-goal (execution-goal))

(def-class measure-goal (execution-goal))

(def-class filter-goal (execution-goal))

(def-class create-surface-parametric-goal (execution-goal))

;;; {{{ import-vtk-goal
(def-class import-vtk-goal (import-goal) ?goal
    ((has-input-role :value has-filename
                     :value #_hasAccount)
     (has-input-soap-binding :value (has-filename "string")
                             :value (#_hasAccount "string"))
     (has-output-role :value has-uri-result)
     (has-output-soap-binding :value (has-uri-result "xml"))
     (has-filename :type lhdl-filename)
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (has-uri-result :type uri-result)
     (has-non-functional-properties :value import-vtk-goal-non-functional-properties)))

(def-class import-vtk-goal-non-functional-properties (non-functional-properties)
    ((has-description "Convert VTK file to VME.  Returns URL pointing at new VME.")))
;;; }}}
;;; {{{ import-stl-goal 
(def-class import-stl-goal (import-goal) ?goal
    ((has-input-role :value has-filename
                     :value #_hasAccount)
     (has-input-soap-binding :value (has-filename "string")
                             :value (#_hasAccount "string"))
     (has-output-role :value has-uri-result)
     (has-output-soap-binding :value (has-uri-result "xml"))
     (has-filename :type lhdl-filename)
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (has-uri-result :type uri-result)
     (has-non-functional-properties :value import-stl-goal-non-functional-properties)))


(def-class import-stl-goal-non-functional-properties (non-functional-properties)
    ((has-description "Convert STL file to VME.  Returns URL pointing at new VME.")))
;;; }}}
;;; {{{ import-dicom-goal
(def-class import-dicom-goal (import-goal) ?goal
    ((has-input-role :value #_hasAccount
                     :value has-dictionary
                     :value has-dicom-zipfile
                     :value has-dicom-type
                     :value has-build-step)
     (has-input-soap-binding
      :value (#_hasAccount "string")
      :value (has-dictionary "string")
      :value (has-dicom-zipfile "string")
      :value (has-dicom-type "string")
      :value (has-build-step "string"))
     (has-output-role :value has-uri-result)
     (has-output-soap-binding :value (has-uri-result "xml"))
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (has-dictionary :type dicom-dictionary)
     (has-dicom-zipfile :type dicom-zipfile)
     (has-dicom-type :type dicom-type)
     (has-build-step :type dicom-build-step)
     (has-uri-result :type uri-result)
     (has-non-functional-properties :value import-dicom-goal-non-functional-properties)))

(def-class import-dicom-goal-non-functional-properties (non-functional-properties)
    ((has-description "Convert DICOM file to VME.  Returns URL pointing at new VME.")))
;;; }}}
;;; {{{ import-vrml-goal
(def-class import-vrml-goal (import-goal) ?goal
    ((has-input-role :value #_hasAccount
                     :value has-filename)
     (has-input-soap-binding :value (has-filename "string")
                             :value (#_hasAccount "string"))
     (has-output-role :value has-uri-result)
     (has-output-soap-binding :value (has-uri-result "xml"))
     (has-filename :type lhdl-filename)
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (has-uri-result :type uri-result)
     (has-non-functional-properties :value import-vrml-goal-non-functional-properties)))

(def-class import-vrml-goal-non-functional-properties (non-functional-properties)
    ((has-description "Convert VTK file to VME.  Returns URL pointing at new VME.")))
;;; }}}
;;; {{{ import-image-sequence-goal
(def-class import-image-sequence-goal (import-goal) ?goal
    ((has-input-role :value #_hasAccount
                     :value has-filenames)
     (has-input-soap-binding :value (has-filenames "sexp")
                             :value (#_hasAccount "string"))
     (has-output-role :value has-uri-result)
     (has-output-soap-binding :value (has-uri-result "xml"))
     (has-filenames :type image-file-sequence)
     (has-uri-result :type uri-result)
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (has-non-functional-properties :value import-image-sequence-goal-non-functional-properties)))

(def-class import-image-sequence-goal-non-functional-properties (non-functional-properties)
    ((has-description "Convert sequence of image files to a VME.
    Returns URL pointing at new VME.")))
;;; }}}
;;; {{{ import-landmark-cloud-goal
(def-class import-landmark-cloud-goal (import-goal) ?goal
    ((has-input-role :value has-filename
                     :value has-tagged-file
                     :value #_hasAccount)
     (has-input-soap-binding :value (has-filename "string")
                             :value (has-tagged-file "string")
                             :value (#_hasAccount "string"))
     (has-output-role :value has-uri-result)
     (has-output-soap-binding :value (has-uri-result "xml"))
     (has-filename :type lhdl-filename)
     (has-tagged-file :type lhdl-boolean)
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (has-uri-result :type uri-result) 
     (has-non-functional-properties :value import-landmark-cloud-goal-non-functional-properties)))

(def-class import-landmark-cloud-goal-non-functional-properties (non-functional-properties)
    ((has-description "Translate landmarks into a VME.

A VMELandmarkCloud is created. The landmark file is read from server
storage. The created VME is stored on the server as ZMSF format. The
service returns a link to the created files.")))
;;; }}}
;;; {{{ import-raw-volume-goal
(def-class import-raw-volume-goal (import-goal) ?goal
    ((has-input-role :value has-filename
                     :value has-scalar-type
                     :value has-endianess
                     :value has-scalar-signed
                     :value has-data-dimension
                     :value has-slice-voi
                     :value has-data-spacing
                     :value has-header-skip-size
                     :value has-z-coord-file
                     :value has-current-slice
                     :value #_hasAccount)
     (has-input-soap-binding
      :value (has-filename "string")
      :value (has-scalar-type "string")
      :value (has-endianess "string")
      :value (has-scalar-signed "string")
      :value (has-data-dimension "string")
      :value (has-slice-voi "string")
      :value (has-data-spacing "string")
      :value (has-header-skip-size "string")
      :value (has-z-coord-file "string")
      :value (has-current-slice "string")
      :value (#_hasAccount "string"))
     (has-output-role :value has-uri-result)
     (has-output-soap-binding :value (has-uri-result "xml"))
     (has-filename :type raw-volume-filename)
     (has-scalar-type :type scalar-type)
     (has-endianess :type endianess)
     (has-scalar-signed :type scalar-sign)
     (has-data-dimension :type 3d-integer)
     (has-slice-voi :type slice-voi)
     (has-data-spacing :type 3d-double)
     (has-header-skip-size :type header-skip-size)
     (has-z-coord-file :type z-coord-filename)
     (has-current-slice :type slice)
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (has-uri-result :type uri-result)
     (has-non-functional-properties :value import-raw-volume-goal-non-functional-properties)))

(def-class import-raw-volume-goal-non-functional-properties (non-functional-properties)
    ((has-description "Convert volume data to VME.  Returns URL pointing at new VME.")))
;;; }}}

;;; {{{ export-stl-goal
(def-class export-stl-goal (export-goal) ?goal
    ((has-input-role :value has-url
                     :value has-xml
                     :value has-abs-matrix
                     :value has-stl-file-format
                     :value #_hasAccount)
     (has-input-soap-binding :value (has-url "string")
                             :value (has-xml "string")
                             :value (has-abs-matrix "string")
                             :value (has-stl-file-format "string")
                             :value (#_hasAccount "string"))
     (has-output-role :value has-url-result)
     (has-output-soap-binding :value (has-url-result "xml"))
     (has-url :type lhdl-url)
     (has-xml :type lhdl-xml)
     (has-abs-matrix :type abs-matrix)
     (has-stl-file-format :type stl-file-format)
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (has-url-result :type uri-result)
     (has-non-functional-properties :value export-stl-goal-non-functional-properties)))

(def-class export-stl-goal-non-functional-properties (non-functional-properties)
    ((has-description "Convert VME file to STL.  Returns URL for the STL resource.")))
;;; }}}
;;; {{{ export-vtk-goal
(def-class export-vtk-goal (export-goal) ?goal
    ((has-input-role :value has-url
                     :value has-xml
                     :value has-abs-matrix
                     :value has-vtk-file-format
                     :value #_hasAccount)
     (has-input-soap-binding :value (has-url "string")
                             :value (has-xml "string")
                             :value (has-abs-matrix "string")
                             :value (has-vtk-file-format "string")
                             :value (#_hasAccount "string"))
     (has-output-role :value has-url-result)
     (has-output-soap-binding :value (has-url-result "xml"))
     (has-url :type lhdl-url)
     (has-xml :type lhdl-xml)
     (has-abs-matrix :type abs-matrix)
     (has-vtk-file-format :type vtk-file-format)
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (has-url-result :type uri-result)
     (has-non-functional-properties :value export-vtk-goal-non-functional-properties)))

(def-class export-vtk-goal-non-functional-properties (non-functional-properties)
    ((has-description "Convert VME file to VTK.  Returns URL pointing at VTK resource.")))
;;; }}}
;;; {{{ export-bmp-goal
(def-class export-bmp-goal (export-goal) ?goal
    ((has-input-role :value has-url
                     :value has-xml
                     :value #_hasAccount)
     (has-input-soap-binding :value (has-url "string")
                             :value (has-xml "string")
                             :value (#_hasAccount "string"))
     (has-output-role :value has-url-result)
     (has-output-soap-binding :value (has-url-result "xml"))
     (has-url :type lhdl-url)
     (has-xml :type lhdl-xml)
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (has-url-result :type uri-result)
     (has-non-functional-properties :value export-bmp-goal-non-functional-properties)))

(def-class export-bmp-goal-non-functional-properties (non-functional-properties)
    ((has-description "Convert VME file to BMP.  Returns URL for the BMP resource.")))
;;; }}}
;;; {{{ export-vrml-goal
(def-class export-vrml-goal (export-goal) ?goal
    ((has-input-role :value has-url
                     :value has-xml
                     :value #_hasAccount)
     (has-input-soap-binding :value (has-url "string")
                             :value (has-xml "string")
                             :value (#_hasAccount "string"))
     (has-output-role :value has-url-result)
     (has-output-soap-binding :value (has-url-result "xml"))
     (has-url :type lhdl-url)
     (has-xml :type lhdl-xml)
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (has-url-result :type uri-result)
     (has-non-functional-properties :value export-vrml-goal-non-functional-properties)))

(def-class export-vrml-goal-non-functional-properties (non-functional-properties)
    ((has-description "Convert VME file to VRML.  Returns URL for the VRML resource.")))
;;; }}}

;;; {{{ create-cone-surface-parametric-goal
(def-class create-cone-surface-parametric-goal (create-surface-parametric-goal) ?goal
    ((has-input-role :value has-height
                     :value has-radius
                     :value has-resolution
                     :value has-cap
                     :value #_hasAccount)
     (has-input-soap-binding
      :value (has-height "string")
      :value (has-radius "string")
      :value (has-resolution "string")
      :value (has-cap "string")
      :value (#_hasAccount "string"))
     (has-output-role :value has-url-result)
     (has-output-soap-binding :value (has-url-result "xml"))
     (has-height :type height)
     (has-radius :type radius)
     (has-resolution :type resolution)
     (has-cap :type lhdl-boolean)
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (has-url-result :type uri-result)
     (has-non-functional-properties :value create-cone-surface-parametric-goal-non-functional-properties)))

(def-class create-cone-surface-parametric-goal-non-functional-properties (non-functional-properties)
    ((has-description "Create a cone surface parametrically.  Return URL identifying new file.")))
;;; }}}
;;; {{{ create-cube-surface-parametric-goal
(def-class create-cube-surface-parametric-goal (create-surface-parametric-goal) ?goal
    ((has-input-role :value has-x-length
                     :value has-y-length
                     :value has-z-length
                     :value #_hasAccount)
     (has-input-soap-binding
      :value (has-x-length "string")
      :value (has-y-length "string")
      :value (has-z-length "string")
      :value (#_hasAccount "string"))
     (has-output-role :value has-url-result)
     (has-output-soap-binding :value (has-url-result "xml"))
     (has-x-length :type x-length)
     (has-y-length :type y-length)
     (has-z-length :type z-length)
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (has-url-result :type uri-result)
     (has-non-functional-properties :value create-cube-surface-parametric-goal-non-functional-properties)))

(def-class create-cube-surface-parametric-goal-non-functional-properties (non-functional-properties)
    ((has-description "Create a cube surface parametrically.  Return URL identifying new file.")))
;;; }}}
;;; {{{ create-cylinder-surface-parametric-goal
(def-class create-cylinder-surface-parametric-goal (create-surface-parametric-goal) ?goal
    ((has-input-role :value has-height
                     :value has-radius
                     :value has-resolution
                     :value #_hasAccount)
     (has-input-soap-binding
      :value (has-height "string")
      :value (has-radius "string")
      :value (has-resolution "string")
      :value (#_hasAccount "string"))
     (has-output-role :value has-url-result)
     (has-output-soap-binding :value (has-url-result "xml"))
     (has-height :type height)
     (has-radius :type radius)
     (has-resolution :type resolution)
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (has-url-result :type uri-result)
     (has-non-functional-properties :value create-cylinder-surface-parametric-goal-non-functional-properties)))

(def-class create-cylinder-surface-parametric-goal-non-functional-properties (non-functional-properties)
    ((has-description "Create a cylinder surface parametrically.  Return URL identifying new file.")))
;;; }}}
;;; {{{ create-plane-surface-parametric-goal
(def-class create-plane-surface-parametric-goal (create-surface-parametric-goal) ?goal
    ((has-input-role :value has-x-resolution
                     :value has-y-resolution
                     :value has-origin
                     :value has-point-1
                     :value has-point-2
                     :value #_hasAccount)
     (has-input-soap-binding
      :value (has-x-resolution "string")
      :value (has-y-resolution "string")
      :value (has-origin "string")
      :value (has-point-1 "string")
      :value (has-point-2 "string")
      :value (#_hasAccount "string"))
     (has-output-role :value has-url-result)
     (has-output-soap-binding :value (has-url-result "xml"))
     (has-x-resolution :type x-resolution)
     (has-y-resolution :type y-resolution)
     (has-origin :type origin)
     (has-point-1 :type point-1)
     (has-point-2 :type point-2)
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (has-url-result :type uri-result)
     (has-non-functional-properties :value create-plane-surface-parametric-goal-non-functional-properties)))

(def-class create-plane-surface-parametric-goal-non-functional-properties (non-functional-properties)
    ((has-description "Create a plane surface parametrically.  Return URL identifying new file.")))
;;; }}}
;;; {{{ create-sphere-surface-parametric-goal
(def-class create-sphere-surface-parametric-goal (create-surface-parametric-goal) ?goal
    ((has-input-role :value #_hasAccount
                     :value has-radius
                     :value has-phi-resolution
                     :value has-theta-resolution)
     (has-input-soap-binding
      :value (#_hasAccount "string")
      :value (has-radius "string")
      :value (has-phi-resolution "string")
      :value (has-theta-resolution "string"))
     (has-output-role :value has-url-result)
     (has-output-soap-binding :value (has-url-result "xml"))
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (has-phi-resolution :type phi-resolution)
     (has-theta-resolution :type theta-resolution)
     (has-radius :type radius)
     (has-url-result :type uri-result)
     (has-non-functional-properties :value create-sphere-surface-parametric-goal-non-functional-properties)))

(def-class create-sphere-surface-parametric-goal-non-functional-properties (non-functional-properties)
    ((has-description "Create a sphere surface parametrically.  Return URL identifying new file.")))
;;; }}}

;;; {{{ clean-filter-surface-goal
(def-class clean-filter-surface-goal (filter-goal) ?goal
    ((has-input-role :value has-filename
                     :value has-xml
                     :value #_hasAccount)
     (has-input-soap-binding
      :value (has-binary "string")
      :value (has-xml "string")
      :value (#_hasAccount "string"))
     (has-output-role :value has-url-result)
     (has-output-soap-binding :value (has-url-result "xml"))
     (has-binary :type lhdl-url)
     (has-xml :type lhdl-xml)
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (has-url-result :type uri-result)
     (has-non-functional-properties :value clean-filter-surface-goal-non-functional-properties)))

(def-class clean-filter-surface-goal-non-functional-properties (non-functional-properties)
    ((has-description "Filter VME surface.  Return URL identifying new file.")))
;;; }}}
;;; {{{ connectivity-filter-surface-goal
(def-class connectivity-filter-surface-goal (filter-goal) ?goal
    ((has-input-role :value has-binary
                     :value has-xml
                     :value #_hasAccount)
     (has-input-soap-binding
      :value (has-binary "string")
      :value (has-xml "string")
      :value (#_hasAccount "string"))
     (has-output-role :value has-url-result)
     (has-output-soap-binding :value (has-url-result "xml"))
     (has-binary :type lhdl-url)
     (has-xml :type lhdl-xml)
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (has-url-result :type uri-result)
     (has-non-functional-properties :value connectivity-filter-surface-goal-non-functional-properties)))

(def-class connectivity-filter-surface-goal-non-functional-properties (non-functional-properties)
    ((has-description "Filter VME surface.  Return URL identifying new file.")))
;;; }}}
;;; {{{ generate-normal-filter-surface-goal
(def-class generate-normal-filter-surface-goal (filter-goal) ?goal
    ((has-input-role :value has-filename
                     :value has-xml
                     :value has-angle
                     :value has-flip-normals
                     :value has-edge-split
                     :value #_hasAccount)
     (has-input-soap-binding
      :value (has-filename "string")
      :value (has-xml "string")
      :value (has-angle "string")
      :value (has-flip-normals "string")
      :value (has-edge-split "string")
      :value (#_hasAccount "string"))
     (has-output-role :value has-url-result)
     (has-output-soap-binding :value (has-url-result "xml"))
     (has-filename :type lhdl-url)
     (has-xml :type lhdl-xml)
     (has-angle :type angle)
     (has-flip-normals :type flip-normals)
     (has-edge-split :type edge-split)
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (has-url-result :type uri-result)
     (has-non-functional-properties :value generate-normal-filter-surface-goal-non-functional-properties)))

(def-class generate-normal-filter-surface-goal-non-functional-properties (non-functional-properties)
    ((has-description "Filter VME surface.  Return URL identifying new file.")))
;;; }}}
;;; {{{ smooth-filter-surface-goal
(def-class smooth-filter-surface-goal (filter-goal) ?goal
    ((has-input-role :value has-filename
                     :value has-xml
                     :value has-iterations
                     :value #_hasAccount)
     (has-input-soap-binding
      :value (has-filename "string")
      :value (has-xml "string")
      :value (has-iterations "string")
      :value (#_hasAccount "string"))
     (has-output-role :value has-url-result)
     (has-output-soap-binding :value (has-url-result "xml"))
     (has-filename :type lhdl-url)
     (has-xml :type lhdl-xml)
     (has-iterations :type iterations)
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (has-url-result :type uri-result)
     (has-non-functional-properties :value smooth-filter-surface-goal-non-functional-properties)))

(def-class smooth-filter-surface-goal-non-functional-properties (non-functional-properties)
    ((has-description "Filter VME surface.  Return URL identifying new file.")))
;;; }}}
;;; {{{ strip-filter-surface-goal
(def-class strip-filter-surface-goal (filter-goal) ?goal
    ((has-input-role :value has-filename
                     :value has-xml
                     :value #_hasAccount)
     (has-input-soap-binding
      :value (has-filename "string")
      :value (has-xml "string")
      :value (#_hasAccount "string"))
     (has-output-role :value has-url-result)
     (has-output-soap-binding :value (has-url-result "xml"))
     (has-filename :type lhdl-url)
     (has-xml :type lhdl-xml)
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (has-url-result :type uri-result)
     (has-non-functional-properties :value strip-filter-surface-goal-non-functional-properties)))

(def-class strip-filter-surface-goal-non-functional-properties (non-functional-properties)
    ((has-description "Filter VME surface.  Return URL identifying new file.")))
;;; }}}
;;; {{{ triangulate-filter-surface-goal
(def-class triangulate-filter-surface-goal (filter-goal) ?goal
    ((has-input-role :value has-filename
                     :value has-xml
                     :value #_hasAccount)
     (has-input-soap-binding
      :value (has-filename "string")
      :value (has-xml "string")
      :value (#_hasAccount "string"))
     (has-output-role :value has-url-result)
     (has-output-soap-binding :value (has-url-result "xml"))
     (has-filename :type lhdl-url)
     (has-xml :type lhdl-xml)
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (has-url-result :type uri-result)
     (has-non-functional-properties :value triangulate-filter-surface-goal-non-functional-properties)))

(def-class triangulate-filter-surface-goal-non-functional-properties (non-functional-properties)
    ((has-description "Filter VME surface.  Return URL identifying new file.")))
;;; }}}

;;; {{{ extract-iso-surface-goal
(def-class extract-iso-surface-goal (modify-goal) ?goal
    ((has-input-role :value has-url
                     :value has-xml
                     :value has-contour
                     :value has-auto-lod
                     :value has-optimise
                     :value #_hasAccount)
     (has-input-soap-binding :value (has-url "string")
                             :value (has-xml "string")
                             :value (has-contour "string")
                             :value (has-auto-lod "string")
                             :value (has-optimise "string")
                             :value (#_hasAccount "string"))
     (has-output-role :value has-uri-result)
     (has-output-soap-binding :value (has-uri-result "xml"))
     (has-url :type lhdl-url)
     (has-xml :type lhdl-xml)
     (has-contour :type contour)
     (has-auto-lod :type autolod)
     (has-optimise :type optimise)
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (has-uri-result :type uri-result)
     (has-non-functional-properties :value extract-iso-surface-goal-non-functional-properties)))

(def-class extract-iso-surface-goal-non-functional-properties
    (non-functional-properties)
    ((has-description "Produce a 2D surface VME from a 3D volume VME.
    Returns URL pointing at new VME.")))
;;; }}}
;;; {{{ render-surface-goal
(def-class render-surface-goal (display-goal) ?goal
    ((has-input-role :value has-file-url
                     :value has-xml
                     :value has-background-colour
                     :value has-zoom-factor
                     :value has-camera-azimuth
                     :value has-camera-roll
                     :value has-image-width
                     :value has-image-height
                     :value #_hasAccount)
     (has-input-soap-binding :value (has-file-url "string")
                             :value (has-xml "string")
                             :value (has-background-colour "string")
                             :value (has-zoom-factor "string")
                             :value (has-camera-azimuth "string")
                             :value (has-camera-roll "string")
                             :value (has-image-width "string")
                             :value (has-image-height "string")
                             :value (#_hasAccount "string"))
     (has-output-role :value has-base64-image)
     (has-output-soap-binding :value (has-base64-image "string"))
     (has-file-url :type lhdl-url)
     (has-xml :type lhdl-xml)
     (has-background-colour :type background-colour)
     (has-zoom-factor :type zoom-factor)
     (has-camera-azimuth :type camera-azimuth)
     (has-camera-roll :type camera-roll)
     (has-image-width :type image-width)
     (has-image-height :type image-height)
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (has-base64-image :type base64-image)
     (has-non-functional-properties :value render-surface-goal-non-functional-properties)))

(def-class render-surface-goal-non-functional-properties (non-functional-properties)
    ((has-description "Render VME as a JPEG image (base64 encoded).")))

;;; }}}
;;; {{{ decimate-surface-goal
(def-class decimate-surface-goal (modify-goal) ?goal
    ((has-input-role :value has-file-url
                     :value has-xml
                     :value has-preserve-topology
                     :value has-target-reduction
                     :value #_hasAccount)
     (has-input-soap-binding :value (has-file-url "string")
                             :value (has-xml "string")
                             :value (has-preserve-topology "string")
                             :value (has-target-reduction "string")
                             :value (#_hasAccount "string"))
     (has-output-role :value has-uri-result)
     (has-output-soap-binding :value (has-uri-result "string"))
     (has-file-url :type lhdl-url)
     (has-xml :type lhdl-xml)
     (has-preserve-topology :type preserve-topology)
     (has-target-reduction :type target-reduction)
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (has-uri-result :type uri-result)
     (has-non-functional-properties :value decimate-surface-goal-non-functional-properties)))

(def-class decimate-surface-goal-non-functional-properties (non-functional-properties)
    ((has-description "Decimate surface.")))

;;; }}}
;;; {{{ create-ref-sys-goal
(def-class create-ref-sys-goal (execution-goal) ?goal
    ((has-input-role :value has-name
                     :value has-origin-landmark
                     :value has-point-1-landmark
                     :value has-point-2-landmark
                     :value has-scale
                     :value #_hasAccount)
     (has-input-soap-binding :value (has-name "string")
                             :value (has-origin-landmark "string")
                             :value (has-point-1-landmark "string")
                             :value (has-point-2-landmark "string")
                             :value (has-scale "string")
                             :Value (#_hasAccount "string"))
     (has-output-role :value has-url-result)
     (has-output-soap-binding :value (has-url-result "xml"))
     (has-name :type lhdl-name)
     (has-origin-landmark :type origin)
     (has-point-1-landmark :type point-1)
     (has-point-2-landmark :type point-2)
     (has-scale :type scale)
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (has-url-result :type uri-result)
     (has-non-functional-properties :value create-ref-sys-goal-non-functional-properties)))

(def-class create-ref-sys-goal-non-functional-properties (non-functional-properties)
    ((has-description "Create a VMEReference System which is an
    orthogonal reference system, defined by three VMELandmarks (one
    for the origin and two to define the plane of
    refSys). VMELandmarks are created through point (X, Y, Z)
    parameters provided. The created VME files are stored on the
    server. The service returns a link to the created files. In case
    of error, an error string is returned.")))
;;; }}}

;;; {{{ measure-volume-goal
(def-class measure-volume-goal (measure-goal) ?goal
    ((has-input-role :value has-url
                     :value has-xml
                     :value #_hasAccount)
     (has-input-soap-binding :value (has-url "string")
                             :value (has-xml "string")
                             :value (#_hasAccount "string"))
     (has-output-role :value has-measurement)
     (has-output-soap-binding :value (has-url-result "xml"))
     (has-url :type lhdl-url)
     (has-xml :type lhdl-xml)
     (#_hasAccount :type #_domain:BiomedTownAccount)
     (has-measurement :type volume-measurement)
     (has-non-functional-properties :value measure-volume-goal-non-functional-properties)))

(def-class measure-volume-goal-non-functional-properties (non-functional-properties)
    ((has-description "Compute a volume of a VMESurface. The
    VMESurface? is read from server storage. The service returns the
    measurement results including surface volume, surface area, and
    normalized shape index (N.S.I). In case of error, return soap
    exception.")))

;;; }}}

;;; {{{ import-vtk-web-service
(DEF-CLASS import-vtk-mediator-non-functional-properties (non-functional-properties)
    nil)

(DEF-CLASS import-vtk-mediator (wg-mediator) ?mediator
    ((HAS-SOURCE-COMPONENT :VALUE import-vtk-GOAL)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE import-vtk-mediator-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS IMPORT-VTK-WEB-SERVICE (WEB-SERVICE) ?WEB-SERVICE
    ((HAS-CAPABILITY :VALUE IMPORT-VTK-WEB-SERVICE-CAPABILITY)
     (HAS-INTERFACE :VALUE IMPORT-VTK-WEB-SERVICE-INTERFACE)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE IMPORT-VTK-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS IMPORT-VTK-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES
    (NON-FUNCTIONAL-PROPERTIES)
    NIL)

(DEF-CLASS IMPORT-VTK-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS IMPORT-VTK-WEB-SERVICE-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE IMPORT-VTK-MEDIATOR)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             IMPORT-VTK-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS IMPORT-VTK-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS IMPORT-VTK-WEB-SERVICE-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING :VALUE ((grounded-to-soap1.1)))
            (has-earthing :value import-vtk-web-service-grounding)))

(def-instance import-vtk-web-service-grounding soap-grounding
  ((has-url :value "http://lhdl.cis.beds.ac.uk/ExecWSV2/ImportVTKService/ImportVTKService.asmx")
   (has-soap-action "http://lhdl.cis.beds.ac.uk/ExecWSV2/ImportVTKService/importVTK")
   (has-soap-method "importVTK")
   (has-target-namespace "http://lhdl.cis.beds.ac.uk/ExecWSV2/ImportVTKService/")
   (has-connection-read-timeout 120)))

(DEF-CLASS IMPORT-VTK-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN        
    (PROBLEM-SOLVING-PATTERN)
    ((HAS-BODY :VALUE nil)))  

(DEF-CLASS IMPORT-VTK-WEB-SERVICE-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             IMPORT-VTK-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS IMPORT-VTK-WEB-SERVICE-INTERFACE (INTERFACE) ?INTERFACE
    ((HAS-CHOREOGRAPHY :VALUE IMPORT-VTK-WEB-SERVICE-INTERFACE-CHOREOGRAPHY)
     (HAS-ORCHESTRATION :VALUE IMPORT-VTK-WEB-SERVICE-INTERFACE-ORCHESTRATION)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE
      IMPORT-VTK-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(def-class import-vtk-web-service-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value import-vtk-web-service-interface)
     (has-web-service-host :value "lhdl.cis.beds.ac.uk")
     (has-web-service-port :value 80)))

;;; }}}
;;; {{{ stl-importer-web-service
(DEF-CLASS import-stl-mediator-non-functional-properties (non-functional-properties)
    nil)

(DEF-CLASS import-stl-mediator (wg-mediator) ?mediator
    ((HAS-SOURCE-COMPONENT :VALUE import-stl-GOAL)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE import-stl-mediator-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS IMPORT-STL-WEB-SERVICE (WEB-SERVICE) ?WEB-SERVICE
    ((HAS-CAPABILITY :VALUE IMPORT-STL-WEB-SERVICE-CAPABILITY)
     (HAS-INTERFACE :VALUE IMPORT-STL-WEB-SERVICE-INTERFACE)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE IMPORT-STL-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS IMPORT-STL-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES
    (NON-FUNCTIONAL-PROPERTIES)
    NIL)

(DEF-CLASS IMPORT-STL-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS IMPORT-STL-WEB-SERVICE-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE IMPORT-STL-MEDIATOR)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             IMPORT-STL-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS IMPORT-STL-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS IMPORT-STL-WEB-SERVICE-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING :VALUE ((grounded-to-soap1.1)))
            (has-earthing :value import-stl-web-service-grounding)))

(def-instance import-stl-web-service-grounding soap-grounding
  ((has-url "http://lhdl.cis.beds.ac.uk/ExecWSV2/ImportSTLService/ImportSTLService.asmx")
   (has-soap-action "http://lhdl.cis.beds.ac.uk/ExecWSV2/ImportSTLService/importSTL")
   (has-soap-method "importSTL")
   (has-target-namespace "http://lhdl.cis.beds.ac.uk/ExecWSV2/ImportSTLService/")
   (has-connection-read-timeout 120)))


(DEF-CLASS IMPORT-STL-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
    (PROBLEM-SOLVING-PATTERN)
    ((HAS-BODY :VALUE nil)))

(DEF-CLASS IMPORT-STL-WEB-SERVICE-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             IMPORT-STL-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS IMPORT-STL-WEB-SERVICE-INTERFACE (INTERFACE) ?INTERFACE
    ((HAS-CHOREOGRAPHY :VALUE IMPORT-STL-WEB-SERVICE-INTERFACE-CHOREOGRAPHY)
     (HAS-ORCHESTRATION :VALUE IMPORT-STL-WEB-SERVICE-INTERFACE-ORCHESTRATION)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE
      IMPORT-STL-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(def-class import-stl-web-service-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value import-stl-web-service-interface)
     (has-web-service-host :value "lhdl.cis.beds.ac.uk")
     (has-web-service-port :value 80)))

;;; }}}
;;; {{{ vrml-importer-web-service
(DEF-CLASS import-vrml-mediator-non-functional-properties (non-functional-properties)
    nil)

(DEF-CLASS import-vrml-mediator (wg-mediator) ?mediator
    ((HAS-SOURCE-COMPONENT :VALUE import-vrml-GOAL)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE import-vrml-mediator-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS IMPORT-VRML-WEB-SERVICE (WEB-SERVICE) ?WEB-SERVICE
    ((HAS-CAPABILITY :VALUE IMPORT-VRML-WEB-SERVICE-CAPABILITY)
     (HAS-INTERFACE :VALUE IMPORT-VRML-WEB-SERVICE-INTERFACE)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE IMPORT-VRML-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS IMPORT-VRML-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES
    (NON-FUNCTIONAL-PROPERTIES)
    NIL)

(DEF-CLASS IMPORT-VRML-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS IMPORT-VRML-WEB-SERVICE-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE IMPORT-VRML-MEDIATOR)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             IMPORT-VRML-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS IMPORT-VRML-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS IMPORT-VRML-WEB-SERVICE-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING :VALUE ((grounded-to-soap1.1)))
            (has-earthing :value import-vrml-web-service-grounding)))

(def-instance import-vrml-web-service-grounding soap-grounding
  ((has-url "http://lhdl.cis.beds.ac.uk/ExecWSV2/ImportVRMLService/ImportVRMLService.asmx")
   (has-soap-action "http://lhdl.cis.beds.ac.uk/ExecWSV2/ImportVRMLService/importVRML")
   (has-soap-method "importVRML")
   (has-target-namespace "http://lhdl.cis.beds.ac.uk/ExecWSV2/ImportVRMLService/")
   (has-connection-read-timeout 120)))

(DEF-CLASS IMPORT-VRML-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN        
    (PROBLEM-SOLVING-PATTERN)
    ((HAS-BODY :VALUE nil)))  

(DEF-CLASS IMPORT-VRML-WEB-SERVICE-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             IMPORT-VRML-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS IMPORT-VRML-WEB-SERVICE-INTERFACE (INTERFACE) ?INTERFACE
    ((HAS-CHOREOGRAPHY :VALUE IMPORT-VRML-WEB-SERVICE-INTERFACE-CHOREOGRAPHY)
     (HAS-ORCHESTRATION :VALUE IMPORT-VRML-WEB-SERVICE-INTERFACE-ORCHESTRATION)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE
      IMPORT-VRML-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(def-class import-vrml-web-service-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value import-vrml-web-service-interface)
     (has-web-service-host :value "lhdl.cis.beds.ac.uk")
     (has-web-service-port :value 80)))

;;; }}}
;;; {{{ image-sequence-importer-web-service
(DEF-CLASS import-image-sequence-mediator-non-functional-properties (non-functional-properties)
    nil)

(DEF-CLASS import-image-sequence-mediator (wg-mediator) ?mediator
    ((HAS-SOURCE-COMPONENT :VALUE import-image-sequence-GOAL)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE import-image-sequence-mediator-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS IMPORT-IMAGE-SEQUENCE-WEB-SERVICE (WEB-SERVICE) ?WEB-SERVICE
    ((HAS-CAPABILITY :VALUE IMPORT-IMAGE-SEQUENCE-WEB-SERVICE-CAPABILITY)
     (HAS-INTERFACE :VALUE IMPORT-IMAGE-SEQUENCE-WEB-SERVICE-INTERFACE)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE IMPORT-IMAGE-SEQUENCE-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS IMPORT-IMAGE-SEQUENCE-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES
    (NON-FUNCTIONAL-PROPERTIES)
    NIL)

(DEF-CLASS IMPORT-IMAGE-SEQUENCE-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS IMPORT-IMAGE-SEQUENCE-WEB-SERVICE-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE IMPORT-IMAGE-SEQUENCE-MEDIATOR)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             IMPORT-IMAGE-SEQUENCE-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS IMPORT-IMAGE-SEQUENCE-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS IMPORT-IMAGE-SEQUENCE-WEB-SERVICE-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((has-grounding :value ((grounded-to-soap1.1)))
            (has-earthing :value import-image-sequence-web-service-grounding)))

(def-instance import-image-sequence-web-service-grounding soap-grounding
  ((has-url "http://lhdl.cis.beds.ac.uk/ExecWSV2/ImportImageService/ImportImageService.asmx")
   (has-soap-action "http://lhdl.cis.beds.ac.uk/ExecWSV2/ImportImageService/importImageAsSequence")
   (has-soap-method "importImageAsSequence")
   (has-target-namespace "http://lhdl.cis.beds.ac.uk/ExecWSV2/ImportImageService/")
   (has-connection-read-timeout 120)))

(DEF-CLASS IMPORT-IMAGE-SEQUENCE-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN        
    (PROBLEM-SOLVING-PATTERN)
    ((HAS-BODY :VALUE nil)))  

(DEF-CLASS IMPORT-IMAGE-SEQUENCE-WEB-SERVICE-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             IMPORT-IMAGE-SEQUENCE-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS IMPORT-IMAGE-SEQUENCE-WEB-SERVICE-INTERFACE (INTERFACE) ?INTERFACE
    ((HAS-CHOREOGRAPHY :VALUE IMPORT-IMAGE-SEQUENCE-WEB-SERVICE-INTERFACE-CHOREOGRAPHY)
     (HAS-ORCHESTRATION :VALUE IMPORT-IMAGE-SEQUENCE-WEB-SERVICE-INTERFACE-ORCHESTRATION)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE
      IMPORT-IMAGE-SEQUENCE-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(def-class import-image-sequence-web-service-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value import-image-sequence-web-service-interface)
     (has-web-service-host :value "lhdl.cis.beds.ac.uk")
     (has-web-service-port :value 80)))

;;; }}}
;;; {{{ import-landmark-cloud-web-service
(DEF-CLASS import-landmark-cloud-mediator-non-functional-properties (non-functional-properties)
    nil)

(DEF-CLASS import-landmark-cloud-mediator (wg-mediator) ?mediator
    ((HAS-SOURCE-COMPONENT :VALUE import-landmark-cloud-GOAL)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE import-landmark-cloud-mediator-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS IMPORT-LANDMARK-CLOUD-WEB-SERVICE (WEB-SERVICE) ?WEB-SERVICE
    ((HAS-CAPABILITY :VALUE IMPORT-LANDMARK-CLOUD-WEB-SERVICE-CAPABILITY)
     (HAS-INTERFACE :VALUE IMPORT-LANDMARK-CLOUD-WEB-SERVICE-INTERFACE)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE IMPORT-LANDMARK-CLOUD-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS IMPORT-LANDMARK-CLOUD-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES
    (NON-FUNCTIONAL-PROPERTIES)
    NIL)

(DEF-CLASS IMPORT-LANDMARK-CLOUD-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS IMPORT-LANDMARK-CLOUD-WEB-SERVICE-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE IMPORT-LANDMARK-CLOUD-MEDIATOR)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             IMPORT-LANDMARK-CLOUD-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS IMPORT-LANDMARK-CLOUD-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS IMPORT-LANDMARK-CLOUD-WEB-SERVICE-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING :VALUE ((grounded-to-soap1.1)))
            (has-earthing :value import-landmark-cloud-web-service-grounding)))

(def-instance import-landmark-cloud-web-service-grounding soap-grounding
  ((has-url "http://lhdl.cis.beds.ac.uk/ExecWSV2/ImportLandmarkCloudService/ImportLandmarkCloudService.asmx")
   (has-soap-action "http://lhdl.cis.beds.ac.uk/ExecWSV2/ImportLandmarkCloudService/importLandmarkCloud")
   (has-soap-method "importLandmarkCloud")
   (has-target-namespace "http://lhdl.cis.beds.ac.uk/ExecWSV2/ImportLandmarkCloudService/")))

(DEF-CLASS IMPORT-LANDMARK-CLOUD-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN        
    (PROBLEM-SOLVING-PATTERN)
    ((HAS-BODY :VALUE nil)))  

(DEF-CLASS IMPORT-LANDMARK-CLOUD-WEB-SERVICE-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             IMPORT-LANDMARK-CLOUD-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS IMPORT-LANDMARK-CLOUD-WEB-SERVICE-INTERFACE (INTERFACE) ?INTERFACE
    ((HAS-CHOREOGRAPHY :VALUE IMPORT-LANDMARK-CLOUD-WEB-SERVICE-INTERFACE-CHOREOGRAPHY)
     (HAS-ORCHESTRATION :VALUE IMPORT-LANDMARK-CLOUD-WEB-SERVICE-INTERFACE-ORCHESTRATION)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE
      IMPORT-LANDMARK-CLOUD-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(def-class import-landmark-cloud-web-service-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value import-landmark-cloud-web-service-interface)
     (has-web-service-host :value "lhdl.cis.beds.ac.uk")
     (has-web-service-port :value 80)))

;;; }}}
;;; {{{ import-dicom-web-service
(DEF-CLASS import-dicom-mediator-non-functional-properties (non-functional-properties)
    nil)

(DEF-CLASS import-dicom-mediator (wg-mediator) ?mediator
    ((HAS-SOURCE-COMPONENT :VALUE import-dicom-GOAL)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE import-dicom-mediator-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS IMPORT-DICOM-WEB-SERVICE (WEB-SERVICE) ?WEB-SERVICE
    ((HAS-CAPABILITY :VALUE IMPORT-DICOM-WEB-SERVICE-CAPABILITY)
     (HAS-INTERFACE :VALUE IMPORT-DICOM-WEB-SERVICE-INTERFACE)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE IMPORT-DICOM-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS IMPORT-DICOM-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES
    (NON-FUNCTIONAL-PROPERTIES)
    NIL)

(DEF-CLASS IMPORT-DICOM-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS IMPORT-DICOM-WEB-SERVICE-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE IMPORT-DICOM-MEDIATOR)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             IMPORT-DICOM-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS IMPORT-DICOM-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS IMPORT-DICOM-WEB-SERVICE-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING :VALUE ((grounded-to-soap1.1)))
            (has-earthing :value import-dicom-web-service-grounding)))

(def-instance import-dicom-web-service-grounding soap-grounding
  ((has-url "http://lhdl.cis.beds.ac.uk/ExecWSV2/ImportDICOMService/ImportDICOMService.asmx")
   (has-soap-action "http://lhdl.cis.beds.ac.uk/ExecWSV2/ImportDICOMService/importDICOM")
   (has-soap-method "importDICOM")
   (has-target-namespace "http://lhdl.cis.beds.ac.uk/ExecWSV2/ImportDICOMService/")
   ;; This can be a slow operation.
   (has-connection-read-timeout 180)))

(DEF-CLASS IMPORT-DICOM-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
    (PROBLEM-SOLVING-PATTERN)
  ((HAS-BODY :VALUE nil)))

(DEF-CLASS IMPORT-DICOM-WEB-SERVICE-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             IMPORT-DICOM-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS IMPORT-DICOM-WEB-SERVICE-INTERFACE (INTERFACE) ?INTERFACE
    ((HAS-CHOREOGRAPHY :VALUE IMPORT-DICOM-WEB-SERVICE-INTERFACE-CHOREOGRAPHY)
     (HAS-ORCHESTRATION :VALUE IMPORT-DICOM-WEB-SERVICE-INTERFACE-ORCHESTRATION)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE
      IMPORT-DICOM-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(def-class import-dicom-web-service-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value import-dicom-web-service-interface)
     (has-web-service-host :value "lhdl.cis.beds.ac.uk")
     (has-web-service-port :value 80)))

;;; }}}
;;; {{{ import-raw-volume-web-service
(DEF-CLASS import-raw-volume-mediator-non-functional-properties (non-functional-properties)
    nil)

(DEF-CLASS import-raw-volume-mediator (wg-mediator) ?mediator
    ((HAS-SOURCE-COMPONENT :VALUE import-raw-volume-GOAL)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE import-raw-volume-mediator-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS IMPORT-RAW-VOLUME-WEB-SERVICE (WEB-SERVICE) ?WEB-SERVICE
    ((HAS-CAPABILITY :VALUE IMPORT-RAW-VOLUME-WEB-SERVICE-CAPABILITY)
     (HAS-INTERFACE :VALUE IMPORT-RAW-VOLUME-WEB-SERVICE-INTERFACE)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE IMPORT-RAW-VOLUME-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS IMPORT-RAW-VOLUME-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES
    (NON-FUNCTIONAL-PROPERTIES)
    NIL)

(DEF-CLASS IMPORT-RAW-VOLUME-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS IMPORT-RAW-VOLUME-WEB-SERVICE-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE IMPORT-RAW-VOLUME-MEDIATOR)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             IMPORT-RAW-VOLUME-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS IMPORT-RAW-VOLUME-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS IMPORT-RAW-VOLUME-WEB-SERVICE-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING :VALUE ((grounded-to-soap1.1)))
            (has-earthing :value import-raw-volume-web-service-grounding)))

(def-instance import-raw-volume-web-service-grounding soap-grounding
  ((has-url "http://lhdl.cis.beds.ac.uk/ExecWSV2/ImportRawVolumeService/ImportRawVolumeService.asmx")
   (has-soap-action "http://lhdl.cis.beds.ac.uk/ExecWSV2/ImportRawVolumeService/importRawVolume")
   (has-soap-method "importRawVolume")
   (has-target-namespace "http://lhdl.cis.beds.ac.uk/ExecWSV2/ImportRawVolumeService/")
   (has-connection-read-timeout 180)))

(DEF-CLASS IMPORT-RAW-VOLUME-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
    (PROBLEM-SOLVING-PATTERN)
  ((HAS-BODY :VALUE nil)))

(DEF-CLASS IMPORT-RAW-VOLUME-WEB-SERVICE-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             IMPORT-RAW-VOLUME-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS IMPORT-RAW-VOLUME-WEB-SERVICE-INTERFACE (INTERFACE) ?INTERFACE
    ((HAS-CHOREOGRAPHY :VALUE IMPORT-RAW-VOLUME-WEB-SERVICE-INTERFACE-CHOREOGRAPHY)
     (HAS-ORCHESTRATION :VALUE IMPORT-RAW-VOLUME-WEB-SERVICE-INTERFACE-ORCHESTRATION)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE
      IMPORT-RAW-VOLUME-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(def-class import-raw-volume-web-service-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value import-raw-volume-web-service-interface)
     (has-web-service-host :value "lhdl.cis.beds.ac.uk")
     (has-web-service-port :value 80)))

;;; }}}

;;; {{{ extract-iso-surface-web-service
(DEF-CLASS extract-iso-surface-mediator-non-functional-properties (non-functional-properties)
    nil)

(DEF-CLASS extract-iso-surface-mediator (wg-mediator) ?mediator
    ((HAS-SOURCE-COMPONENT :VALUE extract-iso-surface-goal)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE extract-iso-surface-mediator-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS EXTRACT-ISO-SURFACE-WEB-SERVICE (WEB-SERVICE) ?WEB-SERVICE
    ((HAS-CAPABILITY :VALUE EXTRACT-ISO-SURFACE-WEB-SERVICE-CAPABILITY)
     (HAS-INTERFACE :VALUE EXTRACT-ISO-SURFACE-WEB-SERVICE-INTERFACE)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE EXTRACT-ISO-SURFACE-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS EXTRACT-ISO-SURFACE-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES
    (NON-FUNCTIONAL-PROPERTIES)
    NIL)

(DEF-CLASS EXTRACT-ISO-SURFACE-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS EXTRACT-ISO-SURFACE-WEB-SERVICE-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE EXTRACT-ISO-SURFACE-MEDIATOR)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             EXTRACT-ISO-SURFACE-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS EXTRACT-ISO-SURFACE-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS EXTRACT-ISO-SURFACE-WEB-SERVICE-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING :VALUE ((grounded-to-soap1.1)))
            (has-earthing :value extract-iso-surface-web-service-grounding)))

(def-instance extract-iso-surface-web-service-grounding soap-grounding
  ((has-url "http://lhdl.cis.beds.ac.uk/ExecWSV2/ExtractIsosurfaceService/ExtractIsosurfaceService.asmx")
   (has-soap-action "http://lhdl.cis.beds.ac.uk/ExecWSV2/ExtractIsosurfaceService/extractIsosurface")
   (has-soap-method "extractIsosurface")
   (has-connection-read-timeout 120)
   (has-target-namespace "http://lhdl.cis.beds.ac.uk/ExecWSV2/ExtractIsosurfaceService/")))

(DEF-CLASS EXTRACT-ISO-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN        
    (PROBLEM-SOLVING-PATTERN)
    ((HAS-BODY :VALUE nil)))  

(DEF-CLASS EXTRACT-ISO-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             EXTRACT-ISO-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS EXTRACT-ISO-SURFACE-WEB-SERVICE-INTERFACE (INTERFACE) ?INTERFACE
    ((HAS-CHOREOGRAPHY :VALUE EXTRACT-ISO-SURFACE-WEB-SERVICE-INTERFACE-CHOREOGRAPHY)
     (HAS-ORCHESTRATION :VALUE EXTRACT-ISO-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE
      EXTRACT-ISO-SURFACE-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(def-class extract-iso-surface-web-service-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value extract-iso-surface-web-service-interface)
     (has-web-service-host :value "lhdl.cis.beds.ac.uk")
     (has-web-service-port :value 80)))

;;; }}}
;;; {{{ render-surface-web-service
(def-class render-surface-mediator-non-functional-properties (non-functional-properties)
    nil)

(DEF-CLASS render-surface-mediator (wg-mediator) ?mediator
    ((HAS-SOURCE-COMPONENT :VALUE render-surface-goal)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE render-surface-mediator-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS RENDER-SURFACE-WEB-SERVICE (WEB-SERVICE) ?WEB-SERVICE
    ((HAS-CAPABILITY :VALUE RENDER-SURFACE-WEB-SERVICE-CAPABILITY)
     (HAS-INTERFACE :VALUE RENDER-SURFACE-WEB-SERVICE-INTERFACE)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE RENDER-SURFACE-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS RENDER-SURFACE-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES
    (NON-FUNCTIONAL-PROPERTIES)
    NIL)

(DEF-CLASS RENDER-SURFACE-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS RENDER-SURFACE-WEB-SERVICE-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE RENDER-SURFACE-MEDIATOR)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             RENDER-SURFACE-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS RENDER-SURFACE-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS RENDER-SURFACE-WEB-SERVICE-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING :VALUE ((grounded-to-soap1.1)))
            (has-earthing :value render-surface-web-service-grounding)))

(def-instance render-surface-web-service-grounding soap-grounding
  ((has-url "http://lhdl.cis.beds.ac.uk/ExecWSV2/RenderSurfaceService/RenderSurfaceService.asmx")
   (has-soap-action "http://lhdl.cis.beds.ac.uk/ExecWSV2/RenderSurfaceService/renderSurface")
   (has-soap-method "renderSurface")
   (has-target-namespace "http://lhdl.cis.beds.ac.uk/ExecWSV2/RenderSurfaceService/")))

(DEF-CLASS RENDER-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN        
    (PROBLEM-SOLVING-PATTERN)
    ((HAS-BODY :VALUE nil)))  

(DEF-CLASS RENDER-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             RENDER-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS RENDER-SURFACE-WEB-SERVICE-INTERFACE (INTERFACE) ?INTERFACE
    ((HAS-CHOREOGRAPHY :VALUE RENDER-SURFACE-WEB-SERVICE-INTERFACE-CHOREOGRAPHY)
     (HAS-ORCHESTRATION :VALUE RENDER-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE
      RENDER-SURFACE-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(def-class render-surface-web-service-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value render-surface-web-service-interface)
     (has-web-service-host :value "lhdl.cis.beds.ac.uk")
     (has-web-service-port :value 80)))

;;; }}}
;;; {{{ decimate-surface-web-service
(def-class decimate-surface-mediator-non-functional-properties (non-functional-properties)
    nil)

(DEF-CLASS decimate-surface-mediator (wg-mediator) ?mediator
    ((HAS-SOURCE-COMPONENT :VALUE decimate-surface-goal)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE decimate-surface-mediator-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS DECIMATE-SURFACE-WEB-SERVICE (WEB-SERVICE) ?WEB-SERVICE
    ((HAS-CAPABILITY :VALUE DECIMATE-SURFACE-WEB-SERVICE-CAPABILITY)
     (HAS-INTERFACE :VALUE DECIMATE-SURFACE-WEB-SERVICE-INTERFACE)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE DECIMATE-SURFACE-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS DECIMATE-SURFACE-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES
    (NON-FUNCTIONAL-PROPERTIES)
    NIL)

(DEF-CLASS DECIMATE-SURFACE-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS DECIMATE-SURFACE-WEB-SERVICE-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE DECIMATE-SURFACE-MEDIATOR)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             DECIMATE-SURFACE-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS DECIMATE-SURFACE-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS DECIMATE-SURFACE-WEB-SERVICE-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING :VALUE ((grounded-to-soap1.1)))
            (has-earthing :value decimate-surface-web-service-grounding)))

(def-instance decimate-surface-web-service-grounding soap-grounding
  ((has-url "http://lhdl.cis.beds.ac.uk/ExecWSV2/DecimateSurfaceService/DecimateSurfaceService.asmx")
   (has-soap-action "http://lhdl.cis.beds.ac.uk/ExecWSV2/DecimateSurfaceService/decimateSurface")
   (has-soap-method "decimateSurface")
   (has-target-namespace "http://lhdl.cis.beds.ac.uk/ExecWSV2/DecimateSurfaceService/")))

(DEF-CLASS DECIMATE-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
    (PROBLEM-SOLVING-PATTERN)
    ((HAS-BODY :VALUE nil)))

(DEF-CLASS DECIMATE-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             DECIMATE-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS DECIMATE-SURFACE-WEB-SERVICE-INTERFACE (INTERFACE) ?INTERFACE
    ((HAS-CHOREOGRAPHY :VALUE DECIMATE-SURFACE-WEB-SERVICE-INTERFACE-CHOREOGRAPHY)
     (HAS-ORCHESTRATION :VALUE DECIMATE-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE
      DECIMATE-SURFACE-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(def-class decimate-surface-web-service-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value decimate-surface-web-service-interface)
     (has-web-service-host :value "lhdl.cis.beds.ac.uk")
     (has-web-service-port :value 80)))

;;; }}}

;;; {{{ create-cone-surface-parametric-web-service
(def-class create-cone-surface-parametric-mediator-non-functional-properties (non-functional-properties)
    nil)

(DEF-CLASS create-cone-surface-parametric-mediator (wg-mediator) ?mediator
    ((HAS-SOURCE-COMPONENT :VALUE create-cone-surface-parametric-goal)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE create-cone-surface-parametric-mediator-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS CREATE-CONE-SURFACE-PARAMETRIC-WEB-SERVICE (WEB-SERVICE) ?WEB-SERVICE
    ((HAS-CAPABILITY :VALUE CREATE-CONE-SURFACE-PARAMETRIC-WEB-SERVICE-CAPABILITY)
     (HAS-INTERFACE :VALUE CREATE-CONE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE CREATE-CONE-SURFACE-PARAMETRIC-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS CREATE-CONE-SURFACE-PARAMETRIC-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES
    (NON-FUNCTIONAL-PROPERTIES)
    NIL)

(DEF-CLASS CREATE-CONE-SURFACE-PARAMETRIC-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS CREATE-CONE-SURFACE-PARAMETRIC-WEB-SERVICE-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE CREATE-CONE-SURFACE-PARAMETRIC-MEDIATOR)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             CREATE-CONE-SURFACE-PARAMETRIC-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS CREATE-CONE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS CREATE-CONE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING :VALUE ((grounded-to-soap1.1)))
            (has-earthing :value create-cone-surface-parametric-web-service-grounding)))

(def-instance create-cone-surface-parametric-web-service-grounding soap-grounding
  ((has-url "http://lhdl.cis.beds.ac.uk/ExecWSV2/CreateSurfaceParametricService/CreateSurfaceParametricService.asmx")
   (has-soap-action "http://lhdl.cis.beds.ac.uk/ExecWSV2/CreateSurfaceParametricService/createConeSurfaceParametric")
   (has-soap-method "createConeSurfaceParametric")
   (has-target-namespace "http://lhdl.cis.beds.ac.uk/ExecWSV2/CreateSurfaceParametricService/")))

(DEF-CLASS CREATE-CONE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
    (PROBLEM-SOLVING-PATTERN)
    ((HAS-BODY :VALUE nil)))

(DEF-CLASS CREATE-CONE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             CREATE-CONE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS CREATE-CONE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE (INTERFACE) ?INTERFACE
    ((HAS-CHOREOGRAPHY :VALUE CREATE-CONE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-CHOREOGRAPHY)
     (HAS-ORCHESTRATION :VALUE CREATE-CONE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-ORCHESTRATION)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE
      CREATE-CONE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(def-class create-cone-surface-parametric-web-service-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value create-cone-surface-parametric-web-service-interface)
     (has-web-service-host :value "lhdl.cis.beds.ac.uk")
     (has-web-service-port :value 80)))

;;; }}}
;;; {{{ create-cube-surface-parametric-web-service
(def-class create-cube-surface-parametric-mediator-non-functional-properties (non-functional-properties)
    nil)

(DEF-CLASS create-cube-surface-parametric-mediator (wg-mediator) ?mediator
    ((HAS-SOURCE-COMPONENT :VALUE create-cube-surface-parametric-goal)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE create-cube-surface-parametric-mediator-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS CREATE-CUBE-SURFACE-PARAMETRIC-WEB-SERVICE (WEB-SERVICE) ?WEB-SERVICE
    ((HAS-CAPABILITY :VALUE CREATE-CUBE-SURFACE-PARAMETRIC-WEB-SERVICE-CAPABILITY)
     (HAS-INTERFACE :VALUE CREATE-CUBE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE CREATE-CUBE-SURFACE-PARAMETRIC-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS CREATE-CUBE-SURFACE-PARAMETRIC-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES
    (NON-FUNCTIONAL-PROPERTIES)
    NIL)

(DEF-CLASS CREATE-CUBE-SURFACE-PARAMETRIC-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS CREATE-CUBE-SURFACE-PARAMETRIC-WEB-SERVICE-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE CREATE-CUBE-SURFACE-PARAMETRIC-MEDIATOR)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             CREATE-CUBE-SURFACE-PARAMETRIC-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS CREATE-CUBE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS CREATE-CUBE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING :VALUE ((grounded-to-soap1.1)))
            (has-earthing :value create-cube-surface-parametric-web-service-grounding)))

(def-instance create-cube-surface-parametric-web-service-grounding soap-grounding
  ((has-url "http://lhdl.cis.beds.ac.uk/ExecWSV2/CreateSurfaceParametricService/CreateSurfaceParametricService.asmx")
   (has-soap-action "http://lhdl.cis.beds.ac.uk/ExecWSV2/CreateSurfaceParametricService/createCubeSurfaceParametric")
   (has-soap-method "createCubeSurfaceParametric")
   (has-target-namespace "http://lhdl.cis.beds.ac.uk/ExecWSV2/CreateSurfaceParametricService/")
   (has-connection-read-timeout 120)))

(DEF-CLASS CREATE-CUBE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
    (PROBLEM-SOLVING-PATTERN)
    ((HAS-BODY :VALUE nil)))

(DEF-CLASS CREATE-CUBE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             CREATE-CUBE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS CREATE-CUBE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE (INTERFACE) ?INTERFACE
    ((HAS-CHOREOGRAPHY :VALUE CREATE-CUBE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-CHOREOGRAPHY)
     (HAS-ORCHESTRATION :VALUE CREATE-CUBE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-ORCHESTRATION)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE
      CREATE-CUBE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(def-class create-cube-surface-parametric-web-service-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value create-cube-surface-parametric-web-service-interface)
     (has-web-service-host :value "lhdl.cis.beds.ac.uk")
     (has-web-service-port :value 80)))

;;; }}}
;;; {{{ create-cylinder-surface-parametric-web-service
(def-class create-cylinder-surface-parametric-mediator-non-functional-properties (non-functional-properties)
    nil)

(DEF-CLASS create-cylinder-surface-parametric-mediator (wg-mediator) ?mediator
    ((HAS-SOURCE-COMPONENT :VALUE create-cylinder-surface-parametric-goal)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE create-cylinder-surface-parametric-mediator-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS CREATE-CYLINDER-SURFACE-PARAMETRIC-WEB-SERVICE (WEB-SERVICE) ?WEB-SERVICE
    ((HAS-CAPABILITY :VALUE CREATE-CYLINDER-SURFACE-PARAMETRIC-WEB-SERVICE-CAPABILITY)
     (HAS-INTERFACE :VALUE CREATE-CYLINDER-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE CREATE-CYLINDER-SURFACE-PARAMETRIC-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS CREATE-CYLINDER-SURFACE-PARAMETRIC-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES
    (NON-FUNCTIONAL-PROPERTIES)
    NIL)

(DEF-CLASS CREATE-CYLINDER-SURFACE-PARAMETRIC-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS CREATE-CYLINDER-SURFACE-PARAMETRIC-WEB-SERVICE-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE CREATE-CYLINDER-SURFACE-PARAMETRIC-MEDIATOR)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             CREATE-CYLINDER-SURFACE-PARAMETRIC-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS CREATE-CYLINDER-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS CREATE-CYLINDER-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING :VALUE ((grounded-to-soap1.1)))
            (has-earthing :value create-cylinder-surface-parametric-web-service-grounding)))

(def-instance create-cylinder-surface-parametric-web-service-grounding soap-grounding
  ((has-url "http://lhdl.cis.beds.ac.uk/ExecWSV2/CreateSurfaceParametricService/CreateSurfaceParametricService.asmx")
   (has-soap-action "http://lhdl.cis.beds.ac.uk/ExecWSV2/CreateSurfaceParametricService/createCylinderSurfaceParametric")
   (has-soap-method "createCylinderSurfaceParametric")
   (has-connection-read-timeout 120)
   (has-target-namespace "http://lhdl.cis.beds.ac.uk/ExecWSV2/CreateSurfaceParametricService/")))

(DEF-CLASS CREATE-CYLINDER-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
    (PROBLEM-SOLVING-PATTERN)
    ((HAS-BODY :VALUE nil)))

(DEF-CLASS CREATE-CYLINDER-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             CREATE-CYLINDER-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS CREATE-CYLINDER-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE (INTERFACE) ?INTERFACE
    ((HAS-CHOREOGRAPHY :VALUE CREATE-CYLINDER-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-CHOREOGRAPHY)
     (HAS-ORCHESTRATION :VALUE CREATE-CYLINDER-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-ORCHESTRATION)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE
      CREATE-CYLINDER-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(def-class create-cylinder-surface-parametric-web-service-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value create-cylinder-surface-parametric-web-service-interface)
     (has-web-service-host :value "lhdl.cis.beds.ac.uk")
     (has-web-service-port :value 80)))

;;; }}}
;;; {{{ create-plane-surface-parametric-web-service
(def-class create-plane-surface-parametric-mediator-non-functional-properties (non-functional-properties)
    nil)

(DEF-CLASS create-plane-surface-parametric-mediator (wg-mediator) ?mediator
    ((HAS-SOURCE-COMPONENT :VALUE create-plane-surface-parametric-goal)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE create-plane-surface-parametric-mediator-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS CREATE-PLANE-SURFACE-PARAMETRIC-WEB-SERVICE (WEB-SERVICE) ?WEB-SERVICE
    ((HAS-CAPABILITY :VALUE CREATE-PLANE-SURFACE-PARAMETRIC-WEB-SERVICE-CAPABILITY)
     (HAS-INTERFACE :VALUE CREATE-PLANE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE CREATE-PLANE-SURFACE-PARAMETRIC-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS CREATE-PLANE-SURFACE-PARAMETRIC-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES
    (NON-FUNCTIONAL-PROPERTIES)
    NIL)

(DEF-CLASS CREATE-PLANE-SURFACE-PARAMETRIC-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS CREATE-PLANE-SURFACE-PARAMETRIC-WEB-SERVICE-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE CREATE-PLANE-SURFACE-PARAMETRIC-MEDIATOR)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             CREATE-PLANE-SURFACE-PARAMETRIC-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS CREATE-PLANE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS CREATE-PLANE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING :VALUE ((grounded-to-soap1.1)))
            (has-earthing :value create-plane-surface-parametric-web-service-grounding)))

(def-instance create-plane-surface-parametric-web-service-grounding soap-grounding
  ((has-url "http://lhdl.cis.beds.ac.uk/ExecWSV2/CreateSurfaceParametricService/CreateSurfaceParametricService.asmx")
   (has-soap-action "http://lhdl.cis.beds.ac.uk/ExecWSV2/CreateSurfaceParametricService/createPlaneSurfaceParametric")
   (has-soap-method "createPlaneSurfaceParametric")
   (has-connection-read-timeout 120)
   (has-target-namespace "http://lhdl.cis.beds.ac.uk/ExecWSV2/CreateSurfaceParametricService/")))

(DEF-CLASS CREATE-PLANE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
    (PROBLEM-SOLVING-PATTERN)
    ((HAS-BODY :VALUE nil)))

(DEF-CLASS CREATE-PLANE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             CREATE-PLANE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS CREATE-PLANE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE (INTERFACE) ?INTERFACE
    ((HAS-CHOREOGRAPHY :VALUE CREATE-PLANE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-CHOREOGRAPHY)
     (HAS-ORCHESTRATION :VALUE CREATE-PLANE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-ORCHESTRATION)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE
      CREATE-PLANE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(def-class create-plane-surface-parametric-web-service-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value create-plane-surface-parametric-web-service-interface)
     (has-web-service-host :value "lhdl.cis.beds.ac.uk")
     (has-web-service-port :value 80)))

;;; }}}
;;; {{{ create-sphere-surface-parametric-web-service
(def-class create-sphere-surface-parametric-mediator-non-functional-properties (non-functional-properties)
    nil)

(DEF-CLASS create-sphere-surface-parametric-mediator (wg-mediator) ?mediator
    ((HAS-SOURCE-COMPONENT :VALUE create-sphere-surface-parametric-goal)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE create-sphere-surface-parametric-mediator-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS CREATE-SPHERE-SURFACE-PARAMETRIC-WEB-SERVICE (WEB-SERVICE) ?WEB-SERVICE
    ((HAS-CAPABILITY :VALUE CREATE-SPHERE-SURFACE-PARAMETRIC-WEB-SERVICE-CAPABILITY)
     (HAS-INTERFACE :VALUE CREATE-SPHERE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE CREATE-SPHERE-SURFACE-PARAMETRIC-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS CREATE-SPHERE-SURFACE-PARAMETRIC-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES
    (NON-FUNCTIONAL-PROPERTIES)
    NIL)

(DEF-CLASS CREATE-SPHERE-SURFACE-PARAMETRIC-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS CREATE-SPHERE-SURFACE-PARAMETRIC-WEB-SERVICE-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE CREATE-SPHERE-SURFACE-PARAMETRIC-MEDIATOR)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             CREATE-SPHERE-SURFACE-PARAMETRIC-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS CREATE-SPHERE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS CREATE-SPHERE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING :VALUE ((grounded-to-soap1.1)))
            (has-earthing :value create-sphere-surface-parametric-web-service-grounding)))

(def-instance create-sphere-surface-parametric-web-service-grounding soap-grounding
  ((has-url "http://lhdl.cis.beds.ac.uk/ExecWSV2/CreateSurfaceParametricService/CreateSurfaceParametricService.asmx")
   (has-soap-action "http://lhdl.cis.beds.ac.uk/ExecWSV2/CreateSurfaceParametricService/createSphereSurfaceParametric")
   (has-soap-method "createSphereSurfaceParametric")
   (has-connection-read-timeout 120)
   (has-target-namespace "http://lhdl.cis.beds.ac.uk/ExecWSV2/CreateSurfaceParametricService/")))

(DEF-CLASS CREATE-SPHERE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
    (PROBLEM-SOLVING-PATTERN)
    ((HAS-BODY :VALUE nil)))

(DEF-CLASS CREATE-SPHERE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             CREATE-SPHERE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS CREATE-SPHERE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE (INTERFACE) ?INTERFACE
    ((HAS-CHOREOGRAPHY :VALUE CREATE-SPHERE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-CHOREOGRAPHY)
     (HAS-ORCHESTRATION :VALUE CREATE-SPHERE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-ORCHESTRATION)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE
      CREATE-SPHERE-SURFACE-PARAMETRIC-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(def-class create-sphere-surface-parametric-web-service-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value create-sphere-surface-parametric-web-service-interface)
     (has-web-service-host :value "lhdl.cis.beds.ac.uk")
     (has-web-service-port :value 80)))

;;; }}}

;;; {{{ export-vtk-web-service
(def-class export-vtk-mediator-non-functional-properties (non-functional-properties)
    nil)

(DEF-CLASS export-vtk-mediator (wg-mediator) ?mediator
    ((HAS-SOURCE-COMPONENT :VALUE export-vtk-goal)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE export-vtk-mediator-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS EXPORT-VTK-WEB-SERVICE (WEB-SERVICE) ?WEB-SERVICE
    ((HAS-CAPABILITY :VALUE EXPORT-VTK-WEB-SERVICE-CAPABILITY)
     (HAS-INTERFACE :VALUE EXPORT-VTK-WEB-SERVICE-INTERFACE)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE EXPORT-VTK-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS EXPORT-VTK-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES
    (NON-FUNCTIONAL-PROPERTIES)
    NIL)

(DEF-CLASS EXPORT-VTK-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS EXPORT-VTK-WEB-SERVICE-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE EXPORT-VTK-MEDIATOR)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             EXPORT-VTK-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS EXPORT-VTK-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS EXPORT-VTK-WEB-SERVICE-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING :VALUE ((grounded-to-soap1.1)))
            (has-earthing :value export-vtk-web-service-grounding)))

(def-instance export-vtk-web-service-grounding soap-grounding
  ((has-url "http://lhdl.cis.beds.ac.uk/ExecWSV2/ExportVTKService/ExportVTKService.asmx")
   (has-soap-action "http://lhdl.cis.beds.ac.uk/ExecWSV2/ExportVTKService/exportVTK")
   (has-soap-method "exportVTK")
   (has-target-namespace "http://lhdl.cis.beds.ac.uk/ExecWSV2/ExportVTKService/")))

(DEF-CLASS EXPORT-VTK-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
    (PROBLEM-SOLVING-PATTERN)
    ((HAS-BODY :VALUE nil)))

(DEF-CLASS EXPORT-VTK-WEB-SERVICE-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             EXPORT-VTK-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS EXPORT-VTK-WEB-SERVICE-INTERFACE (INTERFACE) ?INTERFACE
    ((HAS-CHOREOGRAPHY :VALUE EXPORT-VTK-WEB-SERVICE-INTERFACE-CHOREOGRAPHY)
     (HAS-ORCHESTRATION :VALUE EXPORT-VTK-WEB-SERVICE-INTERFACE-ORCHESTRATION)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE
      EXPORT-VTK-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(def-class export-vtk-web-service-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value export-vtk-web-service-interface)
     (has-web-service-host :value "lhdl.cis.beds.ac.uk")
     (has-web-service-port :value 80)))

;;; }}}
;;; {{{ export-stl-web-service
(def-class export-stl-mediator-non-functional-properties (non-functional-properties)
    nil)

(DEF-CLASS export-stl-mediator (wg-mediator) ?mediator
    ((HAS-SOURCE-COMPONENT :VALUE export-stl-goal)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE export-stl-mediator-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS EXPORT-STL-WEB-SERVICE (WEB-SERVICE) ?WEB-SERVICE
    ((HAS-CAPABILITY :VALUE EXPORT-STL-WEB-SERVICE-CAPABILITY)
     (HAS-INTERFACE :VALUE EXPORT-STL-WEB-SERVICE-INTERFACE)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE EXPORT-STL-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS EXPORT-STL-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES
    (NON-FUNCTIONAL-PROPERTIES)
    NIL)

(DEF-CLASS EXPORT-STL-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS EXPORT-STL-WEB-SERVICE-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE EXPORT-STL-MEDIATOR)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             EXPORT-STL-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS EXPORT-STL-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS EXPORT-STL-WEB-SERVICE-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING :VALUE ((grounded-to-soap1.1)))
            (has-earthing :value export-stl-web-service-grounding)))

(def-instance export-stl-web-service-grounding soap-grounding
  ((has-url "http://lhdl.cis.beds.ac.uk/ExecWSV2/ExportSTLService/ExportSTLService.asmx")
   (has-soap-action "http://lhdl.cis.beds.ac.uk/ExecWSV2/ExportSTLService/exportSTL")
   (has-soap-method "exportSTL")
   (has-target-namespace "http://lhdl.cis.beds.ac.uk/ExecWSV2/ExportSTLService/")))

(DEF-CLASS EXPORT-STL-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
    (PROBLEM-SOLVING-PATTERN)
    ((HAS-BODY :VALUE nil)))

(DEF-CLASS EXPORT-STL-WEB-SERVICE-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             EXPORT-STL-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS EXPORT-STL-WEB-SERVICE-INTERFACE (INTERFACE) ?INTERFACE
    ((HAS-CHOREOGRAPHY :VALUE EXPORT-STL-WEB-SERVICE-INTERFACE-CHOREOGRAPHY)
     (HAS-ORCHESTRATION :VALUE EXPORT-STL-WEB-SERVICE-INTERFACE-ORCHESTRATION)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE
      EXPORT-STL-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(def-class export-stl-web-service-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value export-stl-web-service-interface)
     (has-web-service-host :value "lhdl.cis.beds.ac.uk")
     (has-web-service-port :value 80)))

;;; }}}
;;; {{{ export-bmp-web-service
(def-class export-bmp-mediator-non-functional-properties (non-functional-properties)
    nil)

(DEF-CLASS export-bmp-mediator (wg-mediator) ?mediator
    ((HAS-SOURCE-COMPONENT :VALUE export-bmp-goal)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE export-bmp-mediator-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS EXPORT-BMP-WEB-SERVICE (WEB-SERVICE) ?WEB-SERVICE
    ((HAS-CAPABILITY :VALUE EXPORT-BMP-WEB-SERVICE-CAPABILITY)
     (HAS-INTERFACE :VALUE EXPORT-BMP-WEB-SERVICE-INTERFACE)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE EXPORT-BMP-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS EXPORT-BMP-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES
    (NON-FUNCTIONAL-PROPERTIES)
    NIL)

(DEF-CLASS EXPORT-BMP-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS EXPORT-BMP-WEB-SERVICE-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE EXPORT-BMP-MEDIATOR)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             EXPORT-BMP-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS EXPORT-BMP-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS EXPORT-BMP-WEB-SERVICE-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING :VALUE ((grounded-to-soap1.1)))
            (has-earthing :value export-bmp-web-service-grounding)))

(def-instance export-bmp-web-service-grounding soap-grounding
  ((has-url "http://lhdl.cis.beds.ac.uk/ExecWSV2/ExportBmpService/ExportBmpService.asmx")
   (has-soap-action "http://lhdl.cis.beds.ac.uk/ExecWSV2/ExportBmpService/exportBmp")
   (has-soap-method "exportBmp")
   (has-target-namespace "http://lhdl.cis.beds.ac.uk/ExecWSV2/ExportBmpService/")
   (has-connection-read-timeout 120)))

(DEF-CLASS EXPORT-BMP-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
    (PROBLEM-SOLVING-PATTERN)
    ((HAS-BODY :VALUE nil)))

(DEF-CLASS EXPORT-BMP-WEB-SERVICE-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             EXPORT-BMP-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS EXPORT-BMP-WEB-SERVICE-INTERFACE (INTERFACE) ?INTERFACE
    ((HAS-CHOREOGRAPHY :VALUE EXPORT-BMP-WEB-SERVICE-INTERFACE-CHOREOGRAPHY)
     (HAS-ORCHESTRATION :VALUE EXPORT-BMP-WEB-SERVICE-INTERFACE-ORCHESTRATION)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE
      EXPORT-BMP-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(def-class export-bmp-web-service-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value export-bmp-web-service-interface)
     (has-web-service-host :value "lhdl.cis.beds.ac.uk")
     (has-web-service-port :value 80)))

;;; }}}
;;; {{{ export-vrml-web-service
(def-class export-vrml-mediator-non-functional-properties (non-functional-properties)
    nil)

(DEF-CLASS export-vrml-mediator (wg-mediator) ?mediator
    ((HAS-SOURCE-COMPONENT :VALUE export-vrml-goal)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE export-vrml-mediator-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS EXPORT-VRML-WEB-SERVICE (WEB-SERVICE) ?WEB-SERVICE
    ((HAS-CAPABILITY :VALUE EXPORT-VRML-WEB-SERVICE-CAPABILITY)
     (HAS-INTERFACE :VALUE EXPORT-VRML-WEB-SERVICE-INTERFACE)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE EXPORT-VRML-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS EXPORT-VRML-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES
    (NON-FUNCTIONAL-PROPERTIES)
    NIL)

(DEF-CLASS EXPORT-VRML-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS EXPORT-VRML-WEB-SERVICE-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE EXPORT-VRML-MEDIATOR)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             EXPORT-VRML-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS EXPORT-VRML-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS EXPORT-VRML-WEB-SERVICE-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING :VALUE ((grounded-to-soap1.1)))
            (has-earthing :value export-vrml-web-service-grounding)))

(def-instance export-vrml-web-service-grounding soap-grounding
  ((has-url "http://lhdl.cis.beds.ac.uk/ExecWSV2/ExportVRMLService/ExportVRMLService.asmx")
   (has-soap-action "http://lhdl.cis.beds.ac.uk/ExecWSV2/ExportVRMLService/exportVRML")
   (has-soap-method "exportVRML")
   (has-target-namespace "http://lhdl.cis.beds.ac.uk/ExecWSV2/ExportVRMLService/")))

(DEF-CLASS EXPORT-VRML-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
    (PROBLEM-SOLVING-PATTERN)
    ((HAS-BODY :VALUE nil)))

(DEF-CLASS EXPORT-VRML-WEB-SERVICE-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             EXPORT-VRML-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS EXPORT-VRML-WEB-SERVICE-INTERFACE (INTERFACE) ?INTERFACE
    ((HAS-CHOREOGRAPHY :VALUE EXPORT-VRML-WEB-SERVICE-INTERFACE-CHOREOGRAPHY)
     (HAS-ORCHESTRATION :VALUE EXPORT-VRML-WEB-SERVICE-INTERFACE-ORCHESTRATION)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE
      EXPORT-VRML-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(def-class export-vrml-web-service-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value export-vrml-web-service-interface)
     (has-web-service-host :value "lhdl.cis.beds.ac.uk")
     (has-web-service-port :value 80)))

;;; }}}

;;; {{{ measure-volume-web-service
(def-class measure-volume-mediator-non-functional-properties (non-functional-properties)
    nil)

(DEF-CLASS measure-volume-mediator (wg-mediator) ?mediator
    ((HAS-SOURCE-COMPONENT :VALUE measure-volume-goal)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE measure-volume-mediator-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS MEASURE-VOLUME-WEB-SERVICE (WEB-SERVICE) ?WEB-SERVICE
    ((HAS-CAPABILITY :VALUE MEASURE-VOLUME-WEB-SERVICE-CAPABILITY)
     (HAS-INTERFACE :VALUE MEASURE-VOLUME-WEB-SERVICE-INTERFACE)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE MEASURE-VOLUME-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS MEASURE-VOLUME-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES
    (NON-FUNCTIONAL-PROPERTIES)
    NIL)

(DEF-CLASS MEASURE-VOLUME-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS MEASURE-VOLUME-WEB-SERVICE-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE MEASURE-VOLUME-MEDIATOR)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             MEASURE-VOLUME-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS MEASURE-VOLUME-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS MEASURE-VOLUME-WEB-SERVICE-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING :VALUE ((grounded-to-soap1.1)))
            (has-earthing :value measure-volume-web-service-grounding)))

(def-instance measure-volume-web-service-grounding soap-grounding
  ((has-url "http://lhdl.cis.beds.ac.uk/ExecWSV2/MeasureVolumeService/MeasureVolumeService.asmx")
   (has-soap-action "http://lhdl.cis.beds.ac.uk/ExecWSV2/MeasureVolumeService/measureVolume")
   (has-soap-method "measureVolume")
   (has-target-namespace "http://lhdl.cis.beds.ac.uk/ExecWSV2/MeasureVolumeService/")))

(DEF-CLASS MEASURE-VOLUME-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
    (PROBLEM-SOLVING-PATTERN)
    ((HAS-BODY :VALUE nil)))

(DEF-CLASS MEASURE-VOLUME-WEB-SERVICE-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             MEASURE-VOLUME-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS MEASURE-VOLUME-WEB-SERVICE-INTERFACE (INTERFACE) ?INTERFACE
    ((HAS-CHOREOGRAPHY :VALUE MEASURE-VOLUME-WEB-SERVICE-INTERFACE-CHOREOGRAPHY)
     (HAS-ORCHESTRATION :VALUE MEASURE-VOLUME-WEB-SERVICE-INTERFACE-ORCHESTRATION)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE
      MEASURE-VOLUME-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(def-class measure-volume-web-service-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value measure-volume-web-service-interface)
     (has-web-service-host :value "lhdl.cis.beds.ac.uk")
     (has-web-service-port :value 80)))

;;; }}}

;;; {{{ clean-filter-surface-web-service
(def-class clean-filter-surface-mediator-non-functional-properties (non-functional-properties)
    nil)

(DEF-CLASS clean-filter-surface-mediator (wg-mediator) ?mediator
    ((HAS-SOURCE-COMPONENT :VALUE clean-filter-surface-goal)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE clean-filter-surface-mediator-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS CLEAN-FILTER-SURFACE-WEB-SERVICE (WEB-SERVICE) ?WEB-SERVICE
    ((HAS-CAPABILITY :VALUE CLEAN-FILTER-SURFACE-WEB-SERVICE-CAPABILITY)
     (HAS-INTERFACE :VALUE CLEAN-FILTER-SURFACE-WEB-SERVICE-INTERFACE)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE CLEAN-FILTER-SURFACE-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS CLEAN-FILTER-SURFACE-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES
    (NON-FUNCTIONAL-PROPERTIES)
    NIL)

(DEF-CLASS CLEAN-FILTER-SURFACE-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS CLEAN-FILTER-SURFACE-WEB-SERVICE-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE CLEAN-FILTER-SURFACE-MEDIATOR)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             CLEAN-FILTER-SURFACE-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS CLEAN-FILTER-SURFACE-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS CLEAN-FILTER-SURFACE-WEB-SERVICE-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING :VALUE ((grounded-to-soap1.1)))
            (has-earthing :value clean-filter-surface-web-service-grounding)))

(def-instance clean-filter-surface-web-service-grounding soap-grounding
  ((has-url "http://lhdl.cis.beds.ac.uk/ExecWSV2/FilterSurfaceService/FilterSurfaceService.asmx")
   (has-soap-action "http://lhdl.cis.beds.ac.uk/ExecWSV2/FilterSurfaceService/cleanFilterSurface")
   (has-soap-method "cleanFilterSurface")
   (has-target-namespace "http://lhdl.cis.beds.ac.uk/ExecWSV2/FilterSurfaceService/")))

(DEF-CLASS CLEAN-FILTER-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
    (PROBLEM-SOLVING-PATTERN)
    ((HAS-BODY :VALUE nil)))

(DEF-CLASS CLEAN-FILTER-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             CLEAN-FILTER-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS CLEAN-FILTER-SURFACE-WEB-SERVICE-INTERFACE (INTERFACE) ?INTERFACE
    ((HAS-CHOREOGRAPHY :VALUE CLEAN-FILTER-SURFACE-WEB-SERVICE-INTERFACE-CHOREOGRAPHY)
     (HAS-ORCHESTRATION :VALUE CLEAN-FILTER-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE
      CLEAN-FILTER-SURFACE-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(def-class clean-filter-surface-web-service-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value clean-filter-surface-web-service-interface)
     (has-web-service-host :value "lhdl.cis.beds.ac.uk")
     (has-web-service-port :value 80)))

;;; }}}
;;; {{{ connectivity-filter-surface-web-service
(def-class connectivity-filter-surface-mediator-non-functional-properties (non-functional-properties)
    nil)

(DEF-CLASS connectivity-filter-surface-mediator (wg-mediator) ?mediator
    ((HAS-SOURCE-COMPONENT :VALUE connectivity-filter-surface-goal)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE connectivity-filter-surface-mediator-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS CONNECTIVITY-FILTER-SURFACE-WEB-SERVICE (WEB-SERVICE) ?WEB-SERVICE
    ((HAS-CAPABILITY :VALUE CONNECTIVITY-FILTER-SURFACE-WEB-SERVICE-CAPABILITY)
     (HAS-INTERFACE :VALUE CONNECTIVITY-FILTER-SURFACE-WEB-SERVICE-INTERFACE)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE CONNECTIVITY-FILTER-SURFACE-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS CONNECTIVITY-FILTER-SURFACE-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES
    (NON-FUNCTIONAL-PROPERTIES)
    NIL)

(DEF-CLASS CONNECTIVITY-FILTER-SURFACE-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS CONNECTIVITY-FILTER-SURFACE-WEB-SERVICE-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE CONNECTIVITY-FILTER-SURFACE-MEDIATOR)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             CONNECTIVITY-FILTER-SURFACE-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS CONNECTIVITY-FILTER-SURFACE-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS CONNECTIVITY-FILTER-SURFACE-WEB-SERVICE-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING :VALUE ((grounded-to-soap1.1)))
            (has-earthing :value connectivity-filter-surface-web-service-grounding)))

(def-instance connectivity-filter-surface-web-service-grounding soap-grounding
  ((has-url "http://lhdl.cis.beds.ac.uk/ExecWSV2/FilterSurfaceService/FilterSurfaceService.asmx")
   (has-soap-action "http://lhdl.cis.beds.ac.uk/ExecWSV2/FilterSurfaceService/connectivityFilterSurface")
   (has-soap-method "connectivityFilterSurface")
   (has-connection-read-timeout 120)
   (has-target-namespace "http://lhdl.cis.beds.ac.uk/ExecWSV2/FilterSurfaceService/")))

(DEF-CLASS CONNECTIVITY-FILTER-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
    (PROBLEM-SOLVING-PATTERN)
    ((HAS-BODY :VALUE nil)))

(DEF-CLASS CONNECTIVITY-FILTER-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             CONNECTIVITY-FILTER-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS CONNECTIVITY-FILTER-SURFACE-WEB-SERVICE-INTERFACE (INTERFACE) ?INTERFACE
    ((HAS-CHOREOGRAPHY :VALUE CONNECTIVITY-FILTER-SURFACE-WEB-SERVICE-INTERFACE-CHOREOGRAPHY)
     (HAS-ORCHESTRATION :VALUE CONNECTIVITY-FILTER-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE
      CONNECTIVITY-FILTER-SURFACE-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(def-class connectivity-filter-surface-web-service-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value connectivity-filter-surface-web-service-interface)
     (has-web-service-host :value "lhdl.cis.beds.ac.uk")
     (has-web-service-port :value 80)))
;;; }}}
;;; {{{ generate-normal-filter-surface-web-service
(def-class generate-normal-filter-surface-mediator-non-functional-properties (non-functional-properties)
    nil)

(DEF-CLASS generate-normal-filter-surface-mediator (wg-mediator) ?mediator
    ((HAS-SOURCE-COMPONENT :VALUE generate-normal-filter-surface-goal)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE generate-normal-filter-surface-mediator-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS GENERATE-NORMAL-FILTER-SURFACE-WEB-SERVICE (WEB-SERVICE) ?WEB-SERVICE
    ((HAS-CAPABILITY :VALUE GENERATE-NORMAL-FILTER-SURFACE-WEB-SERVICE-CAPABILITY)
     (HAS-INTERFACE :VALUE GENERATE-NORMAL-FILTER-SURFACE-WEB-SERVICE-INTERFACE)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE GENERATE-NORMAL-FILTER-SURFACE-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS GENERATE-NORMAL-FILTER-SURFACE-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES
    (NON-FUNCTIONAL-PROPERTIES)
    NIL)

(DEF-CLASS GENERATE-NORMAL-FILTER-SURFACE-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS GENERATE-NORMAL-FILTER-SURFACE-WEB-SERVICE-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE GENERATE-NORMAL-FILTER-SURFACE-MEDIATOR)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             GENERATE-NORMAL-FILTER-SURFACE-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS GENERATE-NORMAL-FILTER-SURFACE-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS GENERATE-NORMAL-FILTER-SURFACE-WEB-SERVICE-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING :VALUE ((grounded-to-soap1.1)))
            (has-earthing :value generate-normal-filter-surface-web-service-grounding)))

(def-instance generate-normal-filter-surface-web-service-grounding soap-grounding
  ((has-url "http://lhdl.cis.beds.ac.uk/ExecWSV2/FilterSurfaceService/FilterSurfaceService.asmx")
   (has-soap-action "http://lhdl.cis.beds.ac.uk/ExecWSV2/FilterSurfaceService/generateNormalFilterSurface")
   (has-soap-method "generateNormalFilterSurface")
   (has-target-namespace "http://lhdl.cis.beds.ac.uk/ExecWSV2/FilterSurfaceService/")))

(DEF-CLASS GENERATE-NORMAL-FILTER-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
    (PROBLEM-SOLVING-PATTERN)
    ((HAS-BODY :VALUE nil)))

(DEF-CLASS GENERATE-NORMAL-FILTER-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             GENERATE-NORMAL-FILTER-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS GENERATE-NORMAL-FILTER-SURFACE-WEB-SERVICE-INTERFACE (INTERFACE) ?INTERFACE
    ((HAS-CHOREOGRAPHY :VALUE GENERATE-NORMAL-FILTER-SURFACE-WEB-SERVICE-INTERFACE-CHOREOGRAPHY)
     (HAS-ORCHESTRATION :VALUE GENERATE-NORMAL-FILTER-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE
      GENERATE-NORMAL-FILTER-SURFACE-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(def-class generate-normal-filter-surface-web-service-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value generate-normal-filter-surface-web-service-interface)
     (has-web-service-host :value "lhdl.cis.beds.ac.uk")
     (has-web-service-port :value 80)))
;;; }}}
;;; {{{ smooth-filter-surface-web-service
(def-class smooth-filter-surface-mediator-non-functional-properties (non-functional-properties)
    nil)

(DEF-CLASS smooth-filter-surface-mediator (wg-mediator) ?mediator
    ((HAS-SOURCE-COMPONENT :VALUE smooth-filter-surface-goal)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE smooth-filter-surface-mediator-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS SMOOTH-FILTER-SURFACE-WEB-SERVICE (WEB-SERVICE) ?WEB-SERVICE
    ((HAS-CAPABILITY :VALUE SMOOTH-FILTER-SURFACE-WEB-SERVICE-CAPABILITY)
     (HAS-INTERFACE :VALUE SMOOTH-FILTER-SURFACE-WEB-SERVICE-INTERFACE)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE SMOOTH-FILTER-SURFACE-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS SMOOTH-FILTER-SURFACE-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES
    (NON-FUNCTIONAL-PROPERTIES)
    NIL)

(DEF-CLASS SMOOTH-FILTER-SURFACE-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS SMOOTH-FILTER-SURFACE-WEB-SERVICE-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE SMOOTH-FILTER-SURFACE-MEDIATOR)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             SMOOTH-FILTER-SURFACE-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS SMOOTH-FILTER-SURFACE-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS SMOOTH-FILTER-SURFACE-WEB-SERVICE-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING :VALUE ((grounded-to-soap1.1)))
            (has-earthing :value smooth-filter-surface-web-service-grounding)))

(def-instance smooth-filter-surface-web-service-grounding soap-grounding
  ((has-url "http://lhdl.cis.beds.ac.uk/ExecWSV2/FilterSurfaceService/FilterSurfaceService.asmx")
   (has-soap-action "http://lhdl.cis.beds.ac.uk/ExecWSV2/FilterSurfaceService/smoothSurface")
   (has-soap-method "smoothSurface")
   (has-target-namespace "http://lhdl.cis.beds.ac.uk/ExecWSV2/FilterSurfaceService/")))

(DEF-CLASS SMOOTH-FILTER-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
    (PROBLEM-SOLVING-PATTERN)
    ((HAS-BODY :VALUE nil)))

(DEF-CLASS SMOOTH-FILTER-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             SMOOTH-FILTER-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS SMOOTH-FILTER-SURFACE-WEB-SERVICE-INTERFACE (INTERFACE) ?INTERFACE
    ((HAS-CHOREOGRAPHY :VALUE SMOOTH-FILTER-SURFACE-WEB-SERVICE-INTERFACE-CHOREOGRAPHY)
     (HAS-ORCHESTRATION :VALUE SMOOTH-FILTER-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE
      SMOOTH-FILTER-SURFACE-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(def-class smooth-filter-surface-web-service-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value smooth-filter-surface-web-service-interface)
     (has-web-service-host :value "lhdl.cis.beds.ac.uk")
     (has-web-service-port :value 80)))

;;; }}}
;;; {{{ strip-filter-surface-web-service
(def-class strip-filter-surface-mediator-non-functional-properties (non-functional-properties)
    nil)

(DEF-CLASS strip-filter-surface-mediator (wg-mediator) ?mediator
    ((HAS-SOURCE-COMPONENT :VALUE strip-filter-surface-goal)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE strip-filter-surface-mediator-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS STRIP-FILTER-SURFACE-WEB-SERVICE (WEB-SERVICE) ?WEB-SERVICE
    ((HAS-CAPABILITY :VALUE STRIP-FILTER-SURFACE-WEB-SERVICE-CAPABILITY)
     (HAS-INTERFACE :VALUE STRIP-FILTER-SURFACE-WEB-SERVICE-INTERFACE)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE STRIP-FILTER-SURFACE-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS STRIP-FILTER-SURFACE-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES
    (NON-FUNCTIONAL-PROPERTIES)
    NIL)

(DEF-CLASS STRIP-FILTER-SURFACE-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS STRIP-FILTER-SURFACE-WEB-SERVICE-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE STRIP-FILTER-SURFACE-MEDIATOR)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             STRIP-FILTER-SURFACE-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS STRIP-FILTER-SURFACE-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS STRIP-FILTER-SURFACE-WEB-SERVICE-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING :VALUE ((grounded-to-soap1.1)))
            (has-earthing :value strip-filter-surface-web-service-grounding)))

(def-instance strip-filter-surface-web-service-grounding soap-grounding
  ((has-url "http://lhdl.cis.beds.ac.uk/ExecWSV2/FilterSurfaceService/FilterSurfaceService.asmx")
   (has-soap-action "http://lhdl.cis.beds.ac.uk/ExecWSV2/FilterSurfaceService/stripFilterSurface")
   (has-soap-method "stripFilterSurface")
   (has-target-namespace "http://lhdl.cis.beds.ac.uk/ExecWSV2/FilterSurfaceService/")))

(DEF-CLASS STRIP-FILTER-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
    (PROBLEM-SOLVING-PATTERN)
    ((HAS-BODY :VALUE nil)))

(DEF-CLASS STRIP-FILTER-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             STRIP-FILTER-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS STRIP-FILTER-SURFACE-WEB-SERVICE-INTERFACE (INTERFACE) ?INTERFACE
    ((HAS-CHOREOGRAPHY :VALUE STRIP-FILTER-SURFACE-WEB-SERVICE-INTERFACE-CHOREOGRAPHY)
     (HAS-ORCHESTRATION :VALUE STRIP-FILTER-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE
      STRIP-FILTER-SURFACE-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(def-class strip-filter-surface-web-service-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value strip-filter-surface-web-service-interface)
     (has-web-service-host :value "lhdl.cis.beds.ac.uk")
     (has-web-service-port :value 80)))

;;; }}}
;;; {{{ triangulate-filter-surface-web-service
(def-class triangulate-filter-surface-mediator-non-functional-properties (non-functional-properties)
    nil)

(DEF-CLASS triangulate-filter-surface-mediator (wg-mediator) ?mediator
    ((HAS-SOURCE-COMPONENT :VALUE triangulate-filter-surface-goal)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE triangulate-filter-surface-mediator-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS TRIANGULATE-FILTER-SURFACE-WEB-SERVICE (WEB-SERVICE) ?WEB-SERVICE
    ((HAS-CAPABILITY :VALUE TRIANGULATE-FILTER-SURFACE-WEB-SERVICE-CAPABILITY)
     (HAS-INTERFACE :VALUE TRIANGULATE-FILTER-SURFACE-WEB-SERVICE-INTERFACE)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE TRIANGULATE-FILTER-SURFACE-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS TRIANGULATE-FILTER-SURFACE-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES
    (NON-FUNCTIONAL-PROPERTIES)
    NIL)

(DEF-CLASS TRIANGULATE-FILTER-SURFACE-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS TRIANGULATE-FILTER-SURFACE-WEB-SERVICE-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE TRIANGULATE-FILTER-SURFACE-MEDIATOR)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             TRIANGULATE-FILTER-SURFACE-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS TRIANGULATE-FILTER-SURFACE-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS TRIANGULATE-FILTER-SURFACE-WEB-SERVICE-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING :VALUE ((grounded-to-soap1.1)))
            (has-earthing :value triangulate-filter-surface-web-service-grounding)))

(def-instance triangulate-filter-surface-web-service-grounding soap-grounding
  ((has-url "http://lhdl.cis.beds.ac.uk/ExecWSV2/FilterSurfaceService/FilterSurfaceService.asmx")
   (has-soap-action "http://lhdl.cis.beds.ac.uk/ExecWSV2/FilterSurfaceService/triangulateFilterSurface")
   (has-soap-method "triangulateFilterSurface")
   (has-target-namespace "http://lhdl.cis.beds.ac.uk/ExecWSV2/FilterSurfaceService/")))

(DEF-CLASS TRIANGULATE-FILTER-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
    (PROBLEM-SOLVING-PATTERN)
    ((HAS-BODY :VALUE nil)))

(DEF-CLASS TRIANGULATE-FILTER-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             TRIANGULATE-FILTER-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS TRIANGULATE-FILTER-SURFACE-WEB-SERVICE-INTERFACE (INTERFACE) ?INTERFACE
    ((HAS-CHOREOGRAPHY :VALUE TRIANGULATE-FILTER-SURFACE-WEB-SERVICE-INTERFACE-CHOREOGRAPHY)
     (HAS-ORCHESTRATION :VALUE TRIANGULATE-FILTER-SURFACE-WEB-SERVICE-INTERFACE-ORCHESTRATION)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE
      TRIANGULATE-FILTER-SURFACE-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(def-class triangulate-filter-surface-web-service-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value triangulate-filter-surface-web-service-interface)
     (has-web-service-host :value "lhdl.cis.beds.ac.uk")
     (has-web-service-port :value 80)))
;;; }}}

;;; {{{ create-ref-sys-service
(def-class create-ref-sys-mediator-non-functional-properties (non-functional-properties)
    nil)

(DEF-CLASS create-ref-sys-mediator (wg-mediator) ?mediator
    ((HAS-SOURCE-COMPONENT :VALUE create-ref-sys-goal)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE create-ref-sys-mediator-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS CREATE-REF-SYS-WEB-SERVICE (WEB-SERVICE) ?WEB-SERVICE
    ((HAS-CAPABILITY :VALUE CREATE-REF-SYS-WEB-SERVICE-CAPABILITY)
     (HAS-INTERFACE :VALUE CREATE-REF-SYS-WEB-SERVICE-INTERFACE)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE CREATE-REF-SYS-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS CREATE-REF-SYS-WEB-SERVICE-NON-FUNCTIONAL-PROPERTIES
    (NON-FUNCTIONAL-PROPERTIES)
    NIL)

(DEF-CLASS CREATE-REF-SYS-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS CREATE-REF-SYS-WEB-SERVICE-CAPABILITY
           (CAPABILITY)
           ?CAPABILITY
           ((USED-MEDIATOR :VALUE CREATE-REF-SYS-MEDIATOR)
            (HAS-NON-FUNCTIONAL-PROPERTIES
             :VALUE
             CREATE-REF-SYS-WEB-SERVICE-CAPABILITY-NON-FUNCTIONAL-PROPERTIES)))

(DEF-CLASS CREATE-REF-SYS-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES
           (NON-FUNCTIONAL-PROPERTIES)
           NIL)

(DEF-CLASS CREATE-REF-SYS-WEB-SERVICE-INTERFACE-CHOREOGRAPHY
           (CHOREOGRAPHY)
           ((HAS-GROUNDING :VALUE ((grounded-to-soap1.1)))
            (has-earthing :value create-ref-sys-web-service-grounding)))

(def-instance create-ref-sys-web-service-grounding soap-grounding
  ((has-url "http://lhdl.cis.beds.ac.uk/ExecWSV2/CreateRefSysService/CreateRefSysService.asmx")
   (has-soap-action "http://lhdl.cis.beds.ac.uk/ExecWSV2/CreateRefSysService/createRefSys")
   (has-soap-method "createRefSys")
   (has-target-namespace "http://lhdl.cis.beds.ac.uk/ExecWSV2/CreateRefSysService/")))

(DEF-CLASS CREATE-REF-SYS-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN
    (PROBLEM-SOLVING-PATTERN)
    ((HAS-BODY :VALUE nil)))

(DEF-CLASS CREATE-REF-SYS-WEB-SERVICE-INTERFACE-ORCHESTRATION
           (ORCHESTRATION)
           ((HAS-PROBLEM-SOLVING-PATTERN
             :VALUE
             CREATE-REF-SYS-WEB-SERVICE-INTERFACE-ORCHESTRATION-PROBLEM-SOLVING-PATTERN)))

(DEF-CLASS CREATE-REF-SYS-WEB-SERVICE-INTERFACE (INTERFACE) ?INTERFACE
    ((HAS-CHOREOGRAPHY :VALUE CREATE-REF-SYS-WEB-SERVICE-INTERFACE-CHOREOGRAPHY)
     (HAS-ORCHESTRATION :VALUE CREATE-REF-SYS-WEB-SERVICE-INTERFACE-ORCHESTRATION)
     (HAS-NON-FUNCTIONAL-PROPERTIES
      :VALUE
      CREATE-REF-SYS-WEB-SERVICE-INTERFACE-NON-FUNCTIONAL-PROPERTIES)))

(def-class create-ref-sys-web-service-publisher-information
    (publisher-information)
    ((has-associated-web-service-interface :value create-ref-sys-web-service-interface)
     (has-web-service-host :value "lhdl.cis.beds.ac.uk")
     (has-web-service-port :value 80)))

;;; }}}


;;; {{{

;;; Alessio 18-11-2008

(def-class orchestrated-SWS-goal (goal))


;;; Orchestrated SWS: Manage VME Surface

(def-class manage-VMESurface-goal (orchestrated-SWS-goal) ?goal
    ((has-input-role :value has-source
                     :value has-operation
                     :value has-sink
                     :value has-username
                     :value has-password)
     (has-input-soap-binding :value (has-source "sexpr")
                             :value (has-operation "sexpr")
                             :value (has-sink "sexpr")
                             :value (has-password "string")
                             :value (has-username "string"))
     (has-output-role :value has-final-result)
     (has-output-soap-binding :value (has-final-result "xml"))
     (has-source :type source)
     (has-operation :type operation)
     (has-sink :type sink)
     (has-username :type lhdl-username)
     (has-password :type lhdl-password)
     (has-final-result :type string) ;;;;;; ?
     (has-non-functional-properties :value manage-VMESurface-non-functional-properties)))

(def-class manage-VMESurface-non-functional-properties (non-functional-properties)
    ((has-description "Manage operations on VME surfaces.  Returns URL for the final resource.")))


(def-class manage-VMESurface-mediator-non-functional-properties (non-functional-properties)
    nil)

(def-class manage-VMESurface-mediator (wg-mediator) ?mediator
    ((has-source-component :value manage-VMESurface-goal)
     (has-non-functional-properties
      :value manage-VMESurface-mediator-non-functional-properties)))

(def-class manage-VMESurface-web-service (web-service) ?web-service
    ((has-capability :value manage-VMESurface-web-service-capability)
     (has-interface :value manage-VMESurface-web-service-interface)
     (has-non-functional-properties
      :value manage-VMESurface-web-service-non-functional-properties)))

(def-class manage-VMESurface-web-service-non-functional-properties
    (non-functional-properties)
    nil)

(def-class manage-VMESurface-web-service-capability-non-functional-properties
           (non-functional-properties)
           nil)

(def-class manage-VMESurface-web-service-capability
           (capability)
           ?capability
           ((used-mediator :value manage-VMESurface-mediator)
            (has-non-functional-properties
             :value
             manage-VMESurface-web-service-capability-non-functional-properties)))

(def-class manage-VMESurface-web-service-interface-non-functional-properties
           (non-functional-properties)
           nil)

(def-class manage-VMESurface-web-service-interface-choreography
           (choreography)
           ((has-grounding :value ((normal dummy-function)))))


;;


(defun get-resulting-uri ()  
  (let ((r nil)) 
    (the-slot-value (second (first (setf r wp::*achieve-goal-results*))) 'ocml::has-uri)
    ))

(defun get-resulting-xml ()  
  (let () 
    (concatenate 'string (get-resulting-uri) ".xml")
    ))


(def-function orch-get-resulting-uri ()
  :lisp-fun #'get-resulting-uri)

(def-function orch-get-resulting-xml ()
  :lisp-fun #'get-resulting-xml)


;;

(defun get-source-instance (input)
  (let ()
    (skyhook-source input)))

(def-function orch-get-source-instance (?input)
  :lisp-fun #'get-source-instance) 



(defun get-operation-instance (input)
  (let ()
    (skyhook-operation input)))

(def-function orch-get-operation-instance (?input)
  :lisp-fun #'get-operation-instance) 



(defun get-sink-instance (input)
  (let ()
    (skyhook-sink input)))

(def-function orch-get-sink-instance (?input)
  :lisp-fun #'get-sink-instance)


;; 


(def-class manage-VMESurface-web-service-interface-orchestration-problem-solving-pattern
    (problem-solving-pattern)
    ((has-body :value (;; get the source
                       (cond ((= (the-parent (orch-get-source-instance (orch-get-input-role 'ocml::has-source))) 'ocml::VTKsource) 
                              (achieve-goal 'ocml::import-vtk-goal
                                            (the-slot-value (orch-get-source-instance (orch-get-input-role 'ocml::has-source)) 'ocml::has-filename)
                                            (orch-get-input-role 'ocml::has-username)
                                            (orch-get-input-role 'ocml::has-password)
                                            ))
                             ((= (the-parent (orch-get-source-instance (orch-get-input-role 'ocml::has-source))) 'ocml::STLsource) 
                              (achieve-goal 'ocml::import-STL-goal
                                            (the-slot-value (orch-get-source-instance (orch-get-input-role 'ocml::has-source)) 'ocml::has-filename)
                                            (orch-get-input-role 'ocml::has-username)
                                            (orch-get-input-role 'ocml::has-password)
                                            ))
                             ((= (the-parent (orch-get-source-instance (orch-get-input-role 'ocml::has-source))) 'ocml::CreateCone) 
                              (achieve-goal 'ocml::create-cone-surface-parametric-goal
                                            (the-slot-value (orch-get-source-instance (orch-get-input-role 'ocml::has-source)) 'ocml::has-height)
                                            (the-slot-value (orch-get-source-instance (orch-get-input-role 'ocml::has-source)) 'ocml::has-radius)
                                            (the-slot-value (orch-get-source-instance (orch-get-input-role 'ocml::has-source)) 'ocml::has-resolution)
                                            (the-slot-value (orch-get-source-instance (orch-get-input-role 'ocml::has-source)) 'ocml::has-cap)
                                            (orch-get-input-role 'ocml::has-username)
                                            (orch-get-input-role 'ocml::has-password)
                                            ))
                             ((= (the-parent (orch-get-source-instance (orch-get-input-role 'ocml::has-source))) 'ocml::CreateCube) 
                              (achieve-goal 'ocml::create-cube-surface-parametric-goal
                                            (the-slot-value (orch-get-source-instance (orch-get-input-role 'ocml::has-source)) 'ocml::has-x)
                                            (the-slot-value (orch-get-source-instance (orch-get-input-role 'ocml::has-source)) 'ocml::has-y)
                                            (the-slot-value (orch-get-source-instance (orch-get-input-role 'ocml::has-source)) 'ocml::has-z)
                                            (orch-get-input-role 'ocml::has-username)
                                            (orch-get-input-role 'ocml::has-password)
                                            ))
                             )
                       
                       ;;perform the operation
                       (cond ((= (the-parent (orch-get-operation-instance (orch-get-input-role 'ocml::has-operation))) 'ocml::SmoothSurface) 
                              (achieve-goal 'ocml::smooth-filter-surface-goal  
                                            (orch-get-resulting-uri)
                                            (orch-get-resulting-xml)
                                            (the-slot-value (orch-get-operation-instance (orch-get-input-role 'ocml::has-operation)) 'ocml::has-iterations)
                                            (orch-get-input-role 'ocml::has-username)
                                            (orch-get-input-role 'ocml::has-password)
                                            ))
                             ((= (the-parent (orch-get-operation-instance (orch-get-input-role 'ocml::has-operation))) 'ocml::StripSurface) 
                              (achieve-goal 'ocml::strip-filter-surface-goal  
                                            (orch-get-resulting-uri)
                                            (orch-get-resulting-xml)
                                            (orch-get-input-role 'ocml::has-username)
                                            (orch-get-input-role 'ocml::has-password)
                                            ))
                             ((= (the-parent (orch-get-operation-instance (orch-get-input-role 'ocml::has-operation))) 'ocml::TriangulateSurface) 
                              (achieve-goal 'ocml::triangulate-filter-surface-goal  
                                            (orch-get-resulting-uri)
                                            (orch-get-resulting-xml)
                                            (orch-get-input-role 'ocml::has-username)
                                            (orch-get-input-role 'ocml::has-password)
                                            ))                                                          
                             )

                       ;;get the destination
                       (cond ((= (the-parent (orch-get-sink-instance (orch-get-input-role 'ocml::has-sink))) 'ocml::ExportVTK) 
                              (achieve-goal 'ocml::export-vtk-goal  
                                            (orch-get-resulting-uri)
                                            (orch-get-resulting-xml)
                                            (the-slot-value (orch-get-sink-instance (orch-get-input-role 'ocml::has-sink)) 'ocml::has-ABSMatrixFlag)
                                            (the-slot-value (orch-get-sink-instance (orch-get-input-role 'ocml::has-sink)) 'ocml::has-format)
                                            (orch-get-input-role 'ocml::has-username)
                                            (orch-get-input-role 'ocml::has-password)
                                            ))
                             ((= (the-parent (orch-get-sink-instance (orch-get-input-role 'ocml::has-sink))) 'ocml::ExportSTL) 
                              (achieve-goal 'ocml::export-stl-goal  
                                            (orch-get-resulting-uri)
                                            (orch-get-resulting-xml)
                                            (the-slot-value (orch-get-sink-instance (orch-get-input-role 'ocml::has-sink)) 'ocml::has-ABSMatrixFlag)
                                            (the-slot-value (orch-get-sink-instance (orch-get-input-role 'ocml::has-sink)) 'ocml::has-format)
                                            (orch-get-input-role 'ocml::has-username)
                                            (orch-get-input-role 'ocml::has-password)
                                            ))
                             ((= (the-parent (orch-get-sink-instance (orch-get-input-role 'ocml::has-sink))) 'ocml::RenderSurface) 
                              (achieve-goal 'ocml::render-surface-goal  
                                            (orch-get-resulting-uri)
                                            (orch-get-resulting-xml)
                                            (the-slot-value (orch-get-sink-instance (orch-get-input-role 'ocml::has-sink)) 'ocml::has-bgColour)
                                            (the-slot-value (orch-get-sink-instance (orch-get-input-role 'ocml::has-sink)) 'ocml::has-zoom)
                                            (the-slot-value (orch-get-sink-instance (orch-get-input-role 'ocml::has-sink)) 'ocml::has-camAzimuth)
                                            (the-slot-value (orch-get-sink-instance (orch-get-input-role 'ocml::has-sink)) 'ocml::has-camRoll)
                                            (the-slot-value (orch-get-sink-instance (orch-get-input-role 'ocml::has-sink)) 'ocml::has-imageWidth)
                                            (the-slot-value (orch-get-sink-instance (orch-get-input-role 'ocml::has-sink)) 'ocml::has-imageHeight)
                                            (orch-get-input-role 'ocml::has-username)
                                            (orch-get-input-role 'ocml::has-password)
                                            ))                                                          
                             )
                       ))))

;(describe-instance 'instance1648)

(def-class manage-VMESurface-web-service-interface-orchestration
           (orchestration)
           ((has-problem-solving-pattern
             :value
             manage-VMESurface-web-service-interface-orchestration-problem-solving-pattern)))

(def-class manage-VMESurface-web-service-interface (interface) ?interface
    ((has-choreography :value manage-VMESurface-web-service-interface-choreography)
     (has-orchestration :value manage-VMESurface-web-service-interface-orchestration)
     (has-non-functional-properties
      :value
      manage-VMESurface-web-service-interface-non-functional-properties)))


;;; }}}
