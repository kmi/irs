;; Mode: Lisp; Package: ocml

(in-package "OCML")

(in-ontology lhdl-domain)

;;; Classes

(def-class #_BiomedTownResource ()
    ((has-value :type string)))

(def-class #_BiomedTownAccount ()
    ((#_hasUsername :type lhdl-username)
     (#_hasPassword :type lhdl-password)))

(def-class lhdl-username ()
  ((has-value :type string)))

(def-class lhdl-password ()
  ((has-value :type string)))

(def-class download-response ()
    ((has-filename :type string)
     (has-content :type string)))

(def-class lhdl-name ()
    ((has-value :type string)))

(def-class image-file-sequence () ?seq
    ((has-value :type list))
    :iff-def (every ?seq image-file))

(def-class image-file ()
    ((has-filename :type string)))

(def-class dicom-dictionary ()
    ((has-value :type string)))

(def-class dicom-type () ?i
  ((has-value :type integer))
  :constraint (and (has-value ?i ?v)
		   (>= ?v 0)))

(def-class dicom-zipfile ()
    ((has-value :type string)))

(def-class dicom-build-step () ?i
    ((has-value :type integer))
    :constraint (and (has-value ?i ?v)
                     (>= ?v 1) (<= ?v)))

(def-class vtk-file-format ())

(def-class stl-file-format ())

(def-class colour/grayscale ())

(def-class vme-attribute ()
  ((has-label :type string)))

(def-class vme-body-part ()
  ((has-medical-name :type string)))

(def-class vme-region (vme-body-part))

(def-class vme-type (vme-attribute))

(def-class vme-imaging (vme-attribute))

(def-class vme-bone (vme-body-part))

(def-class vme-uri ()
    ((has-name :type string)))

(def-class uri-result ()
    ((has-uri :type string)))

(def-class lhdl-boolean ()
    ((has-value :type boolean)))

;;; {{{ defined only for the extract-iso-surface goal, so the lowering
;;; can be specific.  This really indicates the lowering machinery is
;;; broken :-(
(def-class autolod (lhdl-boolean))

(def-class optimise (lhdl-boolean))
;;; }}}

;;; {{{ boxed primitives
(def-class lhdl-double ()
    ((has-value :type float)))

(def-class lhdl-integer ()
    ((has-value :type integer)))
;;; }}}

(def-class contour (lhdl-double))

(def-class iterations (lhdl-integer))

(def-class edge-split (lhdl-boolean))

(def-class angle (lhdl-integer))

(def-class flip-normals (lhdl-boolean))

(def-class lhdl-filename ()
    ((has-value :type string)))

(def-class scale (lhdl-double))

(def-class raw-volume-filename (lhdl-filename))

(def-class z-coord-filename (lhdl-filename))

(def-class lhdl-url ()
    ((has-value :type string)))

(def-class lhdl-xml ()
    ((has-value :type string)))

(def-class camera-perspective ()
    ((has-value :type float)))

(def-class colour ())

(def-class rgb-colour (colour)
    ((has-red-value :type float)
     (has-green-value :type float)
     (has-blue-value :type float)))

(def-class background-colour (rgb-colour))

(def-class zoom-factor (lhdl-double))

(def-class abs-matrix (lhdl-boolean))

(def-class camera-azimuth (lhdl-double))

(def-class camera-roll (lhdl-double))

(def-class image-width ()
    ((has-value :type float)))

(def-class image-height ()
    ((has-value :type float)))

(def-class base64-image ()
    ((has-value :type string)))

(def-class preserve-topology (lhdl-boolean))

(def-class target-reduction (lhdl-integer))

;;; Instances

(def-instance ct vme-imaging
  ((has-label "CT")))

(def-instance volume vme-type
  ((has-label "Volume")))

;;; {{{ booleans
(def-instance lhdl-true lhdl-boolean
  ((has-value t)))

(def-instance lhdl-false lhdl-boolean
  ((has-value nil)))
;;; }}}

;;; {{{ camera-perspective
(def-instance camera-perspective-bottom camera-perspective)
(def-instance camera-perspective-top camera-perspective)
(def-instance camera-perspective-back camera-perspective)
(def-instance camera-perspective-front camera-perspective)
(def-instance camera-perspective-left camera-perspective)
(def-instance camera-perspective-right camera-perspective)
;;; }}}
(def-class scalar-type () ?i
    "Indicates the type of a raw file."
    ((has-value :type integer))
  :constraint (and (has-value ?i ?v)
		   (>= ?v 0)))

(def-class scalar-sign (lhdl-boolean))

(def-class endianess () ?i
    :iff-def (member ?i big-endianess little-endianess))

(def-class header-skip-size (lhdl-integer))

(def-class slice (lhdl-integer))

;; This type is underspecified.  Both Xia's example and the docs say
;; it takes two integers, but Xia's scheme says zero or more.  But
;; that schema conflicts with the docs in other places, so I don't
;; place too much faith in it.
(def-class slice-voi ()
    ((has-value-one :type integer)
     (has-value-two :type integer)))

(def-class 3d-integer ()
    ((has-x :type integer)
     (has-y :type integer)
     (has-z :type integer)))

(def-class 3d-double ()
    ((has-x :type double)
     (has-y :type double)
     (has-z :type double)))

(def-class volume-measurement ()
    ((has-nsi :type double)
     (has-surface-area :type double)
     (has-surface-volume :type double)))

(def-class height (lhdl-double))

(def-class radius (lhdl-double))

(def-class resolution (lhdl-double))

(def-class x-length (lhdl-double))
(def-class y-length (lhdl-double))
(def-class z-length (lhdl-double))

(def-class origin (3d-double))
(def-class point-1 (3d-double))
(def-class point-2 (3d-double))

(def-class x-resolution (resolution))
(def-class y-resolution (resolution))
(def-class phi-resolution (resolution))
(def-class theta-resolution (resolution))

;;; {{{ instances

(def-instance vtk-file-format-ascii vtk-file-format)
(def-instance vtk-file-format-binary vtk-file-format)

(def-instance stl-file-format-ascii stl-file-format)
(def-instance stl-file-format-binary stl-file-format)

(def-instance little-endianess endianess)
(def-instance big-endianess endianess)

(def-instance colour/grayscale-colour colour/grayscale)
(def-instance colour/grayscale-grayscale colour/grayscale)
;;; }}}


;;; {{{


;; Alessio 18-11-2008

;;; Classes and Instances for orchestrated SWS (ManageVMESurface)


;; VMESurface sources

(def-class source ())

(def-class VTKsource (source)
  ((has-filename :type string)))

(def-class STLsource (source)
  ((has-filename :type string)))

(def-class CreateCone (source)
  ((has-height :type string)
   (has-radius :type string)
   (has-resolution :type string)
   (has-cap :type string)))

(def-class CreateCube (source)
  ((has-x :type string)
   (has-y :type string)
   (has-z :type string)))


(def-instance vtk-source-1 VTKsource
  ((has-filename "sample/head.vtk")))

(def-instance stl-source-1 STLsource
  ((has-filename "sample/1091_Femur_Right.stl")))

(def-instance create-cone-1 CreateCone 
  ((has-height "5")
   (has-radius "5")
   (has-resolution "20")
   (has-cap "true")))

(def-instance create-cube-1 CreateCube 
  ((has-x "2")
   (has-y "2")
   (has-z "2")))
  

;;; VMESurface operations

(def-class operation ())


(def-class DecimateSurface (operation) ;; the respective WS in beds does not work!
  ((has-topology :type string)
   (has-reduction :type string)))

(def-class SmoothSurface (operation)
  ((has-iterations :type string)))

(def-class TriangulateSurface (operation))

(def-class StripSurface (operation))


(def-instance decimate-surface-1 DecimateSurface ;; the respective WS in beds does not work!
  ((has-topology "true")
   (has-reduction "30")))

(def-instance smooth-surface-1 SmoothSurface
  ((has-iterations "30")))

(def-instance triangulate-surface-1 TriangulateSurface)

(def-instance strip-surface-1 StripSurface)




;; VMEdestinations

(def-class sink ())


(def-class RenderSurface (sink)
  ((has-bgColour :type string)
   (has-zoom :type string)
   (has-camAzimuth :type string)
   (has-camRoll :type string)
   (has-imageWidth :type string)
   (has-imageHeight :type string)))

(def-class ExportVTK (sink)
  ((has-ABSMatrixFlag :type string)
   (has-format :type string)))

(def-class ExportSTL (sink)
  ((has-ABSMatrixFlag :type string)
   (has-format :type string)))




(def-instance render-surface-1 RenderSurface
  ((has-bgColour "0/0/0")
   (has-zoom "1")
   (has-camAzimuth "0")
   (has-camRoll "0")
   (has-imageWidth "300")
   (has-imageHeight "300")))

(def-instance Export-VTK-1 ExportVTK
  ((has-ABSMatrixFlag "true")
   (has-format "ascii")))

(def-instance Export-STL-1 ExportSTL
  ((has-ABSMatrixFlag "true")
   (has-format "ascii")))



;;; }}}
