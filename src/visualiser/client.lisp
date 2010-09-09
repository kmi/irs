(in-package #:ipv)

(defvar *irs-process-visualizer* nil)

(defvar *irs-server-service-name* "IRS Server")

(defvar *irs-viz-image-size*
  #+(and :linux (not :lispworks6)) 24
  #+(or :macosx :win32 :lispworks6) 36)

(defparameter *irs-font-default-description*
  (gp:make-font-description
   :family #+:linux "Andale Mono" #-:linux "Courier New" ;;"Fixedsys" ;;"times"
   :size 12
   :weight :bold ;;:normal
   :slant :roman ;;:italic
   ))

(defvar *info-pane-minimum-width* nil) ;;;220)

(defvar *default-image-width* 72)

(defvar *default-image-height* 72)

(defvar *irs-visualizer-columns* 3)

(defun start (&optional (visualizer-columns *irs-visualizer-columns*))
  (setf *irs-process-visualizer* (create-irs-visualizer visualizer-columns))
  (add-service *irs-process-visualizer* *irs-server-service-name*
               nil nil nil nil nil t)
  (refresh))

(defun refresh ()
  (when *irs-process-visualizer*
    ;; Add web-service vizualisers.
    (mapc #'add-visualizers (mapcar #'car ocml::*all-ontologies*))))

(defun make-rgb-from-255 (r g b)
  (color::make-rgb (coerce (/ r 255) 'short-float) 
                   (coerce (/ g 255) 'short-float)
                   (coerce (/ b 255) 'short-float)))

;;;dark blue
(color:define-color-alias :irs-viz-colour1
                    (make-rgb-from-255 5 102 204))

;;;light blue
(color:define-color-alias :irs-viz-colour2
                    (make-rgb-from-255 153 204 255))


;;;grey
(color:define-color-alias :irs-viz-colour3
                    (make-rgb-from-255 204 204 204))


(defclass irs-viz-text-output-pane (capi:collector-pane)
  ((name :initarg :name :accessor output-pane-name))
  (:default-initargs
   :vertical-scroll t
   :background :irs-viz-colour2 ;;:lightgoldenrod
   #+:win32 :font 
   #+:win32 (gp:make-font-description :family "times" 
				     :weight :medium
				     :slant :roman ;;:italic 
				     :size 12)))

(defmethod update-image ((pane irs-viz-text-output-pane) service-name image-type))

(defmethod format-for-service ((pane irs-viz-text-output-pane) service-name
                               &rest arguments)
  (when (string= (output-pane-name pane) service-name)
    (apply #'format (capi:collector-pane-stream pane)
           arguments)))


(defmethod reset-element ((pane irs-viz-text-output-pane))
  (modify-editor-pane-buffer pane :contents ""))

(defmethod internal-find-service ((pane irs-viz-text-output-pane) service-name))

(defclass irs-viz-button (capi:push-button)
  ((output-pane :initarg :output-pane :accessor button-output-pane)
   (ontology :initarg :ontology :accessor button-ontology)
   (input-roles :initarg :input-roles :accessor button-input-roles)
   (output :initarg :output :accessor button-output)
   (publisher-ip-address :initarg :publisher-ip-address :accessor 
                         button-publisher-ip-address)
   (interface-elements :initarg :interface-elements 
                       :accessor button-interface-elements)
   (publisher-port :initarg :publisher-port :accessor button-publisher-port))
  (:default-initargs
   :callback-type :item ;;:interface 
   :callback 'service-button
   :foreground :irs-viz-colour1 
   :background :irs-viz-colour2))

(defmethod update-image ((pane irs-viz-button) service-name image-type))

(defmethod format-for-service ((pane irs-viz-button) service-name
                               &rest arguments))

(defmethod internal-find-service ((pane irs-viz-button) service-name)
  (when (string= service-name (item-text pane))
    pane))

(defun service-button (button)
  (let ((name (item-text button)))
    (when (string= name *irs-server-service-name*)
      (reset-all-elements button))
    (let ((output-stream (capi:collector-pane-stream (button-output-pane button))))
      (format output-stream
              "~%~%Service: ~a~:[~*~;~%Ontology: ~:(~a~)~]~:[~*~;~%Input Roles:~{~{~%  ~(~a:~) ~s~}~}~]~:[~*~;~%Output: ~s~]~:[~*~;~%Web Service IP Address: ~a~]~:[~*~;~%Web Service Port: ~a~]"
              (item-text button)
              (button-ontology button) (button-ontology button)
              (button-input-roles button) (button-input-roles button)
              (button-output button) (button-output button)
              (button-publisher-ip-address button)
              (button-publisher-ip-address button)
              (button-publisher-port button) (button-publisher-port button)))))

(defun reset-all-elements (button)
  (mapc #'(lambda (element) (reset-element element))
        (button-interface-elements button)))

(defclass irs-viz-state-image (capi:output-pane)
  ((image :initform nil)
   (name :initarg :name :accessor image-pane-name))
  (:default-initargs
   :display-callback 'irs-viz-state-image-callback 
   :foreground :irs-viz-colour3 
   :background :irs-viz-colour3
   :visible-max-width *irs-viz-image-size*
   :visible-max-height *irs-viz-image-size*
   :visible-min-height *irs-viz-image-size*
   :visible-min-width *irs-viz-image-size*
   ;;:best-width *irs-viz-image-width*
   ;;:best-height *irs-viz-image-height*
  ))

(defclass irs-viz-info-pane (capi:row-layout) 
  ((button :accessor info-pane-button :initarg :button)
   (image-pane :accessor info-pane-image-pane :initarg :image-pane))
  (:default-initargs 
   :visible-min-width *info-pane-minimum-width*))


(defmethod internal-find-service ((pane irs-viz-state-image) service-name))

(defmethod internal-find-service ((pane irs-viz-info-pane) service-name)
  (internal-find-service (info-pane-button pane) service-name))

(defmethod update-image ((pane irs-viz-info-pane) service-name image-type)
  (update-image (info-pane-image-pane pane) service-name image-type))

(defmethod update-image ((pane irs-viz-state-image) service-name image-type)
  (when (string= (image-pane-name pane) service-name)
    (internal-update-image pane image-type)))


(defmethod reset-element ((pane irs-viz-info-pane))
  (reset-element (info-pane-image-pane pane)))

(defmethod reset-element ((pane irs-viz-state-image))
  (internal-update-image pane *initial-image-type*))

(defun internal-update-image (pane image-type)
  (with-slots (image) pane
    (let ((new-image (get-image image-type)))
      (when image
        ;;may cause errors
        ;;(gp:free-image pane image)
        )
      (setf image (gp:load-image pane new-image))
      (gp:invalidate-rectangle pane))))

(defmethod format-for-service ((pane irs-viz-info-pane) service-name
                               &rest arguments))


(defmethod format-for-service ((pane irs-viz-state-image) service-name
                               &rest arguments))


(defclass irs-viz-service-pane (capi:column-layout)  ;; was grid-layout
  ((info-pane :accessor service-pane-info-pane :initarg :info-pane)
   (output-pane :accessor service-pane-output-pane :initarg :output-pane))
  (:default-initargs 
   :y-adjust :top
   :background :irs-viz-colour1
   :rows 2));; was columns


(defmethod format-for-service ((pane irs-viz-service-pane) service-name
                               &rest arguments)
  (apply #'format-for-service (service-pane-info-pane pane) service-name arguments)
  (apply #'format-for-service (service-pane-output-pane pane) service-name arguments))


(defmethod reset-element ((pane irs-viz-service-pane))
  (reset-element (service-pane-info-pane pane))
  (reset-element (service-pane-output-pane pane)))

(defmethod update-image ((pane irs-viz-service-pane) service-name image-type)
  (update-image (service-pane-info-pane pane) service-name image-type)
  (update-image (service-pane-output-pane pane) service-name image-type))

(defmethod internal-find-service ((pane irs-viz-service-pane) service-name)
  (or (internal-find-service (service-pane-info-pane pane) service-name)
      (internal-find-service (service-pane-output-pane pane) service-name)))


#| does the same job as the function below using scaling
(defun irs-viz-state-image-callback (pane x y width height)
  (declare (ignore x y width height))
  (with-slots (image) pane
    (when image
      (let ((x-scale (/ (capi:simple-pane-visible-width pane) 
                        *default-image-width*))
            (y-scale (/ (capi:simple-pane-visible-height pane)
                        *default-image-height*)))
      (with-graphics-scale (pane x-scale y-scale)
        (gp:draw-image pane image 0 0))))))
|#

(defun irs-viz-state-image-callback (pane x y width height)
  (declare (ignore x y width height))
  (with-slots (image) pane
    (when image
        (gp:draw-image pane image 0 0
                       :from-width *default-image-width*
                     :from-height *default-image-height*
                     :to-width (capi:simple-pane-visible-width pane)
                     :to-height (capi:simple-pane-visible-height pane)))))

(capi:define-interface irs-vizualizer ()
  ((irs-server-button :initform nil :accessor irs-server-button))
  (:panes)
  (:layouts
   (main-layout
    capi:grid-layout
    nil
    :columns *irs-visualizer-columns*
    :y-adjust :top
    :vertical-scroll t
    :background :irs-viz-colour3
    :Accessor main-layout))
  (:default-initargs
   :title "IRS Visualizer"
   :background :irs-viz-colour3
   :best-x 0
   :best-y 0
   :best-width 1020
   :best-height 700
   :destroy-callback 'destroy-irs-visualizer))

(defun destroy-irs-visualizer (interface)
  (when (eq *irs-process-visualizer* interface)
    (setf *irs-process-visualizer* nil)))


(defun create-irs-visualizer (&optional (number-of-columns *irs-visualizer-columns*))
  (setf *irs-visualizer-columns* number-of-columns)
  (let ((interface (make-instance 'irs-vizualizer)))
    (setf (interface-title interface) 
          "IRS Visualizer")
    (capi:display interface)))

(defun iv-format (interface service-name &rest arguments)
  (dolist (pane (capi:layout-description (main-layout interface)))
    (apply #'format-for-service pane service-name arguments)))

(defun find-service (interface service-name)
  (let ((result nil))
    (dolist (pane (capi:layout-description (main-layout interface)))
      (setf result (or result (internal-find-service pane service-name)))
      (when result
        (return-from find-service result)))))

(defun add-service (interface service-name &optional 
                              ontology input-roles output 
                              publisher-ip-address publisher-port
                              irs-server-service-p)
  (capi:execute-with-interface 
   interface
   #'(lambda (service-name 
              ontology input-roles output 
              publisher-ip-address publisher-port)
       (internal-add-service interface service-name 
                             ontology input-roles output 
                             publisher-ip-address publisher-port 
                             irs-server-service-p))
   service-name 
   ontology input-roles output 
   publisher-ip-address publisher-port)
  nil)

(defun internal-add-service (interface service-name 
                                       ontology input-roles output 
                                       publisher-ip-address publisher-port
                                       irs-server-service-p)                     
  (let* ((layout (main-layout interface))
         (output-pane 
          (make-instance 'irs-viz-text-output-pane 
                         :name service-name))
         (button (make-instance 'irs-viz-button :text service-name
                                :output-pane output-pane
                                :ontology ontology
                                :input-roles input-roles
                                :output output
                                :publisher-ip-address 
                                publisher-ip-address
                                :publisher-port publisher-port))
         (image-pane 
          (make-instance 'irs-viz-state-image :name service-name))
         (info-pane (make-instance 'irs-viz-info-pane 
                                   :button button
                                   :image-pane image-pane))
         (service-pane (make-instance 'irs-viz-service-pane
                                      :output-pane output-pane
                                      :info-pane info-pane)))
    (setf (capi:layout-description info-pane)
          (list image-pane button)
          (capi:layout-description service-pane)
          (list info-pane output-pane)
          (capi:layout-description layout)
          (append
           (capi:layout-description layout)
           (list service-pane)))
    (when irs-server-service-p
      (setf (irs-server-button interface)
            button))
    (setf (button-interface-elements (irs-server-button interface))
          (capi:layout-description layout))
    (internal-update-image image-pane *initial-image-type*)
    (setf (simple-pane-font output-pane) *irs-font-default-description*)
    ))

(defun add-services (interface &rest service-names) 
  (dolist (service-name service-names)
    (add-service interface service-name)))

(defun first-output-pane (interface)
  (service-pane-output-pane (car (capi:layout-description (main-layout interface)))))
