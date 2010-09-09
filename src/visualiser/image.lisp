(in-package #:ipv)

(defun image-file (name)
  (format nil "irs:assets;images;~A~A.bmp" name
	  #+(and :linux (not :lispworks6)) "-small"
	  #+(or :macosx :win32 :lispworks6) ""))

(defvar *receiving-image*
  (gp:read-external-image 
   (translate-logical-pathname 
    (image-file "receiving"))))

(defvar *sending-image*
  (gp:read-external-image 
   (translate-logical-pathname 
    (image-file "sending"))))

(defvar *idle-image*
  (gp:read-external-image 
   (translate-logical-pathname 
    (image-file "idle"))))

(defvar *waiting-image*
  (gp:read-external-image 
   (translate-logical-pathname 
    (image-file "waiting"))))

(defvar *processing-image*
  (gp:read-external-image 
   (translate-logical-pathname 
    (image-file "processing"))))

(defvar *type-image-mappings*
  `((idle . ,*idle-image*) (receiving . ,*receiving-image*)
    (sending . ,*sending-image*) (processing . ,*processing-image*)
    (waiting . ,*waiting-image*)))

(defvar *initial-image-type* 'idle)

(defun get-image (type)
  (cdr (assoc type *type-image-mappings*)))


(defun update-images (interface service-name image-type)
  (dolist (pane (capi:layout-description (main-layout interface)))
    (update-image pane service-name image-type)))
