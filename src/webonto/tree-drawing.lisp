;;; Mode: Lisp; Package: web-onto

;;; Author: John Domingue

;;; The Open University

(in-package "WEB-ONTO")

(defvar *web-onto-font*
  nil
  ;;;(find-font "times" "bold" 10)
  )

(defvar *web-onto-font-name* "TimesRoman"
  )

(defvar *web-onto-font-size* 10 ;;36 
  )

(defvar *web-onto-font-style* "bold"
  )

(defvar *label-gap*
    #+(and :lispworks :unix) 1
    #+(and :lispworks :win32) 16
    #+allegro 16)

(defvar *class-node-height* 30 ;;40 
  )

;(pretty-draw-tree root #'(lambda (event stream)
;			   (declare (ignore event stream)))
;                  #'(lambda (x) (with-slots (clos::direct-subclasses) x
;                                  clos::direct-subclasses)))

;(defmethod tree-node-offset ((basic-domain-class ocml::basic-domain-class)
;                             (orientation (eql :vertical)))
;  (let ((string-width
;         (xlib::text-extents
;	  (xlib:open-font clue:*display* *web-onto-font*)
;	  (ocml::maybe-ocml-name basic-domain-class))))
;;    (if (> string-width *p-node-box-width*)
;        (round (/ string-width 2))
;        0)))


(defvar *label-width-image-extension-ratio*
  #+(and :lispworks :win32) 0
  #+(and :lispworks :unix) 0.2
  #+allegro 0.2
  "Can be found in ocml_image_node image nodes are slightly wider")

(defmethod tree-edges ((class web-onto-named-shape))
  (let ((string-width
         (string-width (ocml-name class) *web-onto-font*)))
      (values (x-position class) (y-position class)
              (+ (x-position class) *label-gap* string-width
                 (floor (* string-width *label-width-image-extension-ratio*)))
              (+ (y-position class) *class-node-height*))))

(defmethod tree-width ((class web-onto-named-shape))
  (let ((string-width (string-width (ocml-name class) *web-onto-font*)))
    (+ *label-gap* string-width
       (floor (* string-width *label-width-image-extension-ratio*)))))

(defmethod tree-width ((label string))
  (let ((string-width (string-width label *web-onto-font*)))
    (+ *label-gap* string-width
       (floor (* string-width *label-width-image-extension-ratio*)))))

(defmethod tree-height ((class web-onto-named-shape))
  *class-node-height*)


(defclass level-position ()
  ((left :initform 0 :accessor left :initarg :left)
   (top :initform 0 :accessor top :initarg :top)
   (right :initform 0 :accessor right :initarg :right)
   (bottom :initform 0 :accessor bottom :initarg :bottom)))

(defun reset-level-position (level-position left top right bottom)
  (setf (left level-position) (min left (left level-position))
        (top level-position) (min top (top level-position))
        (right level-position) (max right (right level-position))
        (bottom level-position) (max bottom (bottom level-position))))

(defvar *max-y*)

(defvar *min-y*)

(defvar *max-x*)

(defvar *min-x*)

(defvar *max-width*)

(defvar *max-generation-sizes*)

(defvar *min-left-outline*)

(defclass outline ()
  ((value :accessor outline-value :initarg :value)
   (history :accessor outline-history :initarg :history)))

(defun clear-tree (r i)
  (setf (web-onto-name r) nil
        (offset r) 0
        (parent-link r) nil
        (left-outline r) nil
        (right-outline r) nil
        )
  (mapcar #'(lambda (x) (clear-tree x i))
          (funcall i r)))


(defmethod generation-size ((orientation (eql :vertical))
                                     parent child)
  (declare (ignore child))
  (if parent
      (tree-height parent)
      0))

(defmethod generation-size ((orientation (eql :horizontal))
                                     parent child)
  (declare (ignore child))
  (if parent
      (tree-width parent)
      0))

(defun horizontal-p (orientation)
  (eq orientation :horizontal))

(defvar *default-generation-separation* 50)

(defvar *default-within-generation-separation* 6)

(defvar *default-tree-start-x* 5)

(defvar *default-tree-start-y* 5)

;;;pretty-draw-tree takes the root object, a print function, and a function
;;;to find the children of a node
(defun pretty-draw-tree (root-object object-printer inferior-producer
                                     &key
                                     (tree-start-x *default-tree-start-x*)
                                     (tree-start-y *default-tree-start-y*)
                                     (position :centre-x)
                                     (stream *standard-output*)
                                     (key (function identity))
                                     (orientation :vertical)
                                     cutoff-depth
                                     (generation-separation
                                      *default-generation-separation*)
                                     (within-generation-separation
                                      *default-within-generation-separation*)
                                     clear-inferior-producer)
  (let ((*max-y* 0) (*max-x* 0) (*min-x* 0) (*max-width* 0)
	(*min-y* 0) (*max-generation-sizes* (make-hash-table)))
    (declare (special *max-x* *min-x* *max-y* *min-y* *max-width*
                      *max-generation-sizes*))
    (shape-tree orientation
                root-object
		inferior-producer key
		within-generation-separation
		1 cutoff-depth 
                (if (horizontal-p orientation)
		    #'tree-height
		    #'tree-width)
                nil)
    (when clear-inferior-producer
      (funcall clear-inferior-producer))
    (multiple-value-bind (offset next-generation-position)
        (tree-starts orientation root-object tree-start-x tree-start-y
                     position
                     (min-left-outline root-object 1))
      (pretty-draw-nodes orientation root-object root-object inferior-producer
		         object-printer 
		         stream 
		         generation-separation
                         within-generation-separation
		         offset
		         next-generation-position
                         1))
    (values *min-x* *min-y* *max-x* *max-y*)))

(defmethod tree-starts ((orientation (eql :vertical))
                        root-object tree-start-x tree-start-y
			position min-left-outline)
  (declare (ignore root-object))
  (values
   (- tree-start-x (if (eq position :centre-x)
		       0
		       min-left-outline))
   tree-start-y))

(defmethod tree-starts ((orientation (eql :horizontal))
                        root-object tree-start-x tree-start-y
			position min-top-outline)
  (declare (ignore root-object))
  (values
   tree-start-x
   (- tree-start-y (if (eq position :centre-x)
		       0
		       0;;need to change one day
                       ))))

(defmethod new-tree-object-position ((orientation (eql :vertical))
                                     object next-generation-position
                                     within-generation-position)
  (new-position object within-generation-position next-generation-position))

(defmethod new-tree-object-position ((orientation (eql :horizontal))
                                     object next-generation-position
                                     within-generation-position)
  (new-position object next-generation-position within-generation-position))

(defmethod maybe-add-level-position (orientation
                                     root depth left top right bottom)
  (declare (ignore orientation))
  (let ((level-position (get-level-position root depth)))
    (if level-position
        (reset-level-position level-position left top right bottom)
        (push (cons depth (make-instance 'level-position
                                         :left left
                                         :top top :right right :bottom bottom))
	      (level-positions root)))))

(defmethod maybe-add-level-position ((orientation (eql :vertical))
                                     root depth
                                     left top right bottom)
  (call-next-method))

(defun get-level-positions (root depth)
  (let ((level-position (get-level-position root depth)))
    (values (left level-position) (top level-position) (right level-position)
            (bottom level-position))))

(defun get-level-position (root depth)
  (cdr (assoc depth (level-positions root))))

(defun reset-maxes (left top right bottom max-width)  
  (setf *max-y* (max *max-y* bottom)
	*min-y* (min *min-y* top)
	*min-x* (min *min-x* left)
	*max-x* (max *max-x* right)
        *max-width* (max *max-width* max-width)))

(defmethod pretty-draw-nodes (orientation
                              root
                              object inferior-producer object-printer 
			      stream
			      generation-separation
                              within-generation-separation
			      offset
			      next-generation-position
			      depth)
  (let* ((children (funcall inferior-producer object))
	 (within-generation-position (+ offset (offset object))))
    (new-tree-object-position orientation object next-generation-position
                              within-generation-position)
    (multiple-value-bind (left top right bottom)
        (tree-edges object)
      (reset-maxes left top right bottom (- right left))
      (maybe-add-level-position orientation root depth left top right bottom))
    (setf (level object) depth
          next-generation-position
	  (+ next-generation-position
	     (generation-gap generation-separation depth orientation)))
    (mapc #'(lambda (child)
              (pretty-draw-nodes orientation root
                                 child inferior-producer object-printer
				 stream
                                 generation-separation
				 within-generation-separation
				 within-generation-position
				 next-generation-position
                                 (1+ depth))
              ;;(draw-link child object orientation stream)
              )
          children)
    (funcall object-printer object stream)))

(defun shape-children (children current-depth last-child)
  (let (centre-offset)  
    (mapc #'(lambda (left-child right-child)
	      ;;;align left and right child because they
	      ;;;we use the offsets of ancestors in our
	      ;;;overlap comparison
	      (setf (offset right-child) (offset left-child))
	      (let ((overlap (max-overlap (1+ current-depth) left-child
					  right-child)))
	        (setf (offset right-child)
		      (+ (offset left-child)
		         overlap))
	        (reset-right-outlines left-child right-child
				      (1+ current-depth))))
	  children (cdr children))
    (setf centre-offset
	  (round
	   (/ (offset last-child)
	      2)))
    (mapc #'(lambda (child)
	      (setf (offset child) (- (offset child)
				      centre-offset)))
	  children)))

(defun reset-max-generation-sizes (orientation parent child current-depth)
  (let ((current (gethash current-depth *max-generation-sizes*))
        (new (generation-size orientation parent child)))
    (when (or (not current)
              (< current new))
      (setf (gethash current-depth *max-generation-sizes*)
            new))))

(defmethod generation-gap (generation-gap current-depth (orientation (eql :vertical)))
  (max generation-gap (or (gethash current-depth *max-generation-sizes*)
                          0)))

(defmethod generation-gap (generation-gap current-depth (orientation (eql :horizontal)))
  (+ generation-gap (or (gethash current-depth *max-generation-sizes*)
                          0)))

(defmethod shape-tree (orientation object inferior-producer key
			           within-generation-separation
			           current-depth
			           cutoff-depth object-dimension-function
                                   ancestors)
  (unless (and cutoff-depth (> current-depth cutoff-depth))
    (let* ((children (funcall inferior-producer object))
           (object-dimension (+ within-generation-separation
                                (funcall object-dimension-function object)))
           ;;(offset (tree-node-offset object orientation))
           (last-child (car (last children))))
      (reset-max-generation-sizes orientation
                                  (car ancestors) object (1- current-depth))
      (cond (children
	     (mapc #'(lambda (child)
		       (shape-tree orientation child inferior-producer key
				   within-generation-separation
				   (1+ current-depth)
				   cutoff-depth
				   object-dimension-function
				   (cons object ancestors)))
		   children)
	     (shape-children children current-depth last-child)
	     (reset-parent current-depth 
			   (car children) last-child
			   object (cons object ancestors)
			   within-generation-separation
			   orientation object-dimension-function))
            (t (make-left-outline object 0 ;;(- offset)
                                  (cons object ancestors)
			          current-depth)
               (make-right-outline object object-dimension
                                   ;;(- object-dimension offset)
                                   (cons object ancestors)
			           current-depth))))))

(defun reset-parent (current-depth left-child right-child parent
                                   ancestors within-generation-separation
                                   orientation object-dimension-function)
  (let* ((parent-width (+ within-generation-separation
			  (funcall object-dimension-function parent)))
	 ;;(offset (tree-node-offset parent orientation))
         )
    (setf (left-outline parent) (left-outline left-child)
          (right-outline parent) (right-outline right-child))
    (add-left-outline parent 0 ;;(- offset)
                      ancestors current-depth)
    (add-right-outline parent parent-width ;;(- parent-width offset)
                       ancestors current-depth)))

(defun make-left-outline (object left-outline-value left-outline-history
				 current-depth)
  (add-left-outline object left-outline-value left-outline-history
				current-depth)
  )

(defun add-left-outline (object left-outline-value left-outline-history
				current-depth)  
  (push (cons current-depth
              (make-instance 'outline
		       :value 
		       left-outline-value
		       :history left-outline-history))
        (left-outline object))
  )


(defun make-right-outline (object right-outline-value right-outline-history
				  current-depth)
  (add-right-outline object right-outline-value right-outline-history
				  current-depth))

(defun add-right-outline (object right-outline-value right-outline-history
				  current-depth)
  (push (cons current-depth
              (make-instance 'outline
		       :value 
		       right-outline-value
		       :history right-outline-history))
        (right-outline object))
  )


(defun get-left-outline (object current-depth)
  (cdr (assoc current-depth (left-outline object) :test #'=)))

(defun get-right-outline (object current-depth)
  (cdr (assoc current-depth (right-outline object) :test #'=)))

(defun get-left-outline-value (object current-depth)
  (outline-value (get-left-outline object current-depth)))

(defun get-right-outline-value (object current-depth)
  (outline-value (get-right-outline object current-depth)))

(defun overlap (left-outline right-outline)
  (- (calculated-outline-value left-outline)
     (calculated-outline-value right-outline)))

(defun calculated-outline-value (outline)
  (internal-calculated-outline-value (outline-value outline)
                                     (outline-history outline)))

(defun internal-calculated-outline-value (value history)
  (+ value
     (apply #'+ (mapcar #'offset history))))

(defun max-overlap (current-depth left-child right-child)
  (do* ((current-depth current-depth (1+ current-depth))
        (left (get-right-outline left-child current-depth)
              (get-right-outline left-child current-depth))
        (right (get-left-outline right-child current-depth)
               (get-left-outline right-child current-depth))
        (overlap (if (and left right) (max 0 (overlap left right)) 0)
                 (if (and left right) (max overlap (overlap left right)) overlap)))
       ((or (null left) (null right))
        overlap)))

(defun reset-right-outlines (left-child right-child current-depth)
  (declare (ignore current-depth))
  (nconc (right-outline right-child) (right-outline left-child))
  (nconc (left-outline left-child) (left-outline right-child)))

(defun min-left-outline (node current-depth)
  (do* ((current-depth current-depth (1+ current-depth))
        (left-outline (get-left-outline node current-depth)
		      (get-left-outline node current-depth))
        (min-left-outline (calculated-outline-value left-outline)
                          (if left-outline
			    (min (calculated-outline-value left-outline)
                                 min-left-outline)
                            min-left-outline)))
       ((null left-outline) min-left-outline)))



