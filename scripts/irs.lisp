;;; Copyright © 2007-2010 The Open University

(in-package #:cl-user)

#+:lispworks
(setf system:*stack-overflow-behaviour* :warn)

;;; Lispworks doesn't seem to guess UTF-8 files properly, which is a
;;; shame since most files in the IRS are UTF-8, and the rest are
;;; heading that way.  We override its encoding detection for files,
;;; and tell the editor to use UTF-8.

#+:lispworks
(lw:set-default-character-element-type 'lw:simple-char)

;; We use UTF-8 encoding for files in the IRS.  So, if the pathname
;; starts with the path in *irs-home*, use UTF-8.  Punt the rest to
;; the next guesser function.
#+:lispworks
(defun irs-file-encoding (pathname ef-spec buffer length)
  (let ((home (pathname-directory (pathname *irs-home*))))
    (if (= (length home)
	   (mismatch home (pathname-directory pathname)
		     :test 'string=))
	(list :utf-8 :eol-style :lf)
	ef-spec)))

#+:lispworks
(pushnew 'irs-file-encoding sys:*file-encoding-detection-algorithm*)

#+:lispworks
(setf (editor:variable-value "output format default") '(:utf-8)
      (editor:variable-value "input format default") '(:utf-8))

(defvar *irs-home*
  (let ((pathname (format nil "~A" *load-truename*)))
    (subseq pathname 0
            (- (length pathname) (length "/scripts/irs.lisp")))))

(defun from-irs-home (partial-path)
  (format nil "~A/~A" *irs-home* partial-path))

(setf (logical-pathname-translations "irs")
      `(("irs:**;*.*.*" ,(from-irs-home "**/*.*"))))


;;; {{{ QuickLisp

(let ((quicklisp (concatenate 'string *irs-home* "/quicklisp.lisp")))
  (when (probe-file quicklisp)
    (load quicklisp)
    (push :quicklisp *features*)))

#+:quicklisp
(let ((quicklisp-setup (quicklisp-quickstart::qmerge "setup.lisp")))
  (if (probe-file quicklisp-setup)
      (load quicklisp-setup)
    (quicklisp-quickstart:install)))

#+:quicklisp
(ql:quickload "asdf")
;;; }}}

;;;; ASDF system definition setup.

(unless (find-package '#:asdf)
  (load (translate-logical-pathname
	 (logical-pathname "irs:external;cl-asdf;asdf.lisp"))))

(defun register-asdf-package (path &key (where :start) (force nil))
  "Add a package to the ASDF registry.  PATH is a logical pathname.
WHERE can be :start or :end.  If FORCE is true, add the PATH even if
it is already in the list. "
  (when (or force
	    (not (member path asdf:*central-registry* :test 'equal)))
    (ecase where
      (:start (push path asdf:*central-registry*))
      (:end (setf asdf:*central-registry*
		  (append asdf:*central-registry* (list path)))))
    asdf:*central-registry*))

;;; Extend Lispworks' REQUIRE function to look for ASDFs first, and
;;; fall back to looking for Lispworks modules.
#+:lispworks
(sys::without-warning-on-redefinition
  (let ((system-require (symbol-function 'require)))
    (defun require (module &optional pathname)
      (if (or pathname (not (asdf:find-system module nil)))
	  (funcall system-require module pathname)
	  (asdf:operate 'asdf:load-op module)))))

(register-asdf-package (translate-logical-pathname
			(logical-pathname "irs:src;")))
(register-asdf-package (translate-logical-pathname
			(logical-pathname "irs:publisher;")))
(register-asdf-package (translate-logical-pathname
			(logical-pathname "irs:tests;")))

(defun directory-contents (logical-dirname)
  (directory (logical-pathname
	      #-:clisp logical-dirname
	      #+:clisp (format nil "~A;" logical-dirname))))

;;; Check in irs/apps for entries, and register them.
(dolist (app (directory-contents "irs:apps;*"))
  (register-asdf-package app))

;;; Prepare to load OCML from external/ocml
(register-asdf-package (translate-logical-pathname
			(logical-pathname "irs:external;ocml;")))

(defvar *lisp-suffix* "lisp")

(defun set-irs-home (irs-home)
  (let (ocml-home)
    (if (eq irs-home :image)
        ;; If :image is specified as IRS-HOME, use the directory the
        ;; image is in.
        (let* ((image-path (format nil "~A" (pathname-location (lisp-image-name))))
               (pwd
                #+:cocoa (subseq image-path 0
                                 (position #\/ image-path :start 0 :end (search "/Contents/MacOS/" image-path)
                                           :from-end t))
                #-:cocoa (subseq image-path 0 (- (length image-path) 1))))
          (setf irs-home pwd)
          (setf ocml-home (format nil "~A/external/ocml" irs-home)))
        (setf ocml-home
              (let ((path (format nil "~A" (asdf:component-pathname (asdf:find-system :ocml)))))
                (subseq path 0 (- (length path) 1)))))
    (setf *irs-home* irs-home)
    (setf (logical-pathname-translations "irs")
          `(("irs:**;*.*.*" ,(from-irs-home "**/*.*"))))
    (setf (logical-pathname-translations "ocml")
          `(("ocml:library;basic;*.*.*"
             ,(format nil
                      #+(or :macosx :unix) "~A/library/basic/*.*"
                      #+:win32 "~A\\library\\basic\\*.*"
                      ocml-home))
          ("ocml:library;**;*.*.*" ,(from-irs-home "ontologies/**/*.*"))))))

;;; We couldn't use SET-IRS-HOME to set *irs-home* earlier because the
;;; definition relies on ASDF already being loaded, which can't be
;;; done until *irs-home* is set...
(set-irs-home *irs-home*)

;;; {{{ Image and deliverable build support
#+:cocoa
(compile-file-if-needed (sys:example-file "configuration/macos-application-bundle") :load t)

(defun executable-name (filename &optional (interface :gui))
  #+:cocoa (ecase interface
		  (:gui (write-macos-application-bundle
			 (from-irs-home (format nil "~A.app" filename)) :document-types nil))
		  (:console filename))
  #-:cocoa (format nil "~A~A" filename  #+:win32 ".exe" #-:win32 ""))
;;; }}}

(eval-when (:load-toplevel :execute)
  (push :webonto *features*)
  ;; New version of OCML doesn't like some of the things IRS makes it
  ;; do, so persuade it.
  (push :irs-ocml-hacks *features*))

;;; Certain components, like Lispweb, WebOnto, CAPI, and a few other
;;; bits, depend heavily on Lispworks.  Porting the required
;;; functionality is not realistic, at least not in the short term, so
;;; we selectively disable it based on the :irs-legacy feature.
#+:lispworks (push :irs-lispworks *features*)

;;; The interface provided by Lispweb is deprecated and not built by
;;; default.  Uncomment the next line to enable it.
;; #+:lispworks(push :irs-use-lispweb *features*)
