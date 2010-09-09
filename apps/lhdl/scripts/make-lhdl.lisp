;;; Makes tutorial image and deliverable of the IRS Server.

(load-all-patches)

(load "scripts/irs.lisp")

(require :irs)

(require :lhdl)

(setf ocml::*muffle-warnings* t)

(defun startup ()
  (set-irs-home :image)
  (irs:start)
  (irs:initialise-application :lhdl)
  (irs:start-application :lhdl)
  (format t "~%IRS server seems to be running...~%")
  (mp::default-listener-function))

(setf *deliver-mode* t)

(deliver 'startup (executable-name "lhdl-irs" :console) 0
         :product-name "lhdl-irs"
         :multiprocessing t :interface nil
         :keep-editor t
         :editor-commands-to-keep :all-groups
         :keep-pretty-printer t
         :packages-to-keep :all
         :keep-debug-mode :keep-packages)

(quit)
