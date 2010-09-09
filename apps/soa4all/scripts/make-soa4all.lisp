(load-all-patches)

(load "scripts/irs.lisp")

(require :irs)

(require :soa4all)

(setf ocml::*muffle-warnings* t)

(defun startup ()
  (set-irs-home :image)
  (irs:start)
  (irs:initialise-application :soa4all)
  (irs:start-application :soa4all)
  (format t "~%IRS server seems to be running...~%")
  (mp::default-listener-function))

(setf *deliver-mode* t)

(deliver 'startup (executable-name "soa4all-irs" :console) 0
         :product-name "soa4all-irs"
         :multiprocessing t :interface nil
         :keep-editor t
         :editor-commands-to-keep :all-groups
         :keep-pretty-printer t
         :packages-to-keep :all
         :keep-debug-mode :keep-packages)

(quit)
