;;; Makes tutorial image and deliverable of the IRS Server.

(load-all-patches)

(load "scripts/irs.lisp")

(require :irs)

(require :tutorial-iswc-2007)

(setf ocml::*muffle-warnings* t)

(defun startup ()
  (set-irs-home :image)
  (irs:start)
  (irs:initialise-application :tutorial-iswc-2007)
  (irs:start-application :tutorial-iswc-2007)
  (format t "~%IRS server seems to be running...~%")
  (mp::default-listener-function))

(save-image (executable-name "tutorial-iswc-2007-server-image")
            :console :input :environment t)

(setf *deliver-mode* t)

(deliver 'startup (executable-name "tutorial-iswc-2007-server" :console) 0 
         :product-name "iswc-2007-tutorial-irs-server"
         :multiprocessing t :interface nil
         :keep-editor t
         :editor-commands-to-keep :all-groups
         :keep-pretty-printer t
         :packages-to-keep :all
         :keep-debug-mode :keep-packages)

(quit)
