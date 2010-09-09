(load-all-patches)
(load "scripts/irs")
(require :irs-publisher)
(require :tutorial-iswc-2007-services)

(setf ocml::*muffle-warnings* t)

(defun startup ()
  (set-irs-home :image)
  (publisher:start-publisher)
  (webonto:initialise)
  (tutorial-iswc-2007-services:publish)
  (format t "~%IRS publisher seems to be running...~%"))

(save-image (executable-name "tutorial-iswc-2007-publisher-image")
            :console :input :environment t)

(deliver 'startup
         (executable-name "tutorial-iswc-2007-publisher" :console) 0
         :multiprocessing t :interface nil
         :keep-editor t :editor-commands-to-keep :all-groups
         :keep-pretty-printer t :packages-to-keep :all
         :product-name "ISWC 2007 tutorial services")

(quit)
