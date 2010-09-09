;;; Makes demo deliverable of the IRS Server.

(load-all-patches)

(load "scripts/irs.lisp")

(require :irs)

(require :math)
(require :trusted-travel)

(setf ocml::*muffle-warnings* t)

(defun startup ()
  (set-irs-home :image)
  (irs:start)
  (dolist (app '(:math :trusted-travel))
    (irs:initialise-application app)
    (irs:start-application app))
  (format t "~%IRS server seems to be running...~%")
  (mp::default-listener-function))

(setf *deliver-mode* t)

(deliver 'startup (executable-name "irs-demo-server" :console) 0
         :product-name "irs-demo-server"
         :multiprocessing t :interface nil
         :keep-editor t
         :editor-commands-to-keep :all-groups
         :keep-pretty-printer t
         :packages-to-keep :all
         :keep-debug-mode :keep-packages)

(quit)
