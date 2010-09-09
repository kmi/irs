(load "scripts/irs")
(require :irs-publisher)
(require :travel-publisher)

(defun start-services ()
  (set-irs-home :image)
  (publisher:start-publisher)
  (irs::initialise)
  (irs:initialise-application :travel)
  (travel:publish)
  (format t "~%Publisher seems to be running..."))

(deliver 'start-services
	 (executable-name "travel-publisher" :console) 0
	 :multiprocessing t :interface nil
         :keep-editor t :editor-commands-to-keep :all-groups
	 :keep-pretty-printer t :packages-to-keep :all
	 :product-name "travel publisher")

(quit)
