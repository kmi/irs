(in-package #:lhdl)

(defparameter *image-size* 400
  "Pixels per side of the image display.")

(defun draw-gui-page ()
  (web:with-html
    (:html
     (:head (:title "Living Human Digital Library")
	    ((:base :id "baseid" :href (str (irs.web:base-href))))
            ((:link :rel "icon" :type "image/png" :href "lhdl/www/lhdl-favicon.png"))
            ;; Yahoo's UI stuff
            ((:link :rel "stylesheet" :type "text/css"
                    :href (str (irs.web:yui-file-url "build/container/assets/skins/sam/container.css"))))
            ((:link :rel "stylesheet" :type "text/css"
                    :href (str (irs.web:yui-file-url "build/button/assets/skins/sam/button.css"))))
            ((:link :rel "stylesheet" :type "text/css"
                    :href (str (irs.web:yui-file-url "build/menu/assets/skins/sam/menu.css"))))
            ((:script :type "text/javascript"
                      :src (str (irs.web:yui-file-url "build/yahoo-dom-event/yahoo-dom-event.js"))))
            ((:script :type "text/javascript"
                      :src (str (irs.web:yui-file-url "build/element/element-min.js"))))
            ((:script :type "text/javascript"
                      :src (str (irs.web:yui-file-url "build/container/container-min.js"))))
            ((:script :type "text/javascript"
                      :src (str (irs.web:yui-file-url "build/container/container_core-min.js"))))
            ((:script :type "text/javascript"
                      :src (str (irs.web:yui-file-url "build/button/button-min.js"))))
            ((:script :type "text/javascript"
                      :src (str (irs.web:yui-file-url "build/menu/menu-min.js")))))
     (:body
      ((:script :src "irs/javascript/irs-api.js" :type "text/javascript"))
      ((:script :src "lhdl/javascript/gui.js" :type "text/javascript"))
      ((:div :class "yui-skin-sam")
       ((:table)
        (:tr ((:td :colspan 2) (:img :src "lhdl/www/banner.png")))
        (:tr ((:td :colspan 2) (:div :id "menubarContainer")))
        (:tr (:td :id "ops-panel")
             (:td :id "image-panel" :width *image-size* :height *image-size*))
        (:tr (:td :id "busyBox")
             (:td :id "return-value-panel"))))
      (:script (str "initGUI();"))))))


;; XXX Hunchentoot bizarrely changes #\+ into spaces, so we revert
;; that.
(defun our-url-decode (string)
  (substitute #\+ #\Space (hunchentoot:url-decode string)))

;; Takes a BASE 64 encoded string and mime type, and dumps the decoded
;; value to the HTTP reply stream.
(defun debase64 ()
  (web:with-parameters (:get (mime-type base64))
    (web::write-http-stream
     mime-type (base64:base64-string-to-usb8-array (our-url-decode base64)))))
