(in-package cl-user)

(eval-when (eval load)
  (irs-wsmo-web-service-registration shipper-producer
                                     ws-get-product-details)
  )

(eval-when (eval load)
  (irs-wsmo-web-service-registration shipper-producer
                                     ws-subtract)
  )

(eval-when (eval load)
  (irs-wsmo-web-service-registration shipper-producer
                                     ws-subtract-right)
  )

(eval-when (eval load)
  (irs-wsmo-web-service-registration shipper-producer
                                     ws-arrange-shipping)
  )

(eval-when (eval load)
  (irs-wsmo-web-service-registration shipper-producer
                                     ws-arrange-shipping-size)
  )

(eval-when (eval load)
  (irs-wsmo-web-service-registration shipper-producer
                                     ws-purchase-product)
  )

(eval-when (eval load)
  (irs-wsmo-web-service-registration shipper-producer
                                     ws-purchase-product-quote-id)
  )

(eval-when (eval load)
  (irs-wsmo-web-service-registration shipper-producer
                                     ws-purchase-product-extra-cost)
  )


(defun get-product-details (product-id)
  (cons '100 (cons 'C1 (cons (random 10000000) nil))))

(defun subtract (left right)
  (- left right))

(defun subtract-right (product-details)
  (car product-details))

(defun arrange-shipping (destination size funds)
  (let ((change (- funds 10)))
    (cond ((> change 0) '10)
          (t change))))

;;(defun arrange-shipping (destination size funds)
;;  (let ((cost (- funds 10)))
;;        (cond ((> cost 0) (cons 'cost (cons cost nil)))
;;              (t (cons 'shortfall (cons (- 0 cost) nil))))))

;;(defun arrange-shipping (destination size funds)
;;  (- funds 10))

(defun arrange-shipping-size (product-details)
  (cadr product-details))

(defun purchase-product (quote-id extra-cost)
  (+ 100 extra-cost))

(defun purchase-product-quote-id (product-details)
  (caddr product-details))

(defun purchase-product-extra-cost (cost)
  (identity cost))