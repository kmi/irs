(in-package cl-user)

;; Define the functions to be loaded
(eval-when (eval load)
  (irs-wsmo-web-service-registration 
   super-prereview
   get-content-license)
)