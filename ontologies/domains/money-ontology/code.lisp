;;; Mode: Lisp; Package: cl-user

(in-package ocml)

(defvar *currency-rates* (make-hash-table :test #'equal))

(eval-when (eval load)  
  (setf (gethash '(usdollar euro) *currency-rates*) 1.09706         
        (gethash '(usdollar pound) *currency-rates*) 0.689562        
        (gethash '(usdollar lira) *currency-rates*) 2124.03       
        (gethash '(usdollar franc) *currency-rates*)7.19565     
        (gethash '(usdollar mark) *currency-rates*) 2.14549))


(defun exchange_rate_provider (source-currency target-currency)  
  (cond ((eq source-currency 'usdollar)        
         (or (gethash (list source-currency target-currency)        
                      *currency-rates*)          
             0))     
        ((eq target-currency 'usdollar)      
         (let ((inverse-rate (gethash (list target-currency source-currency)                     
                                      *currency-rates*)))        
           (if inverse-rate          
             (/ 1 inverse-rate)          
             0)))     
        (t         
         (let ((rate1 (gethash (list 'usdollar source-currency)                     
                               *currency-rates*))              
               (rate2 (gethash (list 'usdollar target-currency)             
                               *currency-rates*)))         
           (if (and rate1 rate2)        
             (/ rate2 rate1)           
             0)))))





