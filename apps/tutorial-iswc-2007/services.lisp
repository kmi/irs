(defpackage :tutorial-iswc-2007-services
  (:use :common-lisp :irs.publisher)
  (:export #:publish))

(in-package tutorial-iswc-2007-services)

(defun publish ()
  (publisher:clear-all-services)
  (publisher:irs-wsmo-web-service-registration 
   cl-user::get-content-license cl-user::get-content-license)
  (publisher:irs-wsmo-web-service-registration 
   cl-user::get-content-url cl-user::get-content-url)
  (publish-all-wsmo-services "wsmo"))

(defvar *content-license*
  "THE WORK IS PROVIDED UNDER THE TERMS OF THIS CREATIVE COMMONS
PUBLIC LICENSE (CCPL OR LICENSE). THE WORK IS PROTECTED BY COPYRIGHT
AND/OR OTHER APPLICABLE LAW. ANY USE OF THE WORK OTHER THAN AS
AUTHORIZED UNDER THIS LICENSE OR COPYRIGHT LAW IS PROHIBITED.  BY
EXERCISING ANY RIGHTS TO THE WORK PROVIDED HERE, YOU ACCEPT AND AGREE
TO BE BOUND BY THE TERMS OF THIS LICENSE. THE LICENSOR
(www.funny.co.uk) GRANTS YOU THE RIGHTS CONTAINED HERE IN
CONSIDERATION OF YOUR ACCEPTANCE OF SUCH TERMS AND CONDITIONS.  VISIT
creativecommons.org FOR MORE INFORMATION.  " )

(defun cl-user::get-content-license (content user)
  (declare (ignore content user))
  *content-license*)

(defun cl-user::get-content-url (content user)
  (declare (ignore user content))
  ;; Linus Torvald's Google Tech Talk on Git.
  ;;wayy too geeky!
  "http://www.youtube.com/watch?v=4XpnKHJAok8"
  ;;;football dribbling skills :-)
  "http://youtube.com/watch?v=3me8kvmEA7w")
