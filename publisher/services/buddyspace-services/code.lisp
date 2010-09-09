(in-package cl-user)

(defun Generate-Login-Command ()
  ;;(setf a 'here)
  'connect)

(defun Generate-presence-Command ()
  'presences)

(defun Generate-logout-Command ()
  'disconnect)

(defun get-foaf-file (url)
  (utilities:http-request
   url :proxy (if irs:*proxy-host*
                  (format nil "http://~A:~A/" irs:*proxy-host* irs:*proxy-port*)
                  nil)))

(eval-when (eval load)
  (irs-wsmo-web-service-registration 
   buddyspace-goals2
   generate-login-command-web-service)
  (irs-wsmo-web-service-registration 
   buddyspace-goals2
   Generate-presence-Command-web-service)
  (irs-wsmo-web-service-registration 
   buddyspace-goals2
   get-foaf-file-web-service)
  (irs-wsmo-web-service-registration 
   buddyspace-goals2
   Generate-logout-Command-web-service)
  (irs-wsmo-web-service-registration 
   buddyspace-goals2
   Generate-Login-Command-Goal-To-User-Name-Command-Goal-Mediation-Service-Web-Service)
  (irs-wsmo-web-service-registration 
   buddyspace-goals2
   Generate-Presence-Command-Goal-To-Session-Id-Command-Goal-Mediation-Service-Web-Service)
  (irs-wsmo-web-service-registration 
   buddyspace-goals2
   Generate-Logout-Command-Goal-To-Session-Id-Command-Goal-Mediation-Service-Web-Service)
)

