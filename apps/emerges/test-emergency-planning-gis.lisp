(in-package user)

;;http://dip.bat.bt.co.uk:8081/axis/services/eccRestCentres?method=&latitude=&longitude=&radiusKM=1

(defun test-emergency-planning-gis ()
  (achieve-goal-through-irs 
   'Emergency-Gis-Goals 'Get-Ecc-Rest-Centres-Goal 
   '(Has-Method "getRestCentresInRadius")
   '(Has-Latitude 51.7479)
   '(Has-Longitude 0.44160)
   '(Has-Radius 1)))