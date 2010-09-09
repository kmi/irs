;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

(in-ontology occupation-ontology)

(def-class occupation ()
  ((has-sector :type economic-sector)))

(def-class transportation-occupation (occupation))

(def-class military-occupation (occupation))

(def-class maintenance-occupation (occupation))

(def-class management-occupation (occupation))

(def-class business-occupation (occupation))

(def-class financial-operations-occupation (occupation))

(def-class computer-occupation (occupation))

(def-class mathematical-occupation (occupation))

(def-class architecture-occupation (occupation))

(def-class engineering-occupation (occupation))

(def-class science-occupation (occupation))

(def-class social-science-occupation (occupation))

(def-class community-occupation (occupation))

(def-class social-services-occupation (occupation))

(def-class legal-occupation (occupation))

(def-class education-occupation (occupation))

(def-class training-occupation (occupation))

(def-class library-occupation (occupation))

(def-class arts-occupation (occupation))

(def-class design-occupation (occupation))

(def-class entertainment-occupation (occupation))

(def-class sports-occupation (occupation))

(def-class media-occupation (occupation))

(def-class healthcare-occupation (occupation))

(def-class protective-services-occupation (occupation))

(def-class food-related-occupation (occupation))

(def-class grounds-cleaning-occupation (occupation))

(def-class personal-care-occupation (occupation))

(def-class personal-service-occupation (occupation))

(def-class sales-occupation (occupation))

(def-class office-occupation (occupation))

(def-class administrative-support-occupation (occupation))

(def-class farming-occupation (occupation))

(def-class fishing-occupation (occupation))

(def-class forestry-occupation (occupation))

(def-class construction-occupation (occupation))

(def-class extraction-occupation (occupation))

(def-class installation-occupation (occupation))

(def-class maintenance-and-repair-occupation (occupation))

(def-class production-occupation (occupation))

(def-class academic-related-occupation (occupation))

(def-class government-occupation (occupation))

(def-instance general-manager management-occupation)

(def-instance administrator management-occupation)

(def-instance production-manager management-occupation)

(def-instance company-secretary management-occupation)

(def-instance financial-manager management-occupation)

(def-instance executive-officer management-occupation)

(def-instance transport-manager management-occupation)

(def-instance warehouse-manager management-occupation)

(def-instance shop-manager management-occupation)

(def-instance doctor healthcare-occupation)

(def-instance dentist healthcare-occupation)

(def-instance pharmacist healthcare-occupation)

(def-instance ophthalmic-optician healthcare-occupation)

(def-instance vet healthcare-occupation)

(def-instance nurse healthcare-occupation)

(def-instance midwife healthcare-occupation)

(def-instance radiographer healthcare-occupation)

(def-instance physiotherapist healthcare-occupation)

(def-instance chiropodist healthcare-occupation)

(def-instance medical-technician healthcare-occupation)

(def-instance psychotherapist healthcare-occupation)

(def-instance occupational-therapist healthcare-occupation)

(def-instance speech-therapist healthcare-occupation)

(def-instance enviromental-health-officer healthcare-occupation)

(def-instance dispensing-optician healthcare-occupation)

(def-instance school-teacher education-occupation
  ((has-sector )))

(def-instance lecturer education-occupation)

(def-instance school-inspector education-occupation)

(def-instance education-officer education-occupation)

(def-instance judge legal-occupation)

(def-instance barrister legal-occupation)

(def-instance solicitor legal-occupation)

(def-instance forestry-worker forestry-occupation)

(def-instance lumberjack forestry-occupation)

(def-instance fisherman fishing-occupation)

(def-instance farm-worker farming-occupation)

(def-instance shepherd farming-occupation)

(def-instance tractor-driver farming-occupation)

(def-instance stablehand farming-occupation)

(def-instance lorry-driver transportation-occupation)

(def-instance taxi-driver transportation-occupation)

(def-instance train-inspector transportation-occupation)

(def-instance train-guard transportation-occupation)

(def-instance bus-driver transportation-occupation)

(def-instance bus-conductor transportation-occupation)

(def-instance bus-inspector transportation-occupation)

(def-instance crane-operator transportation-occupation)

(def-instance police-officer protective-services-occupation)

(def-instance fire-officer protective-services-occupation)

(def-instance prison-officer protective-services-occupation)

(def-instance customs-and-excise-officer protective-services-occupation)

(def-instance traffic-warden protective-services-occupation)

(def-instance security-guard protective-services-occupation)

(def-instance armed-forces-officer military-occupation)

(def-instance non-commissioned-officer military-occupation)

(def-instance chartered-accountant financial-operations-occupation)

(def-instance certified-accountant financial-operations-occupation)

(def-instance chef food-related-occupation)

(def-instance waiter food-related-occupation)

(def-instance bar-staff food-related-occupation)

(def-instance hairdresser personal-care-occupation)

(def-instance beautician personal-care-occupation)

(def-instance brick-layer construction-occupation)

(def-instance glazier construction-occupation)

(def-instance painter construction-occupation)

(def-instance decorator construction-occupation)

(def-instance carpet-fitter construction-occupation)

(def-instance scaffolder construction-occupation)

(def-instance secretary office-occupation)

(def-instance personal-assistant office-occupation)

(def-instance typist office-occupation)

(def-instance word-processor-operator office-occupation)

(def-instance receptionist office-occupation)

(def-instance telephonist office-occupation)

(def-instance radio-operator office-occupation)

(def-instance telephone-operator office-occupation)

(def-instance telegraph-operator office-occupation)

(def-instance computer-operator computer-occupation)

(def-instance athelete sports-occupation)

(def-instance sports-official sports-occupation)

(def-instance valuer financial-operations-occupation)

(def-instance underwriter financial-operations-occupation)

(def-instance stockbroker financial-operations-occupation)

(def-instance taxation-expert financial-operations-occupation)

(def-instance conveyancer legal-occupation)

(def-instance barristers-clerk legal-occupation)

(def-instance legal-executive legal-occupation)

(def-instance architect architecture-occupation)

(def-instance town-planner architecture-occupation)

(def-instance surveyor architecture-occupation)

(def-instance librarian administrative-support-occupation)

(def-instance curator administrative-support-occupation)

(def-instance clergy community-occupation)

(def-instance social-worker community-occupation)

(def-instance probation-officer community-occupation)

(def-instance welfare-worker community-occupation)

(def-instance community-worker community-occupation)

(def-instance youth-worker community-occupation)

(def-instance artist arts-occupation)

(def-instance actor arts-occupation)

(def-instance director arts-occupation)

(def-instance musician arts-occupation)

(def-instance photographer arts-occupation)

(def-instance writer arts-occupation)

(def-instance designer arts-occupation)

(def-instance designer design-occupation)

(def-instance management-consultant business-occupation)

(def-instance economist business-occupation)

(def-instance statistician mathematical-occupation)

(def-instance actuary mathematical-occupation)

(def-instance librarian library-occupation)

(def-instance curator library-occupation)

(def-instance computer-programmer computer-occupation)

(def-instance computer-analyist computer-occupation)

(def-instance engineering-technician engineering-occupation)

(def-instance scientist science-occupation)

(def-instance technologist science-occupation)

(def-instance laboratory-technician science-occupation)

(def-instance engineer engineering-occupation)

(def-instance caretaker maintenance-occupation)

(def-instance sales-assistant sales-occupation)

(def-instance check-out-operator sales-occupation)

(def-instance forecourt-attendant sales-occupation)

(def-instance market-trader sales-occupation)

(def-instance door-to-door-salesperson sales-occupation)

(def-instance insurance-agent sales-occupation)

(def-instance auctioneer sales-occupation)

(def-instance demonstrator sales-occupation)

(def-instance sales-representative sales-occupation)

(def-instance purchasing-officer business-occupation)

(def-instance importer business-occupation)

(def-instance exporter business-occupation)

(def-instance broker business-occupation)

(def-instance caretaker maintenance-and-repair-occupation)

(def-instance construction-worker construction-occupation)

(def-instance plant-operator production-occupation)

(def-instance machine-operator production-occupation)

(def-instance assembler production-occupation)

(def-instance lineworker production-occupation)

(def-instance inspector production-occupation)

(def-instance packer production-occupation)

(def-instance baker food-related-occupation)

(def-instance fishmonger food-related-occupation)

(def-instance butcher food-related-occupation)

(def-instance administrative-officer administrative-support-occupation)

(def-instance administrative-assistant administrative-support-occupation)

(def-instance draughtsperson design-occupation)

(def-instance quantity-surveyor construction-occupation)

(def-instance building-technician construction-occupation)

(def-instance architectural-technician architecture-occupation)

(def-instance building-inspector construction-occupation)

(def-instance train-driver transportation-occupation)

(def-instance tracer design-occupation)

(def-instance drawing-office-assistant design-occupation)

(def-instance office-machine-operator office-occupation)

(def-instance filing-clerk administrative-support-occupation)

(def-instance general-clerk administrative-support-occupation)

(def-instance clerical-officer administrative-support-occupation)

(def-instance clerical-assistant administrative-support-occupation)

(def-instance numerical-clerk administrative-support-occupation)

(def-instance cashier administrative-support-occupation)

(def-instance storekeeper administrative-support-occupation)

(def-instance stores-clerk administrative-support-occupation)

(def-instance despatch-clerk administrative-support-occupation)

(def-instance vocational-trainer training-occupation)

(def-instance driving-instructer training-occupation)

(def-instance metal-machiner engineering-occupation)

(def-instance fitter engineering-occupation)

(def-instance fitter production-occupation)

(def-instance metal-machiner production-occupation)

(def-instance instrument-maker production-occupation)

(def-instance tool-maker production-occupation)

(def-instance lathe-fitter production-occupation)

(def-instance computer-engineer engineering-occupation)

(def-instance telephone-fitter installation-occupation)

(def-instance information-officer administrative-support-occupation)

(def-instance social-scientist social-science-occupation)

(def-instance professor education-occupation
  ((has-sector )))

(def-instance research-assistant education-occupation
  ((has-sector )))

(def-instance research-fellow education-occupation
  ((has-sector )))

(def-instance senior-research-fellow education-occupation
  ((has-sector )))

(def-instance researcher education-occupation
  ((has-sector )))

(def-instance senior-lecturer education-occupation
  ((has-sector )))

(def-instance reader education-occupation
  ((has-sector )))

(def-instance academic education-occupation
  ((has-sector )))

(def-instance business-manager management-occupation
  ((has-sector )))

(def-instance system-manager management-occupation
  ((has-sector )))

(def-instance senior-manager management-occupation
  ((has-sector )))

(def-instance multimedia-designer design-occupation
  ((has-sector )))

(def-instance non-clinical-employee-of-health-care-organization healthcare-occupation
  ((has-sector )))

(def-instance project-officer academic-related-occupation
  ((has-sector )))

(def-instance assistant-project-officer academic-related-occupation
  ((has-sector )))

(def-instance politician government-occupation
  ((has-sector )))
