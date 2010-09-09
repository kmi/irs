;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

(in-ontology occupation-ontology)

(def-class occupation ()
  ((has-sector :type economic-sector-type)))

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

(def-instance general-manager-occupation management-occupation)

(def-instance administrator-occupation management-occupation)

(def-instance production-manager-occupation management-occupation)

(def-instance company-secretary-occupation management-occupation)

(def-instance financial-manager-occupation management-occupation)

(def-instance executive-officer-occupation management-occupation)

(def-instance transport-manager-occupation management-occupation)

(def-instance warehouse-manager-occupation management-occupation)

(def-instance shop-manager-occupation management-occupation)

(def-instance doctor-occupation healthcare-occupation)

(def-instance dentist-occupation healthcare-occupation)

(def-instance pharmacist-occupation healthcare-occupation)

(def-instance ophthalmic-optician-occupation healthcare-occupation)

(def-instance vet-occupation healthcare-occupation)

(def-instance nurse-occupation healthcare-occupation)

(def-instance midwife-occupation healthcare-occupation)

(def-instance radiographer-occupation healthcare-occupation)

(def-instance physiotherapist-occupation healthcare-occupation)

(def-instance chiropodist-occupation healthcare-occupation)

(def-instance medical-technician-occupation healthcare-occupation)

(def-instance psychotherapist-occupation healthcare-occupation)

(def-instance occupational-therapist-occupation healthcare-occupation)

(def-instance speech-therapist-occupation healthcare-occupation)

(def-instance enviromental-health-officer-occupation healthcare-occupation)

(def-instance dispensing-optician-occupation healthcare-occupation)

(def-instance school-teacher-occupation education-occupation
  ((has-sector )))

(def-instance lecturer-occupation education-occupation)

(def-instance school-inspector-occupation education-occupation)

(def-instance education-officer-occupation education-occupation)

(def-instance judge-occupation legal-occupation)

(def-instance barrister-occupation legal-occupation)

(def-instance solicitor-occupation legal-occupation)

(def-instance forestry-worker-occupation forestry-occupation)

(def-instance lumberjack-occupation forestry-occupation)

(def-instance fisherman-occupation fishing-occupation)

(def-instance farm-worker-occupation farming-occupation)

(def-instance shepherd-occupation farming-occupation)

(def-instance tractor-driver-occupation farming-occupation)

(def-instance stablehand-occupation farming-occupation)

(def-instance lorry-driver-occupation transportation-occupation)

(def-instance taxi-driver-occupation transportation-occupation)

(def-instance train-inspector-occupation transportation-occupation)

(def-instance train-guard-occupation transportation-occupation)

(def-instance bus-driver-occupation transportation-occupation)

(def-instance bus-conductor-occupation transportation-occupation)

(def-instance bus-inspector-occupation transportation-occupation)

(def-instance crane-operator-occupation transportation-occupation)

(def-instance police-officer-occupation protective-services-occupation)

(def-instance fire-officer-occupation protective-services-occupation)

(def-instance prison-officer-occupation protective-services-occupation)

(def-instance customs-and-excise-officer-occupation protective-services-occupation)

(def-instance traffic-warden-occupation protective-services-occupation)

(def-instance security-guard-occupation protective-services-occupation)

(def-instance armed-forces-officer-occupation military-occupation)

(def-instance non-commissioned-officer-occupation military-occupation)

(def-instance chartered-accountant-occupation financial-operations-occupation)

(def-instance certified-accountant-occupation financial-operations-occupation)

(def-instance chef-occupation food-related-occupation)

(def-instance waiter-occupation food-related-occupation)

(def-instance bar-staff-occupation food-related-occupation)

(def-instance hairdresser-occupation personal-care-occupation)

(def-instance beautician-occupation personal-care-occupation)

(def-instance brick-layer-occupation construction-occupation)

(def-instance glazier-occupation construction-occupation)

(def-instance painter-occupation construction-occupation)

(def-instance decorator-occupation construction-occupation)

(def-instance carpet-fitter-occupation construction-occupation)

(def-instance scaffolder-occupation construction-occupation)

(def-instance secretary-occupation office-occupation)

(def-instance personal-assistant-occupation office-occupation)

(def-instance typist-occupation office-occupation)

(def-instance word-processor-operator-occupation office-occupation)

(def-instance receptionist-occupation office-occupation)

(def-instance telephonist-occupation office-occupation)

(def-instance radio-operator-occupation office-occupation)

(def-instance telephone-operator-occupation office-occupation)

(def-instance telegraph-operator-occupation office-occupation)

(def-instance computer-operator-occupation computer-occupation)

(def-instance athelete-occupation sports-occupation)

(def-instance sports-official-occupation sports-occupation)

(def-instance valuer-occupation financial-operations-occupation)

(def-instance underwriter-occupation financial-operations-occupation)

(def-instance stockbroker-occupation financial-operations-occupation)

(def-instance taxation-expert-occupation financial-operations-occupation)

(def-instance conveyancer-occupation legal-occupation)

(def-instance barristers-clerk-occupation legal-occupation)

(def-instance legal-executive-occupation legal-occupation)

(def-instance architect-occupation architecture-occupation)

(def-instance town-planner-occupation architecture-occupation)

(def-instance surveyor-occupation architecture-occupation)

(def-instance librarian-occupation administrative-support-occupation)

(def-instance curator-occupation administrative-support-occupation)

(def-instance clergy-occupation community-occupation)

(def-instance social-worker-occupation community-occupation)

(def-instance probation-officer-occupation community-occupation)

(def-instance welfare-worker-occupation community-occupation)

(def-instance community-worker-occupation community-occupation)

(def-instance youth-worker-occupation community-occupation)

(def-instance artist-occupation arts-occupation)

(def-instance actor-occupation arts-occupation)

(def-instance director-occupation arts-occupation)

(def-instance musician-occupation arts-occupation)

(def-instance photographer-occupation arts-occupation)

(def-instance writer-occupation arts-occupation)

(def-instance designer-occupation arts-occupation)

(def-instance designer-occupation design-occupation)

(def-instance management-consultant-occupation business-occupation)

(def-instance economist-occupation business-occupation)

(def-instance statistician-occupation mathematical-occupation)

(def-instance actuary-occupation mathematical-occupation)

(def-instance librarian-occupation library-occupation)

(def-instance curator-occupation library-occupation)

(def-instance computer-programmer-occupation computer-occupation)

(def-instance computer-analyist-occupation computer-occupation)

(def-instance engineering-technician-occupation engineering-occupation)

(def-instance scientist-occupation science-occupation)

(def-instance technologist-occupation science-occupation)

(def-instance laboratory-technician-occupation science-occupation)

(def-instance engineer-occupation engineering-occupation)

(def-instance caretaker-occupation maintenance-occupation)

(def-instance sales-assistant-occupation sales-occupation)

(def-instance check-out-operator-occupation sales-occupation)

(def-instance forecourt-attendant-occupation sales-occupation)

(def-instance market-trader-occupation sales-occupation)

(def-instance door-to-door-salesperson-occupation sales-occupation)

(def-instance insurance-agent-occupation sales-occupation)

(def-instance auctioneer-occupation sales-occupation)

(def-instance demonstrator-occupation sales-occupation)

(def-instance sales-representative-occupation sales-occupation)

(def-instance purchasing-officer-occupation business-occupation)

(def-instance importer-occupation business-occupation)

(def-instance exporter-occupation business-occupation)

(def-instance broker-occupation business-occupation)

(def-instance caretaker-occupation maintenance-and-repair-occupation)

(def-instance construction-worker-occupation construction-occupation)

(def-instance plant-operator-occupation production-occupation)

(def-instance machine-operator-occupation production-occupation)

(def-instance assembler-occupation production-occupation)

(def-instance lineworker-occupation production-occupation)

(def-instance inspector-occupation production-occupation)

(def-instance packer-occupation production-occupation)

(def-instance baker-occupation food-related-occupation)

(def-instance fishmonger-occupation food-related-occupation)

(def-instance butcher-occupation food-related-occupation)

(def-instance administrative-officer-occupation administrative-support-occupation)

(def-instance administrative-assistant-occupation administrative-support-occupation)

(def-instance draughtsperson-occupation design-occupation)

(def-instance quantity-surveyor-occupation construction-occupation)

(def-instance building-technician-occupation construction-occupation)

(def-instance architectural-technician-occupation architecture-occupation)

(def-instance building-inspector-occupation construction-occupation)

(def-instance train-driver-occupation transportation-occupation)

(def-instance tracer-occupation design-occupation)

(def-instance drawing-office-assistant-occupation design-occupation)

(def-instance office-machine-operator-occupation office-occupation)

(def-instance filing-clerk-occupation administrative-support-occupation)

(def-instance general-clerk-occupation administrative-support-occupation)

(def-instance clerical-officer-occupation administrative-support-occupation)

(def-instance clerical-assistant-occupation administrative-support-occupation)

(def-instance numerical-clerk-occupation administrative-support-occupation)

(def-instance cashier-occupation administrative-support-occupation)

(def-instance storekeeper-occupation administrative-support-occupation)

(def-instance stores-clerk-occupation administrative-support-occupation)

(def-instance despatch-clerk-occupation administrative-support-occupation)

(def-instance vocational-trainer-occupation training-occupation)

(def-instance driving-instructer-occupation training-occupation)

(def-instance metal-machiner-occupation engineering-occupation)

(def-instance fitter-occupation engineering-occupation)

(def-instance fitter-occupation production-occupation)

(def-instance metal-machiner-occupation production-occupation)

(def-instance instrument-maker-occupation production-occupation)

(def-instance tool-maker-occupation production-occupation)

(def-instance lathe-fitter-occupation production-occupation)

(def-instance computer-engineer-occupation engineering-occupation)

(def-instance telephone-fitter-occupation installation-occupation)

(def-instance information-officer-occupation administrative-support-occupation)

(def-instance social-scientist-occupation social-science-occupation)

(def-instance professor-occupation education-occupation
  ((has-sector )))

(def-instance research-assistant-occupation education-occupation
  ((has-sector )))

(def-instance research-fellow-occupation education-occupation
  ((has-sector )))

(def-instance senior-research-fellow-occupation education-occupation
  ((has-sector )))

(def-instance researcher-occupation education-occupation
  ((has-sector )))

(def-instance senior-lecturer-occupation education-occupation
  ((has-sector )))

(def-instance reader-occupation education-occupation
  ((has-sector )))

(def-instance academic-occupation education-occupation
  ((has-sector )))

(def-instance business-manager-occupation management-occupation
  ((has-sector )))

(def-instance system-manager-occupation management-occupation
  ((has-sector )))

(def-instance senior-manager-occupation management-occupation
  ((has-sector )))

(def-instance multimedia-designer-occupation design-occupation
  ((has-sector )))

(def-instance non-clinical-employee-of-health-care-organization-occupation healthcare-occupation
  ((has-sector )))

(def-instance project-officer-occupation academic-related-occupation
  ((has-sector )))

(def-instance assistant-project-officer-occupation academic-related-occupation
  ((has-sector )))

(def-instance politician-occupation government-occupation
  ((has-sector )))
