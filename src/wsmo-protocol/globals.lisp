;;; Copyright © 2008 The Open University

(in-package #:wsmo-protocol)

(defvar *choreography-run-number* 0)

(defvar *choreography-state-name* 'ocml::choreography-state)

(defvar *internal-goal-instances*)

(defvar *achieve-goal-results*)

(defvar *current-orchestration-web-service*)

(defvar *default-publisher-information-parent*
  'ocml::publisher-information)

(defvar *achieve-goal-soap-values* nil
  "Used to store the soap values in achieve goal for debugging purposes.")

(defvar *default-non-functional-properties-parent-name* 
  'ocml::non-functional-properties)

(defvar *non-functional-property-names*
  '(ocml::has-accuracy 
    ocml::has-contributor 
    ocml::has-coverage 
    ocml::has-creator 
    ocml::has-date 
    ocml::has-description 
    ocml::has-financial 
    ocml::has-format 
    ocml::has-identifier 
    ocml::has-language 
    ocml::has-net-Related-QoS 
    ocml::has-owner 
    ocml::has-performance 
    ocml::has-publisher 
    ocml::has-relation 
    ocml::has-reliability 
    ocml::has-rights 
    ocml::has-robustness 
    ocml::has-scalability 
    ocml::has-security 
    ocml::has-source 
    ocml::has-subject 
    ocml::has-title 
    ocml::has-transactional 
    ocml::has-trust 
    ocml::has-type 
    ocml::has-type-Of-Match 
    ocml::has-version))

(defvar *non-functional-property-class-name-extension*
  "-NON-FUNCTIONAL-PROPERTIES")

(defvar *default-goal-parent*
  'ocml::goal)

(defvar *default-web-service-parent*
  'ocml::web-service)

(defvar *default-mediator-parent*
  'ocml::mediator)

(defvar *default-capability-parent*
  'ocml::capability)

(defvar *default-interface-parent*
  'ocml::interface)

(defvar *default-core-non-functional-properties-parent*
  'ocml::core-non-functional-properties)

(defvar *default-web-service-non-functional-properties-parent*
  'ocml::web-service-non-functional-properties)

(defvar *default-choreography-parent*
  'ocml::choreography)

(defvar *default-grounding-parent*
  'ocml::grounding)

(defvar *default-orchestration-parent*
  'ocml::orchestration)

(defvar *default-problem-solving-pattern-parent*
  'ocml::problem-solving-pattern)

(defvar *default-non-functional-properties-parent*
  'ocml::non-functional-properties)

(defvar *choreography-properties*
  '(ocml::has-message-exchange-pattern
    ocml::has-grounding
    ocml::has-guarded-transitions))

(defvar *grounding-properties*
  '(ocml::has-operation-mapping))

(defvar *publisher-information-properties*
  '(ocml::has-associated-web-service-interface
    ocml::has-web-service-host ocml::has-web-service-port ocml::has-web-service-location))

(defvar *orchestration-properties*
  '(ocml::has-problem-solving-pattern))

(defvar *core-non-functional-properties*
  '(ocml::title ocml::creator ocml::subject ocml::description 
                                             ocml::publisher ocml::contributor ocml::date 
                                             ocml::type ocml::format 
                                             ocml::identifier ocml::source ocml::language 
                                             ocml::has-relation ocml::coverage
                                             ocml::rights ocml::version))

(defvar *web-service-non-functional-properties*
  '(ocml::title ocml::creator ocml::subject ocml::description 
                                             ocml::publisher ocml::contributor ocml::date 
                                             ocml::type ocml::format 
                                             ocml::identifier ocml::source ocml::language 
                                             ocml::has-relation ocml::coverage
                                             ocml::rights ocml::version
                                             ocml::performance ocml::reliability 
                                             ocml::security ocml::scalability
                                             ocml::robustness ocml::accuracy 
                                             ocml::transactional ocml::trust 
                                             ocml::financial 
                                             ocml::network-related-quality-of-service))

(defvar *orchestration-primitive-ocml-mappings*
  '((ocml::orch-if . ocml::if)
    (ocml::orch-return . ocml::return)
    (ocml::orch-repeat . ocml::repeat)
    (ocml::orch-loop . ocml::loop)))
