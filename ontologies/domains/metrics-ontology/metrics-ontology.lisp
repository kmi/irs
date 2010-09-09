;;; Mode: Lisp; Package: ocml

;; Metrics Ontology
;; (Imports Units Manipulation and COBRA)
;;
;; Ontology for supporting the definition of Metrics for Business Processes
;; Provides general purpose constructs for defining metrics which shall then be computed 
;; by the appropriate PSM (see Metrics Computation Task)
;;
;; Author: Carlos Pedrinaci  (Knowledge Media Institute - The Open University)
;; Version: 1.0

(in-package "OCML")

(in-ontology metrics-ontology)

(def-class #_Metric (#_cobra:QuantitativeAnalysis)
  "A Metric is a Quantitative Analysis that can be evaluated over any Entity in order to quantitatively analyse them. Metrics are further characterised as Function Metrics and Aggregation Metrics. Metrics are described using the concept role as for tasks. This allows us to iterate through all the input roles and compute the metrics if required. As a result, the definition of Metrics can be composed of an arbitrary number of nested metrics. Metrics do not include an output role since their role is to specify metrics but not to store their results given that these are ephemeral and one might be interested in the evolution of the value over time."
    ((has-input-role :type role)))

(def-class #_FunctionMetric (#_Metric)
  "A Function Metric is a Metric which is evaluated over a fixed number or inputs."
  ((#_hasComputationExpression :type unary-procedure :cardinality 1)))

;; Primitive Function Metric
(def-class #_Ratio (#_FunctionMetric) ?fm
  "Ratio is a Function Metric that computes the Ratio between a Constant Quantity or another Metric and a Dimensionless Quantity or another Metric, termed Dividend and Divisor respectively. "
  ((has-input-role :value #_hasDividend
                   :value #_hasDivisor)
   (#_hasDividend :type (or #_phys-q:ConstantQuantity #_Metric) :cardinality 1)
   (#_hasDivisor :type (or #_phys-q:DimensionlessQuantity #_Metric) :cardinality 1)
   (#_hasComputationExpression :value
                               '(lambda (?fm)
                               (in-environment 
                                ((?dividend . (the-slot-value ?fm #_hasDividend))
                                 (?divisor . (the-slot-value ?fm #_hasDivisor))
                                 (?result . (/ ?dividend ?divisor)))
                                ?result)))))                                               

;; Aggregation Metrics
;; TODO: Add restriction so that the type obtained by the filter is compatible
;; with the type of the unbound slot in the function metric
(def-class #_AggregationMetric (#_Metric)
  "An Aggregation Metric is a Metric which is evaluated over a set of elements of a particular type by applying a Function Metric over each of them. An Aggregation Metric is further characterised by the type of population it handles and an optional Population Filter. Both the Population Filter and the Function Metric must be compatible with the type handled by the Aggregation Metric."
  ((has-input-role :value #_hasFunctionMetric
                   :value #_hasUnboundRole
                   :value #_hasAggregationConstruct
                   :value #_hasPopulationFilter)                 
   (#_hasFunctionMetric :type #_FunctionMetric :max-cardinality 1)
   (#_hasUnboundRole :type role :max-cardinality 1)
   (#_hasAggregationConstruct :type unary-function :cardinality 1)
   (#_hasPopulationFilter :type #_PopulationFilter :cardinality 1)))

;;
;; Key Performance Indicator
;; 
(def-class #_KeyPerformanceIndicator (#_cobra:Role)
  "The notion of Key Performance Indicator is well-known and widely applied in Business Process Management. KPIs have however an essentially context-dependent nature. What can be considered as a KPI within a particular domain might not be as relevant for another. For instance, the smallest deviation with respect to the agreed deadline is not as relevant for a software company as it is within the logistics domain. Furthermore, even within the same domain, different departments, business processes, or even particular process instances might be driven by diverse KPIs. A simple example can be the so-called Return of Investment which is certainly crucial for most businesses but can hardly be used to measure the success of the research department within a company. Being a KPI is thus not an intrinsic feature, it is a Role that certain indicators can play.")

;; This class should keep the avoid-infinite-loop to avoid infinite loops
;; when proving the membership of instances in this class 
(def-class #_PopulationFilter (#_cobra:Query) ?q
  "A Population Filter is a kind of Query that simply captures ontological queries for a particular kind population. In other words, population filters when evaluated will only return individuals of a particular kind. This concept is defined intensionally."
  :iff-def (and (instance-of ?q #_cobra:Query)
                (= (length (the-slot-value ?q #_cobra:hasOutputType)) 1))
  :avoid-infinite-loop t)

;;
;; Aggregation Metrics
;;
(def-class #_Count (#_AggregationMetric) 
  "An Aggregation Metric that counts the number of individuals within the population and meeting the given selection criteria."
  ((#_hasAggregationConstruct :value count)))

(def-class #_Maximum (#_AggregationMetric) 
  "An Aggregation Metric that returns the Maximum of the Function Metrics computed for all the individuals from the population meeting the given selection criteria."
  ((#_hasAggregationConstruct :value max)))

(def-class #_Minimum (#_AggregationMetric) 
  "An Aggregation Metric that returns the Minimum of the Function Metrics computed for all the individuals from the population meeting the given selection criteria."
  ((#_hasAggregationConstruct :value min)))

(def-class #_Average (#_AggregationMetric) 
  "An Aggregation Metric that returns the Average of the Function Metrics computed for all the individuals from the population meeting the given selection criteria."
  ((#_hasAggregationConstruct :value mean)))

(def-class #_StandardDeviation (#_AggregationMetric) 
  "An Aggregation Metric that returns the Standard Deviation of the Function Metrics computed for all the individuals from the population meeting the given selection criteria."
  ((#_hasAggregationConstruct :value standard-deviation)))

(def-class #_Variance (#_AggregationMetric) 
  "An Aggregation Metric that returns the Variance of the Function Metrics computed for all the individuals from the population meeting the given selection criteria."
  ((#_hasAggregationConstruct :value variance)))

(def-class #_Prod (#_AggregationMetric) 
  "An Aggregation Metric that returns the Product of the Function Metrics computed for all the individuals from the population meeting the given selection criteria."
  ((#_hasAggregationConstruct :value prod)))

(def-class #_Sum (#_AggregationMetric) 
  "An Aggregation Metric that returns the Sum of the Function Metrics computed for all the individuals from the population meeting the given selection criteria."
  ((#_hasAggregationConstruct :value sum)))


