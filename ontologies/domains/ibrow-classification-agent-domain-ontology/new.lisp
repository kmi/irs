;;; Mode: Lisp; Package: ocml

;;; File created in WebOnto

(in-package "OCML")

(in-ontology ibrow-classification-agent-domain-ontology)

(def-class ai-class)

(def-class automated-reasoning (ai-class)
 (
    (automated-reasoning :value yes)
    (theorem-proving :value yes)
    (deduction :value yes)
    (resource-bounded-reasoning :value yes)
    (logic-programming :value yes)
    (abduction :value yes)
    (satisfiability-testing :value yes)
 )
)

(def-class nonmonotonic-reasoning (ai-class)
 (
    (nonmonotonic-reasoning :value yes)
    (belief-revision :value yes)
    (common-sense-reasoning :value yes)
 )
)

(def-class knowledge-representation (ai-class)
 (
    (knowledge-representation :value yes)
    (description-logics :value yes)
    (conceptual-graphs :value yes)
    (truth-maintenance :value yes)
    (computational-complexity :value yes)
    (geometric-reasoning :value yes)
    (temporal-reasoning :value yes)
    (reasoning-about-actions-and-change :value yes)
 )
)

(def-class cognitive-modelling (ai-class)
 (
    (cognitive-modelling :value yes)
    (computer-aided-learning :value yes)
    (knowledge-acquisition :value yes)
    (tutoring-systems :value yes)
 )
)

(def-class constraint-based-reasoning (ai-class)
 (
    (constraint-based-reasoning :value yes)
    (constraint-programming :value yes)
    (constraint-satisfaction :value yes)
 )
)

(def-class distributed-ai (ai-class)
 (
    (distributed-ai :value yes)
    (multi-agent-systems :value yes)
    (autonomous-agents :value yes)
    (distributed-problem-solving :value yes)
 )
)

(def-class human-language-technology (ai-class)
 (
    (human-language-technology :value yes)
    (speech-processing :value yes)
    (natural-language-processing :value yes)
    (information-extraction :value yes)
    (information-retrieval :value yes)
    (computational-linguistics :value yes)
    (machine-translation :value yes)
    (text-understanding :value yes)
    (message-extraction :value yes)
    (discourse-modelling :value yes)
 )
)

(def-class machine-learning (ai-class)
 (
    (machine-learning :value yes)
    (data-mining :value yes)
    (knowledge-discovery :value yes)
    (text-mining :value yes)
    (reinforcement-learning :value yes)
    (inductive-logic-programming :value yes)
    (bayesian-learning :value yes)
    (case-based-reasoning :value yes)
    (neural-networks :value yes)
    (genetic-algorithms :value yes)
    (scientific-discovery :value yes)
    (induction :value yes)
 )
)

(def-class knowledge-based-systems (ai-class)
 (
    (knowledge-based-systems :value yes)
    (ontologies :value yes)
    (reuse-of-knowledge :value yes)
    (verification :value yes)
    (validation :value yes)
    (design :value yes)
 )
)

(def-class planning (ai-class)
 (
    (planning :value yes)
    (deductive-planning :value yes)
    (scheduling :value yes)
    (skeletelal-planning :value yes)
    (reactive-control :value yes)
    (plan-recognition :value yes)
 )
)

(def-class qualitative-reasoning (ai-class)
 (
    (qualitative-reasoning :value yes)
    (diagnosis :value yes)
    (model-based-reasoning :value yes)
    (causal-reasoning :value yes)
    (reasoning-about-physical-systems :value yes)
 )
)

(def-class robotics (ai-class)
 (
    (robotics :value yes)
    (cognitive-robotics :value yes)
    (vision :value yes)
    (perception :value yes)
    (spatial-reasoning :value yes)
    (signal-understanding :value yes)
    (monitoring :value yes)
    (sensor-interpretation :value yes)
    (sensory-fusion/fission :value yes)
    (real-time-systems :value yes)
 )
)

(def-class search (ai-class)
 (
    (search :value yes)
    (meta-heuristics-for-ai :value yes)
    (game-playing :value yes)
 )
)

(def-class uncertainty-in-ai (ai-class)
 (
    (uncertainty-in-ai :value yes)
    (probabilistic-networks :value yes)
    (reasoning-under-uncertainty :value yes)
    (probabilistic-reasoning :value yes)
    (decision-theory :value yes)
    (fuzzy-logic :value yes)
 )
)

(def-class intelligent-user-interfaces (ai-class)
 (
    (intelligent-user-interfaces :value yes)
    (user-modeling :value yes)
    (multimodal-systems :value yes)
 )
)

(def-class prestigous-applications-of-intelligent-systems-pais (ai-class)
 (
    (prestigous-applications-of-intelligent-systems-pais :value yes)
    (pais :value yes)
 )
)


