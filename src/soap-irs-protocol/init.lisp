(in-package irs-protocol)

(eval-when (eval load)
  ;;allow soap bindings to be defined through webonto
  (push 'ocml::def-irs-soap-bindings
        web-onto::*approved-ocml-defs*))
