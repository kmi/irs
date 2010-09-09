(load-all-patches)
(load "scripts/irs.lisp")
(require :irs)

(save-image (executable-name "irs-server")
	    :console :input :environment t)

(quit)
