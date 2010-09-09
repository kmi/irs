(load-all-patches)
(save-image "lispworks-console"
	    :console t :environment nil
	    :restart-function 'mp:initialize-multiprocessing)
(quit)
