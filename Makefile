lispworks = lispworks

default:
	@echo "There is no default target."

# Under Linux, Lispworks doesn't properly scale the visualiser
# pixmaps, so we do it here.
linux:
	$(MAKE) -C assets/images

doc:
	$(MAKE) -C doc

update-external:
	git submodule init
	git submodule update

install-quicklisp:
	curl -O http://beta.quicklisp.org/quicklisp.lisp
	cat scripts/install-quicklisp.lisp | $(lispworks) -init -

find_object_file_names = \
		-name "*.fasl" -o -name "*.fsl" \
		-o -name "*.pfsl" -o -name "*.ufasl" \
		-o -name "*.wfasl" -o -name "*.xfasl"

clean:
	find apps src publisher tests $(find_object_file_names)\
		| xargs rm -f;
	rm -rf irs-server* travel-services

veryclean:
	find . $(find_object_file_names) | xargs rm -f

distclean:
	git clean -d -x -f

check tests: lw-tests

lw-tests:
	cat scripts/run-tests.lisp | $(lispworks) -init -

lispworks-console:
	lispworks -build scripts/make-lispworks-console.lisp

images:
	$(lispworks) -init scripts/make-irs-image.lisp -siteinit -

travel:
	$(lispworks) -init scripts/make-travel-publisher.lisp -siteinit -

tutorial-iswc-2007:
	$(lispworks) -init scripts/make-tutorial-iswc-2007-server.lisp -siteinit -
	$(lispworks) -init scripts/make-tutorial-iswc-2007-publisher.lisp -siteinit -

### IRS Demostration deliverable and packaging

tarball:
	distversion=`git log --pretty=oneline -1 | cut -c 1-8`; \
	distdir=irs-$${LABEL}$${distversion}; \
	if [ -d $${distdir} ]; then \
		rm -rf $${distdir}; \
	fi; \
	mkdir $${distdir}; \
	manifest=`cat ${MANIFESTS} | grep -v ^\#`; \
	for m in $${MANIFESTS}; do \
	    sh $${m} | cpio -p -d $${distdir}; \
	done; \
	if [ -n "$(EXTRADIST)" ]; then \
	    echo $(EXTRADIST) | tr ' ' '\n' | cpio -p -d $${distdir}; \
	fi; \
	tar cjf $${distdir}.tar.bz2 $${distdir};

### Specific tarball targets.

demo-deliverable:
	$(lispworks) -init scripts/make-demo-server.lisp -siteinit -

demo-tarball:
	MANIFESTS="scripts/manifests/core \
		scripts/manifests/demo" $(MAKE) tarball

lhdl-deliverable:
	$(lispworks) -init apps/lhdl/scripts/make-lhdl.lisp -siteinit -

lhdl-tarball:
	MANIFESTS="scripts/manifests/core \
		apps/lhdl/scripts/manifest" \
	LABEL="lhdl-" \
	$(MAKE) tarball

monitoring-engine:
	$(lispworks) -init scripts/make-monitoring-engine.lisp -siteinit -

monitoring-tarball:
	MANIFESTS="scripts/manifests/core \
		scripts/manifests/monitoring-engine" \
	LABEL="monitoring-engine-" \
	$(MAKE) tarball

.PHONY: clean veryclean
.PHONY: demo-tarball lhdl-deliverable lhdl-tarball monitoring-tarball
.PHONY: src-tarball tarball
.PHONY: doc
