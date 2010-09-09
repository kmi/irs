all: irs-manual.pdf irs-manual.html

irs-manual.pdf: irs-manual.texi
	texi2pdf $<

irs-manual.html: irs-manual.texi
	texi2html $<

clean:
	rm -f *.cp *.cps *.fn *.fns *.fo *.ky *.log *.pg *.toc *.tp *.vr
	rm -f irs-manual.html
	rm -f irs-manual.pdf
