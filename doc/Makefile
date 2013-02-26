all: compile dvi show
		
compile:
	latex  -interaction=batchmode *.tex 1>/dev/null

dvi:
	dvipdf *.dvi

show:
	open *.pdf

verbose:
	latex -halt-on-error *.tex

clean:
	rm -f *.aux *.dvi *.log *.nav *.out *.snm *.toc *.bak
