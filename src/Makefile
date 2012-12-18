GENERATED = parser.ml parser.mli lexer.ml

MODULES = type printast parser error lexer stringMap settings matching wf \
	unionFind unification \
	exproftype elaborate front

EXECUTABLE = joujou

BFLAGS = -dtypes -g
OFLAGS = -g

OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLDEP = ocamldep
OCAMLLEX = ocamllex
MENHIR = menhir

$(EXECUTABLE): $(MODULES:%=%.cmo)
	$(OCAMLC) $(BFLAGS) -o $(EXECUTABLE) $(MODULES:%=%.cmo)

$(EXECUTABLE).opt: $(MODULES:%=%.cmx)
	$(OCAMLOPT) $(OFLAGS) -o $(EXECUTABLE) $(MODULES:%=%.cmx)

%.cmo: %.ml
	$(OCAMLC) $(BFLAGS) -c $*.ml
%.cmi: %.mli
	$(OCAMLC) $(BFLAGS) -c $*.mli
%.cmx: %.ml
	$(OCAMLOPT) $(OFLAGS) -c $*.ml
%.ml %.mli: %.mly
	$(MENHIR) $*.mly	
%.ml: %.mll
	$(OCAMLLEX) $*.mll

clean:
	rm -f $(EXECUTABLE) $(EXECUTABLE).opt lexer.ml *.cm[iox] *.o *.annot

.depend: $(GENERATED) Makefile
	$(OCAMLDEP) $(patsubst %, %.ml, $(MODULES)) \
		$(patsubst %, %.mli, $(MODULES)) $(GENERATED) > .depend

depend: 
	rm -f .depend
	$(MAKE) .depend

include .depend
