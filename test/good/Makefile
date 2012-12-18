JOUJOU = ../../src/joujou 
OPTIONS = 
OCAMLC = ocamlc -w -P-Y-Z

TESTS = $(shell ls *.iml)
RUNS = $(shell ls *_run.ml)

TARGETS = $(patsubst %.iml, %.ml, $(TESTS)) \
	  $(patsubst %.iml, %.cmo, $(TESTS)) \
	  $(patsubst %_run.ml, %.out, $(RUNS))

all: $(TARGETS)

$(JOUJOU):
	@echo "*** Please build $(JOUJOU) first"
	@exit 1

%.ml: %.iml $(JOUJOU) 
	@if OCAMLRUNPARAM=b $(JOUJOU) $(OPTIONS) $<; then \
	  echo [OK] Elaboration of $<;			\
	else						\
	  echo [KO] Elaboration of $<;			\
	fi

# %.ml: %.iml $(JOUJOU) 
# 	OCAMLRUNPARAM=b $(JOUJOU) $(OPTIONS) $<
# 	if diff -q $@ $*_ref.ml > $*.diff; then 	\
# 	  echo [OK] Generation of $@;			\
# 	else						\
# 	  echo [KO] Generation of $@;			\
# 	fi

%.cmo: %.ml
	@ if $(OCAMLC) -c $<; then 		\
	  echo [OK] Compilation of $<;		\
	else 					\
	  echo [KO] Compilation of $<;		\
	fi

%_run.cmo: %.cmo

%.run: %.cmo %_run.cmo
	@ $(OCAMLC) -o $@ $^ # This is our source file. It must compile.

%.out: %.run
	@ if ./$<; then 			\
	  echo [OK] Evaluation of $<;		\
	else					\
	  echo [KO] Evaluation of $<;		\
	fi					
	@ touch $@

clean:
	rm -f $(TARGETS) *.cmo *.cmi
