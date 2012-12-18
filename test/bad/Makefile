JOUJOU = ../../src/joujou 
OPTIONS = 
OCAMLC = ocamlc -w -P-Y-Z

TESTS = $(shell ls *.iml)

all: $(patsubst %.iml, %.ko, $(TESTS)) 

$(JOUJOU):
	@echo "*** Please build $(JOUJOU) first"
	@exit 1

%.ko: %.iml $(JOUJOU) 
	@echo OCAMLRUNPARAM=b $(JOUJOU) $(OPTIONS) $<
	@if OCAMLRUNPARAM=b $(JOUJOU) $(OPTIONS) $<;		\
	then							\
	  echo "*** Succeed when should have failed *** $< \n";	\
	  exit 1; 						\
	else 							\
	  echo [OK] $< "\n";					\
	  touch $@; 						\
	fi

%.cmo: %.ml
	$(OCAMLC) -c $<

clean:
	rm -f *.ml *.ko
