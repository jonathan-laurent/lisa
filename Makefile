OCAMLINC = -I frontend -I libs -I corrections -I +zarith
APRON = /usr/lib/ocaml/apron/

CFLAGS = '-I $(APRON)'
LFLAGS = ' -I $(APRON) bigarray.cmxa gmp.cmxa \
	apron.cmxa boxMPFR.cmxa polkaMPQ.cmxa'

OTHER_LIBS = -package zarith
TEST_DIR = tests/sources/
TEST_FILE = *
TEST_PATH = $(TEST_DIR)$(TEST_FILE)

all : 
	ocamlbuild -use-menhir $(OCAMLINC) $(OTHER_LIBS) \
		-cflags $(CFLAGS) -lflags $(LFLAGS) main.native

	cp _build/main.native lisa
	rm main.native

clean :
	-rm -r _build
	-rm lisa
	-rm tests/tests*

test : 
	./lisa --test --polyhedra $(TEST_PATH) > tests/tests.polyhedra.txt
	./lisa --test --consts -i $(TEST_PATH) > tests/tests.constants.txt
	./lisa --test --intervals $(TEST_PATH) > tests/tests.intervals.txt

