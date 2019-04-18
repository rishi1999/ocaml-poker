MODULES= authors deck player table save_load_engine command main hashone \
	tableflush hand_evaluator constantarrays state card
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=unix,oUnit,str,qcheck,ANSITerminal,dune

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag debug $(TEST) && ./$(TEST)

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

bisect-test:
	$(OCAMLBUILD) -package bisect -syntax camlp4o,bisect_pp \
	  $(TEST) && ./$(TEST) -runner sequential

check:
	bash checkenv.sh && bash checktypes.sh

finalcheck: check
	bash finalcheck.sh

bisect: clean bisect-test
	bisect-report -I _build -html report bisect0001.out


#zip:
#	zip search_src.zip *.ml* _tags Makefile analysis.pdf

docs: docs-public docs-private

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private report final_submission.zip bisect*.out
