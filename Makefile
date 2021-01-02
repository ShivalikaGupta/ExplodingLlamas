MODULES = author deck player gamestate command cardeffects riddles
OBJECTS = $(MODULES:=.cmo)
MLS = $(MODULES:=.ml)
MLIS = $(MODULES:=.mli)
TEST = test.byte
OCAMLBUILD = ocamlbuild -use-ocamlfind
PKGS=ounit2,ANSITerminal

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

testAI:
	ocamlbuild -use-ocamlfind testAI.byte && ./testAI.byte

clean:
	ocamlbuild -clean
	rm -rf doc

zip: 
	zip cs3110-final.zip *.ml* *_tags *.txt Makefile 

play:
	ocamlbuild -use-ocamlfind main.byte && ./main.byte

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
