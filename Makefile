SUFFIX = byte

all:
	ocamlbuild -use-menhir run_lexer.$(SUFFIX)
	ocamlbuild -use-menhir run_parser.$(SUFFIX)
	ocamlbuild -use-menhir run_semant.$(SUFFIX)
	ocamlbuild -use-menhir -package oUnit test_types.$(SUFFIX)

clean:
	ocamlbuild -clean

.PHONY: all clean
