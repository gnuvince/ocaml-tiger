SUFFIX = native

all:
	ocamlbuild -use-menhir run_lexer.$(SUFFIX)
	ocamlbuild -use-menhir run_parser.$(SUFFIX)

clean:
	ocamlbuild -clean

.PHONY: all clean
