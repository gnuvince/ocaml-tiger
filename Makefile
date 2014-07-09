SUFFIX = byte

all:
	ocamlbuild -use-menhir run_lexer.$(SUFFIX)
	ocamlbuild -use-menhir run_parser.$(SUFFIX)
	ocamlbuild -use-menhir run_semant.$(SUFFIX)

clean:
	ocamlbuild -clean

.PHONY: all clean
