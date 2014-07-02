SUFFIX = native

all:
	ocamlbuild -use-menhir run_lexer.$(SUFFIX)

clean:
	ocamlbuild -clean

.PHONY: all clean
