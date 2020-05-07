all: expr evaluation miniml

expr: expr.ml
	ocamlbuild -use-ocamlfind expr.byte

evaluation: evaluation.ml
	ocamlbuild -use-ocamlfind evaluation.byte

miniml: miniml.ml
	ocamlbuild -use-ocamlfind miniml.byte

clean:
	rm -rf _build *.byte