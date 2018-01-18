
Build = ocamlbuild -use-ocamlfind -lib unix -package ppx_deriving.show

all: crint

crint: src/*
	$(Build) src/main.native
	mv main.native bin/$@

clean:
	$(Build) -clean

.PHONY: all clean crint
