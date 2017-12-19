
all: 
	ocamlbuild main.byte 
	mv main.byte mincamlc

asmlparser:
	ocamlbuild basmlparser.byte 
	mv basmlparser.byte asmlparse

clean:
	rm -rf _build

cleanest: clean
	rm -f min-ml
