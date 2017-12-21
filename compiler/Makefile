all: 
	ocamlbuild main.byte 
	mv main.byte mincamlc

asmlparser:
	ocamlbuild basmlparser.byte 
	mv basmlparser.byte asmlparse

armgen:
	ocamlbuild barmgen.byte 
	mv barmgen.byte armgen

doc:
	ocamlbuild mincamlc.docdir/index.html

clean:
	rm -rf _build

cleanest: clean
	rm -f mincamlc
	rm -f armgen
	rm -f asmlparse
