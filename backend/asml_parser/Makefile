all: asmlparser armgen

asmlparser:
	ocamlbuild asmlparser.byte 
	mv asmlparser.byte asmlparse

armgen:
	ocamlbuild armgen.byte 
	mv armgen.byte armgen

clean:
	rm -rf _build

cleanest: clean
	rm -f asmlparse armgen
