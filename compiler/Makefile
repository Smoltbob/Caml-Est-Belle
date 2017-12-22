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
	mv mincamlc.docdir/* ../docs

test_asml:	
	(cd ../ && ./scripts/autotest_asml.sh -p ./scripts/asmlparse -d tests/asml_parser -t asml)

test_gencode:	
	(cd ../ && ./scripts/autotest_gencode.sh -p ./scripts/mincamlc -d tests/gen-code -t ml -o ARM)

test_typecheck:
	(cd ../ && ./scripts/autotest_typecheck.sh -p ./scripts/mincamlc -d tests/typechecking -t tc -v)

test:
	@echo "LAUNCHING TESTS"
	@echo "ASML PARSING ..."
	$(MAKE) test_asml
	@echo "TYPE CHECKING ..."
	$(MAKE) test_typecheck
	@echo "CODE GENERATION ..."
	$(MAKE) test_gencode
	@echo "END OF TESTS"
	@echo "In case of KO, you can launch the scripts separately with the verbose (-v) option to see the outputs."

clean:
	rm -rf _build

cleanest: clean
	rm -f mincamlc
	rm -f armgen
	rm -f asmlparse
