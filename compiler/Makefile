all: asmlparser armgen
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

test_asml:
	(cd ../ && ./scripts/autotest_asml.sh -p ./scripts/asmlparse -d tests/asml_parser -t asml)

test_gencode:
	(cd ../ && ./scripts/autotest_gencode.sh -p ./scripts/mincamlc -d tests/gen-code -t ml -o ARM)

test_teacher:
	(cd ../ && ./scripts/autotest_gencode.sh -p ./scripts/mincamlc -d tests/mincaml -t ml -o ARM)

test_typecheck:
	(cd ../ && ./scripts/autotest_typecheck.sh -p ./scripts/mincamlc -d tests/typechecking -t tc)

test_backend:
	(cd ../ && ./scripts/autotest_backend.sh -p ./scripts/armgen -d tests/backend -t res -o ARM)

test: all
	(rm -f /tmp/compiltest && rm -f /tmp/compiltotal)
	@echo "LAUNCHING TESTS"
	@echo "ASML PARSING ..."
	@$(MAKE) --no-print-directory test_asml
	@echo "TYPE CHECKING ..."
	@$(MAKE) --no-print-directory test_typecheck
	@echo "CODE GENERATION ..."
	@$(MAKE) --no-print-directory test_gencode
	@$(MAKE) --no-print-directory test_backend
	@echo "END OF TESTS"
	@$(MAKE) --no-print-directory stats

stats:
	@{ cpt1=$$(awk '{s+=$$1} END {print s}' /tmp/compiltest);\
		cpt2=$$(awk '{s+=$$1} END {print s}' /tmp/compiltotal);\
		echo -e \\e[33m$$cpt1 successful tests from $$cpt2 : \\e[36m $$(echo "100 * $$cpt1/$$cpt2" | bc)% success rate \\e[0m;\
	}
clean:
	rm -rf _build
	rm -f mincamlc
	rm -f armgen
	rm -f asmlparse
