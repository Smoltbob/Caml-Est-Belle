all:
	(cd compiler && make)

%:
	(cd compiler && make $@)
