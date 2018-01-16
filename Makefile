all:
	(cd compiler && make -s)

%:
	(cd compiler && make -s $@)
