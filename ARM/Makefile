export PATH := /usr/arm-none-eabi/bin:$(PATH)

AS=arm-none-eabi-as
LD=arm-none-eabi-ld

ASM=$(wildcard *.s)
PRG=$(subst .s,.arm,${ASM})

all: ${PRG}

%.o : %.s libmincaml.S talloc.S
	${AS} -o $@ $^

%.arm : %.o
	${LD} -o $@ $^

.PHONY: clean test echo
clean:
	rm *.arm
	rm *.core

echo:
	echo ${ASM}
	echo ${PRG}

test: ${PRG}
	@for p in ${PRG} ; do \
	  echo "*** Launching $$p"; \
	  qemu-arm ./$$p ; \
	done

