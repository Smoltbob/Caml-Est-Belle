export PATH := /usr/arm-none-eabi/bin:$(PATH)

AS=arm-none-eabi-as
LD=arm-none-eabi-ld

ASM=$(wildcard *.s)
PRG=$(subst .s,.arm,${ASM})

all: ${PRG}

echo:
	echo ${ASM}
	echo ${PRG}

%.o : %.s libmincaml.S
	${AS} -o $@ $^

%.arm : %.o
	${LD} -o $@ $^

clean:
	rm *.arm

test: ${PRG}
	@for p in ${PRG} ; do \
	  echo "*** Launching $$p"; \
	  qemu-arm ./$$p ; \
	done

