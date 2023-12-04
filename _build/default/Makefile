
EXE=_build/default/main.exe

all: $(EXE)

$(EXE): *.ml*
	dune build @all
	cp $(EXE) ccomp

n ?= 0

test: $(EXE)
	./ccomp --debug Test/test${n}.c
	tail -1 Test/test${n}.asm | cut -c3- | xclip -i -selection clipboard
	~/lc3tools/build/bin/assembler Test/test${n}.asm
	~/lc3tools/build/bin/simulator Test/test${n}.obj

.PHONY: clean
clean:
	dune clean
	rm *.dot *.dot.png

