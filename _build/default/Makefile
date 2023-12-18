
EXE=_build/default/main.exe

all: $(EXE)

$(EXE): *.ml*
	dune build @all
	cp $(EXE) ccomp

n ?= 0

testn: $(EXE)
	./ccomp --debug Test/test${n}.c
# 	dot -v -Tpng -O Test/test${n}_ast.dot
# 	tail -1 Test/test${n}.asm | cut -c3- | xclip -i -selection clipboard
# 	~/lc3tools2/lc3as Test/test${n}.asm
	~/lc3tools2/lc3as Test/test${n}.asm
	~/lc3tools2/lc3sim Test/test${n}.obj
# 	~/lc3tools/build/bin/assembler Test/test${n}.asm
# 	~/lc3tools/build/bin/simulator Test/test${n}.obj

test: $(EXE)
	./ccomp --debug Test/ok/${v}.c
# 	dot -v -Tpng -O Test/test${n}_ast.dot
# 	tail -1 Test/test${n}.asm | cut -c3- | xclip -i -selection clipboard
# 	~/lc3tools2/lc3as Test/test${n}.asm
	~/lc3tools2/lc3as Test/ok/${v}.asm
	~/lc3tools2/lc3sim Test/ok/${v}.obj
# 	~/lc3tools/build/bin/assembler Test/test${n}.asm
# 	~/lc3tools/build/bin/simulator Test/test${n}.obj

.PHONY: clean
clean:
	dune clean
	rm *.dot *.dot.png

