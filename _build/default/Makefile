
EXE=_build/default/main.exe

all: $(EXE)

$(EXE): *.ml*
	dune build @all
	cp $(EXE) ccomp

test: $(EXE) Test/test0.c
	./ccomp --debug Test/test0.c
	tail -1 Test/test0.asm | cut -c3- | xclip -i -selection clipboard
	~/lc3tools/build/bin/assembler Test/test0.asm
	~/lc3tools/build/bin/simulator Test/test0.obj
# 	dot -v -Tpng -O Test/test0_tast.dot
# 	echo "\n\n\n\n\n"	
# 	(cat Test/test0.ams)

.PHONY: clean
clean:
	dune clean
	rm *.dot *.dot.png

