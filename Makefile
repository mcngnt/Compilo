
EXE=_build/default/main.exe

all: $(EXE)

$(EXE): *.ml*
	dune build @all
	cp $(EXE) ccomp

test: $(EXE) Test/test0.c
	./ccomp Test/test0.c
# 	dot -v -Tpng -O Test/test0_ast.dot
# 	echo "\n\n\n\n\n"
# 	cat Test/test0.s

.PHONY: clean
clean:
	dune clean
	rm *.dot *.dot.png
