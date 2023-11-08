
EXE=_build/default/main.exe

all: $(EXE)

$(EXE): *.ml*
	dune build @all
	cp $(EXE) ccomp

test: $(EXE) test.c
	./ccomp --debug test.c
	# lc2as test.s 
	# gcc -o test.exe test.c
	# ./test.exe
	dot -v -Tpng -O test_ast.dot

.PHONY: clean
clean:
	dune clean
	rm *.dot *.dot.png
