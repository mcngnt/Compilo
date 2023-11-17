
EXE=_build/default/main.exe

all: $(EXE)

$(EXE): *.ml*
	dune build @all
	cp $(EXE) ccomp

test: $(EXE) Tests/test01.c
	./ccomp --debug Tests/test01.c
	# lc2as test.s 
	# gcc -o test.exe test.c
	# ./test.exe
	dot -v -Tpng -O Tests/test01_ast.dot

.PHONY: clean
clean:
	dune clean
	rm *.dot *.dot.png
