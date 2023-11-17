
EXE=_build/default/main.exe

all: $(EXE)

$(EXE): *.ml*
	dune build @all
	cp $(EXE) ccomp

test: $(EXE) Tests/test02.c
	./ccomp Tests/test02.c
	cat Tests/test02.s

.PHONY: clean
clean:
	dune clean
	rm *.dot *.dot.png
