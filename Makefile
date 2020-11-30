# Nome de ficheiros
NATIVE = memoPPX
FILE = Tests/alltests
DUMP = AST
RESULT = Results/result

# Comandos
clean = rm -r -f _build *.native $(RESULT)* $(DUMP).ml
native = ocamlbuild -package compiler-libs.common  $(NATIVE).native 
build = ocamlfind ppx_tools/rewriter ./$(NATIVE).native  $(FILE).ml > $(RESULT).ml
dump = ocamlfind ppx_tools/dumpast -loc_discard -attrs_discard $(FILE).ml > $(DUMP).ml
exec = ocamlc -o $(RESULT) $(RESULT).ml  && ./$(RESULT)
show = cat $(RESULT).ml

all:
	$(clean) 
	$(native)  	
	$(build)
	$(exec)

native: 
	$(native)
build: 
	$(build)
dump: 
	$(dump)
exec: 
	$(exec)
show: 
	$(show)
clean:
	$(clean)


