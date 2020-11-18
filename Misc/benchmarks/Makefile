# Nome de ficheiros
FILE = testfib
RESULT = resultfib

# Comandos
test = ocamlopt -o $(FILE) $(FILE).ml  && ./$(FILE)
result = ocamlopt -o $(RESULT) $(RESULT).ml  && ./$(RESULT)

show = cat $(RESULT).ml

all:
	$(test)
	$(result)

test: 
	$(test)
result:
	$(result)
show: 
	$(show)


