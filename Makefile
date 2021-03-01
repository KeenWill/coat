INCLUDES= util,x86,grading,ll
LIBS = unix,str,nums
SUBMIT := frontend.ml typechecker.ml 

HWNAME := hw05
TIMESTAMP := $(shell /bin/date "+%Y-%m-%d-%H:%M:%S")
ZIPNAME := $(HWNAME)-submit($(TIMESTAMP)).zip


all: main.native

.PHONY: test
test: main.native
	./main.native --test

.PHONY: main.native
main.native: 
	ocamlbuild -Is $(INCLUDES) -libs $(LIBS) main.native -use-menhir -yaccflag --explain

zip: $(SUBMIT)
	zip '$(ZIPNAME)' $(SUBMIT)

.PHONY: clean
clean:
	ocamlbuild -clean
	rm -rf output a.out
