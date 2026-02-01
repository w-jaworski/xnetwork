INCLUDES=-I +str -I +unix
OCAMLFLAGS=$(INCLUDES)
OCAMLOPTFLAGS=$(INCLUDES)
SOURCES=xhttp.ml
INSTALLDIR=`ocamlc -where`/xnetwork

all: xnetwork.cma

opt: xnetwork.cmxa

install: all opt
	mkdir -p $(INSTALLDIR)
	cp xnetwork.cmxa xnetwork.a xnetwork.cma $(INSTALLDIR)
	cp xhttp.cmi $(INSTALLDIR)
	cp xhttp.cmx $(INSTALLDIR)

xhttp.cmx: xhttp.ml
	ocamlfind ocamlopt -o xhttp.cmx -package cohttp-lwt-unix,lwt.unix -linkpkg $(OCAMLOPTFLAGS) -c $^

xnetwork.cma: $(SOURCES)
	ocamlfind ocamlc -linkall -a -o xnetwork.cma -package cohttp-lwt-unix,lwt.unix -linkpkg $(OCAMLFLAGS) $^

xnetwork.cmxa: xhttp.cmx #$(SOURCES)
	ocamlfind ocamlopt -a -o xnetwork.cmxa xhttp.cmx

clean:
	rm -f *~ *.cm[aoix] *.o *.so *.cmxa *.a
