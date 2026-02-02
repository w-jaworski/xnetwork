INCLUDES=-I +xml-light -I +xlib -I +zarith -I +zip -I +bz2 -I +str -I +unix
OCAMLFLAGS=$(INCLUDES)
OCAMLOPTFLAGS=$(INCLUDES) xml_light.cmxa str.cmxa zarith.cmxa zip.cmxa bz2.cmxa
SOURCES=xhttp.ml xwwwTypes.ml xcss.ml xJavaScript.ml xhtml.ml
SOURCES_OPT=xhttp.cmx xwwwTypes.cmx xcss.cmx xJavaScript.cmx xhtml.cmx
INSTALLDIR=`ocamlc -where`/xnetwork

all: xnetwork.cma

opt: xnetwork.cmxa

install: all opt
	mkdir -p $(INSTALLDIR)
	cp xnetwork.cmxa xnetwork.a xnetwork.cma $(INSTALLDIR)
	cp xhttp.cmi xwwwTypes.cmi xcss.cmi xJavaScript.cmi xhtml.cmi $(INSTALLDIR)
	cp xhttp.cmx xwwwTypes.cmx xcss.cmx xJavaScript.cmx xhtml.cmx $(INSTALLDIR)

xhttp.cmx: xhttp.ml
	ocamlfind ocamlopt -o xhttp.cmx -package cohttp-lwt-unix,lwt.unix -linkpkg $(OCAMLOPTFLAGS) -c $^

xwwwTypes.cmx: xwwwTypes.ml
	ocamlfind ocamlopt -o xwwwTypes.cmx -package cohttp-lwt-unix,lwt.unix -linkpkg $(OCAMLOPTFLAGS) -c $^

xcss.cmx: xcss.ml
	ocamlfind ocamlopt -o xcss.cmx -package cohttp-lwt-unix,lwt.unix -linkpkg $(OCAMLOPTFLAGS) -c $^

xJavaScript.cmx: xJavaScript.ml
	ocamlfind ocamlopt -o xJavaScript.cmx -package cohttp-lwt-unix,lwt.unix -linkpkg $(OCAMLOPTFLAGS) -c $^

xhtml.cmx: xhtml.ml
	ocamlfind ocamlopt -o xhtml.cmx -package cohttp-lwt-unix,lwt.unix -linkpkg $(OCAMLOPTFLAGS) -c $^

xnetwork.cma: $(SOURCES)
	ocamlfind ocamlc -linkall -a -o xnetwork.cma -package cohttp-lwt-unix,lwt.unix -linkpkg $(OCAMLFLAGS) $^

xnetwork.cmxa: $(SOURCES_OPT)
	ocamlfind ocamlopt -linkall -a -o xnetwork.cmxa xhttp.cmx

.SUFFIXES: .ml .cmx

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

clean:
	rm -f *~ *.cm[aoix] *.o *.so *.cmxa *.a
