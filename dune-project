(lang dune 3.0)
(name xnetwork)
(generate_opam_files)

(package
 (name xnetwork)
 (synopsis "Biblioteka XNetwork")
 (description "Biblioteka pomocnicza XNetwork (http, html, javascript, etc.)")
 (depends
  (ocaml (>= 5.3))
  (dune (>= 3.0))
  (cohttp-lwt-unix (>= 6.2))
  (lwt (>= 6.0))
  xlib
 ))
