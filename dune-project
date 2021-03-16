(lang dune 2.5)
(using menhir 2.0)

(generate_opam_files true)

(name coatc)

(package
 (name coatc)
 (synopsis "Oat")
 (depends
  (dune (>= 2.5))
  (ocaml (>= 4.10.0))
  num
  menhir
  (ppx_inline_test :with-test)
 )
)
