(lang dune 3.21)
(name karuta)
(license MIT-0)

; https://dune.readthedocs.io/en/stable/opam.html#generating-opam-files
(generate_opam_files true)

(authors "Lemos" "Magueta" "Marinho")

(maintainers "Lemos" "Magueta" "Marinho")

(source
 (github Dr-Nekoma/karuta))

; https://dune.readthedocs.io/en/stable/concepts.html#package-specification
(package
 (name karuta)
 (synopsis "A compiler for a statically-typed relational programming language")
 (depends
  (ocaml (>= 5.4))
  dune
  alcotest
  (batteries (>= 3.8.0))
  cmdliner
  eio
  eio_main
  lambda-term
  lwt
  lwt-exit
  num
  ppx_deriving
  ppx_enumerate
  ppx_sexp_conv
  ppx_variants_conv
  ppxlib
  sexplib)
 (tags
  (prolog logic compiler relational warren)))
