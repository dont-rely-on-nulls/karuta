(lang dune 3.11)
(map_workspace_root false)
(using menhir 2.1)
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
 (depends ocaml dune menhir (batteries (>= 3.8.0)))
 (tags
  (prolog logic compiler relational warren)))
