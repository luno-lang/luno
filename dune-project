(lang dune 3.6)

(name luno)

(generate_opam_files true)

(using menhir 2.1)

(source
 (github luno-lang/luno))

(authors "Luno")

(maintainers "Abigail")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name luno)
 (synopsis "A small toy language with syntax heavily inspired by Lua.")
 (description "A small toy language with syntax heavily inspired by Lua.")
 (depends ocaml dune)
 (tags ("programming language")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
