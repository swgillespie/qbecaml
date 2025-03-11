(lang dune 3.17)

(name qbecaml)

(generate_opam_files true)

(source
 (github swgillespie/ocamlqbe))

(authors "Sean Gillespie <sean@swgillespie.me>")

(maintainers "Sean Gillespie <sean@swgillespie.me>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name qbecaml)
 (synopsis
  "A library for generating and manipulating the QBE intermediate language")
 (description
  "QBE is a simple compiler backend utilizing a simple text-based intermediate language (https://c9x.me/compile). This library provides an object model to represent the textual form of the QBE intermediate language, as well as builders for constructing and emitting QBE IL.")
 (depends ocaml))
