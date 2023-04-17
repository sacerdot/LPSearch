(lang dune 3.7)

(name LPSearch)

(generate_opam_files true)

(source
 (github sacerdot/LPSearch))

(authors "Claudio Sacerdoti Coen")

(maintainers "Claudio Sacerdoti Coen")

(license CECILL-2.1)

(documentation https://github.com/sacerdot/LPSearch)

(package
 (name LPSearch)
 (synopsis "An indexer and search engine for LambdaPi/Dedukti")
 (description "An indexer and search engine for LambdaPi/Dedukti")
 (depends ocaml dune lambdapi)
 (tags
  (search index lambdapi dedukti "interactive theorem provers")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
