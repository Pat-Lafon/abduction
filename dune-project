(lang dune 3.16)

(name abduction)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Patrick LaFontaine")

(maintainers "Patrick LaFontaine")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name abduction)
 (synopsis "Coverage-type abduction over the CEGIS engine")
 (description
  "Infers missing coverage refinements for test input generators by driving the coverage_abduction CEGIS engine against CoverageType subtyping checks.")
 (depends
  ocaml
  dune
  core
  z3
  ppx_optcomp
  ppx_compare
  ppx_sexp_conv
  ppx_inline_test
  ppx_assert
  ppx_deriving
  ppx_here
  zutils)
 (tags
  (topics "to describe" your project)))
