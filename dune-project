(lang dune 3.23)

(name shamir)

(generate_opam_files true)

(source
 (github kdkasad/shamir))

(authors "Kian Kasad <kian@kasad.com>")

(license MIT)

(package
 (name shamir)
 (synopsis "Shamir secret sharing tool")
 (depends
   (ocaml (>= 5.1))
   dune
   ;; Runtime dependencies
   ;; Testing dependencies
   (ppx_expect (and (>= v0.17) :with-test))
   (ppx_inline_test (and (>= v0.17) :with-test))))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
