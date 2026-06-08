(lang dune 3.15)

(version "v0.17.3.0")

(generate_opam_files true)

(name ppx_expect_nobase)

(authors "Jane Street Group, LLC")

(maintainers "Jane Street Group, LLC" "Dmitrii Kosarev a.k.a. Kakadu")

(source
 (github Kakadu/ppx_expect_nobase))

(license MIT)

(homepage "https://github.com/Kakadu/ppx_expect_nobase")

(bug_reports "https://github.com/Kakadu/ppx_expect_nobase/issues")

(package
 (name ppx_expect_nobase)
 (synopsis "Cram like framework for OCaml (with stripped dependencies)")
 (description
  "Testing framework: fork of ppx_expect, but with less dependecies.\nOriginal ppx_expect is a part of the Jane Street's PPX rewriters collection.")
 (depends
  (ocaml
   (or
    (and
     (>= 4.14.2)
     (< 5.0.0))
    (and
     (>= 5.3.0)
     (<= 5.4.0))))
  (ppx_inline_test_nobase
   (>= v0.17.0.2))
  sexplib
  ; re???
  (ppxlib
   (>= 0.35.0))
  opam-core
  dune-build-info
  ;
  ))
