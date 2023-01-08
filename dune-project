(lang dune 3.5)
(name tianshu-cil)
(implicit_transitive_deps false)
(generate_opam_files true)
(version b784182-dirty)
(source (github gsfan15/tianshu-cil))
(documentation "https://github.com/gsfan15/tianshu-cil")
(homepage "https://github.com/gsfan15/tianshu-cil")
(authors "Guangsheng Fan")
(maintainers "Guangsheng Fan <guangshengfan@nudt.edu.cn>" )
(license "BSD-3-Clause")

(package
  (name tianshu-cil)
  (synopsis "A front-end for the C programming language that facilitates program analysis and transformation")
  (description "\
This is a fork of the 'cil' package used for 'goblint'. Major changes include:
* Support for C99 and C11.
* Compatibility with modern OCaml versions.
* Use Zarith instead of Num and use that for integer constants.
* Improved locations with columns and spans.
* Removal of unmaintained extensions and MSVC support.
* Use dune instead of make and ocamlbuild.
* Many bug fixes.")
  (depends
    (ocaml (>= 4.05.0))
    (ocamlfind :with-test)
    num
    (hevea :with-doc)
    dune
    dune-configurator
    (odoc :with-doc)
    stdlib-shims
    ;(ppx_deriving_yojson (>= 3.2))
    ;yojson
    conf-perl
    cppo
    conf-gcc
  )
  (conflicts cil) ; only because we both install the cilly executable
  
)
