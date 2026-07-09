(lang dune 3.12)
(name pcre2)

(generate_opam_files true)

(source (github semgrep/pcre2-ocaml))
(license "LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception")
(homepage "https://github.com/semgrep/pcre2-ocaml")

(maintainers "Cooper Pierce <cooper@semgrep.com>")
(authors
  "Cooper Pierce <cooper@semgrep.com>"
  "Markus Mottl <markus.mottl@gmail.com>")

(package
  (name pcre2)
  (synopsis "Bindings to the Perl Compatibility Regular Expressions library (version 2)")
  (description "\
pcre2-ocaml offers library functions for string pattern matching and
substitution, similar to the functionality offered by the Perl language.")
  (depends
    (ocaml (>= 4.14))
    ; NOTE: cannot be a test only dep since the ppx must be used to remove the
    ; test code when _not_ in a test conf.
    ppx_inline_test
    ppx_deriving
    (ounit2 :with-test)
  )
)

; Development-only package: C oracle bindings (differential testing + perf
; baseline), pcre2test harness, conformance runner, fuzzer, benchmarks.
; Never published to opam.
(package
  (name pcre2-dev)
  (synopsis "Dev-only test oracle and conformance tooling for pcre2 (unpublished)")
  (depends
    (ocaml (>= 4.14))
    pcre2
    dune-configurator
    ppx_inline_test
    ppx_deriving
    (conf-libpcre2-8 :build)
    ounit2
  )
)
