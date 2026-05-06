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
    (ocaml (>= 4.12))
    dune-configurator
    ; NOTE: cannot be a test only dep since the ppx must be used to remove the
    ; test code when _not_ in a test conf.
    ppx_inline_test
    (conf-libpcre2-8 :build)
    (ounit2 :with-test)
  )
)

(package
  (name pcre2-bin-prot)
  (synopsis "Bin_prot serializers for pcre2 compiled patterns")
  (description "\
Provides Bin_prot.Binable.S over Pcre2.Interp.t, implemented on top of
Pcre2.Interp.to_bytes / of_bytes. JIT-compiled patterns are not
supported; serialize before JIT-compiling.")
  (depends
    (ocaml (>= 4.12))
    (pcre2 (= :version))
    bin_prot
    (ounit2 :with-test)
  )
)
