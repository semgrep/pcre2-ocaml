{
  description = "PCRE2 bindings for OCaml";
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
    };
  };
  outputs = { self, flake-utils, opam-nix, nixpkgs, opam-repository }:
    let package = "pcre2";
    in flake-utils.lib.eachDefaultSystem (system:
      let
        # TODO Use pkgsStatic if on linux
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};
        opamRepos = [ "${opam-repository}" ];
        # The pure-OCaml engine is ported from PCRE2 10.44; the dev-only C
        # oracle must be the same version so conformance/differential tests
        # compare against identical upstream behavior.
        pcre2c1044 = pkgs.pcre2.overrideAttrs (old: {
          version = "10.44";
          src = pkgs.fetchurl {
            url =
              "https://github.com/PCRE2Project/pcre2/releases/download/pcre2-10.44/pcre2-10.44.tar.bz2";
            hash = "sha256-008C4RPPcZOh6/J3DTrFJwiNSF1OBH7RDl0hfG713pY=";
          };
          # The 10.42 derivation's sljit path fixups don't apply to 10.44.
          #
          # DEV-ONLY ORACLE HARDENING (NOT a vendor/ or upstream change).
          # This clamp patch pins a C UB in the 8-bit library to the pure
          # engine's documented DEFINED behavior so the differential fuzzer /
          # conformance oracle never crashes and never diverges spuriously on
          # out-of-range code points. In 8-bit UTF mode GET_UCD(ch) has NO
          # bounds guard (the guard exists only in the 32-bit build); an
          # over-long invalid lead byte decodes via GETUTF8INC to a code point
          # above MAX_UTF_CODE_POINT -- the 6-byte-form leads with bit 0 set
          # (0xFD, 0xFF) reach >= 0x40000000, while the other over-long leads
          # (0xF5..0xFB families) decode to smaller but still out-of-range
          # values -- all equally clamped. The OP_XCLASS \p{...} path feeds
          # that code point to GET_UCD -> ucd_stage1[] indexed out of bounds
          # (0xFF: tens of MB; ASan: pcre2_xclass.c:137; rare SIGSEGV inside
          # libpcre2-8, found by fuzz_diff seed 101 case 153814 under
          # PCRE2_MATCH_INVALID_UTF). The
          # patch clamps ch to MAX_UTF_CODE_POINT (0x10FFFF) -- matching
          # src/engine/ucd.ml record_index, NOT the 32-bit dummy record (which
          # would differ on bidiclass/bprops and cause spurious diffs). This
          # affects ONLY the dev oracle library; vendor/pcre2 stays pristine
          # and the published `pcre2` OCaml package is unaffected. Upstream-
          # reportable C bug; see oracle/patches/*.patch header for full detail.
          #
          # The vreverse patch pins a second UB in the same spirit:
          # OP_VREVERSE's per-character back-step `Feptr--; BACKCHAR(Feptr)`
          # (pcre2_match.c:5854-5855) uses the UNBOUNDED continuation-byte
          # walk, so under MATCH_INVALID_UTF a subject whose first code units
          # are all UTF-8 continuation bytes lets it walk below the subject
          # start (OOB read; the lookbehind then matches slack bytes and
          # records ovector entries at -1 == PCRE2_UNSET; fuzz seed 20260708
          # case 125828, minimized to /(?<=(.)?)/match_invalid_utf on
          # "\x80"). The patch bounds ONLY the walk at start_subject and caps
          # the step when it would cross (the 5848-5853 too-few/cap logic),
          # firing exactly where unpatched C reads out of bounds; all defined
          # behavior is untouched. Deliberately NOT a check_subject floor:
          # max_lookbehind under-counts nested lookbehinds
          # (pcre2_compile.c:9604-9612), so defined valid-UTF matching walks
          # below check_subject and must keep doing so. Matches
          # src/engine/interpreter.ml op_vreverse_utf_loop's DEVIATION pin.
          # See the patch header.
          patches = [
            ./oracle/patches/pcre2-10.44-oracle-ucd-clamp.patch
            ./oracle/patches/pcre2-10.44-oracle-vreverse-backchar-bound.patch
          ];
          postPatch = "";
        });
      in let
        devOpamPackagesQuery = {
          # You can add "development" ocaml packages here. They will get added
          # to the devShell automatically.
          ocaml-lsp-server = "*";
          utop = "*";
          ocamlformat = "*";
          earlybird = "*";
          merlin = "*";
        };
        opamQuery = devOpamPackagesQuery // {
          ## You can force versions of certain packages here
          # force the ocaml compiler to be 4.14.2 and from opam
          ocaml-base-compiler = "4.14.2";
          # FIXME: shouldn't be needed. doesn't pick up with-test deps?
          ounit2 = "*";
        };

        # repos = opamRepos to force newest version of opam
        scope = on.buildOpamProject' { repos = opamRepos; } ./. opamQuery;
        scopeOverlay = final: prev: {
          # You can add overrides here
          ${package} = prev.${package}.overrideAttrs (prev: {
            # Prevent the ocaml dependencies from leaking into dependent environments
            doNixSupport = false;
            # add ounit2 since it's not pulled in for whatever reason
            buildInputs = prev.buildInputs ++ [final.ounit2];
          });
        };
        scope' = scope.overrideScope' scopeOverlay;

        # for development
        devOpamPackages = builtins.attrValues
          (pkgs.lib.getAttrs (builtins.attrNames devOpamPackagesQuery) scope');

        # osemgrep/semgrep-core
        # package with all opam deps but nothing else
        baseOpamPackage = scope'.${package}; # Packages from devPackagesQuery

        pcre2 = baseOpamPackage.overrideAttrs (prev: rec {
          pname = "pcre2";
          buildInputs = prev.buildInputs;
          buildPhase' = ''
            dune build
          '';
        });
      in {

        packages.pcre2 = pcre2;

        formatter = pkgs.nixpkgs-fmt;
        devShells.default = pkgs.mkShell {
          # See comment above osemgrep.buildPhase for why we need this
          # This doesnt work there because idk
          shellHook = with pkgs; ''
            export NIX_CXXFLAGS_COMPILE="$NIX_CXXFLAGS_COMPILE -I${pkgs.libcxx.dev}/include/c++/v1"
          '';
          # Since the pure swap, package pcre2 has no C deps, so nothing pulls
          # in pkg-config or a pcre2 C library for the dev-only oracle
          # (pcre2-dev) — both must be explicit here. The nixpkgs pkg-config
          # wrapper only honors .pc paths accumulated from buildInputs (it
          # ignores ambient PKG_CONFIG_PATH), which is also what keeps the
          # oracle pinned to pcre2c1044 (10.44): it is the only pcre2 in the
          # shell. (History: while the 10.42 depext was still present, having
          # pcre2c1044 in buildInputs made GCC drop its -I for the dir's
          # -isystem entry behind 10.42's — moot now that the depext is gone.)
          inputsFrom = [ pcre2 ];
          buildInputs = devOpamPackages ++ [ pcre2c1044 pkgs.pkg-config ];
        };
      });
}
