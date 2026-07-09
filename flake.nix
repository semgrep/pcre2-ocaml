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
          patches = [ ];
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
