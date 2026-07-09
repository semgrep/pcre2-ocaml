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
            # Force the 10.44 C oracle ahead of the 10.42 depext.
            export PKG_CONFIG_PATH="${pcre2c1044.dev}/lib/pkgconfig:$PKG_CONFIG_PATH"
          '';
          # NOTE: pcre2c1044 is deliberately NOT in buildInputs: that would add
          # its include dir as -isystem, and GCC ignores a -I that duplicates
          # an -isystem dir, which lets the 10.42 depext headers win. The
          # PKG_CONFIG_PATH export above + discover.ml's -I/-L are sufficient.
          inputsFrom = [ pcre2 ];
          buildInputs = devOpamPackages;
        };
      });
}
