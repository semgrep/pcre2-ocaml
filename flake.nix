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
          inputsFrom = [ pcre2 ];
          buildInputs = devOpamPackages;
        };
      });
}
