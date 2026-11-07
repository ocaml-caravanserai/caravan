{
  description = "caravan: OCaml 5.5 development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    # The fork bikallem/windtrap: it has changes after the release 0.2.0
    # of invariant-hq, and it has no release. flake.lock pins a commit of
    # its main branch.
    windtrap = {
      url = "github:bikallem/windtrap";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    weft = {
      url = "github:ocaml-caravanserai/weft";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.windtrap.follows = "windtrap";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      windtrap,
      weft,
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];

      forAllSystems =
        f:
        nixpkgs.lib.genAttrs systems (
          system:
          let
            pkgs = nixpkgs.legacyPackages.${system};
            # http-date's pinned commit declares (lang dune 3.24), so it is
            # built with this dune and the dev shell uses it too. Other
            # packages keep nixpkgs' dune.
            dune_3_24 = pkgs.ocaml-ng.ocamlPackages_5_5.dune_3.overrideAttrs (_: rec {
              version = "3.24.2";
              src = pkgs.fetchurl {
                url = "https://github.com/ocaml/dune/releases/download/${version}/dune-${version}.tbz";
                hash = "sha256-RyeYaRsCFtr1OHCfD0cDs2F+8krQhmyQlgaLqrpNdio=";
              };
            });
            ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_5.overrideScope (
              final: prev: {
                # Not on opam yet: the package of the weft flake.
                weft = final.callPackage weft.lib.package { };
                # The TLS flow of weft, for caravan.tls.
                weft-tls = final.callPackage weft.lib.tlsPackage { };

                # nixpkgs ships an older windtrap and no ppx_windtrap. The two
                # packages of the windtrap flake use the OCaml of this scope.
                windtrap = final.callPackage windtrap.lib.windtrap { };
                ppx_windtrap = final.callPackage windtrap.lib.ppx_windtrap { };

                # Not packaged in nixpkgs yet: https://ocaml.org/p/http-date/latest
                # Pinned to an untagged commit (after v0.2) until v0.3 is
                # released. It declares (lang dune 3.24) and nixpkgs has dune
                # 3.23.1, so only this package builds with a newer dune.
                http-date = (final.buildDunePackage.override { dune_3 = dune_3_24; }) {
                  pname = "http-date";
                  version = "0.2-unstable-2026-09-30";
                  minimalOCamlVersion = "4.11";
                  src = pkgs.fetchFromGitHub {
                    owner = "bikallem";
                    repo = "http-date";
                    rev = "8924a84d249a2773f12d79c442bf133eff5d549f";
                    hash = "sha256-6TsG2TvD7z+HRpLRwXq+m+p9ouAHOIvZpsIRMkD52qA=";
                  };
                  meta = {
                    description = "RFC 9110 HTTP date decoder and encoder";
                    homepage = "https://github.com/bikallem/http-date";
                    license = pkgs.lib.licenses.mpl20;
                  };
                };
              }
            );
          in
          f { inherit pkgs ocamlPackages dune_3_24; }
        );
    in
    {
      # All the libraries of caravan (see the table of README.md). The source
      # has only the files of the build, with no test directory, so the package
      # does not run the tests. The installed http-date declares (lang dune
      # 3.24), so caravan builds with dune 3.24 too.
      packages = forAllSystems (
        {
          pkgs,
          ocamlPackages,
          dune_3_24,
        }:
        rec {
          caravan = (ocamlPackages.buildDunePackage.override { dune_3 = dune_3_24; }) {
            pname = "caravan";
            version = "0.1.0-dev";
            minimalOCamlVersion = "5.5";
            src = pkgs.lib.fileset.toSource {
              root = ./.;
              fileset = pkgs.lib.fileset.unions [
                ./dune-project
                ./caravan.opam
                ./src
                ./client
                ./websocket
                ./router
                ./middleware
                ./static
                ./tls
                ./session
                ./auth
                ./compress
                ./html
                ./http2
                ./jobs
                ./testing
                ./LICENSE
                ./README.md
              ];
            };
            propagatedBuildInputs = [
              ocamlPackages.weft
              ocamlPackages.http-date
              # caravan.tls, an optional library.
              ocamlPackages.weft-tls
              ocamlPackages.tls
              ocamlPackages.x509
              ocamlPackages.domain-name
              # caravan.session, an optional library.
              ocamlPackages.digestif
              ocamlPackages.eqaf
              ocamlPackages.mirage-crypto-rng
              # caravan.auth, an optional library.
              ocamlPackages.mirage-crypto-pk
              ocamlPackages.mirage-crypto-ec
              ocamlPackages.yojson
              ocamlPackages.zarith
              # caravan.compress, an optional library.
              ocamlPackages.decompress
            ];
            doCheck = false;
            meta = {
              description = "A web programming library for OCaml 5, built on weft";
              homepage = "https://github.com/bikallem/caravan";
              license = pkgs.lib.licenses.mpl20;
            };
          };
          default = caravan;
        }
      );

      devShells = forAllSystems (
        {
          pkgs,
          ocamlPackages,
          dune_3_24,
        }:
        {
          default = pkgs.mkShell {
            packages = with ocamlPackages; [
              # Toolchain (http-date at its pinned commit needs dune 3.24)
              ocaml
              dune_3_24
              findlib

              # Dependencies (weft here is the flake input, so use ocamlPackages.weft)
              ocamlPackages.weft
              http-date

              # caravan.tls (optional), and its tests: a seeded generator and
              # the test certificates.
              ocamlPackages.weft-tls
              tls
              x509
              domain-name
              mirage-crypto-rng
              mirage-crypto-ec
              ptime
              digestif
              eqaf
              # caravan.auth (optional)
              mirage-crypto-pk
              yojson
              zarith
              # caravan.compress (optional)
              decompress

              # Test dependencies (windtrap here is the flake input, so use
              # ocamlPackages.windtrap)
              ocamlPackages.windtrap
              ocamlPackages.ppx_windtrap
              # The tests of the examples use base64 directly.
              base64

              # Dev tools
              ocaml-lsp
              utop
              ocamlformat
              odoc
              # make h2spec: the conformance suite of HTTP/2.
              pkgs.h2spec
            ];
          };
        }
      );

      formatter = forAllSystems ({ pkgs, ... }: pkgs.nixfmt-rfc-style);
    };
}
