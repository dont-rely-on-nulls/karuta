{
  description = "A compiler for a statically-typed relational programming language";

  # Flake dependency specification
  #
  # To update all flake inputs:
  #
  #     $ nix flake update --commit-lockfile
  #
  # To update individual flake inputs:
  #
  #     $ nix flake lock --update-input <input> ... --commit-lockfile
  #
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    flake-parts.url = "github:hercules-ci/flake-parts";

    # Precisely filter files copied to the nix store
    nix-filter.url = "github:numtide/nix-filter";

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, nix-filter, treefmt-nix }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];

      imports = [
        treefmt-nix.flakeModule
      ];

      perSystem = { config, self', inputs', pkgs, system, ... }:
        let
          # I'm setting OCaml to 5.4, but we must change this from
          # time to time.
          ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_4;
          # Setting OTP to version 27, since LFE is not supported
          # on 28 yet.
          beamPackages = pkgs.beam27Packages;

          # Filtered sources (prevents unecessary rebuilds)
          sources = {
            ocaml = nix-filter.lib {
              root = ./.;
              include = [
                ".ocamlformat"
                "dune-project"
                (nix-filter.lib.inDirectory "bin")
                (nix-filter.lib.inDirectory "lib")
                (nix-filter.lib.inDirectory "test")
                (nix-filter.lib.inDirectory "examples")
              ];
            };

            nix = nix-filter.lib {
              root = ./.;
              include = [
                (nix-filter.lib.matchExt "nix")
              ];
            };
          };

          pname = "karuta";
          version = "0.1.0";
        in
        {
          treefmt = {
            projectRootFile = "flake.nix";
            programs = {
              ocamlformat.enable = true;
              nixpkgs-fmt.enable = true;
            };
          };

          packages.default = ocamlPackages.buildDunePackage {
            inherit pname version;
            src = sources.ocaml;
            duneVersion = "3";
            buildInputs = [
              # Ocaml package dependencies needed to build go here.
              ocamlPackages.alcotest
              ocamlPackages.batteries
              ocamlPackages.cmdliner
              ocamlPackages.earlybird
              ocamlPackages.findlib
              ocamlPackages.lambda-term
              ocamlPackages.lwt
              ocamlPackages.lwt-exit
              ocamlPackages.num
              ocamlPackages.ppx_deriving
              ocamlPackages.ppx_enumerate
              ocamlPackages.ppxlib
              ocamlPackages.ppx_sexp_conv
              ocamlPackages.sexplib
              ocamlPackages.textutils
            ];
          };

          # nix run
          apps.default = {
            type = "app";
            program = "${self'.packages.default}/bin/karuta";
          };

          # Development Shell
          devShells.default = pkgs.mkShell {
            # Inherits build inputs from the package itself
            inputsFrom = [ self'.packages.default ];

            nativeBuildInputs = [
              # BEAM dependencies
              beamPackages.erlang
              beamPackages.lfe

              # OCaml dependencies
              pkgs.ocaml
              pkgs.dune_3

              # For `dune build @doc`
              ocamlPackages.odoc
              # This is needed after the flake update
              ocamlPackages.findlib
              # OCaml editor support
              ocamlPackages.ocaml-lsp
              # Nicely formatted types on hover
              ocamlPackages.ocamlformat
              ocamlPackages.ocamlformat-rpc-lib
              # Fancy REPL thing
              ocamlPackages.utop
              ocamlPackages.ppx_deriving
              ocamlPackages.ppxlib
              ocamlPackages.earlybird
              ocamlPackages.ppx_sexp_conv
              ocamlPackages.sexplib
              ocamlPackages.lwt
              ocamlPackages.lwt-exit
              ocamlPackages.lambda-term
              ocamlPackages.textutils
              ocamlPackages.cmdliner
              ocamlPackages.alcotest
            ];

            shellHook = ''
              echo "Welcome to the Karuta development shell!";
              echo "Run 'nix build' to build the project, and 'dune test' to run tests.";
              # Makes dune use as many cores as possible
              export DUNE_JOBS=$(nproc)
            '';
          };

          # treefmt-nix handles the formatting check automatically.
          checks = {
            default = config.treefmt.build.check self;
          };
        };
    };
}
