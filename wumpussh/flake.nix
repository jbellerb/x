{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    gomod2nix = {
      url = "github:nix-community/gomod2nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { self, nixpkgs, ... }@inputs:
    let
      system = "x86_64-linux";

      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          self.overlays.default
          (final: prev: inputs.gomod2nix.legacyPackages."${system}")
        ];
      };

      treefmt = inputs.treefmt-nix.lib.evalModule pkgs (
        { pkgs, ... }:
        {
          projectRootFile = "flake.nix";
          programs = {
            nixfmt.enable = true;
            gofmt.enable = true;
          };
          settings.global.excludes = [
            ".envrc"
            "LICENSE"
            "README.md"
            "gomod2nix.toml"
          ];
        }
      );

      goHelperFunctionsHook = pkgs.makeSetupHook ({ name = "go-helper-functions-hook"; }) (
        pkgs.writeScript "go-helper-functions-hook.sh" ''
          findGoDirs() {
            find . -path ./vendor -prune -o -type f -name "*.go"\
              -exec dirname {} \; | sort | uniq
          }
        ''
      );

      pname = "wumpussh";
      version = "0.1.0";

      src = pkgs.lib.fileset.toSource {
        root = ./.;
        fileset = pkgs.lib.fileset.unions [
          ./go.mod
          (pkgs.lib.fileset.fileFilter (file: file.hasExt "go") ./.)
        ];
      };

      commonArgs = {
        inherit pname version src;
        modules = ./gomod2nix.toml;

        nativeBuildInputs = [ goHelperFunctionsHook ];
      };

    in
    {
      packages."${system}" = rec {
        wumpussh = pkgs.buildGoApplication commonArgs;
        default = wumpussh;
      };

      checks."${system}" = {
        formatting = treefmt.config.build.check self;

        wumpussh-staticcheck = pkgs.buildGoApplication (
          commonArgs
          // {
            version = "staticcheck";
            postConfigure = ''
              export STATICCHECK_CACHE=$TMPDIR/staticcheck
            '';
            buildPhase = ''
              for dir in $(findGoDirs); do
                ${pkgs.go-tools}/bin/staticcheck "$dir"
              done
            '';
            doCheck = false;
          }
        );

        wumpussh-go-vet = pkgs.buildGoApplication (
          commonArgs
          // {
            version = "go-vet";
            buildPhase = ''
              for dir in $(findGoDirs); do
                go vet "$dir"
              done
            '';
            doCheck = false;
          }
        );
      };

      devShells."${system}".default = pkgs.mkShell {
        nativeBuildInputs = with pkgs; [
          go
          gopls
          go-tools
        ];
      };

      overlays.default = final: prev: { } // self.packages."${system}";

      apps."${system}" = rec {
        wumpussh = {
          type = "app";
          program = "${pkgs.wumpussh}/bin/wumpussh";
          lock = {
            type = "app";
            program = "${pkgs.gomod2nix}/bin/gomod2nix";
          };
        };

        default = wumpussh;
      };

      formatter."${system}" = treefmt.config.build.wrapper;
    };
}
