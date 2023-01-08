{
  description = "A programming language built in Zig";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    zig = {
      url = "github:mitchellh/zig-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    zls = {
      url = "github:erikarvstedt/zls/fix-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {self, nixpkgs, flake-utils, zig, zls, gitignore }:
    flake-utils.lib.eachSystem (builtins.attrNames zig.packages) (system:
      let
        pkgs = import nixpkgs { inherit system; };
        zigLatest = zig.packages.${system}."0.10.0";
        inherit (gitignore.lib) gitignoreSource;
      in
        rec {
          packages = rec {
            default = polaric;

            polaric = pkgs.stdenvNoCC.mkDerivation {
              name = "zlox";
              version = "master";
              src = gitignoreSource ./.;
              nativeBuildInputs = [ zigLatest ];
              dontConfigure = true;
              dontInstall = true;
              buildPhase = ''
                mkdir -p $out
                zig build install -Drelease-safe=true --prefix $out
              '';
              XDG_CACHE_HOME = ".cache";
            };
          };

          devShells.default = pkgs.mkShell {
            buildInputs = (with pkgs; [
              zigLatest
              bashInteractive
              zls.packages.${system}.default
            ]);
          };
        }
    );
}
