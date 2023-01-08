{
  description = "A programming language built in Zig";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    zig = {
      url = "github:mitchellh/zig-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {self, nixpkgs, flake-utils, zig }:
    flake-utils.lib.eachSystem (builtins.attrNames zig.packages) (system:
      let
        pkgs = import nixpkgs { inherit system; };
        zigLatest = zig.packages.${system}."0.10.0";
      in
        rec {
          devShells.default = pkgs.mkShell {
            buildInputs = (with pkgs; [
              zigLatest
              bashInteractive
            ]);
          };
        }
    );
}
