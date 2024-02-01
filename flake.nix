{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-23.11";
  inputs.utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, utils }:
    utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs.elmPackages; [
            elm
            elm-format
            elm-review
            elm-test
            elm-json
          ];
        };
      });
}
