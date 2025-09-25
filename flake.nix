{
  description = "Flake for keyboard tones cheatsheet";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
      in
      with pkgs;
      {
        devShells.default = mkShell {
          nativeBuildInputs = [
            bashInteractive
            clojure
            clojure-lsp
            neil

            typst
            roboto
          ];

          shellHook = ''
            export TYPST_FONT_PATHS=${with pkgs; with builtins; concatStringsSep ":" (map (fontPkg: fontPkg + "/share/fonts/truetype")
                                                                                      [
                                                                                          roboto
                                                                                      ]
                                                                                     )}
            '';
        };
      }
    );
}
