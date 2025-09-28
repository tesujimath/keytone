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

        ci-packages = with pkgs; [
          bashInteractive
          clojure

          coreutils
          git
          just
          elvish
          typst
        ];

        dev-packages = with pkgs; ci-packages ++ [
          clojure-lsp
          neil
        ];

        export-typst-font-paths = ''
          export TYPST_FONT_PATHS=${with pkgs; with builtins; concatStringsSep ":" (map (fontPkg: fontPkg + "/share/fonts/truetype")
                                                                                    [
                                                                                        roboto
                                                                                    ]
                                                                                   )}
        '';
      in
      with pkgs;
      {
        devShells.default = mkShell {
          nativeBuildInputs = dev-packages;

          shellHook = export-typst-font-paths;
        };

        apps = {
          tests = {
            type = "app";
            program = "${writeShellScript "keytone-tests" ''
                export PATH=${pkgs.lib.makeBinPath ci-packages}
                ${export-typst-font-paths}
                just test
              ''}";
          };

          build = {
            type = "app";
            program = "${writeShellScript "keytone-build" ''
                export PATH=${pkgs.lib.makeBinPath ci-packages}
                ${export-typst-font-paths}
                just build
              ''}";
          };
        };
      }
    );
}
