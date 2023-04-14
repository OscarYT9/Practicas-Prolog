with import <nixpkgs> {};

stdenv.mkDerivation {
    name = "csb";
    buildInputs = [
        swiProlog
    ];
    shellHook = ''
        export PATH="$PWD/node_modules/.bin/:$PATH"
    '';
}

