let nixpkgs = <nixpkgs>;
    config  = {};
in
with builtins;
with import nixpkgs { inherit config; };
with lib;
let
  shellWrapper = writeScript "shell-wrapper" ''
    #! ${stdenv.shell}
    set -e

    exec -a shell ${fish}/bin/fish --login --interactive --init-command='
      set -x root '"$root"'
      set config $root/.fish.conf
      set personal_config $root/.personal.fish.conf
      if test -e $personal_config
        source $personal_config
      end
      if test -e $config
        source $config
      end
    ' "$@"
  '';

  md4c = stdenv.mkDerivation rec {
    pname = "md4c";
    version = "0.4.7";

    src = fetchFromGitHub {
      owner = "mity";
      repo = "md4c";
      rev = "release-${version}";
      sha256 = "0m3202zzjvw4k7jw66z1qi3cbisxzvplq5alkygpifvhzm81gwwx";
    };

    nativeBuildInputs = [cmake];
  };
in stdenv.mkDerivation rec {
  name = "nix-shell";
  buildInputs = [
    glibcLocales bashInteractive man
    nix cacert curl utillinux coreutils
    git jq yq-go tmux findutils gnumake

    gerbil-unstable
    zlib
    gnuplot
    md4c
  ];
  shellHook = ''
    export root=$(pwd)

    if [ -f "$root/.env" ]
    then
      source "$root/.env"
    fi

    export LANG="en_US.UTF-8"
    export NIX_PATH="nixpkgs=${nixpkgs}"
    export GERBIL_BUILD_CORES=$(cat /proc/cpuinfo | grep -F processor | wc -l)
    export CFLAGS="-I${zlib.dev}/include -I${md4c}/include"
    export LDFLAGS="-lz -lmd4c"

    if [ ! -z "$PS1" ]
    then
      export SHELL="${shellWrapper}"
      exec "$SHELL"
    fi
  '';
}
