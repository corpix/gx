let nixpkgs = import ./nixpkgs.nix;
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

  gambit = { callPackage, fetchurl }:
    callPackage "${nixpkgs}/pkgs/development/compilers/gambit/build.nix" rec {
      version = "4.9.3";
      git-version = version;
      src = fetchurl {
        url = "http://www.iro.umontreal.ca/~gambit/download/gambit/v4.9/source/gambit-v4_9_3.tgz";
        sha256 = "1p6172vhcrlpjgia6hsks1w4fl8rdyjf9xjh14wxfkv7dnx8a5hk";
      };
    };

  gerbil = { callPackage, fetchFromGitHub, gambit, gambit-support }:
    callPackage "${nixpkgs}/pkgs/development/compilers/gerbil/build.nix" rec {
      version = "unstable-2020-11-05";
      git-version = "0.16-152-g808929ae";
      src = fetchFromGitHub {
        owner = "vyzo";
        repo = "gerbil";
        rev = "808929aeb8823959191f35df53bc0c0150911b4b";
        sha256 = "0d9k2gkrs9qvlnk7xa3gjzs3gln3ydds7yd2313pvbw4q2lcz8iw";
      };
      inherit gambit gambit-support;
      gambit-params = gambit-support.unstable-params;
    };

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

  scheme = let
    gambitscheme = callPackage gambit {};
    gerbilscheme = callPackage gerbil { gambit = gambitscheme; };
  in [
    #gambitscheme
    #gerbilscheme
    gerbil-unstable
  ];
in stdenv.mkDerivation rec {
  name = "nix-shell";
  buildInputs = [
    glibcLocales bashInteractive man
    nix cacert curl utillinux coreutils
    git jq yq-go tmux findutils gnumake
    hivemind

    zlib md4c libyaml openssl
    clickhouse prometheus zookeeper
    gnuplot
    ansible

  ] ++ scheme;
  shellHook = ''
    export root=$(pwd)

    if [ -f "$root/.env" ]
    then
      source "$root/.env"
    fi

    export LANG="en_US.UTF-8"
    export NIX_PATH="nixpkgs=${nixpkgs}"

    export GERBIL_BUILD_CORES=$(cat /proc/cpuinfo | grep -F processor | wc -l)
    # WTF?! strace -s4096 -o out gxi -e "(import :corpix/gx/test)" says it only looks into the $GERBIL_PATH/lib
    export GERBIL_LOADPATH=$HOME/.gerbil/pkg

    export CFLAGS="-I${zlib.dev}/include -I${md4c}/include -I${libyaml}/include"
    export LDFLAGS="-lz -lmd4c -lyaml"

    if [ ! -z "$PS1" ]
    then
      export SHELL="${shellWrapper}"
      exec "$SHELL"
    fi
  '';
}
