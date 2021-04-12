## Use nixpkgs from environment
#import <nixpkgs>

## Use frozen nixpkgs from internet
with builtins;
import (fetchTarball {
  url    = "https://github.com/nixos/nixpkgs/archive/1d61041fdfbf4ab98044b58561b08edf98fcb6fb.tar.gz";
  sha256 = "1bihs9z1fmqmrychi3vcq2glj0bylk9vy2yhv2dhbjg31fmnl4vc";
})
