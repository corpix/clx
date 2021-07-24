{ pkgs            ? import <nixpkgs> {}
, source          ? ./.
, version         ? "dev"
, sharedLibraries ? (with pkgs; [openssl libxml2])
}:
with pkgs;
with lib;
stdenv.mkDerivation {
  name = "clx-${version}";
  src = nix-gitignore.gitignoreSourcePure
    [./.gitignore]
    source;

  inherit version;

  buildInputs = [
    makeWrapper
    sbcl
  ] ++ sharedLibraries;

  buildPhase = ''
    export LD_LIBRARY_PATH=${lib.makeLibraryPath sharedLibraries}
    export CL_SOURCE_REGISTRY="(:source-registry :ignore-inherited-configuration)"
    export HOME="$TMP"
    export XDG_CACHE_HOME="$TMP/.cache"

    make build
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp main $out/bin/main.orig
    makeWrapper $out/bin/main.orig $out/bin/main --suffix LD_LIBRARY_PATH : $LD_LIBRARY_PATH
  '';

  dontStrip = true;
}
