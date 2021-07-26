{ pkgs            ? import <nixpkgs> {}
, source          ? ./.
, version         ? "dev"
, sharedLibraries ? (with pkgs; [openssl libxml2 libssh2 libuv])
}:
with pkgs;
with lib;
stdenv.mkDerivation rec {
  name = "clx-${version}";
  src = nix-gitignore.gitignoreSourcePure
    [./.gitignore]
    source;

  inherit version;

  buildInputs = [
    makeWrapper
    gcc
    sbcl
  ] ++ sharedLibraries;

  librariesEnvironment = ''
    # FIXME: impure path `/[...]' used in link
    export NIX_ENFORCE_PURITY=0

    export LD_LIBRARY_PATH=${lib.makeLibraryPath sharedLibraries}
    export CL_SOURCE_REGISTRY="(:source-registry :ignore-inherited-configuration)"
    # FIXME: version is hardcoded, this will break on update, could we do better?
    export CPATH="${src}/vendor/quicklisp/dists/quicklisp/software/cffi_0.24.1/"
  '';

  buildPhase = ''
    ${librariesEnvironment}
    export XDG_CACHE_HOME="$TMP/.cache"
    export HOME="$TMP"
    make build
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp main $out/bin/main.orig
    makeWrapper $out/bin/main.orig $out/bin/main --suffix LD_LIBRARY_PATH : $LD_LIBRARY_PATH
  '';

  dontStrip = true;
}
