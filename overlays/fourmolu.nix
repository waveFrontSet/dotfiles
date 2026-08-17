{
  stdenvNoCC,
  fetchurl,
  unzip,
  autoPatchelfHook,
  stdenv,
  gmp,
  ncurses,
  zlib,
  lib,
}:

let
  version = "0.20.0.0";
  isLinux = stdenv.hostPlatform.isLinux;
  plat =
    {
      "aarch64-darwin" = {
        suffix = "darwin-arm64";
        hash = "sha256-raI9mgo3DURn5zEPcp/vO/9u5SfTLmK1w36mbmiL89o=";
      };
      "x86_64-darwin" = {
        suffix = "darwin-x86_64";
        hash = "sha256-M+ZW5CEQroXWcJVnI3zYaZ8Fn3ZCoUegSH0gNTwhNcA=";
      };
      "x86_64-linux" = {
        suffix = "linux-x86_64";
        hash = "sha256-weKQ9QQIem+hpke5wZVGUS/uAY5GX37EhCyYWrnimrw=";
      };
      "aarch64-linux" = {
        suffix = "linux-arm64";
        hash = "sha256-EeyEHNY/z2LShmM1obwXOIuoP4fyKs3iL84oG7bYeZQ=";
      };
    }
    .${stdenvNoCC.hostPlatform.system}
      or (throw "fourmolu: no prebuilt binary for ${stdenvNoCC.hostPlatform.system}");
in
stdenvNoCC.mkDerivation {
  pname = "fourmolu";
  inherit version;

  src = fetchurl {
    url = "https://github.com/fourmolu/fourmolu/releases/download/v${version}/fourmolu-${version}-${plat.suffix}.zip";
    inherit (plat) hash;
  };

  sourceRoot = "fourmolu-${version}-${plat.suffix}";

  nativeBuildInputs = [ unzip ] ++ lib.optionals isLinux [ autoPatchelfHook ];
  # Linux release binaries are dynamically linked against glibc; patch for NixOS.
  # The darwin binaries link only system dylibs and need no patching.
  buildInputs = lib.optionals isLinux [
    stdenv.cc.cc.lib
    gmp
    ncurses
    zlib
  ];

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall
    install -Dm755 fourmolu "$out/bin/fourmolu"
    runHook postInstall
  '';

  meta = {
    description = "Fourmolu Haskell formatter (prebuilt release binary)";
    homepage = "https://github.com/fourmolu/fourmolu";
    mainProgram = "fourmolu";
    platforms = [
      "aarch64-darwin"
      "x86_64-darwin"
      "x86_64-linux"
      "aarch64-linux"
    ];
  };
}
