final: prev: {
  talosctl = prev.talosctl.overrideAttrs (old: rec {
    version = "1.13.3";
    src = final.fetchFromGitHub {
      owner = "siderolabs";
      repo = "talos";
      tag = "v${version}";
      hash = "sha256-6WjjJXgsgLzE30VEpnl163nw4wjs8stRd7EGKFlSOoY=";
    };
    vendorHash = "sha256-TyBitraiVyjgfQnx2Yehd+81IDeJtjpHPx7MhJ/4uBE=";
  });
}
