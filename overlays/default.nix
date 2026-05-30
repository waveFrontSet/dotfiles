final: prev: {
  talosctl = prev.talosctl.overrideAttrs (old: rec {
    version = "1.12.8";
    src = final.fetchFromGitHub {
      owner = "siderolabs";
      repo = "talos";
      tag = "v${version}";
      hash = "sha256-EqeohwiE5f1ka1l+gAtM5jZ5KjTQy0oV4JH4Alaie7A=";
    };
    vendorHash = "sha256-9J1XcBJrJiiEMEvwHtHdbZkLSLJaRQcpICNE3hcCwz8=";
  });
}
