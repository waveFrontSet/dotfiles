final: prev: {
  talosctl = prev.talosctl.overrideAttrs (old: rec {
    version = "1.12.7";
    src = final.fetchFromGitHub {
      owner = "siderolabs";
      repo = "talos";
      tag = "v${version}";
      hash = "sha256-8RiEiOqWuFcE/cFUjm9EyC3HCK3QTFjSBudTW05XtdQ=";
    };
    vendorHash = "sha256-6n3h/RAp25IjFjVQ4g6tCJi6sEvKtj59BHdjdCnDl7E=";
  });
}
