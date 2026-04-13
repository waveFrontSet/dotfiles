final: prev: {
  talosctl = prev.talosctl.overrideAttrs (old: rec {
    version = "1.11.6";
    src = final.fetchFromGitHub {
      owner = "siderolabs";
      repo = "talos";
      tag = "v${version}";
      hash = "sha256-RPmveQ52PqpD/OG/7SsJbKlZjggRU8uBqJyNmOszWtg=";
    };
    vendorHash = "sha256-l1OMJKNT1ceZIpMTkJA1Ki5VaP6eeqVq+HFq0fLykyY=";
  });
}
