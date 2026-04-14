{ pkgs, ... }:
{
  networking.hostName = "no-mans-work";
  environment = {
    systemPackages = with pkgs; [
      (google-cloud-sdk.withExtraComponents (
        with pkgs.google-cloud-sdk.components;
        [
          alpha
          beta
          cloud-run-proxy
        ]
      ))
    ];
  };
}
