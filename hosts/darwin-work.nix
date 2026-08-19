{ pkgs, username, ... }:
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
  home-manager.users.${username}.programs = {
    opencode.settings = {
      provider.pcg = {
        npm = "@ai-sdk/openai-compatible";
        name = "PCG AI Gateway";
        options = {
          baseURL = "https://gateway.pcg.io";
          modelsDiscovery.enabled = true;
        };
      };
      enabled_providers = [ "pcg" ];
    };
  };
}
