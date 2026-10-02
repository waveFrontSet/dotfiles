{
  lib,
  pkgs,
  username,
  ...
}:
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
      antigravity-cli
      _1password-cli
    ];
  };
  home-manager.users.${username} = {
    programs.pi-coding-agent.settings = {
      packages = lib.mkAfter [
        "npm:pi-models-discovery"
      ];
      defaultModel = "gpt-6-luna";
      defaultProvider = "pcg";
    };
    home.file.".pi/agent/models.json".text = builtins.toJSON {
      providers.pcg = {
        name = "PCG AI Gateway";
        baseUrl = "https://gateway.pcg.io/v1";
        api = "openai-completions";
        apiKey = "$PCG_API_KEY";
        discoverModels = true;
      };
    };

    home.file.".pi/agent/plannotator.json".text = builtins.toJSON {
      phases.executing.model = {
        provider = "pcg";
        id = "gpt-6-luna";
      };
    };
    programs.opencode.settings = {
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
