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
      _1password-cli
    ];
  };
  home-manager.users.${username} = {
    programs = {
      antigravity-cli.settings.gcp = {
        project = "pcg-gemini";
        location = "eu";
      };
      pi-coding-agent.settings = {
        packages = lib.mkAfter [
          "npm:pi-models-discovery"
        ];
        defaultModel = "gpt-6-luna";
        defaultProvider = "pcg";
      };
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
  };
  homebrew.brews = [
    "weasyprint"
  ];
}
