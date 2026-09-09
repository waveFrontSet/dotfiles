{ lib, pkgs, ... }:
let
  pyright = pkgs.writeShellApplication {
    name = "pyright";
    runtimeInputs = [ pkgs.basedpyright ];
    text = ''
      exec basedpyright "$@"
    '';
  };

  mcpDocs = {
    adk-docs = {
      name = "AgentDevelopmentKit";
      url = "https://adk.dev/llms.txt";
    };
    pydantic-docs = {
      name = "PydanticDocs";
      url = "https://pydantic.dev/llms.txt";
    };
    langfuse-docs = {
      name = "LangfuseDocs";
      url = "https://langfuse.com/llms.txt";
    };
    docker-docs = {
      name = "DockerDocs";
      url = "https://docs.docker.com/llms.txt";
    };
    chainlit-docs = {
      name = "ChainlitDocs";
      url = "https://chainlit.io/llms.txt";
    };
    uv-docs = {
      name = "UVDocs";
      url = "https://docs.astral.sh/uv/llms.txt";
    };
    devenv-docs = {
      name = "DevenvDocs";
      url = "https://devenv.sh/llms-small.txt";
    };
  };

  # pi-mcp-adapter config: pull MCP servers from the opencode config
  # (~/.config/opencode/opencode.json, generated above from `mcp` + mcpDocs).
  piMcpConfig = {
    imports = [ "opencode" ];
    mcpServers = { };
  };
in
{
  programs = {
    pi-coding-agent = {
      enable = true;
      settings = {
        defaultModel = lib.mkDefault "openai/gpt-5.6-terra";
        defaultProvider = lib.mkDefault "openrouter";
        theme = "Tokyo Night Storm";
        packages = [
          "npm:pi-lens@4.0.0"
          "npm:@dietrichgebert/ponytail@4.9.0"
          "npm:pi-mcp-adapter"
          "npm:pi-web-access"
          "npm:pi-simplify"
          "npm:@plannotator/pi-extension"
          "npm:@juicesharp/rpiv-ask-user-question"
          "npm:@juicesharp/rpiv-todo"
        ];
      };
      context = ./AGENTS.md;
    };

    opencode = {
      enable = true;
      tui = {
        theme = "tokyonight";
        keybinds = {
          leader = "ctrl+x";
        };
        attention = {
          enabled = true;
          notifications = true;
          sound = true;
          volume = 0.4;
          sound_pack = "opencode.default";
        };
      };
      settings = {
        model = "anthropic/claude-sonnet-4-6";
        small_model = "anthropic/claude-haiku-4-5";
        autoupdate = true;
        share = "manual";
        plugin = [
          "@dietrichgebert/ponytail"
          "opencode-models-discovery@latest"
        ];
        enabled_providers = lib.mkDefault [
          "openrouter"
        ];
        permission = {
          edit = {
            "*" = "ask";
            "*.json" = "allow";
            "*.md" = "allow";
            "*.py" = "allow";
            "*.tf" = "allow";
            "*.toml" = "allow";
            "*.yaml" = "allow";
            "*.yml" = "allow";
          };
          bash = {
            "*" = "ask";
            "git add *" = "allow";
            "git commit *" = "allow";
            "git diff *" = "allow";
            "git log *" = "allow";
            "git status *" = "allow";
            "grep *" = "allow";
            "head *" = "allow";
            "tail *" = "allow";
            "cat *" = "allow";
            "uv *" = "allow";
            "ls *" = "allow";
            "find *" = "allow";
          };
        };
        compaction = {
          auto = true;
          prune = true;
        };
        formatter = {
          jq = {
            command = [
              "jq"
              "."
            ];
            extensions = [ "json" ];
          };
          prettier-yaml = {
            command = [
              "prettier"
              "--parser"
              "yaml"
            ];
            extensions = [
              "yaml"
              "yml"
            ];
          };
          prettier-markdown = {
            command = [
              "prettier"
              "--parser"
              "markdown"
            ];
            extensions = [ "md" ];
          };
          ruff-format = {
            command = [
              "ruff"
              "format"
            ];
            extensions = [
              "py"
              "pyi"
            ];
          };
          ruff-check = {
            command = [
              "ruff"
              "check"
              "--fix"
            ];
            extensions = [
              "py"
              "pyi"
            ];
          };
        };
        instructions = [ ];
        mcp = {
          excalidraw = {
            type = "remote";
            url = "https://mcp.excalidraw.com";
            enabled = true;
          };
        }
        // lib.mapAttrs (_: doc: {
          type = "local";
          command = [
            "uvx"
            "--from"
            "mcpdoc"
            "--with"
            "mcp[cli]<2"
            "mcpdoc"
            "--urls"
            "${doc.name}:${doc.url}"
            "--transport"
            "stdio"
          ];
          enabled = true;
        }) mcpDocs;
      };
      context = ./AGENTS.md;
      commands = {
        explain = ./opencode/commands/explain.md;
        review = ./opencode/commands/review.md;
        test = ./opencode/commands/test.md;
      };
    };
  };

  home.file = {

    # Pi
    ".pi/agent/mcp.json".text = builtins.toJSON piMcpConfig;
    ".pi/agent/plannotator.json".text = lib.mkDefault (
      builtins.toJSON {
        phases.executing.model = {
          provider = "openrouter";
          id = "glm-5.3-flash";
        };
      }
    );
    ".pi/agent/themes/tokyonight-storm.json".source =
      "${pkgs.vimPlugins.tokyonight-nvim.src}/extras/pi/tokyonight_storm.json";
    ".local/bin/pyright".source = "${pyright}/bin/pyright";
    ".pi-lens/lsp.json".text = builtins.toJSON {
      disabledServers = [
        "python"
        "python-jedi"
      ];
      servers.basedpyright = {
        name = "basedpyright";
        extensions = [
          ".py"
          ".pyi"
        ];
        command = "basedpyright-langserver";
        args = [ "--stdio" ];
        rootMarkers = [
          "pyproject.toml"
          "uv.lock"
          "setup.py"
          "setup.cfg"
          "requirements.txt"
          "Pipfile"
          ".venv"
          "venv"
        ];
      };
      serverOverrides.basedpyright.initializationOptions.basedpyright.analysis = {
        typeCheckingMode = "standard";
        diagnosticSeverityOverrides = {
          reportUnknownMemberType = "none";
          reportUnknownArgumentType = "none";
          reportUnknownVariableType = "none";
          reportUnknownParameterType = "none";
          reportUnknownLambdaType = "none";
          reportMissingTypeStubs = "none";
          reportMissingImports = "hint";
          reportAny = "none";
          reportExplicitAny = "none";
        };
      };
    };

    # Claude Code
    ".claude/settings.json".source = ./claude/settings.json;
    ".claude/hooks".source = ./claude/hooks;
    ".claude/CLAUDE.md".source = ./AGENTS.md;

    # Shared skills (Claude Code + OpenCode both read ~/.claude/skills/)
    ".claude/skills/review/SKILL.md".source = ./skills/review/SKILL.md;
    ".claude/skills/test/SKILL.md".source = ./skills/test/SKILL.md;
    ".claude/skills/explain/SKILL.md".source = ./skills/explain/SKILL.md;
  };
}
