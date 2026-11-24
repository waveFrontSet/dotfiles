{
  lib,
  pkgs,
  mattpocock-skills,
  ...
}:
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
  opencodeMcp = {
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
  piMcp = {
    excalidraw = {
      url = "https://mcp.excalidraw.com";
      enabled = true;
    };
    odoo-mcp = {
      url = "https://external-mcp.gateway.pcg.io/mcp/odoo-mcp";
      enabled = true;
    };
    jira-mcp = {
      url = "https://external-mcp.gateway.pcg.io/mcp/atlassian";
      enabled = true;
    };
  }
  // lib.mapAttrs (_: doc: {
    command = "uvx";
    args = [
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
in
{
  programs = {
    antigravity-cli = {
      enable = true;
      mutableSettings = true;
      settings = {
        colorScheme = "tokyo night";
      };
      permissions = {
        allow = [
          # Filesystem inspection
          "command(ls)"
          "command(ll)"
          "command(tree)"
          "command(pwd)"
          "command(stat)"
          "command(file)"
          "command(wc)"
          "command(du)"
          "command(df)"
          "command(realpath)"
          "command(readlink)"
          "command(which)"

          # Reading & searching
          "command(cat)"
          "command(head)"
          "command(tail)"
          "command(grep)"
          "command(rg)"
          "command(fd)"
          "command(find)"
          "command(jq)"
          "command(yq)"
          "command(diff)"
          "command(sort)"
          "command(uniq)"
          "command(cut)"
          "command(tr)"
          "command(echo)"

          # Git (read-only)
          "command(git status)"
          "command(git log)"
          "command(git show)"
          "command(git diff)"
          "command(git ls-files)"
          "command(git rev-parse)"
          "command(git blame)"
          "command(git describe)"
          "command(git remote -v)"
        ];
        deny = [
          # Forbid reading/writing .env files (while still allowing .envrc)
          "read_file(.env)"
          "read_file(.env.*)"
          "read_file(**/.env)"
          "read_file(**/.env.*)"
          "command(regex:(^|[\\s/\"'=])\\.env(\\.[a-zA-Z0-9_-]+)?($|[\\s\"';|&]))"
        ];
      };
      mcpServers = piMcp;
      context.GEMINI = ./AGENTS.md;
      skills = {
        review = ./skills/review;
        test = ./skills/test;
        explain = ./skills/explain;

        # Matt Pocock's skills (grill-with-docs + wayfinder and their skill dependencies)
        grill-with-docs = "${mattpocock-skills}/skills/engineering/grill-with-docs";
        wayfinder = "${mattpocock-skills}/skills/engineering/wayfinder";
        grilling = "${mattpocock-skills}/skills/productivity/grilling";
        domain-modeling = "${mattpocock-skills}/skills/engineering/domain-modeling";
        research = "${mattpocock-skills}/skills/engineering/research";
        prototype = "${mattpocock-skills}/skills/engineering/prototype";
        setup-matt-pocock-skills = "${mattpocock-skills}/skills/engineering/setup-matt-pocock-skills";
      };
    };

    pi-coding-agent = {
      enable = true;
      settings = {
        defaultModel = lib.mkDefault "openai/gpt-6-luna";
        defaultProvider = lib.mkDefault "openrouter";
        theme = "Tokyo Night Storm";
        packages = [
          "npm:pi-lens"
          "npm:@dietrichgebert/ponytail"
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
          "opencode-models-discovery"
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
        mcp = opencodeMcp;
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
    ".pi/agent/mcp.json".text = builtins.toJSON { mcpServers = piMcp; };
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
    ".pi-lens/config.json".text = builtins.toJSON {
      lsp = {
        format.mode = "immediate";
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
