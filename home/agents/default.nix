{ lib, ... }:
let
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
  };
in
{
  programs = {
    pi-coding-agent = {
      enable = true;
      settings = {
        defaultModel = lib.mkDefault "openai/gpt-5.6-terra";
        defaultProvider = lib.mkDefault "openrouter";
        theme = "tokyo-night-storm";
        themes = [ "themes" ];
        packages = [ "npm:pi-lens@4.0.0" ];
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
    ".pi/agent/themes".source = ./pi/themes;
    ".pi/agent/extensions/bell.ts".source = ./pi/extensions/bell.ts;

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
