{ skills, ... }:
{
  home.file = {
    # OpenCode
    ".config/opencode/opencode.json".source = ./opencode/opencode.json;
    ".config/opencode/tui.json".source = ./opencode/tui.json;
    ".config/opencode/commands".source = ./opencode/commands;
    ".config/opencode/plugins".source = ./opencode/plugins;
    ".config/opencode/AGENTS.md".source = ./AGENTS.md;

    # Claude Code
    ".claude/settings.json".source = ./claude/settings.json;
    ".claude/hooks".source = ./claude/hooks;
    ".claude/CLAUDE.md".source = ./AGENTS.md;

    # Shared skills (Claude Code + OpenCode both read ~/.claude/skills/)
    ".claude/skills/review/SKILL.md".source = ./skills/review/SKILL.md;
    ".claude/skills/test/SKILL.md".source = ./skills/test/SKILL.md;
    ".claude/skills/explain/SKILL.md".source = ./skills/explain/SKILL.md;

    # Third-party skills (flake inputs)
    ".claude/skills/allium".source = skills.allium;
  };
}
