{ ... }:
{
  programs.git = {
    enable = true;
    ignores = [
      # OS X Database files
      ".DS_Store"
      ".AppleDouble"
      ".LSOverride"
      "Icon"

      # Temp-Files for emacs
      "*#"
      "*~"

      # Thumbnails
      "._*"

      # Files that might appear on external disk
      ".Spotlight-V100"
      ".Trashes"

      # Vim swap-files
      "*.swp"

      # VS-Code
      ".vscode"

      # Pycharm
      ".idea"

      # Projectile
      ".dir-locals.el"

      # direnv
      ".direnv/"
      ".envrc"

      # python
      ".venv"
      "default.profraw"

      # ruby
      "vendor/bundle"

      # Emacs auctex
      ".auctex-auto/"

      # AI coding tools
      ".claude/"
      ".opencode/"
    ];
    settings = {
      user = {
        name = "Paul Grillenberger";
        email = "grillenbergerpaul@gmail.com";
        signingkey = "~/.ssh/id_ed25519.pub";
      };
      "includeIf \"gitdir:~/Work/\"" = {
        path = "~/Work/.gitconfig";
      };
      credential = {
        helper = "osxkeychain";
      };
      push = {
        default = "current";
        autoSetupRemote = true;
        followTags = true;
      };
      core = {
        pager = "delta";
      };
      interactive = {
        diffFilter = "delta --color-only";
      };
      delta = {
        navigate = true;
        side-by-side = true;
        line-numbers = true;
      };
      merge = {
        tool = "nvim";
      };
      mergetool = {
        prompt = false;
        keepBackup = false;
      };
      "mergetool \"nvim\"" = {
        cmd = "nvim -d -c \"wincmd l\" -c \"norm ]c\" \"$LOCAL\" \"$MERGED\" \"$REMOTE\"";
      };
      "filter \"lfs\"" = {
        required = true;
        clean = "git-lfs clean -- %f";
        smudge = "git-lfs smudge -- %f";
        process = "git-lfs filter-process";
      };
      rebase = {
        autostash = true;
      };
      pull = {
        rebase = true;
      };
      fetch = {
        prune = true;
      };
      gpg = {
        format = "ssh";
      };
      "gpg \"ssh\"" = {
        allowedSignersFile = "~/.ssh/allowed_signers";
      };
      commit = {
        gpgsign = true;
      };
      init = {
        defaultBranch = "main";
      };
      alias = {
        # View abbreviated SHA, description, and history graph of the latest 20 commits
        l = "log --pretty=oneline -n 20 --graph --abbrev-commit";
        # View the current working tree status using the short format
        s = "status -s";
        # Pull in remote changes for the current repository and all its submodules
        p = "!\"git pull; git submodule foreach git pull origin master\"";
        # Push changes for the current repository and all altered submodules
        pu = "push --recurse-submodules=on-demand";
        # Clone a repository including all submodules
        c = "clone --recursive";
        # Commit all changes
        ca = "!git add -A && git commit -av";
        # Switch to a branch
        co = "checkout";
        # Switch to a branch, creating it if necessary
        go = "checkout -B";
        # Show verbose output about tags, branches or remotes
        tags = "tag -l";
        branches = "branch -a";
        remotes = "remote -v";
        # Credit an author on the latest commit
        credit = "!f() { git commit --amend --author \"$1 <$2>\" -C HEAD; }; f";
        # Interactive rebase with the given number of latest commits
        reb = "!r() { git rebase -i HEAD~$1; }; r";
        d = "difftool --no-symlinks --dir-diff";
      };
    };
  };
}
