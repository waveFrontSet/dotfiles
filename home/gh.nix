{ pkgs, ... }: {
  programs.gh = {
    enable = true;
    settings.git_protocol = "ssh";
    extensions = [ pkgs.gh-stack ];
  };
  programs.gh-dash = {
    enable = true;
    settings = {
      keybindings = {
        prs = [
          {
            key = "M";
            name = "Approve & Merge";
            command = "gh pr review --repo {{.RepoName}} --approve {{.PrNumber}} && gh pr merge --repo {{.RepoName}} -d -m {{.PrNumber}}";
          }
        ];
      };
    };
  };
}
