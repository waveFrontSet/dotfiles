{ ... }:
{
  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    config = {
      load_dotenv = true;
      whitelist.prefix = [
        "~/Work/"
        "~/private/"
      ];
    };
    stdlib = ''
      layout_uv() {
          if [[ -d ".venv" ]]; then
              VIRTUAL_ENV="$(pwd)/.venv"
          fi

          if [[ -z $VIRTUAL_ENV || ! -d $VIRTUAL_ENV ]]; then
              log_status "No virtual environment exists. Executing \`uv venv\` to create one."
              uv venv
              VIRTUAL_ENV="$(pwd)/.venv"
          fi

          PATH_add "$VIRTUAL_ENV/bin"
          export UV_ACTIVE=1  # or VENV_ACTIVE=1
          export VIRTUAL_ENV
      }
    '';
  };
}
