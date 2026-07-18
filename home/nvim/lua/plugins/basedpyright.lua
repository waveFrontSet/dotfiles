return {
  "neovim/nvim-lspconfig",
  opts = {
    servers = {
      basedpyright = {
        settings = {
          basedpyright = {
            analysis = {
              typeCheckingMode = "standard",
              diagnosticSeverityOverrides = {
                reportUnknownMemberType = "none",
                reportUnknownArgumentType = "none",
                reportUnknownVariableType = "none",
                reportUnknownParameterType = "none",
                reportUnknownLambdaType = "none",
                reportMissingTypeStubs = "none",
                reportAny = "none",
                reportExplicitAny = "none",
              },
            },
          },
        },
      },
    },
  },
}
