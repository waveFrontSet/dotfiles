import { type Plugin } from "@opencode-ai/plugin"
import { execSync } from "child_process"
import { extname } from "path"

const LINTERS: Record<string, string[][]> = {
  ".py": [
    ["ruff", "check", "--no-fix"],
    ["mypy"],
  ],
  ".nix": [
    ["nixfmt", "--check"],
    ["statix", "check"],
  ],
  ".go": [
    ["go", "vet"],
    ["staticcheck"],
  ],
  ".hs": [
    ["hlint"],
    ["ghc", "-fno-code"],
  ],
  ".rs": [
    ["cargo", "clippy", "--quiet"],
  ],
}

function runLinters(filePath: string): string[] {
  const ext = extname(filePath)
  const commands = LINTERS[ext]
  if (!commands) return []

  const errors: string[] = []
  for (const cmd of commands) {
    // For Go, lint the package directory; for Rust, let cargo find the manifest
    const args =
      ext === ".go"
        ? [...cmd, `${filePath.substring(0, filePath.lastIndexOf("/"))}/...`]
        : [...cmd, filePath]
    try {
      execSync(args.join(" "), { encoding: "utf-8", stdio: ["pipe", "pipe", "pipe"] })
    } catch (e: any) {
      const output = (e.stdout || "") + (e.stderr || "")
      if (output.trim()) errors.push(output.trim())
    }
  }
  return errors
}

export const PostEditLint: Plugin = async () => ({
  "tool.execute.after": async (input, output) => {
    if (input.tool !== "apply_patch" && input.tool !== "write") return output

    const filePath = (input as any).args?.file_path || (input as any).args?.path
    if (!filePath) return output

    const errors = runLinters(filePath)
    if (errors.length > 0) {
      const feedback = errors.join("\n---\n")
      return {
        ...output,
        output: `${output.output}\n\n⚠ Lint issues:\n${feedback}`,
      }
    }
    return output
  },
})
