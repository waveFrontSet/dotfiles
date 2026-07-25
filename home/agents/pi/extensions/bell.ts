import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { writeFileSync } from "node:fs";

// Ring the terminal bell so kitty shows its 🔔 attention marker when pi is done
// and waiting for the user. `agent_settled` fires once pi will not continue on
// its own — i.e. it has finished the task / emitted a plan, or otherwise handed
// control back and is waiting for your reply.
//
// Note: pi has no native "permission prompt" event to hook (by default it runs
// tools without a blocking approval step; gating is done inside `tool_call`
// handlers), so only the done/your-turn signal is wired up here.
function ringBell(): void {
  try {
    // BEL straight to the controlling terminal, bypassing pi's TUI stdout.
    writeFileSync("/dev/tty", "\x07");
  } catch {
    // No controlling terminal (e.g. print or RPC mode) — nothing to ring.
  }
}

export default function (pi: ExtensionAPI): void {
  pi.on("agent_settled", () => ringBell());
}
