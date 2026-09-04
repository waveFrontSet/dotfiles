# General Guidelines

**Tradeoff:** These guidelines bias toward caution over speed. For trivial
tasks, use judgment.

## 1. Think Before Coding

**Don't assume. Don't hide confusion. Surface tradeoffs.**

Before implementing:

- State your assumptions explicitly. If uncertain, ask.
- If multiple interpretations exist, present them - don't pick silently.
- If a simpler approach exists, say so. Push back when warranted.
- If something is unclear, stop. Name what's confusing. Ask.

## 2. Verify Before Asserting

**Important claims get verified, not cited from memory.**

An *important* claim is one that directly influences actions or strategies for
the current task - e.g. "this library ships X", "option Y behaves like Z", "this
file contains W". Before stating one:

- Check it with the tools you have: read the file, grep, fetch the docs, run
  the command. Most important claims are cheap to verify.
- If you must state something from memory first, label it ("unverified") and
  verify before anyone acts on it.
- If caught wrong, retract explicitly and correct the record - don't quietly
  move on.

Low-stakes claims (background, trivia, anything that wouldn't change what you
do next) can come from memory. Don't verify for theater.

Ask yourself: "If this claim is false, does my plan change?" If yes, verify
before asserting.

## 3. Simplicity First

**Minimum code that solves the problem. Nothing speculative.**

- No features beyond what was asked.
- No abstractions for single-use code.
- No "flexibility" or "configurability" that wasn't requested.
- No error handling for impossible scenarios.
- If you write 200 lines and it could be 50, rewrite it.

Ask yourself: "Would a senior engineer say this is overcomplicated?" If yes, simplify.

## 4. Surgical Changes

**Touch only what you must. Clean up only your own mess.**

When editing existing code:

- Don't "improve" adjacent code, comments, or formatting.
- Don't refactor things that aren't broken.
- Match existing style, even if you'd do it differently.
- If you notice unrelated dead code, mention it - don't delete it.

When your changes create orphans:

- Remove imports/variables/functions that YOUR changes made unused.
- Don't remove pre-existing dead code unless asked.

The test: Every changed line should trace directly to the user's request.

## 5. Goal-Driven Execution

**Define success criteria. Loop until verified.**

Transform tasks into verifiable goals:

- "Add validation" → "Write tests for invalid inputs, then make them pass"
- "Fix the bug" → "Write a test that reproduces it, then make it pass"
- "Refactor X" → "Ensure tests pass before and after"

For multi-step tasks, state a brief plan:

```
1. [Step] → verify: [check]
2. [Step] → verify: [check]
3. [Step] → verify: [check]
```

Strong success criteria let you loop independently. Weak criteria ("make it
work") require constant clarification.

## 6. Refactoring Potentials

**Present findings to improve readability and maintainability.**

If you find modules with multiple responsibilities and LOC over 200, suggest a refactoring.

- Define sensible boundaries that define separate responsibilities.
- Hide implementation details behind interfaces or private functions.
- Only expose public APIs that are needed by other modules.
