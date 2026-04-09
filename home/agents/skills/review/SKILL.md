---
name: review
description: Review code changes for quality, bugs, best practices, and architecture
user-invocable: true
---

Review the following code changes. Focus on:

1. **Correctness** - Logic errors, edge cases, off-by-one errors
2. **Security** - Injection, secrets exposure, auth issues
3. **Performance** - Unnecessary allocations, N+1 queries, missing indexes
4. **Readability** - Naming, complexity, missing comments for non-obvious logic
5. **Best practices** - Idiomatic patterns for the language, DRY, SOLID
6. **Architecture** - Clear separation of concerns, modularity, maintainability, open-closed principle

If reviewing a diff, use `git diff` to get the changes. If reviewing a file, read the specified file.

Be concise. Flag only genuine issues, not style nitpicks.

$ARGUMENTS
