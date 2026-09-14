---
mode: agent
description: Generates a pull request description for FSharp.Data.GraphQL by analyzing the current branch's changes and filling in the project's PR template.
---

# Create PR Description

## Context

#file:'.github/PULL_REQUEST_TEMPLATE.md'

> If the `#file:` reference above cannot be resolved, run `Get-Content .github/PULL_REQUEST_TEMPLATE.md` in PowerShell from the repository root to read the template.

## Task

Analyze the current branch's git diff and commit history relative to the remote base branch (`origin/dev`), then produce a fully filled-in PR description that conforms to the project's `PULL_REQUEST_TEMPLATE.md`.

## Steps

1. Read `#file:'.github/PULL_REQUEST_TEMPLATE.md'` to identify the exact section structure, checkbox labels, and required ordering.
2. Run `git diff origin/dev...HEAD --stat`, `git diff origin/dev...HEAD`, and `git log origin/dev...HEAD --oneline` to determine exactly what changed in the current branch.
3. Fill `## Proposed Changes` with a concise 3–6 sentence summary of the concrete behavior, API, test, or documentation changes visible in the diff.
4. Set `## Types of changes` checkboxes strictly from evidence in commits and diff:
   - mark bugfix only when existing behavior is corrected
   - mark new feature only when new user-facing capability is introduced
   - mark breaking change only when existing public behavior or API compatibility is intentionally broken
5. Evaluate `## Checklist` strictly from branch evidence:
   - mark test-related items only if tests were added or updated in the diff
   - mark documentation-related items only if docs or XML comments were changed
   - if build/test execution is not visible from evidence, leave relevant items unchecked and add a brief inline note
6. Replace all placeholder/template text with branch-specific content and include `## Further comments` only when the change is large or architecturally significant.

## Guidelines

- Be factual and precise – base every statement on the actual diff and commits, not assumptions.
- Keep the **Proposed Changes** section concise but informative (3–6 sentences max).
- For checklist items that cannot be determined from the diff alone, leave them unchecked and add a short inline note.
- Do not invent issue numbers or links unless they appear in commit messages or branch names.
- Use plain English; avoid vague filler phrases like "various improvements".
- Do not start the summary with "This branch adds".
- Use only en dashes (`–`) for dashes; never use em dashes (`—`).
- Always wrap names and versions into backticks (for example, `ObjectListFilter`) when referring to them in the description.
- Preserve all original template headings, checkbox syntax, and section order exactly.

## Output

A single fenced markdown code block containing the fully filled-out PR description, ready to copy and paste directly into GitHub. Do not escape any markdown syntax inside the block. Match the structure of `PULL_REQUEST_TEMPLATE.md` exactly:

- `## Proposed Changes` – narrative paragraph(s)
- `## Types of changes` – checkboxes with `x` placed in the correct box(es)
- `## Checklist` – checkboxes filled based on evidence from the diff
- `## Further comments` – include only when the change warrants extra explanation; omit the section entirely otherwise

Do not include any explanation or commentary outside the code block.
