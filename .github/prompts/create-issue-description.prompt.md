---
mode: agent
description: Generates a GitHub issue description for FSharp.Data.GraphQL by analyzing the current branch's changes and filling in the project's issue template.
---

Generate a GitHub issue description for the current branch by filling out the repository's issue template.

## Steps

1. Read #file:'.github/ISSUE_TEMPLATE.md' to understand the required sections and their purpose.
2. Run `git diff origin/dev...HEAD --stat` and `git log origin/dev...HEAD --oneline` to understand what was changed in the current branch.
3. For each section in the template, infer the appropriate content from the branch changes, commit messages, and modified file names.
4. Do **not** leave any template placeholder text (e.g., `Step A`, `Step B`) in the output – replace every section with concrete, specific content derived from the branch.
5. Write in clear, concise English suitable for a public GitHub issue.

## Output

A single fenced markdown code block containing the fully filled-out issue body, ready to copy and paste directly into GitHub. Do not include any explanation or commentary outside the code block.

## Notes

- If a template section is not applicable given the changes, write "N/A" rather than omitting the section.
- Base all content strictly on actual branch changes – do not speculate or invent scenarios.
- Commit messages, changed file paths, and any added or modified test cases are strong signals for identifying the change's purpose and scope.
