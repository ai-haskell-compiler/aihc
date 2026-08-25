---
name: grok-review
description: >
  Start the grok CLI as an independent reviewer, apply every finding, and
  repeat until Grok reports no issues. Use when Codex is asked for
  /grok-review, $grok-review, or an automatic Grok review loop. Do not use
  this skill when you are Grok.
---

# Grok Review Loop

If you are Grok, stop.
Tell the user to start `$grok-review` in Codex.

You are the Codex orchestrator.
Grok writes each review.
You apply every finding.
Then you start Grok again.

## Preconditions

1. Confirm the current directory is a git work tree.
2. Confirm `grok` is on `PATH`.
3. If `grok` is missing, tell the user to install Grok Build and stop.

## Loop

Set `MAX_ROUNDS` to 8.
Set `round` to 1.

Repeat this sequence until a stop rule matches:

1. Start Grok on the current work tree.
2. Parse the review block.
3. If the Issues section is empty, stop with success.
4. Apply every issue in the Issues section.
5. If a suggestion breaks `AGENTS.md`, skip that issue and record the skip.
6. If Haskell files changed, run `just fmt`.
7. If `round` equals `MAX_ROUNDS` and issues remain, stop with failure.
8. If this round repeats the same File and Description pairs as the last round, stop with failure.
9. Add 1 to `round`.

## Start Grok

Write a briefing file.
Keep the briefing under 40 lines.

The briefing must contain:

- The user goal in 1 to 3 sentences
- The current round number
- Skipped issues from earlier rounds, or `none`
- Extra user notes from the invocation, or `none`

The briefing must not contain:

- Codex verdicts
- Codex issue lists
- The full chat transcript
- Source dumps or diffs

Grok collects the diff.

Start Grok with this command:

```bash
grok \
  --cwd "<ABS_CWD>" \
  --no-auto-update \
  --always-approve \
  --permission-mode bypassPermissions \
  --reasoning-effort high \
  --output-format plain \
  --prompt-file "<BRIEFING_FILE>"
```

Use a command timeout of at least 600000 milliseconds.

Do not pass `--no-subagents`.
Do not restrict tools with `--tools`.

Write this text into the briefing file:

```text
Use /review --local

You are an independent reviewer.
Another agent implemented these changes.
Do not trust that agent's reasoning.

Read surrounding source.
Do not review the diff alone.
Do not edit project source.
Do not commit.

Correctness and missing tests first.
Style last.
Cite file:line.
Do not invent issues.

In the final user message, print the full review between these markers:

---REVIEW-START---
<full review markdown>
---REVIEW-END---

The review markdown must use this shape:

## Summary

<2 to 4 sentences>

## Issues

### Issue N -- Severity: bug|suggestion|nit
- File: path:LINE
- Description: <what is wrong>
- Suggestion: <how to fix>
- Status: open

If there are no issues, keep ## Issues empty.

## Intent
<brief intent>

## Round
<round>

## Skipped earlier
<skips or none>

## Extra notes
<user extra args or none>
```

## Parse findings

Read stdout.
Extract the text between `---REVIEW-START---` and `---REVIEW-END---`.

If the markers are missing, search stdout for a path that contains `grok-review-` and ends with `.md`.
Read that file.

If you cannot get a review, report the Grok output and stop.

Count issues with this heading:

`### Issue <n> -- Severity: (bug|suggestion|nit)`

An empty Issues section means Grok is satisfied.

## Apply findings

Apply every issue.
Use the Suggestion field as the change to make.

If a suggestion needs tests, load `$aihc-test-authoring`.
Do not add a hand-written unit test when a fixture can trigger the same failure.

Do not commit.
Do not push.
Do not open a GitHub pull request from this loop.

## Stop report

Always report:

- Round count
- Issue counts per round
- Skipped issues
- Success or failure

On success, tell the user that Grok reported no issues.
On success, tell the user to run `just check` before a commit.
On failure, print the remaining issues.
Do not start extra fixes after stop.
