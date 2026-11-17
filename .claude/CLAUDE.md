# Personal instructions

- Pick a subagent's model by its task. Routine mechanical work, say bulk renames, repo-wide searches, fixture edits, lint and test runs, goes to Sonnet or Opus via the Agent tool's `model` override. Work that needs analysis or judgement, say reviews and exploration, stays on the session's model.
