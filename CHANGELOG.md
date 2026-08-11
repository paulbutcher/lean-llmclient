# Changelog

## [0.4.1] - 2026-08-11

**Behavioral change:** `converseLoop` now returns `.ok (history, text)` when it exhausts
`config.maxIterations`, instead of `.error`. The tool calls and results made before giving up are
real conversation history and are pushed into `history`, with a final `Msg.assistant` entry
recording the giving-up message, so callers can persist or display them. `.error` is now reserved
solely for `provider.sendRequest` failing outright. Callers that currently treat any `.error` from
`converseLoop` as "nothing happened, don't persist" must be updated to handle this case via `.ok`.

## [0.4.0] - 2026-08-11

Add converseLoop for multi-turn tool-calling exchanges, with an optional progress hook.

## [0.3.0] - 2026-08-08

Add optional system prompt support to Config.

## [0.2.0] - 2026-08-06

Report truncated replies from Reply

## [0.1.1] - 2026-08-03

Add BEq for convenience.

## [0.1.0] - 2026-08-02

Initial release.
