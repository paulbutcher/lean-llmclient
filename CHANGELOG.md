# Changelog

## [0.9.0] - 2026-08-30

- `Msg.toolResult` and `ToolImpl` gain an optional structured (JSON) tool result, which Bedrock sends as a Converse `json` block instead of escaped text.

## [0.8.1] - 2026-08-30

- Expand the theorem documentation in `test/Theorems.lean`.

## [0.8.0] - 2026-08-23

Breaking changes to `converseLoop` and `Msg.toolResult` to improve structure and error reporting.

## [0.7.0] - 2026-08-22

Fix error handling for the Converse API.

## [0.6.0] - 2026-08-21

Switch to lean-json

## [0.5.0] - 2026-08-21

Add Bedrock support

## [0.4.2] - 2026-08-16

Tidying up and restructuring.

## [0.4.1] - 2026-08-11

`converseLoop` now returns `.ok (history, text)` when it exhausts `config.maxIterations`, instead of `.error`.

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
