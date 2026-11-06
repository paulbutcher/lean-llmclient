# Changelog

## [0.12.1] - 2026-11-06

- Chore: bump dependencies.

## [0.12.0] - 2026-11-02

- `Pending.answerWith` runs a turn's tool calls in order, threading a state from each call to the next.

## [0.11.0] - 2026-11-02

- Breaking: `Msg.assistant` holds the turn's `Block`s in order, and `Reply` holds them as `content`, with `text` and `toolCalls` read from it.
- Claude's thinking blocks are kept and replayed unchanged, so the model keeps its reasoning across the tool calls of a turn.
- `Turn`, `Pending` and `Step` expose `converseLoop`'s rules as pure steps, for a caller that drives a turn one round trip at a time.
- `appendResults` and `unknownTool?` answer tool calls as `converseLoop` does.
- `closeOff` closes a history that ends on tool results with an assistant turn.
- `Msg` has a JSON encoding for storage, documented in the README.
- Breaking: `Claude.defaultConfig` uses `claude-opus-5-5`.
- Breaking: `extractText` and `extractToolCalls` are replaced by `extractContent` in the Claude and Bedrock modules.

## [0.10.0] - 2026-11-02

- `Config.extraHeaders` adds headers to every request, such as the one Anthropic requires from a browser.
- `LLMClient.Core` is the library without curl, and `LLMClient.Fetch` adds a `Transport Async` over the browser's `fetch` for Lasm builds.
- Each provider has a `providerWith` that takes the `Transport` to post with; `provider` posts with curl as before.
- Breaking: `Provider`, `ToolImpl`, `LoopConfig` and `Transport` take the monad they run in, and `converseLoop` works in any `Monad`, so the library can run in `Async`.
- Breaking: each provider's `sendRequest` takes the transport first, and Bedrock's then the clock it signs with.
- Each provider exposes a pure `parseReply`, proved to succeed only on a 2xx status.
- An error status is reported with the raw body when the body is not JSON, rather than as a parse failure.

## [0.9.4] - 2026-10-20

- Chore: bump dependencies.

## [0.9.3] - 2026-09-08

- Require lean-aws by the name it declares, so it resolves once for consumers that also depend on it.

## [0.9.2] - 2026-09-08

- Update dependencies.

## [0.9.1] - 2026-08-30

- Pin dependencies.

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
