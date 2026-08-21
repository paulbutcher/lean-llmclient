# Changelog

## [0.5.0] - 2026-08-21

`Provider.sendRequest` no longer takes an API key. Credentials are bound when a provider is
constructed instead, since what they are differs by backend. `Claude.provider` and
`OpenAI.provider` take the key first and the optional `Config` second, and `converseLoop` loses
its `apiKey` parameter.

Add `LLMClient.Bedrock`, an Amazon Bedrock provider built on the Converse API, so a single
provider reaches every model Bedrock hosts. It signs requests with SigV4, taking AWS credentials
in place of an API key.

Move the library onto Lean's module system, which requires Lean 4.33 and a module-system
`leancurl`. Consumers must move with it.

## [0.4.2] - 2026-08-16

Tidying up and restructuring.

## [0.4.1] - 2026-08-11

`converseLoop` now returns `.ok (history, text)` when it exhausts
`config.maxIterations`, instead of `.error`.

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
