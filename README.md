# LLMClient

A provider-agnostic Lean 4 client for LLM chat APIs. `LLMClient` defines a single
request/response vocabulary (`Msg`, `Tool`, `ToolCall`, `Reply`, `Config`) and a `Provider`
interface that drives one round trip against a backend; `LLMClient.OpenAI` and `LLMClient.Claude`
implement that interface against the OpenAI Chat Completions and Anthropic Messages APIs
respectively. Callers write their conversation loop once against `Provider`/`Msg`/`Reply` and swap
backends by swapping which `Provider` they construct.

## Adding it to your project

Add to your `lakefile.toml`:

```toml
[[require]]
name = "LLMClient"
git = "https://github.com/paulbutcher/lean-llmclient"
rev = "main"
```

`LLMClient` depends on [`leancurl`](https://github.com/paulbutcher/leancurl) for HTTP, which
compiles a small C shim against `libcurl`. Building anything that depends on `LLMClient` therefore
needs `pkg-config`, `libcurl` development headers, and a C compiler available (e.g. on Debian/
Ubuntu, `pkg-config` and `libcurl4-openssl-dev`).

## Usage

```lean
import LLMClient

open LLMClient

def main : IO Unit := do
  let apiKey ← IO.getEnv "ANTHROPIC_API_KEY" -- or however you source it
  let provider := Claude.provider
  let history := #[Msg.user "What's the capital of France?"]
  match ← provider.sendRequest apiKey.get! history with
  | .ok reply => IO.println reply.text
  | .error msg => IO.eprintln s!"request failed: {msg}"
```

Swap `Claude.provider` for `OpenAI.provider` to talk to OpenAI instead; both accept an optional
`Config` (`apiUrl`, `model`, `maxOutputTokens`) if you want to override the defaults.

`tools` defaults to `#[]`, so it can be left off when you're not offering any (as above).

## Tools

Tool calls round-trip through `Tool` (what you expose to the model) and `ToolCall`/`Msg.toolResult`
(what the model asks for and what you feed back), independent of either provider's wire format:

```lean
open Lean (Json)

def getWeather : Tool :=
  { name := "get_weather"
    description := "Get the current weather for a city"
    schema := Json.mkObj
      [ ("type", "object"),
        ("properties", Json.mkObj [("city", Json.mkObj [("type", "string")])]),
        ("required", Json.arr #["city"]) ] }

def history := #[Msg.user "What's the weather in Paris?"]

match ← provider.sendRequest apiKey.get! history #[getWeather] with
| .error msg => IO.eprintln s!"request failed: {msg}"
| .ok reply =>
  for call in reply.toolCalls do
    if call.name == "get_weather" then
      let city := (call.input.getObjVal? "city" >>= Json.getStr?).toOption.getD ""
      IO.println s!"model wants the weather for {city}"
      -- feed the result back on the next request:
      -- history.push (.assistant reply.text reply.toolCalls) |>.push (.toolResult call.id "15°C, cloudy")
```

## Development

- `lake build` builds the library.
- `lake test` runs the unit tests (pure JSON encode/decode checks against each provider's wire
  format; it does not make network calls, so no API key is needed to run it).
