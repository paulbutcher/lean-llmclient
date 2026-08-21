# LLMClient

A provider-agnostic Lean 4 client for LLM chat APIs. `LLMClient` defines a single
request/response vocabulary (`Msg`, `Tool`, `ToolCall`, `Reply`, `Config`) and a `Provider`
interface that drives one round trip against a backend; `LLMClient.OpenAI`, `LLMClient.Claude` and
`LLMClient.Bedrock` implement that interface against the OpenAI Chat Completions, Anthropic
Messages and Amazon Bedrock Converse APIs respectively. Callers write their conversation loop
once against `Provider`/`Msg`/`Reply` and swap backends by swapping which `Provider` they
construct.

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
Ubuntu, `pkg-config` and `libcurl4-openssl-dev`). It also depends on
[`lean-json`](https://github.com/paulbutcher/lean-json) for the `Json` type that `Tool.schema`,
`ToolCall.input` and `runTool` are written in, and on
[`lean-aws`](https://github.com/paulbutcher/lean-aws) and, through it,
[`leancrypto`](https://github.com/paulbutcher/leancrypto) for request signing; those three are
pure Lean and add no build requirements of their own.

## Usage

```lean
import LLMClient

open LLMClient

def main : IO Unit := do
  let some apiKey ← IO.getEnv "ANTHROPIC_API_KEY" -- or however you source it
    | IO.eprintln "ANTHROPIC_API_KEY is not set"
  let provider := Claude.provider apiKey
  let history := #[Msg.user "What's the capital of France?"]
  match ← provider.sendRequest history with
  | .ok reply => IO.println reply.text
  | .error msg => IO.eprintln s!"request failed: {msg}"
```

Swap `Claude.provider` for `OpenAI.provider` to talk to OpenAI instead; both take the API key
first and then an optional `Config` (`apiUrl`, `model`, `maxOutputTokens`, `systemPrompt`) if you
want to override the defaults. `systemPrompt` is sent as a top-level `system` field by Claude and
as a leading `developer` message by OpenAI; leave it `none` to omit it from the request entirely.
`Bedrock.provider creds (Bedrock.defaultConfig region)` reaches everything Amazon Bedrock hosts
through its Converse API, chosen with `Config.model`; it signs with SigV4, so it takes AWS
credentials in place of an API key, and its `Config` carries `region` rather than `apiUrl`.
`Bedrock.credentialsFromEnv` and `Bedrock.regionFromEnv` read the usual `AWS_*` variables.

`tools` defaults to `#[]`, so it can be left off when you're not offering any (as above).

## Tools

Tool calls round-trip through `Tool` (what you expose to the model) and `ToolCall`/`Msg.toolResult`
(what the model asks for and what you feed back), independent of any provider's wire format. The
`Json` they carry is `lean-json`'s, re-exported by `import LLMClient`, so there's nothing extra to
import:

```lean
def getWeather : Tool :=
  { name := "get_weather"
    description := "Get the current weather for a city"
    schema := Json.mkObj
      [ ("type", "object"),
        ("properties", Json.mkObj [("city", Json.mkObj [("type", "string")])]),
        ("required", Json.arr #["city"]) ] }

def history := #[Msg.user "What's the weather in Paris?"]

match ← provider.sendRequest history #[getWeather] with
| .error msg => IO.eprintln s!"request failed: {msg}"
| .ok reply =>
  for call in reply.toolCalls do
    if call.name == "get_weather" then
      let city := (call.input.getObjVal? "city" >>= Json.getStr?).toOption.getD ""
      IO.println s!"model wants the weather for {city}"
      -- feed the result back on the next request:
      -- history.push (.assistant reply.text reply.toolCalls) |>.push (.toolResult call.id "15°C, cloudy")
```

## Multi-turn tool loops

The `Tools` example above shows a single round trip. Most tool-using conversations need several:
send the result of each tool call back to the model and keep going until it stops asking for
tools. `converseLoop` runs that exchange for you, in place of writing the loop yourself:

```lean
def runTool (name : String) (input : Json) : IO String :=
  if name == "get_weather" then
    let city := (input.getObjVal? "city" >>= Json.getStr?).toOption.getD ""
    pure s!"15°C, cloudy in {city}" -- however you actually look this up
  else
    pure s!"unknown tool: {name}"

let config : LoopConfig :=
  { onProgress := fun
      | .thinking => IO.println "model is thinking..."
      | .runningTool name => IO.println s!"running tool: {name}" }

match ← converseLoop provider #[getWeather] runTool history config with
| .error msg => IO.eprintln s!"request failed: {msg}"
| .ok (_, text) => IO.println text
```

`config` is optional; drop it (`converseLoop provider #[getWeather] runTool history`)
to get the defaults: `maxIterations := 5` and no progress reporting. `maxIterations` bounds how
many tool round trips are allowed before `converseLoop` gives up, since a model can in principle
keep asking for tools forever.

## Development

- `lake build` builds the library.
- `lake test` runs the tests. They live in the `test/` subproject, which has its own lakefile and
  requires this package by path, so test-only dependencies never reach a downstream consumer.
- The tests are pure JSON encode/decode checks against each provider's wire format, plus
  `test/Theorems.lean`, which proves the round-trip and message-shape invariants those formats
  depend on. Nothing makes a network call, so no API key is needed to run them.
