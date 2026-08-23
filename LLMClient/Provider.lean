-- Copyright (c) 2026 Paul Butcher. All rights reserved.
-- Released under Apache 2.0 license as described in the file LICENSE.

module

public import Json

public section

namespace LLMClient

/-- A tool exposed to the model. Each provider wraps `schema` in its own request shape
(Anthropic's `input_schema`, OpenAI's `parameters`, ...). -/
structure Tool where
  name : String
  description : String
  schema : Json

/-- A tool paired with the code that runs it, so that anything advertised to a model has an
implementation behind it by construction. `run` returns `.error` for a call that failed, which
reaches the model as a tool result flagged as an error rather than as prose it has to read
failure out of. -/
structure ToolImpl where
  tool : LLMClient.Tool
  run : Json → IO (Except String String)

/-- A tool call the model asked for. `input` is `{}` for tools that take no arguments. -/
structure ToolCall where
  id : String
  name : String
  input : Json := Json.mkObj []
deriving Inhabited, BEq

/-- One turn of the conversation, independent of any provider's wire format. Callers persist
this and replay it on the next request. -/
inductive Msg where
  | user (text : String)
  | assistant (text : String) (toolCalls : Array LLMClient.ToolCall := #[])
  | toolResult (id : String) (output : String) (isError : Bool := false)
deriving Inhabited, BEq

/-- A provider's response, translated out of its own wire format. Named `Reply` (not
`Response`) since callers such as HTTP servers typically have their own `Response` type in
scope. -/
structure Reply where
  text : String := ""
  toolCalls : Array LLMClient.ToolCall := #[]
  isRefusal : Bool := false
  isTruncated : Bool := false
deriving Inhabited

/-- Tunable parameters for a request/response round trip. `apiUrl` and `model` have no
sensible cross-provider default, so each provider supplies its own `defaultConfig`. -/
structure Config where
  apiUrl : String
  model : String
  maxOutputTokens : Nat := 1024
  /-- Fixed instruction sent with every request, independent of conversation history. Omitted
  from the request entirely when `none`, rather than sent as an empty or null value. -/
  systemPrompt : Option String := none

/-- A backend capable of driving one request/response round trip against an LLM API.

Credentials are supplied when a provider is constructed rather than on each call, because what
they are differs by backend: a bearer token for some, a set of AWS credentials to sign with for
others. Constructing a provider is just closing over them, so a caller holding credentials that
expire rebuilds it as often as it needs to. -/
structure Provider where
  name : String
  sendRequest :
    (history : Array LLMClient.Msg) → (tools : Array LLMClient.Tool := #[]) →
      IO (Except String LLMClient.Reply)

end LLMClient
