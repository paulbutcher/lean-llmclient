-- Copyright (c) 2026 Paul Butcher. All rights reserved.
-- Released under Apache 2.0 license as described in the file LICENSE.

module

public import Json
public import LLMClient.Provider

public section

namespace LLMClient.Claude

def defaultConfig : LLMClient.Config :=
  { apiUrl := "https://api.anthropic.com/v1/messages", model := "claude-opus-5-5" }

def toolJson (t : LLMClient.Tool) : Json :=
  Json.mkObj [("name", t.name), ("description", t.description), ("input_schema", t.schema)]

/-- One `tool_result` block, as it appears in the content of the user message answering a request
for tools. `is_error` is omitted rather than sent as `false`, in the same way as the system
prompt. -/
def toolResultJson (id output : String) (isError : Bool := false) : Json :=
  let fields : List (String × Json) :=
    [("type", "tool_result"), ("tool_use_id", id), ("content", output)]
  Json.mkObj (if isError then fields ++ [("is_error", Json.bool true)] else fields)

/-- An empty text block is omitted, since the API rejects one. Anything else is written exactly as it
arrived, so that a replayed turn is the turn the model produced. -/
def blockJson : LLMClient.Block → Option Json
  | .text text => if text.isEmpty then none else some (Json.mkObj [("type", "text"), ("text", text)])
  | .toolCall tc =>
    some (Json.mkObj [("type", "tool_use"), ("id", tc.id), ("name", tc.name), ("input", tc.input)])
  | .thinking thinking signature =>
    some (Json.mkObj [("type", "thinking"), ("thinking", thinking), ("signature", signature)])
  | .redactedThinking data => some (Json.mkObj [("type", "redacted_thinking"), ("data", data)])

/-- A lone `.toolResult` becomes a message of its own here, which is right only when it is the one
result outstanding. `messagesJson` is what a request is built from, and it groups.

`Msg.toolResult`'s `structured` is dropped rather than overlooked: a `tool_result` block's content
is a string or text and image blocks, with nowhere to put JSON, so `output` is what the model
sees. Only Converse has a place for it. -/
def msgJson : LLMClient.Msg → Json
  | .user text => Json.mkObj [("role", "user"), ("content", text)]
  | .assistant content =>
    Json.mkObj [("role", "assistant"), ("content", Json.arr (content.filterMap blockJson))]
  | .toolResult id output isError _ =>
    Json.mkObj [("role", "user"), ("content", Json.arr #[toolResultJson id output isError])]

/-- The conversation as the Messages API wants it, which is not one message per `Msg`.

An assistant turn that asks for several tools is answered by a *single* user message carrying one
`tool_result` block per call, and roles have to alternate. A message per result breaks both, and
the API rejects it rather than reading the results in sequence.

So a run of consecutive `.toolResult`s becomes one message. Everything else maps as it reads. -/
def messagesJson (history : Array LLMClient.Msg) : Array Json :=
  let close (pending acc : Array Json) : Array Json :=
    if pending.isEmpty then acc
    else acc.push (Json.mkObj [("role", "user"), ("content", Json.arr pending)])
  let (acc, pending) := history.foldl (init := ((#[] : Array Json), (#[] : Array Json)))
    fun (acc, pending) msg =>
      match msg with
      | .toolResult id output isError _ =>
        (acc, pending.push (toolResultJson id output isError))
      | other => ((close pending acc).push (msgJson other), #[])
  close pending acc

def blockType (b : Json) : String :=
  match b.getObjVal? "type" >>= Json.getStr? with
  | .ok s => s
  | .error _ => ""

def contentOf (resp : Json) : Array Json :=
  match resp.getObjVal? "content" >>= Json.getArr? with
  | .ok arr => arr
  | .error _ => #[]

def stopReasonOf (resp : Json) : String :=
  match resp.getObjVal? "stop_reason" >>= Json.getStr? with
  | .ok s => s
  | .error _ => ""

def errorMessageOf (j : Json) : String :=
  match j.getObjVal? "error" >>= (·.getObjVal? "message") >>= Json.getStr? with
  | .ok msg => msg
  | .error _ => j.compress

def strField (b : Json) (k : String) : Option String := (b.getObjVal? k >>= Json.getStr?).toOption

/-- The inverse of `blockJson`. A block missing a field it cannot do without is dropped, and so is
a block of a type this library does not model. -/
def readBlock (b : Json) : Option LLMClient.Block :=
  match blockType b with
  | "text" => (strField b "text").bind fun t => if t.isEmpty then none else some (.text t)
  | "tool_use" =>
    match strField b "id", strField b "name" with
    | some id, some name =>
      some (.toolCall { id, name, input := (b.getObjVal? "input").toOption.getD (Json.mkObj []) })
    | _, _ => none
  | "thinking" =>
    match strField b "thinking", strField b "signature" with
    | some thinking, some signature => some (.thinking thinking signature)
    | _, _ => none
  | "redacted_thinking" => (strField b "data").map .redactedThinking
  | _ => none

def extractContent (content : Array Json) : Array LLMClient.Block := content.filterMap readBlock

/-- The Messages API takes a system prompt as a top-level `system` string field, a sibling of
`messages` rather than a message inside it, so it's appended here instead of via `msgJson`. -/
def requestBody (config : LLMClient.Config) (history : Array LLMClient.Msg)
    (tools : Array LLMClient.Tool := #[]) : Json :=
  let fields : List (String × Json) :=
    [ ("model", config.model),
      ("max_tokens", Json.ofNat config.maxOutputTokens),
      ("tools", Json.arr (tools.map toolJson)),
      ("messages", Json.arr (messagesJson history)) ]
  Json.mkObj (match config.systemPrompt with
    | some s => fields ++ [("system", Json.str s)]
    | none => fields)

/-- Reads one Messages API response. Pure, so that what a status and body mean is settled here
and the transport carrying them decides nothing. -/
def parseReply (status : UInt32) (body : ByteArray) : Except String LLMClient.Reply :=
  if status >= 200 && status < 300 then
    match String.fromUTF8? body with
    | none => .error "the Claude API returned a response that wasn't valid UTF-8"
    | some text =>
      match Json.parse text with
      | .error e => .error s!"couldn't parse the Claude API response: {e}"
      | .ok json =>
        let content := contentOf json
        .ok
          { content := extractContent content,
            isRefusal := stopReasonOf json == "refusal",
            isTruncated := stopReasonOf json == "max_tokens" }
  else
    .error s!"Claude API error ({status}): {LLMClient.errorDetail errorMessageOf body}"

/-- Posts one Messages API request with the given history; does not loop for tool use. -/
def sendRequest [Monad m] (transport : LLMClient.Transport m) (config : LLMClient.Config)
    (apiKey : String) (history : Array LLMClient.Msg) (tools : Array LLMClient.Tool := #[]) :
    m (Except String LLMClient.Reply) := do
  let body := requestBody config history tools
  let headers :=
    [ ("x-api-key", apiKey),
      ("anthropic-version", "2023-06-01"),
      ("content-type", "application/json") ] ++ config.extraHeaders
  match ← transport config.apiUrl body.compress.toUTF8 headers with
  | .error e => return .error s!"request to the Claude API failed: {e}"
  | .ok resp => return parseReply resp.status resp.body

def providerWith [Monad m] (transport : LLMClient.Transport m) (apiKey : String)
    (config : LLMClient.Config := defaultConfig) : LLMClient.Provider m :=
  { name := "claude"
    sendRequest := fun history tools => sendRequest transport config apiKey history tools }

end LLMClient.Claude
