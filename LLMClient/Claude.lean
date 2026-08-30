-- Copyright (c) 2026 Paul Butcher. All rights reserved.
-- Released under Apache 2.0 license as described in the file LICENSE.

module

public import Json
import Leancurl
public import LLMClient.Provider

public section

namespace LLMClient.Claude

open Leancurl

def defaultConfig : LLMClient.Config :=
  { apiUrl := "https://api.anthropic.com/v1/messages", model := "claude-opus-5" }

def toolJson (t : LLMClient.Tool) : Json :=
  Json.mkObj [("name", t.name), ("description", t.description), ("input_schema", t.schema)]

/-- One `tool_result` block, as it appears in the content of the user message answering a request
for tools. `is_error` is omitted rather than sent as `false`, in the same way as the system
prompt. -/
def toolResultJson (id output : String) (isError : Bool := false) : Json :=
  let fields : List (String × Json) :=
    [("type", "tool_result"), ("tool_use_id", id), ("content", output)]
  Json.mkObj (if isError then fields ++ [("is_error", Json.bool true)] else fields)

/-- A lone `.toolResult` becomes a message of its own here, which is right only when it is the one
result outstanding. `messagesJson` is what a request is built from, and it groups.

`Msg.toolResult`'s `structured` is dropped rather than overlooked: a `tool_result` block's content
is a string or text and image blocks, with nowhere to put JSON, so `output` is what the model
sees. Only Converse has a place for it. -/
def msgJson : LLMClient.Msg → Json
  | .user text => Json.mkObj [("role", "user"), ("content", text)]
  | .assistant text toolCalls =>
    let textBlock := if text.isEmpty then #[] else #[Json.mkObj [("type", "text"), ("text", text)]]
    let toolBlocks := toolCalls.map fun tc =>
      Json.mkObj [("type", "tool_use"), ("id", tc.id), ("name", tc.name), ("input", tc.input)]
    Json.mkObj [("role", "assistant"), ("content", Json.arr (textBlock ++ toolBlocks))]
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

def extractText (content : Array Json) : String :=
  content.foldl (init := "") fun acc b =>
    if blockType b == "text" then
      match b.getObjVal? "text" >>= Json.getStr? with
      | .ok t => acc ++ t
      | .error _ => acc
    else acc

def extractToolCalls (content : Array Json) : Array LLMClient.ToolCall :=
  content.filterMap fun b =>
    if blockType b == "tool_use" then
      match b.getObjVal? "id" >>= Json.getStr?, b.getObjVal? "name" >>= Json.getStr? with
      | .ok id, .ok name =>
        let input := (b.getObjVal? "input").toOption.getD (Json.mkObj [])
        some { id, name, input }
      | _, _ => none
    else none

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

/-- Posts one Messages API request with the given history; does not loop for tool use. -/
def sendRequest (config : LLMClient.Config) (apiKey : String) (history : Array LLMClient.Msg)
    (tools : Array LLMClient.Tool := #[]) : IO (Except String LLMClient.Reply) := do
  let body := requestBody config history tools
  let headers : Headers :=
    [ ("x-api-key", apiKey),
      ("anthropic-version", "2023-06-01"),
      ("content-type", "application/json") ]
  match ← Curl.post config.apiUrl body.compress.toUTF8 headers with
  | .error e => return .error s!"request to the Claude API failed: {e.message}"
  | .ok resp =>
    match String.fromUTF8? resp.body with
    | none => return .error "the Claude API returned a response that wasn't valid UTF-8"
    | some text =>
      match Json.parse text with
      | .error e => return .error s!"couldn't parse the Claude API response: {e}"
      | .ok json =>
        if resp.status >= 200 && resp.status < 300 then
          let content := contentOf json
          return .ok
            { text := extractText content,
              toolCalls := extractToolCalls content,
              isRefusal := stopReasonOf json == "refusal",
              isTruncated := stopReasonOf json == "max_tokens" }
        else
          return .error s!"Claude API error ({resp.status}): {errorMessageOf json}"

def provider (apiKey : String) (config : LLMClient.Config := defaultConfig) : LLMClient.Provider :=
  { name := "claude", sendRequest := fun history tools => sendRequest config apiKey history tools }

end LLMClient.Claude
