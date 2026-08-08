-- Copyright (c) 2026 Paul Butcher. All rights reserved.
-- Released under Apache 2.0 license as described in the file LICENSE.

import Lean.Data.Json
import Lean.Data.Json.Parser
import Leancurl
import LLMClient.Provider

namespace LLMClient.OpenAI

open Lean (Json)
open Leancurl

def defaultConfig : LLMClient.Config :=
  { apiUrl := "https://api.openai.com/v1/chat/completions", model := "gpt-5" }

def toolJson (t : LLMClient.Tool) : Json :=
  Json.mkObj
    [ ("type", "function"),
      ("function", Json.mkObj
        [ ("name", t.name), ("description", t.description), ("parameters", t.schema) ]) ]

def toolCallJson (tc : LLMClient.ToolCall) : Json :=
  Json.mkObj
    [ ("id", tc.id),
      ("type", "function"),
      ("function", Json.mkObj [("name", tc.name), ("arguments", tc.input.compress)]) ]

/-- OpenAI models a tool result as its own message keyed by `tool_call_id`, rather than as a
block inside a user message the way Anthropic does. -/
def msgJson : LLMClient.Msg → Json
  | .user text => Json.mkObj [("role", "user"), ("content", text)]
  | .assistant text toolCalls =>
    let content := if text.isEmpty then Json.null else Json.str text
    if toolCalls.isEmpty then
      Json.mkObj [("role", "assistant"), ("content", content)]
    else
      Json.mkObj
        [ ("role", "assistant"), ("content", content),
          ("tool_calls", Json.arr (toolCalls.map toolCallJson)) ]
  | .toolResult id output =>
    Json.mkObj [("role", "tool"), ("tool_call_id", id), ("content", output)]

def errorMessageOf (j : Json) : String :=
  match j.getObjVal? "error" >>= (·.getObjVal? "message") >>= Json.getStr? with
  | .ok msg => msg
  | .error _ => j.compress

def firstChoice (resp : Json) : Json :=
  match resp.getObjVal? "choices" >>= Json.getArr? with
  | .ok choices => choices.getD 0 (Json.mkObj [])
  | .error _ => Json.mkObj []

def messageOf (resp : Json) : Json :=
  ((firstChoice resp).getObjVal? "message").toOption.getD (Json.mkObj [])

def finishReasonOf (resp : Json) : String :=
  match (firstChoice resp).getObjVal? "finish_reason" >>= Json.getStr? with
  | .ok s => s
  | .error _ => ""

def extractToolCalls (message : Json) : Array LLMClient.ToolCall :=
  match message.getObjVal? "tool_calls" >>= Json.getArr? with
  | .ok arr =>
    arr.filterMap fun tc =>
      match tc.getObjVal? "id" >>= Json.getStr?,
            tc.getObjVal? "function" >>= (·.getObjVal? "name") >>= Json.getStr? with
      | .ok id, .ok name =>
        let arguments :=
          (tc.getObjVal? "function" >>= (·.getObjVal? "arguments") >>= Json.getStr?).toOption
        let input := (arguments.bind fun a => (Json.parse a).toOption).getD (Json.mkObj [])
        some { id, name, input }
      | _, _ => none
  | .error _ => #[]

/-- Newer model families (including `gpt-5`) expect the system prompt as a leading message
with role `developer` rather than the traditional `system`, so it's prepended here instead of
via `msgJson`. -/
def requestBody (config : LLMClient.Config) (history : Array LLMClient.Msg)
    (tools : Array LLMClient.Tool := #[]) : Json :=
  let developerMsg : Array Json :=
    match config.systemPrompt with
    | some s => #[Json.mkObj [("role", "developer"), ("content", s)]]
    | none => #[]
  Json.mkObj
    [ ("model", config.model),
      ("max_completion_tokens", config.maxOutputTokens),
      ("tools", Json.arr (tools.map toolJson)),
      ("messages", Json.arr (developerMsg ++ history.map msgJson)) ]

/-- Posts one Chat Completions request with the given history; does not loop for tool use. -/
def sendRequest (config : LLMClient.Config) (apiKey : String) (history : Array LLMClient.Msg)
    (tools : Array LLMClient.Tool := #[]) : IO (Except String LLMClient.Reply) := do
  let body := requestBody config history tools
  let headers : Headers :=
    [ ("authorization", s!"Bearer {apiKey}"),
      ("content-type", "application/json") ]
  match ← Curl.post config.apiUrl body.compress.toUTF8 headers with
  | .error e => return .error s!"request to the OpenAI API failed: {e.message}"
  | .ok resp =>
    match String.fromUTF8? resp.body with
    | none => return .error "the OpenAI API returned a response that wasn't valid UTF-8"
    | some text =>
      match Json.parse text with
      | .error e => return .error s!"couldn't parse the OpenAI API response: {e}"
      | .ok json =>
        if resp.status >= 200 && resp.status < 300 then
          let message := messageOf json
          let content := (message.getObjVal? "content" >>= Json.getStr?).toOption.getD ""
          return .ok
            { text := content,
              toolCalls := extractToolCalls message,
              isRefusal := finishReasonOf json == "content_filter",
              isTruncated := finishReasonOf json == "length" }
        else
          return .error s!"OpenAI API error ({resp.status}): {errorMessageOf json}"

def provider (config : LLMClient.Config := defaultConfig) : LLMClient.Provider :=
  { name := "openai", sendRequest := fun apiKey history tools => sendRequest config apiKey history tools }

end LLMClient.OpenAI
