-- Copyright (c) 2026 Paul Butcher. All rights reserved.
-- Released under Apache 2.0 license as described in the file LICENSE.

import Lean.Data.Json
import Lean.Data.Json.Parser
import Leancurl
import LLMClient.Provider

namespace LLMClient.Claude

open Lean (Json)
open Leancurl

def defaultConfig : LLMClient.Config :=
  { apiUrl := "https://api.anthropic.com/v1/messages", model := "claude-opus-5" }

def toolJson (t : LLMClient.Tool) : Json :=
  Json.mkObj [("name", t.name), ("description", t.description), ("input_schema", t.schema)]

def msgJson : LLMClient.Msg → Json
  | .user text => Json.mkObj [("role", "user"), ("content", text)]
  | .assistant text toolCalls =>
    let textBlock := if text.isEmpty then #[] else #[Json.mkObj [("type", "text"), ("text", text)]]
    let toolBlocks := toolCalls.map fun tc =>
      Json.mkObj [("type", "tool_use"), ("id", tc.id), ("name", tc.name), ("input", tc.input)]
    Json.mkObj [("role", "assistant"), ("content", Json.arr (textBlock ++ toolBlocks))]
  | .toolResult id output =>
    Json.mkObj
      [ ("role", "user"),
        ("content", Json.arr
          #[Json.mkObj [("type", "tool_result"), ("tool_use_id", id), ("content", output)]]) ]

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

/-- Posts one Messages API request with the given history; does not loop for tool use. -/
def sendRequest (config : LLMClient.Config) (apiKey : String) (history : Array LLMClient.Msg)
    (tools : Array LLMClient.Tool := #[]) : IO (Except String LLMClient.Reply) := do
  let body := Json.mkObj
    [ ("model", config.model),
      ("max_tokens", config.maxOutputTokens),
      ("tools", Json.arr (tools.map toolJson)),
      ("messages", Json.arr (history.map msgJson)) ]
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

def provider (config : LLMClient.Config := defaultConfig) : LLMClient.Provider :=
  { name := "claude", sendRequest := fun apiKey history tools => sendRequest config apiKey history tools }

end LLMClient.Claude
