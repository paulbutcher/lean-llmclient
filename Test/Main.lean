-- Copyright (c) 2026 Paul Butcher. All rights reserved.
-- Released under Apache 2.0 license as described in the file LICENSE.

import Lean.Data.Json
import LLMClient.OpenAI
import LLMClient.Claude

open Lean (Json)

/-- Collects every failing check by name instead of aborting at the first one, so a single run
reports the full set of regressions. -/
structure Results where
  failures : Array String := #[]

def Results.check (r : Results) (name : String) (cond : Bool) : Results :=
  if cond then r else { r with failures := r.failures.push name }

def Results.report (r : Results) : IO UInt32 := do
  if r.failures.isEmpty then
    IO.println "All tests passed."
    return 0
  else
    IO.eprintln s!"{r.failures.size} test(s) failed:"
    for name in r.failures do
      IO.eprintln s!"  - {name}"
    return 1

def sampleTool : LLMClient.Tool :=
  { name := "get_weather", description := "Get the weather", schema := Json.mkObj [("type", "object")] }

def sampleToolCall : LLMClient.ToolCall :=
  { id := "call_1", name := "get_weather", input := Json.mkObj [("city", "Paris")] }

def testOpenAI (r : Results) : Results :=
  open LLMClient.OpenAI in
  let r := r
    |>.check "msgJson user"
      (msgJson (.user "hi") == Json.mkObj [("role", "user"), ("content", "hi")])
    |>.check "msgJson assistant text only"
      (msgJson (.assistant "hello" #[]) == Json.mkObj [("role", "assistant"), ("content", "hello")])
    |>.check "msgJson assistant no text no tools"
      (msgJson (.assistant "" #[]) == Json.mkObj [("role", "assistant"), ("content", Json.null)])
    |>.check "msgJson assistant tool call only"
      (msgJson (.assistant "" #[sampleToolCall]) ==
        Json.mkObj
          [ ("role", "assistant"), ("content", Json.null),
            ("tool_calls", Json.arr #[toolCallJson sampleToolCall]) ])
    |>.check "msgJson assistant text and tool call"
      (msgJson (.assistant "hi" #[sampleToolCall]) ==
        Json.mkObj
          [ ("role", "assistant"), ("content", "hi"),
            ("tool_calls", Json.arr #[toolCallJson sampleToolCall]) ])
    |>.check "msgJson toolResult"
      (msgJson (.toolResult "id1" "42") ==
        Json.mkObj [("role", "tool"), ("tool_call_id", "id1"), ("content", "42")])
    |>.check "toolJson"
      (toolJson sampleTool ==
        Json.mkObj
          [ ("type", "function"),
            ("function", Json.mkObj
              [ ("name", "get_weather"), ("description", "Get the weather"),
                ("parameters", sampleTool.schema) ]) ])
    |>.check "toolCallJson"
      (toolCallJson sampleToolCall ==
        Json.mkObj
          [ ("id", "call_1"), ("type", "function"),
            ("function", Json.mkObj [("name", "get_weather"), ("arguments", sampleToolCall.input.compress)]) ])
  let normalResp := Json.mkObj
    [ ("choices", Json.arr
        #[Json.mkObj
            [ ("message", Json.mkObj [("role", "assistant"), ("content", "Hello there")]),
              ("finish_reason", "stop") ]]) ]
  let filteredResp := Json.mkObj
    [ ("choices", Json.arr #[Json.mkObj [("finish_reason", "content_filter")]]) ]
  let malformedResp := Json.mkObj [("foo", Json.null)]
  let toolCallInput := Json.mkObj [("city", "Paris")]
  let toolCallsMessage := Json.mkObj
    [ ("tool_calls", Json.arr
        #[ Json.mkObj
             [ ("id", "call_1"),
               ("function", Json.mkObj [("name", "get_weather"), ("arguments", toolCallInput.compress)]) ],
           Json.mkObj [("function", Json.mkObj [("name", "missing_id")])] ]) ]
  let errorResp := Json.mkObj [("error", Json.mkObj [("message", "boom")])]
  let r := r
    |>.check "messageOf normal response"
      (messageOf normalResp == Json.mkObj [("role", "assistant"), ("content", "Hello there")])
    |>.check "finishReasonOf normal response"
      (finishReasonOf normalResp == "stop")
    |>.check "finishReasonOf content_filter response"
      (finishReasonOf filteredResp == "content_filter")
    |>.check "firstChoice on malformed response"
      (firstChoice malformedResp == Json.mkObj [])
    |>.check "messageOf on malformed response"
      (messageOf malformedResp == Json.mkObj [])
    |>.check "finishReasonOf on malformed response"
      (finishReasonOf malformedResp == "")
    |>.check "extractToolCalls skips entries missing id, keeps well-formed ones"
      (extractToolCalls toolCallsMessage ==
        #[({ id := "call_1", name := "get_weather", input := toolCallInput } : LLMClient.ToolCall)])
    |>.check "errorMessageOf on error response"
      (errorMessageOf errorResp == "boom")
    |>.check "errorMessageOf falls back to compress on malformed response"
      (errorMessageOf malformedResp == malformedResp.compress)
  r

def testClaude (r : Results) : Results :=
  open LLMClient.Claude in
  let r := r
    |>.check "msgJson user"
      (msgJson (.user "hi") == Json.mkObj [("role", "user"), ("content", "hi")])
    |>.check "msgJson assistant text only"
      (msgJson (.assistant "hello" #[]) ==
        Json.mkObj
          [ ("role", "assistant"),
            ("content", Json.arr #[Json.mkObj [("type", "text"), ("text", "hello")]]) ])
    |>.check "msgJson assistant tool call only"
      (msgJson (.assistant "" #[sampleToolCall]) ==
        Json.mkObj
          [ ("role", "assistant"),
            ("content", Json.arr
              #[Json.mkObj
                  [ ("type", "tool_use"), ("id", sampleToolCall.id), ("name", sampleToolCall.name),
                    ("input", sampleToolCall.input) ]]) ])
    |>.check "msgJson assistant text and tool call"
      (msgJson (.assistant "hi" #[sampleToolCall]) ==
        Json.mkObj
          [ ("role", "assistant"),
            ("content", Json.arr
              #[ Json.mkObj [("type", "text"), ("text", "hi")],
                 Json.mkObj
                   [ ("type", "tool_use"), ("id", sampleToolCall.id), ("name", sampleToolCall.name),
                     ("input", sampleToolCall.input) ] ]) ])
    |>.check "msgJson toolResult"
      (msgJson (.toolResult "id1" "42") ==
        Json.mkObj
          [ ("role", "user"),
            ("content", Json.arr
              #[Json.mkObj [("type", "tool_result"), ("tool_use_id", "id1"), ("content", "42")]]) ])
    |>.check "toolJson"
      (toolJson sampleTool ==
        Json.mkObj
          [ ("name", "get_weather"), ("description", "Get the weather"),
            ("input_schema", sampleTool.schema) ])
  let textBlock := Json.mkObj [("type", "text"), ("text", "Hello ")]
  let textBlock2 := Json.mkObj [("type", "text"), ("text", "world")]
  let toolUseBlock := Json.mkObj [("type", "tool_use"), ("id", "call_1"), ("name", "get_weather"), ("input", Json.mkObj [("city", "Paris")])]
  let malformedToolUseBlock := Json.mkObj [("type", "tool_use"), ("name", "missing_id")]
  let noTypeBlock := Json.mkObj [("text", "no type field")]
  let content := #[textBlock, toolUseBlock, textBlock2, malformedToolUseBlock]
  let normalResp := Json.mkObj [("content", Json.arr content), ("stop_reason", "end_turn")]
  let refusalResp := Json.mkObj [("stop_reason", "refusal")]
  let malformedResp := Json.mkObj [("foo", Json.null)]
  let errorResp := Json.mkObj [("error", Json.mkObj [("message", "boom")])]
  let r := r
    |>.check "blockType on text block"
      (blockType textBlock == "text")
    |>.check "blockType on block missing type"
      (blockType noTypeBlock == "")
    |>.check "contentOf normal response"
      (contentOf normalResp == content)
    |>.check "contentOf on malformed response"
      (contentOf malformedResp == #[])
    |>.check "stopReasonOf end_turn"
      (stopReasonOf normalResp == "end_turn")
    |>.check "stopReasonOf refusal"
      (stopReasonOf refusalResp == "refusal")
    |>.check "stopReasonOf on malformed response"
      (stopReasonOf malformedResp == "")
    |>.check "extractText concatenates only text blocks in order"
      (extractText content == "Hello world")
    |>.check "extractToolCalls skips entries missing id, keeps well-formed ones"
      (extractToolCalls content ==
        #[({ id := "call_1", name := "get_weather", input := Json.mkObj [("city", "Paris")] } : LLMClient.ToolCall)])
    |>.check "errorMessageOf on error response"
      (errorMessageOf errorResp == "boom")
    |>.check "errorMessageOf falls back to compress on malformed response"
      (errorMessageOf malformedResp == malformedResp.compress)
  r

def main : IO UInt32 := do
  let r : Results := {}
  let r := testOpenAI r
  let r := testClaude r
  r.report
