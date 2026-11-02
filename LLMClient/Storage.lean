-- Copyright (c) 2026 Paul Butcher. All rights reserved.
-- Released under Apache 2.0 license as described in the file LICENSE.

module

public import Json
public import LLMClient.Provider

public section

/-!
`Msg` as JSON, for a caller that stores a conversation and replays it later. This is a format
consumers persist, so a change to it breaks their stored data. The README documents it.

The decoder rejects anything the encoder would not have written, unexpected fields included, and
says what was wrong: whether to drop a bad entry or reject the whole history is the caller's call.
-/

namespace LLMClient

private def expectFields (what : String) (allowed : List String) (j : Json) :
    Except String Unit := do
  let fields ← j.getObj?.mapError fun _ => s!"{what}: expected an object"
  match fields.find? (fun (k, _) => !allowed.contains k) with
  | some (k, _) => .error s!"{what}: unexpected field {k}"
  | none => .ok ()

private def field (what : String) (j : Json) (k : String) : Except String Json :=
  (j.getObjVal? k).mapError fun _ => s!"{what}: missing field {k}"

private def strField (what : String) (j : Json) (k : String) : Except String String := do
  (← field what j k).getStr?.mapError fun _ => s!"{what}: field {k} should be a string"

def ToolCall.toJson (tc : ToolCall) : Json :=
  Json.mkObj [("id", tc.id), ("name", tc.name), ("input", tc.input)]

def ToolCall.fromJson? (j : Json) : Except String ToolCall := do
  expectFields "tool call" ["id", "name", "input"] j
  return { id := ← strField "tool call" j "id", name := ← strField "tool call" j "name",
           input := ← field "tool call" j "input" }

def Block.toJson : Block → Json
  | .text t => Json.mkObj [("type", "text"), ("text", t)]
  | .toolCall tc =>
    Json.mkObj [("type", "tool_call"), ("id", tc.id), ("name", tc.name), ("input", tc.input)]
  | .thinking t signature =>
    Json.mkObj [("type", "thinking"), ("thinking", t), ("signature", signature)]
  | .redactedThinking data => Json.mkObj [("type", "redacted_thinking"), ("data", data)]

def Block.fromJson? (j : Json) : Except String Block := do
  match ← strField "block" j "type" with
  | "text" =>
    expectFields "text block" ["type", "text"] j
    return .text (← strField "text block" j "text")
  | "tool_call" =>
    expectFields "tool_call block" ["type", "id", "name", "input"] j
    return .toolCall
      { id := ← strField "tool_call block" j "id", name := ← strField "tool_call block" j "name",
        input := ← field "tool_call block" j "input" }
  | "thinking" =>
    expectFields "thinking block" ["type", "thinking", "signature"] j
    return .thinking (← strField "thinking block" j "thinking")
      (← strField "thinking block" j "signature")
  | "redacted_thinking" =>
    expectFields "redacted_thinking block" ["type", "data"] j
    return .redactedThinking (← strField "redacted_thinking block" j "data")
  | other => .error s!"block: unknown type {other}"

/-- `structured` is written only when there is one, since `null` is a value it can hold. -/
def Msg.toJson : Msg → Json
  | .user text => Json.mkObj [("role", "user"), ("text", text)]
  | .assistant content =>
    Json.mkObj [("role", "assistant"), ("content", Json.arr (content.map Block.toJson))]
  | .toolResult id output isError structured =>
    let fields : List (String × Json) :=
      [("role", "tool_result"), ("id", id), ("output", output), ("is_error", Json.bool isError)]
    Json.mkObj (match structured with
      | some s => fields ++ [("structured", s)]
      | none => fields)

def Msg.fromJson? (j : Json) : Except String Msg := do
  match ← strField "message" j "role" with
  | "user" =>
    expectFields "user message" ["role", "text"] j
    return .user (← strField "user message" j "text")
  | "assistant" =>
    expectFields "assistant message" ["role", "content"] j
    let blocks ← (← field "assistant message" j "content").getArr?.mapError fun _ =>
      "assistant message: field content should be an array"
    return .assistant (← blocks.mapM Block.fromJson?)
  | "tool_result" =>
    expectFields "tool_result message" ["role", "id", "output", "is_error", "structured"] j
    let isError ← (← field "tool_result message" j "is_error").getBool?.mapError fun _ =>
      "tool_result message: field is_error should be a boolean"
    return .toolResult (← strField "tool_result message" j "id")
      (← strField "tool_result message" j "output") isError
      (j.getObjVal? "structured").toOption
  | other => .error s!"message: unknown role {other}"

instance : Json.ToJson Msg := ⟨Msg.toJson⟩
instance : Json.FromJson Msg := ⟨Msg.fromJson?⟩

end LLMClient
