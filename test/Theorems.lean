-- Copyright (c) 2026 Paul Butcher. All rights reserved.
-- Released under Apache 2.0 license as described in the file LICENSE.

import LLMClient

namespace LLMClient.Test

open Lean (Json)

private theorem foldl_eq_init {α β : Type _} (f : β → α → β) (init : β) (a : Array α)
    (h : ∀ b x, f b x = b) : a.foldl f init = init := by
  rw [← Array.foldl_toList]
  induction a.toList generalizing init with
  | nil => simp
  | cons x xs ih => simp [h, ih]

private theorem foldl_map_eq_init {α β γ : Type _} (f : β → γ → β) (g : α → γ) (init : β)
    (a : Array α) (h : ∀ b x, f b (g x) = b) : (a.map g).foldl f init = init := by
  rw [Array.foldl_map]
  exact foldl_eq_init _ init a h

private theorem filterMap_map_id {α γ : Type _} (f : γ → Option α) (g : α → γ) (a : Array α)
    (h : ∀ x, f (g x) = some x) : (a.map g).filterMap f = a := by
  rw [Array.filterMap_map]
  rw [show f ∘ g = some from funext h]
  simp

private theorem contentOf_mkObj (r : Json) (a : Array Json) :
    Claude.contentOf (Json.mkObj [("role", r), ("content", Json.arr a)]) = a := rfl

private theorem extractToolCalls_append (x y : Array Json) :
    Claude.extractToolCalls (x ++ y) =
      Claude.extractToolCalls x ++ Claude.extractToolCalls y := by
  simp [Claude.extractToolCalls, Array.filterMap_append]

/-- Every text block Claude sends back survives the encode/decode round trip, concatenated in
order and with the tool-call blocks contributing nothing. -/
theorem claude_extractText_roundTrip (text : String) (tcs : Array ToolCall) :
    Claude.extractText (Claude.contentOf (Claude.msgJson (.assistant text tcs))) = text := by
  simp only [Claude.msgJson, contentOf_mkObj, Claude.extractText]
  rw [Array.foldl_append, foldl_map_eq_init _ _ _ _ (by intro b tc; rfl)]
  split
  · simp_all [String.isEmpty_iff]
  · rfl

/-- Every tool call survives the encode/decode round trip unchanged and in order, whatever text
accompanies it. -/
theorem claude_extractToolCalls_roundTrip (text : String) (tcs : Array ToolCall) :
    Claude.extractToolCalls (Claude.contentOf (Claude.msgJson (.assistant text tcs))) = tcs := by
  simp only [Claude.msgJson, contentOf_mkObj]
  rw [extractToolCalls_append]
  rw [show Claude.extractToolCalls
    (tcs.map fun tc => Json.mkObj
      [("type", Json.str "tool_use"), ("id", Json.str tc.id), ("name", Json.str tc.name),
        ("input", tc.input)]) = tcs from filterMap_map_id _ _ _ (fun _ => rfl)]
  split
  · rw [show Claude.extractToolCalls (#[] : Array Json) = #[] from rfl]
    simp
  · rw [show Claude.extractToolCalls
      #[Json.mkObj [("type", Json.str "text"), ("text", Json.str text)]] = #[] from rfl]
    simp

/-- The Messages API rejects any role outside this pair, so no `Msg` may encode to another one. -/
theorem claude_msgJson_role (m : Msg) :
    (Claude.msgJson m).getObjVal? "role" = .ok (Json.str "user") ∨
      (Claude.msgJson m).getObjVal? "role" = .ok (Json.str "assistant") := by
  cases m
  · exact Or.inl rfl
  · exact Or.inr rfl
  · exact Or.inl rfl

/-- Chat Completions rejects any role outside this triple, so no `Msg` may encode to another
one. -/
theorem openai_msgJson_role (m : Msg) :
    (OpenAI.msgJson m).getObjVal? "role" = .ok (Json.str "user") ∨
      (OpenAI.msgJson m).getObjVal? "role" = .ok (Json.str "assistant") ∨
      (OpenAI.msgJson m).getObjVal? "role" = .ok (Json.str "tool") := by
  cases m
  · exact Or.inl rfl
  · refine Or.inr (Or.inl ?_)
    simp only [OpenAI.msgJson]
    split <;> rfl
  · exact Or.inr (Or.inr rfl)

/-- Chat Completions requires an absent assistant message body to be `null`; an empty string is
rejected. -/
theorem openai_assistant_emptyText_isNull (tcs : Array ToolCall) :
    (OpenAI.msgJson (.assistant "" tcs)).getObjVal? "content" = .ok Json.null := by
  simp only [OpenAI.msgJson]
  split <;> rfl

private def messagesOf (body : Json) : Array Json :=
  (body.getObjVal? "messages" >>= Json.getArr?).toOption.getD #[]

private theorem messagesOf_openai (model maxTokens tools : Json) (msgs : Array Json) :
    messagesOf (Json.mkObj
      [("model", model), ("max_completion_tokens", maxTokens), ("tools", tools),
        ("messages", Json.arr msgs)]) = msgs := rfl

/-- A system prompt costs exactly one extra message and never displaces a turn of history. -/
theorem openai_requestBody_messageCount (config : Config) (history : Array Msg)
    (tools : Array Tool) :
    (messagesOf (OpenAI.requestBody config history tools)).size =
      history.size + if config.systemPrompt.isSome then 1 else 0 := by
  simp only [OpenAI.requestBody, messagesOf_openai]
  cases config.systemPrompt <;> simp <;> omega

end LLMClient.Test
