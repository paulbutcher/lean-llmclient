-- Copyright (c) 2026 Paul Butcher. All rights reserved.
-- Released under Apache 2.0 license as described in the file LICENSE.

import LLMClient

namespace LLMClient.Test

/-- A fold whose step ignores its element and hands the accumulator straight back leaves the
starting value alone, however long the array. The round-trip theorems below rest on this: rather
than reason about each block that is meant to contribute nothing, they show the step is a no-op
over those blocks and the whole fold collapses.

`f` is the step, `init` the starting accumulator, and `a` an arbitrary array of any element type.
The hypothesis `h` says `f b x = b` at every accumulator and every element, so it constrains `f`
alone and says nothing about `a`; the conclusion is that folding `f` over `a` returns `init`
unchanged. `a` may be empty, which would make the claim trivial, but the induction is over its
length and so covers every other case too. -/
private theorem foldl_eq_init {α β : Type _} (f : β → α → β) (init : β) (a : Array α)
    (h : ∀ b x, f b x = b) : a.foldl f init = init := by
  rw [← Array.foldl_toList]
  induction a.toList generalizing init with
  | nil => simp
  | cons x xs ih => simp [h, ih]

/-- The same collapse one step earlier, for a fold over an array that is itself the image of a
map. It asks only that the step be a no-op on the values the map produces, not on every value of
the mapped-to type, which is the form the round-trip theorems can actually discharge: they fold an
extractor over blocks built by an encoding function.

`g` builds each folded element from an element of `a`, `f` is the step and `init` the starting
accumulator. `h` requires `f b (g x) = b` only for `x` drawn from the source type, so `f` is free
to change the accumulator on values outside `g`'s range; the conclusion is that folding over
`a.map g` still returns `init`. `a` may again be empty, but the proof rewrites to the unmapped
form above rather than relying on that case. -/
private theorem foldl_map_eq_init {α β γ : Type _} (f : β → γ → β) (g : α → γ) (init : β)
    (a : Array α) (h : ∀ b x, f b (g x) = b) : (a.map g).foldl f init = init := by
  rw [Array.foldl_map]
  exact foldl_eq_init _ init a h

/-- Encoding every element of an array and then decoding the results recovers the array exactly:
same elements, same order, nothing dropped. Every round-trip theorem here reduces to this shape,
and it is what rules out a decoder that quietly loses or reorders entries.

`g` is the encoder and `f` the decoder, which returns an `Option` because it is free to reject
what it is given. `h` says the decoder answers `some x` on anything `g` built from `x`, so it
never rejects and never yields some other element. `filterMap` both discards the `none`s and
unwraps the `some`s, and the conclusion is that doing so gives back `a` itself. The hypothesis
constrains `f ∘ g` alone, leaving `a` arbitrary, so the claim covers arrays of every length rather
than just the empty one. -/
private theorem filterMap_map_id {α γ : Type _} (f : γ → Option α) (g : α → γ) (a : Array α)
    (h : ∀ x, f (g x) = some x) : (a.map g).filterMap f = a := by
  rw [Array.filterMap_map]
  rw [show f ∘ g = some from funext h]
  simp

/-- Claude's response reader finds the content blocks in the object shape the encoder produces.
This is what makes it legitimate for the round-trip theorems below to feed an encoded assistant
message straight back into the reader; without it they would be proving something about
`contentOf` applied to an object it never receives.

`r` stands for whatever the `role` field holds, so the claim is independent of the role, and `a`
is the block array under `content`. `contentOf` looks `content` up and asks for an array, falling
back to `#[]` if either step fails. The statement is that on this object it returns `a` itself, so
what the round-trip theorems read is the encoder's own output and not that fallback. -/
private theorem contentOf_mkObj (r : Json) (a : Array Json) :
    Claude.contentOf (Json.mkObj [("role", r), ("content", Json.arr a)]) = a := rfl

/-- Reading tool calls out of a run of blocks does not depend on where that run is split: the
calls found in two concatenated block arrays are those of the first followed by those of the
second. The round-trip theorem below builds content as a text block followed by tool-use blocks,
and this is what lets it take the two halves separately.

`x` and `y` are arbitrary block arrays, well-formed or not. The claim is that `extractToolCalls`
of `x ++ y` equals its result on `x` appended to its result on `y`, which holds because it is a
`filterMap`: each block's fate is decided by that block alone, so nothing is gained or lost at the
join. Neither array is required to be non-empty, and since both are arbitrary the identity is not
carried by an empty case. -/
private theorem extractToolCalls_append (x y : Array Json) :
    Claude.extractToolCalls (x ++ y) =
      Claude.extractToolCalls x ++ Claude.extractToolCalls y := by
  simp [Claude.extractToolCalls, Array.filterMap_append]

/-- Every text block Claude sends back survives the encode/decode round trip, concatenated in
order and with the tool-call blocks contributing nothing. The text a caller reads off `Reply` is
therefore exactly what the model wrote, and a turn that also asks for tools cannot corrupt it.

`text` is the assistant turn's prose and `tcs` the calls it requests. `msgJson` encodes both into
a Messages API assistant message, `contentOf` takes the content blocks back out, and `extractText`
concatenates the `text` field of every block whose `type` is `"text"`, skipping the rest. The
claim is that this yields `text` itself, so the tool-use blocks contribute nothing and the text
block is reproduced verbatim. `text` may be empty, in which case the encoder emits no text block
at all and the theorem still asserts `""`; that is a real case the proof splits on, not an
escape. -/
theorem claude_extractText_roundTrip (text : String) (tcs : Array ToolCall) :
    Claude.extractText (Claude.contentOf (Claude.msgJson (.assistant text tcs))) = text := by
  simp only [Claude.msgJson, contentOf_mkObj, Claude.extractText]
  rw [Array.foldl_append, foldl_map_eq_init _ _ _ _ (by intro b tc; rfl)]
  split
  · simp_all [String.isEmpty_iff]
  · rfl

/-- Every tool call survives the encode/decode round trip unchanged and in order, whatever text
accompanies it. A caller dispatching on `ToolCall.name` and answering with `ToolCall.id` is
therefore working from what the model actually asked for.

`tcs` are the calls the assistant turn requests and `text` the prose alongside them. `msgJson`
writes each call as a `tool_use` block, `contentOf` recovers the block array, and
`extractToolCalls` keeps the `tool_use` blocks whose `id` and `name` are strings and rebuilds a
`ToolCall` from each. The claim is equality with `tcs` itself: no call is dropped by that
well-formedness check, none is reordered, and the text block yields none. `tcs` may be empty, but
the mapping lemma the proof applies holds at every length, and the proof still splits on whether
`text` is empty. -/
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

/-- No `Msg` can encode to a role the Messages API will not accept. `role` is the one field every
message carries and the only one whose set of legal values is fixed, so a mistake in it fails the
whole request rather than degrading one turn.

`m` ranges over all three `Msg` constructors. The claim is that looking `role` up in `msgJson m`
succeeds and finds `"user"` or `"assistant"`: `.user` and `.toolResult` give the first,
`.assistant` the second, and there is no further constructor to check. That those two are the only
roles Anthropic accepts is a fact about their API and is not proved anywhere here; what is proved
is that the encoder emits nothing outside them. -/
theorem claude_msgJson_role (m : Msg) :
    (Claude.msgJson m).getObjVal? "role" = .ok (Json.str "user") ∨
      (Claude.msgJson m).getObjVal? "role" = .ok (Json.str "assistant") := by
  cases m
  · exact Or.inl rfl
  · exact Or.inr rfl
  · exact Or.inl rfl

/-- A tool result reaches Claude flagged as a failure exactly when the caller flagged it as one.
A result that silently lost its flag would reach the model as a success carrying error text, which
is the failure mode this rules out.

`id` and `output` are the call identifier and the tool's output; neither affects the outcome, so
the claim holds for every result. `.toOption.isSome` answers `true` exactly when the lookup of
`is_error` succeeds, that is when the key is present at all, and the claim equates that with
`isError`. So a failure carries the key and a success omits it entirely rather than sending
`false`. That an absent `is_error` is read as a success is a fact about the Messages API, not
something proved here. -/
theorem claude_toolResultJson_isError (id output : String) (isError : Bool) :
    ((Claude.toolResultJson id output isError).getObjVal? "is_error").toOption.isSome =
      isError := by
  cases isError <;> rfl

/-- No `Msg` can encode to a role Chat Completions will not accept. As with the Messages API, the
role is fixed vocabulary and a value outside it fails the request rather than one turn.

`m` ranges over all three `Msg` constructors. The claim is that looking `role` up in `msgJson m`
succeeds and finds `"user"`, `"assistant"` or `"tool"`, one per constructor. The assistant case
needs a split because the encoder builds two different objects depending on whether the turn
carries tool calls, and the claim is that both spell the role the same way. That these three are
the only roles OpenAI accepts is a fact about their API rather than anything proved here. -/
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

/-- An assistant turn with no prose encodes its absent body as `null`. Chat Completions rejects
an empty string there, so this is the difference between a tool-calling turn replaying cleanly and
the whole request being refused.

`tcs` is arbitrary, so the claim covers both the branch that emits a `tool_calls` field and the
branch that does not; only the text is fixed, at `""`. The statement is that `content` is present
and holds `Json.null`, not `Json.str ""`. It says nothing about a non-empty `text`, which is sent
as a string and is covered by the round-trip theorems instead. -/
theorem openai_assistant_emptyText_isNull (tcs : Array ToolCall) :
    (OpenAI.msgJson (.assistant "" tcs)).getObjVal? "content" = .ok Json.null := by
  simp only [OpenAI.msgJson]
  split <;> rfl

private def messagesOf (body : Json) : Array Json :=
  (body.getObjVal? "messages" >>= Json.getArr?).toOption.getD #[]

/-- The helper the message-count theorem below is stated in terms of really does read the message
array out of an OpenAI request body. Without this, that theorem could be counting the helper's
fallback rather than the messages the encoder produced, and would hold whether or not they were
there.

`model`, `maxTokens` and `tools` stand for the other three fields, so the claim does not depend on
what any of them holds, and `msgs` is the array under `messages`. `messagesOf` looks `messages`
up, asks for an array, and returns `#[]` if either step fails; the statement is that on this
object neither fails and the result is `msgs` itself. -/
private theorem messagesOf_openai (model maxTokens tools : Json) (msgs : Array Json) :
    messagesOf (Json.mkObj
      [("model", model), ("max_completion_tokens", maxTokens), ("tools", tools),
        ("messages", Json.arr msgs)]) = msgs := rfl

/-- A system prompt costs exactly one extra message and never displaces a turn of history. OpenAI
takes the prompt as a message rather than a field of its own, so a prompt that overwrote the first
turn instead of preceding it would lose conversation silently.

`config` decides whether there is a prompt, `history` is the conversation, and `tools` is the
catalogue, which goes elsewhere in the body and so cannot affect the count. `messagesOf` extracts
the message array, `.size` counts it, and `config.systemPrompt.isSome` is `true` exactly when a
prompt is configured. The claim is that the count is `history.size` plus one in that case and plus
nothing otherwise, which is both that the developer message is added and that no turn is dropped
to make room. `history` may be empty, but the arithmetic is proved for every size. -/
theorem openai_requestBody_messageCount (config : Config) (history : Array Msg)
    (tools : Array Tool) :
    (messagesOf (OpenAI.requestBody config history tools)).size =
      history.size + if config.systemPrompt.isSome then 1 else 0 := by
  simp only [OpenAI.requestBody, messagesOf_openai]
  cases config.systemPrompt <;> simp <;> omega

/-- Converse nests the reply a level deeper than Anthropic does, so the round-trip theorems below
have to rebuild that envelope rather than hand `msgJson`'s output straight back. -/
private def responseOf (message : Json) : Json :=
  Json.mkObj [("output", Json.mkObj [("message", message)])]

/-- Bedrock's response reader reaches through the `output`/`message` envelope to the content
blocks the encoder produced. The Bedrock round-trip theorems wrap an encoded message in that
envelope and read it back, and this is what makes the reading they perform the real one rather
than one on an object the reader never receives.

`r` stands for whatever the `role` field holds, so the claim is independent of the role, and `a`
is the block array under `content`. `Bedrock.contentOf` walks `output`, then `message`, then
`content`, and asks for an array, returning `#[]` if any step fails; the statement is that on this
envelope every step succeeds and the result is `a` itself. -/
private theorem contentOf_responseOf (r : Json) (a : Array Json) :
    Bedrock.contentOf (responseOf (Json.mkObj [("role", r), ("content", Json.arr a)])) = a := rfl

/-- Reading Converse tool calls out of a run of blocks does not depend on where that run is split,
just as it does not for the Messages API. The Bedrock round-trip theorem builds content as a text
block followed by tool-use blocks and needs to take the two halves separately.

`x` and `y` are arbitrary block arrays, well-formed or not. The claim is that
`Bedrock.extractToolCalls` of `x ++ y` equals its result on `x` appended to its result on `y`,
which holds because it is a `filterMap` and so decides each block on that block alone. Neither
array need be non-empty, and since both are arbitrary nothing rests on an empty case. -/
private theorem bedrock_extractToolCalls_append (x y : Array Json) :
    Bedrock.extractToolCalls (x ++ y) =
      Bedrock.extractToolCalls x ++ Bedrock.extractToolCalls y := by
  simp [Bedrock.extractToolCalls, Array.filterMap_append]

/-- Every text block Bedrock sends back survives the encode/decode round trip, concatenated in
order and with the tool-call blocks contributing nothing. As for Claude, the text a caller reads
off `Reply` is exactly what the model wrote, and a turn that also asks for tools cannot corrupt
it.

`text` is the assistant turn's prose and `tcs` the calls it requests. `msgJson` encodes both into
a Converse message, `responseOf` puts it in the envelope a response arrives in, `contentOf` takes
the blocks back out, and `extractText` concatenates the `text` field of every block that has one.
Converse blocks carry no discriminating type, so what makes the tool-use blocks contribute nothing
is that they have no `text` key at all rather than a type test. `text` may be empty, in which case
the encoder emits no text block and the theorem still asserts `""`; the proof splits on that
case. -/
theorem bedrock_extractText_roundTrip (text : String) (tcs : Array ToolCall) :
    Bedrock.extractText (Bedrock.contentOf (responseOf (Bedrock.msgJson (.assistant text tcs)))) =
      text := by
  simp only [Bedrock.msgJson, contentOf_responseOf, Bedrock.extractText]
  rw [Array.foldl_append, foldl_map_eq_init _ _ _ _ (by intro b tc; rfl)]
  split
  · simp_all [String.isEmpty_iff]
  · rfl

/-- Every tool call survives the Converse encode/decode round trip unchanged and in order,
whatever text accompanies it. A caller dispatching on `ToolCall.name` and answering with
`ToolCall.id` is therefore working from what the model actually asked for.

`tcs` are the calls the assistant turn requests and `text` the prose alongside them.
`toolCallJson` writes each call as a `toolUse` block, `responseOf` and `contentOf` carry it
through the response envelope and back, and `extractToolCalls` keeps the blocks with a `toolUse`
key whose `toolUseId` and `name` are strings, rebuilding a `ToolCall` from each. The claim is
equality with `tcs` itself: nothing is dropped by that check, nothing reordered, and the text block
yields none. `tcs` may be empty, but the mapping lemma applied holds at every length. -/
theorem bedrock_extractToolCalls_roundTrip (text : String) (tcs : Array ToolCall) :
    Bedrock.extractToolCalls
      (Bedrock.contentOf (responseOf (Bedrock.msgJson (.assistant text tcs)))) = tcs := by
  simp only [Bedrock.msgJson, contentOf_responseOf]
  rw [bedrock_extractToolCalls_append]
  rw [show Bedrock.extractToolCalls (tcs.map Bedrock.toolCallJson) = tcs from
    filterMap_map_id _ _ _ (fun _ => rfl)]
  split
  · rw [show Bedrock.extractToolCalls (#[] : Array Json) = #[] from rfl]
    simp
  · rw [show Bedrock.extractToolCalls #[Json.mkObj [("text", Json.str text)]] = #[] from rfl]
    simp

/-- No `Msg` can encode to a role Converse will not accept. The role is fixed vocabulary here too,
and a value outside it fails the request rather than one turn of it.

`m` ranges over all three `Msg` constructors. The claim is that looking `role` up in
`Bedrock.msgJson m` succeeds and finds `"user"` or `"assistant"`: `.user` and `.toolResult` give
the first, `.assistant` the second, and there is no further constructor. That Converse accepts
only these two is a fact about the API and is not proved here; what is proved is that the encoder
emits nothing else. -/
theorem bedrock_msgJson_role (m : Msg) :
    (Bedrock.msgJson m).getObjVal? "role" = .ok (Json.str "user") ∨
      (Bedrock.msgJson m).getObjVal? "role" = .ok (Json.str "assistant") := by
  cases m
  · exact Or.inl rfl
  · exact Or.inr rfl
  · exact Or.inl rfl

/-- A tool result reaches Bedrock flagged as a failure exactly when the caller flagged it as one.
Converse spells the flag differently from Anthropic, and a result that lost it would reach the
model as a success carrying error text.

`id` and `output` are the call identifier and the tool's output and `structured` the same output as
JSON where there is one; none of the three affects the outcome. `.toOption.isSome` answers `true`
exactly when the walk to `status` inside the `toolResult` object succeeds, that is when the key is
present at all, and the claim equates that with `isError`. So a failure carries `status` and a
success omits it rather than sending `"success"`. That an absent `status` is read as a success is a
fact about Converse, not something proved here. -/
theorem bedrock_toolResultJson_status (id output : String) (isError : Bool)
    (structured : Option Json) :
    ((Bedrock.toolResultJson id output isError structured).getObjVal? "toolResult"
      >>= (·.getObjVal? "status")).toOption.isSome = isError := by
  cases isError <;> cases structured <;> rfl

/-- A structured tool result reaches Converse as JSON, and one without reaches it as text, with no
case in which it arrives as both or as neither. This is the whole point of carrying `structured`
alongside `output`: a block that fell back to `text` would send the model compressed JSON to read
back out, and one that sent `json` with nothing in it would lose the result altogether.

`id`, `output` and `isError` are the call identifier, the string output and the failure flag; the
first two reach the block only as its contents and the third only as the sibling `status` field, so
none of them decides which key appears. `has p` answers `true` exactly when there is a value at
every step of `p`, so each side reads the first content block of the `toolResult` object and asks
whether it holds the named key. The first conjunct says `json` is there exactly when `structured`
is `some`, and the second that `text` is there exactly when it is `none`; together they say the
block is one or the other and never both. That Converse reads a `json` block as a structured result
is a fact about the API rather than anything proved here. -/
theorem bedrock_toolResultJson_content (id output : String) (isError : Bool)
    (structured : Option Json) :
    (Bedrock.toolResultJson id output isError structured).has
        ["toolResult", "content", Json.Step.index 0, "json"] = structured.isSome ∧
      (Bedrock.toolResultJson id output isError structured).has
        ["toolResult", "content", Json.Step.index 0, "text"] = !structured.isSome := by
  cases structured <;> cases isError <;> exact ⟨rfl, rfl⟩

/-- A request offering no tools carries no `toolConfig` at all, rather than one holding an empty
list. Converse rejects an empty `tools` array, so a request built the obvious way would fail for
every caller who is not using tools.

`config` and `history` are arbitrary and reach the body only through its other fields; the proof
still has to case on `config.systemPrompt` because that decides where `toolConfig` is appended,
not whether it is there. `.toOption.isSome` is `true` exactly when the `toolConfig` lookup
succeeds, and `!tools.isEmpty` is `true` exactly when at least one tool was offered; equating them
says the key is present in one case and absent in the other. That Converse rejects an empty
`tools` array is a fact about the API rather than anything proved here. -/
theorem bedrock_requestBody_toolConfig (config : Bedrock.Config) (history : Array Msg)
    (tools : Array Tool) :
    ((Bedrock.requestBody config history tools).getObjVal? "toolConfig").toOption.isSome =
      !tools.isEmpty := by
  simp only [Bedrock.requestBody]
  cases config.systemPrompt <;> split <;> rename_i h <;> simp only [h] <;> rfl

end LLMClient.Test
