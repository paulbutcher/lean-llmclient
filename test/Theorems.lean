-- Copyright (c) 2026 Paul Butcher. All rights reserved.
-- Released under Apache 2.0 license as described in the file LICENSE.

import LLMClient

namespace LLMClient.Test

private def isEmptyText : Block → Bool
  | .text t => t.isEmpty
  | _ => false

/-- One block written for the Messages API and read back is the block it was, unless it is an empty
text block, which the encoder omits. The round trip of a whole turn below is this, block by block.

`b` ranges over every `Block`. `Claude.blockJson` answers `none` for a block it omits and the JSON
otherwise, and `Option.bind` feeds that JSON to `Claude.readBlock`, so the left side is what
reading back the encoding yields. `isEmptyText` answers `true` only for a text block holding the
empty string, and the claim is that exactly those come back as `none` and every other block as
itself, signatures and tool inputs included. -/
private theorem claude_block_roundTrip (b : Block) :
    (Claude.blockJson b).bind Claude.readBlock = if isEmptyText b then none else some b := by
  cases b with
  | text t =>
    by_cases h : t.isEmpty
    · simp [Claude.blockJson, isEmptyText, h]
    · simp only [Claude.blockJson, isEmptyText, h]
      show (if t.isEmpty = true then none else some (Block.text t)) = _
      simp [h]
  | toolCall tc => cases tc; rfl
  | thinking => rfl
  | redactedThinking => rfl

/-- An assistant turn written for the Messages API and read back as a response is the turn it was,
block for block and in order, less any empty text block. Claude checks that a replayed thinking
block follows exactly what preceded it, so a turn that changed on the way through, its text merged
or its blocks reordered, would cost the reasoning that follows it or fail the request outright.

`content` is any sequence of blocks. `msgJson` encodes it as an assistant message, `contentOf`
takes the block array back out of an object of that shape, which is how a response carries it,
and `extractContent` reads each block. The claim is that this yields `content` with only the empty
text blocks removed, which `filter` does without disturbing the order of the rest. That the API
treats a turn so replayed as unchanged is a fact about the API, not something proved here. -/
theorem claude_content_roundTrip (content : Array Block) :
    Claude.extractContent (Claude.contentOf (Claude.msgJson (.assistant content))) =
      content.filter (!isEmptyText ·) := by
  show (content.filterMap Claude.blockJson).filterMap Claude.readBlock = _
  rw [Array.filterMap_filterMap]
  simp only [claude_block_roundTrip]
  rw [← Array.filterMap_eq_filter]
  congr 1
  all_goals first | rfl | (funext b; cases h : isEmptyText b <;> simp [Option.guard, h])

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

`content` is any sequence of blocks whose text blocks join to the empty string, which covers a
turn of tool calls alone, one of thinking alone, and one with nothing in it. The claim covers both
the branch that emits a `tool_calls` field and the branch that does not: `content` is present and
holds `Json.null`, not `Json.str ""`. The hypothesis is met by exactly those turns, so it does not
make the claim vacuous. -/
theorem openai_assistant_emptyText_isNull (content : Array Block)
    (h : Block.joinText content = "") :
    (OpenAI.msgJson (.assistant content)).getObjVal? "content" = .ok Json.null := by
  simp only [OpenAI.msgJson, h]
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

/-- Converse nests the reply a level deeper than Anthropic does, so the round-trip theorem below
has to rebuild that envelope rather than hand `msgJson`'s output straight back. -/
private def responseOf (message : Json) : Json :=
  Json.mkObj [("output", Json.mkObj [("message", message)])]

private def onConverse : Block → Bool
  | .text t => !t.isEmpty
  | .toolCall _ => true
  | _ => false

/-- One block written for Converse and read back is the block it was, when Converse carries it at
all. The round trip of a whole turn below is this, block by block.

`b` ranges over every `Block`. `Bedrock.blockJson` answers `none` for a block it omits and the JSON
otherwise, and `Option.bind` feeds that JSON to `Bedrock.readBlock`. `onConverse` answers `true`
for a non-empty text block and for a tool call, and the claim is that those come back as
themselves and every other block, thinking and empty text, as `none`. -/
private theorem bedrock_block_roundTrip (b : Block) :
    (Bedrock.blockJson b).bind Bedrock.readBlock = if onConverse b then some b else none := by
  cases b with
  | text t =>
    by_cases h : t.isEmpty
    · simp [Bedrock.blockJson, onConverse, h]
    · simp only [Bedrock.blockJson, onConverse, h]
      show (if t.isEmpty = true then none else some (Block.text t)) = _
      simp [h]
  | toolCall tc => cases tc; rfl
  | thinking => rfl
  | redactedThinking => rfl

/-- An assistant turn written for Converse and read back as a response keeps its text and its tool
calls, unchanged and in order. A caller dispatching on `ToolCall.name` and answering with
`ToolCall.id` is therefore working from what the model asked for, and the prose it reads is what
the model wrote.

`content` is any sequence of blocks. `msgJson` encodes it, `responseOf` puts it in the envelope a
response arrives in, `contentOf` takes the blocks back out and `extractContent` reads each one.
The claim is that this yields `content` filtered to what `onConverse` accepts: non-empty text and
tool calls, in their original order. Thinking is dropped by design, since Converse carries
reasoning in a shape of its own. -/
theorem bedrock_content_roundTrip (content : Array Block) :
    Bedrock.extractContent (Bedrock.contentOf (responseOf (Bedrock.msgJson (.assistant content)))) =
      content.filter onConverse := by
  show (content.filterMap Bedrock.blockJson).filterMap Bedrock.readBlock = _
  rw [Array.filterMap_filterMap]
  simp only [bedrock_block_roundTrip]
  rw [← Array.filterMap_eq_filter]
  congr 1
  all_goals first | rfl | (funext b; cases h : onConverse b <;> simp [Option.guard, h])

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

/-- A Messages API response is read as a reply only when its status says the request succeeded.
An error response still has a JSON body, often one that `contentOf` and friends read as an empty
reply without complaint, so without this a rejected request would reach the caller as a model
that said nothing.

`status` and `body` are whatever the transport handed back, with no constraint on either. The
hypothesis is that `Claude.parseReply` answered `.ok` with some reply `r`; the conclusion is that
`status` lies in the 2xx range. Read contrapositively, every other status yields `.error`, whatever
the body holds. -/
theorem claude_parseReply_ok_only_2xx (status : UInt32) (body : ByteArray) (r : Reply)
    (h : Claude.parseReply status body = .ok r) : status ≥ 200 ∧ status < 300 := by
  unfold Claude.parseReply at h
  split at h
  · simp_all
  · cases h

/-- A Chat Completions response is read as a reply only when its status says the request
succeeded. An error body has no `choices`, which `messageOf` reads as an empty message without
complaint, so without this a rejected request would reach the caller as a model that said nothing.

`status` and `body` are whatever the transport handed back, with no constraint on either. The
hypothesis is that `OpenAI.parseReply` answered `.ok` with some reply `r`; the conclusion is that
`status` lies in the 2xx range. Read contrapositively, every other status yields `.error`, whatever
the body holds. -/
theorem openai_parseReply_ok_only_2xx (status : UInt32) (body : ByteArray) (r : Reply)
    (h : OpenAI.parseReply status body = .ok r) : status ≥ 200 ∧ status < 300 := by
  unfold OpenAI.parseReply at h
  split at h
  · simp_all
  · cases h

/-- A Converse response is read as a reply only when its status says the request succeeded. An
error body has no `output`, which `contentOf` reads as no content blocks without complaint, so
without this a rejected request, a failed signature included, would reach the caller as a model
that said nothing.

`status` and `body` are whatever the transport handed back, with no constraint on either. The
hypothesis is that `Bedrock.parseReply` answered `.ok` with some reply `r`; the conclusion is that
`status` lies in the 2xx range. Read contrapositively, every other status yields `.error`, whatever
the body holds. -/
theorem bedrock_parseReply_ok_only_2xx (status : UInt32) (body : ByteArray) (r : Reply)
    (h : Bedrock.parseReply status body = .ok r) : status ≥ 200 ∧ status < 300 := by
  unfold Bedrock.parseReply at h
  split at h
  · simp_all
  · cases h

private def resultId? : Msg → Option String
  | .toolResult id .. => some id
  | _ => none

/-- Answering a turn's tool calls adds exactly one tool result per call, in the order of the calls,
each carrying its call's id. The APIs require a result for every call a turn makes, matched by id,
so a result dropped, duplicated or misattributed fails the next request or tells the model
something about the wrong call.

`history` is what came before and `results` pairs each call with its outcome. The claim is that
`appendResults` leaves `history` as it was and appends some array `added`, and that mapping
`resultId?` over `added` gives `some` of each call's id in turn. `resultId?` answers `some` only
for a `.toolResult`, and two arrays are equal under `map` only when they are the same length, so
every appended message is a tool result and there is one per call. -/
theorem appendResults_ids (history : Array Msg) (results : Array (ToolCall × ToolOutcome)) :
    ∃ added, appendResults history results = history ++ added ∧
      added.map resultId? = results.map (fun r => some r.1.id) := by
  refine ⟨_, rfl, ?_⟩
  rw [Array.map_map]
  congr 1; funext ⟨c, o⟩
  rcases o with _ | ⟨_, _⟩ <;> rfl

private def resultIsError? : Msg → Option Bool
  | .toolResult _ _ isError _ => some isError
  | _ => none

private def failed : ToolOutcome → Bool
  | .error _ => true
  | .ok _ => false

/-- A tool result is flagged as an error exactly when its call failed. A failure that lost its
flag would reach the model as a success carrying error text, and a success flagged as a failure
would have the model retry what already worked.

`history` and `results` are as for the theorem above, and `added` again is what is appended.
`resultIsError?` reads the flag off a `.toolResult`, and `failed` answers `true` for an outcome
that is `.error`. The claim is that the flags of `added` are the failures of `results`, position
by position, so neither a failure nor a success can come out the other way. -/
theorem appendResults_isError (history : Array Msg) (results : Array (ToolCall × ToolOutcome)) :
    ∃ added, appendResults history results = history ++ added ∧
      added.map resultIsError? = results.map (fun r => some (failed r.2)) := by
  refine ⟨_, rfl, ?_⟩
  rw [Array.map_map]
  congr 1; funext ⟨c, o⟩
  rcases o with _ | ⟨_, _⟩ <;> rfl

/-- A reply that asks for no tools, or that the model refused, or that was cut off, ends the turn.
Running tools on a refusal or a truncated reply would act on a request the model did not finish
making, and a reply without tool calls has nothing left to run.

`turn` is any turn and `reply` any reply meeting the hypothesis: its tool calls are empty, or
`isRefusal` or `isTruncated` is set. The claim is that `Turn.receive` answers `.done` with some
history and text, so neither `.runTools` nor `.send`. The hypothesis is a disjunction a reply
meets in any of three independent ways, none of which is impossible. -/
theorem receive_ends (turn : Turn) (reply : Reply)
    (h : reply.toolCalls.isEmpty ∨ reply.isRefusal ∨ reply.isTruncated) :
    ∃ history text, turn.receive reply = .done history text := by
  unfold Turn.receive
  repeat' split
  all_goals first | exact ⟨_, _, rfl⟩ | simp_all

/-- The outcomes `Pending.answerWith` hands on, written as its fold writes them. -/
private def foldOutcomes (run : σ → ToolCall → σ × ToolOutcome) (s : σ) (calls : Array ToolCall) :
    σ × Array ToolOutcome :=
  calls.foldl (init := (s, #[])) fun (s, outcomes) call =>
    let (s, outcome) := run s call
    (s, outcomes.push outcome)

private theorem foldOutcomes_stateless (f : ToolCall → ToolOutcome) (calls : List ToolCall)
    (acc : Array ToolOutcome) :
    (calls.foldl (init := ((), acc)) fun ((), outcomes) call => ((), outcomes.push (f call))).2 =
      acc ++ (calls.map f).toArray := by
  induction calls generalizing acc with
  | nil => simp
  | cons c cs ih => simp [ih]

private theorem foldOutcomes_state (run : σ → ToolCall → σ × ToolOutcome) (calls : List ToolCall)
    (s : σ) (acc : Array ToolOutcome) :
    (calls.foldl (init := (s, acc)) fun (s, outcomes) call =>
        let (s, outcome) := run s call
        (s, outcomes.push outcome)).1 =
      calls.foldl (fun s call => (run s call).1) s := by
  induction calls generalizing s acc with
  | nil => rfl
  | cons c cs ih => simp only [List.foldl_cons]; exact ih ..

/-- Answering tool calls with state moves the turn on exactly as answering them without state
would, given the same outcomes. So the iteration cap, giving up when it runs out, and the tool
results `appendResults_ids` and `appendResults_isError` describe all hold for `answerWith` as they
do for `answer`, and a caller choosing between the two cannot change how the turn proceeds.

`run` is any stateful runner starting from `s`, and `foldOutcomes run s pending.calls` is the state
and outcomes its fold over the calls produced. `f` is any stateless runner which, mapped over the
same calls, gives those outcomes; the claim is that the step `answerWith` returns is the step
`pending.answer f` returns. Such an `f` exists whenever no two calls are equal values, which call
ids make so when a provider issues each call in a reply its own id; nothing here proves that a
provider does, and for a reply that repeats a call exactly with differing outcomes the claim is
vacuous. -/
theorem answerWith_step (pending : Pending) (run : σ → ToolCall → σ × ToolOutcome) (s : σ)
    (f : ToolCall → ToolOutcome)
    (h : pending.calls.map f = (foldOutcomes run s pending.calls).2) :
    (pending.answerWith run s).2 = pending.answer f := by
  have hf : (pending.calls.foldl (init := ((), #[]))
      fun ((), outcomes) call => ((), outcomes.push (f call))).2 = pending.calls.map f := by
    rw [← Array.foldl_toList, foldOutcomes_stateless]; exact Array.ext' (by simp)
  simp only [Pending.answer, Pending.answerWith]
  rw [hf] at *
  rw [h]; rfl

/-- The state `answerWith` returns is what running each call once, in order, makes of the state it
started from. A caller keeping its model in that state, such as a todo list, sees a call's effect
in every call after it, and sees no call skipped, repeated or reordered.

`run` takes a state and a call and answers the next state with the call's outcome, and `s` is the
state before the first call. The right side folds `run`'s state half over `pending.calls` from the
left, so it is the state after the first call fed to the second and so on to the last; the claim
is that this is the state `answerWith` returns. -/
theorem answerWith_state (pending : Pending) (run : σ → ToolCall → σ × ToolOutcome) (s : σ) :
    (pending.answerWith run s).1 = pending.calls.foldl (fun s call => (run s call).1) s := by
  simp only [Pending.answerWith]
  rw [← Array.foldl_toList, ← Array.foldl_toList, foldOutcomes_state]

/-- A closed-off history never ends on a tool result. The APIs require roles to alternate, so a
history ending on a tool result cannot be continued with a user turn; this is what makes it safe
to replay.

`history` is any history and `text` the closing assistant turn's text. `back?` is the last message,
or `none` for an empty history. The claim is that for every id, output, flag and structured value,
the last message of the result is not that tool result, which is to say it is not a tool result
at all. -/
theorem closeOff_end (history : Array Msg) (text : String) :
    ∀ id output isError structured,
      (closeOff history text).back? ≠ some (.toolResult id output isError structured) := by
  intro id output isError structured
  unfold closeOff
  split
  · simp
  · next h => exact fun e => h id output isError structured e

/-- Closing off a history only ever appends to it. Every message in it happened, its tool results
included, and a closing step that dropped or rewrote one would lose a record of what was done.

`history` and `text` are as above. The claim is that the result is `history` followed by some
`suffix`, so every message of `history` survives in place. -/
theorem closeOff_prefix (history : Array Msg) (text : String) :
    ∃ suffix, closeOff history text = history ++ suffix := by
  unfold closeOff
  split
  · exact ⟨#[_], rfl⟩
  · exact ⟨#[], by simp⟩

/-- A history that does not end on a tool result is left exactly as it is. Closing one off would
add an assistant turn after an assistant turn, which the APIs reject just as they reject a history
left ending on tool results.

`history` meets the hypothesis that its last message is no tool result, whatever its fields, and
an empty history meets it too. The claim is that `closeOff` returns `history` itself. -/
theorem closeOff_id (history : Array Msg) (text : String)
    (h : ∀ id output isError structured,
      history.back? ≠ some (.toolResult id output isError structured)) :
    closeOff history text = history := by
  unfold closeOff
  split
  · next e => exact absurd e (h _ _ _ _)
  · rfl

/-- A block encoded for storage and decoded again is the block it was. The message round trip
below rests on it for the content of an assistant turn.

`b` ranges over every `Block`. `Block.toJson` writes it and `Block.fromJson?` reads it, and the
claim is `.ok b`, every field included, a tool call's arbitrary `input` among them. -/
private theorem block_roundTrip (b : Block) : Block.fromJson? (Block.toJson b) = .ok b := by
  cases b with
  | toolCall tc => cases tc; rfl
  | _ => rfl

/-- Any message encoded for storage and decoded again is the message it was. A caller that keeps a
conversation and replays it later depends on getting back what it stored, thinking signatures,
error flags and structured results included, since a replayed turn that differs from the original
changes what the model is told.

`m` ranges over every `Msg`, with no side condition: `Msg.toJson` writes it and `Msg.fromJson?`
reads it, and the claim is `.ok m`. That covers a `structured` of `some Json.null`, which a codec
writing `none` as `null` would read back as `none`. The claim is about `Json` values; that text
printed with `compress` parses back to the same value is lean-json's to establish, not proved
here. -/
theorem msg_roundTrip (m : Msg) : Msg.fromJson? (Msg.toJson m) = .ok m := by
  cases m with
  | user => rfl
  | assistant content =>
    show (do let bs ← (Array.map Block.toJson content).mapM Block.fromJson?
             pure (Msg.assistant bs)) = _
    rw [Array.mapM_map]
    simp only [Function.comp_def, block_roundTrip]
    have : content.mapM (m := Except String) (fun x => pure x) = pure content := by
      simpa using Array.mapM_pure (m := Except String) (f := id) (xs := content)
    show Msg.assistant <$> content.mapM (m := Except String) (fun x => pure x) = _
    rw [this]; rfl
  | toolResult id output isError structured => cases isError <;> cases structured <;> rfl

end LLMClient.Test
