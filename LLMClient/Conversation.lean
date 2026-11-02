-- Copyright (c) 2026 Paul Butcher. All rights reserved.
-- Released under Apache 2.0 license as described in the file LICENSE.

module

public import LLMClient.Provider
public import Json

public section

namespace LLMClient

/-- Reported by `converseLoop` before each step of a multi-turn tool-calling exchange, so a
caller can show live status without instrumenting `runTool` itself. -/
inductive Progress where
  | thinking
  | runningTool (name : String)

/-- Tuning knobs for `converseLoop`. `onProgress` defaults to a no-op, so existing call sites
that don't care about progress don't need to change. -/
structure LoopConfig (m : Type → Type) [Pure m] where
  maxIterations : Nat := 5
  onProgress : Progress → m Unit := fun _ => pure ()

/-- A request that failed, and everything the conversation had accumulated by then.

`history` matters because a turn can fail after tools have already run: the model asked, the
tools did what they were asked, and only the round trip carrying their results back failed. Those
calls have changed whatever they change, and a caller that persists conversations needs to record
them or its stored history will disagree with the world.

It is the history including the assistant turn that requested the tools and a result for each,
so it ends mid-exchange. `closeOff` makes it fit to replay. -/
structure LoopFailure where
  message : String
  history : Array Msg

/-- What running a tool call came to: its output and, for a tool that has one, the same answer as
JSON; or the text of its failure. -/
abbrev ToolOutcome := Except String (String × Option Json)

def toolResultOf (call : ToolCall) : ToolOutcome → Msg
  | .ok (output, structured) => .toolResult call.id output false structured
  | .error text => .toolResult call.id text true none

/-- One `.toolResult` per call, in the order given. -/
def appendResults (history : Array Msg) (results : Array (ToolCall × ToolOutcome)) : Array Msg :=
  history ++ results.map fun (call, outcome) => toolResultOf call outcome

private def noSuchTool (call : ToolCall) : ToolOutcome :=
  .error s!"there is no tool called {call.name}"

/-- The answer to a call naming none of `offered`, which is how `converseLoop` answers one without
reaching the caller; `none` when the tool is on offer. -/
def unknownTool? (offered : Array String) (call : ToolCall) : Option ToolOutcome :=
  if offered.contains call.name then none else some (noSuchTool call)

/-- A history about to be sent. Only the step functions build one, so the iteration cap they carry
cannot be lost or reset between round trips. -/
structure Turn where
  private mk ::
  history : Array Msg
  iterationsLeft : Nat

/-- Tool calls waiting on their results. `history` ends on the assistant turn that asked for them. -/
structure Pending where
  private mk ::
  history : Array Msg
  calls : Array ToolCall
  iterationsLeft : Nat

/-- What a caller driving a turn does next. `.done` carries the history and the text `converseLoop`
returns: the model's reply, or a fixed message saying why there is none. -/
inductive Step where
  /-- Send `turn.history`, and pass the reply to `turn.receive`. -/
  | send (turn : Turn)
  /-- Run `pending.calls`, and pass their outcomes to `pending.answer`. -/
  | runTools (pending : Pending)
  | done (history : Array Msg) (text : String)

private def giveUp (history : Array Msg) : Step :=
  let text := "The model kept calling tools without finishing; giving up."
  .done (history.push (.assistant #[.text text])) text

/-- `maxIterations` bounds the round trips that carry tool results, since a model can in principle
keep asking for tools forever. At `0` the turn gives up without sending anything. -/
def Turn.start (history : Array Msg) (maxIterations : Nat := 5) : Step :=
  match maxIterations with
  | 0 => giveUp history
  | n + 1 => .send ⟨history, n + 1⟩

/-- A refusal and a truncation end the turn even when the reply also asks for tools, since a reply
cut off or declined is no request to act on. -/
def Turn.receive (turn : Turn) (reply : Reply) : Step :=
  let history := turn.history.push (.assistant reply.content)
  if reply.isRefusal then
    .done history "The model declined to respond to that."
  else if reply.isTruncated then
    .done history "The model's reply was cut off before it finished; try again."
  else if reply.toolCalls.isEmpty then
    .done history reply.text
  else
    .runTools ⟨history, reply.toolCalls, turn.iterationsLeft⟩

/-- `outcomes` answer `pending.calls` position by position. -/
private def Pending.resume (pending : Pending) (outcomes : Array ToolOutcome) : Step :=
  let history := appendResults pending.history (pending.calls.zip outcomes)
  match pending.iterationsLeft - 1 with
  | 0 => giveUp history
  | n + 1 => .send ⟨history, n + 1⟩

/-- `run` answers each call; `unknownTool?` answers one naming a tool that is not on offer. -/
def Pending.answer (pending : Pending) (run : ToolCall → ToolOutcome) : Step :=
  pending.resume (pending.calls.map run)

private def Step.measure : Step → Nat
  | .send turn => 2 * turn.iterationsLeft + 2
  | .runTools pending => 2 * pending.iterationsLeft + 1
  | .done _ _ => 0

private theorem Turn.receive_measure (turn : Turn) (reply : Reply) :
    (turn.receive reply).measure < (Step.send turn).measure := by
  unfold Turn.receive
  repeat' split
  all_goals simp [Step.measure]

private theorem Pending.resume_measure (pending : Pending) (outcomes : Array ToolOutcome) :
    (pending.resume outcomes).measure < (Step.runTools pending).measure := by
  unfold Pending.resume
  split
  · simp [giveUp, Step.measure]
  · next n h => simp [Step.measure]; omega

/-- Closes off a history a failed turn left ending on tool results, which the APIs here refuse to
continue from because they require roles to alternate. Any other history is returned as it is. -/
def closeOff (history : Array Msg) (text : String) : Array Msg :=
  match history.back? with
  | some (.toolResult ..) => history.push (.assistant #[.text text])
  | _ => history

/-- A tool offering `runStructured` is run through that alone, and the string half of the result is
its JSON compressed, so what the providers that can only carry a string send is the same answer as
what Converse sends as JSON. `.runningTool` is not reported for a tool that is not there, because
nothing runs. -/
private def runCall [Monad m] (tools : Array (ToolImpl m)) (onProgress : Progress → m Unit)
    (tc : ToolCall) : m ToolOutcome :=
  match tools.find? (·.tool.name == tc.name) with
  | some impl => do
    onProgress (.runningTool tc.name)
    match impl.runStructured with
    | some run => return (← run tc.input).map fun j => (j.compress, some j)
    | none => return (← impl.run tc.input).map fun output => (output, none)
  | none => pure (noSuchTool tc)

private def drive [Monad m] (provider : Provider m) (tools : Array (ToolImpl m))
    (onProgress : Progress → m Unit) (step : Step) :
    m (Except LoopFailure (Array Msg × String)) :=
  match step with
  | .done history text => return .ok (history, text)
  | .send turn => do
    onProgress .thinking
    match ← provider.sendRequest turn.history (tools.map (·.tool)) with
    | .error e => return .error { message := e, history := turn.history }
    | .ok reply => drive provider tools onProgress (turn.receive reply)
  | .runTools pending => do
    let outcomes ← pending.calls.mapM (runCall tools onProgress)
    drive provider tools onProgress (pending.resume outcomes)
termination_by step.measure
decreasing_by
  · exact Turn.receive_measure turn reply
  · exact Pending.resume_measure pending outcomes

/-- Keeps sending until the model stops asking for tools, replies, refuses, its reply is
truncated, or `config.maxIterations` is exhausted, whichever happens first. It is the step
functions above driven in `m`, so it follows their rules.

`tools` is what the model is offered and what answers it in one: the catalogue sent with each
request is `tools.map (·.tool)`, and each call the model makes is dispatched to the `run` of the
first entry whose `tool.name` matches, so duplicate names shadow later ones. A call naming a tool
that is not there never reaches the caller: it is answered as `unknownTool?` answers it.

`ToolImpl.run` returning `.error` becomes a `.toolResult` flagged as an error too, with the error
text as its output. An entry with a `runStructured` is dispatched to that instead, and its result
fills in both halves of the `.toolResult`.

`config.onProgress` fires before each request (`.thinking`) and before each tool call
(`.runningTool name`). Every one of those outcomes, including exhausting `maxIterations`, is
returned as `.ok (history, text)`, since real conversation happened and `history` is worth
keeping; `.error` is `provider.sendRequest` itself failing, and carries the history too, for the
reason `LoopFailure` gives. -/
def converseLoop [Monad m] (provider : Provider m) (tools : Array (ToolImpl m))
    (history : Array Msg) (config : LoopConfig m := {}) :
    m (Except LoopFailure (Array Msg × String)) :=
  drive provider tools config.onProgress (Turn.start history config.maxIterations)

end LLMClient
