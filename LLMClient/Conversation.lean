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
structure LoopConfig where
  maxIterations : Nat := 5
  onProgress : Progress → IO Unit := fun _ => pure ()

/-- A request that failed, and everything the conversation had accumulated by then.

`history` matters because a turn can fail after tools have already run: the model asked, the
tools did what they were asked, and only the round trip carrying their results back failed. Those
calls have changed whatever they change, and a caller that persists conversations needs to record
them or its stored history will disagree with the world.

It is the history including the assistant turn that requested the tools and a result for each,
so it ends mid-exchange. A caller replaying it to a model has to close it off; what that means
depends on the API, and for the ones here it means an assistant turn, since they require roles to
alternate. -/
structure LoopFailure where
  message : String
  history : Array Msg

/-- A call naming a tool that isn't on offer is answered here rather than passed on, since there
is nothing to pass it to. `.runningTool` is not reported for it, because nothing runs.

A tool offering `runStructured` is run through that alone, and the string half of the result is
its JSON compressed, so what the providers that can only carry a string send is the same answer as
what Converse sends as JSON. -/
private def runCall (tools : Array ToolImpl) (onProgress : Progress → IO Unit) (tc : ToolCall) :
    IO (Except String (String × Option Json)) :=
  match tools.find? (·.tool.name == tc.name) with
  | some impl => do
    onProgress (.runningTool tc.name)
    match impl.runStructured with
    | some run => return (← run tc.input).map fun j => (j.compress, some j)
    | none => return (← impl.run tc.input).map fun output => (output, none)
  | none => pure (.error s!"there is no tool called {tc.name}")

private def go (provider : Provider) (tools : Array ToolImpl) (onProgress : Progress → IO Unit)
    (history : Array Msg) (iterationsLeft : Nat) :
    IO (Except LoopFailure (Array Msg × String)) := do
  if iterationsLeft == 0 then
    let text := "The model kept calling tools without finishing; giving up."
    return .ok (history.push (Msg.assistant text), text)
  onProgress .thinking
  match ← provider.sendRequest history (tools.map (·.tool)) with
  | .error e => return .error { message := e, history }
  | .ok resp =>
    let history := history.push (Msg.assistant resp.text resp.toolCalls)
    if resp.isRefusal then
      return .ok (history, "The model declined to respond to that.")
    else if resp.isTruncated then
      return .ok (history, "The model's reply was cut off before it finished; try again.")
    else if resp.toolCalls.isEmpty then
      return .ok (history, resp.text)
    else
      let mut history := history
      for tc in resp.toolCalls do
        let (output, isError, structured) ← match ← runCall tools onProgress tc with
          | .ok (output, structured) => pure (output, false, structured)
          | .error text => pure (text, true, none)
        history := history.push (Msg.toolResult tc.id output isError structured)
      go provider tools onProgress history (iterationsLeft - 1)
termination_by iterationsLeft
decreasing_by simp_all; omega

/-- Keeps sending until the model stops asking for tools, replies, refuses, its reply is
truncated, or `config.maxIterations` is exhausted, whichever happens first.

`tools` is what the model is offered and what answers it in one: the catalogue sent with each
request is `tools.map (·.tool)`, and each call the model makes is dispatched to the `run` of the
first entry whose `tool.name` matches, so duplicate names shadow later ones. A call naming a tool
that is not there never reaches the caller: the loop records a `.toolResult` for it flagged as an
error, saying the tool does not exist, and carries on.

`ToolImpl.run` returning `.error` becomes a `.toolResult` flagged as an error too, with the error
text as its output. An entry with a `runStructured` is dispatched to that instead, and its result
fills in both halves of the `.toolResult`.

`config.onProgress` fires before each request (`.thinking`) and before each tool call
(`.runningTool name`). Every one of those outcomes, including exhausting `maxIterations`, is
returned as `.ok (history, text)`, since real conversation happened and `history` is worth
keeping; `.error` is `provider.sendRequest` itself failing, and carries the history too, for the
reason `LoopFailure` gives. -/
def converseLoop (provider : Provider) (tools : Array ToolImpl) (history : Array Msg)
    (config : LoopConfig := {}) : IO (Except LoopFailure (Array Msg × String)) :=
  go provider tools config.onProgress history config.maxIterations

end LLMClient
