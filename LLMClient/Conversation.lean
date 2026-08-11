-- Copyright (c) 2026 Paul Butcher. All rights reserved.
-- Released under Apache 2.0 license as described in the file LICENSE.

import LLMClient.Provider
import Lean.Data.Json

namespace LLMClient

open Lean (Json)

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

private def go (provider : Provider) (apiKey : String) (tools : Array Tool)
    (runTool : String → Json → IO String) (onProgress : Progress → IO Unit)
    (history : Array Msg) (iterationsLeft : Nat) :
    IO (Except String (Array Msg × String)) := do
  if iterationsLeft == 0 then
    return .error "the model kept calling tools without finishing; giving up."
  onProgress .thinking
  match ← provider.sendRequest apiKey history tools with
  | .error e => return .error e
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
        onProgress (.runningTool tc.name)
        let output ← runTool tc.name tc.input
        history := history.push (Msg.toolResult tc.id output)
      go provider apiKey tools runTool onProgress history (iterationsLeft - 1)
termination_by iterationsLeft
decreasing_by simp_all; omega

/-- Keeps sending until the model stops asking for tools, replies, refuses, or its reply is
truncated, whichever happens first; bounded by `config.maxIterations` so a model that never
settles can't loop forever. `runTool` executes one tool call by name; it's a parameter (rather
than this module knowing how tools are implemented) so this loop's control flow stays independent
of any particular tool set. `config.onProgress` fires before each request (`.thinking`) and before
each tool call (`.runningTool name`). -/
def converseLoop (provider : Provider) (apiKey : String) (tools : Array Tool)
    (runTool : String → Json → IO String) (history : Array Msg) (config : LoopConfig := {}) :
    IO (Except String (Array Msg × String)) :=
  go provider apiKey tools runTool config.onProgress history config.maxIterations

end LLMClient
