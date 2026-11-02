-- Copyright (c) 2026 Paul Butcher. All rights reserved.
-- Released under Apache 2.0 license as described in the file LICENSE.

import LLMClient.Core
import LLMClient.Fetch

open LLMClient Lasm.JS

structure Results where
  failures : Array String := #[]

def Results.check (r : Results) (name : String) (cond : Bool) : Results :=
  if cond then r else { r with failures := r.failures.push name }

def Results.report (r : Results) : IO Bool := do
  if r.failures.isEmpty then
    IO.println "All Lasm tests passed."
    return true
  else
    IO.println s!"{r.failures.size} Lasm test(s) failed:"
    for name in r.failures do
      IO.println s!"  - {name}"
    return false

def mentions (haystack needle : String) : Bool := (haystack.splitOn needle).length > 1

def config (url : String) : Config :=
  { Claude.defaultConfig with apiUrl := url, extraHeaders := [("x-test", "1")] }

def history : Array Msg := #[.user "ping"]

def ask (url : String) : Async (Except String Reply) :=
  (Claude.providerWith Fetch.transport "key" (config url)).sendRequest history

def failsMentioning (result : Except String Reply) (needle : String) : Bool :=
  match result with
  | .error e => mentions e needle
  | .ok _ => false

def echoTool : ToolImpl Async :=
  { tool := { name := "echo_tool", description := "Echoes its word", schema := Json.mkObj [] }
    run := fun input => pure ((input.getObjVal? "word" >>= Json.getStr?).mapError (·)) }

/-- `base` is the server standing in for the Claude API and `dead` a URL nothing answers.
`enforcesCors` is whether the runtime refuses a cross-origin response the server has not allowed:
a browser does, Node does not. -/
def runTests (base dead : String) (enforcesCors : Bool) : Async Results := do
  let mut r : Results := {}

  let echoed : Option Json := match ← ask s!"{base}/echo" with
    | .ok reply => (Json.parse reply.text).toOption
    | .error _ => none
  let echoedHeader (name : String) : Option String :=
    echoed.bind fun j => (j.getObjVal? "headers" >>= (·.getObjVal? name) >>= Json.getStr?).toOption
  r := r
    |>.check "the provider's own headers arrive" (echoedHeader "x-api-key" == some "key")
    |>.check "extraHeaders arrive" (echoedHeader "x-test" == some "1")
    |>.check "the body arrives byte for byte"
      ((echoed.bind fun j => (j.getObjVal? "body" >>= Json.getStr?).toOption) ==
        some (Claude.requestBody (config s!"{base}/echo") history).compress)

  let large ← ask s!"{base}/large"
  r := r.check "a response far larger than one read arrives whole"
    (match large with
      | .ok reply => reply.text.length == 2 ^ 20 + 3 && reply.text.startsWith "éé" &&
          reply.text.endsWith "end"
      | .error _ => false)

  let gateway ← ask s!"{base}/html-error"
  r := r.check "an error status with an HTML body reports both"
    (failsMentioning gateway "(502)" && failsMentioning gateway "Bad Gateway")

  let refused ← ask dead
  r := r.check "a refused connection is an error, not an exception"
    (failsMentioning refused "request to the Claude API failed")

  if enforcesCors then
    let disallowed ← ask s!"{base}/no-cors"
    r := r.check "a response the server has not allowed cross-origin is an error"
      (failsMentioning disallowed "request to the Claude API failed")

  let looped ← converseLoop (Claude.providerWith Fetch.transport "key" (config s!"{base}/tool"))
    #[echoTool] history
  r := r.check "converseLoop runs a tool in Async and sends its result back"
    (match looped with
      | .ok (_, text) => text == "result:hi"
      | .error _ => false)

  return r
