-- Copyright (c) 2026 Paul Butcher. All rights reserved.
-- Released under Apache 2.0 license as described in the file LICENSE.

import Lean.Data.Json
import LLMClient.OpenAI
import LLMClient.Claude
import LLMClient.Bedrock
import LLMClient.Conversation
import Theorems

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

def sampleHistory : Array LLMClient.Msg := #[.user "hi"]

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
  let lengthResp := Json.mkObj
    [ ("choices", Json.arr #[Json.mkObj [("finish_reason", "length")]]) ]
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
    |>.check "finishReasonOf length response"
      (finishReasonOf lengthResp == "length")
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

def testOpenAISystemPrompt (r : Results) : Results :=
  open LLMClient.OpenAI in
  let cfgNone := defaultConfig
  let cfgSome := { defaultConfig with systemPrompt := some "respond in GitHub Flavored Markdown" }
  let bodyNone := requestBody cfgNone sampleHistory
  let bodySome := requestBody cfgSome sampleHistory
  let messagesOf (body : Json) : Array Json :=
    (body.getObjVal? "messages" >>= Json.getArr?).toOption.getD #[]
  r
    |>.check "requestBody with no systemPrompt matches the body built without a developer message"
      (bodyNone ==
        Json.mkObj
          [ ("model", cfgNone.model),
            ("max_completion_tokens", cfgNone.maxOutputTokens),
            ("tools", Json.arr #[]),
            ("messages", Json.arr (sampleHistory.map msgJson)) ])
    |>.check "requestBody with systemPrompt prepends a developer-role message with the prompt"
      ((messagesOf bodySome).getD 0 (Json.mkObj []) ==
        Json.mkObj [("role", "developer"), ("content", "respond in GitHub Flavored Markdown")])
    |>.check "requestBody with systemPrompt leaves the rest of the history untouched"
      ((messagesOf bodySome).extract 1 (messagesOf bodySome).size == sampleHistory.map msgJson)
    |>.check "requestBody with systemPrompt adds exactly one extra message"
      ((messagesOf bodySome).size == (messagesOf bodyNone).size + 1)

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
  let maxTokensResp := Json.mkObj [("stop_reason", "max_tokens")]
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
    |>.check "stopReasonOf max_tokens"
      (stopReasonOf maxTokensResp == "max_tokens")
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

def testClaudeSystemPrompt (r : Results) : Results :=
  open LLMClient.Claude in
  let cfgNone := defaultConfig
  let cfgSome := { defaultConfig with systemPrompt := some "respond in GitHub Flavored Markdown" }
  let bodyNone := requestBody cfgNone sampleHistory
  let bodySome := requestBody cfgSome sampleHistory
  r
    |>.check "requestBody with no systemPrompt matches the body built without a system field"
      (bodyNone ==
        Json.mkObj
          [ ("model", cfgNone.model),
            ("max_tokens", cfgNone.maxOutputTokens),
            ("tools", Json.arr #[]),
            ("messages", Json.arr (sampleHistory.map msgJson)) ])
    |>.check "requestBody with systemPrompt sets a top-level system string"
      ((bodySome.getObjVal? "system" >>= Json.getStr?).toOption ==
        some "respond in GitHub Flavored Markdown")
    |>.check "requestBody with systemPrompt leaves messages untouched"
      ((bodySome.getObjVal? "messages").toOption == (bodyNone.getObjVal? "messages").toOption)

def testConverseLoop (r : Results) : IO Results := do
  -- No config is passed here, so the default `LoopConfig` (including its no-op `onProgress`)
  -- is exercised too.
  let provider1 : LLMClient.Provider :=
    { name := "fake"
      sendRequest := fun _ _ => pure (.ok ({ text := "hello", toolCalls := #[] } : LLMClient.Reply)) }
  let runToolUnused : String → Json → IO String := fun _ _ => pure "should not be called"
  let result1 ← LLMClient.converseLoop provider1 #[] runToolUnused #[LLMClient.Msg.user "hi"]
  let r := r.check "converseLoop: no tool calls returns immediately (default LoopConfig)"
    (match result1 with
      | .ok (h, t) => h == #[LLMClient.Msg.user "hi", LLMClient.Msg.assistant "hello" #[]] && t == "hello"
      | .error _ => false)

  let callCount ← IO.mkRef (0 : Nat)
  let toolCall : LLMClient.ToolCall :=
    { id := "call_1", name := "get_weather", input := Json.mkObj [("city", "Paris")] }
  let provider2 : LLMClient.Provider :=
    { name := "fake"
      sendRequest := fun _ _ => do
        let n ← callCount.get
        callCount.set (n + 1)
        if n == 0 then
          pure (.ok ({ text := "", toolCalls := #[toolCall] } : LLMClient.Reply))
        else
          pure (.ok ({ text := "final answer", toolCalls := #[] } : LLMClient.Reply)) }
  let toolCallsSeen ← IO.mkRef (#[] : Array (String × Json))
  let runTool2 : String → Json → IO String := fun name input => do
    toolCallsSeen.modify (·.push (name, input))
    pure "15°C, cloudy"
  let progressLog ← IO.mkRef (#[] : Array LLMClient.Progress)
  let config2 : LLMClient.LoopConfig := { onProgress := fun p => progressLog.modify (·.push p) }
  let result2 ← LLMClient.converseLoop provider2 #[] runTool2 #[LLMClient.Msg.user "weather?"] config2
  let expectedHistory2 : Array LLMClient.Msg :=
    #[ LLMClient.Msg.user "weather?",
       LLMClient.Msg.assistant "" #[toolCall],
       LLMClient.Msg.toolResult "call_1" "15°C, cloudy",
       LLMClient.Msg.assistant "final answer" #[] ]
  let r := r.check "converseLoop: tool round trip ends with the final text and a toolResult in history"
    (match result2 with
      | .ok (h, t) => h == expectedHistory2 && t == "final answer"
      | .error _ => false)
  let toolCallsSeen ← toolCallsSeen.get
  let r := r.check "converseLoop: runTool is invoked with the model's requested name/input"
    (toolCallsSeen == #[("get_weather", Json.mkObj [("city", "Paris")])])
  let progressSeen ← progressLog.get
  let r := r.check "converseLoop: onProgress fires thinking, runningTool, thinking"
    (match progressSeen.toList with
      | [LLMClient.Progress.thinking, LLMClient.Progress.runningTool "get_weather", LLMClient.Progress.thinking] => true
      | _ => false)

  let refusalToolCalled ← IO.mkRef false
  let providerRefusal : LLMClient.Provider :=
    { name := "fake"
      sendRequest := fun _ _ =>
        pure (.ok ({ text := "", toolCalls := #[], isRefusal := true } : LLMClient.Reply)) }
  let runToolRefusal : String → Json → IO String := fun _ _ => refusalToolCalled.set true *> pure "unused"
  let resultRefusal ← LLMClient.converseLoop providerRefusal #[] runToolRefusal #[LLMClient.Msg.user "hi"]
  let r := r.check "converseLoop: isRefusal stops with the fixed decline message"
    (match resultRefusal with
      | .ok (_, t) => t == "The model declined to respond to that."
      | .error _ => false)
  let refusalToolCalled ← refusalToolCalled.get
  let r := r.check "converseLoop: isRefusal does not invoke runTool" (!refusalToolCalled)

  let truncatedToolCalled ← IO.mkRef false
  let providerTruncated : LLMClient.Provider :=
    { name := "fake"
      sendRequest := fun _ _ =>
        pure (.ok ({ text := "", toolCalls := #[], isTruncated := true } : LLMClient.Reply)) }
  let runToolTruncated : String → Json → IO String := fun _ _ => truncatedToolCalled.set true *> pure "unused"
  let resultTruncated ← LLMClient.converseLoop providerTruncated #[] runToolTruncated #[LLMClient.Msg.user "hi"]
  let r := r.check "converseLoop: isTruncated stops with the fixed cut-off message"
    (match resultTruncated with
      | .ok (_, t) => t == "The model's reply was cut off before it finished; try again."
      | .error _ => false)
  let truncatedToolCalled ← truncatedToolCalled.get
  let r := r.check "converseLoop: isTruncated does not invoke runTool" (!truncatedToolCalled)

  -- maxIterations := 0 gives up before ever calling the provider, so there's no history to
  -- preserve; this is the one exhaustion shape that still stops immediately.
  let exhaustedProviderCalled ← IO.mkRef false
  let providerExhausted : LLMClient.Provider :=
    { name := "fake"
      sendRequest := fun _ _ =>
        exhaustedProviderCalled.set true *>
          pure (.ok ({ text := "hello", toolCalls := #[] } : LLMClient.Reply)) }
  let resultExhausted ←
    LLMClient.converseLoop providerExhausted #[] runToolUnused #[LLMClient.Msg.user "hi"]
      ({ maxIterations := 0 } : LLMClient.LoopConfig)
  let giveUpText := "The model kept calling tools without finishing; giving up."
  let r := r.check "converseLoop: maxIterations := 0 gives up as .ok with the giving-up message"
    (match resultExhausted with
      | .ok (h, t) => h == #[LLMClient.Msg.user "hi", LLMClient.Msg.assistant giveUpText] && t == giveUpText
      | .error _ => false)
  let exhaustedProviderCalled ← exhaustedProviderCalled.get
  let r := r.check "converseLoop: exhausting maxIterations := 0 never calls the provider" (!exhaustedProviderCalled)

  -- A provider that always requests a tool call runs to the iteration budget, then gives up
  -- with .ok, preserving every round's real tool call/result plus the giving-up message.
  let alwaysToolCallCount ← IO.mkRef (0 : Nat)
  let providerAlwaysToolCall : LLMClient.Provider :=
    { name := "fake"
      sendRequest := fun _ _ => do
        let n ← alwaysToolCallCount.get
        alwaysToolCallCount.set (n + 1)
        let tc : LLMClient.ToolCall := { id := s!"call_{n}", name := "get_weather", input := Json.mkObj [("city", "Paris")] }
        pure (.ok ({ text := "", toolCalls := #[tc] } : LLMClient.Reply)) }
  let runToolAlwaysCalled ← IO.mkRef (0 : Nat)
  let runToolAlways : String → Json → IO String := fun _ _ =>
    runToolAlwaysCalled.modify (· + 1) *> pure "15°C, cloudy"
  let maxIters := 2
  let resultBudget ←
    LLMClient.converseLoop providerAlwaysToolCall #[] runToolAlways #[LLMClient.Msg.user "weather?"]
      ({ maxIterations := maxIters } : LLMClient.LoopConfig)
  let firstToolCall : LLMClient.ToolCall := { id := "call_0", name := "get_weather", input := Json.mkObj [("city", "Paris")] }
  let r := r.check "converseLoop: exhausting a real tool-call budget returns .ok"
    (match resultBudget with | .ok _ => true | .error _ => false)
  let r := r.check "converseLoop: exhausted history size is user + 2 msgs/round × maxIterations + giving-up"
    (match resultBudget with
      | .ok (h, _) => h.size == 1 + 2 * maxIters + 1
      | .error _ => false)
  let r := r.check "converseLoop: exhausted history has the first round's tool call still present"
    (match resultBudget with
      | .ok (h, _) => h.getD 1 (LLMClient.Msg.user "") == LLMClient.Msg.assistant "" #[firstToolCall]
      | .error _ => false)
  let r := r.check "converseLoop: exhausted history has the first round's tool result still present"
    (match resultBudget with
      | .ok (h, _) => h.getD 2 (LLMClient.Msg.user "") == LLMClient.Msg.toolResult "call_0" "15°C, cloudy"
      | .error _ => false)
  let r := r.check "converseLoop: exhausted history's last entry is the giving-up assistant message"
    (match resultBudget with
      | .ok (h, _) => h.back? == some (LLMClient.Msg.assistant giveUpText)
      | .error _ => false)
  let r := r.check "converseLoop: exhausted reply text matches the giving-up message"
    (match resultBudget with
      | .ok (_, t) => t == giveUpText
      | .error _ => false)
  let runToolAlwaysCalled ← runToolAlwaysCalled.get
  let r := r.check "converseLoop: provider/runTool are each called exactly maxIterations times"
    (runToolAlwaysCalled == maxIters)

  -- provider.sendRequest failing outright is the one remaining .error case: nothing new has
  -- happened yet, so there's nothing to preserve.
  let providerError : LLMClient.Provider :=
    { name := "fake"
      sendRequest := fun _ _ => pure (.error "boom") }
  let resultError ←
    LLMClient.converseLoop providerError #[] runToolUnused #[LLMClient.Msg.user "hi"]
  let r := r.check "converseLoop: a provider error still propagates as .error"
    (match resultError with
      | .error e => e == "boom"
      | .ok _ => false)

  return r

def testBedrock (r : Results) : Results :=
  open LLMClient.Bedrock in
  let r := r
    |>.check "msgJson user"
      (msgJson (.user "hi") ==
        Json.mkObj [("role", "user"), ("content", Json.arr #[Json.mkObj [("text", "hi")]])])
    |>.check "msgJson assistant text only"
      (msgJson (.assistant "hello" #[]) ==
        Json.mkObj
          [ ("role", "assistant"), ("content", Json.arr #[Json.mkObj [("text", "hello")]]) ])
    |>.check "msgJson assistant tool call only"
      (msgJson (.assistant "" #[sampleToolCall]) ==
        Json.mkObj
          [ ("role", "assistant"),
            ("content", Json.arr
              #[Json.mkObj
                  [ ("toolUse", Json.mkObj
                      [ ("toolUseId", sampleToolCall.id), ("name", sampleToolCall.name),
                        ("input", sampleToolCall.input) ]) ]]) ])
    |>.check "msgJson assistant text and tool call"
      (msgJson (.assistant "hi" #[sampleToolCall]) ==
        Json.mkObj
          [ ("role", "assistant"),
            ("content", Json.arr
              #[ Json.mkObj [("text", "hi")], toolCallJson sampleToolCall ]) ])
    |>.check "msgJson toolResult"
      (msgJson (.toolResult "id1" "42") ==
        Json.mkObj
          [ ("role", "user"),
            ("content", Json.arr
              #[Json.mkObj
                  [ ("toolResult", Json.mkObj
                      [ ("toolUseId", "id1"),
                        ("content", Json.arr #[Json.mkObj [("text", "42")]]) ]) ]]) ])
    |>.check "toolJson"
      (toolJson sampleTool ==
        Json.mkObj
          [ ("toolSpec", Json.mkObj
              [ ("name", "get_weather"), ("description", "Get the weather"),
                ("inputSchema", Json.mkObj [("json", sampleTool.schema)]) ]) ])
  let textBlock := Json.mkObj [("text", "Hello ")]
  let textBlock2 := Json.mkObj [("text", "world")]
  let toolUseBlock := Json.mkObj
    [ ("toolUse", Json.mkObj
        [ ("toolUseId", "call_1"), ("name", "get_weather"),
          ("input", Json.mkObj [("city", "Paris")]) ]) ]
  let malformedToolUseBlock := Json.mkObj [("toolUse", Json.mkObj [("name", "missing_id")])]
  let content := #[textBlock, toolUseBlock, textBlock2, malformedToolUseBlock]
  let respWith (stopReason : String) (content : Array Json) : Json :=
    Json.mkObj
      [ ("output", Json.mkObj
          [("message", Json.mkObj [("role", "assistant"), ("content", Json.arr content)])]),
        ("stopReason", stopReason) ]
  let normalResp := respWith "end_turn" content
  let malformedResp := Json.mkObj [("foo", Json.null)]
  let errorResp := Json.mkObj [("message", "boom")]
  r
    |>.check "contentOf reaches through the output/message envelope"
      (contentOf normalResp == content)
    |>.check "contentOf on malformed response"
      (contentOf malformedResp == #[])
    |>.check "stopReasonOf end_turn"
      (stopReasonOf normalResp == "end_turn")
    |>.check "stopReasonOf content_filtered"
      (stopReasonOf (respWith "content_filtered" #[]) == "content_filtered")
    |>.check "stopReasonOf max_tokens"
      (stopReasonOf (respWith "max_tokens" #[]) == "max_tokens")
    |>.check "stopReasonOf on malformed response"
      (stopReasonOf malformedResp == "")
    |>.check "extractText concatenates only text blocks in order"
      (extractText content == "Hello world")
    |>.check "extractToolCalls skips entries missing toolUseId, keeps well-formed ones"
      (extractToolCalls content ==
        #[({ id := "call_1", name := "get_weather",
             input := Json.mkObj [("city", "Paris")] } : LLMClient.ToolCall)])
    |>.check "errorMessageOf reads the top-level message Bedrock returns"
      (errorMessageOf errorResp == "boom")
    |>.check "errorMessageOf falls back to compress on malformed response"
      (errorMessageOf malformedResp == malformedResp.compress)

def testBedrockRequestBody (r : Results) : Results :=
  open LLMClient.Bedrock in
  let cfgNone := defaultConfig "us-east-1"
  let cfgSome := { cfgNone with systemPrompt := some "respond in GitHub Flavored Markdown" }
  let bodyNone := requestBody cfgNone sampleHistory
  let bodySome := requestBody cfgSome sampleHistory
  let bodyTools := requestBody cfgNone sampleHistory #[sampleTool]
  r
    |>.check "requestBody with no systemPrompt and no tools omits both keys"
      (bodyNone ==
        Json.mkObj
          [ ("messages", Json.arr (sampleHistory.map msgJson)),
            ("inferenceConfig", Json.mkObj [("maxTokens", cfgNone.maxOutputTokens)]) ])
    |>.check "requestBody names the model in the path rather than the body"
      ((bodyNone.getObjVal? "model").toOption == none)
    |>.check "requestBody with systemPrompt sets a top-level system block array"
      ((bodySome.getObjVal? "system").toOption ==
        some (Json.arr #[Json.mkObj [("text", "respond in GitHub Flavored Markdown")]]))
    |>.check "requestBody with systemPrompt leaves messages untouched"
      ((bodySome.getObjVal? "messages").toOption == (bodyNone.getObjVal? "messages").toOption)
    |>.check "requestBody with tools wraps them in toolConfig"
      ((bodyTools.getObjVal? "toolConfig").toOption ==
        some (Json.mkObj [("tools", Json.arr #[toolJson sampleTool])]))
    |>.check "path names the model and the Converse operation"
      (path cfgNone == s!"/model/{cfgNone.model}/converse")
    |>.check "host is derived from the region"
      (host cfgNone == "bedrock-runtime.us-east-1.amazonaws.com")

def runAll : IO Results := do
  let r : Results := {}
  let r := testOpenAI r
  let r := testOpenAISystemPrompt r
  let r := testClaude r
  let r := testClaudeSystemPrompt r
  let r := testBedrock r
  let r := testBedrockRequestBody r
  testConverseLoop r

def main : IO UInt32 := do
  (← runAll).report
