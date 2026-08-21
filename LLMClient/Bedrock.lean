-- Copyright (c) 2026 Paul Butcher. All rights reserved.
-- Released under Apache 2.0 license as described in the file LICENSE.

module

public import Lean.Data.Json
import Lean.Data.Json.Parser
import Leancurl
import Std.Time
public import Aws
public import LLMClient.Provider

public section

namespace LLMClient.Bedrock

open Lean (Json)
open Leancurl

/-- Bedrock does not reuse `LLMClient.Config`: the endpoint is wholly determined by the region,
and signing needs the region as a field of its own. An `apiUrl` alongside it would be a second
copy of the same fact, free to disagree with the one the signature is computed from. -/
structure Config where
  region : String
  model : String
  maxOutputTokens : Nat := 1024
  systemPrompt : Option String := none

def defaultConfig (region : String) : Config :=
  { region, model := "anthropic.claude-opus-5" }

def host (config : Config) : String := s!"bedrock-runtime.{config.region}.amazonaws.com"

def path (config : Config) : String := s!"/model/{config.model}/converse"

@[expose] def toolJson (t : LLMClient.Tool) : Json :=
  Json.mkObj
    [ ("toolSpec", Json.mkObj
        [ ("name", t.name),
          ("description", t.description),
          ("inputSchema", Json.mkObj [("json", t.schema)]) ]) ]

@[expose] def toolCallJson (tc : LLMClient.ToolCall) : Json :=
  Json.mkObj
    [ ("toolUse", Json.mkObj
        [ ("toolUseId", tc.id), ("name", tc.name), ("input", tc.input) ]) ]

/-- Converse blocks carry no discriminating `type` field the way Anthropic's do; a block is a
one-key object and the key is what it is. -/
@[expose] def msgJson : LLMClient.Msg → Json
  | .user text =>
    Json.mkObj [("role", "user"), ("content", Json.arr #[Json.mkObj [("text", text)]])]
  | .assistant text toolCalls =>
    let textBlock := if text.isEmpty then #[] else #[Json.mkObj [("text", text)]]
    let toolBlocks := toolCalls.map toolCallJson
    Json.mkObj [("role", "assistant"), ("content", Json.arr (textBlock ++ toolBlocks))]
  | .toolResult id output =>
    Json.mkObj
      [ ("role", "user"),
        ("content", Json.arr
          #[Json.mkObj
            [ ("toolResult", Json.mkObj
                [ ("toolUseId", id),
                  ("content", Json.arr #[Json.mkObj [("text", output)]]) ]) ]]) ]

@[expose] def contentOf (resp : Json) : Array Json :=
  match resp.getObjVal? "output" >>= (·.getObjVal? "message") >>= (·.getObjVal? "content")
      >>= Json.getArr? with
  | .ok arr => arr
  | .error _ => #[]

def stopReasonOf (resp : Json) : String :=
  match resp.getObjVal? "stopReason" >>= Json.getStr? with
  | .ok s => s
  | .error _ => ""

def errorMessageOf (j : Json) : String :=
  match j.getObjVal? "message" >>= Json.getStr? with
  | .ok msg => msg
  | .error _ => j.compress

@[expose] def extractText (content : Array Json) : String :=
  content.foldl (init := "") fun acc b =>
    match b.getObjVal? "text" >>= Json.getStr? with
    | .ok t => acc ++ t
    | .error _ => acc

@[expose] def extractToolCalls (content : Array Json) : Array LLMClient.ToolCall :=
  content.filterMap fun b =>
    match b.getObjVal? "toolUse" with
    | .ok use =>
      match use.getObjVal? "toolUseId" >>= Json.getStr?, use.getObjVal? "name" >>= Json.getStr? with
      | .ok id, .ok name =>
        let input := (use.getObjVal? "input").toOption.getD (Json.mkObj [])
        some { id, name, input }
      | _, _ => none
    | .error _ => none

/-- The model is named by the URL rather than the body, and `toolConfig` is omitted rather than
sent empty: Converse rejects a `tools` array of length zero. -/
@[expose] def requestBody (config : Config) (history : Array LLMClient.Msg)
    (tools : Array LLMClient.Tool := #[]) : Json :=
  let fields : List (String × Json) :=
    [ ("messages", Json.arr (history.map msgJson)),
      ("inferenceConfig", Json.mkObj [("maxTokens", config.maxOutputTokens)]) ]
  let withSystem := match config.systemPrompt with
    | some s => fields ++ [("system", Json.arr #[Json.mkObj [("text", s)]])]
    | none => fields
  Json.mkObj (if tools.isEmpty then withSystem
    else withSystem ++ [("toolConfig", Json.mkObj [("tools", Json.arr (tools.map toolJson))])])

/-- The four variables a Lambda execution role, an ECS task role, or `aws configure export-credentials`
puts in the environment. `AWS_SESSION_TOKEN` is absent only for a long-term IAM user key. -/
def credentialsFromEnv : IO (Except String Aws.Sigv4.Credentials) := do
  let accessKeyId ← IO.getEnv "AWS_ACCESS_KEY_ID"
  let secretAccessKey ← IO.getEnv "AWS_SECRET_ACCESS_KEY"
  let sessionToken ← IO.getEnv "AWS_SESSION_TOKEN"
  match accessKeyId, secretAccessKey with
  | some accessKeyId, some secretAccessKey =>
    return .ok { accessKeyId, secretAccessKey, sessionToken }
  | _, _ =>
    return .error "AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY must both be set"

def regionFromEnv : IO (Option String) := do
  match ← IO.getEnv "AWS_REGION" with
  | some region => return some region
  | none => IO.getEnv "AWS_DEFAULT_REGION"

def currentTimestamp : IO Aws.Sigv4.Timestamp := do
  let now ← Std.Time.Timestamp.now
  return { epochSeconds := now.toSecondsSinceUnixEpoch.toInt.toNat }

/-- `signed.headers` is the complete set curl should send: the signer returns the caller's own
headers alongside the ones it owns, minus `host`, which curl derives from the URL. -/
private def signedPost (creds : Aws.Sigv4.Credentials) (config : Config) (body : ByteArray) :
    IO (Except CurlError Response) := do
  let t ← currentTimestamp
  let signed := Aws.Sigv4.sign creds { region := config.region, service := "bedrock" } t
    { method := "POST",
      path := path config,
      host := host config,
      headers := [("content-type", "application/json")],
      body }
  Curl.post s!"https://{host config}{path config}" body signed.headers

/-- Posts one Converse request with the given history; does not loop for tool use. -/
def sendRequest (config : Config) (creds : Aws.Sigv4.Credentials)
    (history : Array LLMClient.Msg) (tools : Array LLMClient.Tool := #[]) :
    IO (Except String LLMClient.Reply) := do
  let body := requestBody config history tools
  match ← signedPost creds config body.compress.toUTF8 with
  | .error e => return .error s!"request to the Bedrock API failed: {e.message}"
  | .ok resp =>
    match String.fromUTF8? resp.body with
    | none => return .error "the Bedrock API returned a response that wasn't valid UTF-8"
    | some text =>
      match Json.parse text with
      | .error e => return .error s!"couldn't parse the Bedrock API response: {e}"
      | .ok json =>
        if resp.status >= 200 && resp.status < 300 then
          let content := contentOf json
          return .ok
            { text := extractText content,
              toolCalls := extractToolCalls content,
              isRefusal :=
                stopReasonOf json == "content_filtered" ||
                  stopReasonOf json == "guardrail_intervened",
              isTruncated := stopReasonOf json == "max_tokens" }
        else
          return .error s!"Bedrock API error ({resp.status}): {errorMessageOf json}"

/-- Build the provider from freshly resolved credentials each time a request is served rather
than once at start-up; the temporary credentials an execution role issues are rotated underneath
a long-lived process. -/
def provider (creds : Aws.Sigv4.Credentials) (config : Config) : LLMClient.Provider :=
  { name := "bedrock", sendRequest := fun history tools => sendRequest config creds history tools }

end LLMClient.Bedrock
