-- Copyright (c) 2026 Paul Butcher. All rights reserved.
-- Released under Apache 2.0 license as described in the file LICENSE.

module

public import Json
import Std.Time
public import Aws
public import LLMClient.Provider

public section

namespace LLMClient.Bedrock

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

/-- One `toolResult` block, as it appears in the content of the user message answering a request
for tools. `status` is omitted rather than sent as `"success"`, in the same way as the system
prompt.

Converse is alone among the providers here in taking a result as JSON rather than as a string, so
a `structured` result goes in a `json` block and `output` is sent only when there is none. -/
@[expose] def toolResultJson (id output : String) (isError : Bool := false)
    (structured : Option Json := none) : Json :=
  let content := match structured with
    | some v => Json.mkObj [("json", v)]
    | none => Json.mkObj [("text", output)]
  let fields : List (String × Json) :=
    [ ("toolUseId", id), ("content", Json.arr #[content]) ]
  Json.mkObj
    [ ("toolResult",
        Json.mkObj (if isError then fields ++ [("status", Json.str "error")] else fields)) ]

/-- Converse blocks carry no discriminating `type` field the way Anthropic's do; a block is a
one-key object and the key is what it is. An empty text block is omitted, since Converse rejects
one, and thinking blocks are dropped: Converse carries reasoning in a shape of its own, and
replaying none of it is permitted. -/
@[expose] def blockJson : LLMClient.Block → Option Json
  | .text text => if text.isEmpty then none else some (Json.mkObj [("text", text)])
  | .toolCall tc => some (toolCallJson tc)
  | .thinking _ _ | .redactedThinking _ => none

/-- A lone `.toolResult` becomes a message of its own here, which is right only when it is the one
result outstanding. `messagesJson` is what a request is built from, and it groups. -/
@[expose] def msgJson : LLMClient.Msg → Json
  | .user text =>
    Json.mkObj [("role", "user"), ("content", Json.arr #[Json.mkObj [("text", text)]])]
  | .assistant content =>
    Json.mkObj [("role", "assistant"), ("content", Json.arr (content.filterMap blockJson))]
  | .toolResult id output isError structured =>
    Json.mkObj
      [ ("role", "user"),
        ("content", Json.arr #[toolResultJson id output isError structured]) ]

/-- The conversation as Converse wants it, which is not one message per `Msg`.

An assistant turn that asks for several tools is answered by a *single* user message carrying one
`toolResult` block per call. A message each is rejected: Converse looks for every outstanding id
in the one message following the request and reports whichever are not there as missing. It also
requires roles to alternate, which a run of one-result messages breaks by itself.

So a run of consecutive `.toolResult`s becomes one message. Everything else maps as it reads. -/
@[expose] def messagesJson (history : Array LLMClient.Msg) : Array Json :=
  let close (pending acc : Array Json) : Array Json :=
    if pending.isEmpty then acc
    else acc.push (Json.mkObj [("role", "user"), ("content", Json.arr pending)])
  let (acc, pending) := history.foldl (init := ((#[] : Array Json), (#[] : Array Json)))
    fun (acc, pending) msg =>
      match msg with
      | .toolResult id output isError structured =>
        (acc, pending.push (toolResultJson id output isError structured))
      | other => ((close pending acc).push (msgJson other), #[])
  close pending acc

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

/-- The inverse of `blockJson`. A block of a kind this library does not model, reasoning included,
is dropped. -/
@[expose] def readBlock (b : Json) : Option LLMClient.Block :=
  match b.getObjVal? "text" >>= Json.getStr? with
  | .ok t => if t.isEmpty then none else some (.text t)
  | .error _ =>
    match b.getObjVal? "toolUse" with
    | .ok use =>
      match use.getObjVal? "toolUseId" >>= Json.getStr?, use.getObjVal? "name" >>= Json.getStr? with
      | .ok id, .ok name =>
        let input := (use.getObjVal? "input").toOption.getD (Json.mkObj [])
        some (.toolCall { id, name, input })
      | _, _ => none
    | .error _ => none

@[expose] def extractContent (content : Array Json) : Array LLMClient.Block :=
  content.filterMap readBlock

/-- The model is named by the URL rather than the body, and `toolConfig` is omitted rather than
sent empty: Converse rejects a `tools` array of length zero. -/
@[expose] def requestBody (config : Config) (history : Array LLMClient.Msg)
    (tools : Array LLMClient.Tool := #[]) : Json :=
  let fields : List (String × Json) :=
    [ ("messages", Json.arr (messagesJson history)),
      ("inferenceConfig", Json.mkObj [("maxTokens", Json.ofNat config.maxOutputTokens)]) ]
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

/-- Reads one Converse response. Pure, so that what a status and body mean is settled here and the
transport carrying them decides nothing. -/
def parseReply (status : UInt32) (body : ByteArray) : Except String LLMClient.Reply :=
  if status >= 200 && status < 300 then
    match String.fromUTF8? body with
    | none => .error "the Bedrock API returned a response that wasn't valid UTF-8"
    | some text =>
      match Json.parse text with
      | .error e => .error s!"couldn't parse the Bedrock API response: {e}"
      | .ok json =>
        let content := contentOf json
        .ok
          { content := extractContent content,
            isRefusal :=
              stopReasonOf json == "content_filtered" ||
                stopReasonOf json == "guardrail_intervened",
            isTruncated := stopReasonOf json == "max_tokens" }
  else
    .error s!"Bedrock API error ({status}): {LLMClient.errorDetail errorMessageOf body}"

/-- Posts one Converse request with the given history; does not loop for tool use. `clock` gives
the time the request is signed at.

`signed.headers` is the complete set to send: the signer returns the caller's own headers
alongside the ones it owns, minus `host`, which the transport derives from the URL. -/
def sendRequest [Monad m] (transport : LLMClient.Transport m) (clock : m Aws.Sigv4.Timestamp)
    (config : Config) (creds : Aws.Sigv4.Credentials) (history : Array LLMClient.Msg)
    (tools : Array LLMClient.Tool := #[]) : m (Except String LLMClient.Reply) := do
  let body := (requestBody config history tools).compress.toUTF8
  let signed := Aws.Sigv4.sign creds { region := config.region, service := "bedrock" } (← clock)
    { method := "POST",
      path := path config,
      host := host config,
      headers := [("content-type", "application/json")],
      body }
  match ← transport s!"https://{host config}{path config}" body signed.headers with
  | .error e => return .error s!"request to the Bedrock API failed: {e}"
  | .ok resp => return parseReply resp.status resp.body

/-- Build the provider from freshly resolved credentials each time a request is served rather
than once at start-up; the temporary credentials an execution role issues are rotated underneath
a long-lived process. -/
def providerWith [Monad m] (transport : LLMClient.Transport m) (clock : m Aws.Sigv4.Timestamp)
    (creds : Aws.Sigv4.Credentials) (config : Config) : LLMClient.Provider m :=
  { name := "bedrock"
    sendRequest := fun history tools => sendRequest transport clock config creds history tools }

end LLMClient.Bedrock
