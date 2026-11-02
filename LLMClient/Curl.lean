-- Copyright (c) 2026 Paul Butcher. All rights reserved.
-- Released under Apache 2.0 license as described in the file LICENSE.

module

import Leancurl
public import LLMClient.Core

public section

namespace LLMClient

def Curl.transport : Transport IO := fun url body headers => do
  return match ← Leancurl.Curl.post url body headers with
    | .ok resp => .ok { status := resp.status, body := resp.body }
    | .error e => .error e.message

def Claude.provider (apiKey : String) (config : Config := defaultConfig) : Provider IO :=
  providerWith Curl.transport apiKey config

def OpenAI.provider (apiKey : String) (config : Config := defaultConfig) : Provider IO :=
  providerWith Curl.transport apiKey config

def Bedrock.provider (creds : Aws.Sigv4.Credentials) (config : Bedrock.Config) : Provider IO :=
  providerWith Curl.transport currentTimestamp creds config

end LLMClient
