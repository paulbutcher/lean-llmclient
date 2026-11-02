-- Copyright (c) 2026 Paul Butcher. All rights reserved.
-- Released under Apache 2.0 license as described in the file LICENSE.

module

import Leancurl
public import LLMClient.Provider

public section

namespace LLMClient.Curl

def transport : LLMClient.Transport IO := fun url body headers => do
  return match ← Leancurl.Curl.post url body headers with
    | .ok resp => .ok { status := resp.status, body := resp.body }
    | .error e => .error e.message

end LLMClient.Curl
