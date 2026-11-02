-- Copyright (c) 2026 Paul Butcher. All rights reserved.
-- Released under Apache 2.0 license as described in the file LICENSE.

-- Not a `module`: it imports `Lasm.Web`, which is not one. Only Lasm can build this file, and
-- nothing else in the library imports it, so a native `lake build` never reaches it.

import Lasm.Web
import LLMClient.Core

namespace LLMClient

open Lasm.Web Lasm.JS

/-- `fetch` rejects, rather than resolving with a status, when no response arrives at all: a
network failure, or a CORS refusal the browser does not let the page inspect. -/
def Fetch.transport : Transport Async := fun url body headers => do
  try
    let resp ← fetch url { method := "POST", headers := headers.toArray, body := .bytes body }
    return .ok { status := (← resp.status).toUInt32, body := ← resp.bytes }
  catch e =>
    return .error (toString e)

end LLMClient
