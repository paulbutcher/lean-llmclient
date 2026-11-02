-- Copyright (c) 2026 Paul Butcher. All rights reserved.
-- Released under Apache 2.0 license as described in the file LICENSE.

import Tests

open Lasm.JS

def main : IO UInt32 := do
  let some base ← IO.getEnv "BASE_URL" | IO.eprintln "BASE_URL is not set"; return 1
  let some dead ← IO.getEnv "DEAD_URL" | IO.eprintln "DEAD_URL is not set"; return 1
  return if ← (← (runTests base dead false).block).report then 0 else 1
