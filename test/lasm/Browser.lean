-- Copyright (c) 2026 Paul Butcher. All rights reserved.
-- Released under Apache 2.0 license as described in the file LICENSE.

import Tests

open Lasm.Web Lasm.JS

structure Flags where
  base : String
  dead : String
  deriving FromJS

/-- A browser page may not block, so the tests run in the background and report on the console. -/
def main : IO Unit := do
  let flags ← App.flags Flags
  background do
    discard <| (← runTests flags.base flags.dead true).report
