-- Copyright (c) 2026 Paul Butcher. All rights reserved.
-- Released under Apache 2.0 license as described in the file LICENSE.

import Lake

open System Lake DSL

package LLMClient where
  version := v!"0.5.0"
  leanOptions := #[⟨`warningAsError, true⟩]

require leancurl from git "https://github.com/paulbutcher/leancurl"@"main"
require aws from git "https://github.com/paulbutcher/lean-aws"@"main"

@[default_target] lean_lib LLMClient

-- The tests live in their own package so that test-only dependencies stay out of the
-- dependency graph a downstream consumer resolves.
@[test_driver]
script tests do
  let child ← IO.Process.spawn
    { cmd := "lake", args := #["test"], cwd := __dir__ / "test" }
  child.wait
