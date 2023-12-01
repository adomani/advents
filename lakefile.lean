import Lake
open Lake DSL

package «advents» {
  -- add package configuration options here
}

lean_lib «Advents» {
  -- add library configuration options here
}

@[default_target]
lean_exe «advents» {
  root := `Main
}

require std from git "https://github.com/leanprover/std4" @ "main"
