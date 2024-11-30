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

require batteries from git "https://github.com/leanprover-community/batteries" @ "main"
