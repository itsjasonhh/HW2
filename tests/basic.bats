load harness

@test "assign" {
  check 'x := 1' '{x → 1}'
}

@test "skip" {
  check 'skip' '{}'
}

@test "if" {
  check 'if true then x := 1 else x := 0' '{x → 1}'
}

@test "while-false" {
  check 'while false do x := 3' '{}'
}

@test "while-true" {
  check 'while x=0 do x := 3' '{x → 3}'
}

@test "seq" {
  check 'x:= 1*9; if 5 < x then x:=2-2 else y := 9' '{x → 0}'
}

@test "and-tt" {
  check 'if x=0 ∧ y < 4 then x:= 1 else x:= 3' '{x → 1}'
}

@test "and-tf" {
  check 'if x=0 ∧ 4 < 4 then x:= 1 else x:= 3' '{x → 3}'
}

@test "and-ft" {
  check 'if 0<x  ∧  4 = 4 then x := 1 else x:= 3' '{x → 3}'
}

@test "and-ff" {
  check 'if 0<x ∧ 4<y then   x := 1 else x:= 3' '{x → 3}'
}

@test "or-tt" {
  check 'if x=0 ∨ y < 4 then x:= 1 else x:= 3' '{x → 1}'
}

@test "or-tf" {
  check 'if x=0 ∨ 4 < 4 then x:= 1 else x:= 3' '{x → 1}'
}

@test "or-ft" {
  check 'if 0<x ∨ 4 = 4 then x := 1 else x:= 3' '{x → 1}'
}

@test "or-ff" {
  check 'if 0<x ∨ 4<y then x := 1 else x:= 3' '{x → 3}'
}

@test "not-t" {
  check 'while ¬true do x := 1' '{}'
}

@test "not-f" {
  check 'while ¬(x  < 0) do x := -1' '{x → -1}'
}
