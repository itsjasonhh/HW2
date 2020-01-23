load harness

@test "modtest1" {
  check 'x := 5%3' '{x → 2}'
}

@test "modtest2" {
  check 'x:= 10; if x % 3 > 2 then x:= 4 else skip' '{x → 10}'
}

@test "modtest3" {
  check 'x:= 100; while x > 7 do x := x%7' '{x → 2}'
}
