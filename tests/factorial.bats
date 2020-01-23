load harness

@test "fact-5" {
  checkOr 'i:=5; fact := 1; while 0<i do { fact := fact * i; i := i - 1 }' '{fact → 120, i → 0}' '{i → 0, fact → 120}'
}

@test "fact-3" {
  checkOr 'i:=3; fact := 1; while 0<i do { fact := fact * i; i := i - 1 }' '{fact → 6, i → 0}' '{i → 0, fact → 6}'
}

@test "fact--1" {
  checkOr 'i:=-1; fact := 1; while 0<i do { fact := fact * i; i := i - 1 }' '{fact → 1, i → -1}' '{i → -1, fact → 1}'
}
