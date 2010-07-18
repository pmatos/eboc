Machine ACounter

  Variables 
    ii jj

  Invariants
    ii : 0 .. 10
    jj : 0 .. 10
    ii < 11 
    jj >= 0

  Initialisation ==
    ii := 2
    jj := 10

  Event inc ==
    when
      jj > 0
    then
      ii := ii + 1
      jj := jj - 1
    end

End
    