Machine ACounter
  
  Variables 
    ii oftype INT
    jj oftype INT

  Invariants
    ii : 0 .. 10 & jj : 0 .. 10 & ii < 11 & jj >= 0

  Initialisation ==
    ii, jj := 2, 10

  Event inc == 
    when 
      jj > 0
    where
      ii := ii + 1
      jj := jj - 1
    end

End