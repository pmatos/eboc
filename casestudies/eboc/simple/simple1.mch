Machine simple1

  Variables
    x oftype INT

  Initialisation ==
    x := 0

  Event e1 ==
    when 
      x > 0
    then
      x := x + 1
    end

  Event e2 ==
    when 
      x = 0
    then 
      x := x + 2
    end
    
  Invariants
    x /= 1

End