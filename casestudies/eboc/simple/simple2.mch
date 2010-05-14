Machine simple2

  Variables
    x oftype INT

  Initialisation ==
    x := 0

  Event e1 ==
    any 
      y oftype INT
    where 
      x > 0 & y = 2 
    then
      x := x + y
    end

  Event e2 ==
    when 
      x = 0
    then 
      x := x + 2
    end
    
  Invariants
    x mod 2 = 0 // x is always an even number

End