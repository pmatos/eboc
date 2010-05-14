Machine counter
  
  Variables 
    x 
    
  Initialisation ==
    x := 0
    
  Event up ==
    when
      x < 10
    then
      x := x + 1
    end
    
  Event down ==
    when
      x > 0
    then
      x := x - 1
    end
    
  Invariants 
    @inv1 x >= 0
    
End
