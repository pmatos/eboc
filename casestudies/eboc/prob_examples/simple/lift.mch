Machine Lift

  Variables
    floor oftype INT

  Invariants
    floor : 0 .. 99
    
  Initialisation ==
    floor := 4

  Event inc == 
    when 
      floor < 99
    then
      floor := floor + 1
    end

  Event dec ==
    when 
      btrue
    then 
      floor := floor - 1
    end

End
    
    