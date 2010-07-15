Machine FiniteM

  Variables 
    intset

  Invariants
    intset : POW(INT)
    intset /= {-1, 0, 1}    
    
  Initialisation ==
    intset := {}
    
  Event add ==
    any 
      y
    where
      y : INT
    then
      intset := intset \/ {y}
    end

End