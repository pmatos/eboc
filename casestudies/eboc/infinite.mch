Machine Infinite

//  Sees
//    infinite

  Variables 
    intset

  Invariants
    intset : POW(INT)
    
  Initialisation ==
    intset := INT
    
  Event rem ==
    any 
      y
    where
      y : intset
    then
      intset := intset \ {y}
    end

End