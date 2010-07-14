Machine Quants

  Variables
    x

  Invariants
    x : INT                
    # k . k : INT => k > x

  Initialisation ==
    x := 0

  Event change ==
    any 
      xx
    where
      xx : INT
    then 
      x := xx
    end

End