Machine bool_test

  Sees
    bool_test
    
  Variables
    nm oftype POW(Name)
    ee oftype BOOL

  Invariants
    btrue
    
  Initialisation == 
    nm := {}

  Event add ==
    any 
      n : Name
    where
      btrue
    then
      nm := nm \/ {n}
    end

  Event nonempty ==
    when
      btrue
    then
      ee := bool(nm /= {})
    end

  Event empty ==
    when
      btrue
    then
      ee := bool(nm = {})
    end

End