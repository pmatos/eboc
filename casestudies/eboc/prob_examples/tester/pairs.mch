Machine Pairs

  Sees
    Pairs

  Variables
    pairs oftype POW(setB ** setC)
    res_get_T oftype setB ** setC
    res_get_C oftype setB ** setC

  Invariants 
    btrue

  Initialisation ==
    pairs := {}

  Event new_pair ==
    any 
      bb oftype setB
      cc oftype setC
    where
      pairs = {}
    then
      pairs := {bb |-> cc}
    end

  Event add_pair ==
    any 
      bb oftype setB
      cc oftype setC
    where
      pairs /= {}
      bb |-> cc :/ pairs
    then
      pairs := pairs \/ {bb |-> cc}
    end

Event get_T
  any
    bb oftype setB
    xx oftype setB ** setC
  where
    xx : pairs & xx <| prj1 = bb
  then
    res_get_T := xx
  end

Event get_C
  any 
    bb oftype setB
  where
    xx : pairs & xx <| prj1 = bb
  then
    res_get_C := xx <| prj2
  end
End
      
    
    
      
    