Machine Any

  Sees 
    any

  Variables 
    col1 oftype POW(COLOURS)
    col2 oftype POW(COLOURS)
    b : BOOL

  Invariants
    ! cc . (cc : col1 => cc /: col2) & ! cc2 . (cc2 : col2 => cc2 /: col1)
    
  Initialisation ==
    col1 := {red}
    col2 := {green}
    b := TRUE

  Event add ==
    any 
      x oftype COLOURS
    where
      x /: col1
    then
      col1 := col1 \/ {x}
    end

  Event setb ==
    any
      v oftype BOOL
    where
      ((v = TRUE) => (col1 = col2))
    then
      b := v
    end

  Event swap ==
    when
      btrue
    then
      col1 := col2
      col2 := col1
    end

  Event RemoveElement ==
    any 
      x oftype COLOURS
    where
      btrue
    then
      col1 := col1 \ {x}
      col2 := col2 \ {x}
    end

End
      