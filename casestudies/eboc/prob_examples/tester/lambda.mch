Machine Lambda

  Sees
    lambda

  Variables
    col1 oftype POW(COLOURS)
    col2 oftype POW(COLOURS)
    pf oftype COLOURS +-> COLOURS
    cc oftype COLOURS +-> COLOURS

  Invariants 
    btrue

  Initialisation ==
    col1 := {red}
    col2 := {green}
    pf := {}
    cc := {}

  Event swap ==
    when 
      btrue
    then
      col1 := col2
      col2 := col1
    end

  Event Tester ==
    when 
      btrue
    then
      pf := {x1 |-> x2 | x1 : COLOURS & x2 : COLOURS}
    end

  Event TPF ==
    when 
      btrue
    then
      cc := {x1 |-> x2 | x1 : COLOURS & x2 : COLOURS & x2 = red}
    end

  Event test = 
    when
      btrue
    then
      pf := % xx . (xx : COLOURS | red)
    end

End
      
      