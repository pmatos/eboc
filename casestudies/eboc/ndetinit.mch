Machine ndetinit
  
  Variables 
    x y

  Initialisation ==
    x :| x = 11 or x = -100
    y := 0

  Invariants 
    y <= 0
    x > 0 => (x mod 2 = 1)
    x < 0 => (x mod 2 = 0)

  Event up ==
    when
      x > 0
    then
      x, y := x + 2, y - 1
    end

  Event down == 
    when
      x < 0
    then
      x, y := x - 2, y - 51
    end
End