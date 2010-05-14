Machine Bakery1

  Variables
    p1 oftype INT
    p2 oftype INT
    y1 oftype INT
    y2 oftype INT

  Invariants
    p1 : 0 .. 2 & p2 : 0 .. 2 & y1 >= 0 & y2 >= 0 & (p1 = 2 => p2 < 2) & (p2 = 2 => p1 < 2)

  Initialisation ==
    p1, p2, y1, y2 := 0, 0, 0, 0

  Event try1 ==
    when 
      p1 = 0
    then
      p1 := 1
      y1 := y2 + 1
    end

  Event enter1 ==
    when 
      p1 = 1 & (y2 = 0 or y1 < y2)
    then
      p1 := 2
    end
    
  Event leave1 ==
    when
      p1 = 2
    then
      p1 := 0
      y1 := 0
    end

  Event try2 ==
    when 
      p2 = 0
    then
      p2 := 1
      y2 := y1 + 1
    end

  Event enter2 ==
    when 
      p2 = 1 & (y1 = 0 or y2 < y1)
    then
      p2 := 2
    end

  Event leave2 == 
    when 
      p2 = 2
    then 
      p2 := 0
      y2 := 0
    end
    
End
