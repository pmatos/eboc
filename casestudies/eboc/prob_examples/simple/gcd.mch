Machine GCD

  Variables
    x oftype INT
    y oftype INT
    s oftype INT

  Invariants
    x : NAT & y : NAT

  Initialisation == 
    x := 70
    y := 40

  Event GCDSolution1 ==
    when
      y = 0
    then
      s := x
    end
    
  Event GCDSolution2 ==
    when 
      y /= 0
    then 
      s := -1
    end
    
  Event Step ==
    when
      y > 0
    then
      x := y
      y := x mod y
    end

  Event Restart1 == 
    any 
      w1 oftype INT
      w2 oftype INT
    where
      w1 : NAT1
      w2 : NAT1
      w1 > w2
    then
      x, y := w1, w2
    end
      
  Event Restart2 ==
    any 
      w1 oftype INT
      w2 oftype INT
    where
      w1 : NAT1
      w2 : NAT1
      w1 <= w2
    then
      y, x := w1, w2
    end

End
      