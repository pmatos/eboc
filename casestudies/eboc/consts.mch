Machine consts

  Sees
    consts
    
  Variables
    x
    
  Initialisation == 
    x := 0
    
  Event e1 ==
    any 
      xx 
    where 
      xx : INT
    then
      x := x + xx
    end
    
  Event e2 == 
    any 
      xx 
    where
      xx > c2
    then
      x := xx + x + c1
    end

  Event e3 == 
    when 
      btrue
    then 
      x := c1 + c2 + x
    end
    
  Invariants
    c1 = 2 => x /= 150
    (x >= -5000) & (x <= 5000)
    
End