Machine Bakery0

  Variables
    aa oftype INT

  Invariants
    aa : 0 .. 2

  Initialisation == 
    aa := 0

  Event enter1 ==
    when
      aa = 0
    then
      aa := 1
    end
    
  Event enter2 ==
    when 
      aa = 0
    then
      aa := 2
    end

  Event leave1 ==
    when
      aa = 1
    then 
      aa := 0
    end

  Event leave2 == 
    when 
      aa = 2
    then
      aa := 0
    end
    
End