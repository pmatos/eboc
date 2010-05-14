Machine earley_00
  
  Sees 
    earley_ctx_0

  Variables 
    return
    
  Invariants
    return : BOOL
    
  Initialisation ==
    return := FALSE
    
  Event final ==
    when 
      btrue
    then 
      return := TRUE
    end
End
