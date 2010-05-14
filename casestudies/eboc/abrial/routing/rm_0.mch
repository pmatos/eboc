Machine rm_0

  Sees rc_0

  Variables
    NET

  Invariants
    NET <: L

  Initialisation == 
    NET := {}

  Event Modify_up == 
    any l
    when 
      l /: NET
    then 
      NET := NET \/ {l}
    end

  Event Modify_dn ==
    any l
    when
      l : NET
    then
      NET := NET \ {l}
    end
    
End