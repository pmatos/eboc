Machine rm_1

  Sees rc_0
    
  Variables 
    NET 
    net  

  Invariants
    NET <: L    
    net <: L

  Variants
    (NET\net) \/ (net\NET)

  Initialisation ==
    NET := {}
    net := {}

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

  Event discover_up ==
    any l
    when
      l : NET \ net
    then
      net := net \/ {l}
    end

  Event discover_dn ==
    any l
    when
      l : net \ NET
    then
      net := net \ {l}
    end

End      