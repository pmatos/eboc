Machine rm_2

  Sees rc_1
    
  Variables
    NET net l_net m_net_up m_net_dn
    
  Invariants
    @inv1 l_net : N <-> L    
    @inv2 m_net_up : N <-> L
    @inv3 m_net_dn : N <-> L
    @inv4 m_net_up /\ m_net_dn = {}
    @inv7 ! n, l . l : net & n |-> l /: l_net => n |-> l : l_net_up
    @inv9 ! n, l . l : net & n |-> l : l_net => n |-> l /: l_net_dn
    @inv8 ! n, l . l /: net & n |-> l : l_net => n |-> l : m_net_dn
    @inv10 ! n, l . l /: net & n |-> l /: l_net => n |-> l /: m_net_up

  Theorems // There are some theorems in the original model which are not important for model checking
    @thm1 btrue 
    @thm2 btrue
    @thm3 btrue
    @thm4 btrue 

  Initialisation == 
    NET := {}
    net := {}
    l_net := {}
    m_net_up := {}
    m_net_dn := {}

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
    any l n
    when
      l : NET \ net
      n : N
    then
      net := net \/ {l}
      m_net_up := m_net_up \/ ((N \ {n}) ** {l})
      m_net_dn := m_net_dn \ (N ** {l})
      l_net := l_net \/ {n |-> l}
    end

  Event discover_dn == 
    any l n
    when
      l : net \ NET
      n : N
    then
      net := net \ {l}
      m_net_up := m_net_up \ (N ** {l})
      m_net_dn := m_net_dn \/ ((N \ {n}) ** {l})
      l_net := l_net \ {n|->l}
    end

  Event change_link_up ==
    any n l
    when
      n |-> l : m_net_up
    then
      l_net := l_net \/ {n |-> l}
      m_net_up := m_net_up \ {n |-> l}
    end

  Event change_link_dn ==
    any n l
    when 
      n |-> l : m_net_dn
    then
      l_net := l_net \ {n |-> l}
      m_net_dn := m_net_dn \ {n |-> l}
    end

Event change_link_2
  any ln
  when
    ln : N <-> L
    ! n, l . l : net & n |-> l /: ln => n |-> l : m_net_up
    ! n, l . l : net & n |-> l : ln => n |-> l /: m_net_dn
    ! n, l . l /: net & n |-> l : ln => n |-> l : m_net_dn
    ! n, l . l /: net & n |-> l /: ln => n |-> l /: m_net_up
  then
    l_net := ln
  end   
  