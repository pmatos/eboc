Machine earley_0

  Sees
    earley_ctx_0

  Variables
    item return

  Invariants
    item : (P ** NAT) <-> (NAT ** NAT)
    
  Initialisation ==
    item := {(axiom |-> 0) |-> (0 |-> 0)}
    return := FALSE

  Event selector ==
    any p k i j
    where
      (p |-> k) |-> (i |-> j) : item
      k < csize(p)
      j < s
      cright(p)(k+1) = input(j + 1)
      i : NAT & j : NAT & k : NAT & p : P
    then
      item := item \/ {(p |-> k + 1) |-> (i |-> j + 1)}
    end

  Event predictor ==
    any p k i j q
    where
      (p |-> k) |-> (i |-> k) : item
      k < csize(p)
      cleft(q) = cright(p)(k+1)
      i : NAT & j: NAT & k:NAT & p: P & q:P
    then
      item := item \/ {(q |-> 0) |-> (j |-> j)}
    end
    
  Event completer ==
    any p k i j q kp ip
    where
      (p |-> k) |-> (i |-> j) : item
      k = csize(p)
      (q |-> kp) |-> (ip |-> i) : item
      kp < csize(q)
      cright(q)(kp + 1) = cleft(p)
      i : NAT & j : NAT & k : NAT & p : P & q : P & kp : NAT & ip : NAT
    then
      item := item \/ {(q |-> kp + 1) |-> (ip |-> j)}
    end
    
  Event final ==
    when
      (axiom |-> csize(axiom)) |-> (0 |-> s) : item
    then
      return := TRUE
    end
End
  
