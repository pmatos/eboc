Machine FunLaws

  Sees 
    funlaws

  Variables
    ff oftype POW(setX ** setY)
    gg oftype POW(setX ** setY)

  Invariants
    ff : setX <-> setY & gg : setX <-> setY
    @law1 (dom(ff\/gg) = dom(ff) \/ dom(gg))
    @law2 (ran(ff\/gg) = ran(ff) \/ ran(gg))
    @law3 (dom(ff/\gg) <: dom(ff) /\ dom(gg))
    @law4 (ran(ff/\gg) <: ran(ff) /\ ran(gg))
    @law5 ( (ff \/ gg)~ = ff~ \/ gg~)
    @law6 (dom(ff ; (gg~)) <: dom(ff))
    @law7 (!(xx,yy).(xx:setX & yy:setY & xx|->yy : ff  =>  yy: ran(gg)) =>  (dom(ff ; (gg~)) = dom(ff)))
    @law8 (ff : setX --> setY  <=>  (ff: setX +-> setY & dom(ff) = setX))
    @law9 (ff : setX +-> setY  <=> (!(xx,yy,zz).((xx:setX & yy:setY & zz:setY & xx|->yy:ff & xx|->zz:ff) => (yy=zz))))
    @law10 (ff : setX >->> setY  <=>  (ff : setX >-> setY  &  ff~: setY >-> setX))
    @law11 (ff : setX >+> setY  <=> (ff: setX +-> setY & !(xx,yy).(xx:setX & yy:setX & xx/=yy & xx:dom(ff) & yy: dom(ff)  => ff(xx)/=ff(yy))))
    @law12(ff : setX +->> setY  <=>  (ff: setX +-> setY & !yy.(yy:setY => yy: ran(ff))))
    ff : setX --> setY => ff : setX +-> setY
    ff : setX >+> setY => ff : setX +-> setY
    ff : setX >-> setY => ff : setX >+> setY
    ff : setX >-> setY => ff : setX --> setY
    ff : setX >->> setY => ff : setX --> setY
    ff : setX +->> setY => ff : setX +-> setY
    ff : setX -->> setY => (ff : setX --> setY & ff : setX +->> setY)
    ff : setX +->> setY => ran(ff) = setY
    ran(ff) <<: setY => not(ff : setX +->> setY)
    (ff : setX -->> setY or not(ff : setX -->> setY)) &  not(ff:setX-->>setY & not(ff:setX-->>setY))
    (ff:setX>+>setY or not(ff:setX>+>setY)) &  not(ff:setX>+>setY & not(ff:setX>+>setY))
    (ff:setX>->setY or not(ff:setX>->setY)) &  not(ff:setX>->setY & not(ff:setX>->setY))
    (ff:setX+->>setY or not(ff:setX+->>setY)) &  not(ff:setX+->>setY & not(ff:setX+->>setY))
    (ff:setX>->>setY or not(ff:setX>->>setY)) &  not(ff:setX>->>setY & not(ff:setX>->>setY))
    (ff:setX-->setY or not(ff:setX-->setY)) &  not(ff:setX-->setY & not(ff:setX-->setY))
    (ff:setX+->setY or not(ff:setX+->setY)) &  not(ff:setX+->setY & not(ff:setX+->setY))
    (!xx.(xx:setX & xx:dom(ff) => ff(xx):ran(ff)))
    card(dom(ff))<3 
    card(ran(ff))<3 
    (dom(ff) = {xx | xx: setX & #yy.(yy:setY & xx|->yy:ff)}) 
    (ran(ff) = {yy | yy: setY & #xx.(xx:setX & xx|->yy:ff)}) 
    (ff: setX +-> setY => !(xx).(xx:setY & xx:ran(ff) => #ii.(ii:setX & ii:dom(ff) & ff(ii)=xx))) 
    ff[dom(ff)] = ran(ff) 
    (!xx.(xx:setX => ff[{xx}] = {yy | yy:setY & xx |->yy : ff}))
    (!(xx,xx2).(xx:setX & xx2:setX => ff[{xx,xx2}] = {yy | yy:setY & ((xx |->yy : ff) or (xx2|->yy: ff)) })) 
    (ff~[ff[setX]] = dom(ff)) 
    (ff~[setY] = dom(ff)) 
    (ff[setX] = ran(ff)) 
    (ff: setX >+> setY  =>  !xx.(xx<:dom(ff) => ff~[ff[xx]] = xx))

  Initialisation ==
    ff, gg := {}, {}

  Event add_ff ==
    any
      xx oftype setX 
      yy oftype setY
    where
      xx |-> yy /: ff
    then
      ff := ff \/ {xx |-> yy}
    end

   Event add_gg ==
     any 
       xx oftype setX
       yy oftype setY
     where
       xx |-> yy /: gg
     then
       gg := gg \/ {xx |-> yy}
     end

  Event  ff_surj ==
    