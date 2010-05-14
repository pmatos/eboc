Machine TotalFun

  Variables 
    cube oftype FACE
    cc_Rot oftype POW(FACE +-> FACE)

  Invariants
    cc_Rot : FACE +-> FACE

  Initialisation ==
    cube := ftop
    cc_Rot := {}

  Event Rot == 
    any
      rot oftype POW(FACE +-> FACE)
    where
      rot = {fbot |-> fbot}
    then
      cc := rot
    end
End
      