Machine has_db

  Sees
    has_db

  Variables
    enabled
    desired
    sensed_temp
    sensed
    heat
    
  Invariants
    @inv1 enabled : DEVICETYPE --> BOOL
    @inv2 desired : NAT
    @inv3 sensed_temp : NAT
    @inv4 sensed : BOOL
    @inv5 heat : NAT

  Initialisation ==
    @act1 enabled := DEVICETYPE ** {TRUE}
    @act2 desired := 20
    @act3 sensed_temp := 0
    @act4 sensed := FALSE
    @act5 heat := 0

  Event disable ==
    any dd
    where
      @grd1 dd : DEVICETYPE
      @grd2 enabled(dd) = TRUE
    then
      @act1 enabled(dd) := FALSE
    end

  Event enable == 
    any dd
    where 
      @grd1 dd : DEVICETYPE
      @grd2 enabled(dd) = FALSE
    then
      @act1 enabled(dd) := TRUE
    end
    
  Event set_temp == 
    any tt
    where
      @grd1 tt : 10..33
      @grd2 enabled(HEATER) = TRUE
    then
      desired := tt
    end

  Event sense_temp ==
    any tt
    where
      @grd1 tt : 10 .. 30
      @grd2 sensed = FALSE
      @grd3 enabled(TEMPSENSOR) = TRUE
    then
      @act1 sensed_temp := tt
      @act2 sensed := TRUE
    end

  Event maintain ==
    when
      @grd1 desired = sensed_temp
      @grd2 sensed = TRUE
      @grd3 enabled(HEATER) = TRUE
    then
      @act1 sensed := FALSE
      @act2 heat := heat
    end

  Event increase == 
    when
      @grd1 desired > sensed_temp
      @grd2 sensed = TRUE
      @grd3 enabled(HEATER) = TRUE
    then
      sensed := FALSE
      heat := heat + 1
    end

End