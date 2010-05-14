Machine n3 
/* Introduce a control strategy using a pump to increase water level when necessary.
   When the level goes below a low threshold LT, the pump is switched on.
   When the level goes above a high threshold HT, the pump is switched off.
   The pump is switched by control events.
   The maximum delay between successive control events is C.
   Assumptions:
   - When the pump is off, the water level may decrease by
   a maximum amount DD within 1 time unit.
   - When the pump is on, the water level may increase by
   a maximum amount DU within 1 time unit.
   These assumptions allow us to reason about the maximum difference
   between the water level at 2 time points. */
	Sees 
		c3

	Variables 
		wl 
		clk 
		ct 
		pump 
	
	Invariants
		@inv1 pump : BOOL // FALSE means pump is off
		@inv6 ct >= 0 // timestamp of the most recent control event
		@inv7 ct <= clk
		@inv8 clk <= ct+C
		/* timestamp of most recent control event must
    be within C time units of the current time */
		@inv14 btrue // bounds on difference between water level at two time points i,j :
		@inv4 pump=FALSE => (!i,j . ct<=i & i<=j & j<=clk   =>  wl(j) >= wl(i) - (DD*(j-i)) )
		@inv5 pump=TRUE => (!i,j . ct<=i & i<=j & j<=clk   =>  wl(j) >= wl(i) )
		@inv15 btrue // lower bound on the water level at the most recent control time
		@inv9 pump = FALSE  =>  wl(ct) >= L + (DD*C) + DD
		@inv10 btrue
		@inv11 pump=TRUE => (!i,j . ct<=i & i<=j & j<=clk   =>  wl(j) <= wl(i) + (DU*(j-i)) )
		@inv12 pump=FALSE => (!i,j . ct<=i & i<=j & j<=clk   =>  wl(j) <= wl(i) )
		@inv13 pump=TRUE  =>  wl(ct) <= H - (DU*C) - DU
		
  Initialisation ==
    @act1 wl := { 0 |-> L }
    @act2 clk := 0
    @act3 ct := 0
    @act6 pump := TRUE
  
  Event levelDecrease == // Models increase in water level on one time unit
		any 
		  l 
    where
      @grd2 clk < ct+C // ensure time will not go ahead of control deadline
      @grd6 pump=FALSE // pump is off
      @grd4 wl(clk)-DD <= l // lower bound on new water level
      @grd5 l <= wl(clk) // upper bound on new water level
    then
      @act1 wl(clk+1) := l
      @act2 clk := clk+1
	  end

  Event levelIncrease == // Models decrease in water level on one time unit
    any l
    where
      @grd2 clk < ct+C
      @grd6 pump=TRUE
      @grd4 wl(clk) <= l
      @grd5 l <= wl(clk) + DU
    then
      @act1 wl(clk+1) := l
      @act2 clk := clk+1
    end
	
  Event controlLow == // Control action taken when water level is below LT
    any a 
      /* Control decision is based on a  sample
      taken at a slightly earlier time (clk-a) */
		  
    where
      @grd4 0<=a & a<=A
      @grd3 a <= clk // ensure clk−a >= 0
      @grd6 ct <= clk - a
      @grd5 wl(clk-a) <= LT
    then
      @act1 ct := clk
      @act4 pump := TRUE
    end
    
  Event controlMed == // Control action taken when water level is between LT and HT
    any a 
    where
      @grd4 0<=a & a<=A // wl was sensed 'a' time units earlier
      @grd3 a <= clk // clk−a ∈ dom(wl)
      @grd7 ct<=clk - a
      @grd5 pump=FALSE   =>   wl(clk-a) > LT
      @grd6 pump=TRUE   =>   wl(clk-a) < HT
    then
      @act1 ct := clk
    end
	
  Event controlHigh == // Control action taken when water level is above HT
    any a 
    where
      @grd4 0<=a & a<=A // wl was sensed 'a' time units earlier
      @grd3 a <= clk // clk−a ∈ dom(wl)
      @grd6 ct <= clk-a
      @grd5 wl(clk-a) >= HT
    then
      @act1 ct := clk
      @act4 pump := FALSE
    end
End
