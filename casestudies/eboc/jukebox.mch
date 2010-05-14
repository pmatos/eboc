Machine JukeboxM

  Sees 
    jukebox

  Variables
    credit
    play_output
    playset 

  Invariants
    credit >= 0
    credit <= limit
    play_output : Track
    playset : POW(Track)

  Initialisation ==
    credit := 0
    playset := {}
    play_output :| play_output : {t1, t2}

  Event pay == 
    any
      cc
    where
      cc > 0
    then
      credit := min({credit + cc, limit})
    end

  Event select_pay ==
    any
      tt
    where
      tt : Track
      credit > 0
    then
      credit := credit - 1
      playset := playset \/ {tt}
    end

  Event select_free ==
    any
      tt
    where
      tt : Track
      credit > 0
    then
      playset := playset \/ {tt}
    end
    
  Event play ==
    any
      tr 
    where
      tr : Track
      playset /= {}
      tr : playset
    then
      play_output := tr
      playset := playset \ {tr}
    end

  Event penalty_pay == 
    when
      credit > 0
    then
      credit := credit - 1
    end

  Event penalty_losetrack ==
    any
      pp 
    where
      playset /= {}
      pp : playset
    then
      playset := playset \ {pp}
    end
End
        
      