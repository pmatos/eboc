Machine ForAll

  Sees 
    ForAll

  Variables
    col1 oftype POW(COLOURS)
    col2 oftype POW(COLOURS)
    skip oftype INT

  Invariants
    ! cc . (cc : col1 => cc /: col2) & ! cc2 . (cc2 : col2 => cc2 /: col1)

  Initialisation ==
    skip := 0
    col1 := {red}
    col2 := {green}

  Event swap ==
    when
      btrue
    then
      col1 := col2
      col2 := col1
    end

  Event add ==
    any 
      x oftype COLOURS
    where
      x :/ col1
    then
      col1 := col1 \/ {x}
    end

  Event TotalCoverage2 ==
    where
      ! x . (x : COLOURS => (x : col1 or x : col2))
    then
      skip := skip + 1
    end

  Event TotalCoverage2 ==
    where
      {xcs | xcs : COLOURS & not (xcs : col1 or xcs : col2)} = {} 
    then
      skip := skip + 1
    end
      
  Event NotTotalCoverage ==
    where
      not(! x2 . (x2 : COLOURS => (x2 : col1 or x2 : col2)))
    then
      skip := skip + 1
    end

  Event NonEmpty ==
    where
      # x3 . (x3 : COLOURS & (x3 : col1 or x3 : col2)) 
    then
      skip := skip + 1
    end
    
  Event RemoveElement ==
    any 
      x oftype COLOURS
    where
      ! xx . (xx : col1 => x : col2)
    then
      col1 := col1 - {x}
      col2 := col2 - {x}
    end
    
End
      
  