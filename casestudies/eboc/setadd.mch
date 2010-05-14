Machine SetAdd
	Variables
		n 
		s 
      
	Initialisation ==
		n := 0
		s := emptyset
    
	Event add ==
	  any
		  v 
	  where
      v : INT
		  v /: s
	  then
		  s := s \/ {v}
		  n := n + 1
    end
    
	Event rem ==
	  any
		  v 
	  where
		  v : s
	  then
	    s := s \ {v}
		  n := n - 1
    end

	Invariants
		@nisnat n >= 0,
		@prop     n = card(s)
	
End