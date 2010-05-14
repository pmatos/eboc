Machine M2
	Variables 
		x : INT
		k : BOOL	
	End
	
	Initialisation
		x, k :| x' = 0 .. 5 & k' = TRUE
	End

	Invariants
		x : 0 .. 10,
		k = TRUE
	End
End