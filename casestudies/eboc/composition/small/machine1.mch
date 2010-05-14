Machine M1	
	Variables
		x : INT
		y : BOOL
		z : POW(INT ** INT)
	End
	
	Initialisation
		x, y, z :| z' = {} & x' : 0 .. 10 & y' = bool(x' mod 2 = 0)
	End
	
	Invariants 
		x >= 0,
		x /: dom(z)
	End
End