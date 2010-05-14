Machine Regs // Model by John Colley, June 16, 2009
	
	Sees 
		regs
	
	Variables 
		Regs 
		WBop 
		EXop 
		EXALUoutput 
		IDop 
		A 
		B 
	
  Invariants
    Regs : Register --> INT 
    WBop : Opcode
    EXop : Opcode
    EXALUoutput : INT
    IDop : Opcode
    A : INT
    B : INT

  Initialisation ==
    Regs := Register ** {0}
    WBop := NOP
    EXop := NOP
    EXALUoutput := 0
    IDop := NOP
    A := 0
    B := 0

  Event IDnoRAWEXWBnoRAW ==
    any
      pppop 
    where
      pppop : Opcode
      IDop : ArithRROpcode
      EXop : ArithRROpcode
      Rr(EXop) /= Ra(IDop)
      Rr(EXop) /= Rb(IDop)
      Rr(EXop) /= Ra(pppop)
      Rr(EXop) /= Rb(pppop)
    then
      Regs(Rr(EXop)) := EXALUoutput
      WBop := EXop
      EXop := IDop
      EXALUoutput := A + B
      A := Regs(Ra(pppop))
      B := Regs(Rb(pppop))
      IDop := pppop
    end
  
  Event IDRAWaEXWBnoRAW ==
    any 
      pppop 
    where
      pppop : Opcode
      IDop : ArithRROpcode
      EXop : ArithRROpcode
      Rr(EXop) /= Ra(IDop)
      Rr(EXop) /= Rb(IDop)
      Rr(EXop) = Ra(pppop)
      Rr(EXop) /= Rb(pppop)
    then
      Regs(Rr(EXop)) := EXALUoutput
      WBop := EXop
      EXop := IDop
      EXALUoutput := A + B
      A := EXALUoutput
      B := Regs(Rb(pppop))
      IDop := pppop
    end
  
  Event IDRAWbEXWBnoRAW ==
    any 
      pppop 
    where
      pppop : Opcode
      IDop : ArithRROpcode
      EXop : ArithRROpcode
      Rr(EXop) /= Ra(IDop)
      Rr(EXop) /= Rb(IDop)
      Rr(EXop) /= Ra(pppop)
      Rr(EXop) = Rb(pppop)
    then
      Regs := Regs <+ {Rr(EXop) |-> EXALUoutput}
      WBop := EXop
      EXop := IDop
      EXALUoutput := A + B
      A := Regs(Ra(pppop))
      B := EXALUoutput
      IDop := pppop
    end

  Event IDRAWabEXWBnoRAW ==
    any
      pppop 
    where
      pppop : Opcode
      IDop : ArithRROpcode
      EXop : ArithRROpcode
      Rr(EXop) /= Ra(IDop)
      Rr(EXop) /= Rb(IDop)
      Rr(EXop) = Ra(pppop)
      Rr(EXop) = Rb(pppop)
    then
      Regs := Regs <+ {Rr(EXop) |-> EXALUoutput}
      WBop := EXop
      EXop := IDop
      EXALUoutput := A + B
      A := EXALUoutput
      B := EXALUoutput
      IDop := pppop
    end

  Event IDnoRAWEXWBRAWa ==
	  any 
      pppop 
	  where
      pppop : Opcode
	    IDop : ArithRROpcode
	    EXop : ArithRROpcode
	    Rr(EXop) = Ra(IDop)
	    Rr(EXop) /= Rb(IDop)
	    Rr(EXop) /= Ra(pppop)
	    Rr(EXop) /= Rb(pppop)
	  then
	    Regs := Regs <+ {Rr(EXop) |-> EXALUoutput}
	    WBop := EXop
	    EXop := IDop
	    EXALUoutput := EXALUoutput + B
	    A := Regs(Ra(pppop))
	    B := Regs(Rb(pppop))
	    IDop := pppop
    end

  Event IDRAWaEXWBRAWa ==
	  any 
      pppop 
	  where
      pppop : Opcode
	    IDop : ArithRROpcode
	    EXop : ArithRROpcode
	    Rr(EXop) = Ra(IDop)
	    Rr(EXop) /= Rb(IDop)
	    Rr(EXop) = Ra(pppop)
	    Rr(EXop) /= Rb(pppop)
	  then
		  Regs := Regs <+ {Rr(EXop) |-> EXALUoutput}
	    WBop := EXop
	    EXop := IDop
	    EXALUoutput := EXALUoutput + B
	    A := EXALUoutput
	    B := Regs(Rb(pppop))
	    IDop := pppop
    end

  Event IDRAWbEXWBRAWa ==
    any 
      pppop 
    where
      pppop : Opcode
      IDop : ArithRROpcode
      EXop : ArithRROpcode
      Rr(EXop) = Ra(IDop)
      Rr(EXop) /= Rb(IDop)
      Rr(EXop) /= Ra(pppop)
      Rr(EXop) = Rb(pppop)
	  then
		  Regs := Regs <+ {Rr(EXop) |-> EXALUoutput}
      WBop := EXop
      EXop := IDop
      EXALUoutput := EXALUoutput + B
      A := Regs(Ra(pppop))
      B := EXALUoutput
      IDop := pppop
    end

  Event IDRAWabEXWBRAWa ==
    any 
      pppop 
    where
      pppop : Opcode
      IDop : ArithRROpcode
      EXop : ArithRROpcode
      Rr(EXop) = Ra(IDop)
      Rr(EXop) /= Rb(IDop)
      Rr(EXop) = Ra(pppop)
      Rr(EXop) = Rb(pppop)
    then
		  Regs := Regs <+ {Rr(EXop) |-> EXALUoutput}
      WBop := EXop
      EXop := IDop
      EXALUoutput := EXALUoutput + B
      A := EXALUoutput
      B := EXALUoutput
      IDop := pppop
    end

  Event IDnoRAWEXWBRAWb ==
	  any 
      pppop 
	  where
      pppop : Opcode
	    IDop : ArithRROpcode
	    EXop : ArithRROpcode
	    Rr(EXop) /= Ra(IDop)
	    Rr(EXop) = Rb(IDop)
	    Rr(EXop) /= Ra(pppop)
	    Rr(EXop) /= Rb(pppop)
    then
		  Regs := Regs <+ {Rr(EXop) |-> EXALUoutput}
	    WBop := EXop
	    EXop := IDop
	    EXALUoutput := A + EXALUoutput
	    A := Regs(Ra(pppop))
	    B := Regs(Rb(pppop))
	    IDop := pppop
    end

  Event IDRAWaEXWBRAWb ==
    any 
      pppop 
    where
      pppop : Opcode
	    IDop : ArithRROpcode
      EXop : ArithRROpcode
      Rr(EXop) /= Ra(IDop)
      Rr(EXop) = Rb(IDop)
      Rr(EXop) = Ra(pppop)
      Rr(EXop) /= Rb(pppop)
    then
		  Regs := Regs <+ {Rr(EXop) |-> EXALUoutput}
      WBop := EXop
      EXop := IDop
      EXALUoutput := A + EXALUoutput
      A := EXALUoutput
      B := Regs(Rb(pppop))
      IDop := pppop
    end

  Event IDRAWbEXWBRAWb ==
    any 
      pppop 
    where
      pppop : Opcode
      IDop : ArithRROpcode
      EXop : ArithRROpcode
      Rr(EXop) /= Ra(IDop)
      Rr(EXop) = Rb(IDop)
      Rr(EXop) /= Ra(pppop)
      Rr(EXop) = Rb(pppop)
    then
		  Regs := Regs <+ {Rr(EXop) |-> EXALUoutput}
      WBop := EXop
      EXop := IDop
      EXALUoutput := A + EXALUoutput
      A := Regs(Ra(pppop))
      B := EXALUoutput
      IDop := pppop
    end

  Event IDRAWabEXWBRAWb ==
    any 
      pppop
    where
      pppop : Opcode
      IDop : ArithRROpcode
      EXop : ArithRROpcode
      Rr(EXop) /= Ra(IDop)
      Rr(EXop) = Rb(IDop)
      Rr(EXop) = Ra(pppop)
      Rr(EXop) = Rb(pppop)
    then
		  Regs := Regs <+ {Rr(EXop) |-> EXALUoutput}
      WBop := EXop
      EXop := IDop
      EXALUoutput := A + EXALUoutput
      A := EXALUoutput
      B := EXALUoutput
      IDop := pppop
    end

  Event IDnoRAWEXWBRAWab ==
    any 
      pppop
    where
      pppop : Opcode
      IDop : ArithRROpcode
      EXop : ArithRROpcode
      Rr(EXop) = Ra(IDop)
      Rr(EXop) = Rb(IDop)
      Rr(EXop) /= Ra(pppop)
      Rr(EXop) /= Rb(pppop)
    then
		  Regs := Regs <+ {Rr(EXop) |-> EXALUoutput}
      WBop := EXop
      EXop := IDop
      EXALUoutput := EXALUoutput + EXALUoutput
      A := Regs(Ra(pppop))
      B := Regs(Rb(pppop))
      IDop := pppop
    end

  Event IDRAWaEXWBRAWab ==
    any 
      pppop 
    where
      pppop : Opcode
      IDop : ArithRROpcode
      EXop : ArithRROpcode
      Rr(EXop) = Ra(IDop)
      Rr(EXop) = Rb(IDop)
      Rr(EXop) = Ra(pppop)
      Rr(EXop) /= Rb(pppop)
    then
		  Regs := Regs <+ {Rr(EXop) |-> EXALUoutput}
      WBop := EXop
      EXop := IDop
      EXALUoutput := EXALUoutput + EXALUoutput
      A := EXALUoutput
      B := Regs(Rb(pppop))
      IDop := pppop
    end

  Event IDRAWbEXWBRAWab ==
    any 
      pppop
    where
      pppop : Opcode
      IDop : ArithRROpcode
      EXop : ArithRROpcode
      Rr(EXop) = Ra(IDop)
      Rr(EXop) = Rb(IDop)
      Rr(EXop) /= Ra(pppop)
      Rr(EXop) = Rb(pppop)
    then
		  Regs := Regs <+ {Rr(EXop) |-> EXALUoutput}
      WBop := EXop
      EXop := IDop
      EXALUoutput := EXALUoutput + EXALUoutput
      A := Regs(Ra(pppop))
      B := EXALUoutput
      IDop := pppop
    end

  Event IDRAWabEXWBRAWab ==
    any 
      pppop
    where
      pppop : Opcode
      IDop : ArithRROpcode
      EXop : ArithRROpcode
      Rr(EXop) = Ra(IDop)
      Rr(EXop) = Rb(IDop)
      Rr(EXop) = Ra(pppop)
      Rr(EXop) = Rb(pppop)
    then
		  Regs := Regs <+ {Rr(EXop) |-> EXALUoutput}
      WBop := EXop
      EXop := IDop
      EXALUoutput := EXALUoutput + EXALUoutput
      A := EXALUoutput
      B := EXALUoutput
      IDop := pppop
    end

End
