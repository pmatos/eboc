Machine huffman_bug3
        
	Sees
		huffman
          
  Variables 
	  current_char 
	  char_absorbed
	  encoded_string
	  strpos 
	  char_encoded 
	  decpos 
	  level 
	  decoded_char 

	Invariants
    encoded_string : INT +-> BOOL
    current_char : alphabet
    char_absorbed : BOOL
    strpos : INT
    char_encoded : BOOL
    decpos : INT
    level : INT
    decoded_char : alphabet
		@levelBounds level : 0 .. 3
		@maininv     (char_absorbed = TRUE & char_encoded = TRUE) => (current_char = decoded_char)


	Initialisation ==
	  current_char := a
	  char_absorbed := TRUE
	  encoded_string := {}
	  strpos := 0
	  char_encoded := TRUE
	  decpos := 0
	  level := 0
	  decoded_char := a
    
	Event get_char ==
    any
		  char
    where
      char : INT
      char_absorbed = TRUE
		  char_encoded = TRUE
    then
		  current_char := char
		  char_absorbed := FALSE
		  char_encoded := FALSE
    end

	Event encode_e ==
    when
		  current_char = e
      char_absorbed = FALSE
		  char_encoded = FALSE
    then
		  encoded_string := encoded_string <+ {strpos |-> FALSE}
		  strpos := strpos + 1
		  char_encoded := TRUE
	  end

	Event encode_o ==
    when
		  current_char = o
		  char_absorbed = FALSE
		  char_encoded = FALSE
    then
		  encoded_string := encoded_string <+ {strpos |-> TRUE, strpos + 1 |-> TRUE}
		  strpos := strpos + 2
		  char_encoded := TRUE
	  end

	Event encode_a ==
    when
		  current_char = a
		  char_absorbed = FALSE
		  char_encoded = FALSE
    then
		  encoded_string := encoded_string <+ {strpos |-> TRUE, strpos + 1 |-> FALSE, strpos + 2 |-> FALSE}
		  strpos := strpos + 3
		  char_encoded := TRUE
	  end

	Event encode_u ==
    when
		  current_char = u
		  char_absorbed = FALSE
		  char_encoded = FALSE
    then
		  encoded_string := encoded_string <+ {strpos |-> TRUE, strpos + 1 |-> FALSE, strpos + 2 |-> TRUE, strpos + 3 |-> FALSE}
		  strpos := strpos + 4
		  char_encoded := TRUE
	  end

	Event encode_i ==
    when 
      current_char = i
		  char_absorbed = FALSE
		  char_encoded = FALSE
    then 
      encoded_string := encoded_string <+ {strpos |-> TRUE, strpos + 1 |-> FALSE, strpos + 2 |-> TRUE, strpos + 3 |-> TRUE}
		  strpos := strpos + 4
		  char_encoded := TRUE
    end

	Event level_0_0 ==
    when
		  level = 0
		  decpos : dom(encoded_string)	
		  encoded_string(decpos) = FALSE
		  char_absorbed = FALSE
		  char_encoded = TRUE
    then
		  decoded_char := e
		  decpos := decpos + 1
		  char_absorbed := TRUE
		  level := 0
	  end

	Event level_0_1 ==
    when	
      level = 0
		  decpos : dom(encoded_string)
		  encoded_string(decpos) = TRUE
		  char_absorbed = FALSE
		  char_encoded = TRUE
    then
		  level := level + 1
		  decpos := decpos + 1
    end

	Event level_1_0 ==
    when 
      level = 1
		  decpos : dom(encoded_string)
		  encoded_string(decpos) = FALSE
		  char_absorbed = FALSE
		  char_encoded = TRUE
    then
		  level := level + 1
		  decpos := decpos + 1
    end

	Event level_1_1 ==
    when
		  level = 1
		  decpos : dom(encoded_string)
		  encoded_string(decpos) = TRUE
		  char_absorbed = FALSE
		  char_encoded = TRUE
    then
		  decoded_char := o
		  decpos := decpos + 1
		  char_absorbed := TRUE
		  level := 0
    end

	Event level_2_0 ==
    when
		  level = 2
		  decpos : dom(encoded_string) 
		  encoded_string(decpos) = FALSE
		  char_absorbed = FALSE
		  char_encoded = TRUE
    then
		  decoded_char := o // BUG: should be a.
		  decpos := decpos + 1
		  char_absorbed := TRUE
		  level := 0
	  end

	Event level_2_1 ==
    when
		  level = 2
		  decpos : dom(encoded_string)
		  encoded_string(decpos) = TRUE
		  char_absorbed = FALSE
		  char_encoded = TRUE
    then
		  level := level + 1
		  decpos := decpos + 1
	  end
    
	Event level_3_0 ==
    when
		  level = 3
		  decpos : dom(encoded_string)
		  encoded_string(decpos) = FALSE
		  char_absorbed = FALSE
		  char_encoded = TRUE
    then
		  decoded_char := u
		  decpos := decpos + 1
		  char_absorbed := TRUE
		  level := 0
	  end

	Event level_3_1 ==
    when
		  level = 3
		  decpos : dom(encoded_string)
		  encoded_string(decpos) = TRUE
		  char_absorbed = FALSE
		  char_encoded = TRUE
    then
		  decoded_char := i
		  decpos := decpos + 1
		  char_absorbed := TRUE
		  level := 0
	  end


	
End