; In reverse so starts with far right

	; PUSH AF = F5 = Left Side
	; PUSH BC = C5 = Middle
	; PUSH DE = D5 = Right Side
	; PUSH HL = E5 - BLANK

	; Remember row is in reverse, right to left
	
	defb $E5,$E5,$D5,$C5,$C5,$C5,$C5,$C5,$F5,$E5,$D5,$C5,$C5,$F5,$E5,$E5
	; 16
	defb $E5,$E5,$D5,$C5,$C5,$F5,$E5,$D5,$C5,$C5,$F5,$E5,$D5,$C5,$C5,$F5
	;32
	defb $E5,$E5,$D5,$C5,$C5,$F5,$E5,$D5,$C5,$C5,$F5,$E5,$D5,$C5,$C5,$F5
	;48
	defb $E5,$E5,$D5,$C5,$C5,$F5,$E5,$D5,$C5,$C5,$F5,$E5,$D5,$C5,$C5,$F5
	;64
	defb $E5,$E5,$D5,$C5,$C5,$F5,$E5,$D5,$C5,$C5,$F5,$E5,$D5,$C5,$C5,$F5
	;80
	defb $E5,$E5,$D5,$C5,$C5,$F5,$E5,$D5,$C5,$C5,$F5,$E5,$D5,$C5,$C5,$F5
	;96
	defb $E5,$E5,$D5,$C5,$C5,$F5,$E5,$D5,$C5,$C5,$F5,$E5,$D5,$C5,$C5,$F5
	;112
	defb $E5,$E5,$D5,$C5,$C5,$F5,$E5,$D5,$C5,$C5,$F5,$E5,$D5,$C5,$C5,$F5
	;128
	defb $E5,$E5,$D5,$C5,$C5,$F5,$E5,$D5,$C5,$C5,$F5,$E5,$D5,$C5,$C5,$F5
	; 144
	defb $E5,$E5,$D5,$C5,$C5,$F5,$E5,$D5,$C5,$C5,$F5,$E5,$D5,$C5,$C5,$F5
	; 160
	defb $E5,$E5,$D5,$C5,$C5,$F5,$E5,$D5,$C5,$C5,$F5,$E5,$D5,$C5,$C5,$F5
	;176
	defb $E5,$E5,$D5,$C5,$C5,$F5,$E5,$D5,$C5,$C5,$F5,$E5,$D5,$C5,$C5,$F5
	;192
	defb $E5,$E5,$E5,$E5,$E5,$E5,$E5,$E5,$E5,$E5,$E5,$E5,$E5,$E5,$E5,$E5
	;208
	defb $E5,$E5,$E5,$E5,$E5,$E5,$E5,$E5,$E5,$E5,$E5,$E5,$E5,$E5,$E5,$E5
	;224
	defb $E5,$E5,$D5,$C5,$C5,$C5,$C5,$C5,$C5,$C5,$C5,$C5,$C5,$C5,$C5,$F5
	;240
	defb $E5,$E5,$D5,$C5,$C5,$C5,$C5,$C5,$C5,$C5,$C5,$C5,$C5,$C5,$F5,$E5	; Start (Far Left)

