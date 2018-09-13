

	ORG $ffe8
Reset	ldx #$ffff	; this changes for each iteration
	jmp [,x]

