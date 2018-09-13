

; assert sdcs (op7) to turn the LED on to indicate life.


	ORG $ffe0
	lda #$00	; all bits=gpio
	sta $ff0d	; OPCR
	lda #$80	; bit 7 = sdcs
	sta $ff0e	; ROPR
hcf	jmp hcf		; and stop

;aaand done.

;hand asm:
       ;0  1  2  3  4  5  6  7    8  9  a  b  c  d  e  f  
;ffe0: 86 00 97 ff 0d 86 80 97   ff 0e 0e ff 0a 00 00 00
;ff00: ff e0 ff e0 ff e0 ff e0   ff e0 ff e0 ff e0 ff e0
