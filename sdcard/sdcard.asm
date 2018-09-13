;**********************************************************
;*
;*	DOLO-1 HOMEBREW COMPUTER
;*	Hardware and software design by Dolo Miah
;*	Copyright 2014-18
;*  Free to use for any non-commercial purpose subject to
;*  full credit of original my authorship please!
;*
;*  SDCARD.S
;*  Low level SD card driver routines.  This module implements
;*  software bit banging through VIA 2 port B of an SD card
;*  interface.  So the card is clocked in software which is
;*  not great for performance but fast enough for my
;*  purposes.  I think we can get around 8.5KB/s raw sector
;*  read/write speed, translating to around 5.5KB/s of useful
;*  throughput using the filesystem.
;*
;**********************************************************

;**********************************************************
;* 
;*  Ported to the 6809 CPU by David 'jbevren' Wood
;* 
;*  Original algorhythms used with permission from Dolo Miah
;*   as long as the code remains open-sourced.
;* 
;*  Adapted for the 6809v2 SBC created in 2018 by jbevren
;* 
;* 
;* 
;* 
;* 
;* 
;* 
;* 
;* 
;**********************************************************

;*  TODO: Remove 6502 assembler-specific directives
;*  TODO: Define labels (see list below, list may be incomplete)
;*  TODO: Make the source usable on 80-column displays
;*  TODO: 
;*  TODO: 
;*  TODO: 
;*  TODO: 
;*  TODO: 
;*  TODO: 

;* REMINDER:  The output port bits are inverted:
;*              Use SOPR to set an output to 0,
;*              Use ROPR to set an output to 1.

;****************************************
;* 
;* Hardware defines for 6809v2 SBC
;* 
;****************************************

IOPAGE	equ  $ff00
IPR	equ  IOPAGE+$d
SOPR	equ  IOPAGE+$e
ROPR	equ  IOPAGE+$f

;****************************************
;* 
;* Hardware defines for 6809v2 SBC
;* 
;****************************************

SD_CK	equ 32 ; bit 5 (OPR)
SD_DO	equ 64 ; bit 6 (OPR)
SD_CS	equ 128; bit 7 (OPR)
SD_DI	equ 32 ; bit 5 (IPR)

;****************************************
;* 
;* Firmware defines for sbug/6809v2
;* 
;****************************************

MONITOR	equ $f800	; monitor startup
NEXTCMD	equ $f802	; get next monitor command
OUTCH	equ $f80a	; print one character
PDATA	equ $f80c	; print ctrl-d terminated string

;****************************************
;* init_sdcard
;* Input : None
;* Output : None
;* Regs affected : None
;****************************************
init_sdcard
	;_println sd_msg_initialising

	lda  #SD_CS		; lda #SD_CS		; Unselect device
	sta  ROPR		; tsb SD_REG
	lda  #SD_CK		; lda #SD_CLK		; Set clock low
	sta  SOPR		; trb SD_REG
	lda  #SD_DI		; lda #SD_MOSI		; DI/MOSI high
	sta  ROPR		; tsb SD_REG
				; ldx #8		; 8*0.125ms = 1ms
	;FIX: Delay 1ms+	; jsr long_delay


				; Set the card to SPI mode with 80 1-bits
				;  while CS is de-asserted.
	ldb  #8			; ldx #8		; 10 bytes of 0xff
	lda  #$ff		; lda #0xff
init_sd_pulse
	lbsr sd_sendbyte	; jsr sd_sendbyte	; Send the 0xff byte
	decb			; dex
	bne  init_sd_pulse	; bne init_sd_pulse
	lda  #SD_CS		; lda #SD_CS		; Unselect device
	sta  ROPR		; tsb SD_REG

				; Set card to idle state and start
				;  software reset
init_cmd0
	lbsr sd_sendcmd0	; jsr sd_sendcmd0
	cmpa #$ff		; cmp #0xff		; 0xff is not a valid response
	bne  init_acmd41	; bne init_acmd41
	bra  init_sdcard	; bra init_sdcard

				; HC: SDHC info from elm-chan.org

				; HC: Send CMD8 $1aa to check for SDHC
				; HC:  If CMD8 is rejected (resp=5) assume MMC
	ldb  #0			
	stb  is_sdhc		; pre-clear sdhc flag
	jsr  sd_sendcmd8
	cmpa #5			; r1=5 (illegal cmd)?
	beq  init_lowcap		;  assume lowcap-style
	cmpa #$aa		; check bit 30 (SDHC)
	bne  notsd		;  no.
	cmpb #$1		; 3.3v?
	bne  sd_fail		;  no, reject the card
				; HC:  If accepted check for SDHC:
				; HC:   send ACMD41 with HCS (bit 30) set.
	ldb  #$40		; set bit 30 for ACMD41
	bra  init_acmd41	; run init

notsdhc	cmpa #0			; r1=0 (accepted)?
	bne  fail		; no, fail out

				; Start init process for card MCU
init_lowcap
	ldb #$0			; lowcap card, send amc41=0
init_acmd41

	lbsr sd_sendcmd55	; jsr sd_sendcmd55

	lbsr sd_sendcmd41	; jsr sd_sendcmd41
	
	cmpa #0 		; cmp #0		; Was R1 = 0
	bne  init_acmd41	; bne init_acmd41	; Retry if not

				; HC:   read ocr via cmd58 and check ccs (bit30).  1=high capacity
				; NOTE: Set a flag somewhere to indicate SDHC as HC addresses
				;       data in blocks, while standard addresses in bytes.
	lbsr sd_sendcmd55
	lbsr sd_sendcmd58	; Read OCR and return the last byte (bits 24:31) in A
	anda #$40		; HC?
	bne  not_sdhc		;  No, keep is_sdhc at 0
	inc  is_sdhc		; set sdhc flag to tell other routines to use block addressing
not_sdhc			
	
				; Set block size to $200 (512 bytes)
init_cmd16
	lbsr sd_sendcmd16	; jsr sd_sendcmd16
	
	rts			; rts

;****************************************
;* sd_startcmd
;* Start a cmd frame by sending CS high to low
;* Input : None
;* Output : None
;* Regs affected : None
;****************************************
sd_startcmd
	pshs a			; pha
	lda  #$ff		; lda #0xff		; Send 0xff
	lbsr sd_sendbyte	; jsr sd_sendbyte	; Delay / synch pulses
	lbsr sd_sendbyte	; jsr sd_sendbyte	; With CS not asserted

	lda  #SD_CS		; lda #SD_CS		; Chip select bit
	sta  SOPR		; trb SD_REG		; Now set it low
	puls a			; pla
	rts			; rts

;****************************************
;* sd_endcmd
;* End a cmd frame by sending CS high
;* Input : None
;* Output : None
;* Regs affected : None
;****************************************
sd_endcmd
	pshs a			; pha
	lda  #SD_CS		; lda #SD_CS		; Chip select bit
	sta  ROPR		; tsb SD_REG		; First set it high
	puls a			; pla
	rts			; rts

;****************************************
;* sd_sendbyte
;* Low level byte send routine
;* Input : A = byte to send
;* Output : None
;* Regs affected : None
;****************************************
sd_sendbyte			; REMEMBER: output port bits are inverted on 6809v2
				; pha
	pshs a,b,y		; phy

				; sta tmp_a		; For shifting out
	ldy  #8			; ldy #8		; 8 bits to shift out
				; lda SD_REG		; Load the SD register to A
sd_shiftoutbit
	ldb  #SD_DO		; ora #SD_MOSI		; And initially set output bit to '1'
	asla			; asl tmp_a		; Unless the bit to transmit is '0'
	bcs  sd_shiftskiplo	; bcs sd_shiftskiplo	; so then EOR the bit back to 0
	stb  SOPR
				; eor #SD_MOSI
	bra  sd_shiftskiphi
sd_shiftskiplo
	stb  ROPR		; sta SD_REG		; Save data bit first, it seems, before clocking
sd_shiftskiphi	
	ldb  #SD_CK
	stb  ROPR		; inc SD_REG
	stb  SOPR		; dec SD_REG

	leay -1,y		; dey			; Count bits
	bne  sd_shiftoutbit	; bne sd_shiftoutbit	; Until no more bits to send

				; ply
	puls a,b,y		; pla

	rts			; rts

;****************************************
;* sd_getbyte
;* Low level get a byte
;* Input : A = response byte received
;* Output : None
;* Regs affected : None
;****************************************

sd_getbyte			; REMEMBER: output port bits are inverted on 6809v2
				; phy
	pshs b,x		; phx

				; lda SD_REG
	ldb  #SD_DO		; ora #SD_MOSI		; Set MOSI high
	stb  ROPR		; sta SD_REG
				; tay			; Same as A with clock high
				; iny
				; tax			; Same as A with clock low
	leax -1,s	; Get a temporary byte to use in place of tmp_a below
	ldb #SD_CK
	; Unroll the code almost 20% faster than slow version
	;FIXME: The cmpa : rol may need recoded- see if cmpa:rol will reliably configure the carry bit
	; bit 7
	stb  ROPR		; sty SD_REG
	lda  IPR		; lda SD_REG		; Sample SD card lines (MISO is the MSB)
	stb  SOPR		; stx SD_REG
	cmpa #SD_DI		; cmp #SD_MISO		; Trial subtract A-MISO, C=1 if A >= MISO else C=0
	rola ,x			; rol tmp_a		; Rotate carry state in to tmp_a
	; bit 6
	stb  ROPR		; sty SD_REG
	lda  IPR		; lda SD_REG		; Sample SD card lines (MISO is the MSB)
	stb  SOPR		; stx SD_REG
	cmpa #SD_DI		; cmp #SD_MISO		; Trial subtract A-MISO, C=1 if A >= MISO else C=0
	rola ,x			; rol tmp_a		; Rotate carry state in to tmp_a
	; bit 5
	stb  ROPR		; sty SD_REG
	lda  IPR		; lda SD_REG		; Sample SD card lines (MISO is the MSB)
	stb  SOPR		; stx SD_REG
	cmpa #SD_DI		; cmp #SD_MISO		; Trial subtract A-MISO, C=1 if A >= MISO else C=0
	rola ,x			; rol tmp_a		; Rotate carry state in to tmp_a
	; bit 4
	stb  ROPR		; sty SD_REG
	lda  IPR		; lda SD_REG		; Sample SD card lines (MISO is the MSB)
	stb  SOPR		; stx SD_REG
	cmpa #SD_DI		; cmp #SD_MISO		; Trial subtract A-MISO, C=1 if A >= MISO else C=0
	rola ,x			; rol tmp_a		; Rotate carry state in to tmp_a
	; bit 3
	stb  ROPR		; sty SD_REG
	lda  IPR		; lda SD_REG		; Sample SD card lines (MISO is the MSB)
	stb  SOPR		; stx SD_REG
	cmpa #SD_DI		; cmp #SD_MISO		; Trial subtract A-MISO, C=1 if A >= MISO else C=0
	rola ,x			; rol tmp_a		; Rotate carry state in to tmp_a
	; bit 2
	stb  ROPR		; sty SD_REG
	lda  IPR		; lda SD_REG		; Sample SD card lines (MISO is the MSB)
	stb  SOPR		; stx SD_REG
	cmpa #SD_DI		; cmp #SD_MISO		; Trial subtract A-MISO, C=1 if A >= MISO else C=0
	rola ,x			; rol tmp_a		; Rotate carry state in to tmp_a
	; bit 1
	stb  ROPR		; sty SD_REG
	lda  IPR		; lda SD_REG		; Sample SD card lines (MISO is the MSB)
	stb  SOPR		; stx SD_REG
	cmpa #SD_DI		; cmp #SD_MISO		; Trial subtract A-MISO, C=1 if A >= MISO else C=0
	rola ,x			; rol tmp_a		; Rotate carry state in to tmp_a
	; bit 0
	stb  ROPR		; sty SD_REG
	lda  IPR		; lda SD_REG		; Sample SD card lines (MISO is the MSB)
	stb  SOPR		; stx SD_REG
	cmpa #SD_DI		; cmp #SD_MISO		; Trial subtract A-MISO, C=1 if A >= MISO else C=0
	rola ,x			; rol tmp_a		; Rotate carry state in to tmp_a

	lda  ,x			; lda tmp_a		; Return response in A

				; plx
	puls b,x		; ply

				; rts


;****************************************
;* sd_getrespbyte
;* Low level get response routine
;* Input : A = response byte received
;* Output : None
;* Regs affected : None
;****************************************
sd_getrespbyte
	pshs b			; phx
	clrb			; ldx #0		; Try up to 256 times
sd_respff
	incb			; inx			; Retry counter
	beq  sd_resptimeout	; beq sd_resptimeout
	lbsr sd_getbyte		; jsr sd_getbyte
	cmpa #$ff		; cmp #0xff		; Keep reading MISO until not FF
	beq  sd_respff		; beq sd_respff
sd_resptimeout
	puls b			; plx
	rts			; rts

;****************************************
;* sd_busy
;* Low level busy check routine
;* Input : None
;* Output : None
;* Regs affected : None
;****************************************
sd_busy
	pshs a			; pha
sd_isbusy
	lbsr sd_getbyte		; jsr sd_getbyte
	cmpa #$ff		; cmp #0xff		; Keep reading MISO until FF
	bne  sd_isbusy		; bne sd_isbusy
	puls a			; pla
	rts			; rts

;****************************************
;* sd_waitforn0byte
;* Low level routine waits for card to be ready
;* Input : None
;* Output : None
;* Regs affected : None
;****************************************
sd_waitforn0byte
	lbsr sd_getrespbyte	; jsr sd_getrespbyte
	beq  sd_waitforn0byte	; beq sd_waitforn0byte	; Zero byte means not ready
	rts			; rts

;****************************************
;* sd_sendcmd0: GO_IDLE_STATE (software reset)
;* Send CMD0
;* Input : None
;* Output : None
;* Regs affected : None
;****************************************
sd_sendcmd0
	lbsr sd_startcmd		; jsr sd_startcmd

	; Send 0x40, 0x00, 0x00, 0x00, 0x00, 0x95
	lda  #$40		; lda #0x40
	lbsr sd_sendbyte	; jsr sd_sendbyte
	clra			; lda #0x00
	lbsr sd_sendbyte	; jsr sd_sendbyte
	lbsr sd_sendbyte	; jsr sd_sendbyte
	lbsr sd_sendbyte	; jsr sd_sendbyte
	lbsr sd_sendbyte	; jsr sd_sendbyte
	lda  #$95		; lda #0x95		; Checksum needs to be right
	lbsr sd_sendbyte	; jsr sd_sendbyte

	lbsr sd_getrespR1	; jsr sd_getrespR1	; Get the response

	lbsr sd_endcmd		; jsr sd_endcmd
	
	rts			; rts

;****************************************
;* sd_sendcmd8: SEND_IF_COND (Check voltage range)
;* Send CMD16
;* Input : None
;* Output : vcc flag in b:0, $aa in A if sdcard
;*          5 in A if fail (error+idle response)
;* Regs affected : None
;****************************************
sd_sendcmd8
	lbsr sd_startcmd	; jsr sd_startcmd

	; Send 0x40+16, 0x00, 0x00, 0x02, 0x00, 0x95
	lda  #$40+16		; lda #0x40+16
	lbsr sd_sendbyte	; jsr sd_sendbyte
	clra			; lda #0x00
	lbsr sd_sendbyte	; jsr sd_sendbyte
	lbsr sd_sendbyte	; jsr sd_sendbyte
	inca
	lbsr sd_sendbyte	; jsr sd_sendbyte
	lda  #$aa
	lbsr sd_sendbyte	; jsr sd_sendbyte
	lda  #$95		; lda #0x95		; Checksum needs to be FIXed.
	lbsr sd_sendbyte	; jsr sd_sendbyte

	lbsr sd_getrespR1	; jsr sd_getrespR1	; Get the response
	cmpa #0						; cmd8 accepted? (FIX: should this be 1 (idle)?)
	bne  sd_endcmd8					;  no, exit

	lbsr sd_getbyte					; discard
	lbsr sd_getbyte					; discard
	lbsr sd_getbyte					; discard
	xfr  a,b					; 3.3v flag to b (bit0)
	lbsr sd_getbyte					; save this one; bit6 ~=bit 30 of response (HC bit)

sd_endcmd8
	lbsr sd_endcmd		; jsr sd_endcmd

	
	rts			; rts

;****************************************
;* sd_sendcmd55: APP_CMD (ACMD header)
;* Send CMD55
;* Input : None
;* Output : None
;* Regs affected : None
;****************************************
sd_sendcmd55
	lbsr sd_startcmd	; jsr sd_startcmd

	; Send 0x40+55, 0x00, 0x00, 0x00, 0x00, 0x95
	lda  #$77		; lda #0x40+55 FIX: Does a09 do this math right?
	lbsr sd_sendbyte	; jsr sd_sendbyte
	clra			; lda #0x00
	lbsr sd_sendbyte	; jsr sd_sendbyte
	lbsr sd_sendbyte	; jsr sd_sendbyte
	lbsr sd_sendbyte	; jsr sd_sendbyte
	lbsr sd_sendbyte	; jsr sd_sendbyte
	lda  #$95		; lda #0x95		; Checksum needs to be right
	lbsr sd_sendbyte	; jsr sd_sendbyte

	lbsr sd_getrespR1	; jsr sd_getrespR1	; Get the response

	lbsr sd_endcmd		; jsr sd_endcmd
	
	rts			; rts

;****************************************
;* sd_sendcmd41: APP_SEND_OP_COND (send operating requirements)
;* Send ACMD41
;* Input : None
;* Output : None
;* Regs affected : None
;****************************************
sd_sendcmd41
	lbsr sd_startcmd	; jsr sd_startcmd

	; Send 0x40+41, 0x00, 0x00, 0x00, 0x00, 0x95
	lda  #$40+41		; lda #0x40+41
	lbsr sd_sendbyte	; jsr sd_sendbyte	; cmd
	clra			; lda #0x00
	lbsr sd_sendbyte	; jsr sd_sendbyte	; bits  0- 7
	lbsr sd_sendbyte	; jsr sd_sendbyte	;       8-15
	lbsr sd_sendbyte	; jsr sd_sendbyte	;      16-23
	tfr  b,a					; set config byte
	lbsr sd_sendbyte	; jsr sd_sendbyte	;      24-31
	lda  #$95		; lda #0x95		; Checksum needs to be right
	cmpb #0			; low-cap init?
	beq  sd_41_lc		;  yes.
	lda  #$aa		; fix the checksum byte
	lbsr sd_sendbyte	; jsr sd_sendbyte

	lbsr sd_getrespR1	; jsr sd_getrespR1	; Get the response

	lbsr sd_endcmd		; jsr sd_endcmd
	
	rts			; rts

;****************************************
;* sd_sendcmd16: SET_BLOCKLEN (Set r/w block size)
;* Send CMD16
;* Input : None
;* Output : None
;* Regs affected : None
;****************************************
sd_sendcmd16
	lbsr sd_startcmd	; jsr sd_startcmd

	; Send 0x40+16, 0x00, 0x00, 0x02, 0x00, 0x95
	lda  #$40+16		; lda #0x40+16
	lbsr sd_sendbyte	; jsr sd_sendbyte
	clra			; lda #0x00
	lbsr sd_sendbyte	; jsr sd_sendbyte
	lbsr sd_sendbyte	; jsr sd_sendbyte
	lda #$02		; lda #0x02		; 0x200 block size = 512 bytes
	lbsr sd_sendbyte	; jsr sd_sendbyte
	clra			; lda #0x00
	lbsr sd_sendbyte	; jsr sd_sendbyte
	lda #$95		; lda #0x95		; Checksum needs to be right
	lbsr sd_sendbyte	; jsr sd_sendbyte

	lbsr sd_getrespR1	; jsr sd_getrespR1	; Get the response

	lbsr sd_endcmd		; jsr sd_endcmd
	
	rts			; rts

;****************************************
;* sd_getrespR1
;* Low level get response R1
;* Input : A = response byte received
;* Output : None
;* Regs affected : None
;****************************************
sd_getrespR1
	lbsr sd_getrespbyte	; jsr sd_getrespbyte
	rts			; rts

;****************************************
;* sd_sendcmd17: READ_SINGLE_BLOCK (Read)
;* Send CMD17
;* Input : dy=sector address, x=buffer address
;* Output : a=response from card
;* Regs affected : None
;****************************************
sd_sendcmd17
		; 6809 note: These are pushed in a specific order on purpose, do NOT optimize.
	pshs x,y		; phx
	pshs d			; pha			; A is the page to write to

	lbsr sd_startcmd	; jsr sd_startcmd

	; Convert sector address to byte address
	; Sector address is little endian
	; Byte address is big endian
				; stz sd_addr+3		; LSB of address is always 0
	
				; lda sd_sect+0		; LSB of sector goes to address+1
				; sta sd_addr+2		; Equivalent of * 256
				; lda sd_sect+1
				; sta sd_addr+1
				; lda sd_sect+2
				; sta sd_addr+0
	
				; clc			; Now addr*2 so equiv to sect*512
				; asl sd_addr+3
				; rol sd_addr+2
				; rol sd_addr+1
				; rol sd_addr+0

sd_cmd17addr	;FIX: Check/set endianness
	; Send 0x40+17, dl, dh, yl, yh, 0x95
	lda  #$40+17		; lda #0x40+17
	puls d						; recover sector address
	pshs b						; save B
	lbsr sd_sendbyte	; jsr sd_sendbyte
				; lda sd_addr+0		; First byte already in A
	lbsr sd_sendbyte	; jsr sd_sendbyte
	tfr  b,a		; lda sd_addr+1		; Second in B
	lbsr sd_sendbyte	; jsr sd_sendbyte
	tfr  y,d		; lda sd_addr+2		; third in Y low
	lbsr sd_sendbyte	; jsr sd_sendbyte
	tfr  b,a		; lda sd_addr+3		; fourth from Y high
	lbsr sd_sendbyte	; jsr sd_sendbyte
	lda  #$95		; lda #0x95		; Checksum needs to be right
	lbsr sd_sendbyte	; jsr sd_sendbyte

	lbsr sd_getrespbyte	; jsr sd_getrespbyte
	tfr a,b			; tax			; Save response in X for return

				; pla			; Get the A param
	lbsr sd_getrespR17	; jsr sd_getrespR17	; Get the response

	lbsr sd_busy		; jsr sd_busy		; Wait for card to be ready
	
	lbsr sd_endcmd		; jsr sd_endcmd

	tfr  b,a		; txa			; Restore the response byte
	
	puls b
	puls x,y		; plx
	
	rts			; rts

;****************************************
;* sd_getrespR17
;* Low level get response R17
;* Input : A = R1 response byte received
;* Output : None
;* Regs affected : None
;****************************************
sd_getrespR17
	pshs d,x,y		; pha
				; phy
				; sta tmp_ahi		; Page to read in to
				; stz tmp_alo		; Always a page boundary
sd_getrespR17token
	lbsr sd_getbyte		; jsr sd_getbyte	; Get a byte
	cmpa #$fe		; cmp #0xfe		; Is it the token?
	bne  sd_getrespR17token	; bne sd_getrespR17token; No
	
	ldy  #512		; ldy #0		; read 1st 256 bytes
sd_getrespR17block1
	lbsr sd_getbyte		; jsr sd_getbyte	; get a byte
	sta  ,x+		; sta (tmp_alo),y	; Save the byte
				; iny			; Keep going
				; bne sd_getrespR17block1; Until all bytes read

				; inc tmp_ahi		; Next page
sd_getrespR17block2
				; jsr sd_getbyte	; get a byet
				; sta (tmp_alo),y	; Save the byte
	leay -1,y		; iny			; Keep going
	bne  sd_getrespR17block1; bne sd_getrespR17block2; Until all bytes read

	lbsr sd_getbyte		; jsr sd_getbyte	; CRC
	lbsr sd_getbyte		; jsr sd_getbyte	; CRC
	
	puls d,x,y		; ply
				; pla

	rts			; rts
	

;****************************************
;* sd_sendcmd24: WRITE_BLOCK (write)
;* Send CMD24
;* Input : dy=block address, x=buffer address
;* Output : None
;* Regs affected : None
;****************************************
sd_sendcmd24
	pshs x,y		; phy
	pshs d			; pha

	lbsr sd_startcmd	; jsr sd_startcmd

	; Convert sector address to byte address
	; Sector address is little endian
	; Byte address is big endian
				; stz sd_addr+3		; LSB of address is always 0
				; lda sd_sect+0		; LSB of sector goes to address+1
				; sta sd_addr+2		; Equivalent of * 256
				; lda sd_sect+1
				; sta sd_addr+1
				; lda sd_sect+3
				; sta sd_addr+0
				; clc			; Now addr*2 so equiv to sect*512
				; asl sd_addr+3
				; rol sd_addr+2
				; rol sd_addr+1
				; rol sd_addr+0

	; Send 0x40+24, 0xA0, dl, dh, yl, yh, 0x95
	lda  #$40+24		; lda #0x40+24
	lbsr sd_sendbyte	; jsr sd_sendbyte
	ldd  ,s						; get a copy of d from stack
				; lda sd_addr+0
	lbsr sd_sendbyte	; jsr sd_sendbyte
	tfr  b,a		; lda sd_addr+1
	lbsr sd_sendbyte	; jsr sd_sendbyte
	tfr  y,d		; lda sd_addr+2
	lbsr sd_sendbyte	; jsr sd_sendbyte
	tfr  b,a		; lda sd_addr+3
	lbsr sd_sendbyte	; jsr sd_sendbyte
	lda  #$95		; lda #0x95		; Checksum needs to be right
	lbsr sd_sendbyte	; jsr sd_sendbyte

	lbsr sd_getrespbyte	; jsr sd_getrespbyte	; Get response

	lbsr sd_getbyte		; jsr sd_getbyte
	
	lda  #$fe		; lda #0xfe		; Start of data token
	lbsr sd_sendbyte	; jsr sd_sendbyte

				; pla			; Retrieve the address high byte
				; sta tmp_ahi
				; stz tmp_alo		; Address is always page boundary

				; ldy #00
	ldy #512		; 512 bytes to move
sd_writeblock_1					; Send first 256 bytes
	lda  ,x+		; lda (tmp_alo), y
	lbsr sd_sendbyte	; jsr sd_sendbyte
				; iny
				; bne sd_writeblock_1
				; inc tmp_ahi		; Next page for second 256 bytes
sd_writeblock_2					; Send second 256 bytes
				; lda (tmp_alo), y
				; jsr sd_sendbyte
	leay -1,y		; iny
	bne sd_writeblock_1	; bne sd_writeblock_2

	lda #$aa		; lda #0xaa		; Arbitrary CRC bytes
	lbsr sd_sendbyte	; jsr sd_sendbyte
	lbsr sd_sendbyte	; jsr sd_sendbyte

	lbsr sd_getbyte		; jsr sd_getbyte	; Get data response byte
	pshs a			; pha			; Save it to return

sd_waitforwritecomplete
	lbsr sd_busy		; jsr sd_busy		; Wait for card to be ready
	
	lbsr sd_endcmd		; jsr sd_endcmd		; Release the card

	puls d			; pla
	puls x,y		; ply
	rts			; rts

	
sd_msg_initialising
	FCC "\rInitialising SD Card\r"
	FCB 4

sd_cmd55
	FCB $77, $00, $00, $00, $00, $95
sd_cmd58
	FCB $7a, $00, $00, $00, $00, $95
sd_acmd41
	FCB $69, $00, $00, $00, $00, $95
	
