
6809v2 single-board computer

(c) 2018 David Wood

This system is a 3.68mhz 6809-based microcomputer with 128k RAM and 8K rom.  All I/O services are provided via a 68681 DUART.

This documentation is primarily a placeholder and will likely be improved as time goes on.

System memory map:

0000-DFFF	System RAM
E000-FEFF	System ROM  (ROMON=high) or System RAM(ROMON=low)
FF00-FF0F	68681 DUART (see below for I/O definitions)
FF10-FFDF	reserved: Don't use
FFE0-FFFF	System ROM  (ROMON=high) or System RAM(ROMON=low)

DUART I/O Map:
IP0		CTS port A
IP1		CTS port B
IP2		DCD port A (not routed)
IP3		DCD port B (not routed)
IP4		n/c
IP5		SD Data input

OP0		RTS port A
OP1		RTS port B
OP2		RAM A16
OP3		RAM A17
OP4		ROMON
OP5		SDCK
OP6		SDDO
OP7		SDCS
		NOTE: The 68681 sets all outputs high on reset
		NOTE: The 68681 inverts its output bits

The memory is normally provided via a 68128 standard 128kx8 sram.  A 256k sram can be fitted and fully mapped, or a 512k can be used and half of it can be mapped.
A jumper bridge under the SRAM will allow disconnecting A18 from ground.

Many thanks to Dolo@hackaday.IO for the sdcard sample code and permission for the author to use it as long as the code for the firmware remains free and open!

Directories:
kicad/		Kicad files used to generate the system
sbug/		S-BUG port to the 6809v2 SBC (finished)
tests/		Prototype test code
sdcard/		SD-Card code (not yet finished)

License: CC:SA license v4.0
I encourage the sharing of this information as long as some credit is given in the firmware and/or board design.

Please direct inquiries to jbevren@gmail.com.
