
6809v2 single-board computer

(c) 2018 David Wood

This system is a 3.68mhz 6809-based microcomputer with 128k RAM and 8K rom.  It is intended to run 6809-based operating systems that run within 64k of RAM such as FLEX or OS-9 Level I.

All I/O services are provided via a 68681 DUART, either via one of its serial ports or via the extra GPIO pins on the device.

This documentation is primarily a placeholder and will likely be improved as time goes on(TM). ;-)

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
IP5		SD Data input	Input from Micro SD

OP0		RTS port A
OP1		RTS port B
OP2		RAM A16		A16 to RAM
OP3		RAM A17		A17 to RAM (128k has CE2 here)
OP4		ROMON		High enables the 2764 eprom
OP5		SDCK		Clock output to Micro SD
OP6		SDDO		Data output to Micro SD
OP7		SDCS		Chip select output to Micro SD
		NOTE: The 68681 sets all outputs high on reset
		NOTE: The 68681 inverts its output port bits

A summary of programming-related information is also printed on the pcb.

The memory fitted with a standard 128k-256k sram.  A 512k device can be fitted but the board lacks sufficient hardware to bankswitch more than 256k.
A jumper bridge under the SRAM will allow disconnecting A18 from ground.

Directories:
kicad/		Kicad v<5.0 files used to design the board
sbug/		S-BUG port to the 6809v2 SBC (working)
tests/		Prototype test code segments
sdcard/		SD-Card code (not yet finished)

** Many thanks to Dolo@hackaday.IO for the sdcard sample code and permission for the author to use it as long as the code for the firmware remains free and open! **

License: CC:SA license v4.0
I encourage the sharing and adaptation of this design as long as some credit to me is given in the firmware and/or board design.

Reach me at jbevren@gmail.com


