	DEVICE ZXSPECTRUM48

;	This demo scrolls (Cobra Style) at 50 FPS using the Floating Bus trick
; 	on ll moels

GFX0 equ gfx
GFX1 equ gfx

	; Store source graphics in lower memory
	org $6000

gfxsource:


	defb %11111111,%11111111,%11111111,%11111111,%11111111,%10000000,%00000000,%00000000
	defb %11000000,%00000000,%00000000,%00000000,%00000001,%10000000,%00000000,%00000000
	defb %11001111,%11111111,%11111111,%11111111,%11111001,%10000000,%00000000,%00000000
	defb %11001111,%11111111,%11111111,%11111111,%11111001,%10000000,%00000000,%00000000
	defb %11001111,%11111111,%11111111,%11111111,%11111001,%10000000,%00000000,%00000000
	defb %11001111,%11111111,%11111111,%11111111,%11111001,%10000000,%00000000,%00000000
	defb %11000000,%00000000,%00000000,%00000000,%00000001,%10000000,%00000000,%00000000
	defb %11111111,%11111111,%11111111,%11111111,%11111111,%10000000,%00000000,%00000000

	; Start in upper memory 
	org $8000   
	
	; Start

AppEntry:

	; check ROM variable for +2A/+3

	; Note, the code in this listing is set up for +2A/+3
	; If test shows we are not, we meed to change the code

	ld a,($0b53)			; (2899) +2A/+3 ROMs return 126
	cp $7e					; If 126 ($7e) then +2A or +3
	jr z,AppEntry_go		; so do a different FL_BUS Check
							
							; http://sky.relative-path.com/zx/floating_bus.html
							; https://spectrumcomputing.co.uk/forums/viewtopic.php?f=6&t=646&start=40

	; We need to modify the 3 bytes LD A,($5800) to NOP,NOP,DEC HL
	ld a,$00
	ld (v_sync),a		; Add a NOP, purely so easier to disassemble
	ld hl,v_sync+2
	ld (hl),$2b				;opcode for the DEC HL instruction [6]

	; Now when looping ensure we jump to v_sync+2 (Not NOP,NOP but byte 3, DEC HL)
	ld (v_sync_sw+1),hl		;redirect the JP NZ,** to DEC HL
	
	ld a,$40				;change top half of port addr (must be in the $40–$7f range)
	ld (port1+1),a
	ld a,$ff				;change bottom half of port addr
	ld (port2+1),a
	ld a,6
	ld (fl_bus+1),a			; change border colour, yellow if 48/128/2 blue if 2A,2B,3

	; For example the code for 2A/+3 Should look like this:

	;port1:
	;	ld de,$470f			; ATTR color ($01000111=$47) into D, top half of port to read into E
							; For 48k, $4740 and 2A $470F
	;	v_sync:	
	;	ld a,($5800)		;[13]point to contended memory and fetch a "blanking" attr
							;or DEC HL [6] for the 48K/128K/+2
	;	ld a,e					;[4]top half of port into A
	;port2:
	;	in a,($fd)		;[11]read port formed by A (MSB) and FDh (LSB) into A ($FF for 48k)
	;	cp d			;[4]test for ATTR color
	;v_sync_sw:
	;	jp nz,v_sync		;[10]

	; For 48k and 128k, and +2

	;port1:
	;	ld de,$47FF			; ATTR color ($01000111=$47) into D, top half of port to read into E
							; For 48k, $4740 and 2A $470F
	;	v_sync:	
	;	NOP,NOP		; Note this code is redundant as it is only $2b (dec hl) we jump to,
							; purely to delay ($00,$00,$2b)
	;	v_sync_extra:
	;	dec hl
	;	ld a,e					;[4]top half of port into A
	;port2:
	;	in a,($ff)		;[11]read port formed by A (MSB) and FDh (LSB) into A ($FF for 48k)
	;	cp d			;[4]test for ATTR color
	;v_sync_sw:
	;	jp nz,v_sync_extra		;[10]

AppEntry_go:

	di                              ; Disable interrupts (We rely on Floating bus)

	;	Copy from lower ram to upper ram
	ld hl,gfxsource
	ld de,gfx
	ld bc,255
	ldir
	
	;	1 Copy unexpanded to all 8 slots
	ld hl,gfx
	ld de,gfx+256
	ld b,7

	
cpy1:
	push bc
	ld b,255
cpy2:
	ld a,(hl)
	ld (de),a
	inc l
	inc e
	djnz cpy2

	inc h
	inc d

	pop bc
	djnz cpy1

	;	2 Rotate

	;	HL = gfx to rotate
	;	A = number of rotations
	; 	shbit = byte to store offset

	ld hl,gfx
	ld a,7
	call ROTATE_BYTES

	ld hl,gfx+256
	ld a,6
	call ROTATE_BYTES

	ld hl,gfx+512
	ld a,5
	call ROTATE_BYTES

	ld hl,gfx+768
	ld a,4
	call ROTATE_BYTES

	ld hl,gfx+1024
	ld a,3
	call ROTATE_BYTES

	ld hl,gfx+1024+256
	ld a,2
	call ROTATE_BYTES

	ld hl,gfx+1024+512
	ld a,1
	call ROTATE_BYTES

	;ld hl,gfx+1024+768
	;ld a,1
	;call ROTATE_BYTES

	xor a
	ld (screenoffset),a


	ld bc,767                      ;
	ld hl,$5800                     ;
	ld (hl),%01001101
	ld de,$5801                     ;
	ldir

	; FL_BUS = 01000111

	ld a,%01000111                  ; Screen Black, bright white
	ld ($5860),a                    ;
	ld bc,18*32                     ;
	ld hl,$5860                     ;
	ld de,$5861                     ;
	ldir                            ;

	ld de,31-2
	ld hl,$5800
	ld b,24
ae1:
	ld (hl),0
	inc hl
	ld (hl),0
	
	add hl,de
	ld (hl),0
	inc hl
	ld (hl),0
	inc hl
	djnz ae1
	


;	Lets check the Floating Bus for the start of bright white
fl_bus:
	
	; Change the border to zero to show where we are
	ld a,1
    out ($fe),a                     ; border change

port1:
	ld de,$470f		;ATTR color ($01000111=$47) into D, top half of port to read into E
					; For 48k, $4740 and 2A $470F
v_sync:	
	ld a,($5800)		;[13]point to contended memory and fetch a "blanking" attr
						;or DEC HL [6] for the 48K/128K/+2
	ld a,e			;[4]top half of port into A
port2:
	in a,($fd)		;[11]read port formed by A (MSB) and FDh (LSB) into A ($FF for 48k)
	cp d			;[4]test for ATTR color
v_sync_sw:
	jp nz,v_sync		;[10]
; ----------------------------------------------------------------------------------

; Draw each row from top to bottom using modifying code

; Draw top Row (Part)
row0:                   xor a: out ($fe),a : ld hl,row0data : ld de,row1 : jp buff;
row1:                   ld hl,row1data : ld de,row2 : jp buff;
row2:                   ld hl,row2data : ld de,row3 : jp buff;
row3:                   ld hl,row3data : ld de,row4 : jp buff;
row4:                   ld hl,row4data : ld de,row5 : jp buff;
row5:                   ld hl,row5data : ld de,row6 : jp buff;
row6:                   ld hl,row6data : ld de,row7 : jp buff;
row7:                   ld hl,row7data : ld de,row8 : jp buff;
row8:                   ld hl,row8data : ld de,row9 : jp buff;
row9:                   ld hl,row9data : ld de,rowA : jp buff;
rowA:                   ld hl,rowAdata : ld de,rowB : jp buff;
rowB:                   ld hl,rowBdata : ld de,rowC : jp buff;
rowC:                   ld hl,rowCdata : ld de,rowD : jp buff;
rowD:                   ld hl,rowDdata : ld de,rowE : jp buff;
rowE:                   ld hl,rowEdata : ld de,rowF : jp buff;

; Draw bottom (Part)
rowF:                   ld hl,rowFdata : ld de,row10: jp buff;

row10:                  ld hl,row10data : ld de,row11: jp buff;

row11:                  ld hl,row11data : ld de,player: jp buff;


player:

	ld hl,scroll
	ld (Draw_Player_return+1),hl
	ld hl,$40A4
	ld (Draw_Player+1),hl

	jp Draw_Player

	 ;call pause
scroll:



	; call pause
	; Move by 1 pixels (GFX+256)
	ld a,(scrollposition)
	inc a
	and 7
	ld (scrollposition),a
	jp nz,fl_bus

	; Shift the screen
	ld a,(screenoffset)
	xor 1
	ld (screenoffset),a

	and 1
	jp nz,fl_bus

	
	ld a,(mapposition)
	dec a
	jp z,fl_bus
	ld (mapposition),a

	
	jp fl_bus

buff:
	ld (buff1+1),hl                 ;       Poke the locaion of the row data
	ld (jumper+1),de                ;       Poke where to jump to at the end of drawing a row
buff1:
	ld sp,$0000                     ;       Row Data

	pop hl                          ;       Get GFX location for tiles
	ld a,(scrollposition)
	add a,h
	ld h,a
	ld (spstore1+1),hl              ;       Store GFX

	pop hl                          ;       Get Screen Location for Row

	ld a,(screenoffset)				; 		Check to see if we need to shift by 1 char to the left
	and 255
	jr z,buff2
	dec l							; 		Move the column back one

buff2:
	
	ld (spstore2+1),hl              ;
	
	pop hl                          ;       Get location of level data (Push for each row, will be 256 long)

	ld a,(mapposition)
	add l
	ld l,a

	ld de,buffer                    ;       Copy from level data to buffer

	; TODO: Offset on 256 row poke above

	ldi                             ;       Copy 15 bytes (30 chars)
	ldi                             ;
	ldi                             ;
	ldi                             ;
	ldi                             ;
	ldi                             ;
	ldi                             ;
	ldi                             ;
	ldi                             ;
	ldi                             ;
	ldi                             ;
	ldi                             ;
	ldi                             ;
	ldi                             ;
	ldi                             ;

	ld a,8                          ;
blp1:

	ex af,af'                       ;;;

spstore1:
	ld sp,$0000                     ;
	pop af                          ;
	pop bc                          ;                          
	pop de                          ;
	pop hl							; Image or Blank Space
	ld (spstore1+1),sp              ; Store SP for GFX position
spstore2:
	ld sp,$0000                     ; Load screen address to push to

; Self modified push to screen

blanks:
	;ld hl,$0000
buffer:
	nop                             ; Buffer to write pushes to
	nop                             ;
	nop                             ;
	nop                             ;
	nop
	nop                             ;
	nop                             ;
	nop                             ;
	nop
	nop                             ;
	nop                             ;
	nop                             ;
	nop
	nop                             ;
	nop                             ;
	

	ld (spstore2+1),sp              ;  Next Line

	ld hl,$0000						; Copy SP to HL
	add hl,sp                        ;
	
add30:

	
	ld a,30
	add a,l
	ld l,a
	inc h
	ld (spstore2+1),hl              ;


	ex af,af'                       ; Loop 8 times
	dec a                           ;
	jr nz,blp1                      ;


jumper:

	jp $0000                        ; Repeat

pause:
	ld bc,60000
bb1:
	dec bc
	ld a,c
	or b
	jr nz,bb1

	ret

Draw_Player:

	ld hl,$507D                   ; Screen Position
	ld sp,willy                     ; row 8 would need 64 adding, 7,56 etc (Like Bipboi)
					; Then add 256 for each rotated frame (Add d,frame number 0-7)

	xor a
	ld (collision),a

	ld a,8

Draw_Player_loop
	ex af,af'

	
	pop de : pop bc

	ld a,(hl)
	and e
	jr nz,col
	ld a,(hl)
	or e
	ld (hl),a

	inc l

	ld a,(hl)
	and d
	jr nz,col
	ld a,(hl)
	or d
	ld (hl),a

	dec l

	inc h

	ld a,(hl)
	and c
	jr nz,col
	ld a,(hl)
	or c
	ld (hl),a

	inc l

	ld a,(hl)
	and b
	jr nz,col
	ld a,(hl)
	or b
	ld (hl),a

	dec l
	
	inc h
	
	ld a,h
	and 7
	jr nz,Draw_Player_next
 
	ld a,l
	add a,32
	ld l,a

	jr c,Draw_Player_next

	ld a,h
	sub 8
	ld h,a

Draw_Player_next
	ex af,af'
	dec a
	
	jr nz,Draw_Player_loop

	xor a
	ld ($5800),a
Draw_Player_return


	jp $0000

col:

		; Change the border to zero to show where we are
	ld a,%01101010
	ld ($5800),A

	jp Draw_Player_return
scrollposition					  DEFB 0
screenoffset					  DEFB 0

shbit							  DEFB 0

mapposition:					  DEFB 240

collision:						DEFB 0

;	HL = gfx to rotate
	;	A = number of rotations
	; 	shbit = byte to store offset

ROTATE_BYTES:

	EX AF,AF'
	XOR A
	LD (shbit),A
	EX AF,AF'
	
    LD B,A
SH5:
    PUSH HL
    PUSH BC

    LD B,8*8         ; 4 Columns in row4
SH2:
    LD A,(shbit)   ; Will be either %10000000 or %00000000
    LD C,A

; C holds the BIT if after shifting it exits
    LD A,(HL)      ; Get graphic byte (e.g %00111101)
    AND 1          ; Leave the lowest bit (e.g. %00000001)
    RRC A          ; Rotate Right and Carry (e.g. a = %10000000)
    LD (shbit),A    ; SHBIT stores a 128 or a 0 (e.g. %10000000)

    LD A,(HL)       ; Get the Byte to shift
    RR A            ; Rotate Right
    AND %01111111   ; Remove the left hand bit
    OR C            ; Add on the Shifted Bit

SH2C
    LD (HL),A       ; Put back into the graphic
	
	INC L
    ;ADD HL,DE       ;   Next line byte (48 bytes after this one, 1 column)

    DJNZ SH2        ; Repeat for all the columns


	POP BC
	POP HL

	XOR A
	LD (shbit),A

	DJNZ SH5        ; Repeat for the number of times it needs rotating

	RET

	; So we need to have the graphics unshifted
	; then copy unshifed to all 8 slots
	; then starting with the last to the first, rotate right 7,6,5,4,3,2,1

	; e.g. 	unshifted is a gfx
	; 		copy to gfx+256,gfx+512...gfx+(7*256)
	;		then starting at 8*256, call rotes with a=7 and hl=7*256
	;		then a=6 and hl = 6*256
	;		finally last one by 8

	; 	GFX are 0,left,middle1,middle2,end,0
	align 256
gfx:

	; So byte 0,1 are left,2,3 middle,4,5 are end, but middle needs to wrap internally
	;defb %00000001,%11111111,%11111111,%11111111,%11111111,%00000000
	;defb 1,0,1,0,1,0
	;defb 1,0,1,0,1,0
	;defb 1,0,1,0,1,0
	;defb 1,0,1,0,1,0
	;defb 1,0,1,0,1,0
	;defb 1,0,1,0,1,0
	;defb %00000001,%11111111,%11111111,%11111111,%11111111,%00000000
	
	


	; for example

	; 00000001 11111111 = first 2, we need to scroll 7 times but 

	; The middle 2 need to wrap around when scrolled

	; 00000001


	


	defs 2048


	align 256
willy:

	defb %00111100,%00000000
	defb %01111110,%00000000
	defb %11011111,%00000000
	defb %11111111,%00000000
	defb %11111111,%00000000
	defb %11111111,%00000000	
	defb %01111110,%00000000
	defb %00111100,%00000000

; Right to left, Column 28 to 2
	align 256                       ;







; The stack will be one less than the value specified here
; so if SP=$407F, Pushing a value will polace it at $407E and $407D

row0data:               defw GFX0 : defw $407F : defw row0leveldata+256
row1data:               defw GFX0 : defw $409F : defw row1leveldata+256

row2data:               defw GFX0 : defw $40BF : defw row2leveldata+256

row3data:               defw GFX0 : defw $40DF : defw row3leveldata+256

row4data:               defw GFX0 : defw $40FF : defw row4leveldata+256

; Next 3rd
row5data:               defw GFX0 : defw $481F : defw row5leveldata+256

row6data:               defw GFX0 : defw $483F : defw row6leveldata+256


row7data:               defw GFX0 : defw $485F : defw row7leveldata+256


row8data:               defw GFX0 : defw $487F : defw row8leveldata+256

row9data:               defw GFX0 : defw $489F : defw row9leveldata+256

rowAdata:               defw GFX0 : defw $48BF : defw rowAleveldata+256

rowBdata:               defw GFX0 : defw $48DF : defw rowBleveldata+256

rowCdata:               defw GFX0 : defw $48FF : defw rowCleveldata+256

; Next 3rd
rowDdata:               defw GFX0 : defw $501F : defw rowDleveldata+256

rowEdata:               defw GFX0 : defw $503F : defw rowEleveldata+256
rowFdata:               defw GFX0 : defw $505F : defw rowFleveldata+256

row10data:               defw GFX0 : defw $507F : defw row10leveldata+256

row11data:               defw GFX0 : defw $509F : defw row11leveldata+256





	align 256

row0leveldata:

	include "mapbit.asm"

	align 256
row1leveldata:          

	include "mapbit.asm"

	align 256
row2leveldata:        
	include "mapbit.asm"

	align 256

row3leveldata:         
	include "mapbit.asm"

	align 256
row4leveldata:         
	include "mapbit.asm"

	align 256
row5leveldata:         
	include "mapbit.asm"

	align 256
row6leveldata:         	

	include "mapbit.asm"

	align 256

row7leveldata:        	
	include "mapbit.asm"

	align 256

row8leveldata:        	
	include "mapbit.asm"

	align 256

row9leveldata:      	
	include "mapbit.asm"

	align 256

rowAleveldata:        	
	include "mapbit.asm"

	align 256

rowBleveldata:       	
	include "mapbit.asm"

	align 256

rowCleveldata:      	
	include "mapbit.asm"

	align 256

rowDleveldata:        	
	include "mapbit.asm"

	align 256

rowEleveldata:      	
	include "mapbit.asm"

	align 256

rowFleveldata:        	
	include "mapbit.asm"

	align 256


row10leveldata:        		
	include "mapbit.asm"

	align 256

row11leveldata:        
	include "mapbit.asm"

	align 256

row12leveldata:        
	include "mapbit.asm"


	


; These generate some output files




	savetap "./flscroll.tap",AppEntry
	;savesna "src/tests/willyscroll/flscroll.sna",AppEntry