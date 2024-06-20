; Commodore CBM2 Kernal 901244-04a
; modified for ACME assembling by Vossi 06/2024, last update 06/2024
; v1.1 special f-keys
; v1.2 full ramtest selection (fast test checks only byte $0002 in each page)
!cpu 6502
!ct pet		; Standard text/char conversion table -> pet = petscii
!to "kernal.bin", plain
; * switches
STANDARD_FKEYS	= 1	; Standard F-keys
FULL_RAMTEST	= 1	; Standard full and slow RAM-test
;PATCH04B	= 1	; include revision 04B patches
;SOLID_CURSOR	= 1	; solid "Atari style cursor"
; * constants
FILL		= $AA	; Fills free memory areas with $AA
; ########################################### INFO ################################################
; ROM-CHECKSUM-BYTE: cksume
; loop3 E12C = Main loop - wait for key input
; **************************************** DISCLAIMER *********************************************
;***************************************
;*                                     *
;* KK  K EEEEE RRRR  NN  N  AAA  LL    *
;* KK KK EE    RR  R NNN N AA  A LL    *
;* KKK   EE    RR  R NNN N AA  A LL    *
;* KKK   EEEE  RRRR  NNNNN AAAAA LL    *
;* KK K  EE    RR  R NN NN AA  A LL    *
;* KK KK EE    RR  R NN NN AA  A LL    *
;* KK KK EEEEE RR  R NN NN AA  A LLLLL *
;*                                     *
;***************************************
;
;***************************************
;* CBM KERNAL                          *
;*   MEMORY AND I/O DEPENDENT ROUTINES *
;* DRIVING THE HARDWARE OF THE         *
;* FOLLOWING CBM MODELS:               *
;*   B-SERIES (7XX)                    *
;* COPYRIGHT (C) 1983 BY               *
;* COMMODORE BUSINESS MACHINES (CBM)   *
;***************************************
;***************************************
;* THIS SOFTWARE IS FURNISHED FOR USE  *
;* USE IN THE CBM P-SERIES AND B-SERIES*
;* COMPUTERS.                          *
;*                                     *
;* COPIES THEREOF MAY NOT BE PROVIDED  *
;* OR MADE AVAILABLE FOR USE ON ANY    *
;* OTHER SYSTEM.                       *
;*                                     *
;* THE INFORMATION IN THIS DOCUMENT IS *
;* SUBJECT TO CHANGE WITHOUT NOTICE.   *
;*                                     *
;* NO RESPONSIBILITY IS ASSUMED FOR    *
;* RELIABILITY OF THIS SOFTWARE.  RSR  *
;*                                     *
;***************************************
;
; 6509  used to extend memory on bc2 & p2 systems
;   location - used to direct
;   $0000 -  execution register (4 bits)
;   $0001 -  indirect  register (4 bits)
;
;   these registers provide 4 extra high-order address control lines.  
;     on 6509 reset all lines are high.
;
; current memory map:
;   segment 15- $ffff-$e000  rom (kernal)
;               $dfff-$df00  i/o  6525 tpi2
;               $deff-$de00  i/o  6525 tpi1
;               $ddff-$dd00  i/o  6551 acia
;               $dcff-$dc00  i/o  6526 cia
;               $dbff-$db00  i/o  unused (z80,8088,68008)
;               $daff-$da00  i/o  6581 sid
;               $d9ff-$d900  i/o  unused (disks)
;               $d8ff-$d800  i/o  6845 80-col
;               $d7ff-$d000  80-col screen
;               $bfff-$8000  roms external (language)
;               $7fff-$4000  roms external (extensions)
;               $3fff-$2000  rom  external
;               $1fff-$1000  rom  internal
;               $0fff-$0400  unused
;               $03ff-$0002  ram (kernal/basic system)
;   segment 14- segment 8 open (future expansion)
;   segment 7 - $ffff-$0002  ram expansion (external)
;   segment 6 - $ffff-$0002  ram expansion (external)
;   segment 5 - $ffff-$0002  ram expansion (external)
;   segment 4 - $ffff-$0002  ram expansion
;   segment 3 - $ffff-$0002  ram expansion
;   segment 2 - $ffff-$0002  ram standard
;   segment 1 - $ffff-$0002  ram standard
;   segment 0 - $ffff-$0002  ram optional
;
; the 6509 registers appear in locations $0000 and $0001 in all segments of memory.
; ***************************************** ZEROPAGE **********************************************
	e6509		= $00		; 6509 execution bank reg
	i6509		= $01		; 6509 indirect bank reg
;
; $02-$8f BASIC zeropage 
;
; Kernal page zero variables
; Kernal indirect address variables
	fnadr		= $90		; Address of file name string
	sal		= $93		; Current load/store address
	sah		= $94		;   low, high, bank
	sas		= $95
	eal		= $96		; End of load/save
	eah		= $97
	eas		= $98
	stal		= $99		; Start of load/save
	stah		= $9A
	stas		= $9B
; Frequently used kernal variables
	status		= $9C		; I/O operation status
	fnlen		= $9D		; File name length
	la		= $9E		; Current logical index
	fa		= $9F		; Current first address
	sa		= $A0		; Current secondary address
	dfltn		= $A1		; Default input device
	dflto		= $A2		; Default output device
; Tape buffer pointer
	tape1		= $A3		; Address of tape buffer
; RS-232 input buffer
	ribuf		= $A6		; Input buffer
; Variables for kernal speed
	stkey		= $A9		; Stop key flag
	ctemp		= $A9		; used to reduce cassette read times 
	c3po		= $AA		; IEEE buffer flag
	snsw1		= $AA		; used to reduce cassette read times 
	bsour		= $AB		; IEEE character buffer 
; Cassette temps - overlays IPC buffer
	ipoint		= $AC		; next 2 bytes used for transx code
	syno		= $AC
	dpsw		= $AD
; next 18 bytes also used for monitor
	ptr1		= $AE		; index to pass1 errors
	ptr2		= $AF		; index to pass2 errors
	pcntr		= $B0
	firt		= $B1
	cntdn		= $B2
	shcnl		= $B3
	rer		= $B4
	rez		= $B5
	rdflg		= $B6
	flagt1		= $B7		; temp during bit read time
	shcnh		= $B7
	cmp0		= $B8
	diff		= $B9
	prp		= $BA
	ochar		= $BB
	prty		= $BC
	fsblk		= $BD
	mych		= $BE
	cdata		= $BF		; how to turn cassette timers on
; Monitor virtual registers - Place in these locations temporarly...
	pch		= $AE		; counter
	pcl		= $AF
	flgs		= $B0		; Processor status
	acc		= $B1		; Accumulator
	xr		= $B2		; X register
	yr		= $B3		; Y register
	sp		= $B4		; Stack pointer
	xi6509		= $B5		; Old indirection segment
	re6509		= $B6		; Return execution segment
	invh		= $B7		; User interrupt vector
	invl		= $B8
; Monitor indirect variables
	tmp0		= $B9		; Temp pointer
	tmp2		= $BB		; Temp pointer
; Other monitor variables
	tmpc		= $BD		; Place to save last cmd
	t6509		= $BE		; Temporary i6509
	ddisk		= $BF		; Default disk unit # for monitor
; Screen editor page zero variables
; Editor indirect variables
	pkybuf		= $C0		; Start adr of pgm key
	keypnt		= $C2		; Current pgm key buf
	sedsal		= $C4		; Scroll ptr
	sedeal		= $C6		; Scroll ptr
	pnt		= $C8		; Current character pointer
	; Editor variables for speed & size
	tblx		= $CA		; Cursor line
	pntr		= $CB		; Cursor column
	grmode		= $CC		; Graphic/text mode flag $00=graphic, $02=text
	lstx		= $CD		; Last character index
	lstp		= $CE		; Screen editor start position
	lsxp		= $CF		; Screen editor start row
	crsw		= $D0		; cr flag - cr pressed -> input from screen
	ndx		= $D1		; Index to keyd queue
	qtsw		= $D2		; Quote mode flag
	insrt		= $D3		; Insert mode flag
	config		= $D4		; Char before blink (petii)
	indx		= $D5		; last byte position on line (##234-02##244-02)
	kyndx		= $D6		; count of program key string
	rptcnt		= $D7		; Deelay tween chars
	delay		= $D8		; Delay to next repeat
	sedt1		= $D9		; Frequently used temp variables
	sedt2		= $DA
; Frequently used editor variables
	data		= $DB		; Current print data
	sctop		= $DC		; Top screen 0-24 of current window
	scbot		= $DD		; Bottom 0-24
	sclf		= $DE		; Left margin
	scrt		= $DF		; Right margin
	modkey		= $E0		; Keyscanner shift/control flags ($ff-nokey)
	norkey		= $E1		; Keyscanner normal key number ($ff-nokey)
; Screen editor usage
	bitabl		= $E2		; Wrap bitmap
; Free zero page space, 16 bytes
	zpend		= $E6
; ***************************************** ABSOLUTE **********************************************
; System stack area
	stack		= $0100		; Stack
	bad		= $0100		; Cassette bad address table
	stackp		= $01FF		; System Stack pointer transx code
; -------------------------------------------------------------------------------------------------
; $200 - $256 Basic's ROM page work area
	buf		= $0200		; Basic input buffer
; Basic RAM vectors
	ierror		= $0280         ; Basic error indirect
; -------------------------------------------------------------------------------------------------
; System RAM vectors
	cinv		= $0300		; IRQ vector
	cbinv		= $0302		; BRK vector
	nminv		= $0304		; NMI vector
	iopen		= $0306		; Open file vector
	iclose		= $0308		; Close file vector
	ichkin		= $030A		; Open channel in vector
	ickout		= $030C		; Open channel out vector
	iclrch		= $030E		; Close channel vector
	ibasin		= $0310		; Input from channel vector 
	ibsout		= $0312		; Output to channel vector
	istop		= $0314		; Check stop key vector
	igetin		= $0316		; Get from queue vector
	iclall		= $0318		; Close all files vector
	iload		= $031A		; Load from file vector
	isave		= $031C		; Save to file vector
	usrcmd		= $031E		; Monitor extension vector
	escvec		= $0320		; User ESC key vector
	ctlvec		= $0322		; unused control key vector
	isecnd		= $0324		; IEEE listen secondary address
	itksa		= $0326		; IEEE talk secondary address
	iacptr		= $0328		; IEEE character in routine
	iciout		= $032A		; IEEE character out routine
	iuntlk		= $032C		; IEEE bus untalk
	iunlsn		= $032E		; IEEE bus unlisten
	ilistn		= $0330		; IEEE listen device primary address
	italk		= $0332		; IEEE talk device primary address
; Kernal absolute variables
	lat		= $0334		; Logical file numbers / table
	fat		= $033E		; Device numbers / table
	sat		= $0348		; Secondary addresses / table
;
	lowadr		= $0352		; Start of system memory: low, high, bank
	hiadr		= $0355		; Top of system memory: low, high, bank
	memstr		= $0358		; Start of user memory: low, high, bank
	memsiz		= $035B		; Top of user memory: low, high, bank
	timout		= $035E		; IEEE timeout enable
	verck		= $035F		; load/verify flag
	ldtnd		= $0360		; Device table index
	msgflg		= $0361		; Message flag
	bufpt		= $0362		; Cassette buffer index
; Kernal temporary (local) variables
	t1		= $0363
	t2		= $0364 
	xsav		= $0365 
	savx		= $0366 
	svxt		= $0367 
	temp		= $0368 
	alarm		= $0369		; IRQ variable holds 6526 IRQ's
; Kernal cassette variables
	itape		= $036A		; Indirect for cassette code
	cassvo		= $036C		; Cassette read variable
	aservo		= $036D		; Flag1***indicates t1 timeout cassette read
	caston		= $036E		; How to turn on timers
	relsal		= $036F		; moveable start load address
	relsah		= $0370		; 
	relsas		= $0371		; 
	oldinv		= $0372		; restore user IRQ and i6509 after cassettes
	cas1		= $0375		; Cassette switch flag
; RS-232 information storage
	m51ctr		= $0376		; 6551 control image
	m51cdr		= $0377		; 6551 command image
	rsstat		= $037A		; perm. RS-232 status
	dcdsr		= $037B		; last DCD/DSR value
	ridbs		= $037C		; Input start index
	ridbe		= $037D		; Input end index
; Screen editor absolute
; $037E - $037F Block some area for editor
	pkyend		= $0380		; Program key buffer end address
	keyseg		= $0382		; Segment / bank of function key texts
	keysiz		= $0383		; Sizes of function key texts

	rvs		= $0397		; Reverse mode flag
	lintmp		= $0398		; Line # between in and out 
	lstchr		= $0399		; Last char printed
	insflg		= $039A		; Insert mode flag
	scrdis		= $039B		; Scroll disable flag
	bitmsk		= $039C		; Temorary bitmask
	fktmp		= $039C		;   also used for function key temporary
	keyidx		= $039D		; Index to programmables
	logscr		= $039E		; Logical/physical scroll flag
	bellmd		= $039F		; Bell on/off flag
	pagsav		= $03A0		; Temp RAM page
	tab		= $03A1		; Tabstop flags
	keyd		= $03AB		; Keyboard buffer
	funvec		= $03B5		; Vector: funktion key handler
	sedt3		= $03B7		; another temp used during function key listing ***** UNUSED *****
; $03B8 - $3F7 Free absolute space
	absend		= $03B8
; System warm start variables and vectors
	evect		= $03F8		; Warm start vector and flags 5 bytes
; -------------------------------------------------------------------------------------------------
; Free bank 15 RAM 1024 bytes
	ramloc          = $0400		; First free ram location
; -------------------------------------------------------------------------------------------------
; Kernal inter-process communication variables 
	ipbsiz		= 16            ; Ipc buffer size
	ipb		= $0800		; IPC buffer
	ipjtab		= ipb+ipbsiz	; IPC jump table
	ipptab		= $0910		; IPC param spec table
; Ipc buffer offsets
	ipccmd		= 0		; Ipc command
	ipcjmp		= 1		; Ipc jump address
	ipcin		= 3		; Ipc #input bytes
	ipcout		= 4		; Ipc #output bytes
	ipcdat		= 5		; Ipc data buffer (8 bytes max)
; *************************************** IO / EQUATES ********************************************
; Equates
	irom	= $F		; System bank
	id55hz	= 27		; 55hz value required by ioinit (P500 14)
	warm	= $A5		; Warm start flag
	winit	= $5A  		; Initialization complete flag
	llen	= 80            ; Screen length
	nrows	= 25            ; Screen length
	scxmax	= llen-1        ; Max column number
	scymax	= nrows-1       ; Max line number
	cursor = $60		; Init to full flashing cursor
	romimg = $00		; Init to normal image & char rom
	keymax	= 9             ; Keyboard buffer size - 1
	dblzer	= 89            ; Key code for double zero
	pgmkys	= 20            ; Number of progam keys
; Tape block types
	eot	= 5             ; End of tape
	blf	= 1             ; Basic load file
	bdf	= 2             ; Basic data file
	bdfh	= 4             ; Basic data file header
	bufsz	= 192           ; Buffer size
	cr	= $d            ; Carriage return
; ROM / RAM addresses
	basic	= $8000		; Start of ROM (language)
	chrrom	= $C000		; Character ROM
	scnram	= $D000		; Start of screen memory
	kernal	= $E000		; Start of ROM (kernal)
; 6545 VDC Video Display Controller
	vdc = $D800
	adreg = $0		; address register
	dareg = $1		; data register
; 6581 SID Sound interface device
	sid	= $DA00
	osc1	= $00		; base addresses osc1, osc2, osc3
	osc2	= $07
	osc3	= $0E
	freqlo	= $00		; osc registers
	freqhi	= $01
	pulsef	= $02
	pulsec	= $03
	oscctl	= $04
	atkdcy	= $05
	susrel	= $06
	fclow	= $15		; filter control
	fchi	= $16
	resnce	= $17
	volume	= $18
	potx	= $19		; pots, random number and env3 out
	poty	= $1A
	random	= $1B
	env3	= $1C
; 6526 CIA for inter-process communication
	ipcia	= $DB00
	; pra  = data port
	; prb0 = busy1 (1=>6509 off dbus)
	; prb1 = busy2 (1=>8088/z80 off dbus)
	; prb2 = semaphore 8088/z80
	; prb3 = semaphore 6509
	; prb4 = unused
	; prb5 = unused
	; prb6 = irq to 8088/z80 (lo)
	; prb7 = unused
	sem88	= $04	; prb bit2
	sem65	= $08	; prb bit3
; 6526 CIA Complex interface adapter - IEEE data / user
	; timer a: ieee local / cass local / music
	; timer b: ieee deadm / cass deadm / music
	;
	; pra0: ieee data1 / user
	; pra1: ieee data2 / user
	; pra2: ieee data3 / user
	; pra3: ieee data4 / user
	; pra4: ieee data5 / user
	; pra5: ieee data6 / user
	; pra6: ieee data7 / user
	; pra7: ieee data8 / user
	;
	; prb0: user / game 10
	; prb1: user / game 11
	; prb2: user / game 12
	; prb3: user / game 13
	; prb4: user / game 20
	; prb5: user / game 21
	; prb6: user / game 22
	; prb7: user / game 23
	;
	; flag: user / cassette read
	cia	= $DC00
	pra	= $0	; Data reg A
	prb	= $1	; Data reg B
	ddra	= $2	; Direction reg a
	ddrb	= $3	; Direction reg b
	talo	= $4	; Timer A low  byte
	tahi	= $5	; Timer A high byte
	tblo	= $6	; Timer B low  byte
	tbhi	= $7	; Timer B high byte
	tod10	= $8	; 10ths of seconds
	todsec	= $9	; Seconds
	todmin	= $A	; Minutes
	todhr	= $B	; Hours
	sdr	= $C	; Serial data register
	icr	= $D	; Interrupt control register
	cra	= $E	; Control register A
	crb	= $F	; Control register B
; 6551 ACIA RS-232 and network interface
	acia	= $DD00
	drsn	= $0	; Transmitt/receive data register
	srsn	= $1	; Status register
	cdr	= $2	; Command register
	ctr	= $3	; Control register
	; Equates
	dsrerr	= $40	; Data set ready error
	dcderr	= $20	; Data carrier detect error
	doverr	= $08	; Receiver outer buffer overrun
; 6525 TPI1 Triport interface device #1 - IEEE control / cassette / network / irq
	tpi1	= $DE00
	; pa0: ieee dc control (ti parts)
	; pa1: ieee te control (ti parts) (t/r)
	; pa2: ieee ren
	; pa3: ieee atn
	; pa4: ieee dav
	; pa5: ieee eoi
	; pa6: ieee ndac
	; pa7: ieee nrfd
	;
	; pb0: ieee ifc
	; pb1: ieee srq
	; pb2: network transmitter enable
	; pb3: network receiver enable
	; pb4: arbitration logic switch
	; pb5: cassette write
	; pb6: cassette motor
	; pb7: cassette switch
	;
	; irq0: 50/60 hz irq
	; irq1: ieee srq
	; irq2: 6526 irq
	; irq3: (opt) 6526 inter-processor
	; irq4: 6551
	; *irq: user devices
	; cb: nc
	; ca: graphics mode
	pa	= $0	; Port register A
	pb	= $1	; Port register B
	pc	= $2	; Port register C
	lir	= $2	; Interrupt latch register mc=1
	ddpa	= $3	; Data direction register A
	ddpb	= $4	; Data direction register B
	ddpc	= $5	; Data direction register C
	mir	= $5	; Interrupt mask register mc=1
	creg	= $6	; Control reg: #0 mc=IRQ mode / #1 ip= IRQ parity / #2-3 edge i3,i4	
	air	= $7	; Active interrupt register
	; Equates
	dc	= $01	; 75160/75161 control line
	te	= $02	; 75160/75161 control line
	ren	= $04	; Remote enable
	atn	= $08	; Attention
	dav	= $10	; Data available
	eoi	= $20	; End or identify
	ndac	= $40	; Not data accepted
	nrfd	= $80	; Not ready for data
	ifc	= $01	; Interface clear
	srq	= $02	; Service request
	
	rddb	= nrfd+ndac+te+dc+ren	;directions for receiver
	tddb	= eoi+dav+atn+te+dc+ren	;directions for transmitt
	
	eoist	= $40	; eoi status test
	tlkr	= $40	; device is talker
	lstnr	= $20	; device is listener
	utlkr	= $5F	; device untalk
	ulstn	= $3F	; device unlisten
	       
	toout	= $01	; timeout status on output
	toin	= $02	; timeout status on input
	eoist	= $40	; eoi on input
	nodev	= $80	; no device on bus.
	sperr	= $10	; verify error
	; Equates for c3p0 flag bits 6 and 7.
	slock	= $40	; screen editor lock-out
	dibf	= $80	; data in output buffer
; 6525 TPI2 Triport interface device #2
	tpi2	= $DF00
	; pa: kyrd out 8-15
	; pb: kybd out 0-7
	;
	; pc0: kybd in 0
	; pc1: kybd in 1
	; pc2: kybd in 2
	; pc3: kybd in 3
	; pc4: kybd in 4
	; pc5: kybd in 5
	; pc6: select for monitor high=ntsc  low=pal
	; pc7: select for head high=built-in low=monitor
; **************************************** COLD START *********************************************
!initmem FILL                   ; All unused memory filled with $AA
!zone cold
*= kernal
jmoncld:jmp monoff		; Monitor cold start
	nop
; ****************************************** EDITOR ***********************************************
;***************************************
;*                                     *
;* EEEEE DDD   IIIII TTTTT  OOO  RRRR  *
;* E     D  D    I     T   O   O R   R *
;* E     D   D   I     T   O   O R   R *
;* EEE   D   D   I     T   O   O RRRR  *
;* E     D   D   I     T   O   O R R   *
;* E     D  D    I     T   O   O R  R  *
;* EEEE  DDD   IIIII   T    OOO  R   R *
;*                                     *
;***************************************
;
;***************************************
;* CBM EDITOR FOR B/BX-SERIES SYSTEMS  *
;*   KEYBOARD AND SCREEN EDIT ROUTINES *
;* DRIVING THE HARDWARE OF THE         *
;* FOLLOWING CBM MODELS:               *
;*   B/BX-SERIES OR B128/256           *
;* COPYRIGHT (C) 1983 BY               *
;* COMMODORE BUSINESS MACHINES (CBM)   *
;***************************************
!zone editor
*= kernal+4
;****************************************
;
;  80 column cbm ii screen editor
;    with unlimited screen line wrap
;
;****************************************
; E004 Jump vector table
jcint:  jmp cint		; Init Screen editor, VIC, F-keys
jlp2:	jmp lp2			; Read a key from keyboard to A
jloop5:	jmp loop5		; Read character from screen to A
jprt:	jmp prt			; Print character from A on screen
jscror:	jmp scrorg		; Return screen dimensions to X, Y
jkey:	jmp key			; Keyboard scan
jmvcur:	jmp movcur		; jmp prend code removed...mov cursor
jplot:  jmp plot		; Get/set the cursor position to/from X, Y
jiobas:	jmp iobase		; Return CIA base address to X, Y
jescrt:	jmp escape		; Handle an escape sequence
jfunky:	jmp keyfun		; Get/set/list function keys
; -------------------------------------------------------------------------------------------------
; E025 Get/set the cursor position
plot:   bcs rdplt		; if C=1 get cursor position
; set cursor
	stx tblx		; store line, last line 
	stx lsxp
	sty pntr		; store column, last column
	sty lstp
	jsr stupt		; Change pointer to this new line
	jsr movcur		; move the cursor there
rdplt:  ldx tblx
	ldy pntr		; load column, row
nofunc: rts
; -------------------------------------------------------------------------------------------------
; E03A Return CIA base address
iobase: ldx #<cia
	ldy #>cia
	rts
; -------------------------------------------------------------------------------------------------
; E03F Return screen dimensions
scrorg: ldx #llen		; 80 columns
	ldy #nrows		; 25 rows
	rts
; -------------------------------------------------------------------------------------------------
; $E044 Screen editor init (editor, F-Keys, CRTC)
; Clear editor variables
cint:	lda #0
	ldx #zpend-keypnt-1	; erase all but fkey allocation $C2-$EF
cloop1: sta keypnt,x		; clear page 0 variables
	dex
	bpl cloop1
	
	ldx #absend-rvs-1	; $397-$3BF
cloop2:	sta rvs,x		; clear absolute variables but fkey allocations
	dex
	bpl cloop2
; init some variables	
	lda #$C
	sta delay		; init repeat ctr
	lda #cursor
	sta config		; set full flashing cursor
	lda #<dokeyf
	sta funvec		; set up indirecvct for function keys
	lda #>dokeyf
	sta funvec+1
; init F-keys
	lda pkybuf		; check if buffers are allocated
	ora pkybuf+1
	bne noroom		; yes...just reset the screen
	lda hiadr		; get end of key area
	sta pkyend
	lda hiadr+1
	sta pkyend+1
	lda #$40		; NO SENSE - will be overwritten in alocat 
	ldx #0
	ldy #2
	jsr aloca		; get 512 bytes at end of system memory $FEFF
	bcs noroom		; no room found...just reset the screen
	sta keyseg		; store bank for F-keys
	inx
	stx pkybuf		; save start address (returned X+1)
	bne room10
	iny
room10:	sty pkybuf+1		; save start address
kyset:	ldy #keyend-keydef	; load size of F-key texts
	jsr pagst2		; set up function key ram page (indirect segment)
kyset1:	lda keydef-1,y
	dey
	sta (pkybuf),y		; copy key texts to buffer
	bne kyset1

	jsr pagres    		; restore ram page (indirect segment)
	ldy #keydef-keylen	; 10 F-key length bytes
kyset2:	lda keylen-1,y
	sta keysiz-1,y		; copy F-key text length to $383
	dey
	bne kyset2
; init CRTC, screen
noroom:	jsr sreset		; set full screen window
	jsr txcrt		; set text mode/char rom
	jsr crtint		; initialize crt
; E0B3 Clear screen, cursor home
clsr:	jsr nxtd		; start at top of window
cls10:	jsr scrset		; set screen pointers
	jsr clrln		; clear the line
	cpx scbot		; done ?
	inx
	bcc cls10		; no
; E0C1 Cursor home
nxtd:	ldx sctop		; move to top
	stx tblx
	stx lsxp		; for input after home or clear
stu10:	ldy sclf		; left of the screen window
	sty pntr
	sty lstp
; E0CD Reset screen ptr to line begin
stupt:	ldx tblx		; get curent line index
; E0CF Set screen ptr to line X
scrset: lda ldtb2,x		; load start of screen line low
	sta pnt			; and store to screen, color RAM ptr
	lda ldtb1,x		; load high
	sta pnt+1		; and store to char pointer
	rts
; -------------------------------------------------------------------------------------------------
; EoDA movcur - move the cursor routine
;   watch out .y is now zapped
movcur:	ldy #15
	clc
	lda pnt
	adc pntr
	sty vdc+adreg
	sta vdc+dareg		; set low byte of cursor address
	dey
	sty vdc+adreg
	lda vdc+dareg		; get old high byte of cursor address
	and #$F8		; mask out 3 low order bits
	sta sedt1		; save it
	lda pnt+1
	adc #0
	and #$07		; get new low order bits
	ora sedt1		; add in masked bits
	sta vdc+dareg		; set high byte of cursor address
	rts
; -------------------------------------------------------------------------------------------------
; *** Input routines ***
; E0FE Remove character from queue
lp2:  	ldx kyndx		; are there any pgm keys
	beq lp3			; branch if not
	ldy keyidx		; get index to current char
	jsr pagst2		; set up function key ram page (indirect segment)
	lda (keypnt),y		; get current byt
	jsr pagres		; restore ram page (indirect segment)
	dec kyndx		; 1 byte down
	inc keyidx		; bump index to next char
	cli
	rts
; No F-key
lp3: 	ldy keyd		; get key from irq buffer
	ldx #0
lp1:  	lda keyd+1,x		; shift key buffer
	sta keyd,x
	inx
	cpx ndx			; shift till last key in buffer
	bne lp1
	dec ndx			; decrease key index
	tya			; return char in A
	cli
	rts
; -------------------------------------------------------------------------------------------------
; E129 Screen input - Main loop
loop4:	jsr prt			; print the character
loop3:	ldy #10
	lda config
	sty vdc+adreg
	sta vdc+dareg		; set cursor type
; wait for key input
loop3a:	lda ndx			; check key and pgm-key index
	ora kyndx
	beq loop3a		; loop - wait for key input
; key available
	sei			; disable interrupts
; y=10 from loop3
;	lda #$20		; ***** moved to patch 4a-1 - turn off cursor behind key input *****
;	sty vdc+adreg
;	sta vdc+dareg		; turn off cursor
; check key
	jsr lp2			; get key input
	cmp #$D
	bne loop4		; print char if not cr
	jsr patch4a1		; ***** patch4a-1 - turn off cursor *****
	nop
	nop
	nop
	nop
	nop
; return recognized
	sta crsw		; set cr flag - we pass chars now
	jsr fndend		; check nxt line for cont (double line?)
	stx lintmp		; save last line number of sentence
	jsr patch4a2		; ***** patch4a-2 - make space for clear insert flag *****
	sta insrt		; ***** patch4a-2 - clear insert flag *****
	sta qtsw		; clear quote mode
	ldy sclf		; retrieve from line start if left it
	lda lsxp		; input started row
	bmi lp80		; flag we left start line
	cmp tblx
	bcc lp80
	ldy lstp		; input started column
	cmp lintmp		; on start line
	bne lp70
	cpy indx		; past start column
	beq lp75		; ok if the same
lp70:	bcs clp2		; yes - null input
lp75:	sta tblx		; start from here on input
lp80:	sty pntr
	jmp lop5		; input a line
; -------------------------------------------------------------------------------------------------
; E179 Read character from screen
loop5:	tya
	pha
	txa
	pha
	lda crsw		; passing chars to input
	beq loop3		; no - buffer on screen
	bpl lop5		; not done - get next char
clp2:	lda #0			; input done clear flag
	sta crsw
	lda #$D			; pass a return
	bne clp7
lop5:	jsr stupt		; set pnt and user
	jsr get1ch		; get a screen char
; convert screencode to petscii
	sta data		; store screen code for bit#5,6,7 check temporary
	and #$3F		; clear bit#6,7 in A
	asl data		; check: scrcode bit#7->C
	bit data		; check: scrcode bit#6->N, #5->V (shiftet to left)
	bpl lop54		; skip if scrcode #6=0 x0x -> 00x
	ora #$80		; x1x -> 10x
lop54:	bcc lop52		; skip if scrcode #7=0 (not reverse)
	ldx qtsw
	bne lop53		; skip if bit#7=1 & quote on: 10x -> 00x, 11x -> 10x
				; if quote off or bit#7=0:
lop52:	bvs lop53		; skip if scrcode #5=1: 001 -> 001, 011 -> 101
	ora #$40		; 000 -> 010, 100 -> 110
lop53:	jsr qtswc
	ldy tblx		; on input end line ?
	cpy lintmp
	bcc clp00		; no
	ldy pntr		; on input end column ?
	cpy indx
	bcc clp00		; no
	ror crsw		; c=1 minus flags last char sent
	bmi clp1		; always

clp00:	jsr nxtchr		; at next char
clp1:	cmp #$DE		; a pi ?
	bne clp7		; no
	lda #$FF		; translate
clp7:	sta data
	pla
	tax
	pla
	tay
	lda data		; return petscii char in A
	rts
; -------------------------------------------------------------------------------------------------
; *** Test for quote mode ***
; E1CD Switch quote mode depending on in A
qtswc:	cmp #$22 ; "
	bne qtswl		; skip if no quote-char
	lda insrt		; if we are in insert mode...
	bne qtswi		; don't mess with quote mode...
	lda qtsw
	eor #$1			; toggle quoteswitch
	sta qtsw
qtswi:	lda #$22 ; "		; restore quote in A
qtswl:	rts
; -------------------------------------------------------------------------------------------------
; E1DE *** Output chars ***
nxt3:	bit rvs
	bpl nvs
	ora #$80
nvs:	ldx insrt
	beq nvsa
	dec insrt
nvsa:	bit insflg		; are we in auto insert mode?
	bpl nvs1		; branch if not
	pha			; save the char
	jsr insert		; make room for this char
	ldx #0
	stx insrt		; make sure we turn off insert mode.
	pla			; restore char
nvs1:	jsr dspp		; display the character
	cpy #69
	bne nvs2		; do not ring bell
	jsr bell		; ring end of line bell
nvs2:	jsr movchr		; move to next char pos
; -------------------------------------------------------------------------------------------------
; E206 ********* exit from prt *********
loop2:	lda data		; copy last char
	sta lstchr
	jsr movcur		; move the cursor
	pla
	tay
	lda insrt
	beq lop2
	lsr qtsw		; clear quote switch if in insert mode
lop2:	pla
	tax
	pla
	rts
; -------------------------------------------------------------------------------------------------
;********************************
; Display a character
;********************************
; E21A Write blank ($20) at cusor position
doblnk: lda #' '		; load blank
; write char a
dspp:	ldy pntr		; get char index
	jsr pagset		; set to rom page (indirect segment)
	sta (pnt),y		; put char on screen
	jsr pagres		; restore ram page (indirect segment)
	rts
; -------------------------------------------------------------------------------------------------
; E227 Subroutine to clear one line
;                  x = line number
;         clrln :  blank entire line
;         clrprt:  y = starting column position
clrln:	ldy sclf		; load left margin
	jsr clrbit		; make sure non-continued line
clrprt:	txa
	pha			; save X
	lda pntr
	pha			; remember column
	dey
clr10:	iny
	sty pntr
	jsr doblnk		; print a blank
	cpy scrt		; line completely blank?
	bne clr10		; branch if not

	pla
	sta pntr
	pla
	tax
	rts
; -------------------------------------------------------------------------------------------------
; E242 Grab a character from screen
get1ch: ldy pntr		; get char
; E244 Get char from column Y
getych:	jsr pagset		; set to rom page (indirect segment)
	lda (pnt),y		; get the character
	jsr pagres		; restore ram page (indirect segment)
	rts
; -------------------------------------------------------------------------------------------------
; E24D Set text/graphic mode (C=1 graphic)
ctext:	ldy #$10
	bcs crtset		; skip if graphic mode
txcrt:	ldy #0
crtset:	sty grmode		; set text/graphic mode
	lda tpi1+creg
	and #$EF		; mask out text/graphic bit
	ora grmode		; add new text/graphic bit
	sta tpi1+creg
	rts
; -------------------------------------------------------------------------------------------------
; E260 crtint  --  intialize crt
crtint:	ldy #17			; 18 byte table built in -1 for offset
	bit tpi2+pc		; check for type
	bmi crt10		; buit-in
	ldy #18+17		; values for ntsc
	bvs crt10
	ldy #18+18+17		; values for pal
crt10:	ldx #17
crt20:	lda atext,y
	stx vdc+adreg		; index to reg.
	sta vdc+dareg
	dey
	dex
	bpl crt20
	
	rts
; -------------------------------------------------------------------------------------------------
;**************************************************
;   Handle ram paging (indirect segment)
;**************************************************
; E27D Switch to rom segment
pagset:	pha
	lda #$3F		; for rom page (segment)
	bne pagsub
; E282 Switch to segment with key buffer
pagst2:	pha
	lda keyseg		; for function key page (segment)
pagsub:	pha
	lda i6509		; get current page (segment) number
	sta pagsav		; - and save it
	pla
	sta i6509		; switch to new indirect segment
	pla			; restore a-reg
	rts
; -------------------------------------------------------------------------------------------------
; E291 Restore indirect segment
pagres	pha			; save a-reg
	lda pagsav		; get saved ram page (segment) number
	sta i6509		; restore ram page number
	pla			; restore a-reg
	rts
; -------------------------------------------------------------------------------------------------
; E299 *** Print a char ***
prt:	pha
	cmp #$FF
	bne prt10
	lda #$DE		; convert pi character
prt10:	sta data		; save char
	txa			; save regs.
	pha
	tya
	pha
	lda #0			; clear cr flag
	sta crsw
	ldy pntr		; column we are in
	lda data
	and #$7F
	cmp #$20		; test if control character (< $20)
	bcc ntcn		; yes
	ldx lstchr		; was last char an esc
	cpx #$1B
	bne njt10		; no
	jsr sequen		; yes - do esc sequence
	jmp getout

njt10:	and #$3F		; no - make a screen char
njt20:	bit data
	bpl njt30		; skip ahead if normal set - 00 - 3f
	ora #$40		; convert a0 - bf to 60 - 7f & c0 - df to 40 - 5f
njt30:	jsr qtswc		; test for quote
	jmp nxt3		; put on screen
; ********* control keys *********
ntcn:	cmp #$0D 		; test if a return
	beq ntcn20		; no inverse if yes
	cmp #$14		; test if insert or delete
	beq ntcn20		; allow in insert or quote mode
	cmp #$1B		; test if escape key
	bne ntcn1
	bit data
	bmi ntcn1		; its a $9b
	lda qtsw		; test if in quote mode...
	ora insrt		; ...or insert mode
	beq ntcn20		; if not, go execute remaining code
	jsr toqm		; else go turn off all modes
	sta data		; and forget about this character
	beq ntcn20		; always
ntcn1:	cmp #$03		; test if a run/load or stop
	beq ntcn20		; no inverse if yes
	ldy insrt		; test if in insert mode
	bne ntcn10		; go reverse - if yes
	ldy qtsw		; check for quote mode
	beq ntcn20		; do not reverse if not
ntcn10:	ora #$80		; make reverse
	bne njt20
ntcn20:	lda data
	asl			; set carry if shifted ctrl
	tax
	jsr ctdsp		; indirect jsr
getout:	jmp loop2
; Control code dispatcher
ctdsp:	lda ctable+1,x		; hi byte
	pha
	lda ctable,x		; low byte
	pha
	lda data
	rts			; indirect jmp
; -------------------------------------------------------------------------------------------------
; E311 User control code jump vector
cuser:	jmp (ctlvec)
; -------------------------------------------------------------------------------------------------
; E314 Cursor down/up
cdnup:  bcs cup			; cursor up
; cursor down
cdwn:	jsr nxln
cdn10:	jsr getbit		; a wrapped line ?
	bcs cdrts		; skip if yes
	sec			; flag we left line
	ror lsxp

cdrts:  clc
	rts
; E334 Cursor up
cup:	ldx sctop		; cursor up
	cpx tblx		; at top of window ?
	bcs critgo		; yes - do nothing
cup10:	jsr cdn10		; about to wrap to a new line ?
	dec tblx		; up a line
	jmp stupt
; -------------------------------------------------------------------------------------------------
; E331 Cursor right/left
crtlf:  bcs cleft		; cursor left
; cursor right
crit:	jsr nxtchr		; cursor right
	bcs cdn10		; yes - test for wrap

critgo: rts
; E339 Cursor left
cleft:  jsr bakchr		; move back
	bcs critgo		; abort if at top left
	bne cdrts		; no - exit
	inc tblx
	bne cup10		; go set flag if needed
; -------------------------------------------------------------------------------------------------
; E344 RVS on/off
rvsf:   eor #$80
	sta rvs
	rts
; -------------------------------------------------------------------------------------------------
; E34A Home/clear
homclr:	bcc homes		; if C=0 home
	jmp clsr		; Clear screen, cursor home
; E34F Cursor home
homes:	cmp lstchr		; last char a home ?
	bne hm110		; no
	jsr sreset		; top=0,left=0,bot=nrows-1,rt=cols-1
hm110:  jmp nxtd		; set to top left
; -------------------------------------------------------------------------------------------------
; E35A Tab function
tabit:  ldy pntr
	bcs tabtog		; a tab toggle
tab1:	cpy scrt		; at right of window
	bcc tab2		; no - tab to next
	lda scrt		; set to screen right
	sta pntr
	rts

tab2:   iny			; find next tab stop
	jsr gettab
	beq tab1		; not yet !
	sty pntr
	rts
; E370 Toggle tabulator
tabtog: jsr gettab		; flip tab stop
	eor bitmsk
	sta tab,x
	rts
; -------------------------------------------------------------------------------------------------
; E37A skip to next line
;   wrap to top if scroll disabled
nxln:	ldx tblx
	cpx scbot		; of the bottom of window ?
	bcc nxln1		; no
	bit scrdis		; what if scrolling is disabled?
	bpl doscrl		; branch if scroll is enabled
	lda sctop		; wrap to top
	sta tblx
	bcs nowhop		; always

doscrl:	jsr scrup		; scroll it all
	clc			; indicate scroll ok
nxln1:	inc tblx
nowhop:	jmp stupt		; set line base adr
; -------------------------------------------------------------------------------------------------
; E394 a return or shift return
nxt1:   jsr fndend		; find the end of the current line
	inx
	jsr clrbit		; set next line as non-continued
	ldy sclf		; else point to start of next line
	sty pntr
	jsr nxln		; set up next line
;**************************************
; turn off all modes
;   include sound if enabled
;   expected to return zero
;**************************************
toqm:	lda #0
	sta insrt
	sta rvs
	sta qtsw
	cmp bellmd		; check for bell enabled
	bne toqmx		; no...
	sta sid+volume	; turn off sid
toqmx:	rts
; -------------------------------------------------------------------------------------------------
; ****** scroll routines ******
; E3B4 Move one line
movlin:	lda ldtb2,x		; set pointer to line address
	sta sedsal
	lda ldtb1,x
	sta sedsal+1

	jsr pagset		; set to rom page (indirect segment)
movl10:	lda (sedsal),y
	sta (pnt),y
	cpy scrt		; done a whole line ?
	iny
	bcc movl10		; no
	jmp pagres		; restore ram page (indirect segment)
; -------------------------------------------------------------------------------------------------
; E3CD ****** Scroll down ******
scrdwn:	ldx lsxp
	bmi scd30		; skip if new line flag already set
	cpx tblx
	bcc scd30		; skip if old line is below scroll area
	inc lsxp		; else inc start line number
scd30:	ldx scbot		; scroll down, start bottom

scd10:	jsr scrset		; set pnt to line
	ldy sclf
	cpx tblx		; test if at destination line
	beq scd20		; done if yes
	dex			; point to previous line as source
	jsr getbt1
	inx
	jsr putbt1		; move continuation byte
	dex
	jsr movlin		; move one line
	bcs scd10		; always

scd20:	jsr clrln		; set line to blanks
	jmp setbit		; mark as continuation line
; -------------------------------------------------------------------------------------------------
; E3F6 ****** Scroll up ******
scrup:	ldx sctop
scru00:	inx
	jsr getbt1		; find first non-continued line
	bcc scru15
	cpx scbot		; is entire screen 1 line?
	bcc scru00		; do normal scroll if not
	
	ldx sctop
	inx
	jsr clrbit		; clear to only scroll 1 line

scru15:	dec tblx
	bit lsxp
	bmi scru20		; no change if already new line
	dec lsxp		; move input up one
scru20:	ldx sctop
	cpx sedt2
	bcs scru30
	dec sedt2		; in case doing insert
scru30:	jsr scr10		; scroll
	ldx sctop
	jsr getbt1
	php
	jsr clrbit		; make sure top line is not continuation
	plp
	bcc scru10		; done if top line off
	bit logscr		; logical scroll ?
	bmi scru15		; yes - keep scrolling
scru10:	rts
;
scr10:	jsr scrset		; point to start of line
	ldy sclf
	cpx scbot		; at last line ?
	bcs scr40		; yes
	inx			; point to next line
	jsr getbt1
	dex
	jsr putbt1		; move continuation byte
	inx
	jsr movlin		; move one line
	bcs scr10
; Test for slow scroll
scr40:	jsr clrln		; make last line blank
	ldx #$FF
	ldy #$FE		; allow only output line 0
	jsr getlin		; get input
	and #$20		; check if interrupt i5 = control
	bne scr80		; if not skip ahead - not slow scroll
; Slow scroll delay loop
scr60:	nop	 		; yes - waste time
	nop
	dex
	bne scr60
	dey
	bne scr60

scr70:	sty ndx
scr75:	jmp keyxt2		; exit...setup lines for stop key check
; Scroll stop
scr80:	ldx #$F7		; allow only output line 11
	ldy #$FF
	jsr getlin		; get input lines key
	and #$10		; check for the commodore key
	bne scr75		; exit if not - no stop scroll
scr90:	jsr getlin		; get input lines
	and #$10		; check for the commodore key
	beq scr90		; wait until com.key not depressed
scr95:	ldy #0
	ldx #0			; allow all output lines
	jsr getlin		; get inputs
	and #$3F		; check for any input
	eor #$3F
	beq scr95		; wait
	bne scr70		; always
; Keyboard check for slow scroll
getlin:	php			; preserve the irq flag
	sei
	stx tpi2+pa		; set port-a output
	sty tpi2+pb		; set port-b outputs
	jsr getkey		; get port-c inputs
	plp
	rts
; -------------------------------------------------------------------------------------------------
; E48D ring the bell, if enabled
bell:	lda bellmd
	bne bellgo
	lda #$0F
	sta sid+24		; turn up volume
	ldy #00
	sty sid+5		; attack=0-decay=9
	lda #10
	sta sid+6		; sustain=0-release=0
	lda #48
	sta sid+1		; voice 1 freq.
	lda #96
	sta sid+15		; voice 3 freq.
	ldx #$15
	stx sid+4
bell10:	nop
	nop			; wait to reach sustain level
	iny
	bne bell10
	dex
	stx sid+4		; gate off
bellgo:	rts
; -------------------------------------------------------------------------------------------------
; E4BA ce - clear entry
;   always deletes last character entered
;   will delete all <#>s. (0 1 2 3 4 5 6 7 8 9 .)
;   will delete if (<#>e<+/->)
;   cursor must be next posistion beyond entry being deleted.
ce:	lda pntr		; get index on line
	pha			; save for final delete if necessary
cet0:	ldy pntr
	dey
	jsr getych		; get previous character
	cmp #43			; (+)
	beq cet1
	cmp #45			; (-)
	bne cet2
;
cet1:	dey			; try for an <#>e
	jsr getych
	cmp #5			; (e)
	bne cet4		; exit if not...it can only be an <#>e
;
cet2:	cmp #5			; (e)
	bne cet3
	dey
	jsr getych
;
cet3:	cmp #46			; try for a <#>
	bcc cet4		; (.)
	cmp #47
	beq cet4
	cmp #58			;  (0-9)
	bcs cet4
;
	jsr deleet
	jmp cet0
;
cet4:	pla			; check if any deletes occured
	cmp pntr
	bne bellgo		; yes...exit
	jmp deleet		; else... go delete a character
; -------------------------------------------------------------------------------------------------
; ****** wrap table subroutines *******
; E4F5 Check for a double length line
getbit:	ldx tblx		; load current line
; Check line X
getbt1:	jsr bitpos		; get byte & bit positions
	and bitabl,x		; check if bit for line is set in table
	cmp #1			; make carry clear if zero
	jmp bitout		; return 0 if not a double length line
; -------------------------------------------------------------------------------------------------
; E501 Mark current line as double length C=1, unmark C=0
; putbit - set bit according to carry
putbit:	ldx tblx		; load current line
; Mark line X
putbt1:	bcs setbit		; go if to mark as wrappped line
; clrbit - clear wrap bit
clrbit:	jsr bitpos		; get byte & bit positions
	eor #$FF		; invert bit position
	and bitabl,x		; clear bit
bitsav:	sta bitabl,x		; and store it to table at byte position X
bitout:	ldx bitmsk		; move byte table position to X
	rts
; setbit  -  set bit to mark as wrapped line
setbit:	bit scrdis		; auto line link disable...
	bvs getbt1		; branch if scrolling is disabled
	jsr bitpos		; get byte & bit position
	ora bitabl,x		; set wrap bit
	bne bitsav		; always
; Find bit table position for line X
; bitpos - get byte & bit position of wrap bit
;   input - x  = row number
;   output - x = byte number
;            a = bit mask
bitpos
	stx bitmsk		; remember line
	txa
	and #$07		; get bit position
	tax
	lda bits,x		; get bit mask
	pha			; remember it
	lda bitmsk
	lsr
	lsr			; shift to get byte position (/8)
	lsr
	tax			; move byte pos to X
	pla			; return bit value in A
	rts
; -------------------------------------------------------------------------------------------------
; E532 ****** move to start of line
; cursor to line start (esc-j)
fndfst:	ldy sclf
	sty pntr		; set to leftmost column
fistrt:	jsr getbit		; find start of current line
	bcc fnd0		; branch if found
	dec tblx		; up a line
	bpl fistrt		; always
	inc tblx		; whoops went too far
fnd0:	jmp stupt		; set line base adr
; -------------------------------------------------------------------------------------------------
; E544 ****** Find last non-blank char of line
;   pntr= column #
;   tblx= line #
; cursor to end of line (esc-k)
fndend:	lda tblx		; check if we are at the bottom
	cmp scbot
	bcs eloupp		; yes...
	inc tblx
	jsr getbit		; is this line continued
	bcs fndend		; branch if so
eloup0: dec tblx 		; found it - compensate for inc tblx
eloupp: jsr stupt
	ldy scrt		; get right margin
	sty pntr		; point to right margin
	bpl eloup2		; always
;
eloup1: jsr bakchr
	bcs endbye 		; if at top left get out
eloup2: jsr get1ch
	cmp #$20
	bne endbye		; yes
	cpy sclf		; are we at the left margin?
	bne eloup1		; branch if not
	jsr getbit		; if we're on a wraped line
	bcs eloup1		; always scan the above line
;
endbye:	sty indx		; remember this
	rts
; -------------------------------------------------------------------------------------------------
; E574 ****** move to next char
; scroll if enabled
; wrap to top if disabled
nxtchr:	pha
	ldy pntr
	cpy scrt		; are we at the right margin?
	bcc bumpnt		; branch if not

	jsr nxln		; point to nextline
	ldy sclf		; point to first char of 1st line
	dey
	sec			; set to show moved to new line
bumpnt:	iny			; increment char index
	sty pntr
	pla
	rts
; -------------------------------------------------------------------------------------------------
; ****** backup one char - move one char left
; wrap up and stop a top left
bakchr:	ldy pntr
	dey
	bmi bakot1
	cpy sclf		; are we at the left margin
	bcs bakout		; no - past it
bakot1:	ldy sctop
	cpy tblx		; are we at top line last character?
	bcs bakot2		; leave with carry set
	dec tblx		; else backup a line
	pha
	jsr stupt		; set line base adr
	pla
	ldy scrt		; move cursor to right side
bakout:	sty pntr
	cpy scrt		; set z-flag if moved to new line
	clc			; always clear
bakot2:	rts
; -------------------------------------------------------------------------------------------------
; EE5A5 savpos - save row & column position
savpos:	ldy pntr
	sty sedt1
	ldx tblx
	stx sedt2
	rts
; -------------------------------------------------------------------------------------------------
; E5AE Delete or insert a character
delins:	bcs insert		; C=1 is insert
; delete a character
deleet:	jsr cleft		; move back 1 position
	jsr savpos		; save column & row positions
	bcs delout		; abort if at top left corner

deloop:	cpy scrt		; at right margin?
	bcc delop1		; no - skip ahead
	ldx tblx
	inx
	jsr getbt1		; is next line a wrapped line?
	bcs delop1		; yes - continue with delete
	jsr doblnk		; no - balnk last character

delout:	lda sedt1		; restore column and row positions
	sta pntr
	lda sedt2
	sta tblx
	jmp stupt		; restore pnt and exit

delop1:	jsr nxtchr
	jsr get1ch		; get next character
	jsr bakchr
	jsr dspp		; move it back 1 position
	jsr nxtchr		; move up 1 position
	jmp deloop		; loop until at end of line
; -------------------------------------------------------------------------------------------------
; E5E4 insert a character
insert:	jsr savpos		; save column & row positions
	jsr fndend		; move to last char on the line
	cpx sedt2		; last row equal to starting row?
	bne ins10		; no - skip ahead
	cpy sedt1		; is last position before starting position?
ins10:	bcc ins50		; yes - no need to move anything
	jsr movchr		; move to next char position
	bcs insout		; abort if scroll needed but disabled

ins30:	jsr bakchr
	jsr get1ch		; move char forward 1 position
	jsr nxtchr
	jsr dspp
	jsr bakchr
	ldx tblx
	cpx sedt2		; at original position
	bne ins30
	cpy sedt1
	bne ins30		; no - loop till we are

	jsr doblnk		; insert a blank
ins50:	inc insrt		; inc insert count
	bne insout		; only allow up to 255
	dec insrt
insout:	jmp delout		; restore original position
; -------------------------------------------------------------------------------------------------
; E61C stop/run
stprun:	bcc runrts		; exit if a stop code
	sei			; disable interrupts
	ldx #9
	stx ndx			; set keyboard queue size
runlop:	lda runtb-1,x
	sta keyd-1,x		; load run character sequence into kybd queue
	dex
	bne runlop
	
	cli			; enable interrupts
runrts:	rts
; -------------------------------------------------------------------------------------------------
; E62E movchr  -  move to next char position
;    insert blank line if at end of line
;    y = column position
;    on exit - carry set = abort - scroll disabled
movchr:	cpy scrt
	bcc movc10		; easy if not at end of line
	ldx tblx
	cpx scbot
	bcc movc10		; skip if not last line of screen
	bit scrdis
	bmi movc30		; abort if scrolling disabled

movc10:	jsr stupt		; set pnt address
	jsr nxtchr		; move to next char position
	bcc movc30		; done if not move to new line
	jsr getbit		; check if on a continued line
	bcs movc20		; skip ahead if not
	jsr patch1		; patch in a check for single line screen
	sec			; prep for abort...
	bvs movc30
	jsr scrdwn		; else insert a blank line

movc20:	clc			; for clean exit
movc30:	rts
; -------------------------------------------------------------------------------------------------
; E655 Escape sequence vector
sequen:	jmp (escvec)		; escape indirect
; -------------------------------------------------------------------------------------------------
;******************************
; E658 Insert line (esc-i)
;*****************************
iline:	jsr scrdwn		; insert a blank line
	jsr stu10		; move to start of line
	inx
	jsr getbt1
	php
	jsr putbit		; set continuation same as in previous line
	plp
	bcs linrts		; skip if was wrapped
	sec
	ror lsxp		; set flag - new line
linrts:	rts
; -------------------------------------------------------------------------------------------------
;**************************
; E666D Delete line (esc-d)
;**************************
dline:	jsr fistrt		; find start of line
	lda sctop		; save current of window
	pha
	lda tblx		; make 1st display line top of window
	sta sctop
	lda logscr		; make sure logical scrl is off
	pha
	lda #$80
	sta logscr
	jsr scru15		; scroll the top line away
	pla
	sta logscr
	lda sctop		; make old 1st line of this 1 current
	sta tblx
	pla
	sta sctop
	sec
	ror lsxp		; set flag - new line
	jmp stu10		; make this line the current one
; -------------------------------------------------------------------------------------------------
;******************************
; E694 Erase to end of line (esc-q)
;******************************
etoeol:	jsr savpos
etol:	jsr clrprt		; blank rest of line
	inc tblx		; move to next line
	jsr stupt
	ldy sclf
	jsr getbit		; check if next is wrapped line
	bcs etol		; yes - blank next line

etout:	jmp delout		; exit and restore original position
; -------------------------------------------------------------------------------------------------
;*****************************
; E6A9 erase to start of line (esc-p)
;*****************************
etosol:	jsr savpos
etstol:	jsr doblnk		; do a blank
	cpy sclf		; done a line ?
	bne ets100		; no
	jsr getbit		; at top of line
	bcc etout		; yes - exit
ets100:	jsr bakchr		; back up
	bcc etstol		; always
; -------------------------------------------------------------------------------------------------
;*****************************
; E6BD Scroll up (esc-v)
;*****************************
suup:	jsr savpos
	txa
	pha
	jsr scrup
	pla
	sta sedt2
	jmp etout		; always
; -------------------------------------------------------------------------------------------------
;*****************************
; E6CB scroll down (esc-w)
;*****************************
sddn:	jsr savpos
	jsr getbit
	bcs sddn2
	sec
	ror lsxp		; set flag - left line
sddn2:	lda sctop
	sta tblx		; scroll from screen top
	jsr scrdwn
	jsr clrbit		; make first line non-continued
	jmp etout		; always
; -------------------------------------------------------------------------------------------------
; E6E3 scrolling enable/disable
scrsw0:	clc			; enable scrolling (esc-l)
	!byte $24
scrsw1:	sec			; disable scrolling (esc-m)
; Scrolling enable/disable
;   carry set = disable
scrsw:	lda #0
	ror
	sta scrdis
	rts
; -------------------------------------------------------------------------------------------------
; E6ED Enable/Disable logical scroll
logsw0:	clc			; disable logical scroll (single line scroll)
	bcc logsw

logsw1:	sec			; enable logical scroll (scroll a set of lines)
; Logical scroll enable/disable
;   carry set = enable
logsw
	lda #0
	ror
	sta logscr		; store flag: $00 = disable, $80 = enable
	rts
; -------------------------------------------------------------------------------------------------
;******************************************
; E6F8 programmable key functions
;******************************************
keyfun:	sei			; prevent fight over variables with keyscan...
	dey
	bmi listky		; do list if no parameters given
	jmp addkey		; - else go add a new key definition
;   list key defintions
listky: ldy #0			; initialize key counter
;
listlp:	iny
	sty sedt3
	dey			; minus 1 for indexing
	lda keysiz,y		; get key length
	beq nodefn		; no listing if no defintion
	sta keyidx		; save key length
	jsr findky		; get buffer start addr for function key
	sta keypnt
	stx keypnt+1		; save 2 byte address in temp loc
; print 'key ' preamble
	ldx #3
preamb:	lda keword,x
	jsr bsout
	dex
	bpl preamb
; convert to 1 or 2 digit ascii
	ldx #$2F
	lda sedt3		; get key number
	sec
ky2asc:	inx			; .x=$30, if two digits it will inc to $31
	sbc #10
	bcs ky2asc		; repeat if >9
	adc #$3A		; add 10 & make ascii
	cpx #$30
	beq nosec		; skip 2nd digit print
	pha			; save first digit-10
	txa
	jsr bsout		; print second digit
	pla			; restore first digit-10
; print key string
nosec:	jsr bsout		; print first digit
	ldy #0			; init string position counter
	lda #','		; for comma print
lstk20:	jsr bsout		; print char - comma or plus-sign
	ldx #7			; for chr$ printing - no plus-sign or quote to preceed
txtprt:	jsr pagst2		; make sure function key ram page (indirect segment)
	lda (keypnt),y		; get byte
	jsr pagres		; restore ram page (indirect segment)
	cmp #13
	beq lstkcr		; print chr$(13) for return
	cmp #141
	beq lstksc		; print chr$(141) for return
	cmp #34
	beq lstkqt		; print chr$(34) for quote
	cpx #9			; was a normal char printed last time
	beq lstk10		; yes - skip ahead
	pha			; save char
	lda #$22		; "
	jsr bsout		; print a quote
	pla			; restore the char
;
lstk10:	jsr bsout		; print the char
	ldx #9			; for chr$ - print quote and plus next time
	iny
	cpy keyidx
	bne txtprt		; loop to end of string
;
	lda #$22		; "
	jsr bsout		; print ending quote
;
lstk30:	lda #$0D
	jsr bsout		; do a return

nodefn:	ldy sedt3		; get key number
	cpy #pgmkys
	bne listlp		; loop til all keys checked
	cli			; all done...clear the keyscan holdoff
	clc			; okay return always
	rts
;
lstkcr:	ldx #qtword-cdword-1	; index for return
	!byte $2c		; skip 2
lstksc:	ldx #addkey-cdword-1	; index for shifted-return
	!byte $2c		; skip 2
lstkqt:	ldx #scword-cdword-1	; index for quote
;
lstk:	txa			; save value index....
	pha			; save .x
	ldx #crword-cdword-1	; print chr$(
lstklp:	lda cdword,x		; print loop
	beq lstk40		; zero is end...
	jsr bsout
	dex
	bpl lstklp
	
	pla			; move number and repeat
	tax
	bne lstklp		; loop again for 'xxx)' ending part
;
lstk40:	iny
	cpy keyidx
	beq lstk30		; exit if all string printed
	lda #'+'		; set to print plus sign
	bne lstk20		; return to routine
;
keword:	!pet " yek"
cdword:	!pet "($rhc+",$22
crword:	!pet 0,")31"
qtword:	!pet 0,")43"
scword:	!pet 0,")141"
;   insert a new key defintion
addkey:	pha			; save zero page address of params
	tax
	sty sedt1		; save key number in temp loc
	lda $0,x		; get new string length
	sec
	sbc keysiz,y		; subtract old length
	sta sedt2		; save difference in temp location
	ror fktmp		; save the carry
	iny
	jsr findky		; find start addr of next function key
	sta sedsal
	stx sedsal+1		; save 2 byte address in temp loc
	ldy #pgmkys
	jsr findky		; find end of last function key
	sta sedeal
	stx sedeal+1		; save next free byte addr in temp loc
	ldy fktmp		; check if new string is longer or shorter
	bpl keysho		; skip ahead if shorter
	clc
	sbc pkyend		; subtract last available adress
	tay
	txa
	sbc pkyend+1
	tax
	tya
	clc
	adc sedt2		; add difference
	txa
	adc #0
	bcs kyxit		; skip if memory not full
; expand or contract key area to make room for new key definition.
keysho:	jsr pagst2		; set up function key ram page (indirect segment)
kymove:	lda sedeal
	clc			; check if entire area expanded or contracted
	sbc sedsal
	lda sedeal+1
	sbc sedsal+1
	bcc keyins		; go insert new key defintion if yes
	ldy #0
	lda fktmp		; check if expand or contract
	bpl kshort		; skip if needs to be contracted
	
	lda sedeal
	bne newky4		; dec 1 from source addr
	dec sedeal+1		; sub 1 for borrow
newky4:	dec sedeal
	lda (sedeal),y		; move 1 byte up to expand
	ldy sedt2		; get offset = difference
	sta (sedeal),y		; move byte up
	jmp kymove		; loop until all bytes moved

kshort:	lda (sedsal),y		; get source byte
	ldy sedt2		; get offset = difference
	dec sedsal+1		; sub 1 to move down
	sta (sedsal),y		; move the byte down
	inc sedsal+1
	inc sedsal		; move source up 1 byte
	bne kymove
	inc sedsal+1		; add 1 for carry
	bne kymove		; always
; insert the new string defintion
keyins:	ldy sedt1		; get the key index
	jsr findky		; find buffer start address for this key
	sta sedsal
	stx sedsal+1		; save 2 byte address in temp loc
	ldy sedt1
	pla
	pha
	tax			; get zero page addr of params
	lda $0,x
	sta keysiz,y		; save key length
	tay
	beq kyinok		; equal to zero no keys...exit
	lda $1,x		; get & save low byte of string address
	sta sedeal
	lda $2,x		; get & save high byte of string address
	sta sedeal+1

kyinlp:	dey
	lda $3,x		; get string ram page
	sta i6509
	lda (sedeal),y		; get byte
	jsr pagres		; restore original ram page (indirect segment)
	jsr pagst2		; set up function key ram page (indirect segment)
	sta (sedsal),y		; store into buffer
	tya			; .y flags...end?
	bne kyinlp		; no... loop

kyinok:	jsr pagres		; restore ram page (indirect segment)
	clc			; for good exit carry clear
kyxit:	pla			; pop zero page address for params
	cli			; all done...release keyscan
	rts			;  c-set is memory full error
.end
; -------------------------------------------------------------------------------------------------
;*******************************
; E865 keyboard scanner
;*******************************
key:	ldy #$FF		; say no keys pressed (real-time keyscan)
	sty modkey
	sty norkey
	iny			; init base kybd index = 0
	sty tpi2+pb		; allow all output lines
	sty tpi2+pa
	jsr getkey		; get keybd input
	and #$3F		; check if any inputs
	eor #$3F
	bne *+5			; hop over long branch
	jmp nulxit		; exit if none
	lda #$FF
	sta tpi2+pa		; allow only output line 0
	asl
	sta tpi2+pb
	jsr getkey		; get input from line 0
	pha			; save shift & control bits
	sta modkey		; shift keys are down
	ora #$30		; mask them by setting bits
	bne line01

linelp:	jsr getkey		; get line inputs
line01:	ldx #5			; loop for 6 input lines
kyloop:	lsr			; check line
	bcc havkey		; skip ahead if have input
	iny			; inc keyd code count
	dex
	bpl kyloop
	
	sec
	rol tpi2+pb		; rotate to activate next
	rol tpi2+pa		;  - output line
	bcs linelp		; loop until all lines done
	
	pla			; clear shift/control byte
	bcc nulxit		; exit if no key
;      get pet-ascii using keyboard index and shift and control inputs
havkey:	sty norkey		; have a normal keypress
	ldx normtb,y
	pla			; get shift/control byte
	asl
	asl			; move bits left
	asl
	bcc doctl		; skip ahead if control depressed
	bmi havasc		; skip ahead if not shifted - have ascii
	ldx shfttb,y		; assume shited textual
	lda grmode		; test text or graphic mode
	beq havasc		; have key if text mode
	ldx shftgr,y		; get shifted graphic
	bne havasc		; go process ascii key
;
doctl:	ldx ctltbl,y		; get pet-ascii char for this key
; y-reg has keyboard index value
; x-reg has pet-ascii value
havasc:	cpx #$FF
	beq keyxit		; exit if null pet-ascii
	cpx #$E0		; check if function key
	bcc notfun		; skip - not a function key
	tya
	pha
	jsr funjmp		; do function key indirect
	pla
	tay
	bcs keyxit		; done if carry flag set
; Not a function key
notfun:	txa			; get pet-ascii code
	cpy lstx		; check if same key as last
; time through
	beq dorpt		; skip ahead if so
; a new key input - check queue availability
	ldx #19
	stx delay		; reset initial delay count
	ldx ndx			; get key-in queue size
	cpx #keymax		; check if queue full
	beq nulxit		; exit if yes
	cpy #dblzer		; check if keypad - 00
	bne savkey		; go save key-in if not
	cpx #keymax-1		; check if room for two
	beq nulxit		; exit if not
	sta keyd,x		; save first zero
	inx			; update queue size
	bne savkey		; always
;
nulxit:	ldy #$FF
keyxit:	sty lstx		; save last key number
keyxt2:	ldx #$7F
	stx tpi2+pa		; reset output lines to allow
	ldx #$FF		; - stop key input
	stx tpi2+pb
	rts
; check repeat delays
dorpt:	dec delay		; dec initial delay count
	bpl keyxt2		; exit if was not zero - still on 1st delay
	inc delay		;  - else reset count to zero
; check if secondary count down to zero
	dec rptcnt		; dec repeat btwn keys
	bpl keyxt2		; exit if was not zero - still on delay
	inc rptcnt		; reset back to zero
; time to repeat - check if key queue empty
	ldx ndx			; get kybd queue size
	bne keyxt2		; exit if kybd queue not empty
; save pet-ascii into key buffer
savkey:	sta keyd,x		; store pet-ascii in kybd buffer
	inx
	stx ndx
	ldx #3
	stx rptcnt		; reset delay btwn keys
	bne keyxit
; Read keyboard matrix and debounce
getkey:	lda tpi2+pc		; debounce keyboard input
	cmp tpi2+pc
	bne getkey
	rts
; -------------------------------------------------------------------------------------------------
; E927 Jump vector: Function key indirect
funjmp: jmp (funvec)
; -------------------------------------------------------------------------------------------------
; E92A Default function key handler
dokeyf:	cpy lstx
	beq funrts		; exit not allowed to repeat
	lda ndx
	ora kyndx
	bne funrts		; exit - function queue not empty
	sta keyidx		; init pointer index into function area
	txa
	and #$1f		; mask out to get function key number
	tay
	lda keysiz,y		; get function key size
	sta kyndx		; - and store it for key scan
	jsr findky
	sta keypnt		; get function start addr
	stx keypnt+1		; - and save in keypnt

funrts:	sec
	rts
; -------------------------------------------------------------------------------------------------
; E949 Find address of function key given in y-reg
findky:	lda pkybuf
	ldx pkybuf+1
;
findlp:	clc
	dey			; found key yet?
	bmi fndout		; yes - done
	adc keysiz,y		; add function key size
	bcc findlp		; loop if no high byte carry-over
	inx
	bne findlp		; loop - always
;
fndout
	rts
; -------------------------------------------------------------------------------------------------
; E95A Tab set-up (tab positioner)
;   y=column in question
gettab:	tya			; get bit in question
	and #$07
	tax
	lda bits,x
	sta bitmsk
	tya			; get 8 bit block
	lsr
	lsr
	lsr
	tax
	lda tab,x
	bit bitmsk		; set equal flag
	rts
; -------------------------------------------------------------------------------------------------
;************************************************************
;*  routines involved in executing escape functions
;************************************************************
; E970 Main escape sequence handler
;   entry: character following escape character in acc.
escape:	and #$7F
	sec
	sbc #'a' 		; table begins at ascii a
	cmp #$1A  		; 'z'-'a'+1
	bcc escgo		; valid char, go get address
escrts:	rts 			; failed to find entry...ignore it!
; -------------------------------------------------------------------------------------------------
; E97A Get address of escape routine, and go to it.
escgo:	asl			; multiply index by 2
	tax
	lda escvct+1,x 		; get high byte
	pha
	lda escvct,x 		; and low
	pha
	rts 			; and go to that address
; -------------------------------------------------------------------------------------------------
; E985 Escape sequence table
escvct:	!word auton-1		; a Auto insert
	!word sethtb-1		; b set bottom
	!word autoff-1		; c cancel auto insert
	!word dline-1		; d Delete line
	!word setcr4-1		; e select non-flashing cursor
	!word setcr2-1		; f flashing cursor
	!word bellon-1		; g enable bell
	!word bellof-1		; h disable bell
	!word iline-1		; i Insert line
	!word fndfst-1		; j Move to start of line
	!word fndend-1		; k Move to end of line
	!word scrsw0-1		; l enable scrolling
	!word scrsw1-1		; m disable scrolling
	!word nrmscr-1		; n normal screen (not reverse)
	!word toqm-1		; o cancel insert, quote and reverse
	!word etosol-1		; p Erase to start of line
	!word etoeol-1		; q Erase to end of line
	!word revscr-1		; r reverse screen
	!word setcr3-1		; s solid cursor (not underscore)
	!word sethtt-1		; t Set top left of page
	!word setcr1-1		; u underscore cursor
	!word suup-1		; v Scroll up
	!word sddn-1		; w Scroll down
	!word escrts-1		; x cancel esacpe sequence
	!word nrmset-1		; y normal character set
	!word altset-1		; z alternate character set
; -------------------------------------------------------------------------------------------------
; E9B9 Set top left window corner (esc-t)
sethtt: clc			; set upper left corner with C=0
	!byte $24		; skip next instruction with bit $xx
; E9BB Set bottom right window corner (esc-b)
sethtb: sec			; set lower right corner with C=1
window: ldx pntr		; load cursor column
	lda tblx		; load cursour row
	bcc settps		; set upper left corner if C=0
setbts: sta scbot		; store last row
	stx scrt		; store last column
	rts
; -------------------------------------------------------------------------------------------------
; E9C7 Set full screen window
sreset: lda #scymax		; load last row, column of screen
	ldx #scxmax
	jsr setbts		; set lower right corner
	lda #0			; clear A, X to first row, column
	tax
settps: sta sctop		; set first row
	stx sclf		; set first column
	rts
; -------------------------------------------------------------------------------------------------
; E9D6 Bell on (esc-g)
bellon: lda #0			; $00 = bell on
; E9D8 Bell off (esc-h)
bellof:	sta bellmd		; store bell flag - any value = bell off
	rts
; -------------------------------------------------------------------------------------------------
; E9DC alter screen or cursor
; underscore cursor
setcr1:	lda #$0B
	bit tpi2+pc		; check machine type
	bmi setuns		; buit-in display
	lda #$06		; on others start cursor on line 6
	!byte $2c
; flashing cursor
setcr2:	lda #$60
setuns:	ora config
	bne setcr5		; always
; full cursor
setcr3:	lda #$F0
	!byte $2c
; steady cursor
setcr4:	lda #$0F
	and config
setcr5:	sta config
	rts			; wait untill cursor on to update
; -------------------------------------------------------------------------------------------------
; E9F6 reverse screen image
revscr: lda #$20
	!byte $2c
; alternate character set
altset:	lda #$10
	ldx #14
	stx vdc+adreg
	ora vdc+dareg
	bne setscr		; always
; non-reverse screen
nrmscr: lda #$DF
	!byte $2c
; normal character set
nrmset:	lda #$EF
	ldx #14
	stx vdc+adreg
	and vdc+dareg
setscr:	sta vdc+dareg
	and #$30		; use only bits 4 & 5 for starting address high
	ldx #12
ch6845:	stx vdc+adreg
	sta vdc+dareg
	rts
; -------------------------------------------------------------------------------------------------
; EA20 Auto insert mode off (esc-c)
autoff: lda #$00
	!byte $2C		; skips next instruction
; EA23 Auto insert mode on (esc-a)
auton:  lda #$FF
	sta insflg
	rts
; -------------------------------------------------------------------------------------------------
; EA29 Keyboard tables
normtb:					; keyboard table - no control/no shift
;LINE 0: F1, ESCAPE, TAB, NULL, SHIFT, CONTROL
	  !byte $E0,$1B,$09,$FF,$00,$01
	;LINE 1: F2, 1, Q, A, Z, NULL
	  !byte $E1,$31,$51,$41,$5A,$FF
	;LINE 2: F3, 2, W, S, X, C
	  !byte $E2,$32,$57,$53,$58,$43
	;LINE 3: F4, 3, E, D, F, V
	  !byte $E3,$33,$45,$44,$46,$56
	;LINE 4: F5, 4, R, T, G, B
	  !byte $E4,$34,$52,$54,$47,$42
	;LINE 5: F6, 5, 6, Y, H, N
	  !byte $E5,$35,$36,$59,$48,$4E
	;LINE 6: F7, 7, U, J, M, SPACE
	  !byte $E6,$37,$55,$4A,$4D,$20
	;LINE 7: F8, 8, I, K, "," , .
	  !byte $E7,$38,$49,$4B,$2C,$2E
	;LINE 8: F9, 9, O, L, ;, /
	  !byte $E8,$39,$4F,$4C,$3B,$2F
	;LINE 9: F10, 0, -, P, [, '
	  !byte $E9,$30,$2D,$50,$5B,$27
	;LINE 10: DOWN CURSOR, =, _, ], RETURN, PI
	  !byte $11,$3D,$5F,$5D,$0D,$DE
	;LINE 11: UP CUR, LT CUR, RT CUR, DEL, CMDR, NULL
	  !byte $91,$9D,$1D,$14,$02,$FF
	;LINE 12: HOME, ?, 7, 4, 1, 0
	  !byte $13,$3F,$37,$34,$31,$30
	;LINE 13: RVS ON, CANCEL, 8, 5, 2, DECIMAL POINT
	  !byte $12,$04,$38,$35,$32,$2E
	;LINE 14: GRAPHIC, MULT, 9, 6, 3, 00
	  !byte $8E,$2A,$39,$36,$33,$30
	;LINE 15: STOP, DIV, SUBTR, ADD, ENTER, NULL
	  !byte $03,$2F,$2D,$2B,$0D,$FF

shfttb:					; keyboard table - shift only & text mode
	;LINE 0: F11, SHT ESC, TAB TOGGLE, NULL, SHIFT, CTL
	  !byte $EA,$1B,$89,$FF,$00,$01
	;LINE 1: F12, !, Q, A, Z, NULL
	  !byte $EB,$21,$D1,$C1,$DA,$FF
	;LINE 2: F13, @, W, S, X, C
	  !byte $EC,$40,$D7,$D3,$D8,$C3
	;LINE 3: F14, #, E, D, F, V
	  !byte $ED,$23,$C5,$C4,$C6,$D6
	;LINE 4: F15, $, R, T, G, B
	  !byte $EE,$24,$D2,$D4,$C7,$C2
	;LINE 5: F16, %, ^, Y, H, N
	  !byte $EF,$25,$5E,$D9,$C8,$CE
	;LINE 6: F17, &, U, J, M, SHIFTED SPACE
	  !byte $F0,$26,$D5,$CA,$CD,$A0
	;LINE 7: F18, *, I, K, <, >
	  !byte $F1,$2A,$C9,$CB,$3C,$3E
	;LINE 8: F19, (, O, L, :, ?
	  !byte $F2,$28,$CF,$CC,$3A,$3F
	;LINE 9: F20, ), -, P, [, "
	  !byte $F3,$29,$2D,$D0,$5B,$22
	;LINE 10: DOWN CURSOR, +, POUND SIGN, ], SHT RETURN, PI
	  !byte $11,$2B,$5C,$5D,$8D,$DE
	;LINE 11: UP CURSOR,LEFT CURSOR,RIGHT CURSOR, INS, CMDR, NULL
	  !byte $91,$9D,$1D,$94,$82,$FF
	;LINE 12: CLEAR/HOME, ?, 7, 4, 1, 0
	  !byte $93,$3F,$37,$34,$31,$30
	;LINE 13: RVS OFF, SHFT CANCEL, 8, 5, 2, DECIMAL POINT
	  !byte $92,$84,$38,$35,$32,$2E
	;LINE 14: TEXT, MULT, 9, 6, 3, 00
	  !byte $0E,$2A,$39,$36,$33,$30
	;LINE 15: RUN, DIV, SUBTR, ADD, ENTER, NULL
	  !byte $83,$2F,$2D,$2B,$8D,$FF

shftgr:					; keyboard table - shift only & graphic mode
	;LINE 0: F11, SHT ESC, TAB TOGGLE, NULL, SHIFT, CTL
	  !byte $EA,$1B,$89,$FF,$00,$01
	;LINE 1: F12, !, GR, GR, GR, NULL
	  !byte $EB,$21,$D1,$C1,$DA,$FF
	;LINE 2: F13, @, GR, GR, GR, GR
	  !byte $EC,$40,$D7,$D3,$D8,$C0
	;LINE 3: F14, #, GR, GR, GR, GR
	  !byte $ED,$23,$C5,$C4,$C6,$C3
	;LINE 4: F15, $, GR, GR, GR, GR
	  !byte $EE,$24,$D2,$D4,$C7,$C2
	;LINE 5: F16, %, ^, GR, GR, GR
	  !byte $EF,$25,$5E,$D9,$C8,$DD
	;LINE 6: F17, &, GR, GR, GR, SHIFTED SPACE
	  !byte $F0,$26,$D5,$CA,$CD,$A0
	;LINE 7: F18, *, GR, GR, <, >
	  !byte $F1,$2A,$C9,$CB,$3C,$3E
	;LINE 8: F19, (, GR, GR, :, ?
	  !byte $F2,$28,$CF,$D6,$3A,$3F
	;LINE 9: F20, ), -, GR, [, "
	  !byte $F3,$29,$2D,$D0,$5B,$22
	;LINE 10: DOWN CURSOR, +, POUND, ], SHIFTED RETURN, PI
	  !byte $11,$2B,$5C,$5D,$8D,$DE
	;LINE 11: UP CURSOR,LEFT CURSOR,RIGHT CURSOR, INS, CMDR, NULL
	  !byte $91,$9D,$1A,$94,$82,$FF
	;LINE 12: CLEAR/HOME, ?, 7, 4, 1, 0
	  !byte $93,$3F,$37,$34,$31,$30
	;LINE 13: RVS OFF, SHIFT CANCEL, 8, 5, 2, DP
	  !byte $92,$04,$38,$35,$32,$2E
	;LINE 14: TEXT, MULT, 9, 6, 3, 00
	  !byte $0E,$2A,$39,$36,$33,$30
	;LINE 15: RUN, DIV, SUBTR, ADD, ENTER, NULL
	  !byte $83,$2F,$2D,$2B,$8D,$FF

ctltbl:					; keyboard table... control characters, any mode
	;LINE 0: NULL,NULL,NULL,NULL,NULL
	  !byte $FF,$FF,$FF,$FF,$FF,$FF
	;LINE 1: NULL,GR,Q,A,Z,NULL
	  !byte $FF,$A1,$11,$01,$1A,$FF
	;LINE 2: NULL,GR,W,S,X,C
	  !byte $FF,$A2,$17,$13,$18,$03
	;LINE 3: NULL,GR,E,D,F,V
	  !byte $FF,$A3,$05,$04,$06,$16
	;LINE 4: NULL,GR,R,T,G,B
	  !byte $FF,$A4,$12,$14,$07,$02
	;LINE 5: NULL,GR,GR,Y,H,N
	  !byte $FF,$A5,$A7,$19,$08,$0E
	;LINE 6: NULL,GR,U,J,M,NULL
	  !byte $FF,$BE,$15,$0A,$0D,$FF
	;LINE 7: NULL,GR,I,K,GR,NULL
	  !byte $FF,$BB,$09,$0B,$CE,$FF
	;LINE 8: NULL,GR,O,L,GR,NULL
	  !byte $FF,$BF,$0F,$0C,$DC,$FF
	;LINE 9: NULL,GR,GR,P,GR,GR
	  !byte $FF,$AC,$BC,$10,$CC,$A8
	;LINE 10: NULL,GR,GR,GR,NULL,GR
	  !byte $FF,$A9,$DF,$BA,$FF,$A6
	;LINE 11: NULL,NULL,NULL,NULL,NULL,NULL
	  !byte $FF,$FF,$FF,$FF,$FF,$FF
	;LINE 12: NULL,GR,GR,GR,GR,GR
	  !byte $FF,$B7,$B4,$B1,$B0,$AD
	;LINE 13: NULL,GR,GR,GR,GR,GR
	  !byte $FF,$B8,$B5,$B2,$AE,$BD
	;LINE 14: NULL,GR,GR,GR,GR,NULL
	  !byte $FF,$B9,$B6,$B3,$DB,$FF
	;LINE 15: NULL,GR,GR,GR,NULL,NULL
	  !byte $FF,$AF,$AA,$AB,$FF,$FF
; -------------------------------------------------------------------------------------------------
; EBA9 <SHIFT> <RUN/STOP> String: DLOAD "*" + RUN
runtb:  !pet "d",$CC,$22,"*",$0D	; dL"* <RETURN>
	!pet "run",$0D          	; run <RETURN>
; -------------------------------------------------------------------------------------------------
;****** address of screen lines ******
linz0	= scnram
linz1	= linz0+llen
linz2	= linz1+llen
linz3	= linz2+llen
linz4	= linz3+llen
linz5	= linz4+llen
linz6	= linz5+llen
linz7	= linz6+llen
linz8	= linz7+llen
linz9	= linz8+llen
linz10	= linz9+llen
linz11	= linz10+llen
linz12	= linz11+llen
linz13	= linz12+llen
linz14	= linz13+llen
linz15	= linz14+llen
linz16	= linz15+llen
linz17	= linz16+llen
linz18	= linz17+llen
linz19	= linz18+llen
linz20	= linz19+llen
linz21	= linz20+llen
linz22	= linz21+llen
linz23	= linz22+llen
linz24	= linz23+llen

;****** screen lines lo byte table ******
ldtb2:	!byte <linz0
	!byte <linz1
	!byte <linz2
	!byte <linz3
	!byte <linz4
	!byte <linz5
	!byte <linz6
	!byte <linz7
	!byte <linz8
	!byte <linz9
	!byte <linz10
	!byte <linz11
	!byte <linz12
	!byte <linz13
	!byte <linz14
	!byte <linz15
	!byte <linz16
	!byte <linz17
	!byte <linz18
	!byte <linz19
	!byte <linz20
	!byte <linz21
	!byte <linz22
	!byte <linz23
	!byte <linz24

;****** screen lines hi byte table ******
ldtb1:	!byte >linz0
	!byte >linz1
	!byte >linz2
	!byte >linz3
	!byte >linz4
	!byte >linz5
	!byte >linz6
	!byte >linz7
	!byte >linz8
	!byte >linz9
	!byte >linz10
	!byte >linz11
	!byte >linz12
	!byte >linz13
	!byte >linz14
	!byte >linz15
	!byte >linz16
	!byte >linz17
	!byte >linz18
	!byte >linz19
	!byte >linz20
	!byte >linz21
	!byte >linz22
	!byte >linz23
	!byte >linz24
; -------------------------------------------------------------------------------------------------
; EBE4 Dispatch table (control codes $00-$1F, $80-$9F)
ctable:	!word cuser-1
	!word cuser-1
	!word cuser-1
	!word stprun-1		; stop/run
	!word ce-1		; cancel
	!word cuser-1
	!word cuser-1
	!word bell-1		; bell/-
	!word cuser-1
	!word tabit-1		; tab/tab toggle
	!word cuser-1
	!word cuser-1
	!word cuser-1
	!word nxt1-1		; return or shifted return
	!word ctext-1		; text/graphic mode
	!word window-1		; set top/bottom
	!word cuser-1
	!word cdnup-1		; cursor down/up
	!word rvsf-1		; rvs on/off
	!word homclr-1		; home/clr
	!word delins-1		; delete/insert character
	!word cuser-1
	!word cuser-1
	!word cuser-1
	!word cuser-1
	!word cuser-1
	!word cuser-1
	!word cuser-1
	!word cuser-1
	!word crtlf-1		; cursor right/left
	!word cuser-1
	!word cuser-1
; -------------------------------------------------------------------------------------------------
; EC24 Length of function key texts
keylen: !byte key2-key1
	!byte key3-key2
	!byte key4-key3
	!byte key5-key4
	!byte key6-key5
	!byte key7-key6
	!byte key8-key7
	!byte key9-key8
	!byte key10-key9
	!byte keyend-key10	; 57 bytes keydef-text

!ifdef STANDARD_FKEYS{          ; ********** Standard F-keys **********
; EC2E Function key definitions
keydef:
key1:	!pet "print"                    ; F1
key2:	!pet "list"                     ; F2
key3:	!pet "dload",$22                ; F3
key4:	!pet "dsave",$22                ; F4
key5:	!pet "dopen"                    ; F5
key6:	!pet "dclose"                   ; F6
key7:	!pet "copy"                     ; F7
key8:	!pet "directory"                ; F8
key9:	!pet "scratch"                  ; F9
key10:	!pet "chr$("                    ; F10
; -------------------------------------------------------------------------------------------------
} else{                         ; ********** F-keys PATCH **********
; EC2E Function key definitions
keydef:
key1:	!pet "rU",$0d                   ; F1
key2:	!pet "lI",$0d                   ; F2
key3:	!pet "dL",$22                   ; F3
key4:	!pet "dS",$22                   ; F4
key5:	!pet "oP8,8,15,",$22,"cd:"      ; F5
key6:	!pet "oP9,9,15,",$22,"cd:"      ; F6
key7:	!pet "dcL",$0d                  ; F7
key8:	!pet "diRd0onu8"                ; F8
key9:	!pet "sC",$22                   ; F9
key10:	!pet "hE",$22                   ; F10
}
; Function key definitions
;keydef:
;key1:	!pet "run"                      ; F1
;key2:	!pet "list"                     ; F2
;key3:	!pet "dload",$22                ; F3
;key4:	!pet "dsave",$22                ; F4
;key5:	!pet "print"                    ; F5
;key6:	!pet "chr$("                    ; F6
;key7:	!pet "bank"                     ; F7
;key8:	!pet "directory"                ; F8
;key9:	!pet "scratch",$22              ; F9
;key10:	!pet "header",$22               ; F10
;}
keyend:
; -------------------------------------------------------------------------------------------------
; ECEF bits  -  bit position table
bits:	!byte $80,$40,$20,$10,$08,$04,$02,$01
; -------------------------------------------------------------------------------------------------
; EC6F ****** 6845 crtc text mode ******
atext:	!byte $6b,$50,83,$0f		; phillips, hitachi buit-in monitor
	!byte $19,$03,$19,$19
	!byte $00,$0d,cursor,$0d
	!byte romimg,$00,romimg,$00,$00,$00
ntext:	!byte 126,80,98,10,31,6,25,28,0,7,0,7		; ntsc monitors
	!byte romimg,$00,romimg,$00,$00,$00
ptext:	!byte 127,80,96,10,38,1,25,30,0,7,0,7		; pal monitors
	!byte romimg,$00,romimg,$00,$00,$00
etext:
; -------------------------------------------------------------------------------------------------
;**************************************************
cksume	!byte $2e		; e-page checksum
; -------------------------------------------------------------------------------------------------
; some additional  patch subroutines
*=$ECB0
; patch 4a-2 - make space for clear insert flag
patch4a2:
	jsr fistrt		; find begining of line 
	lda #0		
	rts
; -------------------------------------------------------------------------------------------------
; patch 4a-4 - remember bootom line no
patch4a4:
	ora crsw		; NO SENSE - a already 3
	sta crsw		; fake a carriage return
	lda scbot
	sta lintmp		; remember bootom line no
	lda scrt		; moved to patch to make space
	rts
; -------------------------------------------------------------------------------------------------
; patch 4a-3 - RS232 output
patch4a3:
	bcc p4a3		; -> check output ?
	jmp errorx		; moved to patch to make space
p4a3:	php
	pha
	lda sa
	and #$01
	beq p4a3x		; no output
	ldx la
	jsr ckout		; open channel
	jsr clrch		; close channel
	ldx ribuf
p4a3x:	pla
	plp
	rts
; -------------------------------------------------------------------------------------------------
; patch 4a-1 - turn off cursor behind key input
patch4a1:
	pha
	lda #10
	sta vdc+adreg
	lda #$20
	sta vdc+dareg		; turn off cursor
	pla
	rts			; end new code
; -------------------------------------------------------------------------------------------------
;**************************************************
; ED00 patch1 - checks for a single line window
;   aborts if so...
;**************************************************
*=$ED00
patch1:	ldx scbot		; check
	cpx sctop
	bne patcha		; no...pass through old code
	pla			; abort
	pla
patcha:	bit scrdis		; restore patched area (test for scrolling mode)
	rts
; ****************************************** KERNAL ***********************************************
; ##### monitor #####
!zone kernal
;************************************************
;*                                              *
;* kernal monitor                               *
;*                                              *
;* entry via call (jmp) or breakpoint (brk)     *
;* ---functions---                              *
;* <:>      alter memory                        *
;* <;>      alter registers                     *
;* <r>      display registers                   *
;* <m>      display memory                      *
;* <g>      start execution of code             *
;* <l>      load memory                         *
;* <s>      save memory                         *
;* <v>      view segment                        *
;* <@>      disk command                        *
;* <x>      warm start basic                    *
;* <u>      set default disk unit               *
;* <other>  load and execute from disk          *
;*                                              *
;* for syntax & semantics see cbm kernal manual *
;* copyright (c) 1981 by cbm                    *
;************************************************
; EE00 Reset Entry
*= kernal+$E00

; ***** Warm start entry *******
monon:	jsr ioinit		; get i/o
	jsr restor		; vectors
	jsr jcint		; screen editor

; ***** Cold start entry ******
monoff:	jsr clrch		; clear channels
	lda #winit		; waste two bytes so timc=60950
	ldx #<monon		; point reset vectors at monitor on
	ldy #>monon
	jsr vreset
	cli			; release irq's

; ***** Call entry *****
timc:	lda #$40+$80
	sta msgflg		; error+messages on
	lda #ms34-ms1		; call entry
	sta tmpc
	bne b3			; branch always

; ***** Break entry *****
timb:	jsr clrch		; clr channels
	lda #ms36-ms1		; break entry
	sta tmpc
	cld

; Save .y,.x,.a,flags, and pc
	ldx #5
; EE2B Pop registers from stack and save them
b1:	pla
	sta pch,x
	dex
	bpl b1

b3:	lda i6509		; save indirection segment
	sta xi6509
	lda cinv
	sta invl		; save irq low
	lda cinv+1
	sta invh		; save irq high

	tsx
	stx sp			; save original sp
	cli			; clear ints
	lda #8			; set disk default to 8
	sta ddisk

b5:	ldy tmpc		; message code
	jsr spmsg		; print break/call

	lda #'r'		; display regs on entry
	bne s0			; branch always
; EE50 ***** Error entry *****

erropr:	jsr outqst
	pla
	pla

; ***** Command interpreter entry *****
strtm1=*-1
	lda #$40+$80
	sta msgflg		; i/o messages to screen
	lda #<buf		; put filename at bottom of basic buffer
	sta fnadr
	lda #>buf
	sta fnadr+1
	lda #irom
	sta fnadr+2
	jsr crlf

st1:	jsr basin		; read command

	cmp #'.'
	beq st1			; skip prompt characters
	cmp #' '
	beq st1			; span blanks
	jmp (usrcmd)		; user indirect for monitor
; -------------------------------------------------------------------------------------------------
; EE77 Command interpreter
s0:	ldx #0
	stx fnlen
	tay			; save current command

; Put return address for commands on stack
	lda #>strtm1
	pha
	lda #<strtm1
	pha

	tya			; current command in .a

s1:	cmp cmds,x		; is it this one?
	bne s2			; notit

	sta savx		; save current command

; Indirect jmp from table
	lda cmds+1,x
	sta tmp0
	lda cmds+2,x
	sta tmp0+1
	jmp (tmp0)
; -------------------------------------------------------------------------------------------------
; EE98 Each table entry is 3 long---skip to next
s2:	inx
	inx
	inx
	cpx #cmdend-cmds
	bcc s1			; loop for all commands

; Command not in table...look on disk.
; Command name can be any length and have parameters.
	ldx #0			; length to zero
s3:	cmp #$D		; end of name?
	beq s4			; yes...
	cmp #' '		; blank?
	beq s4			; yes
	sta buf,x
	jsr basin		; get next
	inx			; count char
	bne s3			; and continue

s4:	sta tmpc
	txa			; count
	beq s6			; is zero

	sta fnlen
	lda #$40
	sta msgflg		; messages off
	lda ddisk
	sta fa			; will use default disk
	lda #irom		; commands only load to rom segment !!!***
	sta i6509		; turn indirect to rom segment
	ldx #$FF
	ldy #$FF
	jsr load		; try to load command
	bcs s6			; bad load...

	lda tmpc		; pass last character
	jmp (stal)		; go do it

s6:	rts
; -------------------------------------------------------------------------------------------------
; EED5 Command table
cmds:	!pet ':'		; alter memory
	!word altm
	!pet ';'		; alter registers
	!word altr
	!pet 'r'		; display registers
	!word dsplyr
	!pet 'm'		; display memory
	!word dsplym
	!pet 'g'		; start execution
	!word go
	!pet 'l'		; load memory
	!word ld
	!pet 's'		; save memory
	!word ld
	!pet 'v'		; view segment
	!word view
	!pet '@'		; disk command (alternate)
	!word disk
	!pet 'z'		; transfer to 2nd microprocessor
	!word ipcgov		; ipcgo vector
	!pet 'x'		; warm start basic
	!word xeit
	!pet 'u'		; default disk unit set
	!word unitd
cmdend:
; -------------------------------------------------------------------------------------------------
; EEF9 Exit 'x'
xeit:	pla			; remove command return from stack
	pla
	sei			; disable interrupts...all warm start code expects
	jmp (evect)		; go warmstart language
; -------------------------------------------------------------------------------------------------
; EEFF Move tmp0/tmp0+1 to PC memory location
putp:	lda tmp0		; move tmp0 to pch,pcl
	sta pcl
	lda tmp0+1
	sta pch
	rts
; -------------------------------------------------------------------------------------------------
; EF08 Set tmp0 to point to the saved regs in zero page
setr:	lda #<flgs		; set to access regs
	sta tmp0
	lda #>flgs
	sta tmp0+1
	lda #irom		; point indirect at roms
	sta i6509
	lda #5
	rts
; -------------------------------------------------------------------------------------------------
; EF17 Prints '.:' or '.;' before data to permit alter after 'm' or 'r' command

altrit: pha			; preserve alter character
	jsr crlf
	pla
	jsr bsout

space:  lda #' '		; output a space
	!byte $2C		; skip two bytes

outqst: lda #'?'		; output question
	jmp bsout		; go print bytes

crlf:   lda #$D			; do carriage return
	jsr bsout
	lda #'.'		; monitor prompt
	jmp bsout
; -------------------------------------------------------------------------------------------------
; EF31 Data for register display heading
regk:	!pet cr,"  "		; 3 spaces
	!pet " pc "," irq "," sr ac xr yr sp"
; -------------------------------------------------------------------------------------------------
; EF4C Display register function 'r'
dsplyr:	ldx #0
d2:	lda regk,x
	jsr bsout		; print heading
	inx
	cpx #dsplyr-regk	; max length
	bne d2
	lda #';'
	jsr altrit		; allow alter after display
	ldx pch
	ldy pcl
	jsr wroa		; print program counter
	jsr space
	ldx invh
	ldy invl
	jsr wroa		; print irq vector
	jsr setr		; set to print .p,.a,.x,.y,.s

; display memory subroutine
dm:	sta tmpc		; byte count
	ldy #0			; indirect index
	sty fnlen		; fnlen is zero-page crossing flag...
dm1:	jsr space		; space tween bytes
	lda (tmp0),y
	jsr wrob		; write byte of memory

; increment indirect
	inc tmp0
	bne dm2
	inc tmp0+1
	bne dm2			; no zero page crossing
	dec fnlen		; fnlen<>0 is flag

dm2:	dec tmpc		; count bytes
	bne dm1			; until zero
	rts
; -------------------------------------------------------------------------------------------------
; EF8F Display memory function 'm'
dsplym:	jsr rdoae		; read start adr...err if no sa
	jsr t2t2		; sa to tmp2
; allow user to type just one address
	jsr rdoa		; read end adr
	bcc dsp123		; good...no default

	lda tmp2
	sta tmp0		; default low byte
	lda tmp2+1
	sta tmp0+1		; default hi byte

dsp123:	jsr t2t2		; sa to tmp0, ea to tmp2
dsp1:	jsr stop		; stop key?
	beq beqs1		; yes...break list

	lda #':'
	jsr altrit		; allow alter
	ldx tmp0+1
	ldy tmp0
	jsr wroa		; write start address

	lda #16			; count of bytes cbmii
	jsr dm			; display bytes

	lda fnlen		; check for zero-crossing
	bne beqs1		; yup....
	sec
	lda tmp2
	sbc tmp0
	lda tmp2+1
	sbc tmp0+1
	bcs dsp1		; end >= start

beqs1:	rts			; a.o.k. exit
; -------------------------------------------------------------------------------------------------
; EFCB Alter register function ';'
altr:	jsr rdoae		; read new pc...no address=error

	jsr putp		; alter pc

	jsr rdoae		; read new irq...no address=error

	lda tmp0
	sta invl		; alter irq vector
	lda tmp0+1
	sta invh

	jsr setr		; set to alter r's
	bne a4			; branch always
; -------------------------------------------------------------------------------------------------
; EFE1 View a segment (point indirect) 'v'
view:	jsr rdobe		; get a byte...if none...error
	cmp #16			; range 0-15
	bcs errl		; to large no modulo
	sta i6509
	rts
; -------------------------------------------------------------------------------------------------
; EFEB Unit default for disk 'u'
unitd:	jsr rdobe		; get a byte...if none...error
	cmp #32			; range 0-31
	bcs errl		; to large no modulo
	sta ddisk
	rts
; -------------------------------------------------------------------------------------------------
; EFFB Alter memory - read adr and data ':'
altm:	jsr rdoae		; read alter adr...if none...error

	lda #16			; allow 16 bytes change

; common code for ':' and ';'
a4:	sta tmpc		; number of bytes to change

a5:	jsr rdob		; read byte
	bcs a9

	ldy #0
	sta (tmp0),y		; store it away

; increment store address
	inc tmp0
	bne a6
	inc tmp0+1

a6:	dec tmpc		; count byte
	bne a5			; until zero
a9:	rts
; -------------------------------------------------------------------------------------------------
; F010 Start execution function 'g'
go:	jsr rdoc		; see if default
	beq g1			; yes...pc is address
	jsr rdoae		; no...get new addr...none=error
	jsr putp		; move addr to p.c.

g1:	ldx sp
	txs			; orig or new sp value to sp

	sei			; prevent disaster

	lda invh
	sta cinv+1		; set up irq vector
	lda invl
	sta cinv
	lda xi6509		; and indirection register
	sta i6509

; get flags,pch,pcl,.a,.x,.y
	ldx #0
g2:	lda pch,x
	pha			; everybody on stack
	inx
	cpx #6
	bne g2

; interrupt return sets everybody up from data on stack
	jmp prend
; -------------------------------------------------------------------------------------------------
; rdobe - read a byte and error on line end..waste stack
;
rdobe:	jsr rdob
	bcs errlpl
rdoxit:	rts
; -------------------------------------------------------------------------------------------------
; rdoae - read an address and error on line end...waste stack
;
rdoae:	jsr rdoa
	bcc rdoxit
; -------------------------------------------------------------------------------------------------
errlpl:	pla			; zap stack
	pla
errl:	jmp erropr		; syntax error jump
; -------------------------------------------------------------------------------------------------
; F04A Load ram function 'l' and 's'
;  *note - load/save reset indirect to rom
ld:	ldy #1
	sty fa			; default device #1
	dey			; .y=0 to count name length
	lda #$FF		; default no move load
	sta tmp0
	sta tmp0+1
	lda i6509		; save indirect for seg address
	sta t6509
	lda #irom		; indirect to rom for filename
	sta i6509

l1:	jsr rdoc		; default?
	beq l5			; yes...try load

	cmp #' '
	beq l1			; span blanks

	cmp #$22 ; "		; string next?
l2:	bne errl		; no file name...

l3:	jsr rdoc		; get character of name
	beq l5			; end...asssume load

	cmp #$22 ; "		; end of string?
	beq l8			; yes...could still be 'l' or 's'

	sta (fnadr),y		; store name
	inc fnlen
	iny
	cpy #16			; max file name length

l4:	beq errl		; file name too long
	bne l3			; branch always
; see if we got a load
l5:	lda savx		; get last command
	cmp #'l'
	bne l2			; no..not a load..error

	lda t6509		; get segment to load to
	and #irom		; mask off verify bit
	ldx tmp0
	ldy tmp0+1
	jmp load		; yes...do load

l8:	jsr rdoc		; more stuff?
	beq l5			; no...defualt load

	cmp #','		; delimeter?
l9:	bne l2			; no...bad syntax

	jsr rdobe		; yes...get next parm...error if none

	sta fa

	jsr rdoc		; more parms?
	beq l5			; no...default load

	cmp #','		; delimeter?
l12:	bne l9			; no...bad syntax

	jsr rdobe		; segment byte ?...must have
	cmp #16			; 00-0f allowed
	bcs l15			; too big...
	sta t6509
	sta stas		; prep segment
	jsr rdoae		; start address?...must have
; set up start save address
	lda tmp0
	sta stal
	lda tmp0+1
	sta stah

	jsr rdoc		; delimeter?
	beq l5			; cr, do load
	cmp #','
	bne l15			; no delim

	jsr rdobe		; get segment byte...must have
	cmp #16			; allow only 00-0f
	bcs l15			; too big...
	sta eas			; prep segment
	jsr rdoae		; try to read end address...must have

; set up end save address
	lda tmp0
	sta eal
	lda tmp0+1
	sta eah

l20:	jsr basin
	cmp #' '
	beq l20			; span blanks

	cmp #cr
l14:	bne l12			; missing cr at end
	lda savx 		; was command save?
	cmp #'s'
	bne l14			; no...load can't have parms
	ldx #stal		; get params for save
	ldy #eal
	jmp save

l15:	jmp erropr
; -------------------------------------------------------------------------------------------------
; F0F6 Write adr from tmp0 stores
wroa:   txa			; hi-byte
	jsr wrob
	tya			; low-byte
; F0FB Write byte --- a = byte
; unpack byte data into two ascii characters. a=byte; x,a=chars
wrob:   pha
	lsr
	lsr
	lsr
	lsr
	jsr ascii		; convert to ascii
	tax
	pla
	and #$0F
; F107 Convert nybble in a to ascii and print it
ascii:	clc
	adc #$F6
	bcc asc1
	adc #$06
asc1:	adc #$3A
	jmp bsout
; -------------------------------------------------------------------------------------------------
; F113 Exchange temporaries
t2t2:	ldx #2
t2t21:	lda tmp0-1,x
	pha
	lda tmp2-1,x
	sta tmp0-1,x 
	pla
	sta tmp2-1,x
	dex
	bne t2t21
	rts
; -------------------------------------------------------------------------------------------------
; F123 Read hex adr,return hi in tmp0, lo in tmp0+1,and cy=1, if sp cy=0
rdoa:	jsr rdob		; read 2-char byte
	bcs rdoa2		; space
	sta tmp0+1
	jsr rdob
	sta tmp0
rdoa2:	rts
; -------------------------------------------------------------------------------------------------
; F130 Read hex byte and return in a and cy=0 if sp cy=1
rdob:	lda #0			; space
	sta bad			; read next char
	jsr rdoc
	beq rdob4		; fail on cr
	cmp #' '		; blank?
	beq rdob		; span blanks...

	jsr hexit		; convert to hex nybble
	asl
	asl
	asl
	asl
	sta bad
	jsr rdoc		; 2nd char assumed hex
	beq rdob4		; fail on cr
	jsr hexit
	ora bad

rdob4:	rts
; -------------------------------------------------------------------------------------------------
; F154 Convert char in A into hex value
hexit:  cmp #$3A
	php			; save flags
	and #$0F
	plp
	bcc hex09		; 0-9
	adc #8			; alpha add 8+cy=9
hex09:	rts
; -------------------------------------------------------------------------------------------------
; F166 Get character and test for cr
rdoc:	jsr basin
	cmp #$D			; is it a cr
	rts			; return with flags
; -------------------------------------------------------------------------------------------------
; F16C Send disk command or read status '@'
disk:	lda #0			; clear status @ i/o begin
	sta status
	sta fnlen		; filename length of zero...

	ldx ddisk		; get default disk
	ldy #$0F		; open command channel
	jsr setlfs		; .a-0 temporary channel #
	clc
	jsr open		; open a real channel
	bcs disk30		; exit if bad return

	jsr rdoc		; see if status check
	beq disk20		; yes

	pha
	ldx #0
	jsr ckout		; set up as output
	pla
	bcs disk30		; bad status return
	bcc disk15		; no...ok

disk10:	jsr basin		; get a character
disk15:	cmp #$D			; see if end
	php			; save for later
	jsr bsout		; out to floppy
	lda status
	bne disk28		; bad status returned
	plp			; end?
	bne disk10		; no...continue
	beq disk30		; yes...floppy done

disk20:	jsr crlf
	ldx #0
	jsr chkin		; tell floppy to speak
	bcs disk30		; bad device

disk25:	jsr rdoc		; get a character
	php			; save test for later
	jsr bsout		; out to screen
	lda status		; check for bad basin
	and #$FF-$40		; remove eoi bit
	bne disk28		; report bad status
	plp			; end?
	bne disk25		; no...
	beq disk30		; yes...floppy done

disk28:	pla			; clean up...
disk29:	jsr error5		; report error #5 for bad device
disk30:	jsr clrch		; clean up
	lda #0
	clc			; just remove from table
	jmp close
; ****************************************** KERNAL ***********************************************
; F1C3 ##### messages #####
ms1:	!pet $D,"i/o error ",$A3
ms5:	!pet $D,"searching",$A0
ms6:	!pet "for",$A0
;ms7:	!pet $D,"press play on tap",$C5
;ms8:	!pet "press record & play on tap",$C5
ms10:	!pet $D,"loadin",$C7
ms11:	!pet $D,"saving",$A0
ms21:	!pet $D,"verifyin",$C7
ms17:	!pet $D,"found",$A0
ms18:	!pet $D,"ok",$8D
ms34:	!pet $D,"** monitor 1.0 **",$8D
ms36:	!pet $D,"brea",$CB
; -------------------------------------------------------------------------------------------------
; F21C Print message to screen only if output enabled
spmsg:	bit msgflg		; printing messages?
	bpl msg10		; no...
msg:	lda ms1,y
	php
	and #$7F
	jsr bsout
	iny
	plp
	bpl msg
msg10:	clc
	rts
; -------------------------------------------------------------------------------------------------
; ##### ieee #####
; F230 Command ieee-488 device to talk
ntalk:	ora #tlkr		; make a talk adr
	bne list1		; always go to list1
; command ieee-488 device to listen
nlistn:	ora #lstnr		; make a listen adr

list1:	pha			; save device and talk/listen

	lda #tddb		; set control for atn/data out
	sta tpi1+ddpa

	lda #$FF		; set direction for transmitt *
	sta cia+pra		; set data   *
	sta cia+ddra		; set data direction out   *
	lda #$FF-dc-ren		; enable transmitt
	sta tpi1+pa
	lda c3po		; get ieee flags
	bpl list2		; if data in buffer

	lda tpi1+pa		; send eoi
	and #$FF-eoi
	sta tpi1+pa

	lda bsour		; get byte to send
	jsr tbyte		; send last character

	lda c3po		; clear byte in buffer flag
	and #$FF-dibf
	sta c3po

	lda tpi1+pa		; clear eoi
	ora #eoi
	sta tpi1+pa

list2:	lda tpi1+pa		; assert atn
	and #$FF-atn
	sta tpi1+pa

	pla			; get talk/listen address
	jmp tbyte
; -------------------------------------------------------------------------------------------------
; F274 Send secondary address after listen
nsecnd:	jsr tbyte		; send it      
; release attention after listen       
scat1:
scatn:	lda tpi1+pa		; de-assert atn
	ora #atn
	sta tpi1+pa
	rts
; -------------------------------------------------------------------------------------------------
; F280 Talk second address
ntksa:	jsr tbyte		; send secondary address

tkatn:	lda #$FF-nrfd-ndac-te-ren ; pull nrfd and ndac low
	and tpi1+pa
; exit entry for untalk/unlisten
setlns:	sta tpi1+pa
	lda #rddb		; set control lines for input
	sta tpi1+ddpa
	lda #$00		; set data lines for recieve
	sta cia+ddra
	beq scatn
; -------------------------------------------------------------------------------------------------
; F297 Buffered output to ieee-488
nciout:	pha			; save data
	lda c3po		; get ieee flags
	bpl ci1			; if no data in buffer
	lda bsour		; get data in buffer
	jsr tbyte		; transmit byte
	lda c3po		; get ieee flags

ci1:	ora #dibf		; set data in buffer flag
	sta c3po

	pla			; get new data
	sta bsour
	rts
; -------------------------------------------------------------------------------------------------
; F2AB Send untalk command on ie
nuntlk:	lda #utlkr		; untalk command
	bne unls1		; always

; send unlisten command on ieee-488
nunlsn:	lda #ulstn		; unlisten command
unls1:	jsr list1		; send it
	lda #$FF-dc-te-ren	; set for recieve all lines high
	jmp setlns		; go setup proper exit state
; -------------------------------------------------------------------------------------------------
; tbyte -- output byte onto ieee bus.
;   entry a = data byte to be output.
;   uses a register. 1 byte of stack space.
; F2B9
tbyte:	eor #$FF		; compliment data
	sta cia+pra

	lda tpi1+pa
	ora #dav+te		; say data not valid, te=data out
	sta tpi1+pa

	bit tpi1+pa		; test nrfd & ndac in high state
	bvc tby2		; either nrfd or ndac low => ok
	bpl tby2

tby1:	lda #nodev		; set no-device bit in status
	jsr udst
	bne tby7		; always exit

tby2:	lda tpi1+pa
	bpl tby2		; if nrfd is high

	and #$FF-dav
	sta tpi1+pa

tby3:	jsr timero		; set timeout
	bcc tby4		; c-clear means first time through
tby3t:	sec			; c-set is second time

tby4:	bit tpi1+pa
	bvs tby6		; if ndac hi
	lda cia+icr
	and #$02		; timer b posistion (cia)
	beq tby4		; if no timeout
	lda timout		; timeout selection flag
	bmi tby3		; no - loop
	bcc tby3t		; wait full 64us

tby5:	lda #toout		; set timeout on output in status
	jsr udst		; update status

tby6:	lda tpi1+pa		; release dav
	ora #dav
	sta tpi1+pa

tby7:	lda #$FF		; release data bus
	sta cia+pra		; bus failure exit
	rts
; -------------------------------------------------------------------------------------------------
; rbyte -- input byte from ieee bus.
;   uses a register. 1 byte of stack space.
;   exit a = input data byte.
; F30A
nacptr:	; ********************************
nrbyte:
	lda tpi1+pa		; set control lines
	and #$FF-te-ndac-ren	; pull ndac low, te=data in
	ora #nrfd		; say read for data
	sta tpi1+pa

rby1:	jsr timero		; return c-clear for cbmii
	bcc rby2		; c-clear is first time through
rby1t:	sec			; c-set is second time through

rby2:	lda tpi1+pa		; get ieee control lines
	and #dav
	beq rby4		; if data available
	lda cia+icr
	and #$02		; timer b (cia)
	beq rby2		; if not timed out
	lda timout		; get timeout flag
	bmi rby1		; loop
	bcc rby1t		; go through twice

rby3:	lda #toin		; set timeout on input in status
	jsr udst
	lda tpi1+pa
	and #$FF-nrfd-ndac-te	; nrfd & ndac lo on error
	sta tpi1+pa
	lda #cr			; return null input
	rts
;
rby4:	lda tpi1+pa		; say not read for data
	and #$FF-nrfd
	sta tpi1+pa
	and #eoi
	bne rby5		; if not eoi
	lda #eoist		; set eoi in status
	jsr udst

rby5:	lda cia+pra		; get data
	eor #$FF

rby6:	pha			; save data
	lda tpi1+pa		; say data accepted
	ora #ndac
	sta tpi1+pa

rby7:	lda tpi1+pa		; get ieee control lines
	and #dav
	beq rby7		; if dav high

	lda tpi1+pa		; say dat not accpted
	and #$FF-ndac
	sta tpi1+pa
	pla			; return data in a
	rts
; -------------------------------------------------------------------------------------------------
; F36F Set up for timeout (6526)
timero:	lda #$FF		; set time for at least 32us (P500 $80)

	sta cia+tbhi
	lda #$11		; turn on timer continous in case of other irq's
	sta cia+crb
	lda cia+icr		; clear interrupt
	clc
	rts
; -------------------------------------------------------------------------------------------------
; ##### rs232 #####
rs232:	jmp error9		; bad device number
; -------------------------------------------------------------------------------------------------
; opn232 - open an rs-232 channel
;   if sa=1 then output channel
;   if sa=2 then input  channel
;   if sa=3 then bidirectional channel
;   if sa>128 then ascii conversion enabled
;
;   filename consists of 0-4 bytes
;  byte #1- control register 6551
;  byte #2- command register 6551
;  byte #3- c/r lf delay...60ths of sec   (unimplemented)
;  byte #4- auto c/r insert afer xx chars (unimplemented)
;
;    actions:
;  1. clear  rs232 status:  rsstat
;  2. set 6551 contrl (ctr) register
;  3. set 6551 command (cdr) register
;       cdr bits (7-4) = filename byte 2 bits (7-4)
;                (3-2) = 00  (xmitter off)
;                (1)   = 1   (receiver off)
;                (0)   = 0   (dtr off)
;  4. do buffer alocatation, if needed
;---------------------------------------------
; F381
opn232:	jsr rst232		; reset rs232 status
	ldy #0

opn020:	cpy fnlen		; filename all out ?
	beq opn030		; yes...

	jsr fnadry
	sta m51ctr,y
	iny
	cpy #4			; only four bytes in all
	bne opn020

opn030:	lda m51ctr		; set the register
	sta acia+ctr
	lda m51cdr		; clear up conflicts
	and #$F2
	ora #$02
	sta acia+cdr		; everything off
	clc
	lda sa			; check for buffers needed
	and #$02
	beq opn045		; no input

	lda ridbe		; set up pointers
	sta ridbs
	lda ribuf+2		; check for allocation
	and #$F0		; $ff not allocated flag (see alloc error codes, too)
	beq opn045		; already allocated
	jsr req256		; request 256 bytes for storage
	sta ribuf+2		; save starting
	stx ribuf
	sty ribuf+1
opn045:	jmp patch4a3		; ***** patch 4a-3 - RS232 output *****
	nop
	nop
opn050:	rts			; c-clr already allocated
; -------------------------------------------------------------------------------------------------
; toasci - convert cbm text code to
;  ascii for valid ascii ranges.
; entry: .a - cbm text code
; exit : .a - ascii code
;----------------------------------------
; F3C7
toasci:	cmp #'a'		; convert $41 to $5a
	bcc toa020
	cmp #$5B
	bcs toa010
	ora #$20		; to lower case ascii
toa010:	cmp #$C1		; convert $c1 to $da
	bcc toa020
	cmp #$DB
	bcs toa020
	and #$7F		; to upper case ascii
toa020:	rts
; -------------------------------------------------------------------------------------------------
; tocbm - convert ascii code to cbm
;  text code for valid ascii ranges.
; entry: .a - ascii code
; exit : .a - cbm text code
;----------------------------------------
; F3DC
tocbm:	cmp #'a'		; convert upper case ascii
	bcc toc020
	cmp #$5B
	bcs toc010
	ora #$80		; to $c1 to $da
toc010:	cmp #$61		; convert lower case ascii
	bcc toc020
	cmp #$7B
	bcs toc020
	and #$FF-$20		; to $41 - $5a
toc020:	rts
; -------------------------------------------------------------------------------------------------
; xon232 - turn 6551 transmitter on, no transmit interrupts
;        cdr bits(3-2) = 10
;            bit(1)    = 1
;---------------------------------------------------------------
; F3F1
xon232:	lda acia+cdr
	ora #$09
	and #$FB
	sta acia+cdr
	rts
; -------------------------------------------------------------------------------------------------
; F3FC req256 - request 256 bytes of space
;  (don't care where we get it...)
req256:	ldx #00
	ldy #01		; one page = 256 bytes
; -------------------------------------------------------------------------------------------------
; alocat - alocatate space
;  entry:
; *  .a- if .a=$ff then don't care what segment
; *  .a- if .a=$80 then we want bottom of memory
; *  .a- if .a=$40 then we want top of memory
; *  .a- if .a=$0x then we need segment x
;    .x- low # of bytes needed
;    .y- high # of bytes needed
;
;  exit :
;    c-clr  no problem alocatating space
;     .a,.x,.y is start address of alocatated space
;    c-set  problem with alocatation
;     if .a =$ff then alocatation refused (cannot cross segment boundrys)
; *   if .a =$8x then bottom of memory needs to be changed
;     if .a =$4x then top of memory needs to be changed
; *   if .a =$c0 then bottom>top  !! fatal error !!
;     return to language
;
; *=> not implemented yet  10/30/81 rsr (only top alocatation)
;-----------------------------------------------------------------------
; F400
alocat:
tttop:	txa			; calc new hiadr
	sec
	eor #$FF
	adc hiadr		; sub low from end of system RAM
	tax
	tya
	eor #$FF
	adc hiadr+1		; sub high
	tay
	lda hiadr+2		; load highest system RAM bank
	bcs top010
refuse:	lda #$FF		; allocation refused...crossed boundry
topbad:	ora #$40		; want top of memory changed
	sec			; C=1 Not enough memory available 
	rts			; return unsuccessful

top010:	cpy memsiz+1		; compare new high address with user memory high
	bcc topbad		; branch if new high lower = not enough memory alocatatable
	bne topxit		; branch to memoryok if new high > 
	cpx memsiz		; if higbyte equal compare low
	bcc topbad		; branch if lower = not enough memory alocatatable
topxit:	stx hiadr		; store new end of system memory ($)
	sty hiadr+1
	clc
	rts
; -------------------------------------------------------------------------------------------------
; rst232 - reset rs232 and dcd/dsr status
;          note, the dcd and dsr bits of rsstat reflect whether a
;          dsr or dcd error occured since the last time the user
;          examined rsstat.
;          dcdsr has the dcd/dsr states prior to their last state
;          changes.
;-----------------------------------------------------------------
; F42E
rst232:	php
	sei			; disable ints
	lda acia+srsn
	and #$60
	sta rsstat
	sta dcdsr
	plp
	rts
; -------------------------------------------------------------------------------------------------
; ##### channelio #####
;*****************************************
;* getin -- get character from channel   *
;*      channel is determined by dfltn.  *
;* if device is 0, keyboard queue is     *
;* examined and a character removed if   *
;* available.  devices 1,3-31 advance to *
;* basin.                                *
;*                                       *
;* exit:  .a = character                 *
;*        cy = 1, stop key error for cas-*
;*                cassetes and rs232     *
;*           = 0, otherwise.             *
;*        z  = 1, if kbd and queue empty.*
;*****************************************
; F43D
ngetin:	lda dfltn		; check device
	bne gn10		; not keyboard

	lda ndx			; queue index
	ora kyndx		; check function key que
	beq gn20		; nobody there...exit

	sei
	jsr jlp2		; go remove a character
	clc
	rts

; Check for input from device 2 = RS232
gn10:	cmp #2			; is it rs-232
	beq gn232
	jmp basin		; no...use basin

; getin RS232
gn232:	sty xsav		; save .y...
	stx savx		; ..and .x
	ldy ridbs		; get last byte address
	cpy ridbe		; see if buffer emptyy
	bne gn15		; rs232 buffer not empty...

	lda acia+cdr		; make sure receiver is on
	and #$FD
	ora #$01		; bits(10) = 01 now
	sta acia+cdr
	lda rsstat		; set empty input buffer condition
	ora #$10
	sta rsstat
	lda #0			; return a null byte
	beq gnexit		; always

; Get one byte from RS232 input buffer
gn15:	lda rsstat		; clear empty buffer status
	and #$ef
	sta rsstat
	ldx i6509
	lda ribuf+2
	sta i6509		; point at buffer
	lda (ribuf),y		; get last char
	stx i6509		; restore
	inc ridbs		; inc to next posistion
	bit sa			; check for ascii flag
	bpl gnexit		; not on...
	jsr tocbm		; convert to cbm code
gnexit:	ldy xsav		; restore .y
	ldx savx
gn20:	clc			; good return
	rts
; -------------------------------------------------------------------------------------------------
;***************************************
;* basin-- input character from channel*
;*     input differs from get on device*
;* #0 function which is keyboard. the  *
;* screen editor makes ready an entire *
;* line which is passed char by char   *
;* up to the carriage return.  note,   *
;* rs232 uses getin to get each char.  *
;* other devices are:                  *
;*      0 -- keyboard                  *
;*      1 -- cassette #1               *
;*      2 -- rs232                     *
;*      3 -- screen                    *
;*   4-31 -- ieee   bus                *
;*                                     *
;* exit: cy=1, stop key error for cas- *
;*             settes and rs232.       *
;*       cy=0, otherwise.              *
;*                                     *
;*       all other errors must be de-  *
;*       tected by checking status !   *
;***************************************
; F49C
nbasin:	lda dfltn		; check device
	bne bn10		; is not keyboard...

; input from keyboard
	lda pntr		; save current...
	sta lstp		; ... cursor column
	lda tblx		; save current...
	sta lsxp		; ... line number
	jmp bn15		; blink cursor until return

bn10:	cmp #3			; is input from screen?
	bne bn20		; no...

	jsr patch4a4		; ***** patch 4a-4 - remember bootom line no *****
	nop

	sta indx		; ...up on this line
bn15:	jsr jloop5		; pick up characters
	clc
	rts

bn20:	bcs bn30		; devices >3
	cmp #2			; rs232?
	beq bn50

	jsr xtape		; go to tape indirect

; input from ieee bus
bn30:	lda status		; status from last
	beq bn35		; was good
bn31:	lda #$D			; bad...all done
bn32:	clc			; valid data
bn33:	rts

bn35:	jsr acptr		; good...handshake
	clc
	rts

; input from rs232
bn50:	jsr getin		; get data
	bcs bn33		; error return
	cmp #00			; non-null means good data always
	bne bn32		; have valid data
	lda rsstat		; check for valid null byte
	and #$10
	beq bn32		; ok
	lda rsstat		; buffer empty, check for errors in dsr, dcd
	and #dsrerr+dcderr
	bne bn31		; have error...send c/r's
	jsr stop		; check for stop key depressed
	bne bn50		; no, stay in loop 'til we get something
	sec			; .a=0, stop key error
	rts
; -------------------------------------------------------------------------------------------------
;***************************************
;* bsout -- out character to channel   *
;*     determined by variable dflto:   *
;*     0 -- invalid                    *
;*     1 -- cassette #1                *
;*     2 -- rs232                      *
;*     3 -- screen                     *
;*  4-31 -- ieee   bus                 *
;*                                     *
;* exit:  cy=1, stop key error for cas-*
;*              settes and rs232.      *
;*        cy=0, otherwise.             *
;*                                     *
;*       note, other errors must be de-*
;*       tected by checking status !   *
;***************************************
; F4EE
nbsout:	pha			; preserve .a
	lda dflto		; check device
	cmp #3			; is it the screen?
	bne bo10		; no...

; print to crt
	pla			; restore data
	jsr jprt		; print on crt
	clc
	rts

bo10:	bcc bo20		; device 1 or 2

; print to ieee   bus
	pla
	jsr ciout
	clc
	rts

; print to cassette devices
bo20:	cmp #2			; rs232?
	beq bo50

	pla
	jsr xtape		; go to tape indirect

rstbo:	pla			; restore .a (error exit for 232)
	bcc rstor1		; no error
	lda #00			; stop error if c-set
rstor1:	rts

; output to rs232
bo50:	stx t1			; put in a temp
	sty t2

bo55:	lda rsstat		; check for dsr,dcd errors
	and #$60
	bne bo90		; bad....

bo70:	pla			; restore data
	bit sa			; check for cbm to ascii conversion
	bpl bo80		; none
	jsr toasci		; convert cbm to ascii
bo80:	sta acia+drsn		; sending data
	pha

bo60:	lda rsstat
	and #$60		; dcd,dsr errors?
	bne bo90		; yes...
bo64:	lda acia+srsn
	and #$10		; transmit buffer empty?
	bne bo90		; yes, transmit done!
	jsr stop		; check for stop key
	bne bo60		; try again
bo66:	sec			; stop key/error return
	bcs rstbo		; exit....

bo90:	pla
	ldx t1          ;go restore
	ldy t2
	clc
	rts
; -------------------------------------------------------------------------------------------------
; ##### openchannel #####
;***************************************
;* nchkin -- open channel for input    *
;*                                     *
;* the number of the logical file to be*
;* opened for input is passed in .x.   *
;* chkin searches the logical file     *
;* to look up device and command info. *
;* errors are reported if the device   *
;* was not opened for input ,(e.g.     *
;* cassette write file), or the logical*
;* file has no reference in the tables.*
;* device 0, (keyboard), and device 3  *
;* (screen), require no table entries  *
;* and are handled separate.           *
;***************************************
; F549
nchkin:	jsr lookup		; see if file known
	beq jx310		; yup...

	jmp error3		; no...file not open

jx310:	jsr jz100		; extract file info
	lda fa
	beq jx320		; is keyboard...done.

; could be screen, keyboard, or serial
	cmp #3
	beq jx320		; is screen...done.
	bcs jx330		; is serial...address it
	cmp #2			; rs232?
	bne jx315		; no...

; rs232 channel
	lda sa
	and #02			; check for input
	beq jx316		; not input file
	and acia+cdr		; check if running
	beq jx312		; is...done ?? (rceiver on => yes)
	eor #$FF		; flip all bits
	and acia+cdr		; turn on...
	ora #$01		; turn on dtr ;bits(10)=01
	pha
	jsr rst232		; reset rs232 status
	pla
	sta acia+cdr		; set command
jx312:	lda #2			; device
	bne jx320		; bra...done

; some extra checks for tape
jx315:	jsr xtape		; goto tape indirect

jx316:	jmp error6		; not input file

jx320:	sta dfltn		; all input come from here

	clc			; good exit
	rts

; an serial device has to be a talker
jx330:	tax			; device # for dflto
	jsr talk		; tell him to talk

	lda sa			; a second?
	bpl jx340		; yes...send it
	jsr tkatn		; no...let go
	jmp jx350

jx340:	jsr tksa		; send second

jx350:	txa
	bit status		; did he listen?
	bpl jx320		; yes

	jmp error5		; device not present
; -------------------------------------------------------------------------------------------------
;***************************************
;* chkout -- open channel for output   *
;*                                     *
;* the number of the logical file to be*
;* opened for output is passed in .x.  *
;* chkout searches the logical file    *
;* to look up device and command info. *
;* errors are reported if the device   *
;* was not opened for input ,(e.g.     *
;* keyboard), or the logical file has  *
;* reference in the tables.            *
;* device 0, (keyboard), and device 3  *
;* (screen), require no table entries  *
;* and are handled separate.           *
;***************************************
; F5A3
nckout:	jsr lookup		; is file in table?
	beq ck5			; yes...

	jmp error3		; no...file not open

ck5:	jsr jz100		; extract table info
	lda fa			; is it keyboard?
	bne ck10		; no...something else.

ck20:	jmp error7		; yes...not output file

;could be screen,serial,or tapes
ck10:	cmp #3
	beq ck30		; is screen...done
	bcs ck40		; is serial...address it
	cmp #2			; rs232?
	bne ck15

; rs232 output
	lda sa			; check if output file
	lsr
	bcc ck20		; not so...
	jsr rst232		; reset rs232 status
	jsr xon232		; make sure transmit is on
	lda #2			; device#
	bne ck30		; bra...done

; special tape channel handling
ck15:	jsr xtape		; goto system tape indirect

ck30:	sta dflto		; all output goes here

	clc			; good exit
	rts

ck40:	tax			; save device for dflto
	jsr listn		; tell him to listen

	lda sa			; is there a second?
	bpl ck50		; yes...

	jsr scatn		; no...release lines
	bne ck60		; branch always

ck50:	jsr secnd		; send second...

ck60:	txa
	bit status		; did he listen?
	bpl ck30		; yes...finish up

	jmp error5		; no...device not present
; -------------------------------------------------------------------------------------------------
; ##### close #####
;*************************************
;* nclose -- close logical file      *
;*                                   *
;* enter:                            *
;*     cy =1 ,transmit close to dev- *
;*            ice.                   *
;*     cy =0 ,only remove from kernal*
;*            tables.                *
;*                                   *
;*     the logical file number of the*
;* file to be closed is passed in .a.*
;* keyboard, screen, and files not   *
;* open pass straight through. tape  *
;* files open for write are closed by*
;* dumping the last buffer and       *
;* conditionally writing an end of   *
;* tape block.serial files are closed*
;* by sending a close file command if*
;* a secondary address was specified *
;* in its open command.              *
;*************************************
; F5ED
nclose:	php			; save cy flag
	jsr jltlk		; look file up
	beq jx110		; was open...continue
	plp
	clc			; was never open...no error
	rts

jx110:	jsr jz100		; extract table data
	plp			; retrieve cy flag
	txa			; save table index
	pha
	bcc jx150		; close out table entries only

	lda fa			; check device number
	beq jx150		; is keyboard...done
	cmp #3
	beq jx150		; is screen...done
	bcs jx120		; is ieee...process
	cmp #2			; rs232?
	bne jx115		; no...

; close rs-232 file
cls232:	lda #0
	sta acia+cdr		; do a soft reset
	beq jx150		; jmp...remove file

; close cassette file
jx115:	pla			; cassette now closes the channel...
	jsr jx151		; before transmitting out the final data
	jsr xtape		; goto tape indirect

; close an ieee file
jx120:	jsr clsei

; entry to remove a give logical file from table of logical, primary, and secondary addresses
jx150:	pla			; get table index off stack
jx151:	tax			; entry for cassette special
	dec ldtnd
	cpx ldtnd		; is deleted file at end?
	beq jx160		; yes...done

; delete entry in middle by moving last entry to that position.
	ldy ldtnd
	lda lat,y
	sta lat,x
	lda fat,y
	sta fat,x
	lda sat,y
	sta sat,x
jx160:	clc
jx170:	rts			; close exit

; lookup tablized logical file data
lookup:	lda #0
	sta status
	txa
jltlk:	ldx ldtnd
jx600:	dex
	bmi lkups4
	cmp lat,x
	bne jx600
	clc
	rts

; routine to fetch table entries
jz100:	lda lat,x
	sta la
	lda fat,x
	sta fa
	lda sat,x
	sta sa
jz101:	rts

; sa is passed in .y
; routine looks for match in tables
; carry set if not present
; carry clear:
; .a=la,.x=fa,.y=sa
lkupsa:	tya
	ldx ldtnd
lkups2:	dex
	bmi lkups4
	cmp sat,x
	bne lkups2
	clc
lkups3:	jsr jz100		; get table data
	tay
	lda la
	ldx fa
	rts
lkups4:	sec
	rts			; not found exit

; la is passed in .a
; routine looks for match in tables
; carry set if not found
; carry clear:
; .a=la,.x=fa,.y=sa
lkupla:	tax
	jsr lookup
	bcc lkups3
	rts
; -------------------------------------------------------------------------------------------------
; ##### clall #####
;******************************************
;* nclall -- close all logical files      *
;*      deletes all table entries and     *
;* restores default i/o channels          *
;* and clears ieee port devices           *
;******************************************
;------------------------------------------
; new ncall
;  closes all files untill done or an
;  error occurs.
;  entry:
;    c-clr => close all files
;    c-set => .a = fa (device to be closed)
;------------------------------------------
; F67F Close all logical files
nclall:	ror xsav		; save carry
	sta savx		; save .a
ncl010:	ldx ldtnd		; scan index
ncl020:	dex
	bmi nclrch		; all done...clear channels (dclose patch 5/31/83)
	bit xsav		; check for fixed fa
	bpl ncl030		; none...
	lda savx
	cmp fat,x
	bne ncl020		; no match...
ncl030:	lda lat,x		; close this la
	sec			; c-set required to close
	jsr close
	bcc ncl010

ncl040:	lda #0			; original entry for nclall
	sta ldtnd		; forget all files
; -------------------------------------------------------------------------------------------------
;********************************************
;* nclrch -- clear channels                 *
;*   unlisten or untalk ieee devices, but   *
;* leave others alone.  default channels    *
;* are restored.                            *
;********************************************
; F6A6
nclrch:	ldx #3
	cpx dflto		; is output channel ieee?
	bcs jx750		; no...

	jsr unlsn		; yes...unlisten it

jx750:	cpx dfltn		; is input channel ieee?
	bcs clall2		; no...

	jsr untlk		; yes...untalk it

; restore default values
clall2:	ldx #3
	stx dflto		; output chan=3=screen
	lda #0
	sta dfltn		; input chan=0=keyboard
	rts
; -------------------------------------------------------------------------------------------------
; ##### open #####
;***********************************
;*                                 *
;* open function                   *
;*                                 *
;* enter: cy=1, transmit command to*
;*              device.            *
;*        cy=0, perform open opera-*
;*              tion.              *
;*                                 *
;* la, fa, sa must be set up prior *
;* to the call to this routine, as *
;* well as the file name descript- *
;* tor.                            *
;*                                 *
;***********************************
; F6BF
nopen:	bcc     op000		; do open
	jmp     tranr		; do transmit

;***********************************
;*                                 *
;* create an entry in the logical  *
;* files tables consisting of      *
;* logical file number--la, device *
;* number--fa, and secondary cmd-- *
;* sa.                             *
;*                                 *
;* a file name descriptor, fnadr & *
;* fnlen, is passed to this routine*
;*                                 *
;***********************************
op000:	ldx la			; check file #

; bne op98 ;is not the keyboard
; jmp error6 ;not input file...

op98:	jsr lookup		; see if in table
	bne op100		; not found...o.k.

	jmp error2		; file open

op100:	ldx ldtnd		; logical device table end
	cpx #10			; maximum # of open files
	bcc op110		; less than 10...o.k.

	jmp error1		; too many files

op110:	inc ldtnd		; new file
	lda la
	sta lat,x		; store logical file #
	lda sa
	ora #$60		; make sa an ieee command
	sta sa
	sta sat,x		; store command #
	lda fa
	sta fat,x		; store device #

; perform device specific open tasks
	beq op175		; is keyboard...done.
	cmp #3
	beq op175		; is screen...done.
	bcc op150		; are cassettes 1 & 2

	jsr openi		; is on ieee...open it
	bcc op175		; branch always...done

; perform tape open stuff
op150:	cmp #2
	bne op152

	jmp opn232

op152:	jsr xtape		; goto tape device indirect

op175:	clc			; flag good open
op180:	rts			; exit in peace

openi:	lda sa
	bmi op50		; no sa...done

	ldy fnlen
	beq op50		; no file name...done

	lda fa
	jsr listn		; device la to listen

	lda sa
	ora #$f0
openib:	jsr secnd

	lda status		; anybody home?
	bpl op35		; yes...continue

; this routine is called by other kernal routines which are called directly by os.
; kill return address to return to os.
	pla
	pla
	jmp error5		; device not present

op35:	lda fnlen
	beq op45		; no name...done sequence

; send file name over ieee
	ldy #0
op40:	jsr fnadry
	jsr ciout
	iny
	cpy fnlen
	bne op40

op45:	jsr unlsn

op50:	clc			; no  error
	rts
; -------------------------------------------------------------------------------------------------
;*****************************************
;*  transmit command to device           *
;*                                       *
;*   fnlen,fnadr must be set up already  *
;*   to contain the command string.      *
;*   fa must be set for the device.      *
;*****************************************
; F73A
tranr:  lda fa
	jsr listn
	lda #$6F
	sta sa
	jmp openib
; -------------------------------------------------------------------------------------------------
; ##### load #####
;**************************************
;* load ram function     10/30/81     *
;*                                    *
;*  loads from cassette 1 or 2, or    *
;*  ieee bus devices >=4 to 31 as     *
;*  determined by contents of         *
;*  variable fa.                      *
;* entry:                             *
;*   .a(bit 7)=0 performs load        *
;*   .a(bit 7)=1 performs verify      *
;*   .a(bits 0123)=start segment      *
;*   .x=start address low             *
;*   .y=start address high            *
;*   if .x=$ff & .y=$ff => fixed load *
;* exit:                              *
;*   .a(bits 0123)=end segment        *
;*   .x=end address low               *
;*   .y=end address high              *
;*                                    *
;**************************************
; F746
nload:	stx relsal		; save alt address
	sty relsah
	sta verck		; set verify flag (n)
	sta relsas		; save start address
	lda #0			; clear status
	sta status

	lda fa			; check device number
	bne ld20

ld10:	jmp error9		; bad device #-keyboard

ld20:	cmp #3
	beq ld10		; disallow screen load
	bcs *+5
	jmp ld100		; handle tapes different

; load from cbm ieee device
	lda #$60		; special load command
	sta sa

	ldy fnlen		; must have file name
	bne ld25		; yes...ok

	jmp error8		; missing file name

ld25:	jsr luking		; tell user looking
	jsr openi		; open the file

	lda fa
	jsr talk		; establish the channel
	lda sa
	jsr tksa		; tell it to load

	jsr acptr		; get first byte
	sta eal
	sta stal

	lda status		; test status for error
	lsr
	lsr
	bcc *+5			; file  found...

	jmp error4		; file not found error

	jsr acptr
	sta eah
	sta stah

	jsr loding		; tell user loading

; test for fixed or moveable load
	lda relsas		; no segment byte in storage ***
	sta eas
	sta stas
	lda relsal
	and relsah
	cmp #$FF
	beq ld40		; fixed load

	lda relsal
	sta eal
	sta stal
	lda relsah
	sta eah
	sta stah
;
ld40:	lda #$FD		; mask off timeout
	and status
	sta status

	jsr stop		; stop key?
	bne ld45		; no...

	jmp break		; stop key pressed

ld45:	jsr acptr		; get byte off ieee
	tax
	lda status		; was there a timeout?
	lsr
	lsr
	bcs ld40		; yes...try again
	txa
; change indirect pages
	ldx i6509
	ldy eas
	sty i6509
	ldy #0
	bit verck		; performing verify?
	bpl ld50		; no...load
	sta sal			; use as a temp
	lda (eal),y
	cmp sal
	beq ld60		; okay
	lda #sperr		; no good...verify error
	jsr udst		; update status
	!byte $AD		; skip next store

ld50:	sta (eal),y
ld60:	stx i6509		; restore indirect
	inc eal			; increment store addr
	bne ld64
	inc eah
	bne ld64
	inc eas
	lda #2			; skip $0000 $0001
	sta eal
ld64:	bit status		; eoi?
	bvc ld40		; no...continue load

	jsr untlk		; close channel
	jsr clsei		; close the file
	jmp ld180		; exit ieee load

ld90:	jmp error4		; file not found

; load from tape
ld100:	jsr xtape		; goto tape indirect

ld180:	clc			; good exit

; set up end load address
	lda eas
	ldx eal
	ldy eah

ld190:	rts

; subroutine to print to console: searching [for name]
luking:	bit msgflg		; supposed to print?
	bpl ld115
	ldy #ms5-ms1		; "searching"
	jsr spmsg
	lda fnlen
	beq ld115
	ldy #ms6-ms1		; "for"
	jsr spmsg

; subroutine to output file name
outfn:	ldy fnlen		; is there a name?
	beq ld115		; no...done
	ldy #0
ld110:	jsr fnadry
	jsr bsout
	iny
	cpy fnlen
	bne ld110

ld115:	rts

; subroutine to print: loading/verifing
loding:	ldy #ms10-ms1		; assume 'loading'
	lda verck		; check flag
	bpl ld410		; are doing load
	ldy #ms21-ms1		; are 'verifying'
ld410:	jmp spmsg

; rsr  fix segmentation 10/15/81
; rsr  6509 changes  10/15/81
; -------------------------------------------------------------------------------------------------
; ##### save #####
;***************************************
;* nsave              10/30/81         *
;*                                     *
;* saves to cassette 1 or 2, or        *
;* ieee devices 4>=n>=31 as selected   *
;* by variable fa.                     *
;*                                     *
;* .x => zpage address of start vector *
;* .y => zpage address of end vector   *
;***************************************
; F84C
nsave:	lda 0,x			; get start vector
	sta stal
	lda 1,x
	sta stah
	lda 2,x
	sta stas
	tya
	tax
	lda 0,x			; get end vector
	sta eal
	lda 1,x
	sta eah
	lda 2,x
	sta eas

	lda fa
	bne sv20

sv10:	jmp error9		; bad device #

sv20:	cmp #3
	beq sv10
	bcc sv100
	lda #$61
	sta sa
	ldy fnlen
	bne sv25

	jmp error8		; missing file name

sv25:	jsr openi
	jsr saving
	lda fa
	jsr listn
	lda sa
	jsr secnd
	ldx i6509		; indirects switched by rd300
	jsr rd300
	lda sal
	jsr ciout
	lda sah
	jsr ciout

	ldy #0
sv30:	jsr cmpste		; compare start to end
	bcs sv50		; have reached end
	lda (sal),y
	jsr ciout
	jsr incsal
	jsr stop
	bne sv30

	stx i6509		; restore indirects
break:	jsr clsei
	lda #0
	sec
	rts

sv50:	stx i6509		; restore indirects
	jsr unlsn

clsei:	bit sa
	bmi clsei2
	lda fa
	jsr listn
	lda sa
	and #$EF
	ora #$E0
	jsr secnd
	jsr unlsn

clsei2:
sv110:	clc
sv115:	rts

sv100:	jsr xtape		; goto tape device

; subroutine to output: 'saving <file name>'
saving:	lda msgflg
	bpl sv115		; no print

	ldy #ms11-ms1		; 'saving'
	jsr spmsg
	jmp outfn		; <file name>
; -------------------------------------------------------------------------------------------------
; ##### time #####
;----------------------------------------
; time and alarm routines for 6526
;      rsr 11/12/81
;
; rdtim - read the time
;  .y = (bit7=pm,bit6/5=t8/t4,bits4-0 hrs)
;  .x = (bit7=t2,bits6-0 minutes)
;  .a = (bit7=t1,bits6-0 seconds)
;----------------------------------------
; F8E6
rdtim:	lda cia+tod10
	pha			; save for later
	pha
	asl			; shift to add to todhrs
	asl
	asl
	and #$60		; bit posistions 5,6
	ora cia+todhr
	tay			; return in .y
	pla
	ror			; shift to add to todsec
	ror
	and #$80
	ora cia+todsec
	sta sal			; save for later
	ror			; shit to add to todmin
	and #$80
	ora cia+todmin
	tax			; return in .x
	pla
	cmp cia+tod10		; watch out for rollover
	bne rdtim		; ...it changed do again...
	lda sal
	rts
; -------------------------------------------------------------------------------------------------
; settim - set tod and alarm
;  c-set => set alarm
;  c-clr => set tod
;  registers same as rdtim
;----------------------------------------
; F90E
settim:	pha			; save for later
	pha
	ror			; set bit 8
	and #$80
	ora cia+crb
	sta cia+crb
	tya			; get bits from todhrs
	rol
	rol
	rol sal			; bit t8 (don't need to clear sal)
	rol
	rol sal			; bit t4
	txa			; get bit from todmin
	rol
	rol sal			; bit t2
	pla			; get bit from todsec
	rol
	rol sal			; bit t1
	sty cia+todhr
	stx cia+todmin
	pla
	sta cia+todsec
	lda sal
	sta cia+tod10
	rts
; -------------------------------------------------------------------------------------------------
; ##### errorhandler #####
;************************************
;* error handler                    *
;*  restores i/o channels to default*
;*  prints kernal error message if  *
;*  bit 6 of msgflg set.  returns   *
;*  with error # in .a and carry.   *
;************************************
; F939 
error1:	lda #1			; too many files
	!byte $2c
error2:	lda #2			; file open
	!byte $2c
error3:	lda #3			; file not open
	!byte $2c
error4:	lda #4			; file not found
	!byte $2c
error5:	lda #5			; device not present
	!byte $2c
error6:	lda #6			; not input file
	!byte $2c
error7:	lda #7			; not output file
	!byte $2c
error8:	lda #8			; missing file name
	!byte $2c
error9:	lda #9			; bad device #

errorx:	pha			; error number on stack
	jsr clrch		; restore i/o channels

	ldy #ms1-ms1
	bit msgflg		; are we printing error?
	bvc erexit		; no...

	jsr msg			; print "cbm i/o error #"
	pla
	pha
	ora #$30		; make error # ascii
	jsr bsout		; print it

erexit:	pla
	sec
	rts
; -------------------------------------------------------------------------------------------------
;***************************************
;* stop -- check stop key flag and     *
;* return z flag set if flag true.     *
;* also closes active channels and     *
;* flushes keyboard queue.             *
;* also returns key downs from last    *
;* keyboard row in .a.                 *
;***************************************
; F96B Check the stop key
nstop:	lda stkey		; value of last row
	and #$01		; check stop key position
	bne stop2		; not down
	php
	jsr clrch		; clear channels
	sta ndx			; flush queue
	plp
stop2:	rts
; -------------------------------------------------------------------------------------------------
; udtim - update the stop key location
;   expects keyboard outputs set to
;   default value. bit 0 of stkey =0
;   for stop key down.
;---------------------------------------
; F979 
udtim:	lda tpi2+pc		; check keyboard
	lsr
	bcs udexit		; no  stop key
	lda #$FE		; check for shift
	sta tpi2+pb
	lda #$10
	and tpi2+pc
	bne udttt		; no shift key
	sec			; shift key mark
udttt:	lda #$FF		; clear
	sta tpi2+pb
udexit:	rol			; move bit 0 back
	sta stkey
	rts
; -------------------------------------------------------------------------------------------------
; ##### init #####
;------------------------------------------------
; start - system reset routine
;  kernal checks on 4k boundries from $1000-$8000
;    first occurance has priority.
;    if no occurance then $e000 is used for vector
;    $e000 => monitor start
;  kernal expects:
;    $x000 - jmp init  (cold start)
;    $x003 - jmp winit (warm start)
;    $x006 - 'c'(+$80)=> kernal cold start first
;    $x007 - 'b'+$80
;    $x008 - 'm'+$80
;    $x009 - 'x'  x=4k bank (1-8)
;------------------------------------------------
; F995 Test bytes for ROMs
patall: !byte $C2,$CD		; $x004 rom pattern
; F997
start:	ldx #$FE		; do all normal junk...
	sei
	txs
	cld
	lda #$A5
	cmp evect+2			; compare warm start flag
	bne scold			; jump to cold start
	lda evect+3			; load warm start flag
	cmp #$5A
	beq swarm			; jump to warm start
;
; check for warm start
scold:	lda #6			; set up indirect to $0006 = position ROM ident bytes
	sta eal
	lda #0			; clear upper
	sta eah
	sta evect		; set low byte of vector warm start to $00
	ldx #$30		; existance flag 4. rom ident byte compare value to '0'
sloop0: ldy #3			; set counter to 4th ROM ident byte
	lda eah
	bmi sloop2		; no roms but this one... -> monitor cold boot
	clc			; calc new test point
	adc #$10                ; 4k steps
	sta eah
	inx			; next 4. byte compare value $31, $32, $33...
	txa
	cmp (eal),y		; compare if 4. byte $31 at address $1006+3, $32 at $2006...
	bne sloop0		; 4. byte does not mach - > next ROM pos. $2000, $3000...
	dey			; check next byte backwards if 4th byte matches
sloop1: lda (eal),y		; load 3., 2., 1. byte
	dey
	bmi sloop3		; all done...correctly - 2.+3. byte matches -> autostart ROM found!
	cmp patall,y		; compare test bytes 'M', 'B'
	beq sloop1		; 3. byte OK -> check 2. byte
	bne sloop0		; no good... 2. or 3. ident byte does not mach

; monitor (could be test for keydown ***)
sloop2: ldy #$E0                ; monitor vector
	!byte $2C		; skip two bytes
sloop3: ldy eah
	sty evect+1             ; set high byte of vector

	tax                     ; move 1. ident byte to x to set N-flag
	bpl swarm               ; don't use kernal initilization
				;   jump to warm start if value is positive ('c'=$43)

; kernal cold start
	jsr ioinit              ; initilize i/o -> $F9FE (TPI1, TPI2, CIA, TOD)
	lda #$F0		; prevent damage to non-tested buffers
	sta pkybuf+1            ; start F-keys
	jsr jcint               ; cinit call for non-cleared system $E004 -> cint $E044
	jsr ramtas              ; ram-test and set -> $FA94
	jsr restor              ; operationg system vectors -> $FBB1 (copies $0300 Vector Table)
	jsr jcint               ; screen editor init $E004 -> cint $E044 (editor, F-Keys, VIC)
	lda #warm		; Kernal initilize done flag
	sta evect+2             ; save first warm start flag $A5
; F9FB Warm start entry
swarm:  jmp (evect)             ; start exit -> basic warm start $BBA0
; -------------------------------------------------------------------------------------------------
; ioinit - initilize i/o system
;   6509/6525/6525/6526
;   must be entered with irq's disabled
;------------------------------------------
; F9FB I/O register init (TPI1, TPI2, CIA, TOD)

; 6525 tpi1 initilization code
ioinit: lda #%11110011		; cb,ca=hi ie3,4=neg ip=1 mc=1
	sta tpi1+creg		; interrupt mode = on, parity
	ldy #$FF
	sty tpi1+ddpc
; pb4=output 1, to claim dbus
	lda #%01011100  	; wrt=lo unused netr=off
	sta tpi1+pb		; IEEE ifc=0, netw.=0, arb.sw.=1, cass. write=0,motor=1 
	lda #%01111101  	; set directions
	sta tpi1+ddpb		; input: cassette switch, IEEE srq
				; output: IEEE ifc, network, arb.sw., cass. motor,write

	lda #%00111000		; IEEE controls off: dc=0, te=0, ren=0, atn=1, dav=1, eo=1
	sta tpi1+pa
	lda #%00111111  	; IEEE control to transmitt, data to receive
	sta tpi1+ddpa		; in: ndac, nfrd / out: dc, te, ren, atn, dav, eoi
;
; 6525 tpi2 initilization code
; .y = $ff from above
	sty tpi2+pa             ; keyboard out 8-15=1
	sty tpi1+pb             ; IEEE ifc=1, network=1, arb.sw.=1, cass. motor=1,write=1
	sty tpi2+ddpa           ; dir keyboard 8-15 = output
	sty tpi2+ddpb           ; dir keyboard 0-7 = output
	lsr tpi2+pa             ; clear keyboard 15 bit #7
	iny			; .y=0 set up vic selects=in for system jumpers
	sty tpi2+pc
	sty tpi2+ddpc		; keyboard=in (0-5)
;
; 6526 cia initilization code
	lda #$7f		; turn off all irq sources from 6526...
	sta cia+icr
; .y =$00 from above
	sty cia+ddra		; dir input: IEEE data, #6,7 also trigger 1,2
	sty cia+ddrb		; dir input: game 1,2
	sty cia+crb		; Timer B stop, PB7=off, cont, Phi2, activate TOD write
; activate tod
	sta cia+tod10		; clear TOD 1/10 seconds
; 60/50 hz test code for tod
	sty tpi1+lir		; clear all interrupts
io100:  lda tpi1+lir		; wait untill it happens again
	ror			; shift bit #0 to carry
	bcc io100		; pc0 = 1 -> 50/60hz irq
	sty tpi1+lir		; clear it again
;
; start a timmer
	ldx #0			; .y=$00 from above
io110:  inx
	bne io110		; delay 256x -> 1.28 ms @ 1MHz
	iny
	lda tpi1+lir		; load interrrupt latch reg
	ror
	bcc io110		; pc0 = 1 -> 50/60hz irq          
	cpy #id55hz   
	bcc io120               ; it was 60 hz, signal appears again in 14 tries = <18ms
	lda #%10001000		; set for 50hz
	!byte $2C		; skip two bytes
io120:  lda #%00001000
	sta cia+cra            ; set TOD=50/60Hz, run mode=continuous

; 6526  inter-process communication initialization
;  pra = data port
;  prb = ipc lines
;  irq's from 2nd processor via flag input
	lda ipcia+icr		; clear icr
	lda #$90    
	sta ipcia+icr		; flag irqs on
	lda #$40    
	sta ipcia+prb		; no nmi to z80, sem6509 low
	stx ipcia+ddra		; port direction
	stx ipcia+crb		; timer b off
	stx ipcia+cra		; timer a off
	lda #%01001000		; port b lines sem65,ennmi are outs
	sta ipcia+ddrb

; 6551 initilization code handled by reset  10/19/81 rsr

; turn off ifc
	lda #ifc
	ora tpi1+pb		; TPI1 PB set bit #0 IEEE ifc=1
	sta tpi1+pb 
	rts         
; -------------------------------------------------------------------------------------------------
; ramtas - initilize lower ram with $00
;  and test all system dynamic ram
;  set ram limits (64k bank min size)
;  alocatate initial buffer space
;  turn off rs232 and cassette buffers
;  reset xtape vectors to non-cassette
;-----------------------------------------
; FA88 RAM-test / vector init
ramtas: lda #0			; init value A = $00, counter X = 0
	tax
px1:    sta $0002,x		; clear ZP above 6509 bank regs
	sta buf,x		; clear basic input buffer from $0200       
	sta evect-$100,x	; clear kernal RAM till evct $03F8
	inx
	bne px1			; clear next byte

; memory size check
	lda #1			; bottom of memory always segment 1 (cbmii)
	sta i6509
	sta memstr+2		; set bottom of user memory
	sta lowadr+2		; ...and system memory
	lda #2			; start at byte $0002
	sta memstr
	sta lowadr
	dec i6509		; place back one segment for test

; memsiz,sal,lowadr are zeroed above
sizlop: inc i6509		; claculate next ind bank
	lda i6509
	cmp #irom		; all slots full...exit
	beq size
	ldy #2			; always start at $0002, sal/sah already $0000
siz100: lda (sal),y
	tax			; save memory value in X 
	lda #$55		; test with $55
	sta (sal),y
	lda (sal),y
	cmp #$55		; check if $55 
	bne size		; end test if different
	asl			; test with $AA
	sta (sal),y
	lda (sal),y
	cmp #$AA		; check if $AA
	bne size		; end test if different
	txa
	sta (sal),y		; restore old memory value from X
!ifdef FULL_RAMTEST{		; ********** Full RAM-test **********
	iny
	bne siz100		; test next byte
} else{				; ********** Fast RAM-test PATCH **********
	nop
	nop
	nop
}
	inc sah
	bne siz100		; test next page
	beq sizlop		; test next bank

; set top of memory
size:   ldx i6509		; bank number of failure
	dex			; back up one segment
	txa			; .a= bank#
	ldx #$FF
	ldy #$FD		; reserve top two pages for swapping system
	sta hiadr+2		; set system top of memory = $FDFF
	sty hiadr+1
	stx hiadr

; allocate 3 pages (512funcs,256rs232)
	ldy #$FD-3		; user memory top = $FAFF
	clc
	jsr memtop              ; set user top of memory

; flag buffers as not assigned =>$ff
	dec ribuf+2             ; init rs232 buffer bank to $FF
	dec tape1+2             ; init tape buffer bank to $FF
	lda #<nocass		; set up cassette indirects to $FE6B
	sta itape
	lda #>nocass
	sta itape+1
	rts
; -------------------------------------------------------------------------------------------------
; FAFD standard vector table - initialized at boot from restor sub to cinv $0300
jmptab: !word yirq		; FB09 -> FBF8 cinv	
	!word timb		; FB0B -> EE21 cbinv....brk goes to monitor
	!word panic		; FB0D -> FCB8 no.....nminv !!!!!
	!word nopen		; FB0F -> F6C6 open file
	!word nclose		; FB11 -> F5F4 close file
	!word nchkin		; FB13 -> F550 open channel in
	!word nckout		; FB15 -> F5AA open channel out
	!word nclrch		; FB17 -> F6AD close channel
	!word nbasin		; FB19 -> F4A3 input from channel
	!word nbsout		; FB1B -> F4F5 output to channel
	!word nstop		; FB1D -> F972 scan stop key
	!word ngetin		; FB1F -> F444 scan keyboard
	!word nclall		; FB21 -> F686 close all files
	!word nload		; FB23 -> F74D load from file
	!word nsave		; FB25 -> F853 save to file
	!word s0		; FB27 -> EE73 monitor command parser
	!word jescrt		; FB29 -> E01F esc key vector
	!word jescrt		; FB2B -> E01F user ctrl key vector
	!word nsecnd		; FB2D -> F27B IEEE listen secondary address
	!word ntksa		; FB2F -> F287 IEEE talk secondary address
	!word nacptr		; FB31 -> F311 IEEE character in
	!word nciout		; FB33 -> F29E IEEE character out
	!word nuntlk		; FB35 -> F2B2 IEEE untalk bus
	!word nunlsn		; FB37 -> F2B6 IEEE unlisten bus
	!word nlistn		; FB39 -> F23B IEEE listen a device
	!word ntalk		; FB3B -> F237 IEEE talk to a device
tabend:
; -------------------------------------------------------------------------------------------------
; FB31 NMI entry, jumps indirect to NMI routine
nmi:    jmp (nminv)             ; ($0304) default -> panic = $FCB8
; -------------------------------------------------------------------------------------------------
; -------------------------------------------------------------------------------------------------
; FB34 Set file name address
; .a = filename length
; .x = zero page location of 3 byte address
setnam: sta fnlen		; store length
	lda $00,x		; load and store address
	sta fnadr
	lda $01,x
	sta fnadr+1
	lda $02,x
	sta fnadr+2
	rts
; -------------------------------------------------------------------------------------------------
; FB43 Set file paramaters
; .a = logical address
; .x = first address
; .y = secundary address
setlfs: sta la
	stx fa
	sty sa
	rts
; -------------------------------------------------------------------------------------------------
; FB4A Read/write status
; carry set -- read device status into .a
readst: bcc storst
	lda fa			; see which devices' to read
	cmp #2
	bne readss		; not rs-232
	lda rsstat		; yes get it and remember it
	pha
	lda #00			; clear status when read
	beq statxt		; jump
; FB5A Set the system message flag
setmsg: sta msgflg
readss: lda status		; read status
; set status bit
udst:   ora status		; set bit and store status
	sta status
	rts
; carry clear -- set device status with .a
storst: pha
	lda fa
	cmp #2
	bne storss		; not rs-232

statxt:	pla
	sta rsstat		; store rs-232 status
	rts
;
storss pla
	sta status
	rts
; -------------------------------------------------------------------------------------------------
; FB74 IEC timeout on/off
settmo: sta timout
	rts
; -------------------------------------------------------------------------------------------------
; FB78 Read/set top of memory
memtop: bcc settop

; carry set--read top of memory
	lda memsiz+2		; load user memory top in .a.x.y
	ldx memsiz
	ldy memsiz+1

; carry clear--set top of memory
settop:	stx memsiz		; set user memory top
	sty memsiz+1
	sta memsiz+2
	rts
; -------------------------------------------------------------------------------------------------
; FB8D Manage bottom of memory
membot: bcc setbot

; carry set--read bottom of memory
	lda memstr+2		; load bottom mem in .a.x.y
	ldx memstr
	ldy memstr+1

; carry clear--set bottom of memory
setbot: stx memstr		; set bottom mem
	sty memstr+1
	sta memstr+2
	rts
; -------------------------------------------------------------------------------------------------
; FBA2 Restore ram i/o vectors at $0300
restor: ldx #<jmptab		; load vector table address in kernal
	ldy #>jmptab
	lda #irom
	clc

; FBA9 Manage ram i/o vectors
vector: stx sal			; store address
	sty sah
	ldx i6509		; save indirect
	sta i6509		; set ibank .a 
	bcc vect50		; carry=0 -> set/restore table

; carry set--read vectors
	ldy #tabend-jmptab-1
vect20: lda cinv,y		; from ram table $F0300
	sta (sal),y		; into user area
	dey
	bpl vect20

; carry clear--set vectors
vect50: ldy #tabend-jmptab-1
vect60: lda (sal),y		; from user area
	sta cinv,y		; into ram table $F0300
	dey
	bpl vect60

	stx i6509		; restore indirect
	rts
; -------------------------------------------------------------------------------------------------
; FBCA vreset - reset vector flags and control
;   .x - low vector address  .y - high vector address
vreset: stx evect
	sty evect+1
	lda #winit
	sta evect+3
	rts
; -------------------------------------------------------------------------------------------------
; ##### irq #####
;**********************************************
;* nirq - handler for:       10/30/81 rsr     *
;* 6525 irq's:::::::::::::::::::::::::::::::::*
;* 6551 irq's                                 *
;*   (receiver,transmitter,dcd & dsr changes) *
;* 6526 irq's                                 *
;*   (alarm, timera, timerb)                  *
;* 6526 irq's                                 *
;*   (2nd processor)                          *
;* ieee srq                                   *
;* keyboard scan (50/60hz irq)                *
;*                                            *
;* also at present does not handle any of the *
;* 6566 (vic) interrupts.                     *
;**********************************************
; FBD6 IRQ handler
nirq:	pha			; save registers
	txa
	pha
	tya
	pha
	tsx			; check for brk...
	lda stack+4,x
	and #$10
	bne brkirq		; yes...
	jmp (cinv)		; via vector -> yirq $FBF8
brkirq: jmp (cbinv)		; yes...

; entry via indirect vector cinv
yirq:   lda i6509		; save indirect bank #
	pha
	;lda pass	; external break handler
	;pha
	;lda #0 	; clear for normal return
	;sta pass
	cld			; clear dec to prevent future problems
	lda tpi1+air
	bne irq000		; handle priority irq's

; external irq (vic and others)
;  (no code!!!!!!!!!!)
	jmp prendn

; Check for ACIA IRQ
irq000:	cmp #$10		; find irq source
	beq irq002		; not 6551...
	jmp irq100

; interrupt handler
irq002:	lda acia+srsn		; find irq source
	tax
	and #$60		; dcd/dsr changes ??
	tay
	eor dcdsr
	beq irq004		; no change...
	tya
	sta dcdsr		; update old dsr/dcd status
	ora rsstat		; update rs232 status
	sta rsstat
	jmp irq900		; done!
;
irq004:	txa
	and #$08		; receiver ??
	beq irq010		; no...

; receiver service
	ldy ridbe		; check buffers
	iny
	cpy ridbs		; have we passed start?
	bne irq005		; no...

	lda #doverr		; input buffer full error
	bne irq007		; bra...set status

irq005:	sty ridbe		; move end foward
	dey
	ldx ribuf+2
	stx i6509
	ldx acia+srsn		; get status register
	lda acia+drsn
	sta (ribuf),y		; data to buffer
	txa			; set status
	and #$07
irq007:	ora rsstat		; set status
	sta rsstat

irq010:	lda acia+srsn		; find irq source
	and #$10		; transmitter ?
	beq irq090		; no...
	lda acia+cdr		; check for transmitter on
	and #$0C
	cmp #$04		; bits(32)=01 => xmitter int enabled
	bne irq090		; off...

; transmitter service (no interrrupt driven transmissions)
	lda #%11110011		; turn of transmitter
	and acia+cdr
	sta acia+cdr
irq090:	jmp irq900		; exit..pop priority...

; Check for coprozessor IRQ
irq100:	cmp #$08		; check if inter-process irq
	bne irq110		; no...
	lda ipcia+icr		; clear irq condition
	cli			; this irq can be interrupted
	jsr ipserv		; do the request
	jmp irq900		; done!

; Check for CIA IRQ
irq110:	cli			; all other irq's may be interrupted, too
	cmp #$04		; check if 6526
	bne irq200		; no...

; 6526 interrupt reconized
	lda cia+icr		; get active interrupts
	ora alarm		; in case we lose something
	sta alarm

; nothing to do at present....need code ********
	jmp irq900		; ...dump interrupt

; Check for IEC bus IRQ (and ignore it)
irq200:	cmp #$02		; check for ieee srq
	bne irq300

; need code ************
	jmp irq900		; ...dump interrupt

; Must be a 50/60Hz IRQ - poll keyboard, update time
irq300:	jsr jkey		; scan the keyboard
	jsr udtim		; set stopkey flag

; test for cassette switch
	lda tpi1+pb		; get cass switch
	bpl irq310		; switch is down...
	ldy #0			; flag motor off...
	sty cas1
	ora #$40		; turn motor off...
	bne irq320		; jump
irq310:	ldy cas1		; test for flag on...
	bne irq900		; yes computer control..leave alone
	and #$FF-$40		; turn motor on...
irq320:	sta tpi1+pb		; store mods into port

irq900:	sta tpi1+air		; pop the interrupt...

prendn: ;lda pass	; check for foriegn call
	;bne segrti	; yes...return
	;pla
	;sta pass	; restore interrupted interrupt
	pla             ;restore registers
	sta i6509
prend:	pla			; entry point for register only
	tay
	pla
	tax
	pla

; Default NMI routine
panic:	rti			; come here if no new nmi vector.
; -------------------------------------------------------------------------------------------------
; send a request
;   enter:   ipb buffer is initialized to hold the
;            command
;            input parameter bytes
;
;   exit:    ipb buffer holds
;            output parameter bytes
;            all other bytes in ipb unchanged
;---------------------------------------------------------------
; FCAB Coprocessor request
iprqst:	lda ipb+ipccmd
	and #$7F
	tay
	jsr getpar		; get #ins,outs
	lda #sem88		; check 8088 semaphore
	and ipcia+prb
	bne iprqst		; locked out by other processor
	lda #sem65
	ora ipcia+prb		; lock 6509 semaphore
	sta ipcia+prb
	nop			; a pause

	lda ipcia+prb		; collisions with 8088?
	tax
	and #sem88
	beq ipr100		; ok...
	txa
	eor #sem65
	sta ipcia+prb		; nope, clear 6509 semaphore
	txa			; kill some time
	nop
	nop
	nop
	bne iprqst		; try again (br always)

;     send cmd byte and cause irq
ipr100: lda #$FF
	sta ipcia+ddra		; port direction = out
	lda ipb+ipccmd
	sta ipcia+pra		; write cmd byte to port
; cause irq
	jsr frebus		; give up bus
	lda ipcia+prb		; pb6 := 0
	and #$BF
	sta ipcia+prb
	ora #$40		; keep low for 4us (8 cycles)
	cli
	nop
	nop
	nop
	sta ipcia+prb		; pb6 := high

	jsr waithi		; sem8088 -> hi (cmd byte recvd)
	lda #$00
	sta ipcia+ddra		; port direction = in
	jsr acklo		; sem6509 -> lo (ack)
	jsr waitlo		; sem8088 -> lo (ack ack)

;    send data bytes, if any
	ldy #0
	beq ipr250		; always

ipr200:	lda #$FF
	sta ipcia+ddra		; port direction = out
	lda ipb+ipcdat,y	; get next data byte
	sta ipcia+pra		; write cmd out
	jsr ackhi		; sem6509 -> hi (data ready)
	jsr waithi		; sem8088 -> hi (data recvd)
	lda #$00
	sta ipcia+ddra		; port direction = in
	jsr acklo		; sem6509 -> lo (ack)
	jsr waitlo		; sem8088 -> lo (ack ack)
	iny			; bump index to next data byte
ipr250:	cpy ipb+ipcin		; any more ??
	bne ipr200		; yes...

;    receive data bytes, if any
	ldy #0
	beq ipr350		; always

ipr300:	jsr ackhi		; sem6509 -> hi (rdy to receive)
	jsr waithi		; sem8088 -> hi (data available)
	lda ipcia+pra		; get data from port
	sta ipb+ipcdat,y	; stuff it away
	jsr acklo		; sem6509 -> lo (data recvd)
	jsr waitlo		; sem8088 -> lo (ack)
	iny

ipr350:	cpy ipb+ipcout		; more?
	bne ipr300		; yes...
	rts			; done!!
; -------------------------------------------------------------------------------------------------
; service an 8088 request
;-------------------------------------------------------------------
; FD48 Coprocessor irq handler
ipserv: ;ldy #ipbsiz-1	; copy ip buffer to stack
	;ips050 lda ipb,y
	; pha
	; dey
	; bpl ips050

	lda #0
	sta ipcia+ddra		; port dir=in, just in case...
	lda ipcia+pra		; read cmd from port
	sta ipb+ipccmd		; store cmd and decode it
	and #$7F		; mask off bus bit
	tay
	jsr getpar		; get param counts
	tya			; adjust offset for jump table
	asl
	tay
	lda ipjtab,y		; jump address(lo)
	sta ipb+ipcjmp
	iny
	lda ipjtab,y		; jump address (hi)
	sta ipb+ipcjmp+1
	jsr ackhi		; sem6509 -> hi (cmd recvd)
	jsr waitlo		; sem8088 -> lo (ack)

;    receive input bytes, if any
	ldy #0

ips100:	cpy ipb+ipcin		; any more?
	beq ips200		; no...
	jsr acklo		; sem6509 ->lo (ack ack)
	jsr waithi		; sem8088 -> hi (data available)
	lda ipcia+pra		; read data byte
	sta ipb+ipcdat,y	; store it
	jsr ackhi		; sem6509 -> hi (data recvd)
	jsr waitlo		; sem8088 -> lo (ack)
	iny
	bne ips100		; always...

;    process cmd
ips200:	bit ipb+ipccmd		; cmd requires bus?
	bmi ips500		; yes...
	lda #>ipsret		; push return
	pha
	lda #<ipsret
	pha
	jmp (ipb+ipcjmp)	; gone!!!

;    send return bytes, if any
ips300:
ipsret=ips300-1
	jsr acklo		; sem6509 -> lo
	ldy #0
	beq ips350		; always
ips310:
	jsr waithi		; sem8088 -> hi (8088 rdy to recv)
	lda #$FF
	sta ipcia+ddra		; port direction = out
	lda ipb+ipcdat,y
	sta ipcia+pra		; write data to port
	jsr ackhi		; sem6509 -> hi (data available)
	jsr waitlo		; sem8088 -> lo (data recvd)
	lda #0
	sta ipcia+ddra		; port direction = in
	jsr acklo		; sem6509 -> lo (ack)
	iny
ips350:	cpy ipb+ipcout		; any more?
	bne ips310		; yes, repeat...

ips400:	;ldy #0
	;ips450 pla		; restore ip buffer
	; sta ipb,y
	; iny
	; cpy #ipbsiz
	; bne ips450
	rts			; done!

;      special,   for commands requiring the bus
ips500:	lda #>buret
	pha
	lda #<buret
	pha			; push return
	jsr getbus		; grab bus
	jmp (ipb+ipcjmp)	; gone!

ips600:
buret=ips600-1
	jsr frebus		; give up bus
	lda ipb+ipcout		; #bytes to return
	sta ipb+ipcin
	sta ipb+ipccmd		; return op=#bytes to return
	lda #0
	sta ipb+ipcout		; just send to 8088
	jsr iprqst
	jmp ips400		; done!

; waitlo - wait until sem88 goes low
waitlo:	lda ipcia+prb
	and #sem88
	bne waitlo
	rts
; waithi - wait until sem88 goes high

waithi:	lda ipcia+prb
	and #sem88
	beq waithi
	rts

; acklo - acknowlegde sem65 low
acklo:	lda ipcia+prb
	and #$FF-sem65
	sta ipcia+prb
	rts

; ackhi - acknowledge sem6509 hi
ackhi:	lda #sem65
	ora ipcia+prb
	sta ipcia+prb
	rts

; frebus - give up bus
frebus:	lda tpi1+pb		; pb4 := 0
	and #$EF
	sta tpi1+pb
	rts

; getbus - grab bus
getbus:
	lda ipcia+prb		; check nbusy2
	and #$02
	beq getbus		; 2nd proc not off

	lda tpi1+pb		; pb4 := 1
	ora #$10
	sta tpi1+pb
	rts

; getpar
;   enter - .y = table offset
;   exit:   .y = table offset
;          #ins,#outs put into ipb buffer
getpar:	lda ipptab,y		; break apart nibbles
	pha
	and #$0F
	sta ipb+ipcin		; #input bytes
	pla
	lsr
	lsr
	lsr
	lsr
	sta ipb+ipcout		; #output bytes
	rts

; ipcgo - free bus, interrupt 2nd processor
;         go into a loop, waiting for requests.
;  * returns if bus error occurs
ipcgo:	ldx #$FF
	stx i6509		; indirects to bank f only
	lda tpi1+pb		; tpi1 pb4:=0 frees dbus
	and #$EF
	sta tpi1+pb
	nop			; a pause
	lda ipcia+prb		; check nbusy1
	ror
	bcs ipcgx
	rts			; bus not free!, error...

ipcgx:	lda #0			; pb6 lo->hi in 4us...
	sei
	sta ipcia+prb		; interrupt 2nd processeor
	lda #$40		; 2 cycles (4us=8cycles)
	nop
	nop
	nop
	nop			; 8 cycles of garb. 5us safer than 4!
	sta ipcia+prb		; turn pb6 back on
	cli
iploop:	jmp iploop		; sit down
; -------------------------------------------------------------------------------------------------
; FE5A no cassette routines avaliable
xtape:	jmp     (itape)		; goto tape device indirect -> nocass
;
nocass:	pla			; remove jsr xtape and return
	pla
	jmp error5		; send back ?device not present
; -------------------------------------------------------------------------------------------------
; some needed routines
; FE62 
rd300:	lda stah
	sta sah
	lda stal
	sta sal
	lda stas
	sta sas
	sta i6509
	rts
; FE71 
cmpste: sec
	lda sal
	sbc eal
	lda sah
	sbc eah
	lda sas
	sbc eas
	rts
incsal: inc sal
	bne incr20
	inc sah
	bne incr20
	inc sas
	lda sas
	sta i6509
	lda #$02		; skip $0000 and $0001
	sta sal
incr20:	rts
; -------------------------------------------------------------------------------------------------
;-------------------------------------
; tapery - get from the tape buffer
;   lda (tape1)y ;replacement
;-------------------------------------
;tapiry iny
;tapery ldx i6509
; lda tape1+2
; sta i6509
; lda (tape1)y
; stx i6509
; rts
;-------------------------------------
; tapewy - put char in the tape buffer
;   sta (tape1)y ;replacement
;-------------------------------------
;tapzwy ldy #$ff ;first byte in buffer
;tapiwy iny ;auto inc into buffer
;tapewy ldx i6509
; pha
; lda tape1+2
; sta i6509
; pla
; sta (tape1)y
; stx i6509
; rts
;-------------------------------------
; fnadry - get from file name buffer
;   lda (fnadr)y ;replacement
;-------------------------------------
; FE92 Load a byte from the file name
fnadry: ldx i6509
	lda fnadr+2
	sta i6509
	lda (fnadr),y
	stx i6509
	rts
; -------------------------------------------------------------------------------------------------
; ##### transx #####
; FE9D txjmp - transfer-of-execution jumper
;   entry - .a=seg # .x=low .y=high
;   caller must be a jsr txjmp
;   all registers and i6509 destroyed
;   returns directly to caller...
txjmp:	sta i6509		; bp routine
	txa
	clc
	adc #2			; add 2 to target address
	bcc txjmp1
	iny
txjmp1:	tax
	tya
	pha			; store target+2 to stack
	txa
	pha
	jsr ipinit		; go initilize ipoint
	lda #$fe
	sta (ipoint),y		; $fe to top of foreign stack
; 04/14/83 bp
; transfer exec routines for cbm2
; -------------------------------------------------------------------------------------------------
; FEB2 Support routine for cross bank calls
exsub:	php			; save status
	sei			; disable interrupts
	pha			; .a
	txa
	pha			; .x
	tya
	pha			; .y
	jsr ipinit		; init ipoint and load stack from xfer seg
	tay			; .y is xfer seg stack pointer
	lda e6509		; push return segment to user stack
	jsr putas		; push .a to other stack
	lda #<excrt2		; xfer seg rts routn
	ldx #>excrt2		; xfer seg rts routn
	jsr putaxs		; put .a.x to xfer seg stack
	tsx
	lda stack+5,x		; .sp +5 is actual routn addr lo
	sec
	sbc #03			; -3 for jsr to this routn
	pha			; save .a
	lda stack+6,x		; hi addr
	sbc #00	
	tax			; .x hi
	pla			; restore .a lo
	jsr putaxs		; save .a.x onto xfer seg stack
	tya			; xfer seg stack pointer
excomm:	sec
	sbc #04			; 4 bytes .y.x.a.p
	sta stackp		; xfer seg new stack pointer temp storage
	tay			; use this as new pointer also
	ldx #04			; 4 bytes .y.x.a.p
exsu10:	pla
	iny
	sta (ipoint),y		; push regs from this stack to xfer seg stack
	dex
	bne exsu10
	ldy stackp		; restore .y as stack pointer for xfer seg
	lda #<expul2		; pull regs and rts routn
	ldx #>expul2		; .hi prendn routn in xfer seg
	jsr putaxs		; put .a.x on xfer seg stack
	pla			; fix stack
	pla			; fix stack
exgby:	tsx
	stx stackp		; save current stack pointer this seg
	tya			; .y is stack pointer for xfer seg
	tax
	txs			; new stack for xfer seg
	lda i6509		; xfer seg #
	jmp gbye		; good bye
; -------------------------------------------------------------------------------------------------
	nop			; returns here if rti
; FF05 Return from call to foreign bank
excrts: php			; .p
	php			; .p
	sei             	; dis ints
	pha			; .a
	txa
	pha			; .x
	tya
	pha			; .y
	tsx
	lda stack+6,x		; sp +6 is return seg
	sta i6509		; restore i6509 to return seg
	jsr ipinit		; init ipoint and load stack from xfer seg
	jmp excomm
; -------------------------------------------------------------------------------------------------
; FF19 ipoint = $100, Y = $FF (stack)
ipinit: ldy #01
	sty ipoint+1
	dey
	sty ipoint		; ipoint=$0100
	dey			; .y=$ff
	lda (ipoint),y		; load stack pointer from $001ff
	rts
; -------------------------------------------------------------------------------------------------
; FF24 Place X/A to ipoint (build stack in foreign bank)
putaxs: pha			; save A
	txa
	sta (ipoint),y		; .x hi
	dey
	pla
; FF2A Place A to ipoint (build stack in foreign bank)
putas:  sta (ipoint),y		; .a lo
	dey
	rts
; -------------------------------------------------------------------------------------------------
; FF2E Pull registers after calling subroutine in foreign bank
expull: pla
	tay			; .y
	pla
	tax			; .x
	pla			; .a
	plp			; .p
	rts
; -------------------------------------------------------------------------------------------------
; FF35 Helper routine to route interrupts from foreign to system bank
exnmi:  php			; .p
	jmp (hwnmi)		; do nmi proc
; -------------------------------------------------------------------------------------------------
; FF39 Helper routine to route BRK insns from foreign to system bank
exbrk:  brk
	nop
	rts
; -------------------------------------------------------------------------------------------------
; FF3C Helper routine to route interrupts from foreign to system bank
exirq:  cli
	rts
exend:
;
excrt2=excrts-1
expul2=expull-1
; -------------------------------------------------------------------------------------------------
; ##### vectors #####
; FF6F (FF6C) Jump table kernal functions
*= $FF6C
newsys:	jmp txjmp		; Transfer-of-execution jumper
	jmp vreset		; Power-on/off vector reset
ipcgov:	jmp ipcgo		; Loop for ipc system
	jmp jfunky		; Function key vector
	jmp iprqst		; Send ipc request
	jmp ioinit		; I/O initialization
	jmp jcint		; Screen initialization
aloca:	jmp alocat		; Allocation routine
	jmp vector		; read/set I/O vectors
	jmp restor		; restore I/O vectors
	jmp lkupsa		; Match sa--return sa,fa
	jmp lkupla		; Match la--return sa,fa
	jmp setmsg		; Control o.s. messages
secnd:	jmp (isecnd)		; Send sa after listen
tksa:	jmp (itksa)		; Send sa after talk
	jmp memtop		; set/read top of memory
	jmp membot		; set/read bottom of memory
	jmp jkey		; Scan keyboard
	jmp settmo		; set timeout in IEEE
acptr:	jmp (iacptr)		; Handshake IEEE byte in
ciout:	jmp (iciout)		; Handshake IEEE byte out
untlk:	jmp (iuntlk)		; Send untalk out IEEE
unlsn:	jmp (iunlsn)		; Send unlisten out IEEE
listn:	jmp (ilistn)		; Send listen out IEEE
talk:	jmp (italk)		; Send talk out IEEE
	jmp readst		; read/write I/O status byte
	jmp setlfs		; set la, fa, sa
	jmp setnam		; set length and fn adr
open:	jmp (iopen)		; Open logical file/transmit command
close:	jmp (iclose)		; Close logical file
chkin:	jmp (ichkin)		; Open channel in
ckout:	jmp (ickout)		; Open channel out
clrch:	jmp (iclrch)		; Close I/O channel
basin:	jmp (ibasin)		; Input from channel
bsout:	jmp (ibsout)		; Output to channel
load:	jmp (iload)		; Load from file
save:	jmp (isave)		; Save to file
	jmp settim		; Set internal clock
	jmp rdtim		; read internal clock
stop: 	jmp (istop)		; scan stop key
getin:	jmp (igetin)		; Get char from q
clall:	jmp (iclall)		; Close all files
	jmp udtim		; increment clock
	jmp jscror		; Screen org
	jmp jplot		; read/set x,y coord
	jmp jiobas		; return I/O base
; -------------------------------------------------------------------------------------------------
; FFF6 Actual execution segment switch routine
gbye:	sta e6509		; goodbye...
	rts
	!byte $01
*= $FFFA
; -------------------------------------------------------------------------------------------------
; FFFA Hardware vectors
hwnmi:  !word nmi		; FB3D Program defineable
	!word start		; F99E Initialization code
	!word nirq		; FBE5 Interrupt handler