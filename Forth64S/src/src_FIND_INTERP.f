
: DO-VOC R> 1 ANDC
 CONTEXT ! ;

: ?PAIRS        \ x1 x1 --
  XOR -22 ?THROW ;

T: DEFINITIONS  ( --- )
\ Set the definitions wordlist to the last wordlist in the search order.
  CONTEXT @ CURRENT ! ;

T: ALIGN HERE ALIGNED DP! ;
T: HALIGN HERE HALIGNED DP! ;

0 VALUE SOURCE-ID

ATIB VALUE TIB

: SOURCE TIB #TIB @ ;

: SOURCE! ( c-addr u -- ) 
  #TIB ! TO TIB ;

: EndOfChunk ( -- flag )
 >IN @ SOURCE NIP < 0=        \ >IN 
;
\ T\EOF

: IsDelimiter ( char -- flag )
 BL 1+ < ;

: CharAddr ( -- c-addr )
  SOURCE DROP >IN @ +
;

: PeekChar ( -- char )
  CharAddr C@
;

: GetChar ( -- char flag )
 EndOfChunk
 IF 0 FALSE
	ELSE PeekChar TRUE THEN ;

: OnDelimiter ( -- flag )
	GetChar SWAP IsDelimiter AND ;

: SkipDelimiters ( -- ) \ 
	BEGIN	OnDelimiter
	WHILE	>IN 1+!
	REPEAT >IN @   >IN_WORD ! ;

: OnNotDelimiter ( -- flag )
  GetChar SWAP IsDelimiter 0= AND ;

: SkipWord ( -- ) \ 
 BEGIN	OnNotDelimiter
 WHILE	>IN 1+!
 REPEAT ;


: ParseWord ( -- c-addr u )
 CharAddr >IN @  SkipWord
 >IN @ - NEGATE ;

: PSKIP ( char "ccc<char>" -- )
\ 
  BEGIN
    DUP GetChar >R = R> AND
  WHILE
    >IN 1+!
  REPEAT DROP
;

: PARSE-NAME ( -- c-addr u )
  SkipDelimiters
  ParseWord
  >IN @ 1+ #TIB @ MIN >IN !   \ 
;

: SkipUpTo ( char -- ) \ 
  BEGIN
    DUP GetChar >R <> R> AND
  WHILE
    >IN 1+!
  REPEAT DROP
;

: PARSE ( char "ccc<char>" -- c-addr u ) \ 94 CORE EXT
  CharAddr >IN @
  ROT SkipUpTo
  >IN @ - NEGATE
  >IN 1+!
;


: SEARCH-NFA1 ( c-addr u wid --- 0 | nfa )
	@
	BEGIN   DUP \  CR ." S=" DUP H.  DUP 8 TYPE
	WHILE
	>R 2DUP	R@ COUNT COMPARE 0= 
		IF 2DROP R>
		BREAK
 	R> CDR
	REPEAT
	2DROP DROP 0 \ Not found.
;

' SEARCH-NFA1 ->DEFER SEARCH-NFA

: SEARCH-WORDLIST1 ( c-addr u wid --- 0 | xt 1 xt -1)
\ Search the wordlist with address wid for the name c-addr u.
\ Return 0 if not found, the execution token xt and -1 for non-immediate
\ words and xt and 1 for immediate words.
        SEARCH-NFA DUP
	IF	DUP NAME> SWAP
		NAME>F @ 1 AND 1- 1 OR
	THEN

;

' SEARCH-WORDLIST1 ->DEFER SEARCH-WORDLIST

: SFIND ( addr len --- addr len 0| xt 1|xt -1 )
\ Search all word lists in the search order for the name in the
\ counted string at c-addr. If not found return the name address and 0.
\ If found return the execution token xt and -1 if the word is non-immediate
\ and 1 if the word is immediate.
\ ." SFIND=<"
  CONTEXT
  BEGIN	DUP @ \ DUP H.
  WHILE	>R
	2DUP  R@  @  SEARCH-WORDLIST ?DUP
	IF    RDROP 2NIP EXIT \ Exit if found.
	THEN
	R> CELL+
  REPEAT @
;

: ?STACK ( -> ) \ 
 SP@ SP0 @ SWAP U< IF SP0 @ SP! -4 THROW THEN
\  SP@ HERE - 0xE00 U< IF ." STACK: MAM" ABORT THEN
;

' NOOP ->DEFER MEM_TST

: INTERPRET_ ( -> ) \ 
\ CR ." INTERPRET=" SOURCE TYPE
  SAVEERR? ON
  BEGIN	PARSE-NAME \  ." <" 2DUP  TYPE ." >"
  DUP
  WHILE

 SFIND ?DUP
	IF     STATE @ =
		IF COMPILE,
		ELSE EXECUTE \ LLL @ IF ." {L}" 0 LLL ! THEN
		THEN
	ELSE 	?SLITERAL
        THEN

\    ?STACK  MEM_TST \  CACHBREAK
  REPEAT 2DROP
;


\ - &INTERPRET_ VARIABLE  &INTERPRET_

\ CREATE IIII ' INTERPRET_ ,
\ CREATE &INTERPRET ' INTERPRET_ ,
\ : INTERPRET &INTERPRET PERFORM ;
' INTERPRET_ ->DEFER INTERPRET

: .ID ( NFA -- )   COUNT TYPE ;


VARIABLE _fp1
VARIABLE _fp2
VARIABLE _addr 4 ALLOT
VARIABLE _dos
CREATE LT  $0A0A0A0A ,

: READ-LINE ( c-addr u1 fileid -- u2 flag ior ) \ 94 FILE
  OVER 0= IF DROP NIP TRUE 0 BREAK
  _dos 0!
  DUP >R
  FILE-POSITION IF 2DROP 0 0 THEN _fp1 ! _fp2 !
\  1+ \ LTL @ +
  OVER _addr !
  R@ READ-FILE   ?DUP  IF NIP RDROP 0 0 ROT EXIT THEN
  DUP >R 0= IF RDROP RDROP 0 0 0 EXIT THEN \ были в конце файла
  _addr @ R@

 LT 1 SEARCH
  IF   \ найден разделитель строк
     OVER 1- C@ 13 = _dos !
     DROP _addr @ -     DUP   
     1+ U>D _fp2 @ _fp1 @ D+
      RDROP R> REPOSITION-FILE DROP
  ELSE \ не найден разделитель строк
     2DROP
     R>  RDROP  \ если строка прочитана не полностью - будет разрезана
  THEN
  _dos @ + 0 MAX
  TRUE 0
;

: ASCII-Z     ( addr len buff -- buff-z )        \ make an ascii string
   DUP >R $! R> COUNT OVER + 0 SWAP C! ;

\ EBPCLROFF

: Z\TO/  ( Z-addr --  Z-addr  )
  DUP
  BEGIN COUNT DUP
  WHILE  [CHAR] \ = IF [CHAR] / OVER 1- C! THEN
  REPEAT 2DROP

;

CREATE FILE-BUFF  $101 ALLOT
CREATE FILE-BUFF2 $101 ALLOT

: OPEN-FILE ( c-addr u fam -- fileid ior )
  >R  FILE-BUFF  ASCII-Z Z\TO/
  R>  ZOPEN-FILE
;

: DELETE-FILE ( c-addr u -- flg )
   FILE-BUFF  ASCII-Z Z\TO/
    ZDELETE-FILE
;

: RENAME-FILE ( c-addr1 u1 c-addr2 u2 -- ior ) 
   FILE-BUFF  ASCII-Z Z\TO/ -ROT
   FILE-BUFF2  ASCII-Z Z\TO/ 
   ZRENAME-FILE

;

: >CREATE  DUP O_CREATGet OR ;

T: CREATE-FILE ( c-addr u fam -- fileid ior ) \ 94 FILE
  >CREATE \
 OPEN-FILE
 ;

VARIABLE ID_SHIFT
: ID_SHIFT@ ID_SHIFT @ ;

\ : JKEY#  #JKEY >CPF WPF> ;
\ : GKEY#  JKEY#  DUP $200 AND ID_SHIFT ! $1FF AND ;
\ : GKEY?#  #GKEY? >CPF PF> ;

0 VALUE UTT?

: KKEY  ( -- c )
 DUP 
 RP@ $F AND IF <'> NOOP >R THEN  \ for mmx
  C_KEY
  ;

' KKEY ->DEFER KEY

: KKEY?  ( -- c )
 DUP 
 RP@ $F AND IF <'> NOOP >R THEN  \ for mmx
  C_KEYQUERY
 

 ;

' KKEY? ->DEFER KEY?

: CR_C	$D EMIT $A EMIT ;


: ACCEPT1 ( C-ADDR +N -- +N' ) \ GET LINE FROM TERM'L
   OVER + 1- OVER      \ SA EA A
   BEGIN
 KEY       \ SA EA A C
   DUP $D <>
  OVER $A <> AND
  WHILE
       DUP 27 = IF  DROP DUP C@ EMIT  ELSE
       DUP EMIT
       DUP   8  = IF BL EMIT EMIT
                     2- >R OVER 1- R> UMAX ELSE
       DUP   $7F  = IF BL EMIT EMIT
                     2- >R OVER 1- R> UMAX ELSE

       DUP 9  = IF  DROP DUP 8 SPACE
                    >R OVER R>    \ SA EA SA A
                    TUCK  -   \ SA EA SA A-SA
                    8 / 1+ 8 * +  ELSE
	OVER  C! THEN THEN THEN   THEN
	1+ OVER UMIN  \ SA EA A
   REPEAT                          \ SA EA A C

   DROP NIP SWAP -

\ GETXY 2>R  0 2 SETXY .S
\ 2R> SETXY   

 CR ;


' C_ACCEPT ->DEFER ACCEPT


: QUERY	( -- )	\ ACCEPT A LINE OF INPUT FROM THE USER TO TIB
	TIB 180 ACCEPT #TIB !
	0 >IN !  0 SOURCE + C! ;


: OK..  ." OK" CR ;
' OK.. ->DEFER OK.

VARIABLE CURSTR

' NOOP ->DEFER <PRE>

: FREFILL ( h -- flag )
  TIB TC/L ROT READ-LINE THROW
  SWAP
  #TIB !  0 >IN ! CURSTR 1+!
  0 SOURCE + C!
 DUP IF <PRE> THEN
;

: REFILL ( -- flag ) \ 94 FILE EXT
  SOURCE-ID  0 > IF ( included text )
  SOURCE-ID
 FREFILL
  EXIT  THEN
  QUERY TRUE
;


: EVALUATE ( i*x c-addr u -- j*x ) \ 94
  SOURCE-ID >R SOURCE 2>R >IN @ >R
  -1 TO SOURCE-ID
  SOURCE! >IN 0!
    ['] INTERPRET CATCH
  R> >IN ! 2R> SOURCE! R> TO SOURCE-ID
  THROW
;

: FQUIT
 BEGIN  REFILL

 WHILE  INTERPRET
 REPEAT ;

: QUIT
 BEGIN  REFILL
 WHILE  INTERPRET OK.
 REPEAT ;

T: ] STATE ON ;
T: [ STATE OFF ; IMMEDIATE

 VARIABLE BYE_FLG

\ DP><DDP
 CREATE ERTP_DIR	$101 ALLOT
 CREATE CUTP_DIR	$101 ALLOT
\ DP><DDP

: SAVEERR
	DUP SAVEERR? @ AND
	IF	SOURCE ERRTIB $! >IN_WORD @ ER>IN !
		SOURCE-ID ERR&S-ID ! CURSTR @ ERR-LINE ! SAVEERR? OFF
\+ CUTP_DIR		CUTP_DIR COUNT ERTP_DIR $!
	THEN
;

VARIABLE CCOUT&OUTPUT
VARIABLE CCOUTV_EMIT
VARIABLE CCOUTV_TYPE

0 VALUE CCOUT_#

: ERROR_DO1

	DUP 0= IF DROP BREAK
	SAVEERR
	CR ERRTIB COUNT TYPE CR
\ ER>IN @ BEGIN SPACE 1- DUP 0 MAX 0= UNTIL
	ERRTIB 1+ ER>IN @ $3F AND 0
\ . . BYE1
   ?DO COUNT 9 = IF 9 EMIT ELSE SPACE THEN  LOOP DROP
 ." ^" \ DROP
	CR ." ERR=" . 
	CR  SP0 @ SP!   STATE 0!
;

' ERROR_DO1 ->DEFER ERROR_DO

: MAIN_CYCLE
 BEGIN	[COMPILE] [
	&?MINUS 0!
	['] QUIT CATCH
\	BYE_FLG @ IF BREAK
	ERROR_DO
 AGAIN
;

\ Copmiler

T: ' PARSE-NAME SFIND 0= IF -13 THROW THEN  ; \ ???????

\ eof

T: ['] '  LIT, ; IMMEDIATE

: CHAR ( "<spaces>name" -- char ) \ 94
 PARSE-NAME DROP C@ ;


: >S, ( addr u -- addr addr1 u )
  DP @ SWAP DUP ALLOT ;

T: S, ( addr u -- )
  >S, CMOVE ;

: >S", ( addr u -- addr addr1 u ) 
 DUP C, >S,   ;

T: S", ( addr u -- ) 
  >S", CMOVE ;



T: SLIT, ( adr len -- [ adr len ] )
	['] (S") _COMPILE, S",
\	HERE 1 AND IF 0 C, THEN
\	HERE 3 AND IF 0 W, THEN
;

T: S"	[CHAR] " PARSE
	STATE @ IF SLIT, THEN ; IMMEDIATE


T: Z"  ( "ccc<quote>" --- )
\ Parse a string delimited by " and compile the following runtime semantics.
\ Runtime: ( --- c-addr u) Return start address and length of that string.
  [CHAR] " PARSE
   ['] (Z")  _COMPILE,
 1+ S",
 -1 ALLOT 0 C,
\	HERE 1 AND IF 0xFF C, THEN
\	HERE 3 AND IF 0xFFFF W,	THEN
 ; IMMEDIATE



: (ABORT'') ROT
 IF TYPE -2 THROW THEN
 2DROP
;


T: ABORT"
	[COMPILE] S" ['] (ABORT'') COMPILE, ; IMMEDIATE

: CRASH         ( -- ) 
\                2R@ 2- @L >NAME CR .ID
 TRUE                ABORT" <- is an Uninitialized execution vector." 
 ;

' CRASH TO 'CRASH



: ?COMP ( -> )
  STATE @ 0= IF -14 THROW THEN
;

: WORD ( char "<chars>ccc<char>" -- c-addr ) \ 94
\  DUP PSKIP PARSE 255 MIN
\  DUP PAD C! PAD 1+ SWAP CMOVE
\  0 PAD COUNT + C!
  PARSE PAD $!  PAD ;

: CLIT,
	['] (C") COMPILE, COUNT S",
\	HERE 1 AND IF 0xFF C, THEN
\	HERE 3 AND IF 0xFFFF W, THEN
;

T: CLITERAL ( addr -- ) 
  STATE @ IF CLIT, THEN ; IMMEDIATE


T: C"
  [CHAR] " WORD [COMPILE] CLITERAL
; IMMEDIATE

T: EXIT, $C3 C, ;

THS> .(  0)  MCR .( Wortbirne "EXIT" EXITcom 0) 1 TO TLASTFLG

: BRANCH, ( A -- ) \ ZZZZ IF  THEN
   0xE9 C,
  DUP IF THERE 4 + - THEN L,  \  DP @ TO LAST-HERE
;


T: ?BRANCH, ( A -- )
 0x84 TO J_COD
   OPT_INIT SetOP $48 C, $C085 W,  OPT \ test   %rax,%rax
   OPT_CLOSE	['] DROP INLINE,
     0x0F C,  J_COD C, 
  DUP IF THERE 4 + - THEN L, \ DP @ TO LAST-HERE
 0 TO J_COD
;

: >ORESOLVE1 ( A, N -- )
    HERE
    OVER -
    SWAP 4 - L!
;

: >ORESOLVE ( A, N -- )
 DUP	IF_FLAG =	IF   DROP >ORESOLVE1 BREAK
	HEAD_FLAG <>	IF -2007 THROW THEN \ ABORT" Conditionals not paired"
		>ORESOLVE1
;

T: ." [COMPILE] S" ['] TYPE COMPILE, ; IMMEDIATE

: ALIGN-NOP ( n -- )
\ выровнять HERE на n и заполнить NOP
  HERE DUP ROT 2DUP
  MOD DUP IF - + ELSE 2DROP THEN
  OVER - DUP ALLOT 0x90 FILL
;

T: BEGIN ?COMP HERE BEGIN_FLAG ; IMMEDIATE
T: UNTIL ?COMP BEGIN_FLAG <> IF -22 THROW THEN  ?BRANCH, ; IMMEDIATE
T: AGAIN ?COMP BEGIN_FLAG <> IF -22 THROW THEN   BRANCH, ; IMMEDIATE
T: AHEAD ?COMP	THERE BRANCH,	THERE HEAD_FLAG ; IMMEDIATE
T: IF	?COMP	THERE ?BRANCH,	THERE  IF_FLAG ; IMMEDIATE
T: THEN ( A, N -- )
	?COMP   >ORESOLVE ; IMMEDIATE
T: ELSE   ( BO BI ADDR ? -- 0 0 ADDR1 ?1 )
  [COMPILE] AHEAD CS-SWAP [COMPILE] THEN ; IMMEDIATE

T: WHILE  [COMPILE] IF  CS-SWAP ; IMMEDIATE
T: REPEAT [COMPILE] AGAIN [COMPILE] THEN ; IMMEDIATE

 
T: DO            \ Run: n1|u1 n2|u2 -- ; R: -- loop-sys           6.1.1240
\ *G Begin a *\fo{DO ... LOOP} construct. Takes the end-value and
\ ** start-value from the stack.
  ['] (DO) _COMPILE, HERE 0 , HERE 3
; IMMEDIATE

T: ?DO           \ Run: n1|u1 n2|u2 -- ; R: -- | loop-sys ; 6.2.0620
\ *G Compile a *\fo{DO} which will only begin loop execution if
\ ** the loop parameters do not specify an interation count of 0.
  ['] (?DO) _COMPILE, HERE 0 , HERE 3
; IMMEDIATE


T: LOOP          \ Run: -- ; R: loop-sys1 -- | loop-sys2         6.1.1800
\ *G The closing statement of a *\fo{DO ... LOOP} construct.
\ ** Increments the index and terminates when the index crosses
\ ** the limit.
  3 ?PAIRS
 $49 C, $C6FF W, \ inc r14
 $49 C, $C7FF W, \ inc r15
  HERE 2+ -  DUP SHORT? \  SetOP SetJP
  IF
    0x71 C, C, \ jno short 
  ELSE
    4 - 0xF C, 0x81 C, L, \ jno near
  THEN  \ SetOP
  $5E41 W, \ pop r14
  $5F41 W, \ pop r15
  $59 C, \ pop rcx
  HERE SWAP !
 ; IMMEDIATE

T: +LOOP         \ Run: n -- ; R: loop-sys1 -- | loop-sys2       6.1.0140
\ *G As *\fo{LOOP} except that you specify the increment on the
\ ** stack. The action of *\fo{n +LOOP} is peculiar when n is
\ ** negative:
\ *C   -1 0 ?DO  i .  -1 +LOOP
\ *P prints *\fo{0 -1}, whereas:
\ *C   0 0 ?DO  i .  -1 +LOOP
\ *P prints nothing. This a result of the mathematical trick used
\ ** to detect the terminating condition. To prevent confusion
\ ** avoid using *\fo{n +LOOP} with negative *\i{n}.
  3 ?PAIRS
	$49 C, $c601 W,	\   	add    %rax,%r14
	$49 C, $c701 W,	\	add    %rax,%r15
	['] DROP INLINE,
  HERE 2+ -  DUP SHORT? \  SetOP SetJP
  IF
    0x71 C, C, \ jno short 
  ELSE
    4 - 0xF C, 0x81 C, L, \ jno near
  THEN  \ SetOP
  $5E41 W, \ pop r14
  $5F41 W, \ pop r15
  $59 C, \ pop rcx
  HERE SWAP !
 ; IMMEDIATE

T: I
  ['] DUP INLINE,
 OPT_INIT
 SetOP $4c C, $f089 W,  OPT	\	mov %r14,%rax
 OPT_CLOSE
 ; IMMEDIATE

: GET-CURRENT ( -- wid ) \ 94 SEARCH
  CURRENT @ ;

: SET-CURRENT ( wid -- ) \ 94 SEARCH
  CURRENT ! ;


0 VALUE YDP_FL
\ : YDP_FL  &YDP_FL @ ;
VARIABLE YDP
VARIABLE YDP0

: YDP><DP
  YDP @ DP @
  YDP ! DP ! ;

: ?YDP><DP
 YDP_FL \ IS-TEMP-WL 0= AND
 IF YDP><DP
 THEN ;

VARIABLE LAST-CFA
CREATE WARNING 1 ,

: SBUILD ( addr u -- )
  ?YDP><DP
	HERE 0 , DUP LAST-CFA !
	0 L,   ( flags )
	CURSTR @ L,
	-ROT WARNING @
	IF 2DUP GET-CURRENT SEARCH-WORDLIST
	   IF ( NOUNIQUE ) DROP 2DUP TYPE ."  isn't unique" CR THEN
	THEN
	 CURRENT @ @ ,
	HERE LAST !
	S", 
	ALIGN
	?YDP><DP
    HERE SWAP ! ( ......... cfa )
;

: SMUDGE LAST @ CURRENT @ ! ;

T: SHEADER  SBUILD SMUDGE ;

T: HEADER  PARSE-NAME SHEADER ;

T: BUILD  PARSE-NAME SBUILD ;

CREATE &DOCONSTANT	' DOCONSTANT ,
CREATE &DOVALUE		' DOVALUE ,
CREATE &DOCREATE	' DOCREATE ,
CREATE &DOVECT		' DOVECT ,

T: CONSTANT  ( n -- )	HEADER &DOCONSTANT @ COMPILE, , ;

T: VALUE ( n -- )	HEADER &DOVALUE	@ COMPILE, , ;

: CREATED  SHEADER
  THERE 3 - ALIGNED 3 + DP! THERE LAST @ NAME>C !
 &DOCREATE @ COMPILE, ;

T: CREATE	PARSE-NAME CREATED ;

T: VARIABLE	CREATE 0 , ;

T: ->DEFER ( cfa -- )	HEADER 	&DOVECT @ COMPILE, , ;

T: DEFER	( -- )  ['] CRASH  ->DEFER ;

T: #DEFINE  BUILD &DOCONSTANT @ COMPILE, INTERPRET , ; 

: DEFER@ ( xt1 i?? i?? xt2 )
\ xt2 is the execution token xt1 is set to execute. An ambiguous condition exists if xt1 is not
\ the execution token of a word defined by DEFER, or if xt1 has not been set to execute an
  >BODY @ ;
: DEFER! ( xt2 xt1 i?? i?? )
\ Set the word xt1 to execute xt2. An ambiguous condition exists if xt1 is not for a word
\ defined by DEFER.
  >BODY ! ;

T: TO '
 STATE @
 IF
  >BODY  ADDR, ['] !
   COMPILE,
 BREAK
 DEFER!  ; IMMEDIATE

: (DOES>)
 R>	HERE >R
	LAST @ NAME>  DP !
	COMPILE,
 R> DP !
;

T: DOES>
 ['] (DOES>)	COMPILE,
 [COMPILE] R>
; IMMEDIATE

T: \ SOURCE NIP >IN ! ; IMMEDIATE

T: ( [CHAR] ) PARSE 2DROP ; IMMEDIATE
T: .( [CHAR] ) PARSE TYPE ; IMMEDIATE

T: [CHAR] ?COMP CHAR LIT, ; IMMEDIATE

: [DEFINED] ( -- f ) \ "name"
  PARSE-NAME SFIND  IF DROP TRUE ELSE 2DROP FALSE THEN
; IMMEDIATE

: [UNDEFINED]  ( -- f )   [COMPILE] [DEFINED] 0= ; IMMEDIATE

T: \+ [COMPILE] [UNDEFINED]	IF [COMPILE] \ THEN ; IMMEDIATE
T: \- [COMPILE] [DEFINED]	IF [COMPILE] \ THEN ; IMMEDIATE

: FIND ( c-addr -- c-addr 0 | xt 1 | xt -1 ) \ 94 SEARCH
  DUP >R COUNT SFIND
  DUP 0= IF NIP NIP R> SWAP ELSE RDROP THEN
;

: GET-ORDER ( -- widn ... wid1 n ) \ 94 SEARCH
  SP@ >R 0 >R
  CONTEXT
  BEGIN DUP @ ?DUP
  WHILE >R CELL+
  REPEAT  DROP
  BEGIN R> DUP 0=
  UNTIL DROP
  R> SP@ - 8 / 1-
;

: BLANK BL FILL ;

: ID. ( NFA[E] -> )  COUNT TYPE ;

T: LATEST ( -> NFA )    CURRENT @ @ ;

\ : [COMPILE] ' COMPILE, ; IMMEDIATE

T: [COMPILE] ' COMPILE, ; IMMEDIATE

T: POSTPONE
\  ?COMP
\ CR ." P=" 
  PARSE-NAME \ 2DUP U. U.
  SFIND DUP
 0=	IF -13 THROW THEN
 1 = 	IF	COMPILE,
	ELSE	LIT, ['] COMPILE, COMPILE,
	THEN
; IMMEDIATE

T: BREAK  EXIT, [COMPILE] THEN ; IMMEDIATE

: UPPER ( A L -- )
         OVER + SWAP
         ?DO I C@ DUP [CHAR] Z U>
            IF  0xDF AND
            THEN  I C!
         LOOP ;

T: 1RP@  ['] RP@   COMPILE,
         ['] CELL+ COMPILE,  ; IMMEDIATE

T: 2RP@  [COMPILE] 1RP@
         ['] CELL+ COMPILE,  ; IMMEDIATE

T: 3RP@  [COMPILE] 2RP@
         ['] CELL+ COMPILE,  ; IMMEDIATE

T: 4RP@  [COMPILE] 3RP@
         ['] CELL+ COMPILE,  ; IMMEDIATE

T: VOC-NAME. ( wid -- )
  DUP CELL+ CELL+ @ DUP
 IF ID. DROP BREAK
   DROP ." <NONAME>:" U. 
;

: VOCS
        VOC-LIST
        BEGIN @ DUP WHILE
               DUP CELL-  VOC-NAME. SPACE
        REPEAT
        DROP
;

VARIABLE RP0

\ EOF

: WORDLIST ( --- wid)
  HERE	0 , 
  HERE  VOC-LIST @ , VOC-LIST !
	0 ,
\ Make a new wordlist and give its address.
;


T: VOCABULARY ( "<spaces>name" -- )
  WORDLIST
  CREATE DUP ,
   CELL+ \ link
   CELL+ LAST @ SWAP ! \ 
  DOES> @ CONTEXT ! ;

: VARY ( "<spaces>name" -- )
  CREATE 77 ,
  DOES> @  ;


T: ALSO ( --- )
\ Duplicate the last wordlist in the search order.
 CONTEXT CONTEXT  CELL+ CONTEXT_SIZE CELL-  CMOVE> ;

T: FORTH FORTH-WORDLIST CONTEXT ! ;

T: ONLY ( --- )
 FORTH CONTEXT CELL+ 0! ;


T: PREVIOUS ( --- )
\ Remove the last wordlist from search order.
 CONTEXT CELL+ CONTEXT CONTEXT_SIZE CMOVE  ;

: SET-ORDER
  DUP -1 = IF DROP ONLY BREAK
  DUP 0= IF CONTEXT ! BREAK
  0
  DO CONTEXT I CELLS +
     DUP CELL+ 0!  !
  LOOP
;

T: ORDER ( -- ) \ 94 SEARCH EXT
  GET-ORDER ." Context: "
  0 ?DO ( DUP .) VOC-NAME. SPACE LOOP CR
  ." Current: " GET-CURRENT VOC-NAME. CR
;


T: HEX HEX ;


: ALIAS  ( cfa -- ) 
 PARSE-NAME SHEADER
 LAST @  NAME>C !
;

T: : PARSE-NAME  SBUILD  ] ;

T: ; ?COMP  EXIT,
 SMUDGE
 [COMPILE] [ ; IMMEDIATE

1 [IF]
T: FIELD  ( offset size "new-name< >" -- offset+size )
      : OVER
        DUP IF   DUP  LIT,  ['] + COMPILE,
            THEN DROP
       [COMPILE] ;
       + ;  
[ELSE]
T: FIELD
  CREATE OVER , + 
  DOES> @ +
;
[THEN]


1 CONSTANT &IMMEDIATE

T: IMMEDIATE
  &IMMEDIATE LAST @ NAME>F LOR!
;

VARIABLE EMITVAR

