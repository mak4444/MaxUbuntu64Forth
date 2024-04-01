
REQUIRE DUPENDCASE ~mak/case.f

\- M_WL : M_WL CS-DUP POSTPONE WHILE ; IMMEDIATE

\- C>S : C>S ( c -- n )  $FF AND $80 XOR $80 - ;
\- L>S : L>S ( c -- n )  $FFFFFFFF AND $80000000 XOR $80000000 - ;

\- -CELL  -1 CELLS CONSTANT -CELL

\- ANDC : ANDC	INVERT AND ;

\-  XOR! : XOR!	DUP @ ROT XOR SWAP ! ;

\- 4* : 4* 2 << ;
\- 3+ : 3+ 2+ 1+ ;

\- M@ : M@ @ ;
\- THERE : THERE HERE ;
\- TBASE BASE CONSTANT TBASE
\- SHORT? : SHORT? ( n -- -128 < n < 127 )  0x80 + 0x100 U< ;
\- LONG? : LONG? ( n -- -2147483648<n<2147483647 )
\- LONG? $80000000 + $100000000 U< ;
\- HH. : HH. H. ;

0 VALUE J_COD  \ код для Jx, он же признак ?BRANCH

TBASE M@ HEX

0 VALUE MO_TST_VAL

   TRUE VALUE OPT?

: SET-OPT TRUE TO OPT? ;

: DIS-OPT FALSE TO OPT? ;

0x20 VALUE MM_SIZE

0 VALUE OFF-EBP

0 VALUE OFF-EAX

0 VALUE TOFF-PSP

0 VALUE LAST-HERE

$22 CELLS DUP CONSTANT OpBuffSize

CREATE OP0 2 CELLS + ALLOT
OP0
CELL+ DUP CONSTANT OP1
CELL+ DUP CONSTANT OP2
CELL+ DUP CONSTANT OP3
CELL+ DUP CONSTANT OP4
CELL+ DUP CONSTANT OP5
CELL+ DUP CONSTANT OP6
CELL+ DUP CONSTANT OP7
CELL+ DUP CONSTANT OP8
DROP

0 VALUE OP0@
0 VALUE OP1@
0 VALUE OP2@
0 VALUE OP3@
0 VALUE OP4@
0 VALUE OP5@

: OPLast OP0 OpBuffSize + CELL- ;

: ?OPlast  ( OPX -- OPX flag )
     DUP OPLast CELL- U> ;

: SetOP ( -- )
 THERE OP0 @ = IF -8FD THROW THEN \ -2301 THROW \ do not alow OP1 be the same as OP0
 OP0 OP1 OpBuffSize CELL- CMOVE>
 THERE OP0 !
;

: ToOP0 ( OPn -- )
     OP0 OpBuffSize CELL- CMOVE ;

\- :-SET 0 VALUE :-SET

: ?SET THERE
	DUP LAST-HERE XOR IF DUP TO :-SET THEN
	DUP    OP0 @ U< IF OP0 0! THEN
	DUP    OP1 @ U< IF OP1 0! THEN
	DROP
;

: ?^OP ( addr -- flg )
\
  DUP :-SET U< IF DROP FALSE BREAK
  DROP TRUE
;

M\ ' DROP ->DEFER DTST

 TRUE VALUE ?C-JMP

 0 VALUE OPXXX

: OP_SIZE ( OP - n )
  DUP CELL- @ SWAP @ - ;

: RIPADR? ( w -- flg )
	CASE

	DUP C7FF AND	05F6 XOR IF
	DUP D7FF AND	15F7 XOR IF
	DUP E7FF AND	25F7 XOR IF
	DUP F7FF AND	05FE XOR IF
	DUP C7FF AND	05FF XOR IF

	DUP		05c0 XOR IF \ rolb    %e_x,x(%rip)
	DUP		05c1 XOR IF \ roll    %e_x,x(%rip)
	DUP		05c6 XOR IF \ movb    %e_x,x(%rip)
	DUP		05c7 XOR IF \ movl    %e_x,x(%rip)

	DUP C7C4 AND	0500 XOR IF \ add..cmp    %e_x,x(%rip)
	DUP		0563 XOR IF \ movsxd    %e_x,x(%rip)
	C7FD AND
	DUP		0569 XOR IF \ imul    %e_x,x(%rip)
	C7F0 AND
	DUP		0580 XOR IF \ mov..    %e_x,x(%rip)
	DUP		0580 XOR IF \ mov..    %e_x,x(%rip)
	    		05D0 = \ rol..    %e_x,x(%rip)
		EXIT
  DUPENDCASE DROP TRUE ;

VARIABLE PREF_S 

: PREF+  PREF_S @ + ;

: >PREF  F0 AND 40 <> 1+ PREF_S ! ;

: OP_SHIFTS (  n OPX -- )
	OP0
	?DO
	 I @ C@ >PREF
	 I @ PREF+ W@ RIPADR? IF DUP NEGATE I @ PREF+ 2+ L+! THEN
	DUP I +!
	CELL +LOOP
;

: OPresize ( OPX shift+ -- )
\ изменение размера кода на величину n. OPX адрес в таблице ссылок на код.
\ Код сдвигается. Если n больше 0 - сдвиг в сторону старших адресов.
\ Если n меньше 0 - сдвиг затиранием начала кода
  DUP>R
 OVER @ C@ >PREF
 OVER @ PREF+ W@ RIPADR? IF 2DUP NEGATE SWAP @ PREF+ 2+ L+! THEN
  OVER OP_SHIFTS
  ALLOT
\  @ DUP  R> +
\  DUP   THERE - NEGATE MOVE
  @ DUP R>
  DUP 0<
  IF   -  SWAP
  ELSE +  
  THEN DUP THERE - NEGATE MOVE

;


: OPinsert ( OPX n -- )
  DUP>R
  2DUP OPresize DROP
  DUP
  DUP CELL + OVER OP0 -  OpBuffSize CELL- - NEGATE MOVE
  R> SWAP +!
;

: OPsize! ( OPX n -- )
\ установка нового размера кода. OPX - адрес в таблице ссылок на код.
\ код остается на месте. Если размер меньше прежнего, код урезается в конце
  OVER OP_SIZE -
  DUP>R
  OVER OP_SHIFTS
  ALLOT
  CELL- @ DUP R> - SWAP DUP THERE - NEGATE MOVE
;

: OPexcise ( OPX -- )
\ удаление кода вместе со ссылкой на него
	DUP OP0 = IF @ DP! OP1 ToOP0 BREAK
	DUP 0 OPsize!
	DUP>R CELL+ R@ OpBuffSize CELL- R> - OP0 + CMOVE
;

: EVEN-EAX OFF-EAX
   IF      M\ 1000 DTST
	SetOP 48 C, 
	OFF-EAX DUP SHORT?
     IF		J_COD IF c083 ELSE 408D THEN W, C,
     ELSE	J_COD IF 5 C, ELSE 808D W, THEN  L,
     THEN   \  LEA   EAX,  OFF-EBP [EAX]
     0 TO OFF-EAX
       M\ 1001 DTST
   THEN
;

: EVEN-EBP OFF-EBP
   IF SetOP
  48 C, 06D8D W, OFF-EBP C, \   LEA   ebp,  OFF-EBP [EBP]
      0 TO OFF-EBP
   THEN
;

: +>OFF-EBP ( C -- )
   C>S OFF-EBP + TO OFF-EBP ;

: ADD|OR|AND|XOR<>  ( l -- FLAG )
  FFFFFF AND
   CASE
	DUP	450348 XOR IF \ ADD
	DUP	450B48 XOR IF \  OR
	DUP	452348 XOR IF \ AND
	DUP	453348 XOR IF \ XOR
             DROP TRUE EXIT
  DUPENDCASE DROP FALSE ;

: -EVEN-EBP
     OP0 @ :-SET
 U< IF BREAK
     OP0 @ L@ FFFFFF AND 06D8D48 =  \  LEA   ebp,  OFF-EBP [EBP]
     IF  OP0 @ 3 + C@ +>OFF-EBP
         OP1 ToOP0
        -4 ALLOT EXIT
     THEN ;


: DUP2B? ( w -- w flg )
  DUP
   CASE
  DUP		1088	XOR IF \	mov    %dl,(%rax)
  DUP		1089	XOR IF \	mov    %edx,(%rax)
  DUP		1001	XOR IF \	add     %rdx,(%rax)
  DUP		C019	XOR IF \	sbb     %eax,%eax
  DUP		C063	XOR IF \	movslq %eax,%rax
  DUP		D031	XOR IF \	xor     %rdx,%rax
  DUP		D029	XOR IF \	sub     %rdx,%rax
  DUP		008B	XOR IF \	mov	(%rax),%eax
  DUP		00FF	XOR IF \	incl	(%rax)
  DUP		028B	XOR IF \	mov	(%rdx),%eax
  DUP		F9F7	XOR IF \	idiv   %ecx
  DUP		9948	XOR IF \	cqto
  DUP E4FC AND	C088	XOR IF \   mov    %e<abcd>x,%e<abcd>x
  DUP E7FC AND	E0D0	XOR IF \ s<a|h>r	$1,%rax
  F6FF AND
  DUP		C0FF	XOR IF \ <in|de>cq	%eax
  F0FE AND
  DUP		D0F6	XOR IF \ n<ot|eg>	%eax
 DROP  FALSE  EXIT
   DUPENDCASE DROP TRUE ;

: DUP3B?[EBP]  ( u -- u FLAG )
  DUP FFFF AND
   CASE
  DUP E7FC AND	4588	XOR IF \ mov    0x_(%rbp),%e<abcd>x | %e<abcd>x,0x_(%rbp)
  DUP FFC5 AND	4501	XOR IF \ add|or|adc|sbb|and|sub|xor|cmp  0x_(%rbp),%eax | %eax,0x_(%rbp)
  DUP		558B	XOR IF \ 0x00(%rbp),%edx
            DROP FALSE EXIT
  DUPENDCASE DROP TRUE ;

: DUP3B? ( u -- u flg )
  DUP 
  DUP FF AND 48 = IF 8 >> DUP2B? NIP BREAK  \ rex.w
   CASE
 FFFFFF AND
  
  DUP		00B60F	XOR IF \ movzbl  (%rax),%eax 
  DUP		00B70F	XOR IF \ movzwl  (%rax),%eax 
  DUP		C0BE0F	XOR IF \ movsbl %al,%eax
  DUP		C0BF0F	XOR IF \ movswl %ax,%eax
  DUP		C09F0F	XOR IF \ setg	%al
  DUP		C09C0F	XOR IF \ setl	%al
  DUP		108966	XOR IF \ mov     %dx,(%rax)
 FFFF AND
  DUP		E883	XOR IF \ sub	$_,%rax
  DUP		408B	XOR IF \ mov    0x_(%rax),%eax
  DUP		408D	XOR IF \ lea    -0x70(%rax),%eax
  DUP		508D	XOR IF \ lea 0x_(%rax),%rdx
  DUP		048D	XOR IF \ lea (%rax,%rax),%rax
C7FE AND
  DUP		C0C0	XOR IF \ rol..sar $X,%al
	DROP FALSE EXIT
  DUPENDCASE DROP TRUE ;

: DUP4B? ( u -- u flg )
  DUP FF AND 48 = IF DUP 8 >> DUP3B? NIP BREAK  \ rex.w
   CASE
  DUP	40B60F	XOR IF \ movzbl X(%rax),%eax
  DUP	40B70F	= EXIT \ movzwl	X(%rax),%eax             
  DUPENDCASE  TRUE ;


: DUP5B? ( u -- u flg )
  DUP
   CASE
 FF AND
  DUP	B8 XOR IF \ mov    $_,%eax
 CF AND
  DUP	05 XOR IF \ sub	$_,%eax
	DROP FALSE EXIT
  DUPENDCASE DROP TRUE ;

: DUP6B?  ( u -- u FLAG )
  DUP FF AND 48 = IF DUP 8 >> DUP5B? NIP BREAK  \ rex.w
  DUP FFFF AND
   CASE
  DUP		00C7	XOR IF \ movq	$_,(%rax)
            DROP FALSE EXIT
  DUPENDCASE DROP TRUE ;

: DUP4B?[EBP] ( u -- u flg )
  DUP 
  DUP FF AND 48 = IF 8 >> DUP3B?[EBP] NIP BREAK
   CASE
 FFFFFF AND
  DUP FFF0FF AND 45400F	XOR IF \ cmov_  0x0(%rbp),%rax
            DROP FALSE EXIT
  DUPENDCASE DROP TRUE ;

: DUP5B?[EBP] ( u -- u flg )
  DUP FF AND 48 = IF DUP 8 >> DUP4B?[EBP] NIP BREAK  \ 3b have meanings
  0 ;

: DUP6B?IP  ( u -- u FLAG )
  DUP
   CASE
  FFFF AND
  DUP		058D	XOR IF \ lea     X(%rip),%rax
  FFFD AND	\ direction independent
  DUP		0589	XOR IF \ mov     %rax,X(%rip)
  FFC0 AND	\
  DUP		0500	XOR IF \ add .. cmp   %rax|al,X(%rip)
            DROP FALSE EXIT
  DUPENDCASE DROP TRUE ;

: DUP7B? ( l -- l flg )
  DUP FF AND 48 = IF DUP 8 >> DUP6B? NIP BREAK  \ rex.w
 FALSE ;

: DUP7B?IP ( l -- l flg )
  DUP FF AND 48 = IF DUP 8 >> DUP6B?IP NIP BREAK  \ rex.w
 FALSE ;

: NEWRAX0  ( addr -- flg ) \ 0 set rax wichout rax using
 L@ DUP FF AND 48 = IF 8 >> THEN \ rax -> eax
   CASE
 FFFFFF AND
 DUP F0894C XOR IF \ mov     %r14,%rax
 FFFF AND
 DUP	458B XOR IF \	mov    X(%rbp),%rax
 DUP	058a XOR IF \	mov    X(%rip),%al
 DUP	058b XOR IF \	mov    X(%rip),%eax
 DUP	058d XOR IF \	lea    X(%rip),%eax
 DUP	D089 XOR IF \	mov     %rdx,%rax
 DUP	C0C7 XOR IF \	movq    $-0x2CBE,%rax
 FF AND
 DUP	B8 XOR IF \	 mov     $X,%eax
 DROP TRUE EXIT
 DUPENDCASE DROP FALSE ;

: NEWRAX0+  ( addr -- flg ) \ NEWRAX0 with a ban on deletion
  DUP C@ 58 XOR IF \ pop     %rax 
  NEWRAX0
  BREAK DROP FALSE ;

: AX_apply<> ( addr -- flg )  \ 0 is not AX application
 L@ DUP FF AND 48 = IF 8 >> THEN \ rax -> eax
   CASE
 FFFFFF AND
 DUP 75894C XOR IF \ mov     %r14,-0x10(%rbp)
 FFFF AND
 DUP d285 XOR IF \ test   %rdx,%rdx
 DUP 35FF XOR IF \ pushq   X (%rip)
 DUP 45C7 XOR IF \ mov<lq>    $0x4D,-0x08(%rbp)
 FF AND
 DUP	B1 XOR IF \ mov     $0x03,%cl
 DROP TRUE EXIT
 DUPENDCASE DROP FALSE ;

: ?ChRAX<> ( addr -- flg ) \ 0 is't chench rax
 DUP NEWRAX0+ 0= IF DROP TRUE BREAK
 DUP AX_apply<> 0= IF DROP FALSE BREAK
 L@ DUP F8 AND 48 = IF 8 >> THEN \ rax -> eax
	CASE
 FFFFFF AND
 DUP 243C83 XOR IF \ cmp    $X,(%rsp)
 DUP 583880 XOR IF \ cmpb   $X,(%rax)
 FFFF AND

 DUP 0601 XOR IF \ add    %eax,(%rsi) add    %rax,(%r14)
 DUP 0689 XOR IF \ mov    %eax,(%rsi) mov    %rax,(%r14)

 DUP FFF0 AND	0580 XOR IF \ mov     %eax,dcomasubSET+0x5 (%rip) ...
 DUP		05C7 XOR IF \ mov     $X,Y(%rip)
 DUP FFF0 AND	5580 XOR IF \ mov|lea     0x08(%rbp),%rdx ...
 F8 AND
 DUP	38 XOR IF \ cmp
 DROP TRUE EXIT
 DUPENDCASE DROP FALSE ;

: ?ChRDX<> ( addr -- flg ) \ 0 is't chench rax
 DUP NEWRAX0+ 0= IF DROP FALSE BREAK
DROP TRUE
;

: DX_apply<> ( addr -- flg )  \ 0 is not DX application
 L@ DUP F8 AND 48 = IF 8 >> THEN \ rax -> eax
   CASE
 FFFF AND
DUP	058d XOR IF	\ lea     X(%rip),%eax
DUP	408B XOR IF	\ mov	-0x08(%rax),%eax
DUP	458B XOR IF	\ mov	-0x08(%rbp),%eax
DUP	008B XOR IF	\ mov	 (%rax),%eax
DUP	FFC0 AND 4500 XOR IF	\ add .. cmp -0x08(%rbp),%eax
DUP	f089 XOR IF	\ mov    %esi,%eax mov    %r14,%rax
  FF AND
DUP	B8 XOR IF	\ mov     $0x0,%eax
DUP	58 XOR IF	\ pop     %rax

 DROP TRUE EXIT
 DUPENDCASE DROP FALSE ;

: -EBPCLR  ( FLAG OPX  -- FLAG' )
   DUP @  :-SET  U< IF DROP BREAK
  OFF-EBP CELL- TO OFF-EBP
  BEGIN
	DUP @  4 + THERE  U> IF CELL+ THEN
		?OPlast    IF DROP BREAK
	DUP @ C@ F0
		AND 70   = IF DROP BREAK \ Jx

     DUP @  \ FLAG OPX [OPX]
     DUP W@ 6D8D = IF 2DROP BREAK \ lea  ebp, X[rbp]
     DUP 1+ W@ 6D8D = IF 2DROP BREAK \ lea  rbp, X[rbp]
     DUP C@ FF   = IF 2DROP BREAK \ CALL [x]
     DUP C@ E8   = IF 2DROP BREAK \ CALL
     DUP C@ E9   = IF 2DROP BREAK \ JMP
     DUP C@ EB   = IF 2DROP BREAK
     DUP L@ F0FF
          AND 800F = IF 2DROP BREAK \ Jx
     DUP L@ FFFFFF AND C5448B = \ MOV  EAX , FC [EBP] [EAX*8]
              IF 2DROP BREAK

     DUP L@  C5448B48 = \ MOV  EAX , FC [EBP] [EAX*8]
              IF 2DROP BREAK

     DUP C@ >PREF
     DUP PREF+ C@ F = IF 1 PREF_S +! THEN

     DUP PREF+ 2+ C@ C>S OFF-EBP  =
     IF   DUP	PREF+ W@  E7FF AND  4589 =    \ MOV X [EBP] , EAX|EDX|EBX|ECX
          OVER	PREF+ W@            45C7 = OR \ MOV     F8 [EBP] , # 2710
          IF M\ 200 DTST
               OVER  OPexcise >R NIP TRUE SWAP CELL- R>
          ELSE  DUP	PREF+ W@ 408B XOR \ not (%rbp)
		IF  2DROP BREAK
          THEN
     THEN

   DROP  CELL+   DUP @  :-SET  U<
  UNTIL DROP ;

: X?EBP
 DROP EXIT
 C@ C>S OFF-EBP  +
  40 + -80 AND
  IF EVEN-EBP THEN ;

: 2?EBP OVER 2+  X?EBP ;
: 3?EBP OVER 3 + X?EBP ;
: 4?EBP OVER 4 + X?EBP ;

\- LOW_LIM 0 VALUE LOW_LIM
: -EVEN-EAX
   BEGIN
 OP0 @ DUP :-SET U< IF DROP BREAK TO OP0@
  OP0@ L@ FFFFFF AND 408D48 = \      lea     0x02(%rax),%rax # 41CE14 48 8D 40 2
   M_WL  M\ 2 DTST
       OP0@ 3 + C@ C>S OFF-EAX + TO OFF-EAX
       OP1 ToOP0
       -4 ALLOT M\ 3 DTST
   REPEAT

  OP0@ L@ FFFFFF AND 808D48 = \      lea     0x02(%rax),%rax # 41CE14 48 8D 40 2
   M_WL  M\ 22 DTST
       OP0@ 3 + L@ L>S OFF-EAX + TO OFF-EAX
       OP1 ToOP0
       -7 ALLOT M\ 23 DTST
   REPEAT

  OP0@ L@ F7FFFF AND C0FF48 = \      incq|decq    %rax
   M_WL  M\ 4 DTST
       OFF-EAX OP0@ L@ 80000 AND 0<> 1 OR + TO OFF-EAX
       OP1 ToOP0
       -3 ALLOT M\ 5 DTST
   REPEAT

  OP0@ L@ FFFFFF AND C08148 = \     add     $0x30,%rax
   M_WL  M\ E DTST
       OP0@ 3 + L@ L>S OFF-EAX + TO OFF-EAX
       OP1 ToOP0
       -7 ALLOT M\ F DTST
   REPEAT

  OP0@ W@ 0548 = \     add     $0x30,%rax
   WHILE  M\ E DTST
       OP0@ 2+ L@ L>S OFF-EAX + TO OFF-EAX
       OP1 ToOP0
       -6 ALLOT M\ F DTST
   REPEAT
;

: OPT-RULES  ( ADDR  -- ADDR )
   M\ -1 DTST
   BEGIN
   OP0 @ DUP :-SET U< IF DROP BREAK TO OP0@
  OP0@ L@ FFFFFF AND D08948 XOR		\ mov	%rdx,%rax
  OFF-EAX 0=
  OR 0=
  M_WL
    M\ A DTST
	OFF-EAX DUP SHORT?
     IF    428D OP0@ 1+ W! C,
     ELSE  828D OP0@ 1+ W! L,
     THEN   \  lea    0x1(%rdx),%rax
	0 TO OFF-EAX
       M\ B DTST
   REPEAT

  OP0@ L@ FFFFFF AND
 DUP 058d48 XOR	\ lea	X(%rip),%rax
 SWAP C0C748 <> AND	\ movq	$X,%rax
  OFF-EAX 0=
  OR 0=
  M_WL
    M\ 24 DTST
      OP0@ 3 + SL@  OFF-EAX + 
	DUP LONG?
	IF      OP0@ 3 + L!
	ELSE  OP0@ L@ FFFFFF AND C0C748 XOR IF BREAK \ $X,%rax omly
	   -6 ALLOT   $B8 C,  ,	\  movabs $X,%rax
	THEN
	 0 TO OFF-EAX

    M\ 25 DTST
   REPEAT

  OP0@ C@ B8 XOR
  OFF-EAX 0=
  OR 0=
  M_WL
    M\ 46 DTST
      OP0@ 1+ L@  OFF-EAX +
	DUP 0x100000000 U< 
	IF      OP0@ 1+ L!
	ELSE   -5 ALLOT   $B848 W,  ,	\  movabs $X,%rax
	THEN
	 0 TO OFF-EAX

    M\ 47 DTST
   REPEAT

  OP0@ @ 80000000FFFF AND 2548 = \ MO_TST_VAL AND  \ and $_,%rax
  M_WL
    M\ 54 DTST
	OP0 -1 OPresize
    M\ 53 DTST
   REPEAT

  OP1 @ DUP :-SET U< IF DROP BREAK TO OP1@

  OP1@ L@ FFFFFF AND
  DUP 058d48 XOR	\ lea	X (%rip),%rax
 SWAP 408d48 <> AND	\ lea     -0x08(%rax),%rax 
  OP0@ L@ FFFFFF AND 008b48 XOR	\ mov	(%rax),%rax
   OR  0=
  M_WL  M\ 6 DTST
       600  OP1@ XOR! \  mov	X (%rip),%rax
       OP1 ToOP0
       -3 ALLOT M\ 7 DTST
   REPEAT

OP1@ L@ FFFFFF AND 408D48 XOR \       lea     X(%rax),%rax
OP0@ L@ FFFEFF AND 00B60F XOR \         movz<bw>l  (%rax),%eax
   OR  0=
  M_WL  M\ 26 DTST
       OP0@ W@  OP1@ W! \  movz<bw>l X(%rax),%eax
       OP1 ToOP0
       -3 ALLOT M\ 27 DTST
   REPEAT

  OP1@ L@ FFFFFF AND D08948 XOR	\ mov	%rdx,%rax
  OP0@ L@ FFFFFF AND 00B60F XOR	\ movzbl	(%rax),%eax
  OR  0=
  M_WL  M\ 8 DTST
	02b60f  OP1@ L! \           	movzbl (%rdx),%eax
       OP1 ToOP0
       -3 ALLOT M\ 9 DTST
   REPEAT

OP1@ W@ 8B XOR		 \	mov     (%rax),%eax     # 42D551 8B 0
OP0@ L@ FFFFFF AND C06348 XOR \ movslq  %eax,%rax       # 42D553 48 63 C0
  OR  0=
  M_WL  M\ 12 DTST
	OP0 @ W@ OP1 @ L! \ movslq (%rax),%rax
       OP1 ToOP0
	-2 ALLOT
	M\ 13 DTST
   REPEAT

 J_COD  0x84 XOR
  OP1@ L@ FFFFFF AND
   DUP  F08148  XOR     \  xor	$_,%rax
  OVER  F08348  <> AND     \  xor	$_,%rax
  OVER  E08348  <> AND     \  and	$_,%rax
  OVER  F8C148  <> AND     \  and	$_,%rax
  OVER  C0 AND 0<> AND  \   add .. cmp
  SWAP  C0FF AND 0048 <> AND  OR \   add .. cmp
 OP0@ L@ FFFFFF AND c08548 XOR \      test    %rax,%rax
 OR  0= \ MO_TST_VAL AND
 M_WL  M\ 14 DTST
       OP1 ToOP0
	-3 ALLOT
	M\ 15 DTST
 REPEAT

OP1@ 2+ W@ OP0@ 2+ W@ XOR	\	regs & indexes
OP1@ L@ EFFDFF AND 458948 XOR OR \	mov 0x00(%rbp),%rax | %rax,0x00(%rbp)
OP0@ L@ EFFDFF AND 458948 XOR OR \	mov 0x00(%rbp),%rax | %rax,0x00(%rbp)
  0=
 M_WL  M\ 18 DTST
       OP1 ToOP0
	-4 ALLOT
	M\ 19 DTST
 REPEAT

OP1@ L@ FFFFFF AND f0894c XOR \	mov     %r14,%rax
OP0@ L@ FFFFFF AND 008b48 XOR \	mov     (%rax),%rax
 OR 0=
 M_WL  M\ 1A DTST
	068b49 OP1@ L! \ mov    (%r14),%rax
       OP1 ToOP0
	-3 ALLOT
	M\ 1B DTST
 REPEAT

OP1@ L@ FFFFFF AND d08948 XOR \ mov     %rdx,%rax
OP0@ L@ FFFFFF AND 008b48 XOR \ mov     (%rax),%rax
 OR 0=
 M_WL  M\ 1C DTST
	028b48 OP1@ L! \ mov    (%rdx),%rax
	OP1 ToOP0
	-3 ALLOT
	M\ 1D DTST
 REPEAT
J_COD F0 AND 0x80 XOR
OP1@ L@ 1e88348 XOR OR		\ sub     $0x01,%rax      # 80406522      */
OP0@ L@ FFFFFF AND c01948 XOR OR \ sbb     %rax,%rax       # 80406526
  0=
 M_WL  M\ 20 DTST
	C085 OP1@ 1+ W! \ test   %rax,%rax
	J_COD 1 XOR TO J_COD
	OP1 ToOP0
	-4 ALLOT
	M\ 21 DTST
 REPEAT

OP1@ W@ 9248 XOR	\ xchg    %rdx,%rax
OP0@ L@ FFFFFF AND
DUP	458948 XOR	\ mov     %rax,-0x08(%rbp)
OVER	108948 <> AND	\ mov     %rdx,(%rax)
SWAP	100148 <> AND	\ add     %rdx,(%rax)
 OR   0=
 M_WL  M\ 2E DTST
	OP0@ 2+ C@ 45 XOR
	IF	OP0@ W@ 020000 OR \ %rax,(%rdx)
		1 OP0 +!
	ELSE	OP0@ L@ 500000 OR \ %rax,(%rdx)
		2 OP0 +!
	THEN OP1@ L!	\ __	%rax,(%rdx)
        9248 OP0 @ W!	\ xchg    %rdx,%rax
	M\ 2F DTST
 REPEAT

OP1@ C@ B8 XOR \ movq    $0x108,%rax
OP0@ L@ F7FFFF AND d0f748 XOR \ negq|notq    %rax
 OR 0=
 M_WL  M\ 48 DTST
	OP1@ 1+ L@ OP0@ L@ 80000 AND IF NEGATE ELSE INVERT THEN
       OP1 ToOP0
	-8 ALLOT 48 C,
	DUP LONG?
	IF      c0c7 W, L,
	ELSE   $B8 C,  ,	\  movabs $X,%rax
	THEN
	M\ 49 DTST
 REPEAT
OP1@ C@ B8 XOR \ movq    $0x108,%rax
OP0@ W@ C188 XOR \ mov     %al,%cl
 OR 0=
 M_WL	M\ 5E DTST
  B1 OP0@ C! OP1@ 1+ C@ OP0@ 1+ C!   \ mov    $8,%cl
	M\ 5F DTST
 REPEAT


OP1@ L@ FFFFFF AND c0c748 XOR \ movq    $0x108,%rax
OP0@ L@ FFFFFF AND d8f748 XOR \ negq    %rax
 OR 0=
 M_WL  M\ 36 DTST
	OP1@ 3 + SL@  NEGATE OP1@ 3 + L!
       OP1 ToOP0
	-3 ALLOT
	M\ 37 DTST
 REPEAT

OP1@ W@ 9248 XOR		\ xchg    %rdx,%rax
OP0@ L@ FFFFFF AND d08948 XOR	\ mov     %rdx,%rax
 OR 0=
 M_WL  M\ 3A DTST
       OP2 ToOP0
	-5 ALLOT
	M\ 3B DTST
 REPEAT

OP1@ @ 80000000FF AND 25 XOR \ and $_,%rax
OP0@ @ 80000000FFFF AND 3548 XOR \ XOR $_,%rax
 OR 0=
 M_WL  M\ 4E DTST
	OP0 -1 OPresize
	OP0 @ 1+ L@ FF ANDC 0= IF 34 OP0 @ C! -3 ALLOT THEN
	
	M\ 4F DTST
 REPEAT

OP1@ L@ FFFFFF AND 00B60F  XOR \ movzb	(%rax),%eax	# 80406746 	*/.byte 
OP0@ @ 80000000FFFF AND 3548 XOR \ XOR $_,%rax
 OR 0=
 M_WL  M\ 50 DTST
	OP0 -1 OPresize
	OP0 @ 1+ L@ FF ANDC 0= IF 34 OP0 @ C! -3 ALLOT THEN
	
	M\ 51 DTST
 REPEAT

 OP1@ L@ FFFFFF AND
 DUP  f08148 XOR		\ xor     $X,%rax
 SWAP 053348 <> AND	\ xor     X(%rip),%rax
OP0@ L@ FFFFFF AND 458b48 XOR OR \ mov     -0x08(%rbp),%rax
  0=
 M_WL  M\ 16 DTST
	OP1@ 1+ C@ 33 XOR
	IF  	F8 OP1@ 2+ C!  \ cmp    $X,%rax
	ELSE 	3B OP1@ 1+ C!  \ cmp    X(%rip),%rax
	THEN
	M\ 17 DTST
 REPEAT

OP1@ W@		3548	XOR	\ xor     $X,%rax
OP0@ L@ FFFFFF AND 458b48 XOR	OR \ mov     -0x08(%rbp),%rax
  0=
 M_WL  M\ 4C DTST
	3D OP1@	1+ C!  \ cmp	$X,%rax
	M\ 4D DTST
 REPEAT

OP1@ C@ FE AND 34 XOR    \ xor $0x48,%al|eax
OP0@ L@ FFFFFF AND 458b48 XOR \ mov     -0x08(%rbp),%rax
 OR  0=
 M_WL  M\ 56 DTST
	08 OP1@	XOR! \ cmp    $0x48,%al|eax	
	M\ 57 DTST
 REPEAT

OP1@ L@ FFFFFF AND F08348 XOR \ xor	$-0x02,%rax
OP0@ L@ FFFFFF AND 458b48 XOR \ mov     -0x08(%rbp),%rax
 OR  0=
 M_WL  M\ 56 DTST
	F8 OP1@ 2+ C! \ cmp    $-0x02,%rax
	M\ 57 DTST
 REPEAT

OP1@ L@ FFFFFF AND 453348 XOR \ xor	-0x10(%rbp),%rax
OP0@ L@ FFFFFF AND 458b48 XOR \ mov     -0x08(%rbp),%rax
 OR  0=
 M_WL  M\ 58 DTST
	3B OP1@ 1+ C! \ cmp    -0x10(%rbp),%rax
	
	M\ 59 DTST
 REPEAT

OP1@ L@ FFFFFF AND F0894C XOR \ mov	%r14,%rax
OP0@ L@ FFFFFF AND 458948 XOR \ mov	%rax,-8(%rbp)
 OR  0=
 M_WL  M\ 74 DTST
	300004 OP0@ XOR! \ mov	%r14,-0x10(%rbp)
	M\ 75 DTST
 REPEAT

OP1@ 3 + C@  OP0@ 3 + C@ XOR
OP1@ L@ FFFFFF AND 558948 XOR OR \      mov     %rdx,X(%rbp)
OP0@ L@ ADD|OR|AND|XOR<> OR \     __  X(%rbp),%rax
0= M_WL  M\ 5A DTST
	2 OP0@ 1+ XOR!
	D0 OP0@ 2+ C!	\ add    %rdx,%rax
        -1 ALLOT
	M\ 5B DTST
 REPEAT

OP1@ NEWRAX0
OP1@ L@ C0BE0F48 <> AND \  movsbq	%al,%rax
OP0@ NEWRAX0+
OP0@ L@ FFFFFF AND C01948 <> AND OR \  sbb     %rax,%rax        
0= M_WL  M\ 68 DTST
	OP1 OPexcise
	M\ 69 DTST
 REPEAT

OP1@ L@ 00048d48 XOR \      lea     (%rax,%rax,1),%rax
OP0@ L@ 00048d48 XOR \      lea     (%rax,%rax,1),%rax
OR 0= M_WL  M\ 6A DTST
        02e0c148 OP1@ L! \ shl    $0x2,%rax
       OP1 ToOP0
        -4 ALLOT
	M\ 6B DTST
 REPEAT

OP1@ L@ 02e0c148 XOR \       shl    $0x2,%rax
OP0@ L@ 00048d48 XOR \      lea     (%rax,%rax,1),%rax
OR 0= M_WL  M\ 6C DTST
        03e0c148 OP1@ L! \ shl    $0x3,%rax
       OP1 ToOP0
        -4 ALLOT
	M\ 6D DTST
 REPEAT
OP1@ 3 + C@ OP0@ 3 + C@ XOR
OP1@ @ FFFFFF AND 45c748 XOR OR \ movq    $X,-0x10(%rbp)
OP0@ @ FFFFFF AND 453348 XOR OR \ xor     -0x10(%rbp),%rax
0= M_WL  M\ 80 DTST
	35 OP0@	1+ C! \  xor    $X,%rax
	OP1@ 4 + L@ OP0@ 2+ L! \  xor    $X,%rax
	2 ALLOT
	M\ 81 DTST
 REPEAT

  OP2 @ DUP :-SET U< IF DROP BREAK TO OP2@

OP2@ L@ FFFFFF AND d8f748 XOR	\ negq    %rax
OP1@ L@ FFFFFF AND 450348 XOR OR \ add     -0x10(%rbp),%rax
OP0@ L@ FFFFFF AND d8f748 XOR OR \ negq    %rax
  0=
 M_WL  M\ 1E DTST
	28 OP1@ 1+ XOR!  \	sub    -0x10(%rbp),%rax
	OP2 OPexcise
	OP1 ToOP0
	-3 ALLOT
	M\ 1F DTST
 REPEAT

OP2@ L@ FFFFFF AND 058d48 XOR	\ lea     X(%rip),%rax
OP1@ L@ FFFFFF AND
DUP	108948 XOR		\ mov     %rdx,(%rax)
SWAP	100148 <> AND OR	\ add     %rdx,(%rax)
OP0@ L@ FFFFFF AND 458b48 XOR OR \ mov     -0x10(%rbp),%rax
  0=
 M_WL  M\ 3C DTST
	OP1@ 1+ W@ 0500 XOR OP2@ 1+ W!	\ mov    %rdx,X(%rip)
	OP1 OPexcise
	M\ 3D DTST
 REPEAT
OP2@ L@ FFFFFF AND c28948 XOR	\ mov     %rax,%rdx       # 80404e9d
OP1@ L@ FFFFFF AND
DUP	158948 XOR		\ mov     %rdx,X (%rip)
SWAP	150148 <>  AND OR	\ add     %rdx,X (%rip)
OP0@ L@ FFFFFF AND 458b48 XOR OR \ mov     -0x10(%rbp),%rax        # 8
  0=
 M_WL  M\ 3E DTST
	05 OP1@ 2+ C!	\	mov    %rax,X(%rip)
	OP2 OPexcise
	OP0 3 OPinsert
	C289 OP1 @ 1+ W!
	M\ 3F DTST
 REPEAT

OP2@ L@ FFFFFF AND F0894C XOR	\ mov	%r14,%rax
OP1@ L@ FFFFFF AND
DUP	108948 XOR		\ mov	%rdx,(%rax)
SWAP	100148 <> AND OR	\ add	%rdx,(%rax)
OP0@ L@ FFFFFF AND 458b48 XOR OR \ mov	-8(%rbp),%rax
  0=
 M_WL  M\ 6E DTST
	060001 OP1@ XOR!	\ mov	%rdx,(%r14)
	OP2 OPexcise
	M\ 6F DTST
 REPEAT


OP2@ 3 + C@  OP0@ 3 + C@ XOR
OP2@ L@ FFFFFF AND 458948 XOR OR \ mov	%rax,-0x10(%rbp)
OP1@ L@ FFFFFF AND 058b48 XOR OR \ mov	LASTsubHERE+0xd (%rip),%rax
OP0@ L@ FFFFFF AND
DUP 453348 XOR		\ xor	-0x10(%rbp),%rax
SWAP 453948 <> AND OR	\ cmp	-0x10(%rbp),%rax \ !!
0= M_WL M\ 28 DTST
	OP0@ 1+ C@ DUP 39 = IF 2 XOR THEN
	OP1@ 1+ C! \	xor    -0x1b8(%rip),%rax
	OP1 ToOP0
	-4 ALLOT
	M\ 29 DTST
 REPEAT

OP2@ 3 + C@ OP0@ 3 + C@ XOR
OP2@ @ FFFFFF AND 458948 XOR OR	\ mov     %rax,-0x08(%rbp)
OP1@ @ FFFFFF AND 58b48 XOR OR	\ mov     X(%rip),%rax
OP0@ @ FFFFFF AND 453b48 XOR OR	\ cmp     -0x08(%rbp),%rax
0=	M_WL M\ 7E DTST
	OP1@ @ 	OP0@ ! \ удалять некорректно	
	-7 OP0@ 3+ L+!
	3 ALLOT
	39 OP1@ 1+ C!  \ cmp    %rax,X(%rip)
	M\ 7F DTST
 REPEAT

J_COD F0 AND 80 XOR
OP2@ L@ FFFFFF AND
DUP	453948 XOR \ cmp     %rax,-0x10(%rbp)
SWAP	053B48 XOR AND OR \ cmp     LASTsubHERE+0xd (%rip),%rax
OP1@ L@ FFFFFF AND c01948 XOR OR \ sbb	%rax,%rax
OP0@ L@ FFFFFF AND 458b48 XOR OR \ mov	-0x08(%rbp),%rax
  0=
 M_WL  M\ 2A DTST
	OP1 OPexcise
	J_COD 7 XOR TO J_COD
	M\ 2B DTST
 REPEAT

OP2@ 3 + C@ OP1@ 3 + C@ XOR
OP2@ @ FFFFFF AND 458948 XOR OR \ /*    mov     %rax,-0x08(%rbp)
OP1@ @ FFFFFF AND 7d8148 XOR OR \ /*    cmp     $X,-0x08(%rbp)
OP0@ @ FFFFFF AND 458b48 XOR OR \ /*    mov     0x00(%rbp),%rax
  0=
 M_WL  M\ 7A DTST
	OP1@ 4 + L@ OP1@ 2+ L!
	3D OP1@ 1+ C! \ cmp    $X,%rax 
	OP1 6 OPsize!
	M\ 7B DTST
 REPEAT

J_COD 0x84 XOR
OP2@ W@ 3d48 XOR OR		\ cmp     $0x4,%rax
OP1@ @ FFFFFF AND c01948 XOR OR \ sbb     %rax,%rax
OP0@ @ FFFFFF AND 458b48 XOR OR \ mov     0x00(%rbp),%rax
  0=
 M_WL  M\ 7A DTST
	83 TO J_COD
	OP1 OPexcise
	M\ 7B DTST
 REPEAT
J_COD 0x84 XOR
OP2@ @ FFFFFF AND d8f748 XOR OR \            negq    %rax
OP1@ @ FFFFFF AND c01948 XOR OR \    sbb     %rax,%rax
OP0@ @ FFFFFF AND 458b48 XOR OR \    mov     0x08(%rbp),%rax
  0=
 M_WL  M\ 7C DTST
	OP1 OPexcise
	OP1 OPexcise
	M\ 7D DTST
 REPEAT

J_COD 0x84 XOR
OP2@ 3 + C@ OP0@ 3 + C@ XOR OR
OP2@ @ FFFFFF AND 458948 XOR OR \ /*    mov     %rax,-0x08(%rbp)
OP1@ L@		3FF8C148 XOR OR	\ /*    sar     $0x3F,%rax 
OP0@ @ FFFFFF AND 458b48 XOR OR \ /*    mov     -0x08(%rbp),%rax
0=	M_WL  M\ 88 DTST
	C085 OP1@ 1+ W! \ test    %rax,%rax
	8D TO J_COD \  jge
	OP1 ToOP0
	-5 ALLOT
	M\ 89 DTST
 REPEAT


J_COD F0 AND 80 XOR
OP2@ L@ FFFCFF AND C09C0F XOR OR \ set<gl>   %al
OP1@ W@ D8F6 XOR OR	\ neg	%al
OP0@ NEWRAX0+ OR
0=	M_WL M\ 76 DTST
	OP2@ 1+ C@ 95 XOR
	J_COD XOR TO J_COD
	OP1 OPexcise
	OP1 OPexcise
	M\ 74 DTST
 REPEAT

J_COD F0 AND 80 XOR
OP2@ @ 80000000FF AND B8 XOR OR \ movq	$0x30,%rax
OP1@ L@ FFFFFF AND 453B48 XOR OR \ cmp	-0x08(%rbp),%rax
OP0@ NEWRAX0+ OR
0= \  MO_TST_VAL and
	M_WL M\ 78 DTST
\	jae<>jbe jle<>jge
	J_COD DUP 8 AND IF 3 ELSE 5 THEN XOR TO J_COD

	7d81 OP1@ 1+ W! 
	OP1 8 OPsize!
	OP2@ 1+ L@ OP1@ 4 + L!
	OP2 OPexcise
	M\ 74 DTST
 REPEAT

OP2@ L@ FFFFFF AND c0c748 XOR		\ movq	$0x8,%rax
OP2@ @ 	80000000FF AND B8 <> AND	\ movq	$0x8,%rax
OP1@ L@ FFFFFF AND 50148 XOR OR		\ add	%rax,X (%rip)
OP0@ L@ FFFFFF AND 458b48 XOR OR	\ mov	Y(%rbp),%rax # 80404edf
0=	M_WL  M\ 42 DTST
	OP2@ C@ B8 XOR
	IF	OP2@ 3+ SL@
	ELSE	OP2@ 1+ L@
	THEN
	DUP SHORT? \ 0 AND
	IF	OP1 8 OPsize!
		-1  OP1@ 3+ L+!
		82 OP1@ 1+ XOR!	\ addq   $X,Y(%rip)
		OP1@ 7 + C!
	ELSE	OP1 B OPsize!
		-4  OP1@ 3+ L+!
		80 OP1@ 1+ XOR!	\ addq   $X,Y(%rip)
		OP1@ 7 + L!
	THEN
	OP2 OPexcise
	M\ 43 DTST
	REPEAT

OP2@  @ FFFFFFFFFF AND FF25 XOR \ and	$0xFF,%eax
OP1@ C@ 3C XOR OR   \ cmp $0x48,%al|eax

OP2@ C@ B8 XOR \ mov     $0x3,%eax
OP1@ C@ B1 XOR OR   \ mov     $0x03,%cl
0<> AND
OP0@ L@ FFFFFF AND 458b48 XOR OR \ mov     -0x08(%rbp),%rax
0= M_WL	M\ 56 DTST
 OP2 OPexcise	
	M\ 57 DTST
 REPEAT

\ START BLOCK
OP2@ 3 + C@  OP0@ 3 + C@ XOR
OP2@ L@ FFFFFF AND 458948 XOR OR	\ mov	%rax,-0x10(%rbp)
OP0@ L@ FFFFFF AND 558b48 XOR OR \ mov   -0x10(%rbp),%rdx


DUP   \  BLOCK			\ mov	%rax,-0x10(%rbp)
OP1@ L@	FFFFFF AND 058b48 XOR OR	\ mov     X(%rip),%rax
0=	M_WL	DROP		\ mov   -0x10(%rbp),%rdx
	M\ 40 DTST
	15 OP1@ 2+ C!   \ mov     X(%rip),%rdx
	92  OP0@ 1+ C!	\ xchg   %rax,%rdx
	-2 ALLOT
	M\ 41 DTST
	REPEAT

DUP   \  BLOCK		\ mov	%rax,-0x10(%rbp)
OP1@ DX_apply<> OR
0=	M_WL	DROP	\ mov   -0x10(%rbp),%rdx
	M\ 2C DTST
	OP0 OPexcise
	OP0 3 OPinsert
	48 OP1 @ C!   \ mov    %rax,%rdx
	c289 OP1 @ 1+ W!   \ mov    %rax,%rdx
	M\ 2D DTST
	REPEAT

\ END  BLOCK			\ mov	%rax,-0x10(%rbp)
OP1@ L@ FFFFFF AND d08948 XOR OR	\ mov	%rdx,%rax
0=	M_WL			\ mov   -0x10(%rbp),%rdx
	M\ 38 DTST
	92  OP1@ 1+ C!	\ xchg   %rax,%rdx
       OP1 ToOP0
	-5 ALLOT
	M\ 39 DTST
	REPEAT

OP2@ L@ FFFFFF AND 158d48 XOR \       lea     X(%rip),%rdx
OP1@ L@ FFFFFF AND
DUP	28948 XOR	\ mov     %rax,(%rdx)
SWAP	20148 <> AND	\ add     %rax,(%rdx)
 OR  0=
 M_WL  M\ 30 DTST
	OP1@ 1+ C@ 0500 OR
	OP2@ 1+ W! \	mov    %rax,X(%rip)
	OP1 OPexcise
	M\ 31 DTST
 REPEAT

OP2@ L@ FFFFFF AND c0c748 XOR	\ movq	$X,%rax
OP1@ L@ FFFFFF AND 458948 XOR OR	\ mov	%rax,-0x10(%rbp)
OP0@ NEWRAX0+ OR
  0=
 M_WL  M\ 32 DTST
	OP1 8 OPsize!
	45c7  OP1@ 1+ W!
	OP2@ 3 + L@ OP1@ 4 + L! \  movq   $X,0x8(%rbp)
	OP2 OPexcise
	M\ 33 DTST
 REPEAT

OP2@ @ 80000000FF AND B8 XOR \      movq    $0x30,%rax
OP1@ @ FFFFFF AND 458948 XOR OR	\	mov     %rax,-0x08(%rbp)
OP0@ NEWRAX0+ OR
0=	M_WL	M\ 70 DTST
	OP1@ 3+ C@
	OP1 OPexcise
	OP1 3 OPresize
	45c748  OP1 @ L! 
	OP1 @ 3+ C!     \ mov     $0x30,-0x08(%rbp)
	M\ 71 DTST
   REPEAT

OP2@ @ FFFFFF AND 058D48 XOR	\	lea     X (%rip),%rax
OP1@ W@ 	00C6 XOR OR	\	movb    $Y,(%rax)
OP0@ NEWRAX0+ OR
0=	M_WL	M\ 8C DTST
	OP2@ 3+ L@ OP2@ 2+ L!
	OP1@ 2+ C@ OP2@  6 + C!	
	05C6 OP2@ W! \ movb   $0x77,0x9(%rip)
	OP1 OPexcise
	M\ 8D DTST
   REPEAT

OP2@ @ FFFFFF AND 058D48 XOR	\	lea     X (%rip),%rax
OP1@ @ FFFFFF AND 00C748 XOR OR	\	movq    $Y,(%rax)
OP0@ NEWRAX0+ OR
0=	M_WL	M\ 82 DTST
	OP1@ 3+ L@
	OP1 OPexcise
	OP1 B OPsize!
	-4 OP1 @ 3+ L+! 
	OP1 @ 7 + L! 
	C7 OP1 @ 1+ C!  \    movq   $Y,X(%rip)
	M\ 83 DTST
   REPEAT

J_COD FE AND 0x84 XOR
OP2@ @ FFFFFF AND 00B60F XOR OR	\	movzb	(%rax),%eax
OP1@ W@		583C XOR OR	\	cmp	$0x58,%al
OP0@ NEWRAX0+ OR
0=	M_WL	M\ 84 DTST
	583880 	OP2@ L! \ cmpb   $0x58,(%rax)
	OP1 OPexcise
	M\ 85 DTST              
   REPEAT


OP2@ 3 + C@  OP0@ 3 + C@ XOR
OP2@ L@ FFFFFF AND 45c748 XOR OR	\ movq    $-0x1,-0x10(%rbp)
OP1@ L@ FFFFFF AND 058d48 XOR OR	\ lea     OPTque+0x5 (%rip),%rax
OP0@ L@ FFFFFF AND 558b48 XOR OR	\ mov     -0x10(%rbp),%rdx
  0=
 M_WL  M\ 34 DTST
	89 OP0@ 1+ C!	\ mov    %rdx,-0x10(%rbp)
	OP2@ 4+ L@
	OP2 OPexcise
	OP0 7 OPinsert
	c2c748 OP1 @ L!	\ movq	$-0x1,%rdx
	OP1 @ 3 + L!	 
	M\ 35 DTST
 REPEAT

OP2@ 3 + C@  OP0@ 3 + C@ XOR
OP2@ L@ EFFDFF AND 458948 XOR OR \	mov 0x00(%rbp),%rax | %rax,0x00(%rbp)
OP1@ ?ChRAX<>     OR \ 0 not chench rax
OP2@ 3 + C@ OP1@ 3 + C@ = OR
OP0@ L@ EFFDFF AND 458948 XOR OR \	mov 0x00(%rbp),%rax | %rax,0x00(%rbp)
  0=
 M_WL  M\ 40 DTST
	OP1 ToOP0
	-4 ALLOT
	M\ 41 DTST
 REPEAT

OP2@ L@ FFFFFF AND 058b48 XOR \ mov X(%rip),%rax
OP1@ C@ 50 XOR OR \ Push    %rax
OP0@ NEWRAX0+ OR \ mov 0x00(%rbp),%rax
0= 	M_WL	M\ 44 DTST
	35FF OP2@ 1+ W! \ push   X(%rip)
	OP1 OPexcise
	OP1 -1 OPresize
	M\ 45 DTST
	REPEAT

OP2@ C@ 58 XOR	\ pop    %rax
OP1@ L@ FFFFFF AND
 DUP	058948 XOR \ mov %rax,X(%rip)
 SWAP	458948 <>  AND OR \ mov %rax,X(%rbp)
OP0@ NEWRAX0+ OR	\ mov 0x00(%rbp),%rax
0=	M_WL	M\ 64 DTST
	8F OP1@ 1+ C! \ pop   X(%rip)
	-64 DTST
	OP1 -1 OPresize
	OP2 OPexcise
	M\ 63 DTST
	REPEAT

OP2@ L@ FFFFFF AND d08948 XOR	\  mov    %rdx,%rax
OP1@ L@ FFFFFF AND c08548 XOR OR \  test    %rax,%rax
OP0@ NEWRAX0 OR	\ mov 0x00(%rbp),%rax
0=	M_WL	M\ 66 DTST
	d285 OP2@ 1+ W! \ test   %rdx,%rdx
	OP1 OPexcise
	M\ 67 DTST
	REPEAT

OP2@ L@ FFFFFF AND d08948 XOR	\  mov    %rdx,%rax
OP1@ L@ FFFFFF AND 458948 XOR OR \  mov     %rax,0x00(%rbp)
OP0@ NEWRAX0+ OR
0=	M_WL	M\ 8C DTST
	55 OP1@ 2+ C! \ mov     %rdx,0x00(%rbp)
	OP2 OPexcise
	M\ 8D DTST
	REPEAT

OP2@ 3 + C@  OP0@ 3 + C@ XOR
OP2@ 3 + C@  OP1@ 3 + C@ = OR
OP2@ L@ FFFFFF AND 558948 XOR OR \ mov     %rdx,-0x10(%rbp)
OP1@ L@ FFFFFF AND 458948 XOR OR \ mov     %rax,-0x18(%rbp)
OP0@ L@ FFFFFF AND 458b48 XOR OR \ mov     -0x10(%rbp),%rax
0=	M_WL	M\ 5C DTST
   d089 OP0@ 1+ W! -1 ALLOT \ 	mov    %rdx,%rax
		M\ 5B DTST
	REPEAT
(*
OP2@ 3 + C@  OP0@ 3 + C@ XOR
OP2@ L@ FFFFFF AND 558948 XOR OR \ mov     %rdx,-0x10(%rbp)
OP1@ ?ChRDX<> OR
OP0@ L@ ADD|OR|AND|XOR<> OR \	__  X(%rbp),%rax
0= 0 and M_WL	M\ 8E DTST
     4503 D001 XOR  OP0@ 1+ XOR!  \	__  %rdx,%rax
	 -1 ALLOT
		M\ 8F DTST
	REPEAT
*)
OP2@ 3 + C@  OP0@ 3 + C@ XOR
OP2@ L@ FFFFFF AND 458948 XOR OR \      mov     %rax,X(%rbp)
OP1@ @  80000000FF AND B8 XOR OR \      movq    $0x30,%rax
OP0@ @ FFFFFF AND 453948 XOR	\	cmp     %rax,-0x08(%rbp)
OP0@ L@ ADD|OR|AND|XOR<> AND OR \	__  X(%rbp),%rax
0=	M_WL	M\ 4A DTST
	OP1@ 1+ L@ 
	6 OP0@ 1+ XOR! 
	OP1 OPexcise
        2 ALLOT
        OP0 @ 2+ L!
	OP0 @ W@ 3F48 =
	IF	3D48 OP0 @ W!	\ cmp    $0x30,%rax
		SetOP B8 C, OP1 @ 2+ L@ L, \ movq    $0x30,%rax
	THEN
	M\ 4B DTST
   REPEAT

OP2@ L@  24048B48  XOR		\ movq    (%rsp),%rax
OP1@ @ FFFFFF AND F88348 XOR OR	\ cmp     $-2,%rax
OP0@ NEWRAX0+ OR
0=	M_WL	M\ 72 DTST
	OP1 1 OPresize
	243c8348  OP1@ L!  \ cmpq   $-2,(%rsp)
	OP2 OPexcise
	M\ 71 DTST
   REPEAT

OP2@ NEWRAX0
OP1@ AX_apply<> OR
OP0@ NEWRAX0+ OR
0=	M_WL	M\ 72 DTST
	OP2 OPexcise
	M\ 71 DTST
   REPEAT


OP2@ 3 + C@  OP0@ 3 + C@ XOR
OP2@ L@ FFFFFF AND 458948 XOR OR \ mov     %rax,X(%rbp)
OP1@ L@ 24048B48 XOR		\ movq    (%rsp),%rax
OP1@ L@ FFFFFF AND 058B48 <> AND OR \       mov X(%rip),%rax
OP0@ L@ ADD|OR|AND|XOR<> OR	\ __  X(%rbp),%rax
0=	M_WL	M\ 62 DTST
	OP0@ 1+ C@  OP1@ 1+ C!	\  add    (%rsp),%rax
	OP1 ToOP0
	-4 ALLOT	
	M\ 63 DTST
	REPEAT


OP2@ 3 + C@  OP0@ 3 + C@ XOR
OP2@ @ FFFFFF AND 558948 XOR OR \ mov     %rdx,-0x18(%rbp)
OP1@ DX_apply<> OR
OP0@ @ FFFFFF AND 558b48 XOR OR \ mov     -0x18(%rbp),%rdx
0=	M_WL	M\ 60 DTST
         OP1 ToOP0
        -4 ALLOT	
	M\ 61 DTST
   REPEAT

OP2@ 3 + C@  OP0@ 2+ C@ XOR
OP2@ @ FFFFFF AND 45C748 XOR OR \ movq    $0x0,-0x10(%rbp)
OP1@ DX_apply<> OR
OP0@ W@ 558A XOR OR \ mov     -0x10(%rbp),%dl
0=	M_WL	M\ 8A DTST
	OP2@ 4 + C@ OP0@ 1+ C!
	B2 OP0@ C! \ mov    $0x0,%dl
	-1 ALLOT
	M\ 8B DTST
   REPEAT

OP2@ 3 + C@  OP0@ 3 + C@ XOR
OP2@ 3 + C@  OP1@ 3 + C@ = OR
OP2@ L@ FFFFFF AND 458948 XOR OR \ mov     %rax,X(%rbp)
OP1@ L@ FFFFFF AND 458B48 XOR OR \ movq    0x00(%rbp),%rax
OP0@ L@ ADD|OR|AND|XOR<> OR	\ __  X(%rbp),%rax
0=	M_WL	M\ 86 DTST
	OP0@ 1+ C@ OP1@ 1+ C!
       OP1 ToOP0
       -4 ALLOT
 M\ 87 DTST
   REPEAT

OP2@ 3 + C@  OP0@ 3 + C@ XOR
OP2@ L@ FFFFFF AND 458948 XOR OR \ mov     %rax,X(%rbp)
OP1@ L@ FFFFFF AND C0C748 XOR OR \ movq    $0x30,%rax
OP0@ L@ ADD|OR|AND|XOR<> OR	\ __  X(%rbp),%rax
  0=
  WHILE  M\ C DTST
\	C081  OP1@ 1+ W! \  add	$0x30,%rax
	81 OP1@ 1+ C!
	OP0@ 1+ C@ 38 AND OP1@ 2+ XOR!
       OP1 ToOP0
       -4 ALLOT
	OP0 @ 3+ SL@ SHORT?
	IF 83 OP0 @ 1+ C! -3 ALLOT
	THEN
 M\ D DTST
   REPEAT

;

:  MOPT-RULES
 BEGIN  -EVEN-EAX  OPT-RULES
 OP0 @ DUP :-SET U< IF DROP BREAK TO OP0@ 
   OP0@ L@  E7FFFF AND 458948 =  \ mov     %RAX|RDX|RBX|RCX,X(%rbp)
   DUP
   IF DROP OFF-EBP >R   OP0@ 3+ C@ C>S CELL+ TO OFF-EBP   FALSE   OP1 -EBPCLR
              R> TO OFF-EBP 
   THEN
	WHILE   M\ 207 DTST
	REPEAT 
;

: EXIT-RULES  ( ADDR  -- ADDR'  )
\  можно удалять записи во временные ячейки
        -2 DTST
	BEGIN
	OP0 @ :-SET U< IF  BREAK

OP0@ L@ 2448d48 XOR	\	lea     1(%rdx,%rax),%rax
OFF-EAX 0=
OR 0=	M_WL  M\ 20C DTST
	OP0@ 4 + C@ C>S OFF-EAX +
	DUP SHORT?
	IF	OP0@ 4 + C!
	ELSE DUP LONG? 0= IF DROP BREAK
	  84 OP0@ 2+ C! OP0@ 4 + L!
	THEN
        M\ 20D DTST
	MOPT-RULES
	REPEAT

	OP1 @ :-SET U< IF  BREAK

OP1@ C@	B1 XOR		\ mov     $0x03,%cl
OP0@ L@ C0FFFF AND C0D348 XOR OR \ rol..sar     %cl,%r_x
0=	M_WL  M\ 204 DTST
	  C1 OP0@ 1+ C! OP1@ 1+ C@ C, \ rol..sar    $0x3,%rax
	  OP1 OPexcise
	  M\ 205 DTST
	  MOPT-RULES
	REPEAT


OP1@ L@ FFFFFF AND
DUP c28948 XOR			\ mov     %rax,%rdx
SWAP FFFF AND 9248 <> AND	\ xchg    %rdx,%rax       # 80404e
OP0@ L@ FFFFFF AND 458b48 XOR	\ mov     -0x10(%rbp),%rax
 OR 0=	M_WL  M\ 202 DTST
           OP1 OPexcise
        M\ 203 DTST
	MOPT-RULES
	REPEAT
	
OFF-EBP >R
FALSE   OP0 -EBPCLR  OP0 -EBPCLR  OP0 -EBPCLR  OP0 -EBPCLR
R> TO OFF-EBP
	M_WL   M\ 201 DTST
	MOPT-RULES
	REPEAT

 OP2 @ :-SET U< IF  BREAK

OP2@ L@ 1508d48 XOR \        lea     0x01(%rax),%rdx #
OP1@ L@ b60f48 XOR OR \ /*      movzbq  (%rax),%rax     #
OP0@ @ FFFFFF AND d00148 XOR OR \ /*    add     %rdx,%rax
0=	M_WL  M\ 20A DTST
	10 OP1@ 3+ C!	\ movzbq (%rax),%rdx
           OP2 OPexcise
	-2 ALLOT
	0102448d L,	\ lea    0x1(%rdx,%rax),%rax
        M\ 20B DTST
	MOPT-RULES
	REPEAT

OP2@ @ FFFFFF AND c28948 XOR    \ /*    mov     %rax,%rdx
OP1@ @ FFFFFF AND
DUP  168949 XOR  \ /*    mov     %rdx,(%r14)
SWAP 160149 <> AND OR \ /*    add     %rdx,(%r14)
OP0@ @ FFFFFF AND 458b48 XOR OR \ /*    mov     -0x08(%rbp),%rax
0=	M_WL  M\ 20E DTST
        100000 OP1@ XOR!
	OP2 OPexcise
        M\ 20F DTST
	MOPT-RULES
	REPEAT

OP2@ @ FFFFFF AND C28948 XOR \ mov     %rax,%rdx
OP1@ ?ChRAX<> OR
OP0@ @ FFFFFF AND d08948 XOR OR \ mov     %rdx,%rax
0=	M_WL  M\ 210 DTST
	OP1 ToOP0
	-3 ALLOT	
	OP1 OPexcise	
        M\ 211 DTST
	MOPT-RULES
	REPEAT

OP2@ @ FFFFFF AND C2C748 XOR \ movq    $X,%rdx
OP1@ @ FFFFFF AND 108948 XOR OR \ mov     %rdx,(%rax)
OP0@ DX_apply<> OR
0=	M_WL  M\ 212 DTST
	0 OP2@ 2+ C! \ movq   $X,(%rax)
	OP1 OPexcise
        M\ 213 DTST
	MOPT-RULES
	REPEAT

OP2@ C@ B2 XOR \ mov     $X,%dl
OP1@ W@ 1088 XOR OR \ mov     %dl,(%rax)
OP0@ DX_apply<> OR
0=	M_WL  M\ 214 DTST
	OP2 1 OPresize
	00C6 OP2 @ W! \ movb   $X,(%rax)
	OP1 OPexcise
        M\ 215 DTST
	MOPT-RULES
	REPEAT

OP2@ 3 + C@ OP1@ 3 + C@ =
OP2@ @ FFFFFF AND 558b48 XOR OR \ mov     0x08(%rbp),%rdx
OP1@ @ FFFFFF AND 458948 XOR OR \ mov     %rax,-0x08(%rbp)
OP0@ @ FFFFFF AND d08948 XOR OR \ mov     %rdx,%rax
0=	WHILE   M\ 208 DTST
	OP1@ L@	OP2@ L@ 100000 XOR
	OP1@ L!	OP2@ L!
	OP1 ToOP0
	-3 ALLOT
   M\ 209 DTST
	MOPT-RULES
	REPEAT

;

: INLINE?  ( CFA -- CFA FLAG )

  DUP         BEGIN
  2DUP
  MM_SIZE -   U> 0= IF  DROP FALSE  BREAK
  DUP L@      \  CFA CFA+OFF N'
  DUP FF AND 0C3 = IF 2DROP  TRUE BREAK  \ RET
  DUP4B?	M_WL	DROP 4 + REPEAT
  DUP5B?	M_WL	DROP 5 + REPEAT
  DUP7B?	M_WL	DROP 7 + REPEAT
  DUP4B?[EBP]	M_WL 	DROP 4 + REPEAT
  DUP5B?[EBP]	M_WL	DROP 5 + REPEAT
  DUP6B?IP	M_WL	DROP 6 + REPEAT
  DUP7B?IP	M_WL	DROP 7 + REPEAT
  FFFFFF AND
  DUP 6D8D48 =	M_WL	DROP 4 + REPEAT  \ LEA  EBP, OFF-EBP [EBP]
  DUP3B?[EBP]	M_WL	DROP 3 + REPEAT
  DUP3B?	M_WL	DROP 3 + REPEAT
  FFFF AND
  DUP2B?	WHILE 	DROP 2+ REPEAT

 2DROP  FALSE
;

: DO_OPT   ( ADDR -- ADDR' )
  OPT? IF
  MOPT-RULES  
 EVEN-EAX
 THEN ;

: OPT   ( -- )  'NOOP DO_OPT DROP  ;

: 1_,_STEP SetOP DROP COUNT C, ;
: 2_,_STEP SetOP DROP DUP W@ W, 2+ ;
: 3_,_STEP 2_,_STEP COUNT C, ;
: 4_,_STEP SetOP DROP DUP L@ L, 4+ ;
: 2_;_STEP 2_,_STEP DO_OPT ;
: 3_;_STEP 3_,_STEP DO_OPT ;
: 4_;_STEP 4_,_STEP DO_OPT ;
: 5_;_STEP 4_,_STEP COUNT C,	DO_OPT ;
: 6_;_STEP 2_,_STEP DUP L@ L, 4+ DO_OPT ;
: 7_;_STEP 3_,_STEP DUP L@ L, 4+ DO_OPT ;
: +EBP COUNT C>S OFF-EBP + C,	DO_OPT ;
: +REL DUP SL@ OVER + THERE -  L, 4+ \	DO_OPT
 ;

: _INLINE,  (  CFA  --  )
	BEGIN
\  DO_OPT
  DUP L@      \  CFA  N'
  DUP FF AND 0C3 = IF DROP OPT? IF -EVEN-EAX EXIT-RULES EVEN-EAX THEN DROP  BREAK  \ RET
  DUP4B?	M_WL	4_;_STEP REPEAT
  DUP5B?	M_WL	5_;_STEP REPEAT
  DUP6B?	M_WL	6_;_STEP REPEAT
  DUP7B?	M_WL	7_;_STEP REPEAT
  DUP4B?[EBP]	M_WL 3?EBP 3_,_STEP +EBP REPEAT
  DUP5B?[EBP]	M_WL 4?EBP 4_,_STEP +EBP REPEAT
  DUP6B?IP	M_WL	2_,_STEP +REL REPEAT
  DUP7B?IP	M_WL	3_,_STEP +REL  REPEAT
  FFFFFF AND 
  DUP 6D8D48 = M_WL DROP DUP 3 + C@ +>OFF-EBP
                     4 + REPEAT  \ LEA  EBP, OFF-EBP [EBP]
  DUP3B?[EBP]	M_WL 2?EBP 2_,_STEP +EBP REPEAT
  DUP3B?	M_WL	3_;_STEP REPEAT
  FFFF AND
  DUP2B?	WHILE	2_;_STEP REPEAT
  HH. ." @COD, ERROR" ABORT
;

: OPT_INIT  ?SET
\ EVEN-EBP? IF
 -EVEN-EBP
\ THEN
 ;

: OPT_CLOSE
 EVEN-EBP
 THERE TO LAST-HERE
 ;

: OPT; OPT  OPT_CLOSE ;


: INLINE, ( CFA --  )
  OPT_INIT
  _INLINE,
  OPT_CLOSE
 ;

TBASE M!

\  38 DTST
\ [ ' DoTDTST TO TDTST ]
