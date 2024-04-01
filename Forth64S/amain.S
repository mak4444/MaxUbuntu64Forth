//#include <asm-offsets.h>

	.section	.Fsection,"alwx"

#define STACK_SIZE 20480

DEFLAST=0

.macro Wortbirne Name CName FLine Flags
//	.align	3
	.quad	\CName
	.quad	\Flags
	.quad	DEFLAST	// Link einfügen  Insert Link
	DEFLAST=.
	.byte 8f - 7f	// Länge des Namensfeldes berechnen  Calculate length of name field
7:	.ascii "\Name"    // Namen anfügen  Insert name string
8:
.endm

.macro CONSTANT_W Name Value FLine Flags
	.quad	8f
	.quad	\Flags
	.quad	DEFLAST	// Link einfügen  Insert Link
	DEFLAST=.
	.byte 8f - 7f	// Länge des Namensfeldes berechnen  Calculate length of name field
7:	.ascii "\Name"	// Namen anfügen  Insert name string
8:
	call	DOCONSTANT
	.quad	\Value
.endm

.macro VARIABLE_W Name Value FLine Flags
	.quad	8f
	.quad	\Flags
	.quad	DEFLAST	// Link einfügen  Insert Link
	DEFLAST=.
	.byte 8f - 7f	// Länge des Namensfeldes berechnen  Calculate length of name field
7:	.ascii "\Name"	// Namen anfügen  Insert name string
8:
\Name:
	call	DOCREATE
	.quad	\Value
.endm

SVIEWLINK=0

.macro VIEW_S Flags
	.quad	SVIEWLINK,0
	SVIEWLINK=.-16
.endm

.macro S_VIEW_PATH Path
	.quad	SVIEWLINK,-1
	SVIEWLINK=.-16
	.byte 8f - 7f	// Länge des Namensfeldes berechnen  Calculate length of name field
7:	.ascii "\Path"	// Namen anfügen  Insert name string
8:
.endm

.macro S_CALL adr
        mov     %rsp, %rbx
	and	$0xf, %rbx
        jne      0f
	push	$NOOP    // for mmx
0:
	push	%rbp
	mov     %rsp, %rbp
        call   \adr
	pop	%rbp

.endm


DROP = DgreatS
_2SWAP	= CSsubSWAP
RSHIFT = greatgreat
LSHIFT = lessless
_1add  = CHARadd
INVERT = NOT
_0save = OFF
INVERT = NOT
PERFORM = loadEXECUTE
RDROP = RgreatDROP


	.global	fimg
fimg:	//.inst	0x777,0x777,0x777,0x777
IMGLIT:

.include "Forth64S/src/src_PRIMITIVES.S"
.include "Forth64S/src/src_VARS.S"
.include "Forth64S/src/src_HPROC.S"
.include "Forth64S/src/macroopt.S"
.include "Forth64S/src/CORTEX_M_NUMB_PARSE_IO.S"
.include "Forth64S/src/CORTEX_LITERAL.S"
.include "Forth64S/src/src_FIND_INTERP.S"
.include "Forth64S/src/TERM/INCLUDE.S"

//.include "prim64o.S"

//FSTART: ret
.include "Forth64S/src/fstart.S"


	.quad	8f
	.quad	0	// flags
	.quad	0	// Link einfügen  Insert Link
	DEFLAST=.
9:	.byte 8f - 7f     // Länge des Namensfeldes berechnen  Calculate length of name field
7:	.ascii "IMGLIT"    // Namen anfügen  Insert name string
8:	call	DOCONSTANT
	.quad	IMGLIT
//	Wortbirne "BYE" GOODBYE 666 0
//	Wortbirne "ZOPEN-FILE" ZOPENsubFILE 666 0

.include "Forth64S/src/GHEADERS.S"

ORIGVIEWLINK:
	call	DOCONSTANT
	.quad	SVIEWLINK

FORTH_ORIG:
	call	DOCONSTANT
	.quad	DEFLAST

FORTHsubWORDLIST:
	call	DOCREATE
1:	.quad	DEFLAST
2:	.quad	0	# link
	.quad	0f
0:	.byte	5
	.ascii	 "FORTH"   
VOCsubLIST:
	call	DOCREATE
	.quad	2b


CONTEXT_SIZE: 
	call	DOCONSTANT
	.quad	1f - 0f

CONTEXT:
	call DOCREATE
0:	.quad FORTHsubWORDLIST + 5 
	.zero	8*15
1:      .quad	0

CURRENT:
// This variable holds the address of the word list to which new definitions are added.
	call DOCREATE
	.quad FORTHsubWORDLIST + 5 

	.globl start4th_m
start4th_m:
	push %rbp
        xor %eax,%eax
        mov $_Stekc+STACK_SIZE-0x20,%rbp
//	call	c_test
//	call	BYE1


	call	FMAIN
	pop  %rbp
NOOP:	ret

WRITEsubFILE:
        mov    8(%rbp), %rsi
        mov    (%rbp), %edx
        mov    %eax, %edi

	S_CALL  write
	xor    (%rbp),%eax 
        addq    $8*2, %rbp

        ret


READsubFILE:
        mov    8(%rbp),%rsi
        mov    (%rbp),%edx
        mov    %eax,%edi

	S_CALL  read

        mov    (%rbp),%edx
        mov     %rax,8(%rbp)

        cmp     (%rbp),%eax
        setle  %al
        and     $1,%eax
        dec     %eax

        lea     8(%rbp),%rbp
	ret

FREE:
        mov     %rax,%rdi
        call    free
        mov     $0,%eax
        ret


FLUSHsubFILE:
        mov    %eax,%edi

	S_CALL  fsync

        ret

RESIZEsubFILE:
	mov    %eax,%edi
	mov    8(%rbp),%rsi

        mov     %rsp, %rbx
	and	$0xf, %rbx
        jne      0f
	push	$NOOP    // for mmx
0:
	push	%rbp
	mov     %rsp, %rbp
	call	ftruncate
	pop	%rbp

        lea     16(%rbp),%rbp
        ret

ZRENAMEsubFILE:
	mov    %rax,%rdi
	mov    (%rbp),%rsi

        mov     %rsp, %rbx

	and	$0xf, %rbx
        jne      0f
	push	$NOOP    // for mmx
0:	push	%rbp
	mov     %rsp, %rbp
	call	rename
	pop	%rbp

        lea     8(%rbp),%rbp
        ret


ALLOCATE:
        mov    %rax,%rdi

        mov     %rsp, %rbx
	and	$0xf, %rbx
        jne      0f
	push	$NOOP    // for mmx
0:	push	%rbp
	mov     %rsp, %rbp
	call	malloc
	pop	%rbp

        lea     -8(%rbp),%rbp
        mov     %rax,(%rbp)
        cmp     $0,%rax
        je      0f
        mov     $0,%eax
        ret
0:	mov     $-300,%eax
        ret

RESIZE:
        mov     (%rbp),%rdi
        mov     %rax,%rsi

        mov     %rsp, %rbx
	and	$0xf, %rbx
        jne      0f
	push	$NOOP    // for mmx
0:	push	%rbp
	mov     %rsp, %rbp
	call	realloc
	pop	%rbp

        cmp     $0,%rax
        je      0f
        mov     %rax,(%rbp)
        mov     $0,%eax
        ret
0:	mov     $-300,%eax
        ret

C_ACCEPT:

	mov    %eax,%esi
	mov    (%rbp),%rdi

        mov     %rsp, %rbx
	and	$0xf, %rbx
        jne      0f
	push	$NOOP    // for mmx
0:
	push	%rbp
	mov     %rsp, %rbp
	call	CC_ACCEPT
	pop	%rbp

        lea     8(%rbp),%rbp
        retq

BYE:
BYE1:
//        subq    $8*2, %rbp
//        mov $_Stekc+STACK_SIZE-0x20,%rbp
	mov    $0x0,%eax
HALT:	mov    %eax,%edi

	adc	$0xf, %rbp
        mov     %rsp, %rbx
	and	$0xf, %rbx
        je      0f
	push	$NOOP    // for mmx
0:	call   exit

//	.quad 1,2,3,4,5,6,7,8, 1,2,3,4,5,6,7,8 


.comm DPBuff,0x100000
.comm _Stekc,STACK_SIZE
