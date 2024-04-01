 TEXEC_SET

CODE TEST333

	pop    0x77(%rip) \  8f 05 77 00 00 00    # 0x7d
	push   0x77(%rip) \  ff 35 77 00 00 00    # 0x83
	call   0x77(%rip) \  ff 15 77 00 00 00    # 0x89
	jmp    0x77(%rip) \  ff 25 77 00 00 00    # 0x8f

	pop    0x77(%rax)  \  8f 40 77 
	push   0x77(%rax)  \  ff 70 77 
	call   0x77(%rax)  \  ff 50 77 
	jmp    0x77(%rax)  \  ff 60 77 


	POP %r8
	POP %rAX
	not %r8
	  nz? if,
	movsbq	%al,%rax
	movswq %ax,%rax

	movzbq	%al,%rax
	movzwq %ax,%rax
  THEN,
	movslq (%rax),%rax
	mov %rax,%rax
	mov (%rax),%rax
	neg	%al    \	.byte 0xF6,0xD8
	sub    0x0(%rbp),%r15
	setb   (%rax)
	setb   (%rcx,%rcx)
	setb   (%rcx,%rcx)

	btc    $0x3f,%r15
	btc    $0x3f,%rBX

	movzb %al,%eax
	movzbl %al,%eax
	movzbq %al,%eax

	setb   %al
	setb   %cl

	imul   (%rax,%rax),%ax
	imul   (%rax,%rax),%eax
	imul   (%rax),%eax

	not   %Eax
	mul   %Ecx

	not   (%rax)
	mul   (%rcx)

	div   %ecx
	div   (%rcx,%rcx)
	divq   (%rcx,%rcx)

	add    (%Rcx,%Rax),%eax
	add    (%Rcx,%Rax),%rax

	aNd     (%rax)     ,%ecx
	aNd     (,%rax,1)  ,%ecx
	aNd     (,%rax,8)  ,%ecx

	aNd     %ecx,(%rax)
	aNd     %ecx,(,%rax,1)
	aNd     %ecx,(,%rax,2)
	aNd     %ecx,(,%rax,4)
	aNd     %ecx,(,%rax,8)

	lss    (%Rcx,%Rax),%eax
	lfs    (%Rcx,%Rax),%eax
	lgs    (%Rcx,%Rax),%eax



	imul   (%rax,%rax),%Rax

	movzbl (%rax),%eax
	movzbl 7(%rax),%eax

	Imul   %Rax,%Rax
	Imul   %Rcx,%Rax
	Imul   %Rax,%Rcx

	$0f c, $af c, $d9 c,
\	imul   %edx,%ecx
	imul   %ecx
	imul   %ebx
	
	sar    %cl,%rax

	notb   (%rcx)
\	movb   $0x7,(%rcx)
	rol   $0x7,(%rax)
	rcl   $0x7,(%rax)
	rolb   $0x7,(%rax)
	rclb   $0x7,(%rax)
	and     %ecx,(%Rax,1)
	and     %ecx,(%Rax,2)
	notl	7(,%rax,8)
	notl	7(,%rCx,2)
	notl	(,%rax,8)
	notl	(,%rCx,2)

	add    %rax,7(%rax,%rax)
	add    %rCx,7(%rax,%rax)
	add    %rax,7(%rCx,%rax)
	add    %rax,7(%rax,%rCx)

	add    %rax,(%rax,%rax)
	add    %rCx,(%rax,%rax)
	add    %rax,(%rCx,%rax)
	add    %rax,(%rax,%rCx)


	notl	(%rcx)
	notl	(%rax)
	notl	(%rcx,1)
	notl	(%rax,8)
	add	%eax,(%rax,%rax)
	add	%eax,(%rax,%rax,1)
	add	%eax,(%rax,%rax,2)
	add	%eax,(%rax,1)
	add	(%rax,1),%eax

	.byte 0x67,0xC0,0x10

	.byte 0xA3,1,2,3,4,5,6,7,8
	$A1 C, -1 ,
	$89 C, 05 C, 4 L,


   	test   $0xc2,%al
   	test   $0xc2,%ah

       	add    $0xc2,%al \	04 c2     
       	add    $0xc2,%ah \ 	80 c4 c2  


   	test   $0xc2,%eax



	test   $0xc2,%al

	TEST   $0x9,-0x9(%rax,%rcx,2)
	test   $0x44332211,%rcx
	test   $0x44,%rcx

	.byte 0x1E,0xFA
	$83 c, $d8 c, $fa c,
	$80 c, $d8 c, $fa c,
	sbb    $0xfa,%eax
	sbb    $0xfa,%al
	endbr64
	add   $0x9,-0x9(%rax)
	add   $0x776655,0x44332211(%rax)

	add   $0x9,-0x9(%rax,%rcx,2)
	add   $0x776655,0x44332211(%rax,%rcx,2)
	

	xchg	%rbx,  %rax
	xchg	%rax,  %rbx
	xchg	%rax, (%rbx)
	xchg	%rbx, (%rax,%rbx)

	shll   $0x90,(%rax,%rdx,4)
	shll   $0x90,(%rbp)
	shl    $0x90,%rcx
	.byte 0x48,0xC1,0xF8,0x3F
	.byte 0xC1,0xF8,0x3F
	.byte 0xC0,0xF8,0x3F


	mov    7(%rbp,%rax,8),%rax
	add    0x11223344(%rax,8),%rax
	mov    (%rbp,%rax,8),%rax

 mov   0x100(%rax,%rax,2),%edx
 mov   (%rax,%rax,2),%edx
(*
 add   0x100(%rax,%rax,2),%edx
*)
 add   (%rax,%rax,2),%edx

\	$f2 c, $f c, $10 c, 07 c,

END-CODE

\ EOF

CODE test3330

	DUP,
	mov    $-1,%Rax
	movl    $0x90909090,%eax
\	movabs  ' test333 ,%al
	NEXT
END-CODE

CREATE test3331
 $0f C, 00  C, $c0 C,
 $0f C, 01  C, $c0 C,


\ Push the address of the top element of the return stack 
\ onto the parameter stack.
CODE RP@	(  -- addr )
	lea    -0x8(%rbp),%rbp
	mov    %rax,0x0(%rbp)
	lea    0x8(%rsp),%rax
	NEXT
END-CODE

CODE	DROP	\ remove element of parameter stack .
CODL	D>S
	DROP,
	NEXT
END-CODE

' NOOP TO 'CRASH
' NOOP TO 'NOOP
' DROP TO 'DROP

\ Set the return stack pointer to n .
CODE RP!	( N -- )
	pop    %rbx
	mov    %rax,%rsp
	mov    0x0(%rbp),%rax
	lea    0x8(%rbp),%rbp
	jmp	%rbx
END-CODE


CODE 2RDROP
	mov	(%rsp),%rbx
	lea	0x18(%rsp),%rsp
	jmp	%rbx
END-CODE

\ Pop two items from return stack onto parameter stack
CODE 2R>	( -- D.lo D.hi )
	mov    (%rsp),%rbx
	mov    %rax,-0x8(%rbp)
	mov    0x10(%rsp),%rcx
	mov    0x8(%rsp),%rax
	mov    %rcx,-0x10(%rbp)
	lea    -0x10(%rbp),%rbp
	lea    0x18(%rsp),%rsp
	jmp	%rbx
END-CODE


\ Push a copy of the top two items of the return stack onto the parameter stack.
CODE 2R@	( -- D.lo D.hi )
	mov    %rax,-0x8(%rbp)
	mov    0x8(%rsp),%rax
	mov    0x10(%rsp),%rbx
	mov    %rbx,-0x10(%rbp)
	lea    -0x10(%rbp),%rbp
	NEXT
END-CODE


\ Pop two items from parameter stack, push onto return stack.
CODE 2>R	( D -- )
	pop    %rbx
	push   0(%rbp)
	push   %rax
	lea    0x10(%rbp),%rbp
	mov    -0x8(%rbp),%rax
	jmp	%rbx
END-CODE

CODE SP@	( -- N )
	lea    -0x8(%rbp),%rbp
	mov    %rax,0x0(%rbp)
	mov    %rbp,%rax
	NEXT
END-CODE

\ Set the parameter stack pointer to specified value.
CODE SP!	( n -- )
	lea    0x8(%rax),%rbp
	mov    -0x8(%rbp),%rax
	NEXT
END-CODE

CODE PERFORM	( ... [tx] -- ...' )
CODL @EXECUTE
	mov    (%rax),%rax
\ Execute the word whose CFA is on the stack
CODL EXECUTE	( ... tx -- ...' )
	mov    %rax,%rdx
	mov    0x0(%rbp),%rax
	lea    0x8(%rbp),%rbp
	jmp    %rdx
END-CODE

' PERFORM TO 'PERFORM
' EXECUTE TO 'EXECUTE

CODE SL@
	mov    (%rax),%eax
	cltq
	NEXT
END-CODE

CODE L@
	mov    (%rax),%eax
	NEXT
END-CODE

\ ARM Forth word @   ( addr -- n )
\ Fetch a value from addr
CODE @		( A -- N )
	mov    (%rax),%rax
	NEXT
END-CODE

' @ TO 'GET

CODE C@		( A -- c )
	movzbl (%rax),%eax
	NEXT
END-CODE

CODE W@		( A -- c )
	movzwl (%rax),%eax
	NEXT
END-CODE


\ Store the least significant 8 bits of char at the specified addr
CODE C!		( N,A -- )
	mov    0x0(%rbp),%dl
	mov    %dl,(%rax)
	mov    0x8(%rbp),%rax
	lea    0x10(%rbp),%rbp
	NEXT
END-CODE


CODE W!	( N,A -- )
	mov    0x0(%rbp),%edx
	mov    %dx,(%rax)
	mov    0x8(%rbp),%rax
	lea    0x10(%rbp),%rbp
	NEXT
END-CODE

CODE L!	( N,A -- )
	mov    0x0(%rbp),%edx
	mov    %edx,(%rax)
	mov    0x8(%rbp),%rax
	lea    0x10(%rbp),%rbp
	NEXT
END-CODE


\  Store value n into the address addr
CODE !	( N,A -- )
	mov    0x0(%rbp),%rdx
	mov    %rdx,(%rax)
	mov    0x8(%rbp),%rax
	lea    0x10(%rbp),%rbp
	NEXT
END-CODE

CODE +!		( N,A -- )
	mov    0x0(%rbp),%rdx
	add	%rdx, (%rax)
	mov    0x8(%rbp),%rax
	lea    0x10(%rbp),%rbp
	NEXT
END-CODE

CODE C+!		( N,A -- )
	mov    0x0(%rbp),%rdx
	add	%dl, (%rax)
	mov    0x8(%rbp),%rax
	lea    0x10(%rbp),%rbp
	NEXT
END-CODE

CODE L+!		( N,A -- )
	mov    0x0(%rbp),%rdx
	add	%edx, (%rax)
	mov    0x8(%rbp),%rax
	lea    0x10(%rbp),%rbp
	NEXT
END-CODE

CODE LOR!		( N,A -- )
	mov    0x0(%rbp),%rdx
	or	%edx, (%rax)
	mov    0x8(%rbp),%rax
	lea    0x10(%rbp),%rbp
	NEXT
END-CODE

CODE OR!		( N,A -- )
	mov    0x0(%rbp),%rdx
	or	%Rdx, (%rax)
	mov    0x8(%rbp),%rax
	lea    0x10(%rbp),%rbp
	NEXT
END-CODE

CODE XOR!		( N,A -- )
	mov    0x0(%rbp),%rdx
	xor	%Rdx, (%rax)
	mov    0x8(%rbp),%rax
	lea    0x10(%rbp),%rbp
	NEXT
END-CODE

CODE 1+!	( N A -- )
	incq   (%rax)
	mov    0x0(%rbp),%rax
	lea    0x8(%rbp),%rbp
	NEXT
END-CODE

\ [A] = 0
CODE 0!		( A -- )
CODL OFF		( A -- )
	movq   $0,(%rax)
	DROP,
	NEXT
END-CODE

CODE ON		( A -- )
	movq   $-1,(%rax)
	DROP,
	NEXT
END-CODE

\  Fetch a 64 bit value from addr
CODE 2@		( addr -- low32bits high32bits )
	mov 8(%rax), %rdx
	lea -8(%rbp), %rbp
	mov %rdx, (%rbp)
	mov (%rax), %rax

	NEXT
END-CODE

CODE 2!		( D A -- )
	mov (%rbp), %rdx
	mov %rdx, (%rax)
	mov 8(%rbp), %rdx
     	mov %rdx, 8(%rax)
	mov 0x10(%rbp), %rax
	lea 0x18(%rbp),%rbp
	NEXT
END-CODE


CODE COUNT	( a -- a+1 c ) 
	lea    -0x8(%rbp),%rbp
	lea    0x1(%rax),%rdx
	movzbq (%rax),%rax
	mov    %rdx,0x0(%rbp)
	NEXT
END-CODE

CODE U>D
	lea    -0x8(%rbp),%rbp
	mov    %rax,0x0(%rbp)
	xor    %rax,%rax
	NEXT
END-CODE

CODE S>D
	DUP,
	sar    $0x3f,%rax
	NEXT
END-CODE

CODE C>S ( c -- n )
	movsbq	%al,%rax
	NEXT
END-CODE

CODE W>S ( c -- n )
	movswq %ax,%rax
	NEXT
END-CODE

CODE L>S ( c -- n )
	movslq %eax,%rax
	NEXT
END-CODE

CODE MIN	( N1,N2 -- MIN )
	cmp    0x0(%rbp),%rax
	cmovg  0x0(%rbp),%rax
	lea    0x8(%rbp),%rbp
	NEXT
END-CODE

CODE UMIN	( N1,N2 -- MIN )
	cmp    0x0(%rbp),%rax
	cmova  0x0(%rbp),%rax
	lea    0x8(%rbp),%rbp
	NEXT
END-CODE

CODE MAX	( N1,N2 -- MAX )
	cmp    0x0(%rbp),%rax
	cmovl  0x0(%rbp),%rax
	lea    0x8(%rbp),%rbp
	NEXT
END-CODE

\ ÌÀÊÑÈÌÓÌ
CODE UMAX	( N1,N2 -- MAX )
	cmp    0x0(%rbp),%rax
	cmovb  0x0(%rbp),%rax
	lea    0x8(%rbp),%rbp
	NEXT
END-CODE

\ Return absolute value of top stack item
CODE ABS	( n -- +n )
	cqto
	xor %rdx, %rax
	sub %rdx, %rax
	NEXT
ASM_END

\  If n1 is equal to n2, return TRUE. Otherwise FALSE.
CODE =		( a b -- f )
\  If n is equal to 0, return TRUE. Otherwise FALSE.
	xor    0x0(%rbp),%rax
	lea    0x8(%rbp),%rbp
CODL 0=		( a -- f )
     	sub    $1 , %rax
     	sbb	%rax,%rax

	NEXT
END-CODE

' =	TO '=

CODE  D= ( xd1 xd2 -- flag ) \ 94 DOUBLE
\ flag is true if and only if xd1 is bit-for-bit the same as xd2
	mov    (%rbp),%rdx
	xor    8(%rbp),%rax
	xor    0x10(%rbp),%rdx
	or     %rdx,%rax
	sub    $0x1,%rax
	sbb    %rax,%rax
	lea    0x18(%rbp),%rbp
	NEXT
END-CODE

CODE D0=		( a b -- f )
	or     0x0(%rbp),%rax
	lea    0x8(%rbp),%rbp
	sub    $0x1,%rax
	sbb    %rax,%rax
	NEXT
END-CODE

CODE >		( a b -- f )
	cmp	(%rbp),%rax
	setl	%al
	neg	%al
	movsb	%al,%rax
	lea	8(%rbp),%rbp
	NEXT
END-CODE

CODE <		( a b -- f )
	cmp	(%rbp),%rax
	setg	%al
	neg	%al
	movsb	%al,%rax
	lea	8(%rbp),%rbp
	NEXT
END-CODE

\ If n1 is not equal to n2, return TRUE.  Otherwise FALSE.
CODE <>		( a b -- f )
	xor    0x0(%rbp),%rax
	lea    0x8(%rbp),%rbp
\ If n is not equal to 0, return TRUE.  Otherwise FALSE.
CODL 0<>		( a b -- f )
	neg    %rax
	sbb    %rax,%rax
	NEXT
END-CODE

CODE 0>		( n -- f )
	neg    %rax
\ Returns true if top is negative, ie sign bit is on.
CODL 0<		( n -- f )
	sar    $0x3f,%rax
	NEXT
END-CODE

CODE U>	( n1 n2 -- f )
	cmp    0x0(%rbp),%rax
	sbb    %rax,%rax
	lea    0x8(%rbp),%rbp
	NEXT
END-CODE

CODE U< ( a b -- f )
	cmp    %rax,0x0(%rbp)
	sbb    %rax,%rax
	lea    0x8(%rbp),%rbp
	NEXT
END-CODE

CODE WITHIN
	mov    0x8(%rbp),%rdx
	sub    0x0(%rbp),%rax
	sub    0x0(%rbp),%rdx
	sub    %rax,%rdx
	sbb    %rax,%rax
	lea    0x10(%rbp),%rbp
	NEXT
END-CODE

CODE ANDC	( N1,N2 -- N )
	not    %rax
\ Perform bit-wise logical AND of top two items.
CODL AND	( N1,N2 -- N )
	and    0x0(%rbp),%rax
	lea    0x8(%rbp),%rbp
	NEXT
END-CODE


\ Perform bit-wise logical OR of top two items on parameter stack.
CODE OR		( N1,N2 -- N )
	or     0x0(%rbp),%rax
	lea    0x8(%rbp),%rbp
	NEXT
END-CODE

\ Perform bit-wise logical XOR of top two items on parameter stack.
CODE XOR	( N1,N2 -- N )
	xor	0x0(%rbp),%rax
	lea    0x8(%rbp),%rbp
	NEXT
END-CODE



\ Logically invert the bits of top stack item.
CODE INVERT	( N -- N1 )
CODL NOT	( N -- N1 )
	not    %rax
	NEXT
ASM_END

\ Negate number on top of stack.
CODE NEGATE	( n - -n)
	neg    %rax
	NEXT
ASM_END

CODE DNEGATE ( d - -d)
	mov    0x0(%rbp),%rdx
	neg    %rax
	neg    %rdx
     	sbb    $0x0,%rax \ $48 C, $83 C, $d8 C, $00 C, \
	mov    %rdx,0x0(%rbp)
	NEXT
ASM_END

CODE -		( N1,N2 -- N1-N2 )
	neg    %rax
CODL +		( N1 N2 -- N1+N2 )
	add    0x0(%rbp),%rax
	lea    0x8(%rbp),%rbp
	NEXT
END-CODE


CODE D+ ( d1|ud1 d2|ud2 -- d3|ud3 ) \ 94 DOUBLE
	mov    0x0(%rbp),%rdx
	add    %rdx,0x10(%rbp)
	adc    0x8(%rbp),%rax
	lea    0x10(%rbp),%rbp
	NEXT
ASM_END

CODE D- ( d1|ud1 d2|ud2 -- d3|ud3 ) \ 94 DOUBLE
	mov    (%rbp),%rdx
	sub    %rdx,0x10(%rbp)
	mov    8(%rbp),%rdx
	sbb    %rax,%rdx
	mov    %rdx,%rax
	lea	0x10(%rbp),%rbp
	NEXT
ASM_END

CODE D< ( d1 d2 -- flg ) \ 94 DOUBLE
	mov    (%rbp),%rdx
	cmp    %rdx,0x10(%rbp)
	sbb    %rax,0x8(%rbp)
	setl   %al
	neg    %al
	movsbq %al,%rax
	lea	0x18(%rbp),%rbp
	NEXT
ASM_END

CODE D2/ \ d1 -- d1/2
	sar    %rax
	$48 C, $d1 C, $5d C, 0 C, \ rcrq   0x0(%rbp)
	NEXT
ASM_END

CODE CELL+	( N -- N+8 )
	lea    4(%rax),%rax
CODL 4+		( N -- N+4 )
	inc    %rax
CODL 3+		( N -- N+2 )
	inc    %rax
CODL 2+		( N -- N+2 )
	inc    %rax
CODL 1+		( N -- N+1 )
CODL CHAR+
	inc    %rax
	NEXT
END-CODE
\ 4 TR> DECIMAL 1+ .

CODE CELL-	( N -- N-8 )
	lea    -4(%rax),%rax
CODL 4-		( N -- N+4 )
	lea    -2(%rax),%rax
CODL 2-		( N -- N-2 )
	dec    %rax
CODL 1-		( N -- N-1 )
	dec    %rax
	NEXT
END-CODE

CODE CELLS	( N -- 8*N )
	lea    (%rax,%rax,1),%rax
CODL 4*	( N -- 4*N )
	lea    (%rax,%rax,1),%rax
CODL 2*		( N -- 2*N )
	lea    (%rax,%rax,1),%rax
	NEXT
END-CODE

CODE 2/		( N -- N/2 )
	sar    %rax
	NEXT
END-CODE

CODE U2/        ( N -- N/U2 )
	shr    %rax
        NEXT
END-CODE

\ 32*32=lo(32)
CODE *		( a b -- a*b )
	imulq  0x0(%rbp)
	lea    0x8(%rbp),%rbp
	NEXT
END-CODE

CODE /
	mov    %rax,%rcx
	mov    0x0(%rbp),%rax
	cqto
	idiv   %rcx
	lea    0x8(%rbp),%rbp
	NEXT
ASM_END

CODE MOD
	mov    %rax,%rcx
	mov    0x0(%rbp),%rax
	cqto
	idiv   %rcx
	lea    0x8(%rbp),%rbp
	mov    %rdx,%rax
	NEXT
ASM_END


CODE U/
	mov    %rax,%rcx
	mov    0x0(%rbp),%rax
	xor    %rdx,%rdx
	lea    0x8(%rbp),%rbp
	div    %rcx
	NEXT
ASM_END

CODE UM*
	mulq   0x0(%rbp)
	mov    %rax,0x0(%rbp)
	mov    %rdx,%rax
	NEXT
ASM_END

CODE M*
     	imulq  0x0(%rbp)
	mov    %rax,0x0(%rbp)
	mov    %rdx,%rax
	NEXT
ASM_END

CODE /MOD
	mov    %rax,%rcx
	mov    0x0(%rbp),%rax
	cqto
	idiv   %rcx
	mov    %rdx,0x0(%rbp)
	NEXT
ASM_END

CODE UM/MOD
	mov    %rax,%rcx
	mov    0x0(%rbp),%rdx
	mov    0x8(%rbp),%rax
	div    %rcx
	lea    0x8(%rbp),%rbp
	mov    %rdx,0x0(%rbp)
	NEXT
ASM_END

\ duplicate n1 if <> 0
CODE ?DUP	( N -- N,N )  ( 0 -- 0 )
     	test     %rax,%rax
	je	0f
CODL DUP	( N -- N,N )  \ Duplicate the top element of the stack.
	DUP,
0:	NEXT
ASM_END
\ END-CODE
' DUP TO 'DUP

\ Duplicate double element of data stack.
CODE 2DUP	(  a b -- a b a b )
	mov    0x0(%rbp),%rdx
	mov    %rax,-0x8(%rbp)
	mov    %rdx,-0x10(%rbp)
	lea    -0x10(%rbp),%rbp
	NEXT
END-CODE

\ Exchange the top two items on the stack
CODE SWAP	( N1,N2 -- N2,N1 )
	mov    0x0(%rbp),%rdx
	mov    %rax,0x0(%rbp)
	mov    %rdx,%rax
	NEXT
END-CODE

CODE CS-SWAP
	nop
\ Swap double element of data stack.
CODL 2SWAP	( n1 n2  n3 n4 -- n3 n4  n1 n2 )
	mov    0x0(%rbp),%rcx
	mov    0x8(%rbp),%rdx
	mov    0x10(%rbp),%rbx
	mov    %rcx,0x10(%rbp)
	mov    %rax,0x8(%rbp)
	mov    %rbx,0x0(%rbp)
	mov    %rdx,%rax
	NEXT
END-CODE

\ Remove double element of data stack.
CODE CS-DROP
	nop
CODL 2DROP  	( D -- )
	mov    0x8(%rbp),%rax
	lea    0x10(%rbp),%rbp
	NEXT
END-CODE

CODE CS-PICK  ( xu .. x0 u -- xu .. x0 xu )
	lea    (%rax,%rax),%rax
	mov    8(%rbp,%rax,8),%rdx
	lea	-0x08(%rbp),%rbp
	mov	%rdx,(%rbp)
	mov    8(%rbp,%rax,8),%rax
	ret
END-CODE

CODE 2NIP	( D -- )
	mov    (%rbp),%rdx
	lea    0x10(%rbp),%rbp
	mov    %rdx,(%rbp)
	NEXT
END-CODE

\ Push a copy of the second stack item.
CODE OVER	( n1 n2 -- n1 n2 n1 )
	lea    -0x8(%rbp),%rbp
	mov    %rax,0x0(%rbp)
	mov    0x8(%rbp),%rax
	NEXT
END-CODE

' OVER	TO 'OVER

CODE 2OVER	( n1 n2 -- n1 n2 n1 )
	mov 16(%rbp),%rdx
	mov %rax,-8(%rbp)
	mov %rdx,-16(%rbp)
	mov 8(%rbp),%rax
	lea -16(%rbp),%rbp
	NEXT
END-CODE


CODE PICK	( Nm,...,N1,K -- Nm,...,N1,Nk )
	mov    (%rbp,%rax,8),%rax
	NEXT
END-CODE

\ CODE PLUCK	( N1 N2 N3 -- N1 N2 N3 N1 ) 
\ Tuck the first element under the second one.
CODE TUCK	( n1 n2 --- n2 n1 n2 )
	lea    -0x8(%rbp),%rbp
	mov    0x8(%rbp),%rdx
	mov    %rax,0x8(%rbp)
	mov    %rdx,0x0(%rbp)
	NEXT
END-CODE

CODE ROLL          \ nn..n0 n -- nn-1..n0 nn
\ *G Rotate the order of the top N stack items by one place such
\ ** that the current top of stack becomes the second item and
\ ** the Nth item becomes TOS. See also *\fo{ROT}.
	test   %rax,%rax
	je     1f
	mov    %rax,%rcx
	lea    (,%rax,8),%rax
	mov    %rbp,%rdx
	add    %rax,%rdx
	mov    (%rdx),%rbx
2:	lea    -8(%rdx),%rdx
	mov    (%rdx),%rax
	mov    %rax,8(%rdx)
	dec    %rcx
	jne    2b
	mov    %rbx,%rax
	jmp    3f
1:	mov    (%rbp),%rax
3:	lea    8(%rbp),%rbp
	NEXT
END-CODE



\ Delete the second stack item.
CODE NIP	( n1 n2 -- n2 )
	lea    0x8(%rbp),%rbp
	NEXT
END-CODE

\ Rotate top three stack values, bringing the third item to the top.
CODE ROT	( n1 n2 n3 -- n2 n3 n1 )
	mov    0x0(%rbp),%rdx
	mov    %rax,0x0(%rbp)
	mov    0x8(%rbp),%rax
	mov    %rdx,0x8(%rbp)
	NEXT
END-CODE


\ Rotate top three stack values, bringing the top item to the third.
CODE -ROT	( n1 n2 n3 -- n3 n1 n2 )
	mov    0x8(%rbp),%rdx
	mov    %rax,0x8(%rbp)
	mov    0x0(%rbp),%rax
	mov    %rdx,0x0(%rbp)
	NEXT
END-CODE


CODE ARSHIFT	( a b -- a>>b ) \ arithmetic shift
	mov    %al,%cl
	mov    0x0(%rbp),%rax
	sar    %cl,%rax
	lea    0x8(%rbp),%rbp
	NEXT
END-CODE

CODE RSHIFT	( a b -- a>>b )
CODL >>
	mov    %al,%cl
	mov    0x0(%rbp),%rax
	shr    %cl,%rax	
	lea    0x8(%rbp),%rbp
	NEXT
END-CODE


CODE LSHIFT	( a b -- a<<b )
CODL <<
	mov    %al,%cl
	mov    0x0(%rbp),%rax
	shl    %cl,%rax
	lea    0x8(%rbp),%rbp

	NEXT
END-CODE


CODE TlsIndex!
     	mov    %rax,%r14
	' DROP BRANCH,
END-CODE

CODE TlsIndex@
	lea    -0x8(%rbp),%rbp
	mov    %rax,0x0(%rbp)
	mov	%r14, %rax
	NEXT
END-CODE

CODE CDR
     	mov    -8(%rax),%rax
	NEXT
END-CODE

CODE NAME>C
	lea    -0x18(%rax),%rax
	NEXT
END-CODE

CODE NAME>
	mov    -0x18(%rax),%rax
	NEXT
END-CODE

CODE NAME>F
	lea    -0x10(%rax),%rax
	NEXT
END-CODE

CODE ERASE ( addr len  -- )
	DUP,
	MOV	$0,%rax
CODL FILL ( addr len c -- )
	mov    0x0(%rbp),%rcx
	jrcxz  0f
	mov    0x8(%rbp),%rdi
	rep stosb \ %al,%es:(%rdi)
0:	mov    0x10(%rbp),%rax
	lea    0x18(%rbp),%rbp
	NEXT
END-CODE

CODE CMOVE>
	mov    %rax,%rcx
	mov    0x0(%rbp),%rdi
	mov    0x8(%rbp),%rsi
	std    
	add    %rcx,%rdi
	add    %rcx,%rsi
	dec    %rsi
	dec    %rdi
	rep movsb \ %ds:(%rsi),%es:(%rdi)
	cld    
	lea    0x18(%rbp),%rbp
	mov    -0x8(%rbp),%rax
	NEXT
END-CODE


CODE CMOVE 
	mov    %rax,%rcx
	mov    0x0(%rbp),%rdi
	mov    0x8(%rbp),%rsi
 	cld    
	rep movsb \ %ds:(%rsi),%es:(%rdi)
	lea    0x18(%rbp),%rbp
 	mov    -0x8(%rbp),%rax
	NEXT
END-CODE

CODE ASCIIZ>
	lea    -0x8(%rbp),%rbp
	mov    %rax,%rdx
0:	mov    (%rax),%cl
	lea    0x1(%rax),%rax
	or     %cl,%cl
	jne    0b
	lea    -0x1(%rax),%rax
	sub    %rdx,%rax
	mov    %rdx,0x0(%rbp)
	NEXT
END-CODE


CODE DOVALUE
	nop
CODL DOCONSTANT
	lea    -0x8(%rbp),%rbp
	mov    %rax,0x0(%rbp)
	pop    %rax
	mov    (%rax),%rax
	NEXT
END-CODE

CODE DOCREATE
	lea    -0x8(%rbp),%rbp
	mov    %rax,0x0(%rbp)
	pop    %rax
	NEXT
END-CODE

CODE DOVECT
	pop    %rbx
	mov    (%rbx),%rbx
	jmp	%rbx
END-CODE 


CODE FILE-POSITION ( fileid -- ud ior )
        mov    %eax,%edi
        lea     -8 2 * (%rbp),%rbp
        call    ' mtell
        cmp     $-1,%eax
        je      0f
        mov     %rax,8(%rbp)
	movl   $0x0,0x0(%rbp)
        xor     %eax,%eax
        ret
0:	or	%eax,%eax
	mov     %rax,8(%rbp)
        mov     %rax,(%rbp)
	NEXT
END-CODE

CODE REPOSITION-FILE
        mov     8(%rbp),%rsi
        mov     %eax,%edi

        call    ' rmlseek

        mov     %eax,8(%rbp)

        cmp     %esi,%eax
	setle  %al
        and     $1,%eax
        dec     %eax

        lea     8 2 * (%rbp),%rbp
	NEXT
END-CODE


CODE ZOPEN-FILE
        mov     (%rbp),%rdi
        mov     %rax,%rsi
        call   ' LOPEN
        mov     %rax,(%rbp)
        shl    $0x1f,%rax
	sar    $0x3f,%rax
	NEXT
END-CODE

CODE CLOSE-FILE
        mov    %rax,%rdi
        call    ' close
	NEXT
END-CODE


CODE ZDELETE-FILE
        mov    %rax,%rdi
        call    ' remove
	NEXT
END-CODE

CODE FILE-SIZE
        mov    %eax,%edi
        lea     -8 2 * (%rbp),%rbp
        call    ' smtell
        cmp     $-1,%eax
        je      0f
        mov     %eax,8(%rbp)
	movl   $0x0,0x0(%rbp)
        xor     %eax,%eax
        ret
0:	mov     %eax,8(%rbp)
        mov     %rax,(%rbp)
	NEXT
END-CODE




CODE ZCOUNT ( addr -- addr len )
	lea     -8(%rbp),%rbp
	mov     %rax,%rdx
0:	mov     (%rax),%cl
 	lea     1(%rax),%rax
	or      %cl,%cl
	jne     0b
	lea     -1(%rax),%rax
	sub     %rdx,%rax
	mov     %rdx,(%rbp)
	NEXT
END-CODE
\ EOF

CODE (?DO) (  limit index -- )
	cmp    0x0(%rbp),%rax	\ index = limit
	je     0f
CODL (DO) (  limit index -- )
	pop    %rdx		\ rdx = return address
	mov    (%rdx),%rcx	\ exit address
	lea    0x8(%rdx),%rdx	\ step over it
	push   %rcx		\ rcx = exit address  
	push   %r15		\ r15 = old index - limit - msb
	push   %r14		\ r14 = old index     

	mov    %rax,%r15	\ rbx = index, r15 = index                                     
	sub    0x0(%rbp),%r15	\ r15 = index - limit 
	btc    $0x3f,%r15	\ r15 = index - limit  - msb ; BTC=complement
	mov    %rax,%r14 	\ r14 = index

	mov    0x8(%rbp),%rax	\ restore TOS
	lea    0x10(%rbp),%rbp	\ step data stack                               

	jmp	%rdx
0:	mov    0x8(%rbp),%rax	\ restore TOS
	lea    0x10(%rbp),%rbp	\ step data stack
	pop    %rdx		\ rdx = return address
	mov    (%rdx),%rdx	\ exit address
	jmp	%rdx
END-CODE

' (?DO) TO '(?DO)
' (DO) TO '(DO)

CODE LEAVE
\ RSP+18: ESP+C:  exit address
\ RSP+10: ESP+8:  old index-limit-$8000.0000  (incremented by LOOP)
\ RSP+08: ESP+4:  old index                   (incremented by LOOP)
\ RSP+00: return address
\ R15:    new index-limit-xorbit63
\ R14:    new index
        pop     %rdx	\ return address and discard it
        pop	%r14	\ index
        pop	%r15	\ index-limit-xorbit63
	NEXT
END-CODE


CODE UNLOOP
\ RSP+18: ESP+C:  exit address
\ RSP+10: ESP+8:  old index-limit-$8000.0000  (incremented by LOOP)
\ RSP+08: ESP+4:  old index                   (incremented by LOOP)
\ RSP+00: return address
\ R15:    new index-limit-xorbit63
\ R14:    new index
        pop     %rdx	\ return address and discard it
        pop	%r14	\ index
        pop	%r15	\ index-limit-xorbit63
	pop	%rcx
	jmp	%rdx
END-CODE

\EOF
CODE J
	DUP,
	mov	8(%rsp),%rax
	NEXT
END-CODE
