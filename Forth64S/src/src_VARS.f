
VARIABLE SSSTT
VARIABLE STATE
VARIABLE HANDLER

VARIABLE LAST ( --- addr)
\ This variable holds a pointer to the last definition created.
 
255 CONSTANT TC/L
\ DP><DDP
CREATE ATIB TC/L ALLOT
CREATE ERRTIB TC/L ALLOT
VARIABLE >IN
VARIABLE >IN_WORD
VARIABLE ER>IN
VARIABLE ERR&S-ID
VARIABLE SAVEERR?
VARIABLE ERR-LINE
\ DP><DDP

CREATE #TIB 0 ,


\ DP><DDP
VARIABLE  HLD
$100 ALLOT
CREATE PAD $100 ALLOT
\ DP><DDP

CREATE BASE $A , ( -- a-addr ) \ 94



