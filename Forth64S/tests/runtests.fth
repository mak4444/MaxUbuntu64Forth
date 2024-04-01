\ ANS Forth tests - run all tests

\ Adjust the file paths as appropriate to your system
\ Select the appropriate test harness, either the simple tester.fr
\ or the more complex ttester.fs

CR .( Running ANS Forth and Forth 2012 test programs, version 0.13.4) CR
MO_TST_VAL-off
MO_TST_VAL-on
S" tests/prelimtest.fth" INCLUDED_AL
\ S" tests/tester.fr" INCLUDED_AL
S" tests/ttester.fs" INCLUDED_AL
S" tests/core.fr" INCLUDED_AL
S" tests/coreplustest.fth" INCLUDED_AL
S" tests/utilities.fth" INCLUDED_AL
S" tests/errorreport.fth" INCLUDED_AL
S" tests/coreexttest.fth" INCLUDED_AL
\ S" tests/blocktest.fth" INCLUDED_AL
S" tests/doubletest.fth" INCLUDED_AL
S" tests/exceptiontest.fth" INCLUDED_AL
S" tests/facilitytest.fth" INCLUDED_AL
S" tests/filetest.fth" INCLUDED_AL
\ S" tests/localstest.fth" INCLUDED_AL
S" tests/memorytest.fth" INCLUDED_AL
S" tests/toolstest.fth" INCLUDED_AL
S" tests/searchordertest.fth" INCLUDED_AL
S" tests/stringtest.fth" INCLUDED_AL
REPORT-ERRORS

CR .( Forth tests completed ) CR CR


