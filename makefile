#   ----------------------------------------------------------------------------
CFLAGS ?=
CROSS_COMPILE ?=
LIBS ?= -ldl -m64 -lreadline
#   ----------------------------------------------------------------------------
#   Name of the Linux compiler
#   ----------------------------------------------------------------------------
CC := $(CROSS_COMPILE)gcc

#   ----------------------------------------------------------------------------
#   General options, sources and libraries
#   ----------------------------------------------------------------------------
NAME := forth
SRCS := forth.c
SRCS_S := tmpamain.S
HDRS := 
OBJS :=
DEBUG :=
BIN := forth

FORTH_SRC = Forth64S/promram.4 \
 Forth64S/src/src_PRIMITIVES.f \
 Forth64S/src/src_HPROC.f  \
 Forth64S/src/src_VARS.f \
 Forth64S/src/CORTEX_M_NUMB_PARSE_IO.f \
 Forth64S/src/CORTEX_LITERAL.f \
 Forth64S/src/src_FIND_INTERP.f \
 Forth64S/src/TERM/INCLUDE.F \
 Forth64S/src/fstart.4 \
 Forth64S/Meta_x86_64/SRC/gasm64.4 \
 Forth64S/Meta_x86_64/SRC/disgasm.4 \
 Forth64S/Meta_x86_64/SRC/tc.f \
 Forth64S/src/global.4 \
 Forth64S/Meta_x86_64/SRC/mlist.f \
 Forth64S/Meta_x86_64/SRC/LEX.F \
 Forth64S/MLibs.4 \
 Forth64S/Meta_x86_64/mhead0.f \
 Forth64S/src/macroopt.f \
 Forth64S/Meta_x86_64/SRC/macroopt.f 

#   ----------------------------------------------------------------------------
#   Compiler and Linker flags for Release
#   ----------------------------------------------------------------------------
OBJDIR_R := obj
BINDIR_R := $(OBJDIR_R)
LIBS_R := $(LIBS)
OBJS_R := $(SRCS:%.c=$(OBJDIR_R)/%.o)
OBJS_R += $(SRCS_S:%.S=$(OBJDIR_R)/%.o)
ALL_RELEASE :=

#   ----------------------------------------------------------------------------
#   Compiler include directories 
#   ----------------------------------------------------------------------------
INCLUDES := 

#   ----------------------------------------------------------------------------
#   All compiler options to be passed to the command line
#   ----------------------------------------------------------------------------
ALL_CFLAGS := $(INCLUDES)                   \
              -c                            \
              $(CFLAGS)

ALL_S_FLAGS :=

LDFLAGS :=

#   ----------------------------------------------------------------------------
#   Compiler symbol definitions 
#   ----------------------------------------------------------------------------
DEFS :=

#   ----------------------------------------------------------------------------
#   Compiler and Linker procedure
#   From this point and on changes are very unlikely.
#   ----------------------------------------------------------------------------
.PHONY: all
all: $(BIN)

Forth64S/Meta_x86_64/MetaForth: Forth64S/Meta_x86_64/Mak64CppForth
	cp Forth64S/Meta_x86_64/Mak64CppForth Forth64S/Meta_x86_64/MetaForth

Forth64S/Meta_x86_64/Mak64CppForth: Forth64S/Meta_x86_64/Mak64Forth.cpp 
	g++ -o $@ $< -Wreturn-type

tmpamain.S: $(FORTH_SRC) Forth64S/amain.S Forth64S/Meta_x86_64/MetaForth
	./sgen.sh 

#   ----------------------------------------------------------------------------
#   Building Release... 
#   ----------------------------------------------------------------------------
.PHONY: release
release: 

$(BIN): $(OBJS_R)
	@echo Compiling Release...
	$(CC) -o $@ $(OBJS_R) $(LIBS_R) $(LDFLAGS) -Wl,-Map,$(BINDIR_R)/$(NAME).map -lreadline -no-pie

#	cp $@ $(NAME)


$(OBJDIR_R)/%.o : %.c $(HDRS)
	@mkdir -p $(OBJDIR_R)
	$(CC) $(DEFS) $(ALL_RELEASE) $(ALL_CFLAGS) -o$@ $<

$(OBJDIR_R)/%.o : %.S $(HDRS)
	@mkdir -p $(OBJDIR_R)
	$(CC) $(ALL_RELEASE) $(ALL_S_FLAGS) -c -o$@ $<

.PHONY: clean
clean:
	@rm -rf $(OBJDIR_R)
	@rm -f Forth64S/Meta_x86_64/Mak64CppForth tmpamain.S forth 
	@rm -f Forth64S/Meta_x86_64/MetaForth Forth64S/src/*.S


