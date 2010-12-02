#
# File:  compile.mk
#
#----------------------------------------------------------------------------
#
#  This makefile finally performs the compile of the SCRIP code.  It is
#  called from a driver makefile that has already called makefiles for
#  preprocessing and dependency generation.
#
#----------------------------------------------------------------------------

SHELL = /bin/sh

#----------------------------------------------------------------------------
#
#  Define the dependency and include directories.
#
#----------------------------------------------------------------------------

DepDir = $(SCRIPEXEDIR)/compile/Depends

#----------------------------------------------------------------------------
#
#  Set valid suffixes.
#
#----------------------------------------------------------------------------

#  First clean out current list of suffixes, then define them
.SUFFIXES: 
.SUFFIXES: .o .c .f .f90 .d .do

ifeq ($(OPTIMIZE),yes)
  DEPSUF = .do
else
  DEPSUF = .d
endif

#----------------------------------------------------------------------------
#
#  Include architecture-specific flags and options. 
#
#----------------------------------------------------------------------------

ifneq (,$(SCRIPARCH))
  include $(SCRIPDIR)/build/$(SCRIPARCH).gnu
  export SCRIPARCH
else
  bogus:
	@echo "  Please setenv SCRIPARCH"
endif

#----------------------------------------------------------------------------
#
#  At this stage in the compile process, everything should be in the
#  compile and depend directories.
#
#----------------------------------------------------------------------------

SRCDIRS = $(SCRIPEXEDIR)/compile/ $(DepDir)/
VPATH = $(SRCDIRS)

#----------------------------------------------------------------------------
#
# Define source, object and dependency files.
#
#----------------------------------------------------------------------------

OBJS = 
DEPENDS = 

FSRCS   = $(strip $(foreach dir,$(SRCDIRS),$(wildcard $(dir)*.f)))
ifneq (,$(FSRCS))
  OBJS    := $(addprefix $(SCRIPEXEDIR)/compile/, $(notdir $(FSRCS:.f=.o))) $(OBJS)
  DEPENDS := $(addprefix $(DepDir)/, $(notdir $(FSRCS:.f=$(DEPSUF)))) $(DEPENDS)
endif

F90SRCS   = $(strip $(foreach dir,$(SRCDIRS),$(wildcard $(dir)*.f90)))
ifneq (,$(F90SRCS))
  OBJS    := $(addprefix $(SCRIPEXEDIR)/compile/, $(notdir $(F90SRCS:.f90=.o))) $(OBJS)
  DEPENDS := $(addprefix $(DepDir)/, $(notdir $(F90SRCS:.f90=$(DEPSUF)))) $(DEPENDS)
endif

CSRCS   = $(strip $(foreach dir,$(SRCDIRS),$(wildcard $(dir)*.c)))
ifneq (,$(CSRCS))
  OBJS    := $(addprefix $(SCRIPEXEDIR)/compile/, $(notdir $(CSRCS:.c=.o))) $(OBJS)
  DEPENDS := $(addprefix $(DepDir)/, $(notdir $(CSRCS:.c=$(DEPSUF)))) $(DEPENDS)
endif

#----------------------------------------------------------------------------
#
#  Make the executable.
#
#----------------------------------------------------------------------------

$(SCRIPEXEDIR)/$(TARGETX): $(OBJS)
	@echo "  GNUmakefile is making target '$(TARGETX)'"
	@$(LD) -o $(TARGETX) $(LDFLAGS) $(OBJS) $(LDLIBS) 

#----------------------------------------------------------------------------
#
# Include all the dependency files
#
#----------------------------------------------------------------------------

# Sort to remove duplicates
DEPENDS := $(sort $(DEPENDS))

include $(DEPENDS)

#----------------------------------------------------------------------------
#
# Implicit rules for compilation
#
#----------------------------------------------------------------------------
 
# Cancel the implicit make rules for compiling
%.o : %.f
%.o : %.f90
%.o : %.c

%.o: %.f
	@echo $(SCRIPARCH) Compiling with implicit rule $<
	@cd $(SCRIPEXEDIR)/compile && $(F77) $(FFLAGS) -c $(notdir $<)
 
%.o: %.f90
	@echo $(SCRIPARCH) Compiling with implicit rule $<
	@cd $(SCRIPEXEDIR)/compile && $(F90) $(FFLAGS) -c $(notdir $<)
 
%.o: %.c
	@echo $(SCRIPARCH) Compiling with implicit rule $<
	@cd $(SCRIPEXEDIR)/compile && $(CC) $(Cpp_opts) $(CFLAGS) -c $(notdir $<)

