#-----------------------------------------------------------------------
#
# File:  bluefire.gnu
#
#  Contains compiler and loader options for linux machines using the 
#  intel compiler and specifies the serial directory for communications 
#  modules.
#
#-----------------------------------------------------------------------
F77 = xlf
F90 = xlf90
LD = xlf90
CC = cc
Cp = /bin/cp
Cpp = /bin/cpp -P -C 
AWK = /bin/awk
ABI =
COMMDIR = serial
 
#  Enable MPI library for parallel code, yes/no.

MPI = no

# Adjust these to point to where netcdf is installed

# These have been loaded as a module so no values necessary
#NETCDFINC = -I/netcdf_include_path
#NETCDFLIB = -L/netcdf_library_path
#NETCDFINC = -I/usr/projects/climate/maltrud/local/include_coyote
#NETCDFLIB = -L/usr/projects/climate/maltrud/local/lib_coyote
NETCDFINC = -I/usr/local/include
NETCDFLIB = -L/usr/local/lib

#  Enable trapping and traceback of floating point exceptions, yes/no.
#  Note - Requires 'setenv TRAP_FPE "ALL=ABORT,TRACE"' for traceback.

TRAP_FPE = no

#------------------------------------------------------------------
#  precompiler options
#------------------------------------------------------------------

Cpp_opts = -DPOSIX 
 
#----------------------------------------------------------------------------
#
#                           C Flags
#
#----------------------------------------------------------------------------
 
CFLAGS = $(ABI) -q64 -O2

ifeq ($(OPTIMIZE),yes)
  CFLAGS := $(CFLAGS)
else
  CFLAGS := $(CFLAGS)
endif
 
#----------------------------------------------------------------------------
#
#                           FORTRAN Flags
#
#----------------------------------------------------------------------------

FBASE = $(ABI) -g -qrealsize=8 -qfullpath -O2 -qstrict -qmaxmem=-1 -qarch=auto -Q -qsigtrap=xl__trcedump $(NETCDFINC) $(MPI_COMPILE_FLAGS) -I$(DepDir)
MODSUF = mod

ifeq ($(TRAP_FPE),yes)
  FBASE := $(FBASE) 
endif

ifeq ($(OPTIMIZE),yes)
  FFLAGS = $(FBASE)
else
  FFLAGS = $(FBASE)
endif
 
#----------------------------------------------------------------------------
#
#                           Loader Flags and Libraries
#
#----------------------------------------------------------------------------
 
LDFLAGS = $(ABI) -q64 -bdatapsize:64K -bstackpsize:64K -btextpsize:64K
 
LIBS = $(NETCDFLIB) -lnetcdf
 
ifeq ($(MPI),yes)
  LIBS := $(LIBS) $(MPI_LD_FLAGS) -lmpi 
endif

ifeq ($(TRAP_FPE),yes)
  LIBS := $(LIBS) 
endif
 
LDLIBS = $(LIBS)
 
#----------------------------------------------------------------------------
