#-----------------------------------------------------------------------
#
# File:  linuxintel_serial.gnu
#
#  Contains compiler and loader options for linux machines using the 
#  intel compiler and specifies the serial directory for communications 
#  modules.
#
#-----------------------------------------------------------------------
F77 = ifort
F90 = ifort
LD = ifort
CC = cc
Cp = /bin/cp
Cpp = cpp -P -C 
AWK = /usr/bin/gawk
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
NETCDFINC = -I/usr/projects/climate/bzhao/netcdf-3.6.1/include
NETCDFLIB = -L/usr/projects/climate/bzhao/netcdf-3.6.1/lib

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
 
CFLAGS = $(ABI) 

ifeq ($(OPTIMIZE),yes)
  CFLAGS := $(CFLAGS) -O  -fopenmp
else
  CFLAGS := $(CFLAGS) -g -check all -ftrapuv -fopenmp
endif
 
#----------------------------------------------------------------------------
#
#                           FORTRAN Flags
#
#----------------------------------------------------------------------------

FBASE = $(ABI) $(NETCDFINC) $(MPI_COMPILE_FLAGS) -I$(DepDir) -mcmodel=medium -i-dynamic -convert big_endian 
MODSUF = mod

ifeq ($(TRAP_FPE),yes)
  FBASE := $(FBASE) 
endif

ifeq ($(OPTIMIZE),yes)
  FFLAGS = $(FBASE) -O3 -fopenmp
else
  FFLAGS = $(FBASE) -g -check bounds -fopenmp
endif
 
#----------------------------------------------------------------------------
#
#                           Loader Flags and Libraries
#
#----------------------------------------------------------------------------
 
LDFLAGS = $(ABI) -mcmodel=medium -i-dynamic -convert big_endian  -fopenmp
 
LIBS = $(NETCDFLIB) -lnetcdf
 
ifeq ($(MPI),yes)
  LIBS := $(LIBS) $(MPI_LD_FLAGS) -lmpi 
endif

ifeq ($(TRAP_FPE),yes)
  LIBS := $(LIBS) 
endif
 
LDLIBS = $(LIBS)
 
#----------------------------------------------------------------------------
