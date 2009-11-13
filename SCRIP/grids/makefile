#
# Makefile for interpolation code
# CVS:$Id: makefile,v 1.3 2000/04/19 22:05:59 pwjones Exp $
#

COMP = f95
NETCDFINC = -p/sw/include 
NETCDFLIB = -L/sw/lib
FLAGS = -O3 $(NETCDFINC)
#FLAGS = -g
LIB  = $(NETCDFLIB) -lnetcdf 
INCL = 
SRCDIR  = ../source
EXEDIR  = .
OBJ  = \
	SCRIP_KindsMod.o \
	SCRIP_ErrorMod.o \
	SCRIP_IOUnitsMod.o \
	SCRIP_NetcdfMod.o

all: CreateLatLon

CreateLatLon: $(OBJ) CreateLatLon.o
	$(COMP) $(FLAGS) $(OBJ) CreateLatLon.o $(LIB) -o $(EXEDIR)/CreateLatLon

SCRIP_KindsMod.o: $(SRCDIR)/SCRIP_KindsMod.F90 $(INCL)
	$(COMP) $(FLAGS) -c $(SRCDIR)/SCRIP_KindsMod.F90

SCRIP_IOUnitsMod.o: $(SRCDIR)/SCRIP_IOUnitsMod.F90 SCRIP_KindsMod.o $(INCL)
	$(COMP) $(FLAGS) -c $(SRCDIR)/SCRIP_IOUnitsMod.F90

SCRIP_ErrorMod.o: $(SRCDIR)/SCRIP_ErrorMod.F90 SCRIP_KindsMod.o SCRIP_IOUnitsMod.o $(INCL)
	$(COMP) $(FLAGS) -c $(SRCDIR)/SCRIP_ErrorMod.F90

SCRIP_NetcdfMod.o: $(SRCDIR)/SCRIP_NetcdfMod.F90 SCRIP_KindsMod.o SCRIP_ErrorMod.o $(INCL)
	$(COMP) $(FLAGS) -c $(SRCDIR)/SCRIP_NetcdfMod.F90

CreateLatLon.o: CreateLatLon.F90 SCRIP_KindsMod.o SCRIP_ErrorMod.o SCRIP_NetcdfMod.o $(INCL)
	$(COMP) $(FLAGS) -c CreateLatLon.F90

clean: 
	/bin/rm *.o *.mod

