#
# Makefile for interpolation code
# CVS:$Id: makefile,v 1.3 2000/04/19 22:05:59 pwjones Exp $
#

COMP = gfortran
NETCDFINC = -I/usr/local/include 
NETCDFLIB = -L/usr/local/lib
#NETCDFINC = -I$(HOME)/packages/netcdf-3.6.3-x86_64_linux/include
#NETCDFLIB = -L$(HOME)/packages/netcdf-3.6.3-x86_64_linux/lib
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
	SCRIP_NetcdfMod.o \
	constants.o \
        grids.o

all: CreateLatLon OverlayGMV convertgauss convertPOPT

CreateLatLon: $(OBJ) CreateLatLon.o
	$(COMP) $(FLAGS) $(OBJ) CreateLatLon.o $(LIB) -o $(EXEDIR)/CreateLatLon

OverlayGMV: $(OBJ) OverlayGMV.o
	$(COMP) $(FLAGS) $(OBJ) OverlayGMV.o $(LIB) -o $(EXEDIR)/OverlayGMV

convertgauss: $(OBJ) convertgauss.o
	$(COMP) $(FLAGS) $(OBJ) convertgauss.o $(LIB) -o $(EXEDIR)/convertgauss

convertPOPT: $(OBJ) convertPOPT.o
	$(COMP) $(FLAGS) $(OBJ) convertPOPT.o $(LIB) -o $(EXEDIR)/convertPOPT

SCRIP_KindsMod.o: $(SRCDIR)/SCRIP_KindsMod.F90 $(INCL)
	$(COMP) $(FLAGS) -c $(SRCDIR)/SCRIP_KindsMod.F90

SCRIP_IOUnitsMod.o: $(SRCDIR)/SCRIP_IOUnitsMod.F90 SCRIP_KindsMod.o $(INCL)
	$(COMP) $(FLAGS) -c $(SRCDIR)/SCRIP_IOUnitsMod.F90

SCRIP_ErrorMod.o: $(SRCDIR)/SCRIP_ErrorMod.F90 SCRIP_KindsMod.o SCRIP_IOUnitsMod.o $(INCL)
	$(COMP) $(FLAGS) -c $(SRCDIR)/SCRIP_ErrorMod.F90

SCRIP_NetcdfMod.o: $(SRCDIR)/SCRIP_NetcdfMod.F90 SCRIP_KindsMod.o SCRIP_ErrorMod.o $(INCL)
	$(COMP) $(FLAGS) -c $(SRCDIR)/SCRIP_NetcdfMod.F90

constants.o: $(SRCDIR)/constants.f SCRIP_KindsMod.o $(INCL)
	$(COMP) $(FLAGS) -c $(SRCDIR)/constants.f

grids.o: $(SRCDIR)/grids.f SCRIP_NetcdfMod.o SCRIP_KindsMod.o SCRIP_ErrorMod.o $(INCL)
	$(COMP) $(FLAGS) -c $(SRCDIR)/grids.f

CreateLatLon.o: CreateLatLon.F90 SCRIP_KindsMod.o SCRIP_ErrorMod.o SCRIP_NetcdfMod.o $(INCL)
	$(COMP) $(FLAGS) -c CreateLatLon.F90

OverlayGMV.o: OverlayGMV.F90 SCRIP_KindsMod.o SCRIP_ErrorMod.o SCRIP_NetcdfMod.o grids.o constants.o $(INCL)
	$(COMP) $(FLAGS) -c OverlayGMV.F90

convertgauss.o: convertgauss.f SCRIP_KindsMod.o SCRIP_ErrorMod.o SCRIP_NetcdfMod.o grids.o constants.o $(INCL)
	$(COMP) $(FLAGS) -c convertgauss.f

convertPOPT.o: convertPOPT.f SCRIP_KindsMod.o SCRIP_ErrorMod.o SCRIP_NetcdfMod.o grids.o constants.o $(INCL)
	$(COMP) $(FLAGS) -c convertPOPT.f

clean: 
	/bin/rm CreateLatLon OverlayGMV convertgauss convertPOPT *.o *.mod

