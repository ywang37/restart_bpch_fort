SHELL=/bin/bash
#
#*****************************************************************************#
#                          Makefile for generate GC-adj restart bpch file     #
#                          Yi Wang,  08/17/2022                               #
#                          wangyi34@cug.edu.cn                                #
#                          China University of Geoscienses, Wuhan             #
#*****************************************************************************#

# IFORT compilation options
FFLAG = -c -cpp -w -O2 -auto -noalign -convert big_endian -traceback -CB

# IFORT compilation options (for debugging: no optimization)
#FFLAG = -c -cpp -w -noalign -convert big_endian -g -traceback

# Compile command -- multiprocessor
F90        = ifort $(FFLAG)
F90        = ifort $(FFLAG)
F90_SIMPLE = ifort -c -O3 -zero
LINK.f     = ifort -convert big_endian 

# Compile command -- single processor
#F90 = ifort $(FFLAG)

# Used libraries
NetCDFLIB=/opt/apps/netcdf-fortran/4.4.4_parallel_studio-2017.4/lib
NetCDFINC=/opt/apps/netcdf-fortran/4.4.4_parallel_studio-2017.4/include

HDFHOME=/opt/apps/hdf/4.2.12_parallel_studio-2017.4
HDFINC=$(HDFHOME)/include
HDFLIB=$(HDFHOME)/lib

HDFEOS_HOME=/opt/apps/hdfeos/2.19_parallel_studio-2017.4
HDFEOS_INC=$(HDFEOS_HOME)/include
HDFEOS_LIB=$(HDFEOS_HOME)/lib

HDF5HOME=/opt/apps/hdf5/1.8.18_parallel_studio-2017.4
HDF5INC=$(HDF5HOME)/include
HDF5LIB=$(HDF5HOME)/lib

LIBS       = -L$(NetCDFLIB) -L$(HDFLIB) -L$(HDFEOS_LIB) -L$(HDF5LIB) -lnetcdff -lhdf5_fortran

# Complier with libraries
F90_netCDF = $(F90) -I$(NetCDFINC)
#F90_HDF    = $(F90) -I$(NetCDFINC) -I$(HDFINC) -I$(HDFEOS_INC) -I$(HDF5INC)
F90_HDF    = $(F90) -I$(NetCDFINC) -I$(HDFEOS_INC) -I$(HDF5INC)



# Object modules
OBJ_MAIN    = main.o
OBJ_MODULES = tool_kit_mod.o        \
				  parameter_mod.o       \
              ifort_errmsg.o        \
	           error_mod.o           \
              charpak_mod.o         \
				  inquireMod.o          \
				  julday_mod.o          \
				  time_mod.o            \
				  file_mod.o            \
				  input_opt_mod.o       \
				  input_mod.o           \
				  cleanup.o

# Executables
restart_bpch.exe    :  $(OBJ_MODULES) $(OBJ_MAIN)
		$(LINK.f)  $(OBJ_MODULES) $(OBJ_MAIN) $(LIBS) -o restart_bpch.exe

# Common compilations
bpch2_mod.o                    : bpch2_mod.f
charpak_mod.o                  : charpak_mod.f
cleanup.o                      : cleanup.f90
error_mod.o                    : error_mod.f
file_mod.o                     : file_mod.f
grid_mod.o                     : grid_mod.f
ifort_errmsg.o                 : ifort_errmsg.f
input_mod.o                    : input_mod.f90
input_opt_mod.o                : input_opt_mod.f90
inquireMod.o                   : inquireMod.F90
julday_mod.o                   : julday_mod.f
main.o                         : main.f90
parameter_mod.o                : parameter_mod.f90
restart_mod.o                  : restart_mod.f
time_mod.o                     : time_mod.f
tool_kit_mod.o                 : tool_kit_mod.f

# Special compilations


#=============================================================================
#  Other Makefile Commands
#=============================================================================
clean: 
	rm -f *.o *.mod *.exe

.SUFFIXES: .f .F .f90 .F90
.f.o:			; $(F90) -c $*.f
.F.o:			; $(F90) -c $*.F
.f90.o:			; $(F90) -c -free $*.f90 
.F90.o:			; $(F90) -c -free $*.F90 
