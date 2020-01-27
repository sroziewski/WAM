FFLAGS=-O2 -I/usr/local/include -I/usr/lib64/gfortran/modules/ -I/usr/include
LBITS := $(shell getconf LONG_BIT)
ifeq ($(LBITS),64)
LDFLAGS= -L/usr/lib64 -l netcdff -l netcdf -lm -I/usr/include
else
LDFLAGS=-L/usr/local/lib -L/usr/lib -lnetcdf -L/usr/lib -lnetcdff -I/usr/include 
endif
FC=gfortran
VPATH=Module Preproc Chief Chief/User_sub Print

all: preproc chief print_grid print_time_netcdf dis

### -------------------------------------------------------------------
### Preproc program compilation
### -------------------------------------------------------------------
preproc_objects = WAM_FILE_MODULE.o WAM_GENERAL_MODULE.o WAM_FRE_DIR_MODULE.o \
	WAM_GRID_MODULE.o WAM_NEST_MODULE.o PREPROC_MODULE.o \
	Preproc.o READ_TOPOGRAPHY.o READ_PREPROC_USER.o

preproc:: $(preproc_objects)
	$(FC) -o preproc.exe $(preproc_objects)

$(preproc_objects):: %.o: %.f90
	$(FC) -c $(FFLAGS) $< -o $@
	
clean_preproc::
	rm -f preproc.exe *.o *.mod


### -------------------------------------------------------------------
### Chief program compilation
### -------------------------------------------------------------------
chief_objects = WAM_FILE_MODULE.o WAM_GENERAL_MODULE.o WAM_TIMOPT_MODULE.o \
	WAM_FRE_DIR_MODULE.o WAM_INTERFACE_MODULE.o WAM_GRID_MODULE.o \
	WAM_CURRENT_MODULE.o WAM_MODEL_MODULE.o WAM_ICE_MODULE.o \
	WAM_OUTPUT_SET_UP_MODULE.o WAM_OUTPUT_MODULE.o WAM_WIND_MODULE.o \
	WAM_NEST_MODULE.o WAM_BOUNDARY_MODULE.o WAM_SOURCE_MODULE.o \
	WAM_PROPAGATION_MODULE.o PREPROC_MODULE.o WAM_COLDSTART_MODULE.o \
	WAM_RESTART_MODULE.o WAM_INITIAL_MODULE.o chief.o WAVEMDL.o \
	INITMDL.o WAMODEL.o PRINT_WAM_STATUS.o READ_WAM_USER.o \
	READ_WIND_INPUT.o READ_CURRENT.o READ_BOUNDARY_INPUT.o READ_ICE_INPUT.o 

chief:: $(chief_objects)
	$(FC) -o chief.exe $(chief_objects)	

$(chief_objects):: %.o: %.f90
	$(FC) -c $(FFLAGS) $< -o $@

clean_chief::
	rm chief.exe *.o *.mod

### -------------------------------------------------------------------
### Print netcdf 
### -------------------------------------------------------------------
print_time_objects = WAM_FILE_MODULE.o WAM_GENERAL_MODULE.o WAM_TIMOPT_MODULE.o \
	WAM_GRID_MODULE.o WAM_OUTPUT_SET_UP_MODULE.o WAM_PRINT_MODULE.o \
	PRINT_TIME_NETCDF.o INGRID.o READ_GRID_USER.o

print_time_netcdf:: $(print_time_objects)
	$(FC) -o print_time_netcdf.exe $(print_time_objects) $(LDFLAGS)

$(print_time_objects):: %.o: %.f90
	$(FC) -c $(FFLAGS) $< -o $@

clean_time::
	rm print_time_netcdf.exe *.o *.mod
### -------------------------------------------------------------------
### Print grid program compilation
### -------------------------------------------------------------------
print_grid_objects = WAM_FILE_MODULE.o WAM_GENERAL_MODULE.o WAM_TIMOPT_MODULE.o \
	WAM_GRID_MODULE.o WAM_OUTPUT_SET_UP_MODULE.o WAM_PRINT_MODULE.o \
	PRINT_GRID_FILE.o INGRID.o READ_GRID_USER.o

print_grid:: $(print_grid_objects)
	$(FC) -o print_grid.exe $(print_grid_objects)

$(print_grid_objects):: %.o: %.f90
	$(FC) -c $(FFLAGS) $< -o $@

clean_grid_chief::
	rm print_grid.exe *.o *.mod

### -------------------------------------------------------------------
### Prepare data files
### -------------------------------------------------------------------
prep_data_objects = COAMPS/readdatafile.o

prep_data:: $(prep_data_objects)
	$(FC) -o prep_data.exe $(prep_data_objects)

$(prep_data_objects):: %.o: %.f90
	$(FC) -c $(FFLAGS) $< -o $@

clean_prep_data::
	rm print_grid.exe *.o *.mod
run::
	cd ../bin && ./chief.exe

clean::
	rm *.o *.mod *.exe
dis::
	cp *.exe ../bin/

