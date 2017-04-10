#========================================================================
# Makefile to compile the Snow-17/SAC,SCE and driver code for gauge calibrations
#========================================================================
#  Modified:
#  Andy Newman, Sept 2013
#  A. Wood, Dec 2015, updating for model that uses external PET & gfortran comp.
#           Jan 2016, renamed some subroutines, replaced several for code
#                     that now runs in distributed fashion
#           Aug 2015, major overhaul of code, updated makefile
#  Elizabeth Clark, Feb 2017, adding documentation and separating executable
#                     from FC and FC77 definitions so that flags set
#                     properly even if compiler path is needed. Moved compiler
#                     type and executable to PART 0 so that user will be
#                     more likely to set it
#
#========================================================================
# PART 0: Define directory paths
#========================================================================

# Define core directory below which everything resides
F_MASTER_DIR = /Users/eclark/NWS_hydro_models/utils/

# Location of the compiled modules
MOD_PATH = $(F_MASTER_DIR)

# Define the executable and path
EXE = $(F_MASTER_DIR)/ascii2bin.exe

# Define the Fortran Compiler
FCEXE  = gfortran

#========================================================================
# PART 1: Assemble all of the various sub-routines
#========================================================================

# share utilities
calib_UTIL= \
		nrtype.f90 \
		def_namelists.f90 \
		interfaces.f90 \
		read_namelist.f90 \
		ascii_to_bin_io.f90
UTIL = $(patsubst %, $(F_MASTER_DIR)/%, $(calib_UTIL))

#========================================================================
# PART 2: Define the libraries, driver programs, and executables
#========================================================================
# Define the driver routine
calib_DRIVER = rewrite_snow17_state.f90

DRIVER = $(patsubst %, $(F_MASTER_DIR)/%, $(calib_DRIVER))

#========================================================================
# PART 3: Compile the puppy
#========================================================================

# Define flags
FLAGS_DEBUG = -static -Wall -g -ffixed-line-length-none
FLAGS = -O3 -fno-align-commons -ffixed-line-length-none
FLAGS2 = -O3 -fno-align-commons -ffixed-line-length-none


# -- Compile --

all: compile_calib link clean
debug: compile_debug link

check:
	echo test
	echo $(FCEXE)

# compile calibration code
compile_calib:
	$(FCEXE) $(FLAGS) -c $(UTIL) $(DRIVER)

compile_debug:
	$(FCEXE) $(FLAGS) -g -fcheck=all -c $(UTILS) $(DRIVER)

# link routines
link:
	$(FCEXE) -fPIC -I./ $(LIB) -o $(EXE) *.o

# Remove object files
clean:
	rm -f *.o
	rm -f *.mod
