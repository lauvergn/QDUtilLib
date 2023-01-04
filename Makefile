#=================================================================================
#=================================================================================
# Compiler?
#Possible values: (Empty: gfortran)
#                gfortran (version: 9.0 linux and osx)
 FC = gfortran
#
# Optimize? Empty: default No optimization; 0: No Optimization; 1 Optimzation
OPT = 1
## OpenMP? Empty: default with OpenMP; 0: No OpenMP; 1 with OpenMP
OMP = 1
## Lapack/blas/mkl? Empty: default with Lapack; 0: without Lapack; 1 with Lapack
LAPACK = 1
#=================================================================================
#=================================================================================

#=================================================================================
#
# Operating system, OS? automatic using uname:
OS=$(shell uname)

# Extension for the object directory and the library
ext_obj=_$(FC)_opt$(OPT)_omp$(OMP)

# library name
QDLIBA=lib$(QDLIB)$(ext_obj).a
#=================================================================================

OBJ_DIR=OBJ/obj$(ext_obj)
$(shell [ -d $(OBJ_DIR) ] || mkdir -p $(OBJ_DIR))

MOD_DIR=$(OBJ_DIR)
SRC_DIR=SRC
MAIN_DIR=APP
TESTS_DIR=TESTS

#=================================================================================
#=================================================================================
# gfortran (osx and linux)
#=================================================================================
ifeq ($(FC),gfortran)

  CPPpre  = -cpp

  ifeq ($(OPT),1)
    FFLAGS = -O5 -g -fbacktrace -funroll-loops -ftree-vectorize -falign-loops=16
  else
    FFLAGS = -Og -g -fbacktrace -fcheck=all -fwhole-file -fcheck=pointer -Wuninitialized -finit-real=nan -finit-integer=nan
    #FFLAGS = -O0 -fbounds-check -Wuninitialized
  endif

  FFLAGS +=-J$(MOD_DIR)

  # omp management
  ifeq ($(OMP),1)
    FFLAGS += -fopenmp
  endif

  # lapack management with cpreprocessing
  FFLAGS += $(CPPpre) -D__LAPACK="$(LAPACK)"

  # OS management
  ifeq ($(LAPACK),1)
    ifeq ($(OS),Darwin)    # OSX
      # OSX libs (included lapack+blas)
      FLIB = -framework Accelerate
    else                   # Linux
      # linux libs
      FLIB = -llapack -lblas
      #
      # linux libs with mkl and with openmp
      #FLIB = -L$(MKLROOT)/lib/intel64 -lmkl_intel_lp64 -lmkl_core -lmkl_gnu_thread
      # linux libs with mkl and without openmp
      #FLIB = -L$(MKLROOT)/lib/intel64 -lmkl_intel_lp64 -lmkl_core -lmkl_sequential
    endif
  endif

   FC_VER = $(shell $(FC) --version | head -1 )

endif
#=================================================================================
#=================================================================================

$(info ***********************************************************************)
$(info ***********OS:           $(OS))
$(info ***********COMPILER:     $(FC))
$(info ***********COMPILER_VER: $(FC_VER))
$(info ***********OPTIMIZATION: $(OPT))
$(info ***********OpenMP:       $(OMP))
$(info ***********LAPACK:       $(LAPACK))
$(info ***********FFLAGS:       $(FFLAGS))
$(info ***********FLIB:         $(FLIB))
$(info ***********ext_obj:      $(ext_obj))
$(info ***********************************************************************)


VPATH = $(MAIN_DIR):$(TESTS_DIR):$(SRC_DIR): \
        $(SRC_DIR)/Test: \
        $(SRC_DIR)/NumParameters:$(SRC_DIR)/String:$(SRC_DIR)/File:$(SRC_DIR)/Math

QDLIB=QD

MAIN=App_QDLib
TESTS=Test_QDLib

SRCFILES=Test_m.f90 NumParameters_m.f90 MathUtil_m.f90 \
         String_m.f90 RW_MatVec_m.f90 Matrix_m.f90 Diago_m.f90 \
         QDUtil_m.f90

OBJ0=${SRCFILES:.f90=.o}
OBJ=$(addprefix $(OBJ_DIR)/, $(OBJ0))

#===============================================
#============= Tests ===========================
#===============================================
.PHONY: ut UT
UT ut: $(TESTS).x
	./$(TESTS).x > Tests.log
	grep "Number of error(s)" Tests.log
	@echo "  done Tests"


#===============================================
#============= all: lib, tests ...  ============
#===============================================
.PHONY: all
all: $(QDLIBA) $(MAIN).x $(TESTS).x
#===============================================
#============= Main executable and tests  ======
#===============================================
$(MAIN).x: $(OBJ_DIR)/$(MAIN).o $(QDLIBA)
	$(FC) $(FFLAGS) -o $(MAIN).x  $(OBJ_DIR)/$(MAIN).o $(FLIB) $(QDLIBA)

$(TESTS).x: $(OBJ_DIR)/$(TESTS).o $(QDLIBA)
	$(FC) $(FFLAGS) -o $(TESTS).x  $(OBJ_DIR)/$(TESTS).o $(FLIB) $(QDLIBA)
#===============================================
#============= Library: libQD.a  ===============
#===============================================
.PHONY: lib
lib: $(QDLIBA)

$(QDLIBA): $(OBJ)
	ar -cr $(QDLIBA) $(OBJ)
	rm -f $(OBJ_DIR)/*.o
	@echo "  done Library: "$(QDLIBA)

#===============================================
#============= compilation =====================
#===============================================
$(OBJ_DIR)/%.o: %.f90
	$(FC) $(FFLAGS) -o $@ -c $<

#===============================================
#================ cleaning =====================
.PHONY: clean cleanall
clean:
	rm -f $(OBJ_DIR)/*/*.o $(OBJ_DIR)/*.o
	rm -f *.log test*.txt
	rm -f Test*.x App*.x
	@echo "  done cleaning"

cleanall : clean
	rm -fr OBJ/obj* OBJ/*mod build
	rm -f libQD*.a
	rm -f TESTS/res* TESTS/*log
	@echo "  done all cleaning"
#===============================================
#============= module dependencies =============
#===============================================
$(OBJ_DIR)/NumParameters_m.o:       $(OBJ_DIR)/Test_m.o

$(OBJ_DIR)/MathUtil_m.o:            $(OBJ_DIR)/NumParameters_m.o
$(OBJ_DIR)/String_m.o:              $(OBJ_DIR)/NumParameters_m.o
$(OBJ_DIR)/RW_MatVec_m.o:           $(OBJ_DIR)/NumParameters_m.o

$(OBJ_DIR)/Matrix_m.o:              $(OBJ_DIR)/RW_MatVec_m.o
$(OBJ_DIR)/Diago_m.o:               $(OBJ_DIR)/Matrix_m.o $(OBJ_DIR)/RW_MatVec_m.o


$(OBJ_DIR)/QDUtil_m.o:              $(OBJ_DIR)/Diago_m.o $(OBJ_DIR)/Matrix_m.o $(OBJ_DIR)/RW_MatVec_m.o $(OBJ_DIR)/String_m.o

$(OBJ_DIR)/$(MAIN).o:               $(QDLIBA)
$(OBJ_DIR)/$(TESTS).o:              $(QDLIBA)