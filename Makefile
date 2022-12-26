#=================================================================================
#=================================================================================
# Compiler?
#Possible values: (Empty: gfortran)
#                ifort (version: 19.0 linux)
#                gfortran (version: 9.0 linux and osx)
#                pgf90 (version: 17.10-0, linux)
#                nagfor (version 7.0, osx)
#FC = ifort
 FC = gfortran
#FC = nagfor
#
# Optimize? Empty: default No optimization; 0: No Optimization; 1 Optimzation
OPT = 1
## OpenMP? Empty: default with OpenMP; 0: No OpenMP; 1 with OpenMP
OMP = 1
## Lapack/blas/mkl? Empty: default with Lapack; 0: without Lapack; 1 with Lapack
LAPACK = 1
CPPSHELL_LAPACK  = -D__LAPACK="$(LAPACK)"
CPPpre  = -cpp
#=================================================================================
#=================================================================================

FFLAGS=-O3 -Wall -Wextra $(CPPpre) $(CPPSHELL_LAPACK) -fopenmp -J$(MOD_DIR)

OBJ_DIR=OBJ
MOD_DIR=OBJ
SRC_DIR=SRC
MAIN_DIR=APP

VPATH = $(MAIN_DIR):$(SRC_DIR):$(SRC_DIR)/Test:$(SRC_DIR)/NumParameters:$(SRC_DIR)/String:$(SRC_DIR)/File:$(SRC_DIR)/Math

QDLIB=QD



MAIN=Test_QDLib

SRCFILES=Test_m.f90 NumParameters_m.f90 String_m.f90 RW_MatVec_m.f90 Matrix_m.f90 Diago_m.f90
OBJ0=${SRCFILES:.f90=.o}

OBJ=$(addprefix $(OBJ_DIR)/, $(OBJ0))
$(info ************ $(OBJ_DIR)/files: $(OBJ))


$(MAIN).x: $(OBJ_DIR)/$(MAIN).o lib$(QDLIB).a
	$(FC) $(FFLAGS) -o $(MAIN).x  $(OBJ_DIR)/$(MAIN).o -lblas -llapack lib$(QDLIB).a

#===============================================
#============= Library: libQD.a  ===============
#===============================================
lib$(QDLIB).a: $(OBJ)
	ar -cr lib$(QDLIB).a $(OBJ)
	@echo "  done Library: "lib$(QDLIB).a

#===============================================
#============= compilation =====================
#===============================================
$(OBJ_DIR)/%.o: %.f90
	$(FC) $(FFLAGS) -o $@ -c $<

#===============================================
#================ cleaning =====================
.PHONY: clean
clean:
	rm -f $(OBJ_DIR)/*.o $(MOD_DIR)/*.mod
	rm -f *.log test*.txt
	rm -f lib*.a Test*.x


#===============================================
#============= dependencies ====================
#===============================================
$(OBJ_DIR)/NumParameters_m.o:       $(OBJ_DIR)/Test_m.o

$(OBJ_DIR)/String_m.o:              $(OBJ_DIR)/NumParameters_m.o
$(OBJ_DIR)/RW_MatVec_m.o:           $(OBJ_DIR)/NumParameters_m.o

$(OBJ_DIR)/Matrix_m.o:              $(OBJ_DIR)/RW_MatVec_m.o


$(OBJ_DIR)/$(MAIN).o:               lib$(QDLIB).a