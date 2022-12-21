FC=gfortran
FFLAGS=-O3 -Wall -Wextra -fopenmp -J$(MOD_DIR)

OBJ_DIR=OBJ
MOD_DIR=OBJ
SRC_DIR=SRC
MAIN_DIR=APP

VPATH = $(MAIN_DIR):$(SRC_DIR):$(SRC_DIR)/Test:$(SRC_DIR)/NumParameters:$(SRC_DIR)/String:$(SRC_DIR)/File:$(SRC_DIR)/Math

QDLIB=QD



MAIN=Test_QDLib

SRCFILES=Test_m.f90 NumParameters_m.f90 String_m.f90
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


#===============================================
#============= dependencies ====================
#===============================================
$(OBJ_DIR)/NumParameters_m.o:       $(OBJ_DIR)/Test_m.o
$(OBJ_DIR)/String_m.o:              $(OBJ_DIR)/NumParameters_m.o
$(OBJ_DIR)/$(MAIN).o:               lib$(QDLIB).a