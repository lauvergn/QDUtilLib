#===============================================
qdutil_file_m = $(OBJ_DIR)/File_m.o
qdutil_frac_m = $(OBJ_DIR)/Frac_m.o
qdutil_diago_m = $(OBJ_DIR)/Diago_m.o
qdutil_fft_ooura_m = $(OBJ_DIR)/FFT_m.o
qdutil_intvec_m = $(OBJ_DIR)/IntVec_m.o
qdutil_mathutil_m = $(OBJ_DIR)/MathUtil_m.o
qdutil_matrix_m = $(OBJ_DIR)/Matrix_m.o
qdutil_realvec_m = $(OBJ_DIR)/RealVec_m.o
qdutil_rw_matvec_m = $(OBJ_DIR)/RW_MatVec_m.o
qdutil_vector_m = $(OBJ_DIR)/Vector_m.o
qdutil_memory_base_m = $(OBJ_DIR)/Memory_base_m.o
qdutil_memory_m = $(OBJ_DIR)/Memory_m.o
qdutil_memory_notpointer_m = $(OBJ_DIR)/Memory_NotPointer_m.o
qdutil_memory_pointer_m = $(OBJ_DIR)/Memory_Pointer_m.o
qdutil_numparameters_m = $(OBJ_DIR)/NumParameters_m.o
qdutil_m = $(OBJ_DIR)/QDUtil_m.o
qdutil_boxab_m = $(OBJ_DIR)/BoxAB_m.o
qdutil_fourier_m = $(OBJ_DIR)/Fourier_m.o
qdutil_hermitep_m = $(OBJ_DIR)/HermiteP_m.o
qdutil_legendrep_m = $(OBJ_DIR)/LegendreP_m.o
qdutil_quadrature_m = $(OBJ_DIR)/Quadrature_m.o
qdutil_string_m = $(OBJ_DIR)/String_m.o
qdutil_test_m = $(OBJ_DIR)/Test_m.o
qdutil_time_m = $(OBJ_DIR)/Time_m.o
#===============================================
$(OBJ_DIR)/File_m.o : \
          $(qdutil_numparameters_m) \
          $(qdutil_string_m) \
          $(qdutil_test_m)
$(OBJ_DIR)/Frac_m.o : \
          $(qdutil_numparameters_m) \
          $(qdutil_string_m) \
          $(qdutil_test_m)
$(OBJ_DIR)/Diago_m.o : \
          $(qdutil_numparameters_m) \
          $(qdutil_rw_matvec_m) \
          $(qdutil_test_m) \
          $(qdutil_string_m)
$(OBJ_DIR)/FFT_m.o : \
          $(qdutil_numparameters_m) \
          $(qdutil_string_m) \
          $(qdutil_test_m)
$(OBJ_DIR)/IntVec_m.o : \
          $(qdutil_memory_m) \
          $(qdutil_numparameters_m) \
          $(qdutil_test_m) \
          $(qdutil_rw_matvec_m)
$(OBJ_DIR)/MathUtil_m.o : \
          $(qdutil_numparameters_m) \
          $(qdutil_test_m)
$(OBJ_DIR)/Matrix_m.o : \
          $(qdutil_numparameters_m) \
          $(qdutil_rw_matvec_m) \
          $(qdutil_test_m)
$(OBJ_DIR)/RealVec_m.o : \
          $(qdutil_numparameters_m) \
          $(qdutil_memory_m) \
          $(qdutil_test_m) \
          $(qdutil_rw_matvec_m)
$(OBJ_DIR)/RW_MatVec_m.o : \
          $(qdutil_numparameters_m) \
          $(qdutil_string_m) \
          $(qdutil_test_m)
$(OBJ_DIR)/Vector_m.o : \
          $(qdutil_numparameters_m) \
          $(qdutil_test_m) \
          $(qdutil_rw_matvec_m)
$(OBJ_DIR)/Memory_base_m.o : \
          $(qdutil_numparameters_m)
$(OBJ_DIR)/Memory_m.o : \
          $(qdutil_memory_base_m) \
          $(qdutil_memory_pointer_m) \
          $(qdutil_memory_notpointer_m) \
          $(qdutil_numparameters_m) \
          $(qdutil_string_m) \
          $(qdutil_test_m)
$(OBJ_DIR)/Memory_NotPointer_m.o : \
          $(qdutil_numparameters_m) \
          $(qdutil_memory_base_m)
$(OBJ_DIR)/Memory_Pointer_m.o : \
          $(qdutil_numparameters_m) \
          $(qdutil_memory_base_m)
$(OBJ_DIR)/NumParameters_m.o : \
          $(qdutil_test_m)
$(OBJ_DIR)/QDUtil_m.o : \
          $(qdutil_numparameters_m) \
          $(qdutil_mathutil_m) \
          $(qdutil_string_m) \
          $(qdutil_rw_matvec_m) \
          $(qdutil_matrix_m) \
          $(qdutil_vector_m) \
          $(qdutil_diago_m) \
          $(qdutil_intvec_m) \
          $(qdutil_realvec_m) \
          $(qdutil_frac_m) \
          $(qdutil_file_m) \
          $(qdutil_time_m) \
          $(qdutil_memory_m) \
          $(qdutil_fft_ooura_m) \
          $(qdutil_quadrature_m) \
          $(qdutil_hermitep_m) \
          $(qdutil_boxab_m) \
          $(qdutil_fourier_m)
$(OBJ_DIR)/BoxAB_m.o : \
          $(qdutil_numparameters_m)
$(OBJ_DIR)/Fourier_m.o : \
          $(qdutil_numparameters_m)
$(OBJ_DIR)/HermiteP_m.o : \
          $(qdutil_numparameters_m)
$(OBJ_DIR)/LegendreP_m.o : \
          $(qdutil_numparameters_m)
$(OBJ_DIR)/Quadrature_m.o : \
          $(qdutil_numparameters_m) \
          $(qdutil_string_m) \
          $(qdutil_diago_m) \
          $(qdutil_rw_matvec_m) \
          $(qdutil_hermitep_m) \
          $(qdutil_boxab_m) \
          $(qdutil_fourier_m) \
          $(qdutil_legendrep_m) \
          $(qdutil_matrix_m) \
          $(qdutil_test_m)
$(OBJ_DIR)/String_m.o : \
          $(qdutil_numparameters_m) \
          $(qdutil_memory_base_m) \
          $(qdutil_test_m)
$(OBJ_DIR)/Time_m.o : \
          $(qdutil_numparameters_m)
