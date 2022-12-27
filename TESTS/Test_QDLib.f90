PROGRAM Test_QDLib
  USE QDUtil_NumParameters_m
  USE QDUtil_String_m
  USE QDUtil_RW_MatVec_m
  USE QDUtil_Matrix_m
  USE QDUtil_diago_m
  IMPLICIT NONE

  !CALL Test_QDUtil_Diago() ; stop

  CALL Test_QDUtil_NumParameters()
  CALL Test_QDUtil_String()
  CALL Test_QDUtil_RW_MatVec()
  CALL Test_QDUtil_Matrix()
  CALL Test_QDUtil_Diago()

END PROGRAM Test_QDLib