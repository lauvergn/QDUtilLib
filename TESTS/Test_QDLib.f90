PROGRAM Test_QDLib
  USE QDUtil_m
  IMPLICIT NONE


  CALL Test_QDUtil_NumParameters()  
  CALL Test_QDUtil_MathUtil()
  CALL Test_QDUtil_String()
  CALL Test_QDUtil_RW_MatVec()
  CALL Test_QDUtil_Matrix()
  CALL Test_QDUtil_Diago()

END PROGRAM Test_QDLib