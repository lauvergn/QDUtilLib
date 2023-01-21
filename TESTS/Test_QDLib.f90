PROGRAM Test_QDLib
  USE QDUtil_m
  IMPLICIT NONE

!  #if __LAPACK != 1
!    write(out_unit,*) '  Lapack library is linked'
!  #else
!    write(out_unit,*) '  Lapack library is not linked'
!  #endif

  CALL Test_QDUtil_NumParameters()  
  CALL Test_QDUtil_MathUtil()
  CALL Test_QDUtil_String()
  CALL Test_QDUtil_RW_MatVec()
  CALL Test_QDUtil_Matrix()
  CALL Test_QDUtil_Diago()
  CALL Test_QDUtil_Frac

END PROGRAM Test_QDLib