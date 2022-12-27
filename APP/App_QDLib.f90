PROGRAM App_QDLib
  USE QDUtil_NumParameters_m
  USE QDUtil_String_m
  USE QDUtil_RW_MatVec_m
  USE QDUtil_Matrix_m
  USE QDUtil_diago_m
  IMPLICIT NONE

  integer                          :: i,n
  real(kind=Rkind),    allocatable :: RMat(:,:),REigVal(:),REigVec(:,:)

  !====================================================================
  ! Tests for the matrix digonalization
  !
  ! define the matrices
  n = 3
  RMat =  reshape([ONE,HALF,ZERO,                             &
                   HALF,ONE,HALF,                             &
                   ZERO,HALF,ONE],shape=[n,n])

  allocate(REigVal(n))
  allocate(REigVec(n,n))


  CALL diagonalization(RMat,REigVal,REigVec)


  CALL Write_Mat(RMat,out_unit,5,info='RMat')
  write(out_unit,*)
  CALL Write_Mat(REigVec,out_unit,5,info='REigVec (in column)')
  write(out_unit,*)
  CALL Write_Vec(REigVal,out_unit,5,info='REigVal')
  write(out_unit,*)

  DO i=1,n
    write(out_unit,*) i,matmul(Rmat,REigVec(:,i))-REigVal(i)*REigVec(:,i)
  END DO


END PROGRAM App_QDLib