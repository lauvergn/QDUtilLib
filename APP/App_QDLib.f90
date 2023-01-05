PROGRAM App_QDLib
  USE QDUtil_m
  IMPLICIT NONE

  integer                          :: i,n
  real(kind=Rkind),    allocatable :: RMat(:,:),REigVal(:),REigVec(:,:)

  !  #if __LAPACK != 1
  !    write(out_unit,*) '  Lapack library is linked'
  !  #else
  !    write(out_unit,*) '  Lapack library is not linked'
  !  #endif

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

  write(out_unit,*) 'print_level',print_level

END PROGRAM App_QDLib