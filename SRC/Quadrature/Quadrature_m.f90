!===============================================================================
!===============================================================================
!This file is part of QDUtil.
!
!===============================================================================
! MIT License
!
! Copyright (c) 2022 David Lauvergnat
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!===============================================================================
!===============================================================================
MODULE QDUtil_Quadrature_m
  USE QDUtil_NumParameters_m
  IMPLICIT NONE

  PRIVATE

  TYPE Quadrature_t
    real (kind=Rkind), allocatable :: x(:,:) ! x(ndim,nq): position
    real (kind=Rkind), allocatable :: w(:)   ! w(ndim,nq): weight
    character (len=:), allocatable :: name
    real (kind=Rkind)              :: Sii = -HUGE(ONE) ! errors of the overlap (diagonal)
    real (kind=Rkind)              :: Sij = -HUGE(ONE) ! errors of the overlap (off diagonal)
  END TYPE Quadrature_t


  PUBLIC :: Quadrature_t,Init_Quadrature_QDUtil,dealloc_Quadrature_QDUtil,Write_Quadrature_QDUtil
  PUBLIC :: Test_Quadrature_QDUtil

CONTAINS
  SUBROUTINE Init_Quadrature_QDUtil(Quadrature,nq,type_name,A,B)
    USE QDUtil_String_m
    USE QDUtil_diago_m
    USE QDUtil_RW_MatVec_m
    USE QDUtil_HermiteP_m
    USE QDUtil_Sine_m
    USE QDUtil_BoxAB_m
    USE QDUtil_Fourier_m
    IMPLICIT NONE

    TYPE (Quadrature_t), intent(inout)        :: Quadrature
    integer,             intent(in)           :: nq
    character (len=*),   intent(in)           :: type_name
    real (kind=Rkind),   intent(in), optional :: A,B


    real (kind=Rkind)               :: dx
    integer                         :: i
    real (kind=Rkind), allocatable  :: Xmat(:,:),DVR(:,:),x(:)

    integer                          :: ib,jb,ib_max,iq,nb
    real (kind=Rkind), allocatable   :: d0gb(:,:)

    !---------------------------------------------------------------------
    logical,parameter :: debug= .FALSE.
    !logical,parameter :: debug= .TRUE.
    character (len=*), parameter :: name_sub='Init_Quadrature_QDUtil'
    !---------------------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*)
      write(out_unit,*) 'BEGINNING ',name_sub
      write(out_unit,*)
    END IF
    !---------------------------------------------------------------------
    nb = nq

    SELECT CASE(TO_lowercase(type_name))
    CASE ('fourier')
      Quadrature%name = 'Fourier'
      dx  = TWO*PI/nq
      x   = [(dx*(-HALF+i),i=1,nq)]
      Quadrature%x = reshape(x,shape=[1,nq])
      Quadrature%w = [(dx,i=1,nq)]

      allocate(d0gb(nq,nb))
      CALL TabGB_Fourier_QDUtil(d0gb,x,ReNorm=.TRUE.)
    CASE ('sine')
      Quadrature%name = 'sine'
      dx  = PI/nq
      x   = [(dx*(-HALF+i),i=1,nq)]
      Quadrature%x = reshape(x,shape=[1,nq])
      Quadrature%w = [(dx,i=1,nq)]

      allocate(d0gb(nq,nb))
      CALL TabGB_Sine_QDUtil(d0gb,x,ReNorm=.TRUE.)

    CASE ('boxab')
      IF (.NOT. present(A) .AND. .NOT. present(B)) THEN
        STOP 'ERROR in Init_Quadrature_QDUtil: A and B must be present for BoxAB quadrature'
      END IF
      Quadrature%name = 'BoxAB'
      dx  = (B-A)/nq
      x   = [(A+dx*(-HALF+i),i=1,nq)]
      Quadrature%x = reshape(x,shape=[1,nq])
      Quadrature%w = [(dx,i=1,nq)]

      allocate(d0gb(nq,nb))
      CALL TabGB_BoxAB_QDUtil(d0gb,x,A,B,ReNorm=.TRUE.)

    CASE ('ho','hermite')
      Quadrature%name = 'HO'
      allocate(x(nq))
      allocate(DVR(nq,nq))

      allocate(Xmat(nq,nq))
      CALL X_Hermite_QDUtil(Xmat)
      IF (debug) CALL Write_Mat(Xmat,nio=out_unit,nbcol=5,info='Xmat')
      CALL diagonalization(Xmat,x,DVR)
      IF (debug) CALL Write_Mat(DVR,nio=out_unit,nbcol=5,info='DVR')
      Quadrature%x = reshape(x,shape=[1,nq])

      allocate(d0gb(nq,nb))
      allocate(Quadrature%w(nq))
      CALL TabGB_HermiteP0n_QDUtil(d0gb,x,gauss=.TRUE.,renorm=.TRUE.)

      ! weight
      DO iq=1,nq
        ib_max = maxloc(abs(d0gb(iq,:)),dim=1)
        Quadrature%w(iq) = (DVR(ib_max,iq)/d0gb(iq,ib_max))**2
      END DO
    CASE default
      STOP 'ERROR in Init_Quadrature_QDUtil: no default quadrature'
    END SELECT

    !CALL Weight_OF_grid_QDUtil(Quadrature%w,d0gb,nb,nq)

    CALL Check_Overlap_QDUtil(Quadrature,d0gb,nio=out_unit)

    IF (allocated(x))    deallocate(x)
    IF (allocated(Xmat)) deallocate(Xmat)
    IF (allocated(DVR))  deallocate(DVR)
    IF (allocated(d0gb)) deallocate(d0gb)

    !---------------------------------------------------------------------
    IF (debug) THEN
      CALL Write_Quadrature_QDUtil(Quadrature)
      write(out_unit,*) 'END ',name_sub
    END IF
    !---------------------------------------------------------------------
  END SUBROUTINE Init_Quadrature_QDUtil
  SUBROUTINE dealloc_Quadrature_QDUtil(Quadrature)
    IMPLICIT NONE

    TYPE (Quadrature_t), intent(inout)        :: Quadrature


    IF (allocated(Quadrature%name)) deallocate(Quadrature%name)
    IF (allocated(Quadrature%x))    deallocate(Quadrature%x)
    IF (allocated(Quadrature%w))    deallocate(Quadrature%w)

    Quadrature%Sii = -HUGE(ONE)
    Quadrature%Sij = -HUGE(ONE)

  END SUBROUTINE dealloc_Quadrature_QDUtil
  SUBROUTINE Check_Overlap_QDUtil(Quadrature,d0gb,nio)
    USE QDUtil_RW_MatVec_m
    USE QDUtil_HermiteP_m
    IMPLICIT NONE

    TYPE (Quadrature_t), intent(inout)        :: Quadrature
    real (kind=Rkind),   intent(in)           :: d0gb(:,:)
    integer,             intent(in)           :: nio

    integer :: ib,jb,ib_max,iq,nb,nq
    real (kind=Rkind), allocatable   :: d0bgw(:,:)
    real (kind=Rkind), allocatable   :: S(:,:)
    real (kind=Rkind) :: Sii,Sij


    !---------------------------------------------------------------------
    logical,parameter :: debug= .FALSE.
    !logical,parameter :: debug= .TRUE.
    character (len=*), parameter :: name_sub='Init_Quadrature_QDUtil'
    !---------------------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*)
      write(out_unit,*) 'BEGINNING ',name_sub
      write(out_unit,*)
    END IF
    !---------------------------------------------------------------------

    nq = size(d0gb,dim=1)
    nb = size(d0gb,dim=2)
    allocate(d0bgw(nb,nq))
    DO ib=1,nb
      d0bgw(ib,:) = d0gb(:,ib) * Quadrature%w(:)
    END DO

    S = matmul(d0bgw,d0gb)
    Sii = ZERO
    Sij = ZERO
    DO ib=1,nb
      Sii = max(Sii,abs(S(ib,ib)-ONE))
      Sij = max(Sij,maxval(abs(S(1:ib-1,ib))))
      Sij = max(Sij,maxval(abs(S(ib+1:nb,ib))))
    END DO
    Quadrature%Sii = Sii
    Quadrature%Sij = Sij
    IF (print_level == 1 ) write(nio,*) 'Sii,Sij',Quadrature%Sii,Quadrature%Sij

    IF (debug .OR. print_level == 2 .OR. Sii > ONETENTH**6 .OR. Sij > ONETENTH**6 ) THEN
      write(nio,*)
      write(nio,*) 'Sii,Sij',Quadrature%Sii,Quadrature%Sij
      write(nio,*)
      CALL write_Mat(S,nio=nio,nbcol=5,info='S')
    END IF

    IF (allocated(S)) deallocate(S)
    IF (allocated(d0bgw)) deallocate(d0bgw)

    !---------------------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*) 'END ',name_sub
    END IF
    !---------------------------------------------------------------------
  END SUBROUTINE Check_Overlap_QDUtil
  SUBROUTINE Write_Quadrature_QDUtil(Quadrature,nio,info)
    USE QDUtil_String_m
    IMPLICIT NONE
    TYPE (Quadrature_t), intent(in)           :: Quadrature
    integer,             intent(in), optional :: nio
    character (len=*),   intent(in), optional :: info

    integer :: i,nio_loc

    nio_loc = out_unit
    IF (present(nio)) nio_loc = nio

    write(nio_loc,'(a)') '======================================================================='
    IF (present(info)) write(nio_loc,'(a)') info

    IF (allocated(Quadrature%name)) THEN 
      write(nio_loc,'(2a)') '== Quadrature for basis set: ',Quadrature%name
    ELSE
      write(nio_loc,'(a)') '== Quadrature for basis set: no name yet'
    END IF
    IF (allocated(Quadrature%x) .AND. allocated(Quadrature%w)) THEN 
      write(nio_loc,*) 'nq: ',size(Quadrature%w)
    ELSE
      write(nio_loc,*) 'nq: not initialized'
    END IF
    IF (Quadrature%Sii /= -HUGE(ONE) .AND. Quadrature%Sii /= -HUGE(ONE)) THEN
      write(nio_loc,*) 'Sii,Sij (errors on the overlap)',Quadrature%Sii,Quadrature%Sij
    END IF

    IF (allocated(Quadrature%x)) THEN
      DO i=1,size(Quadrature%x,dim=1)
        write(nio_loc,*) 'x(' // TO_string(i) // ',:): ',(Quadrature%x(i,:))
      END DO
    ELSE
      write(nio_loc,'(a)') 'Quadrature%x is not allocated'
    END IF
    IF (allocated(Quadrature%w)) THEN
        write(nio_loc,*) 'w(:):   ',Quadrature%w
    ELSE
      write(nio_loc,'(a)') 'Quadrature%w is not allocated'
    END IF
    write(nio_loc,'(a)') '======================================================================='

  END SUBROUTINE Write_Quadrature_QDUtil


  SUBROUTINE Weight_OF_grid_QDUtil(w,d0RGB,nb,nq)
    USE QDUtil_Matrix_m
    IMPLICIT NONE
    integer,           intent(in)    :: nb,nq
    real (kind=Rkind), intent(inout) :: w(nq)
    real (kind=Rkind), intent(in)    :: d0RGB(nq,nb)

    !---------- working variables ----------------------------------------
    real (kind=Rkind) :: A(nb**2,nq)
    real (kind=Rkind) :: B(nb**2)

    real (kind=Rkind) :: AtA(nq,nq)
    real (kind=Rkind) :: AtB(nq)
    integer           ::  i,j,ij

    !---------------------------------------------------------------------
    logical,parameter :: debug= .FALSE.
    !logical,parameter :: debug= .TRUE.
    character (len=*), parameter :: name_sub='Weight_OF_grid_QDUtil'
    !---------------------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*)
      write(out_unit,*) 'BEGINNING ',name_sub
      write(out_unit,*)
    END IF
    !---------------------------------------------------------------------

    ! matrices of the linear (rectangular) system A.W=B
    ij = 0
    B(:) = ZERO
    DO i=1,nb
    DO j=1,nb
      ij = ij + 1
      IF (i == j) B(ij) = ONE
      A(ij,:) = d0RGB(:,i)*d0RGB(:,j)
    END DO
    END DO

    ! matrices of the linear (square nq*nq) system AtA.W=AtB
    AtA(:,:) = matmul(transpose(A),A)
    AtB(:)   = matmul(transpose(A),B)

    ! Solve the linear system AtA.W=AtB
    W = LinearSys_Solve(AtA,AtB)

    !---------------------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*) 'w',w
      write(out_unit,*) 'END ',name_sub
    END IF
    !---------------------------------------------------------------------
  END SUBROUTINE Weight_OF_grid_QDUtil
  SUBROUTINE Test_Quadrature_QDUtil()
    USE QDUtil_Test_m
    USE QDUtil_NumParameters_m
    USE QDUtil_String_m
    IMPLICIT NONE

    TYPE (test_t)                    :: test_var
    logical                          :: res_test
    real (kind=Rkind), parameter     :: ZeroTresh    = ONETENTH**10
    integer                          :: iVal,iExaVal
    real (kind=Rkind)                :: Val,ExaVal

    TYPE (Quadrature_t) :: xw
    integer :: nq

    !----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub='Test_Quadrature_QDUtil'
    logical, parameter :: debug = .FALSE.
    !logical, parameter :: debug = .TRUE.
    !-----------------------------------------------------------

    CALL set_print_level(prtlev=1,force=.TRUE.)
    !-----------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*) 'BEGINNING ',name_sub
    END IF
    flush(out_unit)
    !-----------------------------------------------------------
    CALL Initialize_Test(test_var,test_name='Quadrature')

    ! sine quadrature test
    nq=128
    CALL Init_Quadrature_QDUtil(xw,nq=nq,type_name='fourier')
    res_test = (xw%Sii < ZeroTresh .AND. xw%Sij < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info=('Fourier quadrature, Overlap check. nq=' // TO_string(nq)))
    IF (.NOT. res_test .OR. debug) THEN
      CALL Write_Quadrature_QDUtil(xw,nio=test_var%test_log_file_unit)
    END IF
    CALL dealloc_Quadrature_QDUtil(xw)

    ! sine quadrature test
    nq=10
    CALL Init_Quadrature_QDUtil(xw,nq=nq,type_name='sine')
    res_test = (xw%Sii < ZeroTresh .AND. xw%Sij < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info=('Sine quadrature, Overlap check. nq=' // TO_string(nq)))
    IF (.NOT. res_test .OR. debug) THEN
      CALL Write_Quadrature_QDUtil(xw,nio=test_var%test_log_file_unit)
    END IF
    CALL dealloc_Quadrature_QDUtil(xw)

    ! BoxAB quadrature test
    nq=10
    CALL Init_Quadrature_QDUtil(xw,nq=nq,A=-ONE,B=ONE,type_name='BoxAB')
    res_test = (xw%Sii < ZeroTresh .AND. xw%Sij < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info=('BoxAB quadrature, Overlap check. nq=' // TO_string(nq)))
    IF (.NOT. res_test .OR. debug) THEN
      CALL Write_Quadrature_QDUtil(xw,nio=test_var%test_log_file_unit)
    END IF
    CALL dealloc_Quadrature_QDUtil(xw)

    ! HO quadrature test
    DO nq=1,5
    CALL Init_Quadrature_QDUtil(xw,nq=nq,type_name='HO')
    res_test = (xw%Sii < ZeroTresh .AND. xw%Sij < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info=('HO quadrature, Overlap check. nq=' // TO_string(nq)))
    IF (.NOT. res_test .OR. debug) THEN
      CALL Write_Quadrature_QDUtil(xw,nio=test_var%test_log_file_unit)
    END IF
    CALL dealloc_Quadrature_QDUtil(xw)
    END DO

    nq = 65
    CALL Init_Quadrature_QDUtil(xw,nq=nq,type_name='HO')
    res_test = (xw%Sii < ZeroTresh .AND. xw%Sij < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info=('HO quadrature, Overlap check. nq=' // TO_string(nq)))
    IF (.NOT. res_test .OR. debug) THEN
      CALL Write_Quadrature_QDUtil(xw,nio=test_var%test_log_file_unit)
    END IF
    CALL dealloc_Quadrature_QDUtil(xw)

    CALL Finalize_Test(test_var)

    !-----------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*) 'END ',name_sub
    END IF
    flush(out_unit)
    !-----------------------------------------------------------

  END SUBROUTINE Test_Quadrature_QDUtil
END MODULE QDUtil_Quadrature_m