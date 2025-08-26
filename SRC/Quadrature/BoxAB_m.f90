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
MODULE QDUtil_BoxAB_m
  USE QDUtil_NumParameters_m
  IMPLICIT NONE

  PRIVATE

  PUBLIC :: TabB_BoxAB_QDUtil,TabGB_BoxAB_QDUtil

CONTAINS
  SUBROUTINE TabGB_BoxAB_QDUtil(d0GB,x,A,B,ReNorm)
    IMPLICIT NONE

    real (kind=Rkind),   intent(inout)        :: d0GB(:,:)
    real (kind=Rkind),   intent(in)           :: x(:)
    real (kind=Rkind),   intent(in)           :: A,B
    logical,             intent(in)           :: ReNorm

    integer           :: nb,nq
    integer           :: iq
    real (kind=Rkind),   allocatable          :: Rnorm(:)

    nq = size(d0GB,dim=1)
    nb = size(d0GB,dim=2)

    IF (nb < 1) THEN
      write(out_unit,*) 'ERROR in TabGB_BoxAB_QDUtil:'
      write(out_unit,*) 'nb < 1',nb
      STOP 'ERROR in TabGB_BoxAB_QDUtil: nb<1'
    END IF
    IF (nq /= size(x)) THEN
      write(out_unit,*) 'ERROR in TabGB_BoxAB_QDUtil:'
      write(out_unit,*) 'size(x) and nq differ',size(x),nq
      STOP 'ERROR in TabGB_BoxAB_QDUtil: size(x) and nq differ'
    END IF

    DO iq=1,nq
      CALL TabB_BoxAB_QDUtil(d0gb(iq,:),x(iq),A,B,ReNorm)
    END DO
    IF (nb == nq .AND. ReNorm)  d0gb(:,nq) = d0gb(:,nq) / sqrt(TWO)

  END SUBROUTINE TabGB_BoxAB_QDUtil
  SUBROUTINE TabB_BoxAB_QDUtil(BoxAB,x,A,B,ReNorm)
    IMPLICIT NONE

    real (kind=Rkind),   intent(inout)        :: BoxAB(:)
    real (kind=Rkind),   intent(in)           :: x
    real (kind=Rkind),   intent(in)           :: A,B
    logical,             intent(in)           :: ReNorm

    integer           :: ib,nb
    logical           :: ReNorm_loc

    nb = size(BoxAB)
    IF ( nb < 1 ) RETURN

    DO ib=1,nb
      BoxAB(ib) = BoxAB_QDutil(x,ib,A,B,ReNorm)
    END DO

  END SUBROUTINE TabB_BoxAB_QDUtil
  FUNCTION BoxAB_QDutil(x,ib,A,B,ReNorm) RESULT(f)
    IMPLICIT NONE

    real(kind=Rkind)    :: f

    real (kind=Rkind),   intent(in)   :: x
    integer,             intent(in)   :: ib
    real (kind=Rkind),   intent(in)   :: A,B
    logical,             intent(in)   :: ReNorm

    !---------------------------------------------------------------------
    real(kind=Rkind) :: xx
    real(kind=Rkind), parameter :: Rnorm = ONE/sqrt(pi*HALF)
    !---------------------------------------------------------------------

    xx = mod((x-A)/(B-A)*pi*ib,pi+pi)
    f = sin(xx)
    IF (ReNorm) THEN
      f = f * Rnorm*sqrt(pi/(B-A))
    END IF

  END function BoxAB_QDutil

END MODULE QDUtil_BoxAB_m
