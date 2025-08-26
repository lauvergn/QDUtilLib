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
MODULE QDUtil_Sine_m
  USE QDUtil_NumParameters_m
  IMPLICIT NONE

  PRIVATE

  PUBLIC :: TabB_Sine_QDUtil,TabGB_Sine_QDUtil

CONTAINS
  SUBROUTINE TabGB_Sine_QDUtil(d0GB,x,ReNorm)
    IMPLICIT NONE

    real (kind=Rkind),   intent(inout)        :: d0GB(:,:)
    real (kind=Rkind),   intent(in)           :: x(:)
    logical,             intent(in)           :: ReNorm

    integer           :: nb,nq
    integer           :: iq
    real (kind=Rkind),   allocatable          :: Rnorm(:)

    nq = size(d0GB,dim=1)
    nb = size(d0GB,dim=2)

    IF (nb < 1) THEN
      write(out_unit,*) 'ERROR in TabGB_Sine_QDUtil:'
      write(out_unit,*) 'nb < 1',nb
      STOP 'ERROR in TabGB_Sine_QDUtil: nb<1'
    END IF
    IF (nq /= size(x)) THEN
      write(out_unit,*) 'ERROR in TabGB_Sine_QDUtil:'
      write(out_unit,*) 'size(x) and nq differ',size(x),nq
      STOP 'ERROR in TabGB_Sine_QDUtil: size(x) and nq differ'
    END IF

    DO iq=1,nq
      CALL TabB_Sine_QDUtil(d0gb(iq,:),x(iq),ReNorm)
    END DO
    IF (nb == nq .AND. ReNorm)  d0gb(:,nq) = d0gb(:,nq) / sqrt(TWO)

  END SUBROUTINE TabGB_Sine_QDUtil
  SUBROUTINE TabB_Sine_QDUtil(Sine,x,ReNorm)
    IMPLICIT NONE

    real (kind=Rkind),   intent(inout)        :: Sine(:)

    real (kind=Rkind),   intent(in)           :: x
    logical,             intent(in)           :: ReNorm

    integer           :: ib,nb
    logical           :: ReNorm_loc

    nb = size(Sine)
    IF ( nb < 1 ) RETURN

    DO ib=1,nb
      Sine(ib) = Sine_QDutil(x,ib,ReNorm)
    END DO

  END SUBROUTINE TabB_Sine_QDUtil
  FUNCTION Sine_QDutil(x,ib,ReNorm) RESULT(f)
    IMPLICIT NONE

    real(kind=Rkind)    :: f

    real (kind=Rkind),   intent(in)   :: x
    integer,             intent(in)   :: ib
    logical,             intent(in)   :: ReNorm

    !---------------------------------------------------------------------
    real(kind=Rkind) :: xx
    real(kind=Rkind), parameter :: Rnorm = ONE/sqrt(pi*HALF)
    !---------------------------------------------------------------------

    xx = mod(x*real(ib,kind=Rkind),pi+pi)
    f = sin(xx)
    IF (ReNorm) THEN
      f = f * Rnorm
    END IF

  END function Sine_QDutil

END MODULE QDUtil_Sine_m
