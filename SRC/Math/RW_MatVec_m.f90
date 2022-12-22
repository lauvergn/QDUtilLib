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
MODULE QDUtil_RW_MatVec_m
  USE QDUtil_NumParameters_m, ONLY : Name_longlen
  IMPLICIT NONE

  PRIVATE

  INTERFACE Write_VecMat
    MODULE PROCEDURE QDUtil_Write_real32Mat,QDUtil_Write_cplx32Mat,QDUtil_Write_real32Mat_string
    MODULE PROCEDURE QDUtil_Write_real64Mat,QDUtil_Write_cplx64Mat,QDUtil_Write_real64Mat_string
    MODULE PROCEDURE QDUtil_Write_real128Mat,QDUtil_Write_cplx128Mat,QDUtil_Write_real128Mat_string
    MODULE PROCEDURE QDUtil_Write_real32Vec,QDUtil_Write_cplx32Vec
    MODULE PROCEDURE QDUtil_Write_real64Vec,QDUtil_Write_cplx64Vec
    MODULE PROCEDURE QDUtil_Write_real128Vec,QDUtil_Write_cplx128Vec
  END INTERFACE
  INTERFACE Write_Mat
    MODULE PROCEDURE QDUtil_Write_real32Mat,QDUtil_Write_cplx32Mat,QDUtil_Write_real32Mat_string
    MODULE PROCEDURE QDUtil_Write_real64Mat,QDUtil_Write_cplx64Mat,QDUtil_Write_real64Mat_string
    MODULE PROCEDURE QDUtil_Write_real128Mat,QDUtil_Write_cplx128Mat,QDUtil_Write_real128Mat_string
  END INTERFACE
  INTERFACE Write_Vec
    MODULE PROCEDURE QDUtil_Write_real32Vec,QDUtil_Write_cplx32Vec
    MODULE PROCEDURE QDUtil_Write_real64Vec,QDUtil_Write_cplx64Vec
    MODULE PROCEDURE QDUtil_Write_real128Vec,QDUtil_Write_cplx128Vec
  END INTERFACE
  INTERFACE Read_Mat
    MODULE PROCEDURE QDUtil_Read_real32Mat,QDUtil_Read_cplx32Mat
    MODULE PROCEDURE QDUtil_Read_real64Mat,QDUtil_Read_cplx64Mat
    MODULE PROCEDURE QDUtil_Read_real128Mat,QDUtil_Read_cplx128Mat
  END INTERFACE
  INTERFACE Read_Vec
    MODULE PROCEDURE QDUtil_Read_real32Vec,QDUtil_Read_cplx32Vec
    MODULE PROCEDURE QDUtil_Read_real64Vec,QDUtil_Read_cplx64Vec
    MODULE PROCEDURE QDUtil_Read_real128Vec,QDUtil_Read_cplx128Vec
  END INTERFACE

  PUBLIC :: Write_VecMat, Write_Mat, Write_Vec, Read_Mat, Read_Vec
  PUBLIC :: Test_QDUtil_RW_MatVec

  character (len=Name_longlen) :: RMatIO_format = "f18.10"
  character (len=Name_longlen) :: CMatIO_format = "'(',f15.7,',',f15.7,')'"
  !character (len=Name_longlen) :: CMatIO_format = "'(',f15.7,' +i',f15.7,')'" ! this format does not work while reading

  CONTAINS

  !!@description: Defined a format to write a matrix line 
  !!@param: TODO
  SUBROUTINE QDUtil_Format_OF_Line(wformat,nb_line,max_col,cplx,Rformat,info)
    USE QDUtil_String_m
    USE QDUtil_NumParameters_m, ONLY : RkD,out_unit
    IMPLICIT NONE

    character (len=:), allocatable, intent(inout)  :: wformat
    integer,                        intent(in)     :: nb_line,max_col
    logical,                        intent(in)     :: cplx
    character (len=*), optional,    intent(in)     :: Rformat
    character (len=*), optional,    intent(in)     :: info


    ! local variables
    character (len=:), allocatable :: NMatformat,wformat_loc
    integer                        :: ilen

    !$OMP  CRITICAL (QDUtil_Format_OF_Line_CRIT)

    IF (allocated(wformat)) deallocate(wformat)

    IF (present(info)) THEN
      wformat_loc = '(2x,"' // trim(adjustl(info)) // ' ",'
    ELSE
      wformat_loc = '('
    END IF

    IF (present(Rformat)) THEN
      IF (len_trim(Rformat) > 10) THEN
        write(out_unit,*) ' ERROR in QDUtil_Format_OF_Line'
        write(out_unit,*) ' The format (len_trim) in "Rformat" is too long',len_trim(Rformat)
        write(out_unit,*) ' Rformat: ',Rformat
        STOP
      END IF
        IF (cplx) THEN
          NMatformat = "'('," // trim(adjustl(Rformat)) //           &
                       ",' +i'," // trim(adjustl(Rformat)) // ",')'"
        ELSE
          NMatformat = trim(adjustl(Rformat))
        END IF
    ELSE
      IF (cplx) THEN
        NMatformat = trim(adjustl(CMatIO_format))
      ELSE
        NMatformat = trim(adjustl(RMatIO_format))
      END IF
    END IF

    IF (nb_line > 0) THEN

        !ilen = int(log10(real(nb_line,kind=RkD)))+1
        ! ensure compatible with very small system in test
        ilen = MAX(int(log10(real(nb_line,kind=RkD)))+1,2)

        !write(*,*) 'max_col check:',max_col,ilen

        wformat_loc = wformat_loc // '1x,i' //                       &
                    int_TO_char(ilen) // ',2x,' //                   &
                    int_TO_char(max_col) // '(' //                   &
                    trim(adjustl(NMatformat)) // ',1x))'


    ELSE

        wformat_loc = wformat_loc // int_TO_char(max_col) // '(' //  &
                      trim(adjustl(NMatformat)) // ',1x))'


    END IF
    !write(out_unit,*) 'NMatformat: ',NMatformat
    !write(out_unit,*) 'wformat: ',wformat
    !flush(out_unit)

    wformat = wformat_loc

    deallocate(NMatformat)
    deallocate(wformat_loc)
    !$OMP  END CRITICAL (QDUtil_Format_OF_Line_CRIT)

    !write(out_unit,*) 'format?: ',trim(wformat)
  END SUBROUTINE QDUtil_Format_OF_Line

  !!@description:  write a rectangular real or complex matrix, f(nl,nc),
  !!   with a specific format selected with Format_OF_Line
  !!@param: TODO
  SUBROUTINE QDUtil_Write_real64Mat(f,nio,nbcol1,Rformat,info,iprint)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real64
    USE QDUtil_NumParameters_m, ONLY : out_unit
    IMPLICIT NONE

    integer,                     intent(in) :: nio,nbcol1
    real(kind=real64),           intent(in) :: f(:,:)

    character (len=*), optional, intent(in) :: Rformat
    character (len=*), optional, intent(in) :: info
    integer,           optional, intent(in) :: iprint

    integer         :: nl,nc
    integer         :: i,j,nb,nbblocs,nfin,nbcol
    character (len=:), allocatable  :: wformat

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF

    nl = size(f,dim=1)
    nc = size(f,dim=2)
    !write(out_unit,*) 'nl,nc,nbcol',nl,nc,nbcol
    nbcol = nbcol1
    IF (nbcol > 10) nbcol=10
    nbblocs=int(nc/nbcol)
    IF (nbblocs*nbcol == nc) nbblocs=nbblocs-1

    IF (present(Rformat)) THEN
      IF (present(info)) THEN
        CALL QDUtil_Format_OF_Line(wformat,nl,nbcol,.FALSE.,Rformat,info)
      ELSE
        CALL QDUtil_Format_OF_Line(wformat,nl,nbcol,.FALSE.,Rformat=Rformat)
      END IF
    ELSE
      IF (present(info)) THEN
        CALL QDUtil_Format_OF_Line(wformat,nl,nbcol,.FALSE.,info=info)
      ELSE
        CALL QDUtil_Format_OF_Line(wformat,nl,nbcol,.FALSE.)
      END IF
    END IF

      DO nb=0,nbblocs-1
        DO j=1,nl
          write(nio,wformat) j,(f(j,i+nb*nbcol),i=1,nbcol)
        END DO
        IF (nl > 1 ) write(nio,*)
      END DO
      DO j=1,nl
        nfin=nc-nbcol*nbblocs
        write(nio,wformat) j,(f(j,i+nbcol*nbblocs),i=1,nfin)
      END DO

    deallocate(wformat)

  END SUBROUTINE QDUtil_Write_real64Mat
  SUBROUTINE QDUtil_Write_real64Mat_string(f,string,nbcol1,Rformat,info,iprint)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real64
    USE QDUtil_NumParameters_m, ONLY : out_unit
    USE QDUtil_String_m,        ONLY : TO_string
    IMPLICIT NONE

    integer,                        intent(in)    :: nbcol1
    character (len=:), allocatable, intent(inout) :: string
    real(kind=real64),              intent(in)    :: f(:,:)

    character (len=*), optional,    intent(in)    :: Rformat
    character (len=*), optional,    intent(in)    :: info
    integer,           optional,    intent(in)    :: iprint

    integer         :: nl,nc
    integer         :: i,j,nb,nbblocs,nfin,nbcol
    character (len=:), allocatable :: BeginString
    character (len=:), allocatable :: Rf

    string = ''

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF

    nl = size(f,dim=1)
    nc = size(f,dim=2)
 
    nbcol = nbcol1
    IF (nbcol > 10) nbcol=10
    nbblocs=int(nc/nbcol)
    IF (nbblocs*nbcol == nc) nbblocs=nbblocs-1

    !write(out_unit,*) 'nl,nc,nbcol',nl,nc,nbcol
    !write(out_unit,*) 'string: ',string ; flush(out_unit)

    IF (present(info)) THEN
      BeginString = trim(info) // ' '
    ELSE
      BeginString = ' '
    END IF

    IF (present(Rformat)) THEN
      Rf = trim(Rformat)
    ELSE
      Rf = RMatIO_format
    END IF

    DO nb=0,nbblocs-1
      DO j=1,nl
        string = string // BeginString // TO_string(j)
        DO i=1,nbcol
          string = string // ' ' // TO_string(f(j,i+nb*nbcol),rformat=Rf)
        END DO
        string = string // new_line('a')
      END DO
      IF (nl > 1 ) string = string // new_line('a')
      !write(out_unit,*) 'string: ' // new_line('a'),string ; flush(out_unit)
    END DO

    DO j=1,nl
      nfin=nc-nbcol*nbblocs
      string = string // BeginString // TO_string(j)
      DO i=1,nfin
        string = string // ' ' // TO_string(f(j,i+nbcol*nbblocs),rformat=Rf)
      END DO
      string = string // new_line('a')
    END DO

  END SUBROUTINE QDUtil_Write_real64Mat_string
  !!@description: TODO
  !!@param: TODO
  SUBROUTINE QDUtil_Write_cplx64Mat(f,nio,nbcol1,Rformat,info,iprint)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real64
    USE QDUtil_NumParameters_m, ONLY : out_unit
    IMPLICIT NONE

    integer,                     intent(in) :: nio,nbcol1
    complex(kind=real64),        intent(in) :: f(:,:)

    character (len=*), optional, intent(in) :: Rformat
    character (len=*), optional, intent(in) :: info
    integer,           optional, intent(in) :: iprint


    integer         :: nl,nc
    integer i,j,nb,nbblocs,nfin,nbcol
    character (len=:), allocatable  :: wformat

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF

    nl = size(f,dim=1)
      nc = size(f,dim=2)
      !write(out_unit,*) 'nl,nc,nbcol',nl,nc,nbcol
      nbcol = nbcol1
      IF (nbcol > 10) nbcol=10
      nbblocs=int(nc/nbcol)
      IF (nbblocs*nbcol == nc) nbblocs=nbblocs-1

      IF (present(Rformat)) THEN
        IF (present(info)) THEN
          CALL QDUtil_Format_OF_Line(wformat,nl,nbcol,.TRUE.,Rformat,info)
        ELSE
          CALL QDUtil_Format_OF_Line(wformat,nl,nbcol,.TRUE.,Rformat=Rformat)
        END IF
      ELSE
        IF (present(info)) THEN
          CALL QDUtil_Format_OF_Line(wformat,nl,nbcol,.TRUE.,info=info)
        ELSE
          CALL QDUtil_Format_OF_Line(wformat,nl,nbcol,.TRUE.)
        END IF
      END IF


      DO nb=0,nbblocs-1
        DO j=1,nl
          write(nio,wformat) j,(f(j,i+nb*nbcol),i=1,nbcol)
        END DO
        IF (nl > 1 ) write(nio,*)
      END DO
      DO j=1,nl
        nfin=nc-nbcol*nbblocs
        write(nio,wformat) j,(f(j,i+nbcol*nbblocs),i=1,nfin)
      END DO

      deallocate(wformat)

  END SUBROUTINE QDUtil_Write_cplx64Mat

  !!@description: TODO
  !!@param: TODO
  SUBROUTINE QDUtil_Write_real64Vec(l,nio,nbcol1,Rformat,info,iprint)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real64
    USE QDUtil_NumParameters_m, ONLY : out_unit
    IMPLICIT NONE

    integer,                     intent(in) :: nio,nbcol1
    real(kind=real64),            intent(in) :: l(:)

    character (len=*), optional, intent(in) :: Rformat
    character (len=*), optional, intent(in) :: info
    integer,           optional, intent(in) :: iprint


    integer           :: n,i,nb,nbblocs,nfin,nbcol
    character (len=:), allocatable  :: wformat

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF

    n = size(l)
       !write(out_unit,*) 'n,nbcol',n,nbcol
       nbcol = nbcol1
       IF (nbcol > 10) nbcol=10
       nbblocs=int(n/nbcol)
       IF (nbblocs*nbcol == n) nbblocs=nbblocs-1


       IF (present(Rformat)) THEN
         IF (present(info)) THEN
           CALL QDUtil_Format_OF_Line(wformat,0,nbcol,.FALSE.,Rformat,info)
         ELSE
           CALL QDUtil_Format_OF_Line(wformat,0,nbcol,.FALSE.,Rformat=Rformat)
         END IF
       ELSE
         IF (present(info)) THEN
           CALL QDUtil_Format_OF_Line(wformat,0,nbcol,.FALSE.,info=info)
         ELSE
           CALL QDUtil_Format_OF_Line(wformat,0,nbcol,.FALSE.)
         END IF
       END IF

       DO nb=0,nbblocs-1
         write(nio,wformat) (l(i+nb*nbcol),i=1,nbcol)
       END DO
       nfin=n-nbcol*nbblocs
       write(nio,wformat) (l(i+nbcol*nbblocs),i=1,nfin)

       deallocate(wformat)
  END SUBROUTINE QDUtil_Write_real64Vec

  !!@description: TODO
  !!@param: TODO
  SUBROUTINE QDUtil_Write_cplx64Vec(l,nio,nbcol1,Rformat,info,iprint)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real64
    USE QDUtil_NumParameters_m, ONLY : out_unit
    IMPLICIT NONE

    integer,                     intent(in) :: nio,nbcol1
    complex(kind=real64),         intent(in) :: l(:)

    character (len=*), optional, intent(in) :: Rformat
    character (len=*), optional, intent(in) :: info
    integer,           optional, intent(in) :: iprint

    integer           :: n,i,nb,nbblocs,nfin,nbcol
    character (len=:), allocatable  :: wformat

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF

      n = size(l)
      !write(out_unit,*) 'n,nbcol',n,nbcol
      nbcol = nbcol1
      IF (nbcol > 10) nbcol=10
      nbblocs=int(n/nbcol)
      IF (nbblocs*nbcol == n) nbblocs=nbblocs-1

      IF (present(Rformat)) THEN
        IF (present(info)) THEN
          CALL QDUtil_Format_OF_Line(wformat,0,nbcol,.TRUE.,Rformat,info)
        ELSE
          CALL QDUtil_Format_OF_Line(wformat,0,nbcol,.TRUE.,Rformat=Rformat)
        END IF
      ELSE
        IF (present(info)) THEN
          CALL QDUtil_Format_OF_Line(wformat,0,nbcol,.TRUE.,info=info)
        ELSE
          CALL QDUtil_Format_OF_Line(wformat,0,nbcol,.TRUE.)
        END IF
      END IF

      DO nb=0,nbblocs-1
        write(nio,wformat) (l(i+nb*nbcol),i=1,nbcol)
      END DO
      nfin=n-nbcol*nbblocs
      write(nio,wformat) (l(i+nbcol*nbblocs),i=1,nfin)

      deallocate(wformat)
  END SUBROUTINE QDUtil_Write_cplx64Vec

  SUBROUTINE QDUtil_Read_real64Mat(f,nio,nbcol,err)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real64
    USE QDUtil_NumParameters_m, ONLY : out_unit
    IMPLICIT NONE

    integer,          intent(in)    :: nio,nbcol
     integer,          intent(inout) :: err
     real(kind=real64), intent(inout) :: f(:,:)

     integer i,j,jj,nb,nbblocs,nfin,nl,nc

     nl = size(f,dim=1)
     nc = size(f,dim=2)
     !write(out_unit,*) 'nl,nc,nbcol',nl,nc,nbcol


     nbblocs=int(nc/nbcol)

     IF (nbblocs*nbcol == nc) nbblocs=nbblocs-1
     err = 0

     !write(out_unit,*) 'nl,nc,nbcol,nbblocs',nl,nc,nbcol,nbblocs


     DO nb=0,nbblocs-1

         DO j=1,nl
           read(nio,*,IOSTAT=err) jj,(f(j,i+nb*nbcol),i=1,nbcol)
           IF (err /= 0) EXIT
         END DO

         IF (err /= 0) EXIT

         IF (nl > 1) read(nio,*,IOSTAT=err)
         IF (err /= 0) EXIT

     END DO

     nfin=nc-nbcol*nbblocs
     IF (err == 0) THEN
       DO j=1,nl
         read(nio,*,IOSTAT=err) jj,(f(j,i+nbcol*nbblocs),i=1,nfin)
         !write(out_unit,*) err,jj,(f(j,i+nbcol*nbblocs),i=1,nfin)
         IF (err /= 0) EXIT
       END DO
     END IF

     IF (err /= 0) THEN
       CALL QDUtil_Write_real64Mat(f,out_unit,nbcol)
       write(out_unit,*) ' ERROR in QDUtil_Read_real64Mat'
       write(out_unit,*) '  while reading a matrix'
       write(out_unit,*) '  end of file or end of record'
       write(out_unit,*) '  The matrix paramters: nl,nc,nbcol',nl,nc,nbcol
       write(out_unit,*) '  Internal paramters: nbblocs,nfin',nbblocs,nfin
       write(out_unit,*) ' Check your data !!'
     END IF

  END SUBROUTINE QDUtil_Read_real64Mat
  SUBROUTINE QDUtil_Read_cplx64Mat(f,nio,nbcol,err)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real64
    USE QDUtil_NumParameters_m, ONLY : out_unit
    IMPLICIT NONE

    integer,             intent(in)    :: nio,nbcol
    complex(kind=real64), intent(inout) :: f(:,:)
    integer,             intent(inout) :: err

     integer i,j,jj,nb,nbblocs,nfin,nl,nc

     nl = size(f,dim=1)
     nc = size(f,dim=2)
     !write(out_unit,*) 'nl,nc,nbcol',nl,nc,nbcol


     nbblocs=int(nc/nbcol)
     err = 0
     IF (nbblocs*nbcol == nc) nbblocs=nbblocs-1

     DO nb=0,nbblocs-1

         DO j=1,nl
           read(nio,*,IOSTAT=err) jj,(f(j,i+nb*nbcol),i=1,nbcol)
           IF (err /= 0) EXIT
         END DO

         IF (err /= 0) EXIT

         IF (nl > 1) read(nio,*,IOSTAT=err)
         IF (err /= 0) EXIT

     END DO

     IF (err == 0) THEN
       DO j=1,nl
         nfin=nc-nbcol*nbblocs
         read(nio,*,IOSTAT=err) jj,(f(j,i+nbcol*nbblocs),i=1,nfin)
         IF (err /= 0) EXIT
       END DO
     END IF

     IF (err /= 0) THEN
       CALL QDUtil_Write_cplx64Mat(f,out_unit,nbcol)
       write(out_unit,*) ' ERROR in QDUtil_Read_cplx64Mat'
       write(out_unit,*) '  while reading a matrix'
       write(out_unit,*) '  end of file or end of record'
       write(out_unit,*) '  The matrix paramters: nl,nc,nbcol',nl,nc,nbcol
       write(out_unit,*) ' Check your data !!'
     END IF

  END SUBROUTINE QDUtil_Read_cplx64Mat

  !================================================================
  ! ++    read a vector in line
  !================================================================
  SUBROUTINE QDUtil_Read_real64Vec(l,nio,nbcol,err)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real64
    USE QDUtil_NumParameters_m, ONLY : out_unit
    IMPLICIT NONE

    integer, intent(in)                :: nio,nbcol
     real(kind=real64), intent(inout)    :: l(:)
     integer, intent(inout)             :: err

     integer :: n,i,nb,nbblocs,nfin

     n = size(l,dim=1)
     nbblocs=int(n/nbcol)
     err = 0


     IF (nbblocs*nbcol == n) nbblocs=nbblocs-1

     DO nb=0,nbblocs-1
       read(nio,*,IOSTAT=err) (l(i+nb*nbcol),i=1,nbcol)
       IF (err /= 0) EXIT
     END DO

     nfin=n-nbcol*nbblocs
     read(nio,*,IOSTAT=err) (l(i+nbcol*nbblocs),i=1,nfin)

     IF (err /= 0) THEN
       write(out_unit,*) ' ERROR in QDUtil_Read_real64Vec'
       write(out_unit,*) '  while reading a vector'
       write(out_unit,*) '  end of file or end of record'
       write(out_unit,*) '  The vector paramters: n,nbcol',n,nbcol
       write(out_unit,*) ' Check your data !!'
     END IF

  END SUBROUTINE QDUtil_Read_real64Vec
  SUBROUTINE QDUtil_Read_cplx64Vec(l,nio,nbcol,err)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real64
    USE QDUtil_NumParameters_m, ONLY : out_unit
    IMPLICIT NONE

    integer, intent(in)                 :: nio,nbcol
    complex(kind=real64), intent(inout) :: l(:)
    integer, intent(inout)              :: err

     integer :: n,i,nb,nbblocs,nfin

     n = size(l,dim=1)
     nbblocs=int(n/nbcol)
     err = 0

     IF (nbblocs*nbcol == n) nbblocs=nbblocs-1

     DO nb=0,nbblocs-1
       read(nio,*,IOSTAT=err) (l(i+nb*nbcol),i=1,nbcol)
       IF (err /= 0) EXIT
     END DO

     nfin=n-nbcol*nbblocs
     read(nio,*,IOSTAT=err) (l(i+nbcol*nbblocs),i=1,nfin)

     IF (err /= 0) THEN
       write(out_unit,*) ' ERROR in QDUtil_Read_cplx64Vec'
       write(out_unit,*) '  while reading a vector'
       write(out_unit,*) '  end of file or end of record'
       write(out_unit,*) '  The vector paramters: n,nbcol',n,nbcol
       write(out_unit,*) ' Check your data !!'
     END IF

  END SUBROUTINE QDUtil_Read_cplx64Vec


  SUBROUTINE QDUtil_Write_real32Mat(f,nio,nbcol1,Rformat,info,iprint)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real32
    USE QDUtil_NumParameters_m, ONLY : out_unit
    IMPLICIT NONE

    integer,                     intent(in) :: nio,nbcol1
    real(kind=real32),           intent(in) :: f(:,:)

    character (len=*), optional, intent(in) :: Rformat
    character (len=*), optional, intent(in) :: info
    integer,           optional, intent(in) :: iprint

    integer         :: nl,nc
    integer         :: i,j,nb,nbblocs,nfin,nbcol
    character (len=:), allocatable  :: wformat

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF

    nl = size(f,dim=1)
    nc = size(f,dim=2)
    !write(out_unit,*) 'nl,nc,nbcol',nl,nc,nbcol
    nbcol = nbcol1
    IF (nbcol > 10) nbcol=10
    nbblocs=int(nc/nbcol)
    IF (nbblocs*nbcol == nc) nbblocs=nbblocs-1

    IF (present(Rformat)) THEN
      IF (present(info)) THEN
        CALL QDUtil_Format_OF_Line(wformat,nl,nbcol,.FALSE.,Rformat,info)
      ELSE
        CALL QDUtil_Format_OF_Line(wformat,nl,nbcol,.FALSE.,Rformat=Rformat)
      END IF
    ELSE
      IF (present(info)) THEN
        CALL QDUtil_Format_OF_Line(wformat,nl,nbcol,.FALSE.,info=info)
      ELSE
        CALL QDUtil_Format_OF_Line(wformat,nl,nbcol,.FALSE.)
      END IF
    END IF

      DO nb=0,nbblocs-1
        DO j=1,nl
          write(nio,wformat) j,(f(j,i+nb*nbcol),i=1,nbcol)
        END DO
        IF (nl > 1 ) write(nio,*)
      END DO
      DO j=1,nl
        nfin=nc-nbcol*nbblocs
        write(nio,wformat) j,(f(j,i+nbcol*nbblocs),i=1,nfin)
      END DO

    deallocate(wformat)

  END SUBROUTINE QDUtil_Write_real32Mat
  SUBROUTINE QDUtil_Write_real32Mat_string(f,string,nbcol1,Rformat,info,iprint)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real32
    USE QDUtil_NumParameters_m, ONLY : out_unit
    USE QDUtil_String_m,        ONLY : TO_string
    IMPLICIT NONE

    integer,                        intent(in)    :: nbcol1
    character (len=:), allocatable, intent(inout) :: string
    real(kind=real32),              intent(in)    :: f(:,:)

    character (len=*), optional,    intent(in)    :: Rformat
    character (len=*), optional,    intent(in)    :: info
    integer,           optional,    intent(in)    :: iprint

    integer         :: nl,nc
    integer         :: i,j,nb,nbblocs,nfin,nbcol
    character (len=:), allocatable :: BeginString
    character (len=:), allocatable :: Rf

    string = ''

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF

    nl = size(f,dim=1)
    nc = size(f,dim=2)
 
    nbcol = nbcol1
    IF (nbcol > 10) nbcol=10
    nbblocs=int(nc/nbcol)
    IF (nbblocs*nbcol == nc) nbblocs=nbblocs-1

    !write(out_unit,*) 'nl,nc,nbcol',nl,nc,nbcol
    !write(out_unit,*) 'string: ',string ; flush(out_unit)

    IF (present(info)) THEN
      BeginString = trim(info) // ' '
    ELSE
      BeginString = ' '
    END IF

    IF (present(Rformat)) THEN
      Rf = trim(Rformat)
    ELSE
      Rf = RMatIO_format
    END IF

    DO nb=0,nbblocs-1
      DO j=1,nl
        string = string // BeginString // TO_string(j)
        DO i=1,nbcol
          string = string // ' ' // TO_string(f(j,i+nb*nbcol),rformat=Rf)
        END DO
        string = string // new_line('a')
      END DO
      IF (nl > 1 ) string = string // new_line('a')
      !write(out_unit,*) 'string: ' // new_line('a'),string ; flush(out_unit)
    END DO

    DO j=1,nl
      nfin=nc-nbcol*nbblocs
      string = string // BeginString // TO_string(j)
      DO i=1,nfin
        string = string // ' ' // TO_string(f(j,i+nbcol*nbblocs),rformat=Rf)
      END DO
      string = string // new_line('a')
    END DO

  END SUBROUTINE QDUtil_Write_real32Mat_string
  !!@description: TODO
  !!@param: TODO
  SUBROUTINE QDUtil_Write_cplx32Mat(f,nio,nbcol1,Rformat,info,iprint)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real32
    USE QDUtil_NumParameters_m, ONLY : out_unit
    IMPLICIT NONE

    integer,                     intent(in) :: nio,nbcol1
    complex(kind=real32),        intent(in) :: f(:,:)

    character (len=*), optional, intent(in) :: Rformat
    character (len=*), optional, intent(in) :: info
    integer,           optional, intent(in) :: iprint


    integer         :: nl,nc
    integer i,j,nb,nbblocs,nfin,nbcol
    character (len=:), allocatable  :: wformat

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF

    nl = size(f,dim=1)
      nc = size(f,dim=2)
      !write(out_unit,*) 'nl,nc,nbcol',nl,nc,nbcol
      nbcol = nbcol1
      IF (nbcol > 10) nbcol=10
      nbblocs=int(nc/nbcol)
      IF (nbblocs*nbcol == nc) nbblocs=nbblocs-1

      IF (present(Rformat)) THEN
        IF (present(info)) THEN
          CALL QDUtil_Format_OF_Line(wformat,nl,nbcol,.TRUE.,Rformat,info)
        ELSE
          CALL QDUtil_Format_OF_Line(wformat,nl,nbcol,.TRUE.,Rformat=Rformat)
        END IF
      ELSE
        IF (present(info)) THEN
          CALL QDUtil_Format_OF_Line(wformat,nl,nbcol,.TRUE.,info=info)
        ELSE
          CALL QDUtil_Format_OF_Line(wformat,nl,nbcol,.TRUE.)
        END IF
      END IF


      DO nb=0,nbblocs-1
        DO j=1,nl
          write(nio,wformat) j,(f(j,i+nb*nbcol),i=1,nbcol)
        END DO
        IF (nl > 1 ) write(nio,*)
      END DO
      DO j=1,nl
        nfin=nc-nbcol*nbblocs
        write(nio,wformat) j,(f(j,i+nbcol*nbblocs),i=1,nfin)
      END DO

      deallocate(wformat)

  END SUBROUTINE QDUtil_Write_cplx32Mat

  !!@description: TODO
  !!@param: TODO
  SUBROUTINE QDUtil_Write_real32Vec(l,nio,nbcol1,Rformat,info,iprint)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real32
    USE QDUtil_NumParameters_m, ONLY : out_unit
    IMPLICIT NONE

    integer,                     intent(in) :: nio,nbcol1
    real(kind=real32),            intent(in) :: l(:)

    character (len=*), optional, intent(in) :: Rformat
    character (len=*), optional, intent(in) :: info
    integer,           optional, intent(in) :: iprint


    integer           :: n,i,nb,nbblocs,nfin,nbcol
    character (len=:), allocatable  :: wformat

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF

    n = size(l)
       !write(out_unit,*) 'n,nbcol',n,nbcol
       nbcol = nbcol1
       IF (nbcol > 10) nbcol=10
       nbblocs=int(n/nbcol)
       IF (nbblocs*nbcol == n) nbblocs=nbblocs-1


       IF (present(Rformat)) THEN
         IF (present(info)) THEN
           CALL QDUtil_Format_OF_Line(wformat,0,nbcol,.FALSE.,Rformat,info)
         ELSE
           CALL QDUtil_Format_OF_Line(wformat,0,nbcol,.FALSE.,Rformat=Rformat)
         END IF
       ELSE
         IF (present(info)) THEN
           CALL QDUtil_Format_OF_Line(wformat,0,nbcol,.FALSE.,info=info)
         ELSE
           CALL QDUtil_Format_OF_Line(wformat,0,nbcol,.FALSE.)
         END IF
       END IF

       DO nb=0,nbblocs-1
         write(nio,wformat) (l(i+nb*nbcol),i=1,nbcol)
       END DO
       nfin=n-nbcol*nbblocs
       write(nio,wformat) (l(i+nbcol*nbblocs),i=1,nfin)

       deallocate(wformat)
  END SUBROUTINE QDUtil_Write_real32Vec

  !!@description: TODO
  !!@param: TODO
  SUBROUTINE QDUtil_Write_cplx32Vec(l,nio,nbcol1,Rformat,info,iprint)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real32
    USE QDUtil_NumParameters_m, ONLY : out_unit
    IMPLICIT NONE

    integer,                     intent(in) :: nio,nbcol1
    complex(kind=real32),         intent(in) :: l(:)

    character (len=*), optional, intent(in) :: Rformat
    character (len=*), optional, intent(in) :: info
    integer,           optional, intent(in) :: iprint

    integer           :: n,i,nb,nbblocs,nfin,nbcol
    character (len=:), allocatable  :: wformat

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF

      n = size(l)
      !write(out_unit,*) 'n,nbcol',n,nbcol
      nbcol = nbcol1
      IF (nbcol > 10) nbcol=10
      nbblocs=int(n/nbcol)
      IF (nbblocs*nbcol == n) nbblocs=nbblocs-1

      IF (present(Rformat)) THEN
        IF (present(info)) THEN
          CALL QDUtil_Format_OF_Line(wformat,0,nbcol,.TRUE.,Rformat,info)
        ELSE
          CALL QDUtil_Format_OF_Line(wformat,0,nbcol,.TRUE.,Rformat=Rformat)
        END IF
      ELSE
        IF (present(info)) THEN
          CALL QDUtil_Format_OF_Line(wformat,0,nbcol,.TRUE.,info=info)
        ELSE
          CALL QDUtil_Format_OF_Line(wformat,0,nbcol,.TRUE.)
        END IF
      END IF

      DO nb=0,nbblocs-1
        write(nio,wformat) (l(i+nb*nbcol),i=1,nbcol)
      END DO
      nfin=n-nbcol*nbblocs
      write(nio,wformat) (l(i+nbcol*nbblocs),i=1,nfin)

      deallocate(wformat)
  END SUBROUTINE QDUtil_Write_cplx32Vec

  SUBROUTINE QDUtil_Read_real32Mat(f,nio,nbcol,err)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real32
    USE QDUtil_NumParameters_m, ONLY : out_unit
    IMPLICIT NONE

    integer,          intent(in)    :: nio,nbcol
     integer,          intent(inout) :: err
     real(kind=real32), intent(inout) :: f(:,:)

     integer i,j,jj,nb,nbblocs,nfin,nl,nc

     nl = size(f,dim=1)
     nc = size(f,dim=2)
     !write(out_unit,*) 'nl,nc,nbcol',nl,nc,nbcol


     nbblocs=int(nc/nbcol)

     IF (nbblocs*nbcol == nc) nbblocs=nbblocs-1
     err = 0

     !write(out_unit,*) 'nl,nc,nbcol,nbblocs',nl,nc,nbcol,nbblocs


     DO nb=0,nbblocs-1

         DO j=1,nl
           read(nio,*,IOSTAT=err) jj,(f(j,i+nb*nbcol),i=1,nbcol)
           IF (err /= 0) EXIT
         END DO

         IF (err /= 0) EXIT

         IF (nl > 1) read(nio,*,IOSTAT=err)
         IF (err /= 0) EXIT

     END DO

     nfin=nc-nbcol*nbblocs
     IF (err == 0) THEN
       DO j=1,nl
         read(nio,*,IOSTAT=err) jj,(f(j,i+nbcol*nbblocs),i=1,nfin)
         !write(out_unit,*) err,jj,(f(j,i+nbcol*nbblocs),i=1,nfin)
         IF (err /= 0) EXIT
       END DO
     END IF

     IF (err /= 0) THEN
       CALL QDUtil_Write_real32Mat(f,out_unit,nbcol)
       write(out_unit,*) ' ERROR in QDUtil_Read_real32Mat'
       write(out_unit,*) '  while reading a matrix'
       write(out_unit,*) '  end of file or end of record'
       write(out_unit,*) '  The matrix paramters: nl,nc,nbcol',nl,nc,nbcol
       write(out_unit,*) '  Internal paramters: nbblocs,nfin',nbblocs,nfin
       write(out_unit,*) ' Check your data !!'
     END IF

  END SUBROUTINE QDUtil_Read_real32Mat
  SUBROUTINE QDUtil_Read_cplx32Mat(f,nio,nbcol,err)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real32
    USE QDUtil_NumParameters_m, ONLY : out_unit
    IMPLICIT NONE

    integer,             intent(in)    :: nio,nbcol
    complex(kind=real32), intent(inout) :: f(:,:)
    integer,             intent(inout) :: err

     integer i,j,jj,nb,nbblocs,nfin,nl,nc

     nl = size(f,dim=1)
     nc = size(f,dim=2)
     !write(out_unit,*) 'nl,nc,nbcol',nl,nc,nbcol


     nbblocs=int(nc/nbcol)
     err = 0
     IF (nbblocs*nbcol == nc) nbblocs=nbblocs-1

     DO nb=0,nbblocs-1

         DO j=1,nl
           read(nio,*,IOSTAT=err) jj,(f(j,i+nb*nbcol),i=1,nbcol)
           IF (err /= 0) EXIT
         END DO

         IF (err /= 0) EXIT

         IF (nl > 1) read(nio,*,IOSTAT=err)
         IF (err /= 0) EXIT

     END DO

     IF (err == 0) THEN
       DO j=1,nl
         nfin=nc-nbcol*nbblocs
         read(nio,*,IOSTAT=err) jj,(f(j,i+nbcol*nbblocs),i=1,nfin)
         IF (err /= 0) EXIT
       END DO
     END IF

     IF (err /= 0) THEN
       CALL QDUtil_Write_cplx32Mat(f,out_unit,nbcol)
       write(out_unit,*) ' ERROR in QDUtil_Read_cplx32Mat'
       write(out_unit,*) '  while reading a matrix'
       write(out_unit,*) '  end of file or end of record'
       write(out_unit,*) '  The matrix paramters: nl,nc,nbcol',nl,nc,nbcol
       write(out_unit,*) ' Check your data !!'
     END IF

  END SUBROUTINE QDUtil_Read_cplx32Mat

  !================================================================
  ! ++    read a vector in line
  !================================================================
  SUBROUTINE QDUtil_Read_real32Vec(l,nio,nbcol,err)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real32
    USE QDUtil_NumParameters_m, ONLY : out_unit
    IMPLICIT NONE

    integer, intent(in)                :: nio,nbcol
     real(kind=real32), intent(inout)    :: l(:)
     integer, intent(inout)             :: err

     integer :: n,i,nb,nbblocs,nfin

     n = size(l,dim=1)
     nbblocs=int(n/nbcol)
     err = 0


     IF (nbblocs*nbcol == n) nbblocs=nbblocs-1

     DO nb=0,nbblocs-1
       read(nio,*,IOSTAT=err) (l(i+nb*nbcol),i=1,nbcol)
       IF (err /= 0) EXIT
     END DO

     nfin=n-nbcol*nbblocs
     read(nio,*,IOSTAT=err) (l(i+nbcol*nbblocs),i=1,nfin)

     IF (err /= 0) THEN
       write(out_unit,*) ' ERROR in QDUtil_Read_real32Vec'
       write(out_unit,*) '  while reading a vector'
       write(out_unit,*) '  end of file or end of record'
       write(out_unit,*) '  The vector paramters: n,nbcol',n,nbcol
       write(out_unit,*) ' Check your data !!'
     END IF

  END SUBROUTINE QDUtil_Read_real32Vec
  SUBROUTINE QDUtil_Read_cplx32Vec(l,nio,nbcol,err)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real32
    USE QDUtil_NumParameters_m, ONLY : out_unit
    IMPLICIT NONE

    integer, intent(in)                 :: nio,nbcol
    complex(kind=real32), intent(inout) :: l(:)
    integer, intent(inout)              :: err

     integer :: n,i,nb,nbblocs,nfin

     n = size(l,dim=1)
     nbblocs=int(n/nbcol)
     err = 0

     IF (nbblocs*nbcol == n) nbblocs=nbblocs-1

     DO nb=0,nbblocs-1
       read(nio,*,IOSTAT=err) (l(i+nb*nbcol),i=1,nbcol)
       IF (err /= 0) EXIT
     END DO

     nfin=n-nbcol*nbblocs
     read(nio,*,IOSTAT=err) (l(i+nbcol*nbblocs),i=1,nfin)

     IF (err /= 0) THEN
       write(out_unit,*) ' ERROR in QDUtil_Read_cplx32Vec'
       write(out_unit,*) '  while reading a vector'
       write(out_unit,*) '  end of file or end of record'
       write(out_unit,*) '  The vector paramters: n,nbcol',n,nbcol
       write(out_unit,*) ' Check your data !!'
     END IF

  END SUBROUTINE QDUtil_Read_cplx32Vec



  SUBROUTINE QDUtil_Write_real128Mat(f,nio,nbcol1,Rformat,info,iprint)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real128
    USE QDUtil_NumParameters_m, ONLY : out_unit
    IMPLICIT NONE

    integer,                     intent(in) :: nio,nbcol1
    real(kind=real128),           intent(in) :: f(:,:)

    character (len=*), optional, intent(in) :: Rformat
    character (len=*), optional, intent(in) :: info
    integer,           optional, intent(in) :: iprint

    integer         :: nl,nc
    integer         :: i,j,nb,nbblocs,nfin,nbcol
    character (len=:), allocatable  :: wformat

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF

    nl = size(f,dim=1)
    nc = size(f,dim=2)
    !write(out_unit,*) 'nl,nc,nbcol',nl,nc,nbcol
    nbcol = nbcol1
    IF (nbcol > 10) nbcol=10
    nbblocs=int(nc/nbcol)
    IF (nbblocs*nbcol == nc) nbblocs=nbblocs-1

    IF (present(Rformat)) THEN
      IF (present(info)) THEN
        CALL QDUtil_Format_OF_Line(wformat,nl,nbcol,.FALSE.,Rformat,info)
      ELSE
        CALL QDUtil_Format_OF_Line(wformat,nl,nbcol,.FALSE.,Rformat=Rformat)
      END IF
    ELSE
      IF (present(info)) THEN
        CALL QDUtil_Format_OF_Line(wformat,nl,nbcol,.FALSE.,info=info)
      ELSE
        CALL QDUtil_Format_OF_Line(wformat,nl,nbcol,.FALSE.)
      END IF
    END IF

      DO nb=0,nbblocs-1
        DO j=1,nl
          write(nio,wformat) j,(f(j,i+nb*nbcol),i=1,nbcol)
        END DO
        IF (nl > 1 ) write(nio,*)
      END DO
      DO j=1,nl
        nfin=nc-nbcol*nbblocs
        write(nio,wformat) j,(f(j,i+nbcol*nbblocs),i=1,nfin)
      END DO

    deallocate(wformat)

  END SUBROUTINE QDUtil_Write_real128Mat
  SUBROUTINE QDUtil_Write_real128Mat_string(f,string,nbcol1,Rformat,info,iprint)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real128
    USE QDUtil_NumParameters_m, ONLY : out_unit
    USE QDUtil_String_m,        ONLY : TO_string
    IMPLICIT NONE

    integer,                        intent(in)    :: nbcol1
    character (len=:), allocatable, intent(inout) :: string
    real(kind=real128),              intent(in)    :: f(:,:)

    character (len=*), optional,    intent(in)    :: Rformat
    character (len=*), optional,    intent(in)    :: info
    integer,           optional,    intent(in)    :: iprint

    integer         :: nl,nc
    integer         :: i,j,nb,nbblocs,nfin,nbcol
    character (len=:), allocatable :: BeginString
    character (len=:), allocatable :: Rf

    string = ''

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF

    nl = size(f,dim=1)
    nc = size(f,dim=2)
 
    nbcol = nbcol1
    IF (nbcol > 10) nbcol=10
    nbblocs=int(nc/nbcol)
    IF (nbblocs*nbcol == nc) nbblocs=nbblocs-1

    !write(out_unit,*) 'nl,nc,nbcol',nl,nc,nbcol
    !write(out_unit,*) 'string: ',string ; flush(out_unit)

    IF (present(info)) THEN
      BeginString = trim(info) // ' '
    ELSE
      BeginString = ' '
    END IF

    IF (present(Rformat)) THEN
      Rf = trim(Rformat)
    ELSE
      Rf = RMatIO_format
    END IF

    DO nb=0,nbblocs-1
      DO j=1,nl
        string = string // BeginString // TO_string(j)
        DO i=1,nbcol
          string = string // ' ' // TO_string(f(j,i+nb*nbcol),rformat=Rf)
        END DO
        string = string // new_line('a')
      END DO
      IF (nl > 1 ) string = string // new_line('a')
      !write(out_unit,*) 'string: ' // new_line('a'),string ; flush(out_unit)
    END DO

    DO j=1,nl
      nfin=nc-nbcol*nbblocs
      string = string // BeginString // TO_string(j)
      DO i=1,nfin
        string = string // ' ' // TO_string(f(j,i+nbcol*nbblocs),rformat=Rf)
      END DO
      string = string // new_line('a')
    END DO

  END SUBROUTINE QDUtil_Write_real128Mat_string
  !!@description: TODO
  !!@param: TODO
  SUBROUTINE QDUtil_Write_cplx128Mat(f,nio,nbcol1,Rformat,info,iprint)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real128
    USE QDUtil_NumParameters_m, ONLY : out_unit
    IMPLICIT NONE

    integer,                     intent(in) :: nio,nbcol1
    complex(kind=real128),        intent(in) :: f(:,:)

    character (len=*), optional, intent(in) :: Rformat
    character (len=*), optional, intent(in) :: info
    integer,           optional, intent(in) :: iprint


    integer         :: nl,nc
    integer i,j,nb,nbblocs,nfin,nbcol
    character (len=:), allocatable  :: wformat

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF

    nl = size(f,dim=1)
      nc = size(f,dim=2)
      !write(out_unit,*) 'nl,nc,nbcol',nl,nc,nbcol
      nbcol = nbcol1
      IF (nbcol > 10) nbcol=10
      nbblocs=int(nc/nbcol)
      IF (nbblocs*nbcol == nc) nbblocs=nbblocs-1

      IF (present(Rformat)) THEN
        IF (present(info)) THEN
          CALL QDUtil_Format_OF_Line(wformat,nl,nbcol,.TRUE.,Rformat,info)
        ELSE
          CALL QDUtil_Format_OF_Line(wformat,nl,nbcol,.TRUE.,Rformat=Rformat)
        END IF
      ELSE
        IF (present(info)) THEN
          CALL QDUtil_Format_OF_Line(wformat,nl,nbcol,.TRUE.,info=info)
        ELSE
          CALL QDUtil_Format_OF_Line(wformat,nl,nbcol,.TRUE.)
        END IF
      END IF


      DO nb=0,nbblocs-1
        DO j=1,nl
          write(nio,wformat) j,(f(j,i+nb*nbcol),i=1,nbcol)
        END DO
        IF (nl > 1 ) write(nio,*)
      END DO
      DO j=1,nl
        nfin=nc-nbcol*nbblocs
        write(nio,wformat) j,(f(j,i+nbcol*nbblocs),i=1,nfin)
      END DO

      deallocate(wformat)

  END SUBROUTINE QDUtil_Write_cplx128Mat

  !!@description: TODO
  !!@param: TODO
  SUBROUTINE QDUtil_Write_real128Vec(l,nio,nbcol1,Rformat,info,iprint)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real128
    USE QDUtil_NumParameters_m, ONLY : out_unit
    IMPLICIT NONE

    integer,                     intent(in) :: nio,nbcol1
    real(kind=real128),            intent(in) :: l(:)

    character (len=*), optional, intent(in) :: Rformat
    character (len=*), optional, intent(in) :: info
    integer,           optional, intent(in) :: iprint


    integer           :: n,i,nb,nbblocs,nfin,nbcol
    character (len=:), allocatable  :: wformat

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF

    n = size(l)
       !write(out_unit,*) 'n,nbcol',n,nbcol
       nbcol = nbcol1
       IF (nbcol > 10) nbcol=10
       nbblocs=int(n/nbcol)
       IF (nbblocs*nbcol == n) nbblocs=nbblocs-1


       IF (present(Rformat)) THEN
         IF (present(info)) THEN
           CALL QDUtil_Format_OF_Line(wformat,0,nbcol,.FALSE.,Rformat,info)
         ELSE
           CALL QDUtil_Format_OF_Line(wformat,0,nbcol,.FALSE.,Rformat=Rformat)
         END IF
       ELSE
         IF (present(info)) THEN
           CALL QDUtil_Format_OF_Line(wformat,0,nbcol,.FALSE.,info=info)
         ELSE
           CALL QDUtil_Format_OF_Line(wformat,0,nbcol,.FALSE.)
         END IF
       END IF

       DO nb=0,nbblocs-1
         write(nio,wformat) (l(i+nb*nbcol),i=1,nbcol)
       END DO
       nfin=n-nbcol*nbblocs
       write(nio,wformat) (l(i+nbcol*nbblocs),i=1,nfin)

       deallocate(wformat)
  END SUBROUTINE QDUtil_Write_real128Vec

  !!@description: TODO
  !!@param: TODO
  SUBROUTINE QDUtil_Write_cplx128Vec(l,nio,nbcol1,Rformat,info,iprint)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real128
    USE QDUtil_NumParameters_m, ONLY : out_unit
    IMPLICIT NONE

    integer,                     intent(in) :: nio,nbcol1
    complex(kind=real128),         intent(in) :: l(:)

    character (len=*), optional, intent(in) :: Rformat
    character (len=*), optional, intent(in) :: info
    integer,           optional, intent(in) :: iprint

    integer           :: n,i,nb,nbblocs,nfin,nbcol
    character (len=:), allocatable  :: wformat

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF

      n = size(l)
      !write(out_unit,*) 'n,nbcol',n,nbcol
      nbcol = nbcol1
      IF (nbcol > 10) nbcol=10
      nbblocs=int(n/nbcol)
      IF (nbblocs*nbcol == n) nbblocs=nbblocs-1

      IF (present(Rformat)) THEN
        IF (present(info)) THEN
          CALL QDUtil_Format_OF_Line(wformat,0,nbcol,.TRUE.,Rformat,info)
        ELSE
          CALL QDUtil_Format_OF_Line(wformat,0,nbcol,.TRUE.,Rformat=Rformat)
        END IF
      ELSE
        IF (present(info)) THEN
          CALL QDUtil_Format_OF_Line(wformat,0,nbcol,.TRUE.,info=info)
        ELSE
          CALL QDUtil_Format_OF_Line(wformat,0,nbcol,.TRUE.)
        END IF
      END IF

      DO nb=0,nbblocs-1
        write(nio,wformat) (l(i+nb*nbcol),i=1,nbcol)
      END DO
      nfin=n-nbcol*nbblocs
      write(nio,wformat) (l(i+nbcol*nbblocs),i=1,nfin)

      deallocate(wformat)
  END SUBROUTINE QDUtil_Write_cplx128Vec

  SUBROUTINE QDUtil_Read_real128Mat(f,nio,nbcol,err)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real128
    USE QDUtil_NumParameters_m, ONLY : out_unit
    IMPLICIT NONE

    integer,          intent(in)    :: nio,nbcol
     integer,          intent(inout) :: err
     real(kind=real128), intent(inout) :: f(:,:)

     integer i,j,jj,nb,nbblocs,nfin,nl,nc

     nl = size(f,dim=1)
     nc = size(f,dim=2)
     !write(out_unit,*) 'nl,nc,nbcol',nl,nc,nbcol


     nbblocs=int(nc/nbcol)

     IF (nbblocs*nbcol == nc) nbblocs=nbblocs-1
     err = 0

     !write(out_unit,*) 'nl,nc,nbcol,nbblocs',nl,nc,nbcol,nbblocs


     DO nb=0,nbblocs-1

         DO j=1,nl
           read(nio,*,IOSTAT=err) jj,(f(j,i+nb*nbcol),i=1,nbcol)
           IF (err /= 0) EXIT
         END DO

         IF (err /= 0) EXIT

         IF (nl > 1) read(nio,*,IOSTAT=err)
         IF (err /= 0) EXIT

     END DO

     nfin=nc-nbcol*nbblocs
     IF (err == 0) THEN
       DO j=1,nl
         read(nio,*,IOSTAT=err) jj,(f(j,i+nbcol*nbblocs),i=1,nfin)
         !write(out_unit,*) err,jj,(f(j,i+nbcol*nbblocs),i=1,nfin)
         IF (err /= 0) EXIT
       END DO
     END IF

     IF (err /= 0) THEN
       CALL QDUtil_Write_real128Mat(f,out_unit,nbcol)
       write(out_unit,*) ' ERROR in QDUtil_Read_real128Mat'
       write(out_unit,*) '  while reading a matrix'
       write(out_unit,*) '  end of file or end of record'
       write(out_unit,*) '  The matrix paramters: nl,nc,nbcol',nl,nc,nbcol
       write(out_unit,*) '  Internal paramters: nbblocs,nfin',nbblocs,nfin
       write(out_unit,*) ' Check your data !!'
     END IF

  END SUBROUTINE QDUtil_Read_real128Mat
  SUBROUTINE QDUtil_Read_cplx128Mat(f,nio,nbcol,err)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real128
    USE QDUtil_NumParameters_m, ONLY : out_unit
    IMPLICIT NONE

    integer,             intent(in)    :: nio,nbcol
    complex(kind=real128), intent(inout) :: f(:,:)
    integer,             intent(inout) :: err

     integer i,j,jj,nb,nbblocs,nfin,nl,nc

     nl = size(f,dim=1)
     nc = size(f,dim=2)
     !write(out_unit,*) 'nl,nc,nbcol',nl,nc,nbcol


     nbblocs=int(nc/nbcol)
     err = 0
     IF (nbblocs*nbcol == nc) nbblocs=nbblocs-1

     DO nb=0,nbblocs-1

         DO j=1,nl
           read(nio,*,IOSTAT=err) jj,(f(j,i+nb*nbcol),i=1,nbcol)
           IF (err /= 0) EXIT
         END DO

         IF (err /= 0) EXIT

         IF (nl > 1) read(nio,*,IOSTAT=err)
         IF (err /= 0) EXIT

     END DO

     IF (err == 0) THEN
       DO j=1,nl
         nfin=nc-nbcol*nbblocs
         read(nio,*,IOSTAT=err) jj,(f(j,i+nbcol*nbblocs),i=1,nfin)
         IF (err /= 0) EXIT
       END DO
     END IF

     IF (err /= 0) THEN
       CALL QDUtil_Write_cplx128Mat(f,out_unit,nbcol)
       write(out_unit,*) ' ERROR in QDUtil_Read_cplx128Mat'
       write(out_unit,*) '  while reading a matrix'
       write(out_unit,*) '  end of file or end of record'
       write(out_unit,*) '  The matrix paramters: nl,nc,nbcol',nl,nc,nbcol
       write(out_unit,*) ' Check your data !!'
     END IF

  END SUBROUTINE QDUtil_Read_cplx128Mat

  !================================================================
  ! ++    read a vector in line
  !================================================================
  SUBROUTINE QDUtil_Read_real128Vec(l,nio,nbcol,err)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real128
    USE QDUtil_NumParameters_m, ONLY : out_unit
    IMPLICIT NONE

    integer, intent(in)                :: nio,nbcol
     real(kind=real128), intent(inout)    :: l(:)
     integer, intent(inout)             :: err

     integer :: n,i,nb,nbblocs,nfin

     n = size(l,dim=1)
     nbblocs=int(n/nbcol)
     err = 0


     IF (nbblocs*nbcol == n) nbblocs=nbblocs-1

     DO nb=0,nbblocs-1
       read(nio,*,IOSTAT=err) (l(i+nb*nbcol),i=1,nbcol)
       IF (err /= 0) EXIT
     END DO

     nfin=n-nbcol*nbblocs
     read(nio,*,IOSTAT=err) (l(i+nbcol*nbblocs),i=1,nfin)

     IF (err /= 0) THEN
       write(out_unit,*) ' ERROR in QDUtil_Read_real128Vec'
       write(out_unit,*) '  while reading a vector'
       write(out_unit,*) '  end of file or end of record'
       write(out_unit,*) '  The vector paramters: n,nbcol',n,nbcol
       write(out_unit,*) ' Check your data !!'
     END IF

  END SUBROUTINE QDUtil_Read_real128Vec
  SUBROUTINE QDUtil_Read_cplx128Vec(l,nio,nbcol,err)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real128
    USE QDUtil_NumParameters_m, ONLY : out_unit
    IMPLICIT NONE

    integer, intent(in)                 :: nio,nbcol
    complex(kind=real128), intent(inout) :: l(:)
    integer, intent(inout)              :: err

     integer :: n,i,nb,nbblocs,nfin

     n = size(l,dim=1)
     nbblocs=int(n/nbcol)
     err = 0

     IF (nbblocs*nbcol == n) nbblocs=nbblocs-1

     DO nb=0,nbblocs-1
       read(nio,*,IOSTAT=err) (l(i+nb*nbcol),i=1,nbcol)
       IF (err /= 0) EXIT
     END DO

     nfin=n-nbcol*nbblocs
     read(nio,*,IOSTAT=err) (l(i+nbcol*nbblocs),i=1,nfin)

     IF (err /= 0) THEN
       write(out_unit,*) ' ERROR in QDUtil_Read_cplx128Vec'
       write(out_unit,*) '  while reading a vector'
       write(out_unit,*) '  end of file or end of record'
       write(out_unit,*) '  The vector paramters: n,nbcol',n,nbcol
       write(out_unit,*) ' Check your data !!'
     END IF

  END SUBROUTINE QDUtil_Read_cplx128Vec


  SUBROUTINE Test_QDUtil_RW_MatVec()
    USE QDUtil_Test_m
    USE QDUtil_NumParameters_m
    IMPLICIT NONE

    TYPE (test_t)                    :: test_var
    logical                          :: res_test
    real (kind=Rkind),   parameter   :: ZeroTresh    = ONETENTH**10

    integer                          :: io,ioerr
    real(kind=Rkind),    allocatable :: R1Mat(:,:),R1Vec(:)
    complex(kind=Rkind), allocatable :: C1Mat(:,:),C1Vec(:)
    real(kind=Rkind),    allocatable :: R2Mat(:,:),R2Vec(:)
    complex(kind=Rkind), allocatable :: C2Mat(:,:),C2Vec(:)
    character (len=:),   allocatable :: string


    ! define the matrices and the vectors
    R1Mat = reshape([ZERO,ONE,TWO,THREE,FOUR,FIVE,                              &
                     ZERO,ONE,TWO,THREE,FOUR,FIVE,                              &
                     ZERO,ONE,TWO,THREE,FOUR,FIVE,                              &
                     ZERO,ONE,TWO,THREE,FOUR,FIVE,                              &
                     ZERO,ONE,TWO,THREE,FOUR,FIVE],shape=[6,5])
    C1Mat = R1Mat + EYE*R1Mat
    allocate(C2Mat(6,5))
    allocate(R2Mat(6,5))

    R1Vec = [ZERO,ONE,TWO,THREE,FOUR,FIVE]
    C1Vec = R1Vec-EYE*R1Vec
    allocate(R2Vec(6))
    allocate(C2Vec(6))

    ! tests
    CALL Initialize_Test(test_var,test_name='RW_MatVec')

    ! Test1 for the real matrix
    open(newunit=io,file='test_io_file.txt')
    CALL Write_Mat(R1Mat,io,4) ; close(io)

    open(newunit=io,file='test_io_file.txt')
    CALL Read_Mat(R2Mat,io,4,ioerr) ; close(io)

    IF (ioerr /= 0) THEN
      write(out_unit,*) 'ERROR while reading R2Mat'
      res_test = .FALSE.
    ELSE
      res_test = all(abs(R1Mat-R2Mat) < ZeroTresh)
    END IF
    CALL Logical_Test(test_var,test1=res_test,info='Read-Write real64Mat')

   ! Test1bis for the real matrix (in a string)
    open(newunit=io,file='test_io_file.txt')
    CALL Write_Mat(R1Mat,string,4)
    write(io,*) string  ; close(io)

    open(newunit=io,file='test_io_file.txt')
    CALL Read_Mat(R2Mat,io,4,ioerr) ; close(io)

    IF (ioerr /= 0) THEN
      write(out_unit,*) 'ERROR while reading R2Mat'
      res_test = .FALSE.
    ELSE
      res_test = all(abs(R1Mat-R2Mat) < ZeroTresh)
    END IF
    CALL Logical_Test(test_var,test1=res_test,info='Read-Write_string real64Mat')

    ! Test2 for the complex matrix
    open(newunit=io,file='test_io_file.txt')
    CALL Write_Mat(C1Mat,io,4) ; close(io)

    open(newunit=io,file='test_io_file.txt')
    CALL Read_Mat(C2Mat,io,4,ioerr) ; close(io)

    IF (ioerr /= 0) THEN
      write(out_unit,*) 'ERROR while reading C2Mat'
      res_test = .FALSE.
    ELSE
      res_test = all(abs(C1Mat-C2Mat) < ZeroTresh)
    END IF
    CALL Logical_Test(test_var,test1=res_test,info='Read-Write cplx64Mat')


    ! Test3 for the real vector
    open(newunit=io,file='test_io_file.txt')
    CALL Write_Vec(R1Vec,io,4) ; close(io)

    open(newunit=io,file='test_io_file.txt')
    CALL Read_Vec(R2Vec,io,4,ioerr) ; close(io)

    IF (ioerr /= 0) THEN
      write(out_unit,*) 'ERROR while reading R2Vec'
      res_test = .FALSE.
    ELSE
      res_test = all(abs(R1Vec-R2Vec) < ZeroTresh)
    END IF
    CALL Logical_Test(test_var,test1=res_test,info='Read-Write real64Vec')

    ! Test4 for the complex vector
    open(newunit=io,file='test_io_file.txt')
    CALL Write_Vec(C1Vec,io,4) ; close(io)

    open(newunit=io,file='test_io_file.txt')
    CALL Read_Vec(C2Vec,io,4,ioerr) ; close(io)

    IF (ioerr /= 0) THEN
      write(out_unit,*) 'ERROR while reading C2Vec'
      res_test = .FALSE.
    ELSE
      res_test = all(abs(C1Vec-C2Vec) < ZeroTresh)
    END IF
    CALL Logical_Test(test_var,test1=res_test,info='Read-Write cplx64Vec')

    CALL Flush_Test(test_var)
    ! finalize the tests
    CALL Finalize_Test(test_var)
  END SUBROUTINE Test_QDUtil_RW_MatVec
END MODULE QDUtil_RW_MatVec_m
