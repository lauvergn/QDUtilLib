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
MODULE QDUtil_String_m
IMPLICIT NONE

  PRIVATE

  PUBLIC :: string_uppercase_TO_lowercase,string_lowercase_TO_uppercase,TO_uppercase,TO_lowercase

  PUBLIC :: string_IS_empty

  PUBLIC :: Read_line

  PUBLIC :: int_TO_char,logical_TO_char,real_TO_char
  PUBLIC :: TO_string

  PUBLIC :: Test_QDUtil_String

  INTERFACE string_uppercase_TO_lowercase
    MODULE PROCEDURE QDUtil_string_uppercase_TO_lowercase
  END INTERFACE
  INTERFACE string_lowercase_TO_uppercase
    MODULE PROCEDURE QDUtil_string_lowercase_TO_uppercase
  END INTERFACE
  INTERFACE TO_lowercase
    MODULE PROCEDURE QDUtil_string_TO_lowercase
  END INTERFACE
  INTERFACE TO_uppercase
    MODULE PROCEDURE QDUtil_string_TO_uppercase
  END INTERFACE

  INTERFACE strdup
    MODULE PROCEDURE QDUtil_strdup
  END INTERFACE
  INTERFACE string_IS_empty
    MODULE PROCEDURE QDUtil_string_IS_empty
  END INTERFACE

  INTERFACE TO_string
    MODULE PROCEDURE QDUtil_int32_TO_string,QDUtil_int64_TO_string
    MODULE PROCEDURE QDUtil_logical_TO_string
    MODULE PROCEDURE QDUtil_real32_TO_string,QDUtil_real64_TO_string,QDUtil_real128_TO_string
  END INTERFACE

  INTERFACE int_TO_char
    MODULE PROCEDURE QDUtil_int32_TO_string,QDUtil_int64_TO_string
  END INTERFACE
  INTERFACE logical_TO_char
    MODULE PROCEDURE QDUtil_logical_TO_string
  END INTERFACE
  INTERFACE real_TO_char
    MODULE PROCEDURE QDUtil_real32_TO_string,QDUtil_real64_TO_string,QDUtil_real128_TO_string
  END INTERFACE

  INTERFACE Read_line
    MODULE PROCEDURE QDUtil_Read_line
  END INTERFACE
  


CONTAINS
  PURE FUNCTION QDUtil_string_TO_lowercase(string) RESULT (lstring)
    IMPLICIT NONE

    character (len=*),         intent(in)  :: string
    character (len=len(string))            :: lstring

    integer  :: i,ascii_char

    lstring = string

    DO i=1,len_trim(lstring)
      ascii_char = iachar(lstring(i:i))
      IF (ascii_char >= 65 .AND. ascii_char <= 90) lstring(i:i) = achar(ascii_char+32)
    END DO

  END FUNCTION QDUtil_string_TO_lowercase
  PURE FUNCTION QDUtil_string_TO_uppercase(string) RESULT (ustring)
    IMPLICIT NONE
  
    character (len=*),         intent(in)  :: string
    character (len=len(string))            :: ustring

    integer  :: i,ascii_char
  
    ustring = string

    DO i=1,len_trim(ustring)
      ascii_char = iachar(ustring(i:i))
      IF (ascii_char >= 97 .AND. ascii_char <= 122) ustring(i:i) = achar(ascii_char-32)
    END DO

  END FUNCTION QDUtil_string_TO_uppercase
  !!@description: Change the case of a string (default lowercase)
  !!@param: string: character (len=*)
  !!@param: lower If the variable is present and its value is F,
  !!              the string will be converted into a uppercase string, otherwise,
  !!              it will be convert into a lowercase string.
  SUBROUTINE QDUtil_string_uppercase_TO_lowercase(string,lower)
  IMPLICIT NONE

    character (len=*), intent(inout)  :: string
    logical, optional  :: lower

    IF (present(lower)) THEN
      IF (lower) THEN
        string = QDUtil_string_TO_lowercase(string)
      ELSE
        string = QDUtil_string_TO_uppercase(string)
      END IF
    ELSE
      string = QDUtil_string_TO_lowercase(string)
    END IF

  END SUBROUTINE QDUtil_string_uppercase_TO_lowercase
  !!@description: Change the case of a string to upercase
  !!@param: string: character (len=*)
  SUBROUTINE QDUtil_string_lowercase_TO_uppercase(string)

    character (len=*), intent(inout)  :: string
  
    string = QDUtil_string_TO_uppercase(string)
 
  END SUBROUTINE QDUtil_string_lowercase_TO_uppercase
  PURE FUNCTION QDUtil_strdup(string)
  IMPLICIT NONE

   character (len=*), intent(in)   :: string
   character (len=:), allocatable  :: QDUtil_strdup

   allocate(character(len=len_trim(string)) :: QDUtil_strdup)
   QDUtil_strdup = trim(string)

  END FUNCTION QDUtil_strdup
  PURE FUNCTION QDUtil_logical_TO_string(l)  RESULT(string)

    character (len=:), allocatable  :: string
    logical, intent(in)             :: l

    IF (l) THEN
      string = 'T'
    ELSE
      string = 'F'
    END IF

  END FUNCTION QDUtil_logical_TO_string
  PURE FUNCTION QDUtil_int32_TO_string(i) RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : IkS,RkD
    IMPLICIT NONE

    character (len=:),  allocatable             :: string
    integer (kind=IkS),             intent(in)  :: i


    character (len=:), allocatable  :: name_int
    integer :: clen

    ! first approximated size of name_int
    IF (i == 0) THEN
      clen = 1
    ELSE IF (i < 0) THEN
      clen = int(log10(abs(real(i,kind=RkD))))+2
    ELSE
      clen = int(log10(real(i,kind=RkD)))+1
    END IF

    ! allocate name_int
    allocate(character(len=clen) :: name_int)

    ! write i in name_int
    write(name_int,'(i0)') i

    ! transfert name_int in QDUtil_int_TO_char
    string = trim(adjustl(name_int))

    ! deallocate name_int
    deallocate(name_int)

  END FUNCTION QDUtil_int32_TO_string
  PURE FUNCTION QDUtil_int64_TO_string(i) RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : IkD,RkD
    IMPLICIT NONE

    character (len=:),  allocatable             :: string
    integer (kind=IkD),             intent(in)  :: i


    character (len=:), allocatable  :: name_int
    integer :: clen

    ! first approximated size of name_int
    IF (i == 0) THEN
      clen = 1
    ELSE IF (i < 0) THEN
      clen = int(log10(abs(real(i,kind=RkD))))+2
    ELSE
      clen = int(log10(real(i,kind=RkD)))+1
    END IF

    ! allocate name_int
    allocate(character(len=clen) :: name_int)

    ! write i in name_int
    write(name_int,'(i0)') i

    ! transfert name_int in QDUtil_int_TO_char
    string = trim(adjustl(name_int))

    ! deallocate name_int
    deallocate(name_int)

  END FUNCTION QDUtil_int64_TO_string
  FUNCTION QDUtil_real128_TO_string(r,Rformat) RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : RkQ
    IMPLICIT NONE

    character (len=:), allocatable           :: string

    real (kind=RkQ), intent(in)              :: r
    character (len=*), intent(in), optional  :: Rformat


    integer,                parameter :: Line_len = 256
    character(len=Line_len)           :: name_real
    integer :: clen,i

    !$OMP  CRITICAL (QDUtil_real128_TO_string_CRIT)

    IF (allocated(string)) deallocate(string)


    IF (present(Rformat)) THEN
      write(name_real,'(' // Rformat // ')') r

      clen = len_trim(adjustl(name_real))
      allocate(character(len=clen) :: string)

      string = trim(adjustl(name_real))

    ELSE
      write(name_real,*) r

      clen = len_trim(adjustl(name_real))
      allocate(character(len=clen) :: string)

      string = trim(adjustl(name_real))

      DO i=len(string),2,-1
        IF (string(i:i) == '0') THEN
          string(i:i) = ' '
        ELSE
          EXIT
        END IF
      END DO
      string = trim(adjustl(string))

      !this is add for ifort because ZERO is written as 0.000..0E+000
      i = len(string)
      IF (string(i:i) == '+' .OR. string(i:i) == '-') string = string(1:i-1)
      i = len(string)
      IF (string(i:i) == 'E' .OR. string(i:i) == 'e') string = string(1:i-1)
      i = len(string)
      IF (string(i:i) == 'D' .OR. string(i:i) == 'd') string = string(1:i-1) ! just in case we have 0.000...D+000

      !then, the 0 at the end are removed
      DO i=len(string),2,-1
        IF (string(i:i) == '0') THEN
          string(i:i) = ' '
        ELSE
          EXIT
        END IF
      END DO
      string = trim(adjustl(string))
    END IF

    !$OMP  END CRITICAL (QDUtil_real128_TO_string_CRIT)

  END FUNCTION QDUtil_real128_TO_string
  FUNCTION QDUtil_real64_TO_string(r,Rformat) RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : RkD
    IMPLICIT NONE

    character (len=:), allocatable           :: string

    real (kind=RkD),   intent(in)            :: r
    character (len=*), intent(in), optional  :: Rformat


    integer,                parameter :: Line_len = 256
    character(len=Line_len)           :: name_real
    integer :: clen,i

    !$OMP  CRITICAL (QDUtil_real64_TO_string_CRIT)

    IF (allocated(string)) deallocate(string)


    IF (present(Rformat)) THEN
      write(name_real,'(' // Rformat // ')') r

      clen = len_trim(adjustl(name_real))
      allocate(character(len=clen) :: string)

      string = trim(adjustl(name_real))

    ELSE
      write(name_real,*) r

      clen = len_trim(adjustl(name_real))
      allocate(character(len=clen) :: string)

      string = trim(adjustl(name_real))

      DO i=len(string),2,-1
        IF (string(i:i) == '0') THEN
          string(i:i) = ' '
        ELSE
          EXIT
        END IF
      END DO
      string = trim(adjustl(string))

      !this is add for ifort because ZERO is written as 0.000..0E+000
      i = len(string)
      IF (string(i:i) == '+' .OR. string(i:i) == '-') string = string(1:i-1)
      i = len(string)
      IF (string(i:i) == 'E' .OR. string(i:i) == 'e') string = string(1:i-1)
      i = len(string)
      IF (string(i:i) == 'D' .OR. string(i:i) == 'd') string = string(1:i-1) ! just in case we have 0.000...D+000

      !then, the 0 at the end are removed
      DO i=len(string),2,-1
        IF (string(i:i) == '0') THEN
          string(i:i) = ' '
        ELSE
          EXIT
        END IF
      END DO
      string = trim(adjustl(string))

    END IF

    !$OMP  END CRITICAL (QDUtil_real64_TO_string_CRIT)

  END FUNCTION QDUtil_real64_TO_string
  FUNCTION QDUtil_real32_TO_string(r,Rformat) RESULT(string)
    USE QDUtil_NumParameters_m, ONLY : RkS
    IMPLICIT NONE

    character (len=:), allocatable           :: string

    real (kind=RkS), intent(in)              :: r
    character (len=*), intent(in), optional  :: Rformat


    integer,                parameter :: Line_len = 256
    character(len=Line_len)           :: name_real
    integer :: clen,i

    !$OMP  CRITICAL (QDUtil_real32_TO_string_CRIT)

    IF (allocated(string)) deallocate(string)


    IF (present(Rformat)) THEN
      write(name_real,'(' // Rformat // ')') r

      clen = len_trim(adjustl(name_real))
      allocate(character(len=clen) :: string)

      string = trim(adjustl(name_real))

    ELSE
      write(name_real,*) r

      clen = len_trim(adjustl(name_real))
      allocate(character(len=clen) :: string)

      string = trim(adjustl(name_real))

      DO i=len(string),2,-1
        IF (string(i:i) == '0') THEN
          string(i:i) = ' '
        ELSE
          EXIT
        END IF
      END DO
      string = trim(adjustl(string))

      !this is add for ifort because ZERO is written as 0.000..0E+000
      i = len(string)
      IF (string(i:i) == '+' .OR. string(i:i) == '-') string = string(1:i-1)
      i = len(string)
      IF (string(i:i) == 'E' .OR. string(i:i) == 'e') string = string(1:i-1)
      i = len(string)
      IF (string(i:i) == 'D' .OR. string(i:i) == 'd') string = string(1:i-1) ! just in case we have 0.000...D+000

      !then, the 0 at the end are removed
      DO i=len(string),2,-1
        IF (string(i:i) == '0') THEN
          string(i:i) = ' '
        ELSE
          EXIT
        END IF
      END DO
      string = trim(adjustl(string))
    END IF

    !$OMP  END CRITICAL (QDUtil_real32_TO_string_CRIT)

  END FUNCTION QDUtil_real32_TO_string

  FUNCTION QDUtil_string_IS_empty(String)
    IMPLICIT NONE

    logical                          :: QDUtil_string_IS_empty
    character(len=*), intent(in)     :: String

    QDUtil_string_IS_empty = (len_trim(String) == 0)

  END FUNCTION QDUtil_string_IS_empty

  FUNCTION QDUtil_Read_line(nio,ioerr)  RESULT(string)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : IOSTAT_END,IOSTAT_EOR
    IMPLICIT NONE
    
    character(len=:), allocatable                 :: string
    integer,                      intent(in)      :: nio
    integer,                      intent(inout)   :: ioerr
  
  
    character(len=:), allocatable    :: line
    character(len=1)                 :: ch
  
  
    line = ""
    DO
      read(nio,'(a1)',IOSTAT=ioerr,advance='no') ch
      IF (ioerr /= 0) EXIT
      !write(6,*) 'ch: ',ch ; flush(6)
      line = line // ch
    END DO
    IF (ioerr == IOSTAT_EOR) ioerr = 0 ! end of record: the full line is read.
  
    string = line
  
    deallocate(line)
  
  END FUNCTION QDUtil_Read_line

  SUBROUTINE Test_QDUtil_String()
    USE QDUtil_Test_m
    USE QDUtil_NumParameters_m
    IMPLICIT NONE

    TYPE (test_t)                    :: test_var
    logical                          :: res_test

    character (len=:), allocatable :: string,lstring,ustring


    !----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub='Test_QDUtil_String'
    !logical, parameter :: debug = .FALSE.
    logical, parameter :: debug = .TRUE.
    !-----------------------------------------------------------

    CALL Initialize_Test(test_var,test_name='String')
  
  
    lstring = "abcdefghijklmnopqrstuvwxyz0123456789"
    ustring = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    string  = lstring
  
    write(out_unit,*) 'string:           ',string
    write(out_unit,*) 'string lower:     ',TO_lowercase(string)
  
    write(out_unit,*) 'string upper:     ',TO_uppercase(string)
    CALL string_uppercase_TO_lowercase(string)
    write(out_unit,*) 'string lower sub: ',string
    CALL string_lowercase_TO_uppercase(string)
    write(out_unit,*) 'string upper sub: ',string
    write(out_unit,*) 'string empty:             ',string_IS_empty(string)
    string = ''
    write(out_unit,*) 'string empty:             ',string_IS_empty(string)
    string = '  '
    write(out_unit,*) 'string with blanks empty: ',string_IS_empty(string)
  
    string   = lstring
    !#1
    res_test = (ustring == TO_lowercase(string))
    CALL Logical_Test(test_var,test1=res_test,info='TO_lowercase',test2=.FALSE.)

    !#2
    res_test = (lstring == TO_lowercase(string))
    CALL Logical_Test(test_var,test1=res_test,info='TO_lowercase',test2=.TRUE.)

    !#3
    res_test = (ustring == TO_uppercase(string))
    CALL Logical_Test(test_var,test1=res_test,info='TO_uppercase',test2=.TRUE.)

    !#4
    res_test = (lstring == TO_uppercase(string))
    CALL Logical_Test(test_var,test1=res_test,info='TO_uppercase',test2=.FALSE.)

    CALL Flush_Test(test_var)
  
    !#5 and 6
    res_test = ('T' == TO_string(.TRUE.))
    CALL Logical_Test(test_var,test1=res_test,info='TO_string (T)')
    res_test = ('F' == TO_string(.FALSE.))
    CALL Logical_Test(test_var,test1=res_test,info='TO_string (F)')
    CALL Flush_Test(test_var)

    !#7, 8, 9
    res_test = ('0' == TO_string(0))
    CALL Logical_Test(test_var,test1=res_test,info='TO_string (0)')
    res_test = ('10' == TO_string(10_IkS))
    CALL Logical_Test(test_var,test1=res_test,info='TO_string (10_IkS)')
    res_test = ('-1099' == TO_string(-1099_IkD))
    CALL Logical_Test(test_var,test1=res_test,info='TO_string (-1099_IkD)')
    CALL Flush_Test(test_var)

    !#10-12
    res_test = ('0.' == TO_string(0._Rkind))
    CALL Logical_Test(test_var,test1=res_test,info='TO_string (0.)')
    res_test = ('1.' == TO_string(1._RkS))
    CALL Logical_Test(test_var,test1=res_test,info='TO_string (1._RkS)')
    res_test = ('-10.' == TO_string(-10._RkD))
    CALL Logical_Test(test_var,test1=res_test,info='TO_string (-10._RkD)')
    res_test = ('-999.5' == TO_string(-999.5_RkQ))
    CALL Logical_Test(test_var,test1=res_test,info='TO_string (-999.5_RkQ)')
    CALL Flush_Test(test_var)

    !#13-14
    res_test = string_IS_empty(string)
    CALL Logical_Test(test_var,test1=res_test,info='string_IS_empty (F)',test2=.FALSE.)
    string = ''
    res_test = string_IS_empty(string)
    CALL Logical_Test(test_var,test1=res_test,info='string_IS_empty (T)')
    CALL Flush_Test(test_var)

    ! finalize the tests
    CALL Finalize_Test(test_var)

  END SUBROUTINE Test_QDUtil_String
END MODULE QDUtil_String_m
