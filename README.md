# QDUtilLib

List of Fortran modules which contain utilities for codes.
All modules contain a testing unit.

## 1) Installation and testing

### a) with a makefile:

To build the library, **libQD_gfortran_opt1_omp1.a**, with the default options (OPt=1, OMP=1, LAPACK=1)
```bash
make lib
```
It creates a library with the folowing name: **libQD_XXX_optY_ompZ.a**
with XXX, the compiler name (like gfortran), Y or Z the value O or 1
For instance, the default library is: **libQD_gfortran_opt1_omp1.a**
The module file (.mod) are in the OBJ/obj__XXX_optY_ompZ directory.

Two options to clean:
```bash
make clean
```

Remove some files, but keep the libraries, **libQD_XXX_optY_ompZ.a**

```bash
make cleanall
```

Remove all files

To test the module, in TESTS directory, run

```bash
./run_tests.sh
```

The tests are running with gfortran and several option combinations:

- OPT=1 or 0: compilation with optimization or without optimization
- OMP=1 or 0: with or without openmp
- LAPACK=1 or 0: with or without blas and lapack libraries

The file, **ALL_Tests.log**, contains a summary of all the tests.

### b) with fpm:

To build the library, **libQDUtilLib.a**, with the Lapack (LAPACK=1)
```bash
fpm build
```
The library is in **build/gfortran_xxxx/QDUtilLib** directory.
To remove Lapack library, the **fpm.tom** file must be edited.

Two options to clean:
```bash
fpm clean
```

Remove the build directory.

Remove some files, but keep the libraries, **libQDUtilLib.a**

```bash
fpm cleanall
```

To test the module:

```bash
fpm test | grep TESTING | grep Number
```

To run an example:

```bash
fpm run AppQDLib 
```

## 2) List of modules

### NumParameters

This module contains:

- **kind** definitions from the intrinsinc ISO_FORTRAN_ENV module
  - Rkind: default real kind (normaly real64)
  - RkS, RKD, RKQ for real32, real64, real128
  - Ikind: default integer kind (usualy int32)
  - ILkind: default long integer kind (usualy int64)
  - IkS, IkD for int32, int63
- Some real (kind=Rkind) numbers : ZERO to TWELVE, HALF, HUNDRED,ONETENTH ..., PI
- Some complexe (kind=Rkind) numbers : EYE (i), CZERO, CONE, CHALF
- out_unit and in_unit: the standard output and input units from the intrinsinc ISO_FORTRAN_ENV module

### String

This module contains functions and subroutines to manipulate character string (character(len=)):

- Functions to change to upper to lowercase or the reverse: **TO_lowercase**, **TO_uppercase**
- Functions to convert numbers in string: **TO_string**
  All kinds defined in NumParameters module and logical type are possible
  For real convertion, an optional format (Rformat) can be given.
- Function to read a line from a file define with its unit: **Read_line**
- Function to check is a string is empty: **string_IS_empty**

### RW_MatVec

This module contains public subroutines to read and write real and complex (kind=Rkind) vectors and matrices:

- Subroutines to read a matrix from a file: **Read_Mat** or **Read_Vec**
  All real and complexe kinds from NumParameters module are possible
  **Read_Mat(Mat,nio,nbcol,err)** or **Read_Vec(Vec,nio,nbcol,err)**
  - Mat or Vec: the matrice or the Vector to be readed. Mat or Vec have to be allocated
  - nio: the file unit
  - nbcol: the number of columns to be readed per block. If the number of columns of Mat or Vec is larger than nbcol, then several blocks are readed
  - err: integer to catch error (err=0 => no error)
- Subroutines to write a matrix to a file or string: **Write_Mat** or **Write_Vec**
  All real and complexe kinds from NumParameters module are possible. For complex number, it is not possible to write into a string.
  **Write_Mat(Mat,nio,nbcol,Rformat,info,iprint)** or **Write_Mat(Mat,string,nbcol,Rformat,info,iprint)** or **Write_Vec(Vec,nio,nbcol,Rformat,info,iprint)** or **Write_Vec(Vec,string,nbcol,Rformat,info,iprint)**
  - Mat or Vec: the matrice or the Vector to be printed. Mat or Vec have to be allocated
  - nio: the file unit
  - string: a character string with len long enough. It is better an allocatable character length [character (len=:), allocatable :: string]
  - nbcol: the number of columns to be printed per block. If the number of columns of Mat or Vec is larger than nbcol, then several blocks are printed
  - Rformat: (optional) format for real and complex. If Rformat is not present, then the default formats (RMatIO_format for real and CMatIO_format for complex) are used.
  - iprint: (optional) the Mat or Vec are printed if iprint is not present or if iprint=0
- The module contains the two default formats which are public:
  - RMatIO_format: (f18.10)
  - CMatIO_format: '(',f15.7,',',f15.7,')'

### Matrix

This module contains public functions and subroutines to perform some lienar algebra operations with the default real kind (kind=Rkind). It can use some LAPACK subroutines:

- Function to compute the inverse of a matrix:     **inv_OF_Mat_TO**
- Function to solve a linear system of equation:   **LinearSys_Solve**
- Function to compute the determinant of a matrix: **Det_OF**
- Function to initialyze a real identity matrix:   **Identity_Mat**

### Diago

This module contains public subroutines to digonlize a matrix with the default real kind (kind=Rkind). It can use some LAPACK subroutines:

- Subroutine to diagnalize a real matrix: **diagonalization**