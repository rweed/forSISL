   
!  Copyright (C) 2021 Richard Weed.
!  All rights reserved.
  
!  Redistribution and use in source and binary forms, with or without 
!  modification, are permitted provided that the following conditions are met:
  
!  1. Redistributions of source code, in whole or in part, must retain the  
!  above copyright notice, this list of conditions and the following 
!  disclaimer.
  
!  2. Redistributions in binary form, in whole or in part, must reproduce the 
!  above copyright notice, this list of conditions and the following disclaimer 
!  in the documentation and/or other materials provided with the distribution.
  
!  3. The names of the contributors may not be used to endorse or promote from 
!  products derived from this software without specific prior written 
!  permission.

!  4. Redistributions of this software, in whole or in part, in any form, 
!  must be freely available and licensed under this original License. The 
!  U.S. Government may add additional restrictions to their modified and 
!  redistributed software as required by Law. However, these restrictions 
!  do not apply to the original software distribution.
 
!  5. Redistribution of this source code, including any modifications, may 
!  not be intentionally obfuscated.
  
!  6. Other code may make use of this software, in whole or in part, without 
!  restriction, provided that it does not apply any restriction to this 
!  software other than outlined above.

!  7. This software requires a version of the SINTEF SISL library
!     (https://github.com/SINTEF-Geometry/SISL). It is assumed that users
!     will download and install this software separate from this code.
!     Users are required to honor the SISL license clauses that cover usage
!     for both commercial and non-commercial applications. See the copy of 
!     the SISL license information and copyright that occompanies this software.

 
!  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
!  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
!  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
!  PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND
!  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, 
!  EXEMPLARARY OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
!  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; 
!  OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
!  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
!  OTHERWISE), ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
!  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


Module Surface_Definition

!! Module Surface_Definition contains Modern Fortran C-interoperability routines
!! for all of the C functions described in Chapter 6 of version 4.4 of the SISL
!! reference manual

!! Written by: Richard Weed, Ph.D.
!! Version no. 1.0 - Feb. 2021
!!   Initial development version

!! Contact: rweedmsu@gmail.com

  USE forSISLdata, ONLY: REAL64, C_INT, C_DOUBLE, C_PTR, C_NULL_PTR, c_free,   &
                         C_F_POINTER, C_ASSOCIATED, SISLsurf, SISLcurve,       &
                         surfCtoF

  Implicit NONE

  PRIVATE :: REAL64, C_INT, C_DOUBLE, C_PTR, C_NULL_PTR, c_free,               &
             C_F_POINTER, C_ASSOCIATED, SISLsurf, SISLcurve, surfCtoF

Contains

!---------------------------------- s1536 -------------------------------------

  Subroutine s1536(points, im1, im2, idim, ipar, con1, con2, con3, con4,       &
                   iorder1, iorder2, iopen1, iopen2, rsurf, jstat)

!! PURPOSE
!!   s1536 - To compute a tensor surface interpolating a set of points, auto-
!!           matic parameterization. The output is represented as a B-spline
!!           surface.

!! INTERFACE
!!   Subroutine s1536(points, im1, im2, idim, ipar, con1, con2, con3, con4,    &
!!                    iorder1, iorder2, iopen1, iopen2, rsurf, jstat)
!!     Real(REAL64),    Intent(IN)    :: points(*) 
!!     Integer,         Intent(IN)    :: im1 
!!     Integer,         Intent(IN)    :: im2 
!!     Integer,         Intent(IN)    :: idim 
!!     Integer,         Intent(IN)    :: ipar 
!!     Integer,         Intent(IN)    :: con1 
!!     Integer,         Intent(IN)    :: con2 
!!     Integer,         Intent(IN)    :: con3 
!!     Integer,         Intent(IN)    :: con4 
!!     Integer,         Intent(IN)    :: iorder1 
!!     Integer,         Intent(IN)    :: iorder2 
!!     Integer,         Intent(IN)    :: iopen1 
!!     Integer,         Intent(IN)    :: iopen2 
!!     Type(SISLsurf),  Intent(INOUT) :: rsurf
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Real(REAL64),    Intent(IN)    :: points(*) 
    Integer,         Intent(IN)    :: im1 
    Integer,         Intent(IN)    :: im2 
    Integer,         Intent(IN)    :: idim 
    Integer,         Intent(IN)    :: ipar 
    Integer,         Intent(IN)    :: con1 
    Integer,         Intent(IN)    :: con2 
    Integer,         Intent(IN)    :: con3 
    Integer,         Intent(IN)    :: con4 
    Integer,         Intent(IN)    :: iorder1 
    Integer,         Intent(IN)    :: iorder2 
    Integer,         Intent(IN)    :: iopen1 
    Integer,         Intent(IN)    :: iopen2 
    Type(SISLsurf),  Intent(INOUT) :: rsurf
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_im1, c_im2, c_idim, c_ipar, c_con1, c_con2, c_con3,    &
                      c_con4, c_iorder1, c_iorder2, c_iopen1, c_iopen2, c_jstat

    Interface
      Subroutine c_s1536(points, im1, im2, idim, ipar, con1, con2, con3, con4, &
                   iorder1, iorder2, iopen1, iopen2, rsurf, jstat)             &
                   BIND(C, name="s1536")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Real(C_DOUBlE),       Intent(IN)    :: points(*)
        Integer(C_INT), VALUE               :: im1 
        Integer(C_INT), VALUE               :: im2 
        Integer(C_INT), VALUE               :: idim 
        Integer(C_INT), VALUE               :: ipar 
        Integer(C_INT), VALUE               :: con1 
        Integer(C_INT), VALUE               :: con2 
        Integer(C_INT), VALUE               :: con3 
        Integer(C_INT), VALUE               :: con4 
        Integer(C_INT), VALUE               :: iorder1 
        Integer(C_INT), VALUE               :: iorder2 
        Integer(C_INT), VALUE               :: iopen1 
        Integer(C_INT), VALUE               :: iopen2 
        Type(C_PTR),          Intent(INOUT) :: rsurf
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1536
    End Interface

    c_im1     = im1
    c_im2     = im2
    c_idim    = idim
    c_ipar    = ipar
    c_con1    = con1
    c_con2    = con2
    c_con3    = con3
    c_con4    = con4
    c_iorder1 = iorder1
    c_iorder2 = iorder2
    c_iopen1  = iopen1
    c_iopen2  = iopen2
    Call c_s1536(points, c_im1, c_im2, c_idim, c_ipar, c_con1, c_con2, c_con3, &
                 c_con4, c_iorder1, c_iorder2, c_iopen1, c_iopen2,             &
                 rsurf%cptr,  c_jstat)

    Call surfCtoF(rsurf)

    jstat      = c_jstat

  End Subroutine s1536

!---------------------------------- s1537 -------------------------------------

  Subroutine s1537(points, im1, im2, idim, par1, par2, con1, con2, con3, con4, &
                   iorder1, iorder2, iopen1, iopen2, rsurf, jstat)

!! PURPOSE
!!   s1537 - Compute a tensor surface interpolating a set of points, parameter-
!!           ization as input. The output is represented as a B-spline surface.

!! INTERFACE
!!   Subroutine s1537(points, im1, im2, idim, par1, par2, con1, con2, con3,  &
!!                    con4, iorder1, iorder2, iopen1, iopen2, rsurf, jstat)
!!     Real(REAL64),    Intent(IN)    :: points(*) 
!!     Integer,         Intent(IN)    :: im1 
!!     Integer,         Intent(IN)    :: im2 
!!     Integer,         Intent(IN)    :: idim
!!     Real(REAL64),    Intent(IN)    :: par1(*) 
!!     Real(REAL64),    Intent(IN)    :: par2(*) 
!!     Integer,         Intent(IN)    :: con1 
!!     Integer,         Intent(IN)    :: con2 
!!     Integer,         Intent(IN)    :: con3 
!!     Integer,         Intent(IN)    :: con4 
!!     Integer,         Intent(IN)    :: iorder1 
!!     Integer,         Intent(IN)    :: iorder2 
!!     Integer,         Intent(IN)    :: iopen1 
!!     Integer,         Intent(IN)    :: iopen2 
!!     Type(SISLsurf),  Intent(INOUT) :: rsurf
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Real(REAL64),    Intent(IN)    :: points(*) 
    Integer,         Intent(IN)    :: im1 
    Integer,         Intent(IN)    :: im2 
    Integer,         Intent(IN)    :: idim
    Real(REAL64),    Intent(IN)    :: par1(*) 
    Real(REAL64),    Intent(IN)    :: par2(*) 
    Integer,         Intent(IN)    :: con1 
    Integer,         Intent(IN)    :: con2 
    Integer,         Intent(IN)    :: con3 
    Integer,         Intent(IN)    :: con4 
    Integer,         Intent(IN)    :: iorder1 
    Integer,         Intent(IN)    :: iorder2 
    Integer,         Intent(IN)    :: iopen1 
    Integer,         Intent(IN)    :: iopen2 
    Type(SISLsurf),  Intent(INOUT) :: rsurf
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_im1, c_im2, c_idim, c_con1, c_con2, c_con3, c_con4,    &
                      c_iorder1, c_iorder2, c_iopen1, c_iopen2, c_jstat

    Interface
      Subroutine c_s1537(points, im1, im2, idim, par1, par2, con1, con2, con3, &
                         con4, iorder1, iorder2, iopen1, iopen2, rsurf, jstat) &
                         BIND(C, name="s1537")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Real(C_DOUBlE),       Intent(IN)    :: points(*)
        Integer(C_INT), VALUE               :: im1 
        Integer(C_INT), VALUE               :: im2 
        Integer(C_INT), VALUE               :: idim 
        Real(C_DOUBLE),       Intent(IN)    :: par1(*) 
        Real(C_DOUBLE),       Intent(IN)    :: par2(*) 
        Integer(C_INT), VALUE               :: con1 
        Integer(C_INT), VALUE               :: con2 
        Integer(C_INT), VALUE               :: con3 
        Integer(C_INT), VALUE               :: con4 
        Integer(C_INT), VALUE               :: iorder1 
        Integer(C_INT), VALUE               :: iorder2 
        Integer(C_INT), VALUE               :: iopen1 
        Integer(C_INT), VALUE               :: iopen2 
        Type(C_PTR),          Intent(INOUT) :: rsurf
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1537
    End Interface

    c_im1     = im1
    c_im2     = im2
    c_idim    = idim
    c_con1    = con1
    c_con2    = con2
    c_con3    = con3
    c_con4    = con4
    c_iorder1 = iorder1
    c_iorder2 = iorder2
    c_iopen1  = iopen1
    c_iopen2  = iopen2
    Call c_s1537(points, c_im1, c_im2, c_idim, par1, par2, c_con1, c_con2,     &
                 c_con3, c_con4, c_iorder1, c_iorder2, c_iopen1, c_iopen2,     &
                 rsurf%cptr,  c_jstat)

    Call surfCtoF(rsurf)

    jstat      = c_jstat

  End Subroutine s1537

!---------------------------------- s1534 -------------------------------------

  Subroutine s1534(points, der10, der01, der11, im1, im2, idim, ipar, con1,    &
                   con2, con3, con4, iorder1, iorder2, iopen1, iopen2, rsurf,  &
                   jstat)

!! PURPOSE
!!    s1534 - To compute a surface interpolating a set of points, derivatives as
!!            input. The output is represented as a B-spline surface.

!! INTERFACE
!!   Subroutine s1534(points, der10, der01, der11, im1, im2, idim, ipar, con1, &
!!                    con2, con3, con4, iorder1, iorder2, iopen1, iopen2,      &
!!                    rsurf, jstat)
!!     Real(REAL64),    Intent(IN)    :: points(*)
!!     Real(REAL64),    Intent(IN)    :: der10(*) 
!!     Real(REAL64),    Intent(IN)    :: der01(*) 
!!     Real(REAL64),    Intent(IN)    :: der11(*) 
!!     Integer,         Intent(IN)    :: im1 
!!     Integer,         Intent(IN)    :: im2 
!!     Integer,         Intent(IN)    :: idim 
!!     Integer,         Intent(IN)    :: ipar 
!!     Integer,         Intent(IN)    :: con1 
!!     Integer,         Intent(IN)    :: con2 
!!     Integer,         Intent(IN)    :: con3 
!!     Integer,         Intent(IN)    :: con4 
!!     Integer,         Intent(IN)    :: iorder1 
!!     Integer,         Intent(IN)    :: iorder2 
!!     Integer,         Intent(IN)    :: iopen1 
!!     Integer,         Intent(IN)    :: iopen2 
!!     Type(SISLsurf),  Intent(INOUT) :: rsurf
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Real(REAL64),    Intent(IN)    :: points(*)
    Real(REAL64),    Intent(IN)    :: der10(*) 
    Real(REAL64),    Intent(IN)    :: der01(*) 
    Real(REAL64),    Intent(IN)    :: der11(*) 
    Integer,         Intent(IN)    :: im1 
    Integer,         Intent(IN)    :: im2 
    Integer,         Intent(IN)    :: idim 
    Integer,         Intent(IN)    :: ipar 
    Integer,         Intent(IN)    :: con1 
    Integer,         Intent(IN)    :: con2 
    Integer,         Intent(IN)    :: con3 
    Integer,         Intent(IN)    :: con4 
    Integer,         Intent(IN)    :: iorder1 
    Integer,         Intent(IN)    :: iorder2 
    Integer,         Intent(IN)    :: iopen1 
    Integer,         Intent(IN)    :: iopen2 
    Type(SISLsurf),  Intent(INOUT) :: rsurf
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_im1, c_im2, c_idim, c_ipar, c_con1, c_con2, c_con3,    &
                      c_con4, c_iorder1, c_iorder2, c_iopen1, c_iopen2, c_jstat

    Interface
      Subroutine c_s1534(points, der10, der01, der11, im1, im2, idim, ipar,    &
                   con1, con2, con3, con4, iorder1, iorder2, iopen1, iopen2,   &
                   rsurf, jstat)                                               &
                   BIND(C, name="s1534")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Real(C_DOUBlE),       Intent(IN)    :: points(*)
        Real(C_DOUBLE),       Intent(IN)    :: der10(*)
        Real(C_DOUBLE),       Intent(IN)    :: der01(*)
        Real(C_DOUBLE),       Intent(IN)    :: der11(*)
        Integer(C_INT), VALUE               :: im1 
        Integer(C_INT), VALUE               :: im2 
        Integer(C_INT), VALUE               :: idim 
        Integer(C_INT), VALUE               :: ipar 
        Integer(C_INT), VALUE               :: con1 
        Integer(C_INT), VALUE               :: con2 
        Integer(C_INT), VALUE               :: con3 
        Integer(C_INT), VALUE               :: con4 
        Integer(C_INT), VALUE               :: iorder1 
        Integer(C_INT), VALUE               :: iorder2 
        Integer(C_INT), VALUE               :: iopen1 
        Integer(C_INT), VALUE               :: iopen2 
        Type(C_PTR),          Intent(INOUT) :: rsurf
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1534
    End Interface

    c_im1     = im1
    c_im2     = im2
    c_idim    = idim
    c_ipar    = ipar
    c_con1    = con1
    c_con2    = con2
    c_con3    = con3
    c_con4    = con4
    c_iorder1 = iorder1
    c_iorder2 = iorder2
    c_iopen1  = iopen1
    c_iopen2  = iopen2
    Call c_s1534(points, der10, der01, der11, c_im1, c_im2, c_idim, c_ipar,   &
                 c_con1, c_con2, c_con3, c_con4, c_iorder1, c_iorder2,        &
                 c_iopen1, c_iopen2,rsurf%cptr,  c_jstat)

    Call surfCtoF(rsurf)

    jstat      = c_jstat

  End Subroutine s1534

!---------------------------------- s1535 -------------------------------------

  Subroutine s1535(points, der10, der01, der11, im1, im2, idim, par1, par2,    &
                   con1, con2, con3, con4, iorder1, iorder2, rsurf, jstat) 

!! PURPOSE
!!   s1535 - Compute a surface interpolating a set of points, derivatives and
!!           parameterization as input. The output is represented as a B-spline
!!           surface.

!! INTERFACE
!!   Subroutine s1535(points, der10, der01, der11, im1, im2, idim, par1, par2, &
!!                    con1, con2, con3, con4, iorder1, iorder2, rsurf, jstat) 
!!     Real(REAL64),    Intent(IN)    :: points(*)
!!     Real(REAL64),    Intent(IN)    :: der10(*) 
!!     Real(REAL64),    Intent(IN)    :: der01(*) 
!!     Real(REAL64),    Intent(IN)    :: der11(*) 
!!     Integer,         Intent(IN)    :: im1 
!!     Integer,         Intent(IN)    :: im2 
!!     Integer,         Intent(IN)    :: idim 
!!     Real(REAL64),    Intent(IN)    :: par1(*)
!!     Real(REAL64),    Intent(IN)    :: par2(*)
!!     Integer,         Intent(IN)    :: con1 
!!     Integer,         Intent(IN)    :: con2 
!!     Integer,         Intent(IN)    :: con3 
!!     Integer,         Intent(IN)    :: con4 
!!     Integer,         Intent(IN)    :: iorder1 
!!     Integer,         Intent(IN)    :: iorder2 
!!     Type(SISLsurf),  Intent(INOUT) :: rsurf
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Real(REAL64),    Intent(IN)    :: points(*)
    Real(REAL64),    Intent(IN)    :: der10(*) 
    Real(REAL64),    Intent(IN)    :: der01(*) 
    Real(REAL64),    Intent(IN)    :: der11(*) 
    Integer,         Intent(IN)    :: im1 
    Integer,         Intent(IN)    :: im2 
    Integer,         Intent(IN)    :: idim 
    Real(REAL64),    Intent(IN)    :: par1(*)
    Real(REAL64),    Intent(IN)    :: par2(*)
    Integer,         Intent(IN)    :: con1 
    Integer,         Intent(IN)    :: con2 
    Integer,         Intent(IN)    :: con3 
    Integer,         Intent(IN)    :: con4 
    Integer,         Intent(IN)    :: iorder1 
    Integer,         Intent(IN)    :: iorder2 
    Type(SISLsurf),  Intent(INOUT) :: rsurf
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_im1, c_im2, c_idim, c_con1, c_con2, c_con3, c_con4,    &
                      c_iorder1, c_iorder2, c_jstat

    Interface
      Subroutine c_s1535(points, der10, der01, der11, im1, im2, idim, par1,    &
                         par2, con1, con2, con3, con4, iorder1, iorder2,       &
                         rsurf, jstat)                                         &
                         BIND(C, name="s1535")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Real(C_DOUBlE),       Intent(IN)    :: points(*)
        Real(C_DOUBLE),       Intent(IN)    :: der10(*)
        Real(C_DOUBLE),       Intent(IN)    :: der01(*)
        Real(C_DOUBLE),       Intent(IN)    :: der11(*)
        Integer(C_INT), VALUE               :: im1 
        Integer(C_INT), VALUE               :: im2 
        Integer(C_INT), VALUE               :: idim 
        Real(C_DOUBLE),       Intent(IN)    :: par1(*)
        Real(C_DOUBLE),       Intent(IN)    :: par2(*)
        Integer(C_INT), VALUE               :: con1 
        Integer(C_INT), VALUE               :: con2 
        Integer(C_INT), VALUE               :: con3 
        Integer(C_INT), VALUE               :: con4 
        Integer(C_INT), VALUE               :: iorder1 
        Integer(C_INT), VALUE               :: iorder2 
        Type(C_PTR),          Intent(INOUT) :: rsurf
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1535
    End Interface

    c_im1     = im1
    c_im2     = im2
    c_idim    = idim
    c_con1    = con1
    c_con2    = con2
    c_con3    = con3
    c_con4    = con4
    c_iorder1 = iorder1
    c_iorder2 = iorder2
    Call c_s1535(points, der10, der01, der11, c_im1, c_im2, c_idim, par1,      &
                 par2, c_con1, c_con2, c_con3, c_con4, c_iorder1, c_iorder2,   &
                 rsurf%cptr,  c_jstat)

    Call surfCtoF(rsurf)

    jstat      = c_jstat

  End Subroutine s1535

!---------------------------------- s1529 -------------------------------------

  Subroutine s1529(ep, eder10, eder01, eder11, im1, im2, idim, ipar, rsurf,    &
                   jstat) 

!! PURPOSE
!!   s1529 - Compute the cubic Hermite surface interpolant to the data given.
!!           More specifically, given positions, (u’,v), (u,v’), and (u’,v’)
!!           derivatives at points of a rectangular grid, the routine computes
!!           a cubic tensor-product B-spline interpolant to the given data with
!!           double knots at each data (the first knot vector will have double
!!           knots at all interior points in epar1, quadruple knots at the first
!!           and last points, and similarly for the second knot vector). The
!! output is represented as a B-spline surface.

!! INTERFACE
!!   Subroutine s1529(ep, eder10, eder01, eder11, im1, im2, idim, ipar, rsurf, &
!!                    jstat) 
!!     Real(REAL64),    Intent(IN)    :: ep(*)
!!     Real(REAL64),    Intent(IN)    :: eder10(*) 
!!     Real(REAL64),    Intent(IN)    :: eder01(*) 
!!     Real(REAL64),    Intent(IN)    :: eder11(*) 
!!     Integer,         Intent(IN)    :: im1 
!!     Integer,         Intent(IN)    :: im2 
!!     Integer,         Intent(IN)    :: idim 
!!     Integer,         Intent(IN)    :: ipar 
!!     Type(SISLsurf),  Intent(INOUT) :: rsurf
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Real(REAL64),    Intent(IN)    :: ep(*)
    Real(REAL64),    Intent(IN)    :: eder10(*) 
    Real(REAL64),    Intent(IN)    :: eder01(*) 
    Real(REAL64),    Intent(IN)    :: eder11(*) 
    Integer,         Intent(IN)    :: im1 
    Integer,         Intent(IN)    :: im2 
    Integer,         Intent(IN)    :: idim 
    Integer,         Intent(IN)    :: ipar 
    Type(SISLsurf),  Intent(INOUT) :: rsurf
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_im1, c_im2, c_idim, c_ipar, c_jstat 

    Interface
      Subroutine c_s1529(ep, eder10, eder01, eder11, im1, im2, idim, ipar,    &
                         rsurf, jstat)                                        &
                         BIND(C, name="s1529")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Real(C_DOUBlE),       Intent(IN)    :: ep(*)
        Real(C_DOUBLE),       Intent(IN)    :: eder10(*)
        Real(C_DOUBLE),       Intent(IN)    :: eder01(*)
        Real(C_DOUBLE),       Intent(IN)    :: eder11(*)
        Integer(C_INT), VALUE               :: im1 
        Integer(C_INT), VALUE               :: im2 
        Integer(C_INT), VALUE               :: idim 
        Integer(C_INT), VALUE               :: ipar 
        Type(C_PTR),          Intent(INOUT) :: rsurf
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1529
    End Interface

    c_im1  = im1
    c_im2  = im2
    c_idim = idim
    c_ipar = ipar 
    Call c_s1529(ep, eder10, eder01, eder11, c_im1, c_im2, c_idim, c_ipar,     &
                 rsurf%cptr,  c_jstat)

    Call surfCtoF(rsurf)

    jstat      = c_jstat

  End Subroutine s1529

!---------------------------------- s1530 -------------------------------------

  Subroutine s1530(ep, eder10, eder01, eder11, epar1, epar2, im1, im2, idim,   &
                   rsurf, jstat) 

!! PURPOSE
!!   s1530 - To compute the cubic Hermite interpolant to the data given. More
!!           specifically, given positions, 10, 01, and 11 derivatives at 
!!           points of a rectangular grid, the routine computes a cubic tensor-
!!           product B-spline interpolant to the given data with double knots
!!           at each data point (the first knot vector will have double knots at
!!           all interior points in epar1, quadruple knots at the first and last
!!           points, and similarly for the second knot vector). The output is
!!           represented as a B-spline surface.

!! INTERFACE
!!   Subroutine s1530(ep, eder10, eder01, eder11, epar1, epar2, im1, im2, idim,&
!!                    rsurf, jstat) 
!!     Real(REAL64),    Intent(IN)    :: ep(*)
!!     Real(REAL64),    Intent(IN)    :: eder10(*) 
!!     Real(REAL64),    Intent(IN)    :: eder01(*) 
!!     Real(REAL64),    Intent(IN)    :: eder11(*) 
!!     Real(REAL64),    Intent(IN)    :: epar1(*) 
!!     Real(REAL64),    Intent(IN)    :: epar2(*) 
!!     Integer,         Intent(IN)    :: im1 
!!     Integer,         Intent(IN)    :: im2 
!!     Integer,         Intent(IN)    :: idim 
!!     Type(SISLsurf),  Intent(INOUT) :: rsurf
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Real(REAL64),    Intent(IN)    :: ep(*)
    Real(REAL64),    Intent(IN)    :: eder10(*) 
    Real(REAL64),    Intent(IN)    :: eder01(*) 
    Real(REAL64),    Intent(IN)    :: eder11(*) 
    Real(REAL64),    Intent(IN)    :: epar1(*) 
    Real(REAL64),    Intent(IN)    :: epar2(*) 
    Integer,         Intent(IN)    :: im1 
    Integer,         Intent(IN)    :: im2 
    Integer,         Intent(IN)    :: idim 
    Type(SISLsurf),  Intent(INOUT) :: rsurf
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_im1, c_im2, c_idim, c_jstat 

    Interface
      Subroutine c_s1530(ep, eder10, eder01, eder11, epar1, epar2, im1, im2,   &
                         idim, rsurf, jstat)                                   &
                         BIND(C, name="s1530")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Real(C_DOUBlE),       Intent(IN)    :: ep(*)
        Real(C_DOUBLE),       Intent(IN)    :: eder10(*)
        Real(C_DOUBLE),       Intent(IN)    :: eder01(*)
        Real(C_DOUBLE),       Intent(IN)    :: eder11(*)
        Real(C_DOUBLE),       Intent(IN)    :: epar1(*)
        Real(C_DOUBLE),       Intent(IN)    :: epar2(*)
        Integer(C_INT), VALUE               :: im1 
        Integer(C_INT), VALUE               :: im2 
        Integer(C_INT), VALUE               :: idim 
        Type(C_PTR),          Intent(INOUT) :: rsurf
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1530
    End Interface

    c_im1  = im1
    c_im2  = im2
    c_idim = idim
    Call c_s1530(ep, eder10, eder01, eder11, epar1, epar2, c_im1, c_im2,       &
                 c_idim, rsurf%cptr,  c_jstat)

    Call surfCtoF(rsurf)

    jstat      = c_jstat

  End Subroutine s1530

!---------------------------------- s1538 -------------------------------------

  Subroutine s1538(inbcrv, vpcurv, nctyp, astpar, iopen, iord2, iflag, rsurf,  &
                   gpar, jstat)

!! PURPOSE
!!   s1538 - To create a lofted surface from a set of B-spline (i.e. NOT 
!!           rational) input curves. The output is represented as a B-spline 
!!           surface.

!! INTERFACE
!!   Subroutine s1538(inbcrv, vpcurv, nctyp, astpar, iopen, iord2, iflag,      &
!!                    rsurf, gpar, jstat)
!!     Integer,                         Intent(IN)    :: inbcrv 
!!     Type(SISLcurve),                 Intent(IN)    :: vpcurv(*)
!!     Integer,                         Intent(IN)    :: nctyp(*)
!!     Real(REAL64),                    Intent(IN)    :: astpar
!!     Integer,                         Intent(IN)    :: iopen 
!!     Integer,                         Intent(IN)    :: iord2
!!     Integer,                         Intent(IN)    :: iflag
!!     Type(SISLsurf),                  Intent(INOUT) :: rsurf
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: gpar(:)
!!     Integer,                         Intent(INOUT) :: jstat

    Implicit NONE

    Integer,                         Intent(IN)    :: inbcrv 
    Type(SISLcurve),                 Intent(IN)    :: vpcurv(*)
    Integer,                         Intent(IN)    :: nctyp(*)
    Real(REAL64),                    Intent(IN)    :: astpar
    Integer,                         Intent(IN)    :: iopen 
    Integer,                         Intent(IN)    :: iord2
    Integer,                         Intent(IN)    :: iflag
    Type(SISLsurf),                  Intent(INOUT) :: rsurf
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: gpar(:)
    Integer,                         Intent(INOUT) :: jstat


    Integer :: i

    Integer(C_INT)        :: c_inbcrv, c_iopen, c_iord2, c_iflag, c_jstat
    Integer(C_INT)        :: c_nctyp(inbcrv)
    Type(C_PTR)           :: vpcurv_p(inbcrv)
    Type(C_PTR)           :: c_gpar_p
    Real(REAL64), Pointer :: gparp(:)

    Interface
    Subroutine c_s1538(inbcrv, vpcurv, nctyp, astpar, iopen, iord2, iflag,     &
                       rsurf, gpar, jstat)                                     &
                       BIND(C,name="s1538")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Integer(C_INT), VALUE               :: inbcrv
        Type(C_PTR),          Intent(IN)    :: vpcurv(*)
        Integer(C_INT),       Intent(IN)    :: nctyp(*)
        Real(C_DOUBLE), VALUE               :: astpar 
        Integer(C_INT), VALUE               :: iopen 
        Integer(C_INT), VALUE               :: iord2 
        Integer(C_INT), VALUE               :: iflag 
        Type(C_PTR),          Intent(INOUT) :: rsurf 
        Type(C_PTR),          Intent(INOUT) :: gpar
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1538
    End Interface

    c_inbcrv   = inbcrv
    c_iopen    = iopen
    c_iord2    = iord2
    c_iflag    = iflag
    c_nctyp(:) = nctyp(1:inbcrv)

    Do i=1,inbcrv
      vpcurv_p(i) = vpcurv(i)%cptr
    EndDo

    Call c_s1538(c_inbcrv, vpcurv_p, c_nctyp, astpar, c_iopen, c_iord2,     &
                 c_iflag, rsurf%cptr, c_gpar_p, c_jstat)

    If (C_ASSOCIATED(c_gpar_p)) Then
      Call C_F_Pointer(c_gpar_p, gparp,[inbcrv])
      If (ALLOCATED(gpar)) DEALLOCATE(gpar)
      ALLOCATE(gpar(inbcrv), SOURCE=0.0_REAL64)
      gpar(:) = gparp(:)
      NULLIFY(gparp)
      Call c_free(c_gpar_p)
    EndIf

    Call surfCtoF(rsurf)
    jstat = c_jstat

  End Subroutine s1538

!---------------------------------- s1539 -------------------------------------

  Subroutine s1539(inbcrv, vpcurv, nctyp, epar, astpar, iopen, iord2, iflag,   &
                   rsurf, gpar, jstat)

!! PURPOSE
!!   s1539 - To create a spline lofted surface from a set of input curves. The
!!           parametrization of the position curves is given in epar.

!! INTERFACE
!!   Subroutine s1539(inbcrv, vpcurv, nctyp, epar, astpar, iopen, iord2,       &
!!                    iflag, rsurf, gpar, jstat)
!!     Integer,                         Intent(IN)    :: inbcrv 
!!     Type(SISLcurve),                 Intent(IN)    :: vpcurv(*)
!!     Integer,                         Intent(IN)    :: nctyp(*)
!!     Real(REAL64),                    Intent(IN)    :: epar(*) 
!!     Real(REAL64),                    Intent(IN)    :: astpar
!!     Integer,                         Intent(IN)    :: iopen 
!!     Integer,                         Intent(IN)    :: iord2
!!     Integer,                         Intent(IN)    :: iflag
!!     Type(SISLsurf),                  Intent(INOUT) :: rsurf
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: gpar(:)
!!     Integer,                         Intent(INOUT) :: jstat

    Implicit NONE

    Integer,                         Intent(IN)    :: inbcrv 
    Type(SISLcurve),                 Intent(IN)    :: vpcurv(*)
    Integer,                         Intent(IN)    :: nctyp(*)
    Real(REAL64),                    Intent(IN)    :: epar(*) 
    Real(REAL64),                    Intent(IN)    :: astpar
    Integer,                         Intent(IN)    :: iopen 
    Integer,                         Intent(IN)    :: iord2
    Integer,                         Intent(IN)    :: iflag
    Type(SISLsurf),                  Intent(INOUT) :: rsurf
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: gpar(:)
    Integer,                         Intent(INOUT) :: jstat

    Integer :: i

    Integer(C_INT)        :: c_inbcrv, c_iopen, c_iord2, c_iflag, c_jstat
    Integer(C_INT)        :: c_nctyp(inbcrv)
    Type(C_PTR)           :: c_vpcurv_p(inbcrv)
    Type(C_PTR)           :: c_gpar_p
    Real(REAL64), Pointer :: gparp(:)

    Interface
    Subroutine c_s1539(inbcrv, vpcurv, nctyp, epar, astpar, iopen, iord2,      &
                       iflag, rsurf, gpar, jstat)                              &
                       BIND(C,name="s1539")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Integer(C_INT), VALUE               :: inbcrv
        Type(C_PTR),          Intent(IN)    :: vpcurv(*) 
        Integer(C_INT),       Intent(IN)    :: nctyp(*)
        Real(C_DOUBLE),       Intent(IN)    :: epar(*) 
        Real(C_DOUBLE), VALUE               :: astpar
        Integer(C_INT), VALUE               :: iopen 
        Integer(C_INT), VALUE               :: iord2 
        Integer(C_INT), VALUE               :: iflag 
        Type(C_PTR),          Intent(INOUT) :: rsurf 
        Type(C_PTR),          Intent(INOUT) :: gpar
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1539
    End Interface

    c_inbcrv   = inbcrv
    c_iopen    = iopen
    c_iord2    = iord2
    c_iflag    = iflag
    c_nctyp(:) = nctyp(1:inbcrv)

    Do i=1,inbcrv
      c_vpcurv_p(i) = vpcurv(i)%cptr
    EndDo

    Call c_s1539(c_inbcrv, c_vpcurv_p, c_nctyp, epar, astpar, c_iopen,         &
                 c_iord2, c_iflag, rsurf%cptr, c_gpar_p, c_jstat)

    If (C_ASSOCIATED(c_gpar_p)) Then
      Call C_F_Pointer(c_gpar_p, gparp,[inbcrv])
      If (ALLOCATED(gpar)) DEALLOCATE(gpar)
      ALLOCATE(gpar(inbcrv), SOURCE=0.0_REAL64)
      gpar(:) = gparp(:)
      NULLIFY(gparp)
      Call c_free(c_gpar_p)
    EndIf

    Call surfCtoF(rsurf)

    jstat = c_jstat

  End Subroutine s1539

!---------------------------------- s1508 -------------------------------------

  Subroutine s1508(inbcrv, vpcurv, par_arr, rsurf, jstat) 

!! PURPOSE
!!   s1508 - To create a rational lofted surface from a set of rational input-
!!           curves

!! INTERFACE
!!   Subroutine s1508(inbcrv, vpcurv, par_arr, rsurf, jstat) 
!!     Integer,                         Intent(IN)    :: inbcrv 
!!     Type(SISLcurve),                 Intent(IN)    :: vpcurv(*)
!!     Real(REAL64),                    Intent(IN)    :: par_arr(*) 
!!     Type(SISLsurf),                  Intent(INOUT) :: rsurf
!!     Integer,                         Intent(INOUT) :: jstat

    Implicit NONE

    Integer,                         Intent(IN)    :: inbcrv 
    Type(SISLcurve),                 Intent(IN)    :: vpcurv(*)
    Real(REAL64),                    Intent(IN)    :: par_arr(*) 
    Type(SISLsurf),                  Intent(INOUT) :: rsurf
    Integer,                         Intent(INOUT) :: jstat

    Integer :: i

    Integer(C_INT)        :: c_inbcrv, c_jstat
    Type(C_PTR)           :: c_vpcurv_p(inbcrv)

    Interface
    Subroutine c_s1508(inbcrv, vpcurv, par_arr, rsurf, jstat)                  &
                       BIND(C,name="s1508")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Integer(C_INT), VALUE               :: inbcrv
        Type(C_PTR),          Intent(IN)    :: vpcurv(*) 
        Real(C_DOUBLE),       Intent(IN)    :: par_arr(*) 
        Type(C_PTR),          Intent(INOUT) :: rsurf 
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1508
    End Interface

    c_inbcrv   = inbcrv

    Do i=1,inbcrv
      c_vpcurv_p(i) = vpcurv(i)%cptr
    EndDo

    Call c_s1508(c_inbcrv, c_vpcurv_p, par_arr, rsurf%cptr, c_jstat) 

    Call surfCtoF(rsurf)

    jstat = c_jstat

  End Subroutine s1508

!---------------------------------- s1390 -------------------------------------

  Subroutine s1390(curves, rsurf, numder, stat) 

!! PURPOSE
!! s1390 - Make a 4-edged blending surface between 4 B-spline (i.e. NOT
!!         rational) curves where each curve is associated with a number of
!!         cross-derivative B-spline (i.e. NOT rational) curves. The output is
!!         represented as a B-spline surface. The input curves are numbered
!1         successively around the blending parameter, and the directions
!!         of the curves are expected to be directed in the positive u or v 
!!         directions with cross-derivatives always pointing into the patch

!! INTERFACE
!!   Subroutine s1390(curves, rsurf, numder, stat) 
!!     Type(SISLcurve),                 Intent(IN)    :: curves(:)
!!     Type(SISLsurf),                  Intent(INOUT) :: rsurf
!!     Integer,                         Intent(IN)    :: numder(4) 
!!     Integer,                         Intent(INOUT) :: stat
 
    Implicit NONE

    Type(SISLcurve),                 Intent(IN)    :: curves(:)
    Type(SISLsurf),                  Intent(INOUT) :: rsurf
    Integer,                         Intent(IN)    :: numder(4) 
    Integer,                         Intent(INOUT) :: stat


    Integer :: i, ncurves, snumder

    Integer(C_INT)        :: c_stat
    Integer(C_INT)        :: c_numder(4)
    Type(C_PTR)           :: c_curves_p(SIZE(curves))
    
    Interface
    Subroutine c_s1390(curves, rsurf, numder, stat)                            &
                       BIND(C,name="s1390")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),          Intent(IN)    :: curves(*) 
        Type(C_PTR),          Intent(INOUT) :: rsurf 
        Integer(C_INT),       Intent(IN)    :: numder(4) 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1390
    End Interface

    ncurves = SIZE(curves) 
    snumder = SUM(numder)
    If (snumder /= ncurves) Then
       stat = -1000
       RETURN
    EndIf

    Do i=1,ncurves
      c_curves_p(i) = curves(i)%cptr
    EndDo

    c_numder = numder
    Call c_s1390(c_curves_p, rsurf%cptr, c_numder, c_stat) 

    Call surfCtoF(rsurf)

    stat = c_stat

  End Subroutine s1390

!---------------------------------- s1391 -------------------------------------

  Subroutine s1391(pc, ws, icurv, nder, jstat) 

!! PURPOSE
!!   s1391 - To create a first derivative continuous blending surface set
!!           over a 3-, 4-, 5- and 6-sided region in space. The boundary of the
!!           region are B-spline (i.e. NOT rational) curves and the cross
!!           boundary derivatives are given as B-spline (i.e. NOT rational)
!!           curves. This function automatically preprocesses the input cross
!!           tangent curves in order to make them suitable for the blending. 
!!           Thus, the cross tangent curves should be taken as the cross tangents 
!!           of the surrounding surface. It is not necessary and not advisable
!!           to match tangents etc. in the corners. The output is represented as
!!           a set of B-spline surfaces

!! INTERFACE
!!   Subroutine s1391(pc, ws, icurv, nder, jstat) 
!!     Type(SISLcurve),              Intent(IN)    :: pc(:)
!!     Type(SISLsurf),  ALLOCATABLE, Intent(INOUT) :: ws(:)
!!     Integer,                      Intent(IN)    :: icurv
!!     Integer,                      Intent(IN)    :: nder(icurv)
!!     Integer,                      Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLcurve),              Intent(IN)    :: pc(:)
    Type(SISLsurf),  ALLOCATABLE, Intent(INOUT) :: ws(:)
    Integer,                      Intent(IN)    :: icurv
    Integer,                      Intent(IN)    :: nder(icurv)
    Integer,                      Intent(INOUT) :: jstat


    Integer :: i, npc  

    Integer(C_INT)         :: c_jstat, c_icurv
    Integer(C_INT)         :: c_nder(icurv)
    Type(C_PTR)            :: c_pc_p(SIZE(pc))
    Type(C_PTR)            :: c_ws_p
    Type(C_PTR),   Pointer :: cwsp(:)
 
    Interface
    Subroutine c_s1391(pc, ws, icurv, nder, jstat)                             &
                       BIND(C,name="s1391")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),          Intent(IN)    :: pc(*) 
        Type(C_PTR),          Intent(INOUT) :: ws
        Integer(C_INT), VALUE               :: icurv 
        Integer(C_INT),       Intent(IN)    :: nder(*) 
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1391
    End Interface

    npc = SIZE(pc) 

    Do i=1,npc
      c_pc_p(i) = pc(i)%cptr
    EndDo
    c_nder = nder(1:icurv)

    c_icurv = icurv
    Call c_s1391(c_pc_p, c_ws_p, c_icurv, c_nder, c_jstat) 

    If (C_ASSOCIATED(c_ws_p)) Then
      Call C_F_POINTER(c_ws_p, cwsp, [icurv])
      If (ALLOCATED(ws)) DEALLOCATE(ws)
      ALLOCATE(ws(icurv))
      Do i=1,icurv
        If (C_ASSOCIATED(cwsp(i))) Then
          ws(i)%cptr = cwsp(i)
        Else
          ws(i)%cptr = C_NULL_PTR
        EndIf  
        Call surfCtoF(ws(i))
      EndDo
    End If

    jstat = c_jstat

  End Subroutine s1391

!---------------------------------- s1401 -------------------------------------

  Subroutine s1401(vcurv, etwist, rsurf, jstat) 

!! PURPOSE
!!   s1401 - Compute a Gordon patch, given position and cross tangent con-
!!           ditions as B-spline (i.e. NOT rational) curves at the boundary of
!!           a squared region and the twist vector in the corners. The output
!!           is represented as a B-spline surface.

!! INTERFACE
!!   Subroutine s1401(vcurv, etwist, rsurf, jstat) 
!!     Type(SISLcurve), Intent(IN)    :: vcurv(8)
!!     Real(REAL64),    Intent(IN)    :: etwist(*) 
!!     Type(SISLsurf),  Intent(INOUT) :: rsurf
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: vcurv(8)
    Real(REAL64),    Intent(IN)    :: etwist(*) 
    Type(SISLsurf),  Intent(INOUT) :: rsurf
    Integer,         Intent(INOUT) :: jstat

    Integer :: i

    Integer(C_INT)        :: c_jstat
    Type(C_PTR)           :: c_vcurv_p(8)

    Interface
    Subroutine c_s1401(vcurv, etwist, rsurf, jstat)                            &
                       BIND(C,name="s1401")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),          Intent(IN)    :: vcurv(*) 
        Real(C_DOUBLE),       Intent(IN)    :: etwist(*) 
        Type(C_PTR),          Intent(INOUT) :: rsurf 
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1401
    End Interface

    Do i=1,8
      c_vcurv_p(i) = vcurv(i)%cptr
    EndDo

    Call c_s1401(c_vcurv_p, etwist, rsurf%cptr, c_jstat) 

    Call surfCtoF(rsurf)

    jstat = c_jstat

  End Subroutine s1401

!---------------------------------- s1620 -------------------------------------

  Subroutine s1620(epoint, inbpnt1, inbpnt2, ipar, iopen1, iopen2, ik1, ik2,   &
                   idim, rs, jstat)

!! PURPOSE
!!   s1620 - To calculate a surface using the input points as control vertices.
!!           The parametrization is calculated according to ipar. The output
!!           is represented as a B-spline surface.

!! INTERFACE
!!   Subroutine s1620(epoint, inbpnt1, inbpnt2, ipar, iopen1, iopen2, ik1, ,   &
!!                    ik2, idim, rs, jstat)
!!     Real(REAL64),    Intent(IN)    :: epoint(*) 
!!     Integer,         Intent(IN)    :: inbpnt1 
!!     Integer,         Intent(IN)    :: inbpnt2 
!!     Integer,         Intent(IN)    :: ipar 
!!     Integer,         Intent(IN)    :: iopen1 
!!     Integer,         Intent(IN)    :: iopen2 
!!     Integer,         Intent(IN)    :: ik1 
!!     Integer,         Intent(IN)    :: ik2 
!!     Integer,         Intent(IN)    :: idim 
!!     Type(SISLsurf),  Intent(INOUT) :: rs
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Real(REAL64),    Intent(IN)    :: epoint(*) 
    Integer,         Intent(IN)    :: inbpnt1 
    Integer,         Intent(IN)    :: inbpnt2 
    Integer,         Intent(IN)    :: ipar 
    Integer,         Intent(IN)    :: iopen1 
    Integer,         Intent(IN)    :: iopen2 
    Integer,         Intent(IN)    :: ik1 
    Integer,         Intent(IN)    :: ik2 
    Integer,         Intent(IN)    :: idim 
    Type(SISLsurf),  Intent(INOUT) :: rs
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_inbpnt1, c_inbpnt2, c_idim, c_ipar, c_iopen1,          &
                      c_iopen2, c_ik1, c_ik2, c_jstat

    Interface
      Subroutine c_s1620(epoint, inbpnt1, inbpnt2, ipar, iopen1, iopen2, ik1,  &
                   ik2, idim, rs, jstat)                                       &
                   BIND(C, name="s1620")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Real(C_DOUBlE),       Intent(IN)    :: epoint(*)
        Integer(C_INT), VALUE               :: inbpnt1 
        Integer(C_INT), VALUE               :: inbpnt2 
        Integer(C_INT), VALUE               :: ipar 
        Integer(C_INT), VALUE               :: iopen1 
        Integer(C_INT), VALUE               :: iopen2 
        Integer(C_INT), VALUE               :: ik1 
        Integer(C_INT), VALUE               :: ik2 
        Integer(C_INT), VALUE               :: idim 
        Type(C_PTR),          Intent(INOUT) :: rs
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1620
    End Interface

    c_inbpnt1 = inbpnt1 
    c_inbpnt2 = inbpnt2 
    c_idim    = idim
    c_ipar    = ipar
    c_iopen1  = iopen1
    c_iopen2  = iopen2
    c_ik1     = ik1
    c_ik2     = ik2
    Call c_s1620(epoint, c_inbpnt1, inbpnt2, c_ipar, c_iopen1, c_iopen2,       &
                 c_ik1, c_ik2, c_idim, rs%cptr,  c_jstat)

    Call surfCtoF(rs)

    jstat      = c_jstat

  End Subroutine s1620

!---------------------------------- s1332 -------------------------------------

  Subroutine s1332(curve1, curve2, epsge, point, surf, stat) 

!! PURPOSE
!!   s1332 - To create a linear swept surface by making the tensor-product of
!!           two curves.

!! INTERFACE
!!   Subroutine s1332(curve1, curve2, epsge, point, surf, stat) 
!!     Type(SISLcurve), Intent(IN)    :: curve1
!!     Type(SISLcurve), Intent(IN)    :: curve2
!!     Real(REAL64),    Intent(IN)    :: epsge 
!!     Real(REAL64),    Intent(IN)    :: point(*) 
!!     Type(SISLsurf),  Intent(INOUT) :: surf
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: curve1
    Type(SISLcurve), Intent(IN)    :: curve2
    Real(REAL64),    Intent(IN)    :: epsge 
    Real(REAL64),    Intent(IN)    :: point(*) 
    Type(SISLsurf),  Intent(INOUT) :: surf
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT)  :: c_stat

    Interface
    Subroutine c_s1332(curve1, curve2, epsge, point, surf, stat)               &
                       BIND(C,name="s1332")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: curve1 
        Type(C_PTR),    VALUE               :: curve2 
        Real(C_DOUBLE), VALUE               :: epsge 
        Real(C_DOUBLE),       Intent(IN)    :: point(*) 
        Type(C_PTR),          Intent(INOUT) :: surf 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1332
    End Interface
 
    Call c_s1332(curve1%cptr, curve2%cptr, epsge, point, surf%cptr, c_stat) 

    Call surfCtoF(surf)

    stat = c_stat

  End Subroutine s1332

!---------------------------------- s1302 -------------------------------------

  Subroutine s1302(curve, epsge, angle, point, axis, surf, stat)

!! PURPOSE
!!   s1302 - To create a rotational swept surface by rotating a curve a given
!!           angle around the axis defined by point[ ] and axis[ ]. The maxi-
!!           mal deviation allowed between the true rotational surface and the
!!           generated surface, is epsge. If epsge is set to 0, a NURBS surface
!!           is generated and if epsge > 0, a B-spline surface is generated.

!! INTERFACE
!!   Subroutine s1302(curve, epsge, angle, point, axis, surf, stat)
!!     Type(SISLcurve), Intent(IN)    :: curve
!!     Real(REAL64),    Intent(IN)    :: epsge 
!!     Real(REAL64),    Intent(IN)    :: angle 
!!     Real(REAL64),    Intent(IN)    :: point(*) 
!!     Real(REAL64),    Intent(IN)    :: axis(*) 
!!     Type(SISLsurf),  Intent(INOUT) :: surf
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: curve
    Real(REAL64),    Intent(IN)    :: epsge 
    Real(REAL64),    Intent(IN)    :: angle 
    Real(REAL64),    Intent(IN)    :: point(*) 
    Real(REAL64),    Intent(IN)    :: axis(*) 
    Type(SISLsurf),  Intent(INOUT) :: surf
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT)  :: c_stat

    Interface
    Subroutine c_s1302(curve, epsge, angle, point, axis, surf, stat)           &
                       BIND(C,name="s1302")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: curve 
        Real(C_DOUBLE), VALUE               :: epsge 
        Real(C_DOUBLE), VALUE               :: angle 
        Real(C_DOUBLE),       Intent(IN)    :: point(*) 
        Real(C_DOUBLE),       Intent(IN)    :: axis(*) 
        Type(C_PTR),          Intent(INOUT) :: surf 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1302
    End Interface
 
    Call c_s1302(curve%cptr, epsge, angle, point, axis, surf%cptr, c_stat)

    Call surfCtoF(surf)

    stat = c_stat

  End Subroutine s1302

!---------------------------------- s1365 -------------------------------------

  Subroutine s1365(ps, aoffset, aepsge, amax, idim, rs, jstat)

!! PURPOSE
!!   s1365 - Create a surface approximating the offset of a surface. The output
!!           is represented as a B-spline surface.
!!           With an offset of zero, this routine can be used to approximate any
!!           NURBS (rational) surface with a B-spline (non-rational) surface.

!! INTERFACE
!!   Subroutine s1365(ps, aoffset, aepsge, amax, idim, rs, jstat)
!!     Type(SISLsurf), Intent(IN)    :: ps 
!!     Real(REAL64),   Intent(IN)    :: aoffset 
!!     Real(REAL64),   Intent(IN)    :: aepsge 
!!     Real(REAL64),   Intent(IN)    :: amax 
!!     Integer,        Intent(IN)    :: idim 
!!     Type(SISLsurf), Intent(INOUT) :: rs
!!     Integer,        Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLsurf), Intent(IN)    :: ps 
    Real(REAL64),   Intent(IN)    :: aoffset 
    Real(REAL64),   Intent(IN)    :: aepsge 
    Real(REAL64),   Intent(IN)    :: amax 
    Integer,        Intent(IN)    :: idim 
    Type(SISLsurf), Intent(INOUT) :: rs
    Integer,        Intent(INOUT) :: jstat

    Integer(C_INT)  :: c_jstat

    Interface
    Subroutine c_s1365(ps, aoffset, aepsge, amax, idim, rs, jstat)             &
                       BIND(C,name="s1365")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: ps 
        Real(C_DOUBLE), VALUE               :: aoffset 
        Real(C_DOUBLE), VALUE               :: aepsge 
        Real(C_DOUBLE), VALUE               :: amax 
        Integer(C_INT), VALUE               :: idim 
        Type(C_PTR),          Intent(INOUT) :: rs 
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1365
    End Interface
 
    Call c_s1365(ps%cptr, aoffset, aepsge, amax, idim, rs%cptr, c_jstat)

    Call surfCtoF(rs)

    jstat = c_jstat

  End Subroutine s1365

!---------------------------------- s1601 -------------------------------------

  Subroutine s1601(psurf, epoint, enorm, idim, rsurf, stat)

!! PURPOSE
!!   s1601 - Mirror a surface about a plane.

!! INTERFACE
!!   Subroutine s1601(psurf, epoint, enorm, idim, rsurf, stat)
!!     Type(SISLsurf), Intent(IN)    :: psurf
!!     Real(REAL64),   Intent(IN)    :: epoint(*) 
!!     Real(REAL64),   Intent(IN)    :: enorm(*) 
!!     Integer,        Intent(IN)    :: idim 
!!     Type(SISLsurf), Intent(INOUT) :: rsurf
!!     Integer,        Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf), Intent(IN)    :: psurf
    Real(REAL64),   Intent(IN)    :: epoint(*) 
    Real(REAL64),   Intent(IN)    :: enorm(*) 
    Integer,        Intent(IN)    :: idim 
    Type(SISLsurf), Intent(INOUT) :: rsurf
    Integer,        Intent(INOUT) :: stat

    Integer(C_INT)  :: c_stat

    Interface
    Subroutine c_s1601(psurf, epoint, enorm, idim, rsurf, stat)                &
                       BIND(C,name="s1601")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: psurf 
        Real(C_DOUBLE),       Intent(IN)    :: epoint(*) 
        Real(C_DOUBLE),       Intent(IN)    :: enorm(*) 
        Integer(C_INT), VALUE               :: idim 
        Type(C_PTR),          Intent(INOUT) :: rsurf 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1601
    End Interface
 
    Call c_s1601(psurf%cptr, epoint, enorm, idim, rsurf%cptr, c_stat)

    stat = c_stat

  End Subroutine s1601

!---------------------------------- s1388 -------------------------------------

  Subroutine s1388(surf, coons, numcoons1, numcoons2, dim, stat)

!! PURPOSE
!!   s1388 - To convert a surface of order less than or equal to 4 in both direc
!!           -tions to a mesh of Coons patches with uniform parameterization.
!!           This subroutine assumes that the surface is C1 continuous.

!! INTERFACE
!!   Subroutine s1388(surf, coons, numcoons1, numcoons2, dim, stat)
!!     Type(SISLsurf),              Intent(IN)    :: surf
!!     Real(REAL64),   ALLOCATABLE, Intent(INOUT) :: coons(:)
!!     Integer,                     Intent(INOUT) :: numcoons1 
!!     Integer,                     Intent(INOUT) :: numcoons2 
!!     Integer,                     Intent(INOUT) :: dim 
!!     Integer,                     Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf),              Intent(IN)    :: surf
    Real(REAL64),   ALLOCATABLE, Intent(INOUT) :: coons(:)
    Integer,                     Intent(INOUT) :: numcoons1 
    Integer,                     Intent(INOUT) :: numcoons2 
    Integer,                     Intent(INOUT) :: dim 
    Integer,                     Intent(INOUT) :: stat

    Integer                :: nc
    Integer(C_INT)         :: c_numcoons1, c_numcoons2, c_dim, c_stat
    Type(C_PTR)            :: c_coons_p
    Real(REAL64),  Pointer :: rcoons_p(:)

    Interface
    Subroutine c_s1388(surf, coons, numcoons1, numcoons2, dim, stat)           &
                       BIND(C,name="s1388")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf 
        Type(C_PTR),          Intent(INOUT) :: coons 
        Integer(C_INT),       Intent(INOUT) :: numcoons1 
        Integer(C_INT),       Intent(INOUT) :: numcoons2 
        Integer(C_INT),       Intent(INOUT) :: dim 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1388
    End Interface
 
    Call c_s1388(surf%cptr, c_coons_p, c_numcoons1, c_numcoons2, c_dim,     &
                 c_stat)

    numcoons1 = c_numcoons1
    numcoons2 = c_numcoons2
    dim       = c_dim
    nc        = 16*dim*numcoons1*numcoons2

    If (C_ASSOCIATED(c_coons_p)) Then
      Call C_F_POINTER(c_coons_p, rcoons_p, [nc])
      If (ASSOCIATED(rcoons_p)) Then
        If(ALLOCATED(coons)) DEALLOCATE(coons) 
        ALLOCATE(coons(nc), SOURCE=0.0_REAL64)
        coons(:) = rcoons_p(1:nc)
        NULLIFY(rcoons_p)
        Call c_free(c_coons_p)
      End If
    End If  
    stat = c_stat

  End Subroutine s1388

!---------------------------------- s1731 -------------------------------------

  Subroutine s1731(surf, newsurf, stat) 

!! PURPOSE
!!   s1731 - To convert a surface to a mesh of Bezier surfaces. The Bezier
!!           surfaces are stored in a surface with all knots having multiplicity
!!           equal to the order of the surface in the corresponding parameter
!!           direction. If the input surface is rational, the generated Bezier
!!           surfaces will be rational too (i.e. there will be rational weights
!!           in the representation of the Bezier surfaces).

!! INTERFACE
!!   Subroutine s1731(surf, newsurf, stat) 
!!     Type(SISLsurf), Intent(IN)    :: surf 
!!     Type(SISLsurf), Intent(INOUT) :: newsurf
!!     Integer,        Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf), Intent(IN)    :: surf 
    Type(SISLsurf), Intent(INOUT) :: newsurf
    Integer,        Intent(INOUT) :: stat

    Integer(C_INT)        :: c_stat
    
    Interface
    Subroutine c_s1731(surf, newsurf, stat)                                    &
                       BIND(C,name="s1731")

        IMPORT :: C_PTR, C_INT
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf 
        Type(C_PTR),          Intent(INOUT) :: newsurf 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1731
    End Interface

    Call c_s1731(surf%cptr, newsurf%cptr, c_stat) 

    Call surfCtoF(newsurf)

    stat = c_stat

  End Subroutine s1731

!---------------------------------- s1733 -------------------------------------

  Subroutine s1733(surf, number1, number2, startpar1, endpar1, startpar2,      &
                   endpar2, coef, stat)

!! PURPOSE
!!   s1733 - To pick the next Bezier surface from a surface. This function
!!           requires a surface represented as the result of s1731(). See page
!!           209. This routine does not check that the surface is correct. If
!!           the input surface is rational, the generated Bezier surfaces will
!!           be rational too (i.e. there will be rational weights in the
!!           representation of the Bezier surfaces).

!! INTERFACE
!!   Subroutine s1733(surf, number1, number2, startpar1, endpar1, startpar2,   &
!!                    endpar2, coef, stat)
!!     Type(SISLsurf), Intent(IN)    :: surf
!!     Integer,        Intent(IN)    :: number1 
!!     Integer,        Intent(IN)    :: number2 
!!     Real(REAL64),   Intent(INOUT) :: startpar1 
!!     Real(REAL64),   Intent(INOUT) :: endpar1 
!!     Real(REAL64),   Intent(INOUT) :: startpar2 
!!     Real(REAL64),   Intent(INOUT) :: endpar2 
!!     Real(REAL64),   Intent(INOUT) :: coef(*)
!!     Integer,        Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf), Intent(IN)    :: surf
    Integer,        Intent(IN)    :: number1 
    Integer,        Intent(IN)    :: number2 
    Real(REAL64),   Intent(INOUT) :: startpar1 
    Real(REAL64),   Intent(INOUT) :: endpar1 
    Real(REAL64),   Intent(INOUT) :: startpar2 
    Real(REAL64),   Intent(INOUT) :: endpar2 
    Real(REAL64),   Intent(INOUT) :: coef(*)
    Integer,        Intent(INOUT) :: stat

    Integer(C_INT)         :: c_number1, c_number2, c_stat 

    Interface
    Subroutine c_s1733(surf, number1, number2, startpar1, endpar1, startpar2,  &
                       endpar2, coef, stat)                                    &
                       BIND(C,name="s1733")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf 
        Integer(C_INT), VALUE               :: number1 
        Integer(C_INT), VALUE               :: number2 
        Real(C_DOUBLE),       Intent(INOUT) :: startpar1 
        Real(C_DOUBLE),       Intent(INOUT) :: endpar1 
        Real(C_DOUBLE),       Intent(INOUT) :: startpar2 
        Real(C_DOUBLE),       Intent(INOUT) :: endpar2 
        Real(C_DOUBLE),       Intent(INOUT) :: coef(*) 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1733
    End Interface

    c_number1 = number1 
    c_number2 = number2 

    coef(1:((surf%idim+1)*surf%ik1*surf%ik2)) = 0.0_REAL64

    Call c_s1733(surf%cptr, c_number1, c_number2, startpar1, endpar1,          &
                 startpar2, endpar2, coef, c_stat)

    stat = c_stat

  End Subroutine s1733

!---------------------------------- s1387 -------------------------------------

  Subroutine s1387(surf, order1, order2, newsurf, stat) 

!! PURPOSE
!!   s1387 - To express a surface as a surface of higher order.

!! INTERFACE
!!   Subroutine s1387(surf, order1, order2, newsurf, stat) 
!!     Type(SISLsurf), Intent(IN)    :: surf 
!!     Integer,        Intent(IN)    :: order1 
!!     Integer,        Intent(IN)    :: order2 
!!     Type(SISLsurf), Intent(INOUT) :: newsurf
!!     Integer,        Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf), Intent(IN)    :: surf 
    Integer,        Intent(IN)    :: order1 
    Integer,        Intent(IN)    :: order2 
    Type(SISLsurf), Intent(INOUT) :: newsurf
    Integer,        Intent(INOUT) :: stat

    Integer(C_INT)  :: c_order1, c_order2, c_stat
    
    Interface
    Subroutine c_s1387(surf, order1, order2, newsurf, stat)                    &
                       BIND(C,name="s1387")

        IMPORT :: C_PTR, C_INT
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf 
        Integer(C_INT), VALUE               :: order1 
        Integer(C_INT), VALUE               :: order2 
        Type(C_PTR),          Intent(INOUT) :: newsurf 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1387
    End Interface

    c_order1 = order1
    c_order2 = order2
    Call c_s1387(surf%cptr, c_order1, c_order2, newsurf%cptr, c_stat) 

    Call surfCtoF(newsurf)

    stat = c_stat

  End Subroutine s1387

!---------------------------------- s1386 -------------------------------------

  Subroutine s1386(surf, der1, der2, newsurf, stat) 

!! PURPOSE
!!   s1386 - To express the (der1, der2)-th derivative of an open surface as a
!!           surface.

!! INTERFACE
!!   Subroutine s1386(surf, der1, der2, newsurf, stat) 
!!     Type(SISLsurf), Intent(IN)    :: surf 
!!     Integer,        Intent(IN)    :: der1 
!!     Integer,        Intent(IN)    :: der2 
!!     Type(SISLsurf), Intent(INOUT) :: newsurf
!!     Integer,        Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf), Intent(IN)    :: surf 
    Integer,        Intent(IN)    :: der1 
    Integer,        Intent(IN)    :: der2 
    Type(SISLsurf), Intent(INOUT) :: newsurf
    Integer,        Intent(INOUT) :: stat

    Integer(C_INT)  :: c_der1, c_der2, c_stat
    
    Interface
    Subroutine c_s1386(surf, der1, der2, newsurf, stat)                        &
                       BIND(C,name="s1386")

        IMPORT :: C_PTR, C_INT
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf 
        Integer(C_INT), VALUE               :: der1 
        Integer(C_INT), VALUE               :: der2 
        Type(C_PTR),          Intent(INOUT) :: newsurf 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1386
    End Interface

    c_der1 = der1
    c_der2 = der2
    Call c_s1386(surf%cptr, c_der1, c_der2, newsurf%cptr, c_stat) 

    Call surfCtoF(newsurf)

    stat = c_stat

  End Subroutine s1386

!---------------------------------- s1023 -------------------------------------

  Subroutine s1023(centre, axis, equator, latitude, longitude, sphere, stat)

!! PURPOSE
!!   s1023 - To express the octants of a sphere as a surface. This can also be
!!           used to describe the complete sphere. The sphere/the octants of
!!           the sphere will be geometrically exact.

!! INTERFACE
!!   Subroutine s1023(centre, axis, equator, latitude, longitude, sphere, stat)
!!     Real(REAL64),   Intent(IN)    :: centre(*)
!!     Real(REAL64),   Intent(IN)    :: axis(*)
!!     Real(REAL64),   Intent(IN)    :: equator(*)
!!     Integer,        Intent(IN)    :: latitude 
!!     Integer,        Intent(IN)    :: longitude 
!!     Type(SISLsurf), Intent(INOUT) :: sphere
!!     Integer,        Intent(INOUT) :: stat

    Implicit NONE

    Real(REAL64),   Intent(IN)    :: centre(*)
    Real(REAL64),   Intent(IN)    :: axis(*)
    Real(REAL64),   Intent(IN)    :: equator(*)
    Integer,        Intent(IN)    :: latitude 
    Integer,        Intent(IN)    :: longitude 
    Type(SISLsurf), Intent(INOUT) :: sphere
    Integer,        Intent(INOUT) :: stat

    Integer(C_INT)  :: c_stat, c_latitude, c_longitude

    Interface
    Subroutine c_s1023(centre, axis, equator, latitude, longitude, sphere,     &
                       stat) BIND(C,name="s1023")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Real(C_DOUBLE),       Intent(IN)    :: centre(*) 
        Real(C_DOUBLE),       Intent(IN)    :: axis(*) 
        Real(C_DOUBLE),       Intent(IN)    :: equator(*) 
        Integer(C_INT), VALUE               :: latitude 
        Integer(C_INT), VALUE               :: longitude 
        Type(C_PTR),          Intent(INOUT) :: sphere 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1023
    End Interface

    c_latitude  = latitude
    c_longitude = longitude 
    Call c_s1023( centre, axis, equator, c_latitude, c_longitude, sphere%cptr, &
                 c_stat)

    Call surfCtoF(sphere)

    stat = c_stat

  End Subroutine s1023

!---------------------------------- s1021 -------------------------------------

  Subroutine s1021(bottom_pos, bottom_axis, ellipse_ratio, axis_dir, height,   &
                   cyl, stat)

!! PURPOSE
!!   s1021 - To express a truncated cylinder as a surface. The cylinder can be
!!           elliptic. The cylinder will be geometrically exact.

!! INTERFACE
!!   Subroutine s1021(bottom_pos, bottom_axis, ellipse_ratio, axis_dir, height,&
!!                    cyl, stat)
!!     Real(REAL64),   Intent(IN)    :: bottom_pos(*)
!!     Real(REAL64),   Intent(IN)    :: bottom_axis(*)
!!     Real(REAL64),   Intent(IN)    :: ellipse_ratio 
!!     Real(REAL64),   Intent(IN)    :: axis_dir(*)
!!     Real(REAL64),   Intent(IN)    :: height 
!!     Type(SISLsurf), Intent(INOUT) :: cyl
!!     Integer,        Intent(INOUT) :: stat

    Implicit NONE

    Real(REAL64),   Intent(IN)    :: bottom_pos(*)
    Real(REAL64),   Intent(IN)    :: bottom_axis(*)
    Real(REAL64),   Intent(IN)    :: ellipse_ratio 
    Real(REAL64),   Intent(IN)    :: axis_dir(*)
    Real(REAL64),   Intent(IN)    :: height 
    Type(SISLsurf), Intent(INOUT) :: cyl
    Integer,        Intent(INOUT) :: stat

    Integer(C_INT)  :: c_stat

    Interface
    Subroutine c_s1021(bottom_pos, bottom_axis, ellipse_ratio, axis_dir,       &
                       height, cyl, stat)                                      &
                       BIND(C,name="s1021")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Real(C_DOUBLE),       Intent(IN)    :: bottom_pos(*) 
        Real(C_DOUBLE),       Intent(IN)    :: bottom_axis(*) 
        Real(C_DOUBLE), VALUE               :: ellipse_ratio 
        Real(C_DOUBLE),       Intent(IN)    :: axis_dir(*) 
        Real(C_DOUBLE), VALUE               :: height 
        Type(C_PTR),          Intent(INOUT) :: cyl 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1021
    End Interface
 
    Call c_s1021( bottom_pos, bottom_axis, ellipse_ratio, axis_dir, height,    &
                 cyl%cptr, c_stat)

    Call surfCtoF(cyl)

    stat = c_stat

  End Subroutine s1021

!---------------------------------- s1024 -------------------------------------

  Subroutine s1024(centre, axis, equator, minor_radius, start_minor,           &
                   end_minor, numb_major, torus, stat)

!! PURPOSE
!!   s1024 - To express the octants of a torus as a surface. This can also be
!!           used to describe the complete torus. The torus/the octants of the
!!           torus will be geometrically exact.

!! INTERFACE
!!   Subroutine s1024(centre, axis, equator, minor_radius, start_minor,        &
!!                    end_minor, numb_major, torus, stat)
!!     Real(REAL64),   Intent(IN)    :: centre(*)
!!     Real(REAL64),   Intent(IN)    :: axis(*)
!!     Real(REAL64),   Intent(IN)    :: equator(*)
!!     Real(REAL64),   Intent(IN)    :: minor_radius 
!!     Integer,        Intent(IN)    :: start_minor 
!!     Integer,        Intent(IN)    :: end_minor 
!!     Integer,        Intent(IN)    :: numb_major 
!!     Type(SISLsurf), Intent(INOUT) :: torus
!!     Integer,        Intent(INOUT) :: stat

    Implicit NONE

    Real(REAL64),   Intent(IN)    :: centre(*)
    Real(REAL64),   Intent(IN)    :: axis(*)
    Real(REAL64),   Intent(IN)    :: equator(*)
    Real(REAL64),   Intent(IN)    :: minor_radius 
    Integer,        Intent(IN)    :: start_minor 
    Integer,        Intent(IN)    :: end_minor 
    Integer,        Intent(IN)    :: numb_major 
    Type(SISLsurf), Intent(INOUT) :: torus
    Integer,        Intent(INOUT) :: stat

    Integer(C_INT)  :: c_start_minor, c_end_minor, c_numb_major, c_stat

    Interface
    Subroutine c_s1024(centre, axis, equator, minor_radius, start_minor,       &
                       end_minor,numb_major, torus, stat)                      &
                       BIND(C,name="s1024")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Real(C_DOUBLE),       Intent(IN)    :: centre(*) 
        Real(C_DOUBLE),       Intent(IN)    :: axis(*) 
        Real(C_DOUBLE),       Intent(IN)    :: equator(*) 
        Real(C_DOUBLE), VALUE               :: minor_radius 
        Integer(C_INT), VALUE               :: start_minor 
        Integer(C_INT), VALUE               :: end_minor 
        Integer(C_INT), VALUE               :: numb_major 
        Type(C_PTR),          Intent(INOUT) :: torus 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1024
    End Interface

    c_start_minor = start_minor
    c_end_minor   = end_minor
    c_numb_major  = numb_major 
    Call c_s1024(centre, axis, equator, minor_radius, c_start_minor,           &
                 c_end_minor, c_numb_major,  torus%cptr, c_stat)

    Call surfCtoF(torus)

    stat = c_stat

  End Subroutine s1024

!---------------------------------- s1022 -------------------------------------

  Subroutine s1022(bottom_pos, bottom_axis, ellipse_ratio, axis_dir,           &
                   cone_angle, height, cone, stat)

!! PURPOSE
!!   s1022 - To express a truncated cone as a surface. The cone can be elliptic.
!!           The cone will be geometrically exact.

!! INTERFACE
!!   Subroutine s1022(bottom_pos, bottom_axis, ellipse_ratio, axis_dir,        &
!!                    cone_angle, height, cone, stat)
!!     Real(REAL64),   Intent(IN)    :: bottom_pos(*)
!!     Real(REAL64),   Intent(IN)    :: bottom_axis(*)
!!     Real(REAL64),   Intent(IN)    :: ellipse_ratio 
!!     Real(REAL64),   Intent(IN)    :: axis_dir(*)
!!     Real(REAL64),   Intent(IN)    :: cone_angle 
!!     Real(REAL64),   Intent(IN)    :: height 
!!     Type(SISLsurf), Intent(INOUT) :: cone
!!     Integer,        Intent(INOUT) :: stat

    Implicit NONE

    Real(REAL64),   Intent(IN)    :: bottom_pos(*)
    Real(REAL64),   Intent(IN)    :: bottom_axis(*)
    Real(REAL64),   Intent(IN)    :: ellipse_ratio 
    Real(REAL64),   Intent(IN)    :: axis_dir(*)
    Real(REAL64),   Intent(IN)    :: cone_angle 
    Real(REAL64),   Intent(IN)    :: height 
    Type(SISLsurf), Intent(INOUT) :: cone
    Integer,        Intent(INOUT) :: stat

    Integer(C_INT)  :: c_stat

    Interface
    Subroutine c_s1022(bottom_pos, bottom_axis, ellipse_ratio, axis_dir,       &
                       cone_angle, height, cone, stat)                         &
                       BIND(C,name="s1022")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Real(C_DOUBLE),       Intent(IN)    :: bottom_pos(*) 
        Real(C_DOUBLE),       Intent(IN)    :: bottom_axis(*) 
        Real(C_DOUBLE), VALUE               :: ellipse_ratio 
        Real(C_DOUBLE),       Intent(IN)    :: axis_dir(*) 
        Real(C_DOUBLE), VALUE               :: cone_angle 
        Real(C_DOUBLE), VALUE               :: height 
        Type(C_PTR),          Intent(INOUT) :: cone 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1022
    End Interface
 
   Call c_s1022( bottom_pos, bottom_axis, ellipse_ratio, axis_dir,             &
                 cone_angle, height, cone%cptr, c_stat)

    Call surfCtoF(cone)

    stat = c_stat

  End Subroutine s1022

End Module Surface_Definition
