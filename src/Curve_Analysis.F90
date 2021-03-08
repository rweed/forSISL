
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


Module Curve_Analysis

!! Module Curve_Analysis contains Modern Fortran C-interoperability routines
!! for the C Routines described in Chapter 4 of version 4.4 of the SISL
!! reference manual

!! Written by: Richard Weed, Ph.D.
!! Version no. 1.0 - Feb. 2021
!!   Initial development version

!! Contact: rweedmsu@gmail.com

  USE forSISLdata, ONLY: REAL64, C_INT, C_DOUBLE, C_PTR, SISLcurve

  Implicit NONE

  PRIVATE :: REAL64, C_INT, C_DOUBLE, C_PTR, SISLcurve

Contains

!---------------------------------- s2550 -------------------------------------

  Subroutine s2550(curve, ax, num_ax, curvature, jstat)

!! PURPOSE
!!   s2550 - Evaluate the curvature of a curve at given parameter values ax( 1
!!           ),...,ax(num_ax ).

!! INTERFACE
!!   Subroutine s2550(curve, ax, num_ax, curvature, jstat)
!!     Type(SISLcurve), Intent(IN)    :: curve
!!     Real(REAL64),    Intent(IN)    :: ax(*)
!!     Integer,         Intent(IN)    :: num_ax 
!!     Real(REAL64),    Intent(INOUT) :: curvature(*)
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: curve
    Real(REAL64),    Intent(IN)    :: ax(*)
    Integer,         Intent(IN)    :: num_ax 
    Real(REAL64),    Intent(INOUT) :: curvature(*)
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_num_ax, c_jstat

! C interface

    Interface
      Subroutine c_s2550(curve, ax, num_ax, curvature, jstat)                  &
                         BIND(C,name="s2550")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: curve
        Real(C_DOUBLE),       Intent(IN)    :: ax(*)
        Integer(C_INT), VALUE               :: num_ax 
        Real(C_DOUBLE),       Intent(INOUT) :: curvature(*)
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s2550
    End Interface

    c_num_ax = num_ax

    curvature(1:num_ax) = 0.0_REAL64
    Call c_s2550(curve%cptr, ax, c_num_ax, curvature, c_jstat)

    jstat = c_jstat

  End Subroutine s2550

!---------------------------------- s2553 -------------------------------------

  Subroutine s2553(curve, ax, num_ax, torsion, jstat)

!! PURPOSE
!!   s2553 - Evaluate the torsion of a curve at given parameter values ax( 1
!!           ),...,ax( num_ax ).

!! INTERFACE
!!   Subroutine s2553(curve, ax, num_ax, torsion, jstat)
!!     Type(SISLcurve), Intent(IN)    :: curve
!!     Real(REAL64),    Intent(IN)    :: ax(*)
!!     Integer,         Intent(IN)    :: num_ax 
!!     Real(REAL64),    Intent(INOUT) :: torsion(*)
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: curve
    Real(REAL64),    Intent(IN)    :: ax(*)
    Integer,         Intent(IN)    :: num_ax 
    Real(REAL64),    Intent(INOUT) :: torsion(*)
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_num_ax, c_jstat

! C interface

    Interface
      Subroutine c_s2553(curve, ax, num_ax, torsion, jstat)                  &
                         BIND(C,name="s2553")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: curve
        Real(C_DOUBLE),       Intent(IN)    :: ax(*)
        Integer(C_INT), VALUE               :: num_ax 
        Real(C_DOUBLE),       Intent(INOUT) :: torsion(*)
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s2553
    End Interface

    c_num_ax          = num_ax
    torsion(1:num_ax) = 0.0_REAL64
    Call c_s2553(curve%cptr, ax, c_num_ax, torsion, c_jstat)

    jstat = c_jstat

  End Subroutine s2553

!---------------------------------- s2556 -------------------------------------

  Subroutine s2556(curve, ax, num_ax, VoC, jstat)

!! PURPOSE
!!   s2556 - Evaluate the Variation of Curvature (VoC) of a curve at given
!!           parameter values ax( 1 ),...,ax( num_ax ).

!! INTERFACE
!!   Subroutine s2556(curve, ax, num_ax, VoC, jstat)
!!     Type(SISLcurve), Intent(IN)    :: curve
!!     Real(REAL64),    Intent(IN)    :: ax(*)
!!     Integer,         Intent(IN)    :: num_ax 
!!     Real(REAL64),    Intent(INOUT) :: VoC(*)
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: curve
    Real(REAL64),    Intent(IN)    :: ax(*)
    Integer,         Intent(IN)    :: num_ax 
    Real(REAL64),    Intent(INOUT) :: VoC(*)
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_num_ax, c_jstat

! C Interface

    Interface
      Subroutine c_s2556(curve, ax, num_ax, VoC, jstat)                        &
                         BIND(C,name="s2556")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: curve
        Real(C_DOUBLE),       Intent(IN)    :: ax(*)
        Integer(C_INT), VALUE               :: num_ax 
        Real(C_DOUBLE),       Intent(INOUT) :: VoC(*)
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s2556
    End Interface

    c_num_ax      = num_ax
    VoC(1:num_ax) = 0.0_REAL64
    Call c_s2556(curve%cptr, ax, c_num_ax, VoC, c_jstat)

    jstat = c_jstat

  End Subroutine s2556

!---------------------------------- s2559 -------------------------------------

  Subroutine s2559(curve, ax, num_ax, p, t, n, b, jstat)

!! PURPOSE
!!   s2559 - Evaluate the Frenet Frame (t,n,b) of a curve at given parameter
!!           values ax( 1 ),...,ax( num_ax).

!! INTERFACE
!!   Subroutine s2559(curve, ax, num_ax, p, t, n, b, jstat)
!!     Type(SISLcurve), Intent(IN)    :: curve
!!     Real(REAL64),    Intent(IN)    :: ax(*)
!!     Integer,         Intent(IN)    :: num_ax 
!!     Real(REAL64),    Intent(INOUT) :: p(*)
!!     Real(REAL64),    Intent(INOUT) :: t(*)
!!     Real(REAL64),    Intent(INOUT) :: n(*)
!!     Real(REAL64),    Intent(INOUT) :: b(*)
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: curve
    Real(REAL64),    Intent(IN)    :: ax(*)
    Integer,         Intent(IN)    :: num_ax 
    Real(REAL64),    Intent(INOUT) :: p(*)
    Real(REAL64),    Intent(INOUT) :: t(*)
    Real(REAL64),    Intent(INOUT) :: n(*)
    Real(REAL64),    Intent(INOUT) :: b(*)
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_num_ax, c_jstat

! C interface

    Interface
      Subroutine c_s2559(curve, ax, num_ax, p, t, n, b, jstat)                 &
                         BIND(C,name="s2559")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: curve
        Real(C_DOUBLE),       Intent(IN)    :: ax(*)
        Integer(C_INT), VALUE               :: num_ax 
        Real(C_DOUBLE),       Intent(INOUT) :: p(*) 
        Real(C_DOUBLE),       Intent(INOUT) :: t(*) 
        Real(C_DOUBLE),       Intent(INOUT) :: n(*) 
        Real(C_DOUBLE),       Intent(INOUT) :: b(*) 
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s2559
    End Interface

    c_num_ax      = num_ax
    p(1:num_ax)   = 0.0_REAL64
    t(1:3*num_ax) = 0.0_REAL64
    n(1:3*num_ax) = 0.0_REAL64
    b(1:3*num_ax) = 0.0_REAL64
    Call c_s2559(curve%cptr, ax, c_num_ax, p, t, n, b, c_jstat)

    jstat = c_jstat

  End Subroutine s2559

!---------------------------------- s2562 -------------------------------------

  Subroutine s2562(curve, ax, num_ax, val_flag, p, t, n, b, val, jstat)

!! PURPOSE
!!   s2562 - Evaluate the 3D position, the Frenet Frame (t,n,b) and geometric
!!           property (curvature, torsion or variation of curvature) of a curve
!!           at given parameter values ax(1),...,ax(num_ ax). These data are
!!           needed to produce spike plots (using the Frenet Frame and the
!!           geometric property) and circular tube plots (using circular in the
!!           normal plane (t,b), where the radius is equal to the geometric
!!           property times a scaling factor for visual effects).

!! INTERFACE
!!   Subroutine s2562(curve, ax, num_ax, val_flag, p, t, n, b, val, jstat)
!!     Type(SISLcurve), Intent(IN)    :: curve
!!     Real(REAL64),    Intent(IN)    :: ax(*)
!!     Integer,         Intent(IN)    :: num_ax 
!!     Integer,         Intent(IN)    :: val_flag 
!!     Real(REAL64),    Intent(INOUT) :: p(*)
!!     Real(REAL64),    Intent(INOUT) :: t(*)
!!     Real(REAL64),    Intent(INOUT) :: n(*)
!!     Real(REAL64),    Intent(INOUT) :: b(*)
!!     Real(REAL64),    Intent(INOUT) :: val(*)
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: curve
    Real(REAL64),    Intent(IN)    :: ax(*)
    Integer,         Intent(IN)    :: num_ax 
    Integer,         Intent(IN)    :: val_flag 
    Real(REAL64),    Intent(INOUT) :: p(*)
    Real(REAL64),    Intent(INOUT) :: t(*)
    Real(REAL64),    Intent(INOUT) :: n(*)
    Real(REAL64),    Intent(INOUT) :: b(*)
    Real(REAL64),    Intent(INOUT) :: val(*)
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_num_ax, c_val_flag, c_jstat

! C interface

    Interface
      Subroutine c_s2562(curve, ax, num_ax, val_flag, p, t, n, b, val, jstat)  &
                         BIND(C,name="s2562")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: curve
        Real(C_DOUBLE),       Intent(IN)    :: ax(*)
        Integer(C_INT), VALUE               :: num_ax 
        Integer(C_INT), VALUE               :: val_flag 
        Real(C_DOUBLE),       Intent(INOUT) :: p(*) 
        Real(C_DOUBLE),       Intent(INOUT) :: t(*) 
        Real(C_DOUBLE),       Intent(INOUT) :: n(*) 
        Real(C_DOUBLE),       Intent(INOUT) :: b(*) 
        Real(C_DOUBLE),       Intent(INOUT) :: val(*) 
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s2562
    End Interface

    c_num_ax   = num_ax
    c_val_flag = val_flag

    p(1:num_ax)   = 0.0_REAL64
    t(1:3*num_ax) = 0.0_REAL64
    n(1:3*num_ax) = 0.0_REAL64
    b(1:3*num_ax) = 0.0_REAL64
    val(1:num_ax) = 0.0_REAL64

    Call c_s2562(curve%cptr, ax, c_num_ax, c_val_flag, p, t, n, b, val,        &
                 c_jstat)

    jstat = c_jstat

  End Subroutine s2562

End Module Curve_Analysis
