
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

Module Surface_Analysis

!! Module Surface_Analysis contains Modern Fortran C-interoperability routines
!! for the C functions described in Chapter 8 of version 4.4 of the SISL
!! reference manual

!! Written by: Richard Weed, Ph.D.
!! Version no. 1.0 - Feb. 2021
!!   Initial development version

!! Contact: rweedmsu@gmail.com

  USE forSISLdata, ONLY: REAL64, C_INT, C_DOUBLE, C_PTR, C_NULL_PTR, c_free,   &
                         C_F_POINTER, C_ASSOCIATED, SISLsurf, surfCtoF

  Implicit NONE

  PRIVATE :: REAL64, C_INT, C_DOUBLE, C_PTR, C_NULL_PTR, c_free,               &
             C_F_POINTER, C_ASSOCIATED, SISLsurf, surfCtoF

Contains

!---------------------------------- s2500 -------------------------------------

  Subroutine s2500(surf, ider, iside1, iside2, parvalue, leftknot1,            &
                   leftknot2, gaussian, jstat)

!! PURPOSE
!!   s2500 - To compute the Gaussian curvature K(u,v) of a spline surface at
!!           given values (u,v) = (parvalue(1),parvalue(2)), where et1(leftknot1+1)
!!           <= parvalue(1) < et1(leftknot1+2) and et2[leftknot2+1] <= par-
!!           value(2) < et2([leftknot2+2). See also s2501().

!! INTERFACE
!!   Subroutine s2500(surf, ider, iside1, iside2, parvalue, leftknot1,      &
!!                    leftknot2, gaussian, jstat)
!!     Type(SISLsurf),  Intent(IN)    :: surf
!!     Integer,         Intent(IN)    :: ider
!!     Integer,         Intent(IN)    :: iside1
!!     Integer,         Intent(IN)    :: iside2
!!     Real(REAL64),    Intent(IN)    :: parvalue(*)
!!     Integer,         Intent(INOUT) :: leftknot1
!!     Integer,         Intent(INOUT) :: leftknot2
!!     Real(REAL64),    Intent(INOUT) :: gaussian 
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLsurf),  Intent(IN)    :: surf
    Integer,         Intent(IN)    :: ider
    Integer,         Intent(IN)    :: iside1
    Integer,         Intent(IN)    :: iside2
    Real(REAL64),    Intent(IN)    :: parvalue(*)
    Integer,         Intent(INOUT) :: leftknot1
    Integer,         Intent(INOUT) :: leftknot2
    Real(REAL64),    Intent(INOUT) :: gaussian 
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_ider, c_jstat, c_iside1, c_iside2, c_leftknot1,        &
                      c_leftknot2

    Interface
      Subroutine c_s2500(surf, ider, iside1, iside2, parvalue, leftknot1,      &
                         leftknot2, gaussian, jstat)                           &
                         BIND(C, name="s2500")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf
        Integer(C_INT), VALUE               :: ider
        Integer(C_INT), VALUE               :: iside1
        Integer(C_INT), VALUE               :: iside2
        Real(C_DOUBLE),       Intent(IN)    :: parvalue(*)
        Integer(C_INT),       Intent(INOUT) :: leftknot1
        Integer(C_INT),       Intent(INOUT) :: leftknot2
        Real(C_DOUBLE),       Intent(INOUT) :: gaussian
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s2500
    End Interface

    c_ider        = ider
    c_iside1      = iside1
    c_iside2      = iside2
    c_leftknot1   = leftknot1
    c_leftknot2   = leftknot2

    Call c_s2500(surf%cptr, c_ider, c_iside1, c_iside2, parvalue,           &
                 c_leftknot1, c_leftknot2, gaussian, c_jstat)

    leftknot1 = c_leftknot1
    leftknot2 = c_leftknot2
    jstat     = c_jstat

  End Subroutine s2500
   
!---------------------------------- s2502 -------------------------------------

  Subroutine s2502(surf, ider, iside1, iside2, parvalue, leftknot1,          &
                   leftknot2, meancurvature, jstat)

!! PURPOSE
!!   s2502 - To compute the mean curvature H(u,v) of a spline surface at given
!!           values (u,v) = (parvalue(1),parvalue(2)), where etl[leftknot1+1] <=
!!           parvalue(1) < etl[leftknot1+2] and et2[leftknot2+1] <= parvalue(2)
!!           < et2[leftknot2+2].

!! INTERFACE
!!   Subroutine s2502(surf, ider, iside1, iside2, parvalue, leftknot1,      &
!!                    leftknot2, meancurvature, jstat)
!!     Type(SISLsurf),  Intent(IN)    :: surf
!!     Integer,         Intent(IN)    :: ider
!!     Integer,         Intent(IN)    :: iside1
!!     Integer,         Intent(IN)    :: iside2
!!     Real(REAL64),    Intent(IN)    :: parvalue(*)
!!     Integer,         Intent(INOUT) :: leftknot1
!!     Integer,         Intent(INOUT) :: leftknot2
!!     Real(REAL64),    Intent(INOUT) :: meancurvature 
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLsurf),  Intent(IN)    :: surf
    Integer,         Intent(IN)    :: ider
    Integer,         Intent(IN)    :: iside1
    Integer,         Intent(IN)    :: iside2
    Real(REAL64),    Intent(IN)    :: parvalue(*)
    Integer,         Intent(INOUT) :: leftknot1
    Integer,         Intent(INOUT) :: leftknot2
    Real(REAL64),    Intent(INOUT) :: meancurvature 
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_ider, c_jstat, c_iside1, c_iside2, c_leftknot1,        &
                      c_leftknot2

    Interface
      Subroutine c_s2502(surf, ider, iside1, iside2, parvalue, leftknot1,      &
                         leftknot2, meancurvature, jstat)                           &
                         BIND(C, name="s2502")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf
        Integer(C_INT), VALUE               :: ider
        Integer(C_INT), VALUE               :: iside1
        Integer(C_INT), VALUE               :: iside2
        Real(C_DOUBLE),       Intent(IN)    :: parvalue(*)
        Integer(C_INT),       Intent(INOUT) :: leftknot1
        Integer(C_INT),       Intent(INOUT) :: leftknot2
        Real(C_DOUBLE),       Intent(INOUT) :: meancurvature
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s2502
    End Interface

    c_ider        = ider
    c_iside1      = iside1
    c_iside2      = iside2
    c_leftknot1   = leftknot1
    c_leftknot2   = leftknot2

    Call c_s2502(surf%cptr, c_ider, c_iside1, c_iside2, parvalue,           &
                 c_leftknot1, c_leftknot2, meancurvature, c_jstat)

    leftknot1 = c_leftknot1
    leftknot2 = c_leftknot2
    jstat     = c_jstat

  End Subroutine s2502

!---------------------------------- s2504 -------------------------------------

  Subroutine s2504(surf, ider, iside1, iside2, parvalue, leftknot1,          &
                   leftknot2, absCurvature, jstat)

!! PURPOSE
!!   s2504 - To compute the absolute curvature A(u,v) of a spline surface at
!!           given values (u,v) = (parvalue(1l,parvalue(2)), where et1[leftknot1+1]
!!           <= parvalue(1) < et1[leftknot1+2] and et2[leftknot2+1] <= par-
!!           value(2) < et2[leftknot2+2].

!! INTERFACE
!!   Subroutine s2504(surf, ider, iside1, iside2, parvalue, leftknot1,      &
!!                    leftknot2, absCurvature, jstat)
!!     Type(SISLsurf),  Intent(IN)    :: surf
!!     Integer,         Intent(IN)    :: ider
!!     Integer,         Intent(IN)    :: iside1
!!     Integer,         Intent(IN)    :: iside2
!!     Real(REAL64),    Intent(IN)    :: parvalue(*)
!!     Integer,         Intent(INOUT) :: leftknot1
!!     Integer,         Intent(INOUT) :: leftknot2
!!     Real(REAL64),    Intent(INOUT) :: absCurvature 
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLsurf),  Intent(IN)    :: surf
    Integer,         Intent(IN)    :: ider
    Integer,         Intent(IN)    :: iside1
    Integer,         Intent(IN)    :: iside2
    Real(REAL64),    Intent(IN)    :: parvalue(*)
    Integer,         Intent(INOUT) :: leftknot1
    Integer,         Intent(INOUT) :: leftknot2
    Real(REAL64),    Intent(INOUT) :: absCurvature 
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_ider, c_jstat, c_iside1, c_iside2, c_leftknot1,        &
                      c_leftknot2

    Interface
      Subroutine c_s2504(surf, ider, iside1, iside2, parvalue, leftknot1,      &
                         leftknot2, absCurvature, jstat)                       &
                         BIND(C, name="s2504")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf
        Integer(C_INT), VALUE               :: ider
        Integer(C_INT), VALUE               :: iside1
        Integer(C_INT), VALUE               :: iside2
        Real(C_DOUBLE),       Intent(IN)    :: parvalue(*)
        Integer(C_INT),       Intent(INOUT) :: leftknot1
        Integer(C_INT),       Intent(INOUT) :: leftknot2
        Real(C_DOUBLE),       Intent(INOUT) :: absCurvature
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s2504
    End Interface

    c_ider        = ider
    c_iside1      = iside1
    c_iside2      = iside2
    c_leftknot1   = leftknot1
    c_leftknot2   = leftknot2

    Call c_s2504(surf%cptr, c_ider, c_iside1, c_iside2, parvalue,           &
                 c_leftknot1, c_leftknot2, absCurvature, c_jstat)

    leftknot1 = c_leftknot1
    leftknot2 = c_leftknot2
    jstat     = c_jstat

  End Subroutine s2504
 
!---------------------------------- s2506 -------------------------------------

  Subroutine s2506(surf, ider, iside1, iside2, parvalue, leftknot1,          &
                   leftknot2, totalCurvature, jstat)

!! PURPOSE
!!   s2506 - To compute the total curvature T(u,v) of a surface at given val-
!!           ues (u,v) = (parvalue[1],parvalue[2]), where et1[leftknot1+1] <= par-
!!           value(1) < et1[leftknot1+2] and et2[leftknot2+1] <= parvalue(2) <
!!           et2[leftknot2+2].

!! INTERFACE
!!   Subroutine s2506(surf, ider, iside1, iside2, parvalue, leftknot1,        &
!!                    leftknot2, totalCurvature, jstat)
!!     Type(SISLsurf),  Intent(IN)    :: surf
!!     Integer,         Intent(IN)    :: ider
!!     Integer,         Intent(IN)    :: iside1
!!     Integer,         Intent(IN)    :: iside2
!!     Real(REAL64),    Intent(IN)    :: parvalue(*)
!!     Integer,         Intent(INOUT) :: leftknot1
!!     Integer,         Intent(INOUT) :: leftknot2
!!     Real(REAL64),    Intent(INOUT) :: totalCurvature 
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLsurf),  Intent(IN)    :: surf
    Integer,         Intent(IN)    :: ider
    Integer,         Intent(IN)    :: iside1
    Integer,         Intent(IN)    :: iside2
    Real(REAL64),    Intent(IN)    :: parvalue(*)
    Integer,         Intent(INOUT) :: leftknot1
    Integer,         Intent(INOUT) :: leftknot2
    Real(REAL64),    Intent(INOUT) :: totalCurvature 
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_ider, c_jstat, c_iside1, c_iside2, c_leftknot1,        &
                      c_leftknot2

    Interface
      Subroutine c_s2506(surf, ider, iside1, iside2, parvalue, leftknot1,      &
                         leftknot2, totalCurvature, jstat)                           &
                         BIND(C, name="s2506")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf
        Integer(C_INT), VALUE               :: ider
        Integer(C_INT), VALUE               :: iside1
        Integer(C_INT), VALUE               :: iside2
        Real(C_DOUBLE),       Intent(IN)    :: parvalue(*)
        Integer(C_INT),       Intent(INOUT) :: leftknot1
        Integer(C_INT),       Intent(INOUT) :: leftknot2
        Real(C_DOUBLE),       Intent(INOUT) :: totalCurvature
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s2506
    End Interface

    c_ider        = ider
    c_iside1      = iside1
    c_iside2      = iside2
    c_leftknot1   = leftknot1
    c_leftknot2   = leftknot2

    Call c_s2506(surf%cptr, c_ider, c_iside1, c_iside2, parvalue,           &
                 c_leftknot1, c_leftknot2, totalCurvature, c_jstat)

    leftknot1 = c_leftknot1
    leftknot2 = c_leftknot2
    jstat     = c_jstat

  End Subroutine s2506

!---------------------------------- s2508 -------------------------------------

  Subroutine s2508(surf, ider, iside1, iside2, parvalue, leftknot1,          &
                   leftknot2, mehlum, jstat)

!! PURPOSE
!!   s2508 - To compute the second order Mehlum curvature M(u,v) of
!!           a surface at given values (u,v) = (parvalue(1),parvalue(2)),
!!           where et1(leftknot1+1) <= parvalue(1) < et1(leftknot1+2) and
!!           et2(leftknot2+1) <= parvalue(2) < et2(leftknot2+2). See also s2509

!! INTERFACE
!!   Subroutine s2508(surf, ider, iside1, iside2, parvalue, leftknot1,         &
!!                    leftknot2, mehlum, jstat)
!!     Type(SISLsurf),  Intent(IN)    :: surf
!!     Integer,         Intent(IN)    :: ider
!!     Integer,         Intent(IN)    :: iside1
!!     Integer,         Intent(IN)    :: iside2
!!     Real(REAL64),    Intent(IN)    :: parvalue(*)
!!     Integer,         Intent(INOUT) :: leftknot1
!!     Integer,         Intent(INOUT) :: leftknot2
!!     Real(REAL64),    Intent(INOUT) :: mehlum 
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLsurf),  Intent(IN)    :: surf
    Integer,         Intent(IN)    :: ider
    Integer,         Intent(IN)    :: iside1
    Integer,         Intent(IN)    :: iside2
    Real(REAL64),    Intent(IN)    :: parvalue(*)
    Integer,         Intent(INOUT) :: leftknot1
    Integer,         Intent(INOUT) :: leftknot2
    Real(REAL64),    Intent(INOUT) :: mehlum 
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_ider, c_jstat, c_iside1, c_iside2, c_leftknot1,        &
                      c_leftknot2

    Interface
      Subroutine c_s2508(surf, ider, iside1, iside2, parvalue, leftknot1,      &
                         leftknot2, mehlum, jstat)                             &
                         BIND(C, name="s2508")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf
        Integer(C_INT), VALUE               :: ider
        Integer(C_INT), VALUE               :: iside1
        Integer(C_INT), VALUE               :: iside2
        Real(C_DOUBLE),       Intent(IN)    :: parvalue(*)
        Integer(C_INT),       Intent(INOUT) :: leftknot1
        Integer(C_INT),       Intent(INOUT) :: leftknot2
        Real(C_DOUBLE),       Intent(INOUT) :: mehlum
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s2508
    End Interface

    c_ider        = ider
    c_iside1      = iside1
    c_iside2      = iside2
    c_leftknot1   = leftknot1
    c_leftknot2   = leftknot2

    Call c_s2508(surf%cptr, c_ider, c_iside1, c_iside2, parvalue,           &
                 c_leftknot1, c_leftknot2, mehlum, c_jstat)

    leftknot1 = c_leftknot1
    leftknot2 = c_leftknot2
    jstat     = c_jstat

  End Subroutine s2508

!---------------------------------- s2510 -------------------------------------

  Subroutine s2510(surf, ider, iside1, iside2, parvalue, leftknot1,          &
                   leftknot2, mehlum, jstat)

!! PURPOSE
!!   s2510 - To compute the third order Mehlum curvature M(u,v) of a
!!           surface at given values (u,v) = (parvalue(1),parvalue(2)), where
!!           et1(leftknot1+1) <= parvalue(1) < et1(leftknot1+2), et2(leftknot2+1)
!!           <= parvalue(2) < et2(leftknot2+2).

!! INTERFACE
!!   Subroutine s2510(surf, ider, iside1, iside2, parvalue, leftknot1,      &
!!                    leftknot2, mehlum, jstat)
!!     Type(SISLsurf),  Intent(IN)    :: surf
!!     Integer,         Intent(IN)    :: ider
!!     Integer,         Intent(IN)    :: iside1
!!     Integer,         Intent(IN)    :: iside2
!!     Real(REAL64),    Intent(IN)    :: parvalue(*)
!!     Integer,         Intent(INOUT) :: leftknot1
!!     Integer,         Intent(INOUT) :: leftknot2
!!     Real(REAL64),    Intent(INOUT) :: mehlum 
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLsurf),  Intent(IN)    :: surf
    Integer,         Intent(IN)    :: ider
    Integer,         Intent(IN)    :: iside1
    Integer,         Intent(IN)    :: iside2
    Real(REAL64),    Intent(IN)    :: parvalue(*)
    Integer,         Intent(INOUT) :: leftknot1
    Integer,         Intent(INOUT) :: leftknot2
    Real(REAL64),    Intent(INOUT) :: mehlum 
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_ider, c_jstat, c_iside1, c_iside2, c_leftknot1,        &
                      c_leftknot2

    Interface
      Subroutine c_s2510(surf, ider, iside1, iside2, parvalue, leftknot1,      &
                         leftknot2, mehlum, jstat)                           &
                         BIND(C, name="s2510")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf
        Integer(C_INT), VALUE               :: ider
        Integer(C_INT), VALUE               :: iside1
        Integer(C_INT), VALUE               :: iside2
        Real(C_DOUBLE),       Intent(IN)    :: parvalue(*)
        Integer(C_INT),       Intent(INOUT) :: leftknot1
        Integer(C_INT),       Intent(INOUT) :: leftknot2
        Real(C_DOUBLE),       Intent(INOUT) :: mehlum
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s2510
    End Interface

    c_ider        = ider
    c_iside1      = iside1
    c_iside2      = iside2
    c_leftknot1   = leftknot1
    c_leftknot2   = leftknot2

    Call c_s2510(surf%cptr, c_ider, c_iside1, c_iside2, parvalue,           &
                 c_leftknot1, c_leftknot2, mehlum, c_jstat)

    leftknot1 = c_leftknot1
    leftknot2 = c_leftknot2
    jstat     = c_jstat

  End Subroutine s2510
 
!---------------------------------- s2532 -------------------------------------

  Subroutine s2532(surf, u_continuity, v_continuity, u_surfnumb, v_surfnumb,   &
                   gauss_surf, stat)

!! PURPOSE
!!   s2532 - To interpolate or approximate the Gaussian curvature of a B-spline
!!           or NURBS surface by a NURBS surface. The desired continuity
!!           of the Gaussian curvature surface is input and this may lead to a
!!           patchwork of output surfaces. Interpolation results in a high order
!!           surface. If the original surface is a B-spline surface of order k, 
!!           the result is of order 8k − 11, in the NURBS case, order 32k − 35.
!!           To avoid instability because of this, a maximum order is applied.
!!           This may lead to an approximation rather than an interpolation.

!! INTERFACE
!!   Subroutine s2532(surf, u_continuity, v_continuity, u_surfnumb, v_surfnumb,&
!!                    gauss_surf, stat)
!!     Type(SISLsurf),              Intent(IN)    :: surf
!!     Integer,                     Intent(IN)    :: u_continuity 
!!     Integer,                     Intent(IN)    :: v_continuity 
!!     Integer,                     Intent(INOUT) :: u_surfnumb 
!!     Integer,                     Intent(INOUT) :: v_surfnumb 
!!     Type(SISLsurf), ALLOCATABLE, Intent(INOUT) :: gauss_surf(:) 
!!     Integer,                     Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf),              Intent(IN)    :: surf
    Integer,                     Intent(IN)    :: u_continuity 
    Integer,                     Intent(IN)    :: v_continuity 
    Integer,                     Intent(INOUT) :: u_surfnumb 
    Integer,                     Intent(INOUT) :: v_surfnumb 
    Type(SISLsurf), ALLOCATABLE, Intent(INOUT) :: gauss_surf(:) 
    Integer,                     Intent(INOUT) :: stat

    Integer :: i

    Integer(C_INT) :: c_stat, c_ucontinuity, c_vcontinuity,                    &
                      c_usurfnumb, c_vsurfnumb

    Type(C_PTR)          :: c_gauss_surf_p
    Type(C_PTR), Pointer :: cgauss_surfp(:)

    Interface
      Subroutine c_s2532(surf, u_continuity, v_continuity, u_surfnumb,         &
                         v_surfnumb, gauss_surf,  stat)                        &
                         BIND(C, name="s2532")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf
        Integer(C_INT), VALUE               :: u_continuity 
        Integer(C_INT), VALUE               :: v_continuity 
        Integer(C_INT),       Intent(INOUT) :: u_surfnumb 
        Integer(C_INT),       Intent(INOUT) :: v_surfnumb
        Type(C_PTR),          Intent(INOUT) :: gauss_surf
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s2532
    End Interface

    c_ucontinuity = u_continuity 
    c_vcontinuity = v_continuity 
    c_usurfnumb   = u_surfnumb
    c_vsurfnumb   = v_surfnumb

    Call c_s2532(surf%cptr, c_ucontinuity, c_vcontinuity, c_usurfnumb,      &
                 c_vsurfnumb, c_gauss_surf_p, c_stat)

    u_surfnumb = c_usurfnumb
    v_surfnumb = c_vsurfnumb

    If (C_ASSOCIATED(c_gauss_surf_p)) Then
      Call C_F_Pointer(c_gauss_surf_p, cgauss_surfp, [u_surfnumb*v_surfnumb])
      If (ALLOCATED(gauss_surf)) DEALLOCATE(gauss_surf)
      ALLOCATE(gauss_surf(u_surfnumb*v_surfnumb))
      Do i=1, u_surfnumb*v_surfnumb
        If (C_ASSOCIATED(cgauss_surfp(i))) Then
          gauss_surf(i)%cptr = cgauss_surfp(i)
        Else
          gauss_surf(i)%cptr = C_NULL_PTR 
        EndIf
        Call surfCtoF(gauss_surf(i))
      EndDo
    End If

    stat     = c_stat

  End Subroutine s2532

!---------------------------------- s2536 -------------------------------------

  Subroutine s2536(surf, u_continuity, v_continuity, u_surfnumb, v_surfnumb,   &
                   mehlum_surf, stat)

!! PURPOSE
!!   s2536 - To interpolate or approximate the Mehlum curvature of a B-spline
!!           or NURBS surface by a NURBS surface. The desired continuity
!!           of the Mehlum curvature surface is input and this may lead to a
!!           patchwork of output surfaces. Interpolation results in a high order
!!           surface. If the original surface is a B-spline surface of order k,
!!           the result is of order 12k − 17, in the NURBS case, order 48k − 53.
!!           To avoid instability beacuse of this, a maximum order is applied.
!!           This may lead to an approximation rather than an interpolation.

!! INTERFACE
!!   Subroutine s2536(surf, u_continuity, v_continuity, u_surfnumb, v_surfnumb,&
!!                    mehlum_surf, stat)
!!     Type(SISLsurf),              Intent(IN)    :: surf
!!     Integer,                     Intent(IN)    :: u_continuity 
!!     Integer,                     Intent(IN)    :: v_continuity 
!!     Integer,                     Intent(INOUT) :: u_surfnumb 
!!     Integer,                     Intent(INOUT) :: v_surfnumb 
!!     Type(SISLsurf), ALLOCATABLE, Intent(INOUT) :: mehlum_surf(:) 
!!     Integer,                     Intent(INOUT) :: stat
 
   Implicit NONE

    Type(SISLsurf),              Intent(IN)    :: surf
    Integer,                     Intent(IN)    :: u_continuity 
    Integer,                     Intent(IN)    :: v_continuity 
    Integer,                     Intent(INOUT) :: u_surfnumb 
    Integer,                     Intent(INOUT) :: v_surfnumb 
    Type(SISLsurf), ALLOCATABLE, Intent(INOUT) :: mehlum_surf(:) 
    Integer,                     Intent(INOUT) :: stat

    Integer :: i

    Integer(C_INT) :: c_stat, c_ucontinuity, c_vcontinuity,                    &
                      c_usurfnumb, c_vsurfnumb

    Type(C_PTR)          :: c_mehlum_surf_p
    Type(C_PTR), Pointer :: cmehlum_surfp(:)

    Interface
      Subroutine c_s2536(surf, u_continuity, v_continuity, u_surfnumb,         &
                         v_surfnumb, mehlum_surf,  stat)                       &
                         BIND(C, name="s2536")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf
        Integer(C_INT), VALUE               :: u_continuity 
        Integer(C_INT), VALUE               :: v_continuity 
        Integer(C_INT),       Intent(INOUT) :: u_surfnumb 
        Integer(C_INT),       Intent(INOUT) :: v_surfnumb
        Type(C_PTR),          Intent(INOUT) :: mehlum_surf
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s2536
    End Interface

    c_ucontinuity = u_continuity 
    c_vcontinuity = v_continuity 
    c_usurfnumb   = u_surfnumb
    c_vsurfnumb   = v_surfnumb

    Call c_s2536(surf%cptr, c_ucontinuity, c_vcontinuity, c_usurfnumb,      &
                 c_vsurfnumb, c_mehlum_surf_p, c_stat)

    u_surfnumb = c_usurfnumb
    v_surfnumb = c_vsurfnumb

    If (C_ASSOCIATED(c_mehlum_surf_p)) Then
      Call C_F_Pointer(c_mehlum_surf_p, cmehlum_surfp, [u_surfnumb*v_surfnumb])
      If (ALLOCATED(mehlum_surf)) DEALLOCATE(mehlum_surf)
      ALLOCATE(mehlum_surf(u_surfnumb*v_surfnumb))
      Do i=1, u_surfnumb*v_surfnumb
        If (C_ASSOCIATED(cmehlum_surfp(i))) Then
          mehlum_surf(i)%cptr = cmehlum_surfp(i)
        Else
          mehlum_surf(i)%cptr = C_NULL_PTR
        EndIf
        Call surfCtoF(mehlum_surf(i))
      EndDo
    End If

    stat     = c_stat

  End Subroutine s2536

!---------------------------------- s2540 -------------------------------------

  Subroutine s2540(surf, curvature_type, export_par_val, pick_subpart,         &
                   boundary, n_u, n_v, garr, stat)

!! PURPOSE
!!   s2540 - To compute a set of curvature values on a uniform grid in a
!!           selected subset of the parameter domain of a NURBS surface.

!! INTERFACE
!!   Subroutine s2540(surf, curvature_type, export_par_val, pick_subpart,      &
!!                    boundary, n_u, n_v, garr, stat)
!!     Type(SISLsurf),                  Intent(IN)    :: surf
!!     Integer,                         Intent(IN)    :: curvature_type 
!!     Integer,                         Intent(IN)    :: export_par_val 
!!     Integer,                         Intent(IN)    :: pick_subpart
!!     Real(REAL64),                    Intent(IN)    :: boundary(*) 
!!     Integer,                         Intent(IN)    :: n_u 
!!     Integer,                         Intent(IN)    :: n_v 
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: garr(:)
!!     Integer,                         Intent(INOUT) :: stat


    Implicit NONE

    Type(SISLsurf),                  Intent(IN)    :: surf
    Integer,                         Intent(IN)    :: curvature_type 
    Integer,                         Intent(IN)    :: export_par_val 
    Integer,                         Intent(IN)    :: pick_subpart
    Real(REAL64),                    Intent(IN)    :: boundary(*) 
    Integer,                         Intent(IN)    :: n_u 
    Integer,                         Intent(IN)    :: n_v 
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: garr(:)
    Integer,                         Intent(INOUT) :: stat

    Integer :: ngarr

    Integer(C_INT)        :: c_curvature_type, c_export_par_val,               &
                             c_pick_subpart, c_n_u, c_n_v, c_stat
    Type(C_PTR)           :: c_garr_p
    Real(REAL64), Pointer :: garrp(:)

    Interface
    Subroutine c_s2540(surf, curvature_type, export_par_val, pick_subpart,     &
                   boundary, n_u, n_v, garr, stat)                             &
                   BIND(C,name="s2540")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf
        Integer(C_INT), VALUE               :: curvature_type 
        Integer(C_INT), VALUE               :: export_par_val
        Integer(C_INT), VALUE               :: pick_subpart 
        Real(C_DOUBLE),       Intent(IN)    :: boundary(*)
        Integer(C_INT), VALUE               :: n_u 
        Integer(C_INT), VALUE               :: n_v 
        Type(C_PTR),          Intent(INOUT) :: garr
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s2540
    End Interface

    c_curvature_type = curvature_type
    c_export_par_val = export_par_val
    c_pick_subpart   = pick_subpart
    c_n_u            = n_u
    c_n_v            = n_v
    Call c_s2540(surf%cptr, c_curvature_type, c_export_par_val,             &
                 c_pick_subpart, boundary, c_n_u,  c_n_v, c_garr_p, c_stat)


    If (export_par_val>0) Then
      ngarr = 3*(n_u+1)*(n_v+1)
    Else
      ngarr = (n_u+1)*(n_v+1)
    End If 
    If (C_ASSOCIATED(c_garr_p)) Then
      Call C_F_Pointer(c_garr_p, garrp,[ngarr])
      If (ALLOCATED(garr)) DEALLOCATE(garr)
      ALLOCATE(garr(ngarr), SOURCE=0.0_REAL64)
      garr(:) = garrp(:)
      NULLIFY(garrp)
      Call c_free(c_garr_p)
    EndIf

    stat = c_stat

  End Subroutine s2540

!---------------------------------- s2542 -------------------------------------

  Subroutine s2542(surf, ider, iside1, iside2, parvalue, leftknot1,          &
                   leftknot2, k1, k2, d1, d2, jstat)

!! PURPOSE
!!   s2542 - To compute principal curvatures (k1,k2) with corresponding
!!           principal directions (d1,d2) of a spline surface at given values
!!           (u,v) = (parvalue(1),parvalue(2)), where etl[leftknot1+1] <= par-
!!           value(1) < etl[leftknot1+2] and et2[leftknot2+1] <= parvalue(2) <
!!           et2(leftknot2+2).

!! INTERFACE
!!   Subroutine s2542(surf, ider, iside1, iside2, parvalue, leftknot1,      &
!!                    leftknot2, k1, k2, d1, d2, jstat)
!!     Type(SISLsurf),  Intent(IN)    :: surf
!!     Integer,         Intent(IN)    :: ider
!!     Integer,         Intent(IN)    :: iside1
!!     Integer,         Intent(IN)    :: iside2
!!     Real(REAL64),    Intent(IN)    :: parvalue(*)
!!     Integer,         Intent(INOUT) :: leftknot1
!!     Integer,         Intent(INOUT) :: leftknot2
!!     Real(REAL64),    Intent(INOUT) :: k1 
!!     Real(REAL64),    Intent(INOUT) :: k2 
!!     Real(REAL64),    Intent(INOUT) :: d1(*) 
!!     Real(REAL64),    Intent(INOUT) :: d2(*) 
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLsurf),  Intent(IN)    :: surf
    Integer,         Intent(IN)    :: ider
    Integer,         Intent(IN)    :: iside1
    Integer,         Intent(IN)    :: iside2
    Real(REAL64),    Intent(IN)    :: parvalue(*)
    Integer,         Intent(INOUT) :: leftknot1
    Integer,         Intent(INOUT) :: leftknot2
    Real(REAL64),    Intent(INOUT) :: k1 
    Real(REAL64),    Intent(INOUT) :: k2 
    Real(REAL64),    Intent(INOUT) :: d1(*) 
    Real(REAL64),    Intent(INOUT) :: d2(*) 
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_ider, c_jstat, c_iside1, c_iside2, c_leftknot1,        &
                      c_leftknot2

    Interface
      Subroutine c_s2542(surf, ider, iside1, iside2, parvalue, leftknot1,      &
                         leftknot2, k1, k2, d1, d2, jstat)                     &
                         BIND(C, name="s2542")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf
        Integer(C_INT), VALUE               :: ider
        Integer(C_INT), VALUE               :: iside1
        Integer(C_INT), VALUE               :: iside2
        Real(C_DOUBLE),       Intent(IN)    :: parvalue(*)
        Integer(C_INT),       Intent(INOUT) :: leftknot1
        Integer(C_INT),       Intent(INOUT) :: leftknot2
        Real(C_DOUBLE),       Intent(INOUT) :: k1 
        Real(C_DOUBLE),       Intent(INOUT) :: k2 
        Real(C_DOUBLE),       Intent(INOUT) :: d1(*) 
        Real(C_DOUBLE),       Intent(INOUT) :: d2(*) 
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s2542
    End Interface

    c_ider        = ider
    c_iside1      = iside1
    c_iside2      = iside2
    c_leftknot1   = leftknot1
    c_leftknot2   = leftknot2
    d1(1:2)       = 0.0_REAL64
    d2(1:2)       = 0.0_REAL64

    Call c_s2542(surf%cptr, c_ider, c_iside1, c_iside2, parvalue,           &
                 c_leftknot1, c_leftknot2, k1, k2, d1, d2, c_jstat)

    leftknot1 = c_leftknot1
    leftknot2 = c_leftknot2
    jstat     = c_jstat

  End Subroutine s2542

!---------------------------------- s2544 -------------------------------------

  Subroutine s2544(surf, ider, iside1, iside2, parvalue, leftknot1,          &
                   leftknot2, norcurv, jstat)

!! PURPOSE
!!   s2544 - To compute the Normal curvature of a splne surface at
!!           given values (u,v) = (parvalue(1),parvalue[2)) in the direc-
!!           tion (parvalue(2),parvalue(l)) where et1(leftknot1+1) <= par-
!!           value(1) < et1(leftknot1+2) and et2(leftknot2+1) <= parvalue(2) <
!!           et2(leftknot2+2).

!! INTERFACE
!!   Subroutine s2544(surf, ider, iside1, iside2, parvalue, leftknot1,      &
!!                    leftknot2, norcurv, jstat)
!!     Type(SISLsurf),  Intent(IN)    :: surf
!!     Integer,         Intent(IN)    :: ider
!!     Integer,         Intent(IN)    :: iside1
!!     Integer,         Intent(IN)    :: iside2
!!     Real(REAL64),    Intent(IN)    :: parvalue(*)
!!     Integer,         Intent(INOUT) :: leftknot1
!!     Integer,         Intent(INOUT) :: leftknot2
!!     Real(REAL64),    Intent(INOUT) :: norcurv(*) 
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLsurf),  Intent(IN)    :: surf
    Integer,         Intent(IN)    :: ider
    Integer,         Intent(IN)    :: iside1
    Integer,         Intent(IN)    :: iside2
    Real(REAL64),    Intent(IN)    :: parvalue(*)
    Integer,         Intent(INOUT) :: leftknot1
    Integer,         Intent(INOUT) :: leftknot2
    Real(REAL64),    Intent(INOUT) :: norcurv(*) 
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_ider, c_jstat, c_iside1, c_iside2, c_leftknot1,        &
                      c_leftknot2

    Interface
      Subroutine c_s2544(surf, ider, iside1, iside2, parvalue, leftknot1,      &
                         leftknot2, norcurv, jstat)                            &
                         BIND(C, name="s2544")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf
        Integer(C_INT), VALUE               :: ider
        Integer(C_INT), VALUE               :: iside1
        Integer(C_INT), VALUE               :: iside2
        Real(C_DOUBLE),       Intent(IN)    :: parvalue(*)
        Integer(C_INT),       Intent(INOUT) :: leftknot1
        Integer(C_INT),       Intent(INOUT) :: leftknot2
        Real(C_DOUBLE),       Intent(INOUT) :: norcurv(*) 
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s2544
    End Interface

    c_ider        = ider
    c_iside1      = iside1
    c_iside2      = iside2
    c_leftknot1   = leftknot1
    c_leftknot2   = leftknot2
    norcurv(1:2) = 0.0_REAL64

    Call c_s2544(surf%cptr, c_ider, c_iside1, c_iside2, parvalue,           &
                 c_leftknot1, c_leftknot2, norcurv, c_jstat)

    leftknot1 = c_leftknot1
    leftknot2 = c_leftknot2
    jstat     = c_jstat

  End Subroutine s2544

!---------------------------------- s2545 -------------------------------------

  Subroutine s2545(surf, curvature_type, export_par_val, boundary, n_u, n_v,   &
                   scale, garr, stat)

!! PURPOSE
!!   s2545 - To compute a set of focal values on a uniform grid in a selected
!!           subset of the parameter domain of a NURBS surface. A focal
!!           value is a surface position offset by the surface curvature.

!! INTERFACE
!!   Subroutine s2545(surf, curvature_type, export_par_val, boundary, n_u, n_v,&
!!                    garr, stat)
!!     Type(SISLsurf),                  Intent(IN)    :: surf
!!     Integer,                         Intent(IN)    :: curvature_type 
!!     Integer,                         Intent(IN)    :: export_par_val 
!!     Real(REAL64),                    Intent(IN)    :: boundary(*) 
!!     Integer,                         Intent(IN)    :: n_u 
!!     Integer,                         Intent(IN)    :: n_v 
!!     Real(REAL64),                    Intent(IN)    :: scale 
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: garr(:)
!!     Integer,                         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf),                  Intent(IN)    :: surf
    Integer,                         Intent(IN)    :: curvature_type 
    Integer,                         Intent(IN)    :: export_par_val 
    Real(REAL64),                    Intent(IN)    :: boundary(*) 
    Integer,                         Intent(IN)    :: n_u 
    Integer,                         Intent(IN)    :: n_v 
    Real(REAL64),                    Intent(IN)    :: scale 
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: garr(:)
    Integer,                         Intent(INOUT) :: stat

    Integer :: ngarr, idim

    Integer(C_INT)        :: c_curvature_type, c_export_par_val,               &
                             c_n_u, c_n_v, c_stat
    Type(C_PTR)           :: c_garr_p
    Real(REAL64), Pointer :: garrp(:)

    Interface
    Subroutine c_s2545(surf, curvature_type, export_par_val, boundary, n_u,    &
                       n_v, scale, garr, stat)                                 &
                       BIND(C,name="s2545")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf
        Integer(C_INT), VALUE               :: curvature_type 
        Integer(C_INT), VALUE               :: export_par_val
        Real(C_DOUBLE),       Intent(IN)    :: boundary(*)
        Integer(C_INT), VALUE               :: n_u 
        Integer(C_INT), VALUE               :: n_v 
        Real(C_DOUBLE), VALUE               :: scale 
        Type(C_PTR),          Intent(INOUT) :: garr
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s2545
    End Interface

    c_curvature_type = curvature_type
    c_export_par_val = export_par_val
    c_n_u            = n_u
    c_n_v            = n_v
    Call c_s2545(surf%cptr, c_curvature_type, c_export_par_val, boundary,   &
                 c_n_u,  c_n_v, scale, c_garr_p, c_stat)

    idim = surf%idim
    If (export_par_val>0) Then
      ngarr = (idim+2)*(n_u+1)*(n_v+1)
    Else
      ngarr = idim*(n_u+1)*(n_v+1)
    End If 
    If (C_ASSOCIATED(c_garr_p)) Then
      Call C_F_Pointer(c_garr_p, garrp,[ngarr])
      If (ALLOCATED(garr)) DEALLOCATE(garr)
      ALLOCATE(garr(ngarr), SOURCE=0.0_REAL64)
      garr(:) = garrp(:)
      NULLIFY(garrp)
      Call c_free(c_garr_p)
    EndIf

    stat = c_stat

  End Subroutine s2545

End Module Surface_Analysis
