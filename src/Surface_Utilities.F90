   
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


Module Surface_Utilities

!! Module Surface_Utilities contains Modern Fortran C-interoperability routines
!! for most of the C functions described in Chapter 9 of version 4.4 of the SISL
!! reference manual. The drawing and graphics routines (s6drawseq, s6move, 
!! s6line, s1237, and s1238) are not supported at this time.

!! Written by: Richard Weed, Ph.D.
!! Version no. 1.0 - Feb. 2021
!!   Initial development version

!! Contact: rweedmsu@gmail.com

  USE forSISLdata, ONLY: REAL64, C_INT, C_DOUBLE, C_PTR, C_ASSOCIATED,         &
                         C_F_POINTER, SISLsurf, SISLcurve, curveCtoF,          &
                         surfCtoF

  Implicit NONE

  PRIVATE :: REAL64, C_INT, C_DOUBLE, C_PTR, C_ASSOCIATED, C_F_POINTER,        &
             SISLsurf, SISLcurve, curveCtoF, surfCtoF


Contains

!---------------------------------- newSurf ------------------------------------

  Subroutine newSurf(number1, number2, order1, order2, knot1, knot2, coef,     &
                   kind, dim, copy, surf)

!! PURPOSE
!!   newSurf - Create and initialize a surface object instance.

!! INTERFACE
!!   Subroutine newSurf(number1, number2, order1, order2, knot1, knot2, coef, &
!!                      kind, dim, copy, surf)
!!     Integer,        Intent(IN)    :: number1
!!     Integer,        Intent(IN)    :: number2
!!     Integer,        Intent(IN)    :: order1
!!     Integer,        Intent(IN)    :: order2
!!     Integer,        Intent(IN)    :: kind
!!     Integer,        Intent(IN)    :: dim 
!!     Integer,        Intent(IN)    :: copy 
!!     Real(REAL64),   Intent(IN)    :: knot1(*)
!!     Real(REAL64),   Intent(IN)    :: knot2(*)
!!     Real(REAL64),   Intent(IN)    :: coef(*)
!!     Type(SISLsurf), Intent(INOUT) :: surf

    Implicit NONE

    Integer,        Intent(IN)    :: number1, number2, order1, order2, kind,   &
                                     dim, copy
    Real(REAL64),   Intent(IN)    :: knot1(*)
    Real(REAL64),   Intent(IN)    :: knot2(*)
    Real(REAL64),   Intent(IN)    :: coef(*)
    Type(SISLsurf), Intent(INOUT) :: surf

    Integer(C_INT) :: c_number1, c_number2, c_order1, c_order2, c_kind, c_dim, &
                      c_copy
    
    Interface
      Function c_newSurf(number1, number2, order1, order2, knot1, knot2, coef, &
                          kind, dim, copy) BIND(C, name="newSurf")

        IMPORT :: C_PTR, C_DOUBLE, C_INT
        Implicit NONE
  
        Integer(C_INT), VALUE            :: number1
        Integer(C_INT), VALUE            :: number2
        Integer(C_INT), VALUE            :: order1
        Integer(C_INT), VALUE            :: order2
        Real(C_DOUBLE),       Intent(IN) :: knot1(*)
        Real(C_DOUBLE),       Intent(IN) :: knot2(*)
        Real(C_DOUBLE),       Intent(IN) :: coef(*)
        Integer(C_INT), VALUE            :: kind 
        Integer(C_INT), VALUE            :: dim 
        Integer(C_INT), VALUE            :: copy
        Type(C_PTR)                      :: c_newSurf
      
      End Function c_newSurf
    End Interface 
    
    c_number1 = number1
    c_number2 = number2
    c_order1  = order1 
    c_order2  = order2 
    c_kind    = kind
    c_dim     = dim
    c_copy    = copy
 
    surf%cptr = c_newSurf(c_number1, c_number2, c_order1, c_order2, knot1,  &
                             knot2, coef, c_kind, c_dim, c_copy)

    Call surfCtoF(surf)

  End Subroutine newSurf

!-------------------------------- copySurface ---------------------------------

  Subroutine copySurface(old_surf, new_surf)

!! PURPOSE
!!   copySurface - Make a copy of a SISLSurface object.

!! INTERFACE
!!   Subroutine copySurface(old_surf, new_surf)
!!     Type(SISLSurf), Intent(IN)    :: old_surf
!!     Type(SISLSurf), Intent(INOUT) :: new_surf

    Implicit NONE

    Type(SISLSurf), Intent(IN)    :: old_surf
    Type(SISLSurf), Intent(INOUT) :: new_surf

    Interface
      Function c_copySurface(psurf) BIND(C, name="copySurface")

        IMPORT :: C_PTR
        Implicit NONE
  
        Type(C_PTR), VALUE :: psurf
        Type(C_PTR)        :: c_copySurface

      End Function c_copySurface
    End Interface

    new_surf%cptr = c_copySurface(old_surf%cptr)

    Call surfCtoF(new_surf)

  End Subroutine copySurface

!---------------------------------- freeSurf ----------------------------------

  Subroutine freeSurf(surf)

!! PURPOSE
!!   freeSurf - Free the space occupied by the surface. Before using freeSurf,
!!              make sure that the surface object exists.

!! INTERFACE
!!   Subroutine freeSurf(surf)
!!     Type(SISLSurf), Intent(INOUT) :: surf

    Implicit NONE

    Type(SISLSurf), Intent(INOUT) :: surf

    Integer :: i

    Interface
      Subroutine c_freeSurf(surf) BIND(C, name="freeSurf")

        IMPORT :: C_PTR
        Implicit NONE
  
        Type(C_PTR), VALUE :: surf

      End Subroutine c_freeSurf
    End Interface

    surf%in1          = 0
    surf%in2          = 0
    surf%ik1          = 0
    surf%ik2          = 0
    surf%ikind        = 0
    surf%idim         = 0
    surf%icopy        = 0
    surf%cuopen_1     = 0
    surf%cuopen_2     = 0
    surf%pdir%igtpi   = 0
    surf%pdir%aang    = 0.0_REAL64
    surf%pbox%imin    = 0
    surf%pbox%imax    = 0
    surf%pbox%etol    = 0.0_REAL64
    surf%seg1%num_seg = 0
    surf%seg2%num_seg = 0

    If (ASSOCIATED(surf%et1))          NULLIFY(surf%et1)
    If (ASSOCIATED(surf%et2))          NULLIFY(surf%et2)
    If (ASSOCIATED(surf%ecoef))        NULLIFY(surf%ecoef)
    If (ASSOCIATED(surf%rcoef))        NULLIFY(surf%rcoef)
    If (ASSOCIATED(surf%pdir%ecoef))   NULLIFY(surf%pdir%ecoef)
    If (ASSOCIATED(surf%pdir%esmooth)) NULLIFY(surf%pdir%esmooth)
    If (ASSOCIATED(surf%pbox%emax))    NULLIFY(surf%pbox%emax)
    If (ASSOCIATED(surf%pbox%emin))    NULLIFY(surf%pbox%emin)
    Do i=1,3
      If (ASSOCIATED(surf%pbox%e2max(i)%ptr)) NULLIFY(surf%pbox%e2max(i)%ptr)
      If (ASSOCIATED(surf%pbox%e2min(i)%ptr)) NULLIFY(surf%pbox%e2min(i)%ptr)
    EndDo

    If (ASSOCIATED(surf%seg1%seg_val))  NULLIFY(surf%seg1%seg_val)
    If (ASSOCIATED(surf%seg1%seg_type)) NULLIFY(surf%seg1%seg_type)
    If (ASSOCIATED(surf%seg2%seg_val))  NULLIFY(surf%seg2%seg_val)
    If (ASSOCIATED(surf%seg2%seg_type)) NULLIFY(surf%seg2%seg_type)

    If (C_ASSOCIATED(surf%cptr)) Then
      Call c_freeSurf(surf%cptr)
    EndIf

  End Subroutine freeSurf

!---------------------------------- s1421 -------------------------------------

  Subroutine s1421(surf, der, parvalue, leftknot1, leftknot2, derive, normal,  &
                   stat)

!! PURPOSE
!!   s1421 - Evaluate the surface at a given parameter value pair. Compute
!!           der derivatives and the normal if der ≥ 1. See also s1424() on
!!           page 381.

!! INTERfACE
!!   Subroutine s1421(surf, der, parvalue, leftknot1, leftknot2, derive,       &
!!                    normal, stat)
!!     Type(SISLsurf),  Intent(IN)    :: surf
!!     Integer,         Intent(IN)    :: der
!!     Real(REAL64),    Intent(IN)    :: parvalue(*)
!!     Integer,         Intent(INOUT) :: leftknot1  
!!     Integer,         Intent(INOUT) :: leftknot2
!!     Real(REAL64),    Intent(INOUT) :: derive(*)
!!     Real(REAL64),    Intent(INOUT) :: normal(*)
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf),  Intent(IN)    :: surf
    Integer,         Intent(IN)    :: der
    Real(REAL64),    Intent(IN)    :: parvalue(*)
    Integer,         Intent(INOUT) :: leftknot1  
    Integer,         Intent(INOUT) :: leftknot2
    Real(REAL64),    Intent(INOUT) :: derive(*)
    Real(REAL64),    Intent(INOUT) :: normal(*)
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_der, c_stat, c_leftknot1, c_leftknot2

    Interface
      Subroutine c_s1421(surf, der, parvalue, leftknot1, leftknot2, derive,    &
                        normal, stat)                                          &
                        BIND(C, name="s1421")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf
        Integer(C_INT), VALUE               :: der
        Real(C_DOUBLE),       Intent(IN)    :: parvalue(*)
        Integer(C_INT),       Intent(INOUT) :: leftknot1   
        Integer(C_INT),       Intent(INOUT) :: leftknot2
        Real(C_DOUBLE),       Intent(INOUT) :: derive(*)
        Real(C_DOUBLE),       Intent(INOUT) :: normal(*)
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1421
    End Interface

    c_der         = der
    c_leftknot1   = leftknot1 
    c_leftknot2   = leftknot2

    derive(1:((surf%idim*(der+1)*(der+2))/2)) = 0.0_REAL64
    normal(1:surf%idim)                      = 0.0_REAL64
 
    Call c_s1421(surf%cptr, c_der, parvalue, c_leftknot1, c_leftknot2,      &
                 derive, normal, c_stat)

    leftknot1 = c_leftknot1 
    leftknot2 = c_leftknot2
    stat      = c_stat

  End Subroutine s1421

!---------------------------------- s1424 -------------------------------------

  Subroutine s1424(surf, der1, der2, parvalue, leftknot1, leftknot2, derive,   &
                   stat)

!! PURPOSE
!!   s1424 - Evaluate the surface the parameter value (parvalue(1), par-
!!           value(2)). Compute the der1 × der2 first derivatives. The deriva-
!!           tives that will be computed are D i,j , i = 1, 2, . . . , der1+1, 
!!           j=1, 2, ..., der2+1.

!! INTERFACE
!!   Subroutine s1424(surf, der1, der2, parvalue, leftknot1, leftknot2,   &
!!                    derive, stat)
!!     Type(SISLsurf),  Intent(IN)    :: surf
!!     Integer,         Intent(IN)    :: der1
!!     Integer,         Intent(IN)    :: der2
!!     Real(REAL64),    Intent(IN)    :: parvalue(*)
!!     Integer,         Intent(INOUT) :: leftknot1  
!!     Integer,         Intent(INOUT) :: leftknot2
!!     Real(REAL64),    Intent(INOUT) :: derive(*)
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf),  Intent(IN)    :: surf
    Integer,         Intent(IN)    :: der1
    Integer,         Intent(IN)    :: der2
    Real(REAL64),    Intent(IN)    :: parvalue(*)
    Integer,         Intent(INOUT) :: leftknot1  
    Integer,         Intent(INOUT) :: leftknot2
    Real(REAL64),    Intent(INOUT) :: derive(*)
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_der1, c_der2, c_stat, c_leftknot1, c_leftknot2

    Interface
      Subroutine c_s1424(surf, der1, der2, parvalue, leftknot1, leftknot2,     &
                        derive, stat)                                          &
                        BIND(C, name="s1424")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf
        Integer(C_INT), VALUE               :: der1
        Integer(C_INT), VALUE               :: der2
        Real(C_DOUBLE),       Intent(IN)    :: parvalue(*)
        Integer(C_INT),       Intent(INOUT) :: leftknot1   
        Integer(C_INT),       Intent(INOUT) :: leftknot2
        Real(C_DOUBLE),       Intent(INOUT) :: derive(*)
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1424
    End Interface

    c_der1        = der1
    c_der2        = der2
    c_leftknot1   = leftknot1 
    c_leftknot2   = leftknot2

    derive(1:(surf%idim*(der1+1)*(der2+1))) = 0.0_REAL64

    Call c_s1424(surf%cptr, c_der1, c_der2, parvalue, c_leftknot1,          &
                 c_leftknot2, derive, c_stat)

    leftknot1 = c_leftknot1 
    leftknot2 = c_leftknot2
    stat      = c_stat

  End Subroutine s1424

!---------------------------------- s1422 -------------------------------------

  Subroutine s1422(ps1, ider, iside1, iside2, epar, ilfs, ilft, eder, enorm,  &
                   jstat)

!! PURPOSE
!!   s1422 - Evaluate and compute the left- or right-hand derivatives of a sur-
!!           face at a given parameter position.

!! INTERFACE
!!   Subroutine s1422(ps1, ider, iside1, iside2, epar, ilfs, ilft, eder,   &
!!                    enorm, jstat)
!!     Type(SISLsurf),  Intent(IN)    :: ps1 
!!     Integer,         Intent(IN)    :: ider
!!     Integer,         Intent(IN)    :: iside1
!!     Integer,         Intent(IN)    :: iside2
!!     Real(REAL64),    Intent(IN)    :: epar(*)
!!     Integer,         Intent(INOUT) :: ilfs  
!!     Integer,         Intent(INOUT) :: ilft  
!!     Real(REAL64),    Intent(INOUT) :: eder(*)
!!     Real(REAL64),    Intent(INOUT) :: enorm(*)
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLsurf),  Intent(IN)    :: ps1 
    Integer,         Intent(IN)    :: ider
    Integer,         Intent(IN)    :: iside1
    Integer,         Intent(IN)    :: iside2
    Real(REAL64),    Intent(IN)    :: epar(*)
    Integer,         Intent(INOUT) :: ilfs  
    Integer,         Intent(INOUT) :: ilft  
    Real(REAL64),    Intent(INOUT) :: eder(*)
    Real(REAL64),    Intent(INOUT) :: enorm(*)
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_ider, c_jstat, c_iside1, c_iside2, c_ilfs, c_ilft

    Interface
      Subroutine c_s1422(ps1, ider, iside1, iside2, epar, ilfs, ilft, eder,    &
                         enorm, jstat)                                         &
                        BIND(C, name="s1422")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: ps1 
        Integer(C_INT), VALUE               :: ider
        Integer(C_INT), VALUE               :: iside1
        Integer(C_INT), VALUE               :: iside2
        Real(C_DOUBLE),       Intent(IN)    :: epar(*)
        Integer(C_INT),       Intent(INOUT) :: ilfs   
        Integer(C_INT),       Intent(INOUT) :: ilft   
        Real(C_DOUBLE),       Intent(INOUT) :: eder(*)
        Real(C_DOUBLE),       Intent(INOUT) :: enorm(*)
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1422
    End Interface

    c_ider   = ider
    c_iside1 = iside1
    c_iside2 = iside2
    c_ilfs   = ilfs 
    c_ilft   = ilft 

    enorm(1:ps1%idim) = 0.0_REAL64
    Call c_s1422(ps1%cptr, c_ider, c_iside1, c_iside2, epar, c_ilfs,        &
                 c_ilft, eder, enorm, c_jstat)

    ilfs  = c_ilfs 
    ilft  = c_ilft 
    jstat = c_jstat

  End Subroutine s1422

!---------------------------------- s1425 -------------------------------------

  Subroutine s1425(ps1, ider1, ider2, iside1, iside2, epar, ileft1, ileft2,    &
                   eder, jstat)

!! PURPOSE
!!   s1425 - To compute the value and ider1 × ider2 first derivatives of a ten-
!!           sor product surface at the point with parameter value (epar(1),
!!           epar(2)). The derivatives that will be computed are D(i, j),
!!           i = 1, 2, . . . , ider1+1, j = 1, 2, . . . , ider2+1. The
!!           calculations are from the right hand or left hand side.

!! INTERFACE
!!   Subroutine s1425(ps1, ider1, ider2, iside1, iside2, epar, ileft1, ileft2, &
!!                    eder, jstat)
!!     Type(SISLsurf),  Intent(IN)    :: ps1 
!!     Integer,         Intent(IN)    :: ider1
!!     Integer,         Intent(IN)    :: ider2
!!     Integer,         Intent(IN)    :: iside1
!!     Integer,         Intent(IN)    :: iside2
!!     Real(REAL64),    Intent(IN)    :: epar(*)
!!     Integer,         Intent(INOUT) :: ileft1  
!!     Integer,         Intent(INOUT) :: ileft2  
!!     Real(REAL64),    Intent(INOUT) :: eder(*)
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLsurf),  Intent(IN)    :: ps1 
    Integer,         Intent(IN)    :: ider1
    Integer,         Intent(IN)    :: ider2
    Integer,         Intent(IN)    :: iside1
    Integer,         Intent(IN)    :: iside2
    Real(REAL64),    Intent(IN)    :: epar(*)
    Integer,         Intent(INOUT) :: ileft1  
    Integer,         Intent(INOUT) :: ileft2  
    Real(REAL64),    Intent(INOUT) :: eder(*)
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_ider1, c_ider2, c_jstat, c_iside1, c_iside2, c_ileft1, &
                      c_ileft2

    Interface
      Subroutine c_s1425(ps1, ider1, ider2, iside1, iside2, epar, ileft1,      &
                         ileft2, eder, jstat)                                  &
                        BIND(C, name="s1425")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: ps1 
        Integer(C_INT), VALUE               :: ider1
        Integer(C_INT), VALUE               :: ider2
        Integer(C_INT), VALUE               :: iside1
        Integer(C_INT), VALUE               :: iside2
        Real(C_DOUBLE),       Intent(IN)    :: epar(*)
        Integer(C_INT),       Intent(INOUT) :: ileft1   
        Integer(C_INT),       Intent(INOUT) :: ileft2   
        Real(C_DOUBLE),       Intent(INOUT) :: eder(*)
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1425
    End Interface

    c_ider1  = ider1
    c_ider2  = ider2
    c_iside1 = iside1
    c_iside2 = iside2
    c_ileft1 = ileft1 
    c_ileft2 = ileft2 

    Call c_s1425(ps1%cptr, c_ider1, c_ider2, c_iside1, c_iside2, epar,      &
                 c_ileft1, c_ileft2, eder, c_jstat)

    ileft1 = c_ileft1 
    ileft2 = c_ileft2 
    jstat  = c_jstat

  End Subroutine s1425

!---------------------------------- s1506 -------------------------------------

  Subroutine s1506(ps1, ider, m1, x, m2, y, eder, norm, jstat)

!! PURPOSE
!!   s1506 - Evaluate the surface pointed at by ps1 over an m1 * m2 grid of
!!           points (x(i),y(j)). Compute ider derivatives and normals if 
!!           suitable.

!! INTERFACE
!!   Subroutine s1506(ps1, ider, m1, x, m2, y, eder, norm, jstat)
!!     Type(SISLsurf),  Intent(IN)    :: ps1 
!!     Integer,         Intent(IN)    :: ider
!!     Integer,         Intent(IN)    :: m1 
!!     Real(REAL64),    Intent(IN)    :: x(*)
!!     Integer,         Intent(IN)    :: m2 
!!     Real(REAL64),    Intent(IN)    :: y(*)
!!     Real(REAL64),    Intent(INOUT) :: eder(*)
!!     Real(REAL64),    Intent(INOUT) :: norm(*)
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLsurf),  Intent(IN)    :: ps1 
    Integer,         Intent(IN)    :: ider
    Integer,         Intent(IN)    :: m1 
    Real(REAL64),    Intent(IN)    :: x(*)
    Integer,         Intent(IN)    :: m2 
    Real(REAL64),    Intent(IN)    :: y(*)
    Real(REAL64),    Intent(INOUT) :: eder(*)
    Real(REAL64),    Intent(INOUT) :: norm(*)
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_ider, c_m1, c_m2, c_jstat 

    Interface
      Subroutine c_s1506(ps1, ider, m1, x, m2, y, eder, norm, jstat)           &
                        BIND(C, name="s1506")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: ps1 
        Integer(C_INT), VALUE               :: ider
        Integer(C_INT), VALUE               :: m1 
        Real(C_DOUBLE),       Intent(IN)    :: x(*)
        Integer(C_INT), VALUE               :: m2 
        Real(C_DOUBLE),       Intent(IN)    :: y(*)
        Real(C_DOUBLE),       Intent(INOUT) :: eder(*)
        Real(C_DOUBLE),       Intent(INOUT) :: norm(*)
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1506
    End Interface

    c_ider = ider
    c_m1   = m1 
    c_m2   = m2 

    Call c_s1506(ps1%cptr, c_ider, c_m1, x, c_m2, y, eder, norm, c_jstat)

    jstat  = c_jstat

  End Subroutine s1506

!---------------------------------- s1711 -------------------------------------

  Subroutine s1711(surf, pardir, parval, newsurf1, newsurf2, stat) 

!! PURPOSE
!!   s1711 - Subdivide a surface along a given internal parameter line.

!! INTERFACE
!!   Subroutine s1711(surf, pardir, parval, newsurf1, newsurf2, stat) 
!!     Type(SISLsurf),  Intent(IN)    :: surf
!!     Integer,         Intent(IN)    :: pardir
!!     Real(REAL64),    Intent(IN)    :: parval
!!     Type(SISLsurf),  Intent(INOUT) :: newsurf1
!!     Type(SISLsurf),  Intent(INOUT) :: newsurf2
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf),  Intent(IN)    :: surf
    Integer,         Intent(IN)    :: pardir
    Real(REAL64),    Intent(IN)    :: parval
    Type(SISLsurf),  Intent(INOUT) :: newsurf1
    Type(SISLsurf),  Intent(INOUT) :: newsurf2
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_pardir, c_stat

    Interface
      Subroutine c_s1711(surf, pardir, parval, newsurf1, newsurf2, stat)       &
                         BIND(C, name="s1711")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf
        Integer(C_INT), VALUE               :: pardir 
        Real(C_DOUBLE), VALUE               :: parval
        Type(C_PTR),          Intent(INOUT) :: newsurf1   
        Type(C_PTR),          Intent(INOUT) :: newsurf2   
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1711
    End Interface

    c_pardir = pardir 

    Call c_s1711(surf%cptr, c_pardir, parval, newsurf1%cptr, newsurf2%cptr,    &
                 c_stat)

    Call surfCtoF(newsurf1)
    Call surfCtoF(newsurf2)

    stat      = c_stat

  End Subroutine s1711

!---------------------------------- s1025 -------------------------------------

  Subroutine s1025(ps, epar1, inpar1, epar2, inpar2, rsnew, jstat) 

!! PURPOSE
!!   s1025 - Insert a given set of knots in each parameter direction into the
!!           description of a surface.
!!           NOTE : When the surface is periodic in one direction, the input
!!           parameter values in this direction must lie in the half-open 
!!           interval (et(kk), et(kn+1)), the function will automatically update
!!           the extra knots and coefficients.

!! INTERFACE
!!   Subroutine s1025(ps, epar1, inpar1, epar2, inpar2, rsnew, jstat) 
!!     Type(SISLsurf),  Intent(IN)    :: ps 
!!     Real(REAL64),    Intent(IN)    :: epar1(*) 
!!     Integer,         Intent(IN)    :: inpar1 
!!     Real(REAL64),    Intent(IN)    :: epar2(*) 
!!     Integer,         Intent(IN)    :: inpar2 
!!     Type(SISLsurf),  Intent(INOUT) :: rsnew
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLsurf),  Intent(IN)    :: ps 
    Real(REAL64),    Intent(IN)    :: epar1(*) 
    Integer,         Intent(IN)    :: inpar1 
    Real(REAL64),    Intent(IN)    :: epar2(*) 
    Integer,         Intent(IN)    :: inpar2 
    Type(SISLsurf),  Intent(INOUT) :: rsnew
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_inpar1, c_inpar2, c_jstat

    Interface
      Subroutine c_s1025(ps, epar1, inpar1, epar2, inpar2, rsnew, jstat)       &
                         BIND(C, name="s1025")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: ps 
        Real(C_DOUBLE),       Intent(IN)    :: epar1(*) 
        Integer(C_INT), VALUE               :: inpar1 
        Real(C_DOUBLE),       Intent(IN)    :: epar2(*) 
        Integer(C_INT), VALUE               :: inpar2 
        Type(C_PTR),          Intent(INOUT) :: rsnew   
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1025
    End Interface

    c_inpar1 = inpar1
    c_inpar2 = inpar2

    Call c_s1025(ps%cptr, epar1, c_inpar1, epar2, c_inpar2, rsnew%cptr,        &
                 c_jstat)

    Call surfCtoF(rsnew)

    jstat      = c_jstat

  End Subroutine s1025

!---------------------------------- s1439 -------------------------------------

  Subroutine s1439(ps1, apar, idirec, rcurve, jstat) 

!! PURPOSE
!!   s1439 - Make a constant parameter curve along a given parameter direc-
!!           tion in a surface.

!! INTERFACE
!!   Subroutine s1439(ps1, apar, idirec, rcurve, jstat) 
!!     Type(SISLsurf),  Intent(IN)    :: ps1 
!!     Real(REAL64),    Intent(IN)    :: apar 
!!     Integer,         Intent(IN)    :: idirec 
!!     Type(SISLcurve), Intent(INOUT) :: rcurve
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLsurf),  Intent(IN)    :: ps1 
    Real(REAL64),    Intent(IN)    :: apar 
    Integer,         Intent(IN)    :: idirec 
    Type(SISLcurve), Intent(INOUT) :: rcurve
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_idirec, c_jstat

    Interface
      Subroutine c_s1439(ps1, apar, idirec, rcurve, jstat)                     &
                         BIND(C, name="s1439")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: ps1 
        Real(C_DOUBLE), VALUE               :: apar 
        Integer(C_INT), VALUE               :: idirec 
        Type(C_PTR),          Intent(INOUT) :: rcurve   
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1439
    End Interface

    c_idirec = idirec

    Call c_s1439(ps1%cptr, apar, idirec, rcurve%cptr, c_jstat) 

    Call curveCtoF(rcurve)

    jstat      = c_jstat

  End Subroutine s1439

!---------------------------------- s1383 -------------------------------------

  Subroutine s1383(surf, curve, epsge, maxstep, der, newcurve1, newcurve2,     &
                   newcurve3, stat) 

!! PURPOSE
!!   s1383 - To create a 3D approximation to the curve in a surface, traced
!!           out by a curve in the parameter plane. The output is represented
!!           as a B-spline curve.

!! INTERFACE
!!   Subroutine s1383(surf, curve, epsge, maxstep, der, newcurve1, newcurve2, &
!!                    newcurve3, stat) 
!!     Type(SISLsurf),  Intent(IN)    :: surf 
!!     Type(SISLcurve), Intent(IN)    :: curve 
!!     Real(REAL64),    Intent(IN)    :: epsge 
!!     Real(REAL64),    Intent(IN)    :: maxstep 
!!     Integer,         Intent(IN)    :: der 
!!     Type(SISLcurve), Intent(INOUT) :: newcurve1 
!!     Type(SISLcurve), Intent(INOUT) :: newcurve2 
!!     Type(SISLcurve), Intent(INOUT) :: newcurve3 
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf),  Intent(IN)    :: surf 
    Type(SISLcurve), Intent(IN)    :: curve 
    Real(REAL64),    Intent(IN)    :: epsge 
    Real(REAL64),    Intent(IN)    :: maxstep 
    Integer,         Intent(IN)    :: der 
    Type(SISLcurve), Intent(INOUT) :: newcurve1 
    Type(SISLcurve), Intent(INOUT) :: newcurve2 
    Type(SISLcurve), Intent(INOUT) :: newcurve3 
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_der, c_stat

    Interface
      Subroutine c_s1383(surf, curve, epsge, maxstep, der, newcurve1,          &
                         newcurve2, newcurve3, stat)                           &
                         BIND(C, name="s1383")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf 
        Type(C_PTR),    VALUE               :: curve 
        Real(C_DOUBLE), VALUE               :: epsge 
        Real(C_DOUBLE), VALUE               :: maxstep 
        Integer(C_INT), VALUE               :: der 
        Type(C_PTR),          Intent(INOUT) :: newcurve1   
        Type(C_PTR),          Intent(INOUT) :: newcurve2   
        Type(C_PTR),          Intent(INOUT) :: newcurve3   
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1383
    End Interface

    c_der = der 

    Call c_s1383(surf%cptr, curve%cptr, epsge, maxstep, c_der,          &
                 newcurve1%cptr, newcurve2%cptr, newcurve3%cptr,        &
                 c_stat) 

    Call curveCtoF(newcurve1)
    Call curveCtoF(newcurve2)
    Call curveCtoF(newcurve3)

    stat      = c_stat

  End Subroutine s1383

!---------------------------------- s1001 -------------------------------------

  Subroutine s1001(ps, min1, min2, max1, max2, rsnew, jstat) 

!! PURPOSE
!!   s1001 - To pick a part of a surface. The surface produced will always be
!!           k-regular, i.e. with k-tupple start/end knots.

!! INTERFACE
!!   Subroutine s1001(ps, min1, min2, max1, max2, rsnew, jstat) 
!!     Type(SISLsurf),  Intent(IN)    :: ps 
!!     Real(REAL64),    Intent(IN)    :: min1 
!!     Real(REAL64),    Intent(IN)    :: min2 
!!     Real(REAL64),    Intent(IN)    :: max1 
!!     Real(REAL64),    Intent(IN)    :: max2 
!!     Type(SISLsurf),  Intent(INOUT) :: rsnew
!!     Integer,         Intent(INOUT) :: jstat
 
    Implicit NONE

    Type(SISLsurf),  Intent(IN)    :: ps 
    Real(REAL64),    Intent(IN)    :: min1 
    Real(REAL64),    Intent(IN)    :: min2 
    Real(REAL64),    Intent(IN)    :: max1 
    Real(REAL64),    Intent(IN)    :: max2 
    Type(SISLsurf),  Intent(INOUT) :: rsnew
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_jstat

    Interface
      Subroutine c_s1001(ps, min1, min2, max1, max2, rsnew, jstat)             &
                         BIND(C, name="s1001")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: ps 
        Real(C_DOUBLE), VALUE               :: min1 
        Real(C_DOUBLE), VALUE               :: min2 
        Real(C_DOUBLE), VALUE               :: max1
        Real(C_DOUBLE), VALUE               :: max2
        Type(C_PTR),          Intent(INOUT) :: rsnew   
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1001
    End Interface

    Call c_s1001(ps%cptr, min1, min2, max1, max2, rsnew%cptr, c_jstat) 

    Call surfCtoF(rsnew)

    jstat      = c_jstat

  End Subroutine s1001

!---------------------------------- s1440 -------------------------------------

  Subroutine s1440(surf, newsurf, stat) 

!! PURPOSE
!!   s1440 - Interchange the two parameter directions used in the mathemat-
!!           ical description of a surface and thereby change the direction of
!!           the normal vector of the surface.

!! INTERFACE
!!   Subroutine s1440(surf, newsurf, stat) 
!!     Type(SISLsurf),  Intent(IN)    :: surf
!!     Type(SISLsurf),  Intent(INOUT) :: newsurf
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf),  Intent(IN)    :: surf
    Type(SISLsurf),  Intent(INOUT) :: newsurf
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_stat

    Interface
      Subroutine c_s1440(surf, newsurf,  stat)                                 &
                         BIND(C, name="s1440")

        IMPORT :: C_PTR, C_INT
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf 
        Type(C_PTR),          Intent(INOUT) :: newsurf   
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1440
    End Interface

    Call c_s1440(surf%cptr, newsurf%cptr, c_stat) 

    Call surfCtoF(newsurf)

    stat = c_stat

  End Subroutine s1440

End Module Surface_Utilities 
