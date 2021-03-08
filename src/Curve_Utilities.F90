
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


Module Curve_Utilities 

!! Module Curve_Utilities contains Modern Fortran C-interoperability routines
!! for the C routines described in Chapter 5 of version 4.4 of the SISL
!! reference manual. The drawing and graphics routines (s6drawseq, s6move, and
!! s6line) are not supported at this time.

!! Written by: Richard Weed, Ph.D.
!! Version no. 1.0 - Feb. 2021
!!   Initial development version

!! Contact: rweedmsu@gmail.com

  USE forSISLdata, ONLY: REAL64, C_INT, C_DOUBLE, C_PTR, C_ASSOCIATED, c_free, &
                         SISLcurve, curveCtoF

  Implicit NONE

  PRIVATE :: REAL64, C_INT, C_DOUBLE, C_PTR, C_ASSOCIATED, c_free, SISLcurve,  &
             curveCtoF

Contains

!-------------------------------- newCurve ------------------------------------

  Subroutine newCurve(number, order, knots, coef, ikind, idim, copy, curve)

!! PURPOSE
!!   newCurve - Create and intialize a SISLCurve-instance

!! INTERFACE
!!   Subroutine newCurve(number, order, knots, coef, ikind, idim, copy, curve)
!!     Integer,                 Intent(IN)    :: number
!!     Integer,                 Intent(IN)    :: order
!!     Real(REAL64),            Intent(IN)    :: knots(*)
!!     Real(REAL64),            Intent(IN)    :: coef(*)
!!     Integer,                 Intent(IN)    :: ikind
!!     Integer,                 Intent(IN)    :: idim
!!     Integer,                 Intent(IN)    :: copy
!!     Type(SISLcurve), TARGET, Intent(INOUT) :: curve

    Implicit NONE

    Integer,                 Intent(IN)    :: number, order, ikind, idim, copy
    Real(REAL64),            Intent(IN)    :: knots(*)
    Real(REAL64),            Intent(IN)    :: coef(*)
    Type(SISLcurve), TARGET, Intent(INOUT) :: curve

    Integer(C_INT) :: c_number, c_order, c_kind, c_dim, c_copy

    Interface

      Function c_newCurve(number, order, knots, coef, ikind, idim, copy)       &
                          BIND(C,name="newCurve")

        IMPORT :: C_INT, C_DOUBLE, C_PTR

        Implicit NONE

        Integer(C_INT), VALUE :: number
        Integer(C_INT), VALUE :: order
        Real(C_DOUBLE)        :: knots(*)
        Real(C_DOUBLE)        :: coef(*)
        Integer(C_INT), VALUE :: ikind
        Integer(C_INT), VALUE :: idim
        Integer(C_INT), VALUE :: copy
        Type(C_PTR)           :: c_newCurve

      End Function c_newCurve
    End Interface

    c_number = number
    c_order  = order
    c_kind   = ikind
    c_dim    = idim
    c_copy   = copy

    curve%cptr = c_newCurve(c_number, c_order, knots, coef, c_kind, c_dim,    &
                            c_copy)

    Call curveCtoF(curve)

  End Subroutine newCurve

!-------------------------------- copyCurve -----------------------------------

  Subroutine copyCurve(old_curve, new_curve)

!! PURPOSE
!!   Make a copy of a curve

!! INTERFACE
!!   Subroutine copyCurve(old_curve, new_curve)
!!     Type(SISLCurve), TARGET, Intent(IN)    :: new_curve
!!     Type(SISLCurve), TARGET, Intent(INOUT) :: new_curve

    Implicit NONE

    Type(SISLCurve), TARGET, Intent(IN)    :: old_curve
    Type(SISLCurve), TARGET, Intent(INOUT) :: new_curve

    Interface

      Function c_copyCurve(curve) BIND(C, name="copyCurve")

        IMPORT :: C_PTR

        Type(C_PTR), VALUE :: curve

        Type(C_PTR)        :: c_copyCurve

      End Function c_copyCurve

    End Interface

    new_curve%cptr = c_copyCurve(old_curve%cptr)

    Call curveCtoF(new_curve)

  End Subroutine copyCurve

!-------------------------------- freeCurve -----------------------------------

  Subroutine freeCurve(curve, free_cptr)

!! PURPOSE
!! freeCurve - Free the space occupied by the curve. Before using freeCurve,
!!             make sure the curve object exists.

!! INTERFACE
!!   Subroutine freeCurve(curve, free_cptr)
!!     Type(SISLcurve), TARGET,   Intent(INOUT) :: curve
!!     Logical,         OPTIONAL, Intent(IN)    :: free_cptr

    Implicit NONE

    Type(SISLcurve), TARGET,   Intent(INOUT) :: curve
    Logical,         OPTIONAL, Intent(IN)    :: free_cptr

    Integer :: i
    Logical :: freeptr

    Interface
      Subroutine c_freeCurve(curve_p) BIND(C, name="freeCurve")
        IMPORT :: C_PTR
        Implicit NONE

        Type(C_PTR), VALUE :: curve_p

      End Subroutine c_freeCurve
    End Interface
    
    freeptr = .TRUE.
    If (PRESENT(free_cptr)) freeptr = free_cptr

    curve%in         = 0
    curve%ik         = 0
    curve%ikind      = 0
    curve%idim       = 0
    curve%icopy      = 0
    curve%cuopen     = 0
    curve%pdir%igtpi = 0
    curve%pdir%aang  = 0.0_REAL64
    curve%pbox%imin  = 0
    curve%pbox%imax  = 0
    curve%pbox%etol  = 0.0_REAL64

    If (ASSOCIATED(curve%et))           NULLIFY(curve%et)
    If (ASSOCIATED(curve%ecoef))        NULLIFY(curve%ecoef)
    If (ASSOCIATED(curve%rcoef))        NULLIFY(curve%rcoef)
    If (ASSOCIATED(curve%pdir%ecoef))   NULLIFY(curve%pdir%ecoef)
    If (ASSOCIATED(curve%pdir%esmooth)) NULLIFY(curve%pdir%esmooth)
    If (ASSOCIATED(curve%pbox%emax))    NULLIFY(curve%pbox%emax)
    If (ASSOCIATED(curve%pbox%emin))    NULLIFY(curve%pbox%emin)
    Do i=1,3
      If (ASSOCIATED(curve%pbox%e2max(i)%ptr)) NULLIFY(curve%pbox%e2max(i)%ptr)
      If (ASSOCIATED(curve%pbox%e2min(i)%ptr)) NULLIFY(curve%pbox%e2min(i)%ptr)
    EndDo

! free the instance of the C pointer to the C version of SISLcurve contained
! in the Fortran version. This can be overriden by the free_ptr option to
! avoid a "double free" error when freeing the curve instances contained in
! intCurve objects using freeIntcurve

    If (C_ASSOCIATED(curve%cptr)) Then
      If (freeptr) Call c_freeCurve(curve%cptr)
    EndIf

  End Subroutine freeCurve

!---------------------------------- s1227 -------------------------------------

  Subroutine s1227(curve, der, parvalue, leftknot, deriv, stat)

!! PURPOSE
!!   s1227 - To compute the position and the first derivatives of the curve at
!!           a given parameter value Evaluation from the left hand side.

!! INTERFACE
!!   Subroutine s1227(curve, der, parvalue, leftknot, deriv, stat)
!!     Type(SISLcurve), Intent(IN)    :: curve
!!     Integer,         Intent(IN)    :: der
!!     Real(REAL64),    Intent(IN)    :: parvalue
!!     Integer,         Intent(INOUT) :: leftknot
!!     Real(REAL64),    Intent(INOUT) :: deriv(*)
!!     Integer,         Intent(INOUT) :: stat


    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: curve
    Integer,         Intent(IN)    :: der
    Real(REAL64),    Intent(IN)    :: parvalue
    Integer,         Intent(INOUT) :: leftknot
    Real(REAL64),    Intent(INOUT) :: deriv(*)
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_der, c_leftknot, c_stat

    Interface
      Subroutine c_s1227(curve, der, parvalue, leftknot, deriv, stat)          &
                         BIND(C, name="s1227")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE         :: curve
        Integer(C_INT), VALUE         :: der
        Real(C_DOUBLE), VALUE         :: parvalue
        Integer(C_INT), Intent(INOUT) :: leftknot
        Real(C_DOUBLE), Intent(INOUT) :: deriv(*)
        Integer(C_INT), Intent(INOUT) :: stat

      End Subroutine c_s1227

    End Interface

    c_der      = der
    c_leftknot = leftknot

    deriv(1:((der+1)*curve%idim)) = 0.0_REAL64
    Call c_s1227(curve%cptr, c_der, parvalue, c_leftknot, deriv, c_stat)

    leftknot = c_leftknot
    stat     = c_stat

  End Subroutine s1227 

!---------------------------------- s1221 -------------------------------------

  Subroutine s1221(curve, der, parvalue, leftknot, deriv, stat)

!! PURPOSE
!!   s1221 - To compute the positione and the first derivatives of a curve at a
!!           given parameter value. Evaluation from the right hand side.

!! INTERFACE
!!   Subroutine s1221(curve, der, parvalue, leftknot, deriv, stat)
!!     Type(SISLcurve), Intent(IN)    :: curve
!!     Integer,         Intent(IN)    :: der
!!     Real(REAL64),    Intent(IN)    :: parvalue
!!     Integer,         Intent(INOUT) :: leftknot
!!     Real(REAL64),    Intent(INOUT) :: deriv(*)
!!     Integer,         Intent(INOUT) :: stat


    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: curve
    Integer,         Intent(IN)    :: der
    Real(REAL64),    Intent(IN)    :: parvalue
    Integer,         Intent(INOUT) :: leftknot
    Real(REAL64),    Intent(INOUT) :: deriv(*)
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_der, c_leftknot, c_stat

    Interface
      Subroutine c_s1221(curve, der, parvalue, leftknot, deriv, stat)          &
                         BIND(C, name="s1221")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE         :: curve
        Integer(C_INT), VALUE         :: der
        Real(C_DOUBLE), VALUE         :: parvalue
        Integer(C_INT), Intent(INOUT) :: leftknot
        Real(C_DOUBLE), Intent(INOUT) :: deriv(*)
        Integer(C_INT), Intent(INOUT) :: stat

      End Subroutine c_s1221

    End Interface

    c_der      = der
    c_leftknot = leftknot
    deriv(1:((der+1)*curve%idim)) = 0.0_REAL64

    Call c_s1221(curve%cptr, c_der, parvalue, c_leftknot, deriv, c_stat)

    leftknot = c_leftknot
    stat     = c_stat

  End Subroutine s1221
 
!---------------------------------- s1225 -------------------------------------

  Subroutine s1225(curve, der, parvalue, leftknot, deriv, curvature,           &
                   radius_of_curvature, jstat)

!! PURPOSE
!!   s1225 - Evaluate position, first derivative, curvature and radius of curva-
!!           ture of a curve at a given parameter value, from the left hand
!!           side.

!! INTERFACE
!!   Subroutine s1225(curve, der, parvalue, leftknot, deriv, curvature,        &
!!                    radius_of_curvature, jstat)
!!     Type(SISLcurve), Intent(IN)    :: curve
!!     Integer,         Intent(IN)    :: der
!!     Real(REAL64),    Intent(IN)    :: parvalue
!!     Integer,         Intent(INOUT) :: leftknot
!!     Real(REAL64),    Intent(INOUT) :: deriv(*)
!!     Real(REAL64),    Intent(INOUT) :: curvature(*)
!!     Real(REAL64),    Intent(INOUT) :: radius_of_curvature
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: curve
    Integer,         Intent(IN)    :: der
    Real(REAL64),    Intent(IN)    :: parvalue
    Integer,         Intent(INOUT) :: leftknot
    Real(REAL64),    Intent(INOUT) :: deriv(*)
    Real(REAL64),    Intent(INOUT) :: curvature(*)
    Real(REAL64),    Intent(INOUT) :: radius_of_curvature
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_der, c_leftknot, c_jstat

    Interface
      Subroutine c_s1225(curve, der, parvalue, leftknot, deriv, curvature,   &
                         radius_of_curvature, jstat)                         &
                         BIND(C, name="s1225")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE         :: curve
        Integer(C_INT), VALUE         :: der
        Real(C_DOUBLE), VALUE         :: parvalue
        Integer(C_INT), Intent(INOUT) :: leftknot
        Real(C_DOUBLE), Intent(INOUT) :: deriv(*)
        Real(C_DOUBLE), Intent(INOUT) :: curvature(*)
        Real(C_DOUBLE), Intent(INOUT) :: radius_of_curvature 
        Integer(C_INT), Intent(INOUT) :: jstat

      End Subroutine c_s1225
    End Interface

    c_der      = der
    c_leftknot = leftknot

    deriv(1:((der+1)*curve%idim)) = 0.0_REAL64
    curvature(1:curve%idim)        = 0.0_REAL64

    Call c_s1225(curve%cptr, c_der, parvalue, c_leftknot, deriv,           &
                 curvature, radius_of_curvature, c_jstat)

    leftknot = c_leftknot
    jstat    = c_jstat

  End Subroutine s1225
 
!---------------------------------- s1226 -------------------------------------

  Subroutine s1226(curve, der, parvalue, leftknot, deriv, curvature,           &
                   radius_of_curvature, jstat)

!! PURPOSE
!!   s1226 - Evaluate position, first derivative, curvature and radius of curva-
!!           ture of a curve at a given parameter value, from the right hand
!!           side.

!! INTERFACE
!!   Subroutine s1226(curve, der, parvalue, leftknot, deriv, curvature,        &
!!                    radius_of_curvature, jstat)
!!     Type(SISLcurve), Intent(IN)    :: curve
!!     Integer,         Intent(IN)    :: der
!!     Real(REAL64),    Intent(IN)    :: parvalue
!!     Integer,         Intent(INOUT) :: leftknot
!!     Real(REAL64),    Intent(INOUT) :: deriv(*)
!!     Real(REAL64),    Intent(INOUT) :: curvature(*)
!!     Real(REAL64),    Intent(INOUT) :: radius_of_curvature
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: curve
    Integer,         Intent(IN)    :: der
    Real(REAL64),    Intent(IN)    :: parvalue
    Integer,         Intent(INOUT) :: leftknot
    Real(REAL64),    Intent(INOUT) :: deriv(*)
    Real(REAL64),    Intent(INOUT) :: curvature(*)
    Real(REAL64),    Intent(INOUT) :: radius_of_curvature
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_der, c_leftknot, c_jstat

    Interface
      Subroutine c_s1226(curve, der, parvalue, leftknot, deriv, curvature,   &
                         radius_of_curvature, jstat) BIND(C, name="s1226")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE         :: curve
        Integer(C_INT), VALUE         :: der
        Real(C_DOUBLE), VALUE         :: parvalue
        Integer(C_INT), Intent(INOUT) :: leftknot
        Real(C_DOUBLE), Intent(INOUT) :: deriv(*)
        Real(C_DOUBLE), Intent(INOUT) :: curvature(*)
        Real(C_DOUBLE), Intent(INOUT) :: radius_of_curvature 
        Integer(C_INT), Intent(INOUT) :: jstat

      End Subroutine c_s1226
    End Interface

    c_der      = der
    c_leftknot = leftknot
    deriv(1:((der+1)*curve%idim)) = 0.0_REAL64
    curvature(1:curve%idim)        = 0.0_REAL64

    Call c_s1226(curve%cptr, c_der, parvalue, c_leftknot, deriv,               &
                 curvature, radius_of_curvature, c_jstat)

    leftknot = c_leftknot
    jstat    = c_jstat

  End Subroutine s1226

!---------------------------------- s1542 -------------------------------------

  Subroutine s1542(pc1, m, x, eder, jstat)

!! PURPOSE
!!   s1542 - Evaluate the curve defined by pc1 over a m grid of points
!!           (x(i)). Only positions are evaluated. This does not work for in the
!!                   rational case.

!! INTERFACE
!!   Subroutine s1542(pc1, m, x, eder, jstat)
!!     Type(SISLcurve), Intent(IN)    :: pc1 
!!     Integer,         Intent(IN)    :: m 
!!     Real(REAL64),    Intent(INOUT) :: x(*)
!!     Real(REAL64),    Intent(INOUT) :: eder(*)
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: pc1 
    Integer,         Intent(IN)    :: m 
    Real(REAL64),    Intent(INOUT) :: x(*)
    Real(REAL64),    Intent(INOUT) :: eder(*)
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_m, c_jstat

    Interface
      Subroutine c_s1542(pc1, m, x, eder, jstat) BIND(C, name="s1542")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE         :: pc1 
        Integer(C_INT), VALUE         :: m 
        Real(C_DOUBLE), Intent(INOUT) :: x(*)
        Real(C_DOUBLE), Intent(INOUT) :: eder(*)
        Integer(C_INT), Intent(INOUT) :: jstat

      End Subroutine c_s1542
    End Interface

    c_m = m 
    eder(1:(pc1%idim*m)) = 0.0_REAL64

    Call c_s1542(pc1%cptr, c_m, x, eder, c_jstat)

    jstat = c_jstat

  End Subroutine s1542 

!---------------------------------- s1710 -------------------------------------

  Subroutine s1710(pc1, apar, rcnew1, rcnew2, jstat)

!! PURPOSE
!!   s1710 - Subdivide a curve at a given parameter value.
!!           NOTE: When the curve is periodic (i.e. when the cuopen flag of
!!           the curve has value = −1), this function will return only ONE
!!           curve through rcnew1. This curve is the same geometric curve as
!!           pc1, but is represented on a closed basis, i.e. with k-tuple start/end
!!           the knots and coinciding start/end coefficients. The cuopen flag of
!!           curve will then be set to closed (= 0) and a status value jstat 
!!           equal to 2 will be returned.

!! INTERFACE
!!   Subroutine s1710(pc1, apar, rcnew1, rcnew2, jstat)
!!     Type(SISLcurve), Intent(IN)    :: pc1
!!     Real(REAL64),    Intent(IN)    :: apar
!!     Type(SISLcurve), Intent(INOUT) :: rcnew1
!!     Type(SISLcurve), Intent(INOUT) :: rcnew2
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: pc1
    Real(REAL64),    Intent(IN)    :: apar
    Type(SISLcurve), Intent(INOUT) :: rcnew1
    Type(SISLcurve), Intent(INOUT) :: rcnew2
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_jstat

    Interface
      Subroutine c_s1710 (pc1, apar, rcnew1, rcnew2, jstat)                    &
                          BIND(C, name="s1710")
 
        IMPORT :: C_PTR, C_DOUBLE, C_INT
        Implicit NONE
     
        Type(C_PTR),    VALUE               :: pc1
        Real(C_DOUBLE), VALUE               :: apar
        Type(C_PTR),          Intent(INOUT) :: rcnew1 
        Type(C_PTR),          Intent(INOUT) :: rcnew2
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1710 
    End Interface 
    
    Call c_s1710(pc1%cptr, apar, rcnew1%cptr, rcnew2%cptr, c_jstat)

    jstat = c_jstat

    Call curveCtoF(rcnew1)
    Call curveCtoF(rcnew2)

  End Subroutine s1710

!---------------------------------- s1017 -------------------------------------

  Subroutine s1017(pc, rc, apar, jstat)

!!PURPOSE
!!  s1017 - Insert a given knot into the description of a curve.
!!          NOTE : When the curve is periodic (i.e. the curve flag cuopen =
!!          −1), the input parameter value must lie in the half-open [et(kk −
!!         1), el(kn) interval, the function will automatically update the extra
!!          knots and coeffisients. rcnew->in is still equal to pc->in + 1!

!! INTERFACE
!!   Subroutine s1017(pc, rc, apar, jstat)
!!     Type(SISLcurve), Intent(IN)    :: pc
!!     Type(SISLcurve), Intent(INOUT) :: rc
!!     Real(REAL64),    Intent(IN)    :: apar
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: pc
    Type(SISLcurve), Intent(INOUT) :: rc
    Real(REAL64),    Intent(IN)    :: apar
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_jstat

    Interface
      Subroutine c_s1017 (pc, rc, apar, jstat)                                 &
                          BIND(C, name="s1017")
 
        IMPORT :: C_PTR, C_DOUBLE, C_INT
        Implicit NONE
     
        Type(C_PTR),    VALUE               :: pc
        Type(C_PTR),          Intent(INOUT) :: rc 
        Real(C_DOUBLE), VALUE               :: apar
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1017 
    End Interface 
    
    Call c_s1017(pc%cptr, rc%cptr, apar, c_jstat)

    jstat = c_jstat

    Call curveCtoF(rc)

  End Subroutine s1017

!---------------------------------- s1018 -------------------------------------

  Subroutine s1018(pc, epar, inpar, rcnew, jstat)

!! PURPOSE
!!   s1018 - Insert a given set of knots into the description of a curve.
!!           NOTE : When the curve is periodic (i.e. when the curve flag
!!           cuopen = −1), the input parameter values must lie in the half-
!!           open (et(kk), et(kn+1), the function will automatically update
!!           the extra knots and coeffisients. The rcnew%in will still be equal
!!           to pc%in + inpar.

!! INTERFACE
!!   Subroutine s1018(pc, epar, inpar, rcnew, jstat)
!!     Type(SISLcurve), Intent(IN)    :: pc
!!     Real(REAL64),    Intent(IN)    :: epar(*)
!!     Integer,         Intent(IN)    :: inpar
!!     Type(SISLcurve), Intent(INOUT) :: rcnew
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: pc
    Real(REAL64),    Intent(IN)    :: epar(*)
    Integer,         Intent(IN)    :: inpar
    Type(SISLcurve), Intent(INOUT) :: rcnew
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_jstat, c_inpar

    Interface
      Subroutine c_s1018 (pc, epar, inpar, rcnew, jstat)                    &
                          BIND(C, name="s1018")
 
        IMPORT :: C_PTR, C_DOUBLE, C_INT
        Implicit NONE
     
        Type(C_PTR),    VALUE               :: pc
        Real(C_DOUBLE),       Intent(IN)    :: epar(*)
        Integer(C_INT),       Intent(IN)    :: inpar
        Type(C_PTR),          Intent(INOUT) :: rcnew 
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1018 
    End Interface 
   
    c_inpar = inpar 
    Call c_s1018(pc%cptr, epar, c_inpar, rcnew%cptr, c_jstat)

    jstat = c_jstat

    Call curveCtoF(rcnew)

  End Subroutine s1018

!---------------------------------- s1714 -------------------------------------

  Subroutine s1714(curve, parval1, parval2, newcurve1, newcurve2, stat)

!! PURPOSE
!!   s1714 - Split a curve in two parts at two specified parameter values. The
!!           first curve starts at parval1. If the curve is open, the last part
!!           of the curve is translated so that the end of the curve joins the
!!           start.

!! INTERFACE
!!   Subroutine s1712(curve, parval1, parval2, newcurve1, newcurve2, stat)
!!     Type(SISLcurve), Intent(IN)    :: curve
!!     Real(REAL64),    Intent(IN)    :: parval1 
!!     Real(REAL64),    Intent(IN)    :: parval2 
!!     Type(SISLcurve), Intent(INOUT) :: newcurve1
!!     Type(SISLcurve), Intent(INOUT) :: newcurve2
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: curve
    Real(REAL64),    Intent(IN)    :: parval1 
    Real(REAL64),    Intent(IN)    :: parval2 
    Type(SISLcurve), Intent(INOUT) :: newcurve1
    Type(SISLcurve), Intent(INOUT) :: newcurve2
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_stat

    Interface
      Subroutine c_s1714(curve, parval1, parval2, newcurve1, newcurve2, stat)  &
                         BIND(C, name="s1712")
 
        IMPORT :: C_PTR, C_DOUBLE, C_INT
        Implicit NONE
     
        Type(C_PTR),    VALUE               :: curve
        Real(C_DOUBLE), VALUE               :: parval1 
        Real(C_DOUBLE), VALUE               :: parval2 
        Type(C_PTR),          Intent(INOUT) :: newcurve1 
        Type(C_PTR),          Intent(INOUT) :: newcurve2 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1714 
    End Interface 
   
    Call c_s1714(curve%cptr, parval1, parval2, newcurve1%cptr, newcurve2%cptr, &
                 c_stat)

    stat = c_stat

    Call curveCtoF(newcurve1)
    Call curveCtoF(newcurve2)

  End Subroutine s1714

!---------------------------------- s1712 -------------------------------------

  Subroutine s1712(curve, begpar, endpar, newcurve, stat)

!! PURPOSE
!!   s1712 - To pick one part of a curve and make a new curve of the part. If
!!           endpar < begpar the direction of the new curve is turned. Use
!!           s1713() to pick a curve part crossing the start/end points of a
!!           closed (or periodic) curve.

!! INTERFACE
!!   Subroutine s1712(curve, begpar, endpar, newcurve, stat)
!!     Type(SISLcurve), Intent(IN)    :: curve
!!     Real(REAL64),    Intent(IN)    :: begpar
!!     Real(REAL64),    Intent(IN)    :: endpar
!!     Type(SISLcurve), Intent(INOUT) :: newcurve
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: curve
    Real(REAL64),    Intent(IN)    :: begpar
    Real(REAL64),    Intent(IN)    :: endpar
    Type(SISLcurve), Intent(INOUT) :: newcurve
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_stat

    Interface
      Subroutine c_s1712 (curve, begpar, endpar, newcurve, stat)              &
                          BIND(C, name="s1712")
 
        IMPORT :: C_PTR, C_DOUBLE, C_INT
        Implicit NONE
     
        Type(C_PTR),    VALUE               :: curve
        Real(C_DOUBLE), VALUE               :: begpar
        Real(C_DOUBLE), VALUE               :: endpar
        Type(C_PTR),          Intent(INOUT) :: newcurve 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1712 
    End Interface 
   
    Call c_s1712(curve%cptr, begpar, endpar, newcurve%cptr, c_stat)

    stat = c_stat

    Call curveCtoF(newcurve)

  End Subroutine s1712

!---------------------------------- s1713 -------------------------------------

  Subroutine s1713(curve, begpar, endpar, newcurve, stat)

!! PURPOSE
!!   s1713 - To pick one part of a closed curve and make a new curve of that
!!           part. If the routine is used on an open curve and endpar ≤ begpar,
!!           the last part of the curve is translated so that the end of the
!!           curve joins the start.

!! INTERFACE
!!   Subroutine s1713(curve, begpar, endpar, newcurve, stat)
!!     Type(SISLcurve), Intent(IN)    :: curve 
!!     Real(REAL64),    Intent(IN)    :: begpar
!!     Real(REAL64),    Intent(IN)    :: endpar
!!     Type(SISLcurve), Intent(INOUT) :: newcurve
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: curve 
    Real(REAL64),    Intent(IN)    :: begpar
    Real(REAL64),    Intent(IN)    :: endpar
    Type(SISLcurve), Intent(INOUT) :: newcurve
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_stat

    Interface
      Subroutine c_s1713 (curve, begpar, endpar, newcurve, stat)              &
                          BIND(C, name="s1713")
 
        IMPORT :: C_PTR, C_DOUBLE, C_INT
        Implicit NONE
     
        Type(C_PTR),    VALUE               :: curve
        Real(C_DOUBLE), VALUE               :: begpar
        Real(C_DOUBLE), VALUE               :: endpar
        Type(C_PTR),          Intent(INOUT) :: newcurve 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1713 
    End Interface 
   
    Call c_s1713(curve%cptr, begpar, endpar, newcurve%cptr, c_stat)

    stat = c_stat

    Call curveCtoF(newcurve)

  End Subroutine s1713

!---------------------------------- s1715 -------------------------------------

  Subroutine s1715(curve1, curve2, end1, end2, newcurve, stat)

!! PURPOSE
!!   s1715 - To join one end of one curve with one end of another curve by
!!           translating the second curve. If curve1 is to be joined at the
!!           start, the direction of the curve is turned. If curve2 is to be 
!!           joined at the end, the direction of this curve is turned. This
!!           means that curve1 always makes the first part of the new curve.

!! INTERFACE
!!   Subroutine s1715(curve1, curve2, end1, end2, newcurve, stat)
!!     Type(SISLcurve), Intent(IN)    :: curve1 
!!     Type(SISLcurve), Intent(IN)    :: curve2 
!!     Integer,         Intent(IN)    :: end1 
!!     Integer,         Intent(IN)    :: end2 
!!     Type(SISLcurve), Intent(INOUT) :: newcurve
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: curve1 
    Type(SISLcurve), Intent(IN)    :: curve2 
    Integer,         Intent(IN)    :: end1 
    Integer,         Intent(IN)    :: end2 
    Type(SISLcurve), Intent(INOUT) :: newcurve
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_stat, c_end1, c_end2

    Interface
      Subroutine c_s1715 (curve1, curve2, end1, end2, newcurve, stat)         &
                          BIND(C, name="s1715")
 
        IMPORT :: C_PTR, C_INT
        Implicit NONE
     
        Type(C_PTR),    VALUE               :: curve1
        Type(C_PTR),    VALUE               :: curve2
        Integer(C_INT), VALUE               :: end1 
        Integer(C_INT), VALUE               :: end2 
        Type(C_PTR),          Intent(INOUT) :: newcurve 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1715 
    End Interface 

    c_end1 = end1
    c_end2 = end2
 
    Call c_s1715(curve1%cptr, curve2%cptr, c_end1, c_end2, newcurve%cptr,      &
                 c_stat)

    stat = c_stat

    Call curveCtoF(newcurve)

  
  End Subroutine s1715

!---------------------------------- s1716 -------------------------------------

  Subroutine s1716(curve1, curve2, espge, newcurve, stat)

!! PURPOSE
!!   s1716 - To join two curves at the ends that lie closest to each other, if
!!           the distance between the ends is less than the tolerance epsge. If
!!           curve1 is to be joined at the start, the direction of the curve is
!!           turned. If curve2 is to be joined at the end, the direction of this
!!           curve is turned. This means that curve1 always makes up the first
!!           part of the new curve. If epsge is positive, but smaller than the
!!           smallest distance between the ends of the two curves, a NULL
!!           pointer is returned.

!! INTERFACE
!!   Subroutine s1716(curve1, curve2, espge, newcurve, stat)
!!     Type(SISLcurve), Intent(IN)    :: curve1 
!!     Type(SISLcurve), Intent(IN)    :: curve2 
!!     Real(REAL64),    Intent(IN)    :: espge 
!!     Type(SISLcurve), Intent(INOUT) :: newcurve
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: curve1 
    Type(SISLcurve), Intent(IN)    :: curve2 
    Real(REAL64),    Intent(IN)    :: espge 
    Type(SISLcurve), Intent(INOUT) :: newcurve
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_stat

    Interface
      Subroutine c_s1716 (curve1, curve2, espge, newcurve, stat)               &
                          BIND(C, name="s1716")
 
        IMPORT :: C_PTR, C_DOUBLE, C_INT
        Implicit NONE
     
        Type(C_PTR),    VALUE               :: curve1
        Type(C_PTR),    VALUE               :: curve2
        Real(C_DOUBLE), VALUE               :: espge 
        Type(C_PTR),          Intent(INOUT) :: newcurve 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1716 
    End Interface 

    Call c_s1716(curve1%cptr, curve2%cptr, espge, newcurve%cptr, c_stat)

    stat = c_stat

    Call curveCtoF(newcurve)

  End Subroutine s1716

!---------------------------------- s1706 -------------------------------------

  Subroutine s1706(curve)

!! PURPOSE
!!   s1706 - Turn the direction of a curve by reversing the ordering of the
!!           coefficients. The start parameter value of the new curve is the
!!           same as the start parameter value of the old curve. This routine
!!           turns the direction of the orginal curve. If you want a copy with
!!           a turned direction, just make a copy and turn the direction of the
!!           copy.

!! INTERFACE
!!   Subroutine s1706(curve)
!!     Type(SISLcurve), Intent(INOUT) :: curve 

    Implicit NONE

    Type(SISLcurve), Intent(INOUT) :: curve 


    Interface
      Subroutine c_s1706 (curve) BIND(C, name="s1706")
 
        IMPORT :: C_PTR
        Implicit NONE
     
        Type(C_PTR), VALUE  :: curve

      End Subroutine c_s1706 
    End Interface 


    Call c_s1706(curve%cptr)
    If (C_ASSOCIATED(curve%cptr)) Then
      If (ASSOCIATED(curve%et))    NULLIFY(curve%et)
      If (ASSOCIATED(curve%ecoef)) NULLIFY(curve%ecoef)
      If (ASSOCIATED(curve%rcoef)) NULLIFY(curve%rcoef)
    EndIf

    Call curveCtoF(curve)

  End Subroutine s1706

!---------------------------------- s1233 -------------------------------------

  Subroutine s1233(pc, afak1, afak2, rc, jstat)

!! PURPOSE
!!   s1233 - To extend a B-spline curve (i.e. NOT rationals) at the start and/or
!!           the end of the curve by continuing the polynomial behaviour of
!!           the curve.

!! INTERFACE
!!   Subroutine s1233(pc, afak1, afak2, rc, jstat)
!!     Type(SISLCurve), Intent(IN)    :: pc 
!!     Real(REAL64),    Intent(IN)    :: afak1
!!     Real(REAL64),    Intent(IN)    :: afak2
!!     Type(SISLCurve), Intent(INOUT) :: rc
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLCurve), Intent(IN)    :: pc 
    Real(REAL64),    Intent(IN)    :: afak1
    Real(REAL64),    Intent(IN)    :: afak2
    Type(SISLCurve), Intent(INOUT) :: rc
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_jstat

    Interface
      Subroutine c_s1233 (pc, afak1, afak2, rc, jstat)                         &
                          BIND(C, name="s1233")
 
        IMPORT :: C_PTR, C_DOUBLE, C_INT
        Implicit NONE
     
        Type(C_PTR),    VALUE               :: pc
        Real(C_DOUBLE), VALUE               :: afak1
        Real(C_DOUBLE), VALUE               :: afak2
        Type(C_PTR),          Intent(INOUT) :: rc 
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1233 
    End Interface 
   
    Call c_s1233(pc%cptr, afak1, afak2, rc%cptr, c_jstat)

    jstat = c_jstat

    Call curveCtoF(rc)

  End Subroutine s1233

End Module Curve_Utilities 
