   
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


Module Data_Reduction

!! Module Data_Reduction contains Modern Fortran C-interoperability routines
!! for the C functions described in Chapter 10 of version 4.4 of the SISL
!! reference manual

!! Written by: Richard Weed, Ph.D.
!! Version no. 1.0 - Feb. 2021
!!   Initial development version

!! Contact: rweedmsu@gmail.com

  USE forSISLdata, ONLY: REAL64, C_INT, C_DOUBLE, C_PTR, C_LOC, C_NULL_PTR,    &
                         SISLcurve, SISLsurf, curveCtoF, surfCtoF

  Implicit NONE

  PRIVATE :: REAL64, C_INT, C_DOUBLE, C_PTR, C_LOC, C_NULL_PTR, SISLcurve,     &
             SISLsurf

Contains

!---------------------------------- s1940 -------------------------------------

  Subroutine s1940(oldcurve, eps, startfix, endfix, iopen, itmax, newcurve,    &
                   maxerr, stat)

!! PURPOSE
!!   s1940 - To remove as many knots as possible from a spline curve without
!!           perturbing the curve more than a given tolerance.

!! INTERFACE
!!   Subroutine s1940(oldcurve, eps, startfix, endfix, iopen, itmax, newcurve, &
!!                    maxerr, stat)
!!     Type(SISLcurve), Intent(IN)    :: oldcurve
!!     Real(REAL64),    Intent(IN)    :: eps(*)
!!     Integer,         Intent(IN)    :: startfix 
!!     Integer,         Intent(IN)    :: endfix 
!!     Integer,         Intent(IN)    :: iopen 
!!     Integer,         Intent(IN)    :: itmax 
!!     Type(SISLcurve), Intent(INOUT) :: newcurve
!!     Real(REAL64),    Intent(INOUT) :: maxerr(*)
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: oldcurve
    Real(REAL64),    Intent(IN)    :: eps(*)
    Integer,         Intent(IN)    :: startfix 
    Integer,         Intent(IN)    :: endfix 
    Integer,         Intent(IN)    :: iopen 
    Integer,         Intent(IN)    :: itmax 
    Type(SISLcurve), Intent(INOUT) :: newcurve
    Real(REAL64),    Intent(INOUT) :: maxerr(*)
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_startfix, c_endfix, c_iopen, c_itmax, c_stat

    Interface
      Subroutine c_s1940(oldcurve, eps, startfix, endfix, iopen, itmax,        &
                         newcurve, maxerr, stat)                               &
                         BIND(C,name="s1940")

        IMPORT :: C_PTR, C_DOUBLE, C_INT
        Implicit NONE

        Type(C_PTR),    VALUE               :: oldcurve
        Real(C_DOUBLE),       Intent(IN)    :: eps(*)
        Integer(C_INT), VALUE               :: startfix
        Integer(C_INT), VALUE               :: endfix
        Integer(C_INT), VALUE               :: iopen 
        Integer(C_INT), VALUE               :: itmax 
        Type(C_PTR),          Intent(INOUT) :: newcurve
        Real(C_DOUBLE),       Intent(INOUT) :: maxerr(*)
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1940
    End Interface

    c_startfix = startfix
    c_endfix   = endfix
    c_iopen    = iopen 
    c_itmax    = itmax 

    maxerr(1:oldcurve%idim) = 0.0_REAL64
       
    Call c_s1940(oldcurve%cptr, eps, c_startfix, c_endfix, c_iopen,        &
                 c_itmax, newcurve%cptr, maxerr, c_stat)

    Call curveCtoF(newcurve)

    stat = c_stat

  End Subroutine s1940

!---------------------------------- s1961 -------------------------------------

  Subroutine s1961(ep, im, idim, ipar, epar, eeps, ilend, irend, iopen,        &
                   afctol, itmax, ik, rc, emxerr, jstat)

!! PURPOSE
!!   s1961 - To compute a spline-approximation to the data given by the points
!!           ep, and represent it as a B-spline curve with parameterization de-
!!           termined by the parameter ipar. The approximation is determined
!!           by first forming the piecewise linear interpolant to the data, and
!!           then performing knot removal on this initial approximation.

!! INTERFACE
!!   Subroutine s1961(ep, im, idim, ipar, epar, eeps, ilend, irend, iopen,     &
!!                    afctol, itmax, ik, rc, emxerr, jstat)
!!     Integer,         Intent(IN)    :: im 
!!     Integer,         Intent(IN)    :: idim 
!!     Integer,         Intent(IN)    :: ipar 
!!     Real(REAL64),    Intent(IN)    :: epar(:)
!!     Real(REAL64),    Intent(IN)    :: eeps(*)
!!     Integer,         Intent(IN)    :: ilend 
!!     Integer,         Intent(IN)    :: irend 
!!     Integer,         Intent(IN)    :: iopen 
!!     Real(REAL64),    Intent(IN)    :: afctol 
!!     Integer,         Intent(IN)    :: itmax 
!!     Integer,         Intent(IN)    :: ik 
!!     Type(SISLcurve), Intent(INOUT) :: rc 
!!     Real(REAL64),    Intent(INOUT) :: emxerr(*)
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Real(REAL64),            Intent(IN)    :: ep(*)
    Integer,                 Intent(IN)    :: im 
    Integer,                 Intent(IN)    :: idim 
    Integer,                 Intent(IN)    :: ipar 
    Real(REAL64),    TARGET, Intent(IN)    :: epar(:)
    Real(REAL64),            Intent(IN)    :: eeps(*)
    Integer,                 Intent(IN)    :: ilend 
    Integer,                 Intent(IN)    :: irend 
    Integer,                 Intent(IN)    :: iopen 
    Real(REAL64),            Intent(IN)    :: afctol 
    Integer,                 Intent(IN)    :: itmax 
    Integer,                 Intent(IN)    :: ik 
    Type(SISLcurve),         Intent(INOUT) :: rc 
    Real(REAL64),            Intent(INOUT) :: emxerr(*)
    Integer,                 Intent(INOUT) :: jstat

    Integer(C_INT) :: c_im, c_idim, c_ipar, c_ilend, c_irend, c_iopen,         &
                      c_itmax, c_ik, c_jstat
    Type(C_PTR)  :: c_epar_p

    Interface
      Subroutine c_s1961(ep, im, idim, ipar, epar, eeps, ilend, irend, iopen,  &
                         afctol, itmax, ik, rc, emxerr, jstat)                 &
                         BIND(C,name="s1961")

        IMPORT :: C_PTR, C_DOUBLE, C_INT
        Implicit NONE

        Real(C_DOUBLE),       Intent(IN)    :: ep(*)
        Integer(C_INT), VALUE               :: im
        Integer(C_INT), VALUE               :: idim
        Integer(C_INT), VALUE               :: ipar
        Type(C_PTR),    VALUE               :: epar
        Real(C_DOUBLE),       Intent(IN)    :: eeps(*)
        Integer(C_INT), VALUE               :: ilend 
        Integer(C_INT), VALUE               :: irend 
        Integer(C_INT), VALUE               :: iopen 
        Real(C_DOUBLE), VALUE               :: afctol 
        Integer(C_INT), VALUE               :: itmax 
        Integer(C_INT), VALUE               :: ik 
        Type(C_PTR),          Intent(INOUT) :: rc
        Real(C_DOUBLE),       Intent(INOUT) :: emxerr(*)
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1961
    End Interface

    c_im    = im 
    c_idim  = idim
    c_ipar  = ipar 
    c_ilend = ilend 
    c_irend = irend 
    c_iopen = iopen 
    c_itmax = itmax 
    c_ik    = ik 

    c_epar_p = C_NULL_PTR
    If (ipar==3 .AND. SIZE(epar) >= 0) Then
      c_epar_p = C_LOC(epar)
    EndIf

    emxerr(1:idim) = 0.0_REAL64
       
    Call c_s1961(ep, c_im, c_idim, c_ipar, c_epar_p, eeps, c_ilend, c_irend,   &
                 c_iopen, afctol, c_itmax, c_ik, rc%cptr, emxerr, c_jstat)

    Call curveCtoF(rc)

    jstat = c_jstat

  End Subroutine s1961

!---------------------------------- s1962 -------------------------------------

  Subroutine s1962(ep, ev, im, idim, ipar, epar, eeps, ilend, irend, iopen,    &
                   itmax, rc, emxerr, jstat)

!! PURPOSE
!!   s1962 - To compute the approximation to the data given by the points
!!           ep and the derivatives (tangents) ev, and represent it as a B-
!!           spline curve with parametrization determined by the parameter
!!           ipar. The approximation is determined by first forming the cubic
!!           hermite interpolant to the data, and then performing knot removal
!!           on this initial approximation.

!! INTERFACE
!!   Subroutine s1962(ep, ev, im, idim, ipar, epar, eeps, ilend, irend, iopen,    &
!!                    itmax, rc, emxerr, jstat)
!!     Real(REAL64),    Intent(IN)    :: ep(*)
!!     Real(REAL64),    Intent(IN)    :: ev(*)
!!     Integer,         Intent(IN)    :: im 
!!     Integer,         Intent(IN)    :: idim 
!!     Integer,         Intent(IN)    :: ipar 
!!     Real(REAL64),    Intent(IN)    :: epar(:)
!!     Real(REAL64),    Intent(IN)    :: eeps(*)
!!     Integer,         Intent(IN)    :: ilend 
!!     Integer,         Intent(IN)    :: irend 
!!     Integer,         Intent(IN)    :: iopen 
!!     Integer,         Intent(IN)    :: itmax 
!!     Type(SISLcurve), Intent(INOUT) :: rc 
!!     Real(REAL64),    Intent(INOUT) :: emxerr(*)
!!     Integer,         Intent(INOUT) :: jstat


    Implicit NONE

    Real(REAL64),            Intent(IN)    :: ep(*)
    Real(REAL64),            Intent(IN)    :: ev(*)
    Integer,                 Intent(IN)    :: im 
    Integer,                 Intent(IN)    :: idim 
    Integer,                 Intent(IN)    :: ipar 
    Real(REAL64),    TARGET, Intent(IN)    :: epar(:)
    Real(REAL64),            Intent(IN)    :: eeps(*)
    Integer,                 Intent(IN)    :: ilend 
    Integer,                 Intent(IN)    :: irend 
    Integer,                 Intent(IN)    :: iopen 
    Integer,                 Intent(IN)    :: itmax 
    Type(SISLcurve),         Intent(INOUT) :: rc 
    Real(REAL64),            Intent(INOUT) :: emxerr(*)
    Integer,                 Intent(INOUT) :: jstat

    Integer(C_INT) :: c_im, c_idim, c_ipar, c_ilend, c_irend, c_iopen,         &
                      c_itmax, c_jstat
    Type(C_PTR) :: c_epar_p

    Interface
      Subroutine c_s1962(ep, ev, im, idim, ipar, epar, eeps, ilend, irend,     &
                         iopen, itmax, rc, emxerr, jstat)                      &
                         BIND(C,name="s1962")

        IMPORT :: C_PTR, C_DOUBLE, C_INT
        Implicit NONE

        Real(C_DOUBLE),       Intent(IN)    :: ep(*)
        Real(C_DOUBLE),       Intent(IN)    :: ev(*)
        Integer(C_INT), VALUE               :: im 
        Integer(C_INT), VALUE               :: idim 
        Integer(C_INT), VALUE               :: ipar 
        Type(C_PTR),    VALUE               :: epar
        Real(C_DOUBLE),       Intent(IN)    :: eeps(*)
        Integer(C_INT), VALUE               :: ilend 
        Integer(C_INT), VALUE               :: irend 
        Integer(C_INT), VALUE               :: iopen 
        Integer(C_INT), VALUE               :: itmax 
        Type(C_PTR),          Intent(INOUT) :: rc
        Real(C_DOUBLE),       Intent(INOUT) :: emxerr(*)
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1962
    End Interface

    c_im    = im 
    c_idim  = idim
    c_ipar  = ipar 
    c_ilend = ilend 
    c_irend = irend 
    c_iopen = iopen 
    c_itmax = itmax 

    c_epar_p = C_NULL_PTR
    If (ipar==3 .AND. SIZE(epar) > 0) Then
      c_epar_p = C_LOC(epar)
    EndIf

    emxerr(1:idim) = 0.0_REAL64
       
    Call c_s1962(ep, ev, c_im, c_idim, c_ipar, c_epar_p, eeps, c_ilend,        &
                 c_irend, c_iopen, c_itmax, rc%cptr, emxerr, c_jstat)

    Call curveCtoF(rc)

    jstat = c_jstat

  End Subroutine s1962

!---------------------------------- s1963 -------------------------------------

  Subroutine s1963(pc, eeps, ilend, irend, iopen, itmax, rc, emxerr, jstat)

!! PURPOSE
!!   s1963 - To approximate the input spline curve by a cubic spline curve with
!!           error less than eeps in each of the kdim components.

!! INTERFACE
!!   Subroutine s1963(pc, eeps, ilend, irend, iopen, itmax, rc, emxerr, jstat)
!!     Type(SISLcurve), Intent(IN)    :: pc 
!!     Real(REAL64),    Intent(IN)    :: eeps(*)
!!     Integer,         Intent(IN)    :: ilend 
!!     Integer,         Intent(IN)    :: irend 
!!     Integer,         Intent(IN)    :: iopen 
!!     Integer,         Intent(IN)    :: itmax 
!!     Type(SISLcurve), Intent(INOUT) :: rc 
!!     Real(REAL64),    Intent(INOUT) :: emxerr(*)
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: pc 
    Real(REAL64),    Intent(IN)    :: eeps(*)
    Integer,         Intent(IN)    :: ilend 
    Integer,         Intent(IN)    :: irend 
    Integer,         Intent(IN)    :: iopen 
    Integer,         Intent(IN)    :: itmax 
    Type(SISLcurve), Intent(INOUT) :: rc 
    Real(REAL64),    Intent(INOUT) :: emxerr(*)
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_ilend, c_irend, c_iopen, c_itmax, c_jstat

    Interface
      Subroutine c_s1963(pc, eeps, ilend, irend, iopen, itmax, rc, emxerr,     &
                         jstat) BIND(C,name="s1963")

        IMPORT :: C_PTR, C_DOUBLE, C_INT
        Implicit NONE

        Type(C_PTR),    VALUE               :: pc
        Real(C_DOUBLE),       Intent(IN)    :: eeps(*)
        Integer(C_INT), VALUE               :: ilend 
        Integer(C_INT), VALUE               :: irend 
        Integer(C_INT), VALUE               :: iopen 
        Integer(C_INT), VALUE               :: itmax 
        Type(C_PTR),          Intent(INOUT) :: rc
        Real(C_DOUBLE),       Intent(INOUT) :: emxerr(*)
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1963
    End Interface

    c_ilend = ilend 
    c_irend = irend 
    c_iopen = iopen 
    c_itmax = itmax 

    emxerr(1:pc%idim) = 0.0_REAL64
       
    Call c_s1963(pc%cptr, eeps, c_ilend, c_irend, c_iopen, c_itmax,        &
                 rc%cptr, emxerr, c_jstat)

    Call curveCtoF(rc)

    jstat = c_jstat

  End Subroutine s1963

!---------------------------------- s1965 -------------------------------------

  Subroutine s1965(oldsurf, eps, edgefix, iopen1, iopen2, edgeps, opt, itmax,  &
                   newsurf, maxerr, stat)

!! PURPOSE
!!   s1965 - To remove as many knots as possible from a spline surface without
!!           perturbing the surface more than the given tolerance. The error
!!           in continuity over the start and end of a closed or periodic surface
!!           is only guaranteed to be within edgeps.

!! INTERFACE
!!   Subroutine s1965(oldsurf, eps, edgefix, iopen1, iopen2, edgeps, opt,   &
!!                    itmax, newsurf, maxerr, stat)
!!     Type(SISLsurf),  Intent(IN)    :: oldsurf
!!     Real(REAL64),    Intent(IN)    :: eps(*)
!!     Integer,         Intent(IN)    :: edgefix(4) 
!!     Integer,         Intent(IN)    :: iopen1 
!!     Integer,         Intent(IN)    :: iopen2 
!!     Real(REAL64),    Intent(IN)    :: edgeps(*)
!!     Integer,         Intent(IN)    :: opt 
!!     Integer,         Intent(IN)    :: itmax 
!!     Type(SISLsurf),  Intent(INOUT) :: newsurf
!!     Real(REAL64),    Intent(INOUT) :: maxerr(*)
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf),  Intent(IN)    :: oldsurf
    Real(REAL64),    Intent(IN)    :: eps(*)
    Integer,         Intent(IN)    :: edgefix(4) 
    Integer,         Intent(IN)    :: iopen1 
    Integer,         Intent(IN)    :: iopen2 
    Real(REAL64),    Intent(IN)    :: edgeps(*)
    Integer,         Intent(IN)    :: opt 
    Integer,         Intent(IN)    :: itmax 
    Type(SISLsurf),  Intent(INOUT) :: newsurf
    Real(REAL64),    Intent(INOUT) :: maxerr(*)
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_iopen1, c_iopen2, c_opt, c_itmax, c_stat
    Integer(C_INT) :: c_edgefix(4)

    Interface
      Subroutine c_s1965(oldsurf, eps, edgefix, iopen1, iopen2, edgeps, opt,   &
                         itmax, newsurf, maxerr, stat)                         &
                         BIND(C,name="s1965")

        IMPORT :: C_PTR, C_DOUBLE, C_INT
        Implicit NONE

        Type(C_PTR),    VALUE               :: oldsurf
        Real(C_DOUBLE),       Intent(IN)    :: eps(*)
        Integer(C_INT),       Intent(IN)    :: edgefix(*) 
        Integer(C_INT), VALUE               :: iopen1 
        Integer(C_INT), VALUE               :: iopen2 
        Real(C_DOUBLE),       Intent(IN)    :: edgeps(*)
        Integer(C_INT), VALUE               :: opt 
        Integer(C_INT), VALUE               :: itmax 
        Type(C_PTR),          Intent(INOUT) :: newsurf
        Real(C_DOUBLE),       Intent(INOUT) :: maxerr(*)
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1965
    End Interface

    c_edgefix = edgefix
    c_iopen1  = iopen1
    c_iopen2  = iopen2
    c_opt     = opt 
    c_itmax   = itmax 

    maxerr(1:oldsurf%idim) = 0.0_REAL64       
    Call c_s1965(oldsurf%cptr, eps, c_edgefix, c_iopen1, c_iopen2, edgeps,  &
                 c_opt, c_itmax, newsurf%cptr, maxerr, c_stat)

    Call surfCtoF(newsurf)

    stat = c_stat

  End Subroutine s1965

!---------------------------------- s1966 -------------------------------------

  Subroutine s1966(ep, im1, im2, idim, ipar, epar1, epar2, eeps, nend, iopen1, &
                   iopen2, edgeps, afctol, iopt, itmax, ik1, ik2, rs, emxerr,  &
                   jstat)

!! PURPOSE
!!   s1966 - To compute a tensor-product spline-approximation of order
!!           (ik1,ik2) to the rectangular array of idim-dimensional points given
!!           by ep.

!! INTERFACE
!!   Subroutine s1966(ep, im1, im2, idim, ipar, epar1, epar2, eeps, nend,      &
!!                    iopen1, iopen2, edgeps, afctol, iopt, itmax, ik1, ik2,   &
!!                    rs, emxerr, jstat)
!!     Real(REAL64),    Intent(IN)    :: ep(*)
!!     Integer,         Intent(IN)    :: im1 
!!     Integer,         Intent(IN)    :: im2 
!!     Integer,         Intent(IN)    :: idim 
!!     Integer,         Intent(IN)    :: ipar 
!!     Real(REAL64),    Intent(IN)    :: epar1(:)
!!     Real(REAL64),    Intent(IN)    :: epar2(:)
!!     Real(REAL64),    Intent(IN)    :: eeps(*)
!!     Integer,         Intent(IN)    :: nend(4)
!!     Integer,         Intent(IN)    :: iopen1 
!!     Integer,         Intent(IN)    :: iopen2 
!!     Real(REAL64),    Intent(IN)    :: edgeps(*) 
!!     Real(REAL64),    Intent(IN)    :: afctol 
!!     Integer,         Intent(IN)    :: iopt 
!!     Integer,         Intent(IN)    :: itmax 
!!     Integer,         Intent(IN)    :: ik1 
!!     Integer,         Intent(IN)    :: ik2 
!!     Type(SISLsurf),  Intent(INOUT) :: rs 
!!     Real(REAL64),    Intent(INOUT) :: emxerr(*)
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Real(REAL64),            Intent(IN)    :: ep(*)
    Integer,                 Intent(IN)    :: im1 
    Integer,                 Intent(IN)    :: im2 
    Integer,                 Intent(IN)    :: idim 
    Integer,                 Intent(IN)    :: ipar 
    Real(REAL64),    TARGET, Intent(IN)    :: epar1(:)
    Real(REAL64),    TARGET, Intent(IN)    :: epar2(:)
    Real(REAL64),            Intent(IN)    :: eeps(*)
    Integer,                 Intent(IN)    :: nend(4)
    Integer,                 Intent(IN)    :: iopen1 
    Integer,                 Intent(IN)    :: iopen2 
    Real(REAL64),            Intent(IN)    :: edgeps(*) 
    Real(REAL64),            Intent(IN)    :: afctol 
    Integer,                 Intent(IN)    :: iopt 
    Integer,                 Intent(IN)    :: itmax 
    Integer,         Intent(IN)    :: ik1 
    Integer,         Intent(IN)    :: ik2 
    Type(SISLsurf),  Intent(INOUT) :: rs 
    Real(REAL64),    Intent(INOUT) :: emxerr(*)
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_im1, c_im2, c_idim, c_ipar, c_iopen1, c_iopen2,        &
                      c_iopt, c_itmax, c_ik1, c_ik2, c_jstat

    Integer(C_INT) :: c_nend(4)

    Type(C_PTR)    :: c_epar1_p, c_epar2_p

    Interface
      Subroutine c_s1966(ep, im1, im2, idim, ipar, epar1, epar2, eeps, nend,   &
                         iopen1, iopen2, edgeps, afctol, iopt, itmax, ik1,     &
                         ik2, rs, emxerr, jstat)                               &
                         BIND(C,name="s1966")

        IMPORT :: C_PTR, C_DOUBLE, C_INT
        Implicit NONE

        Real(C_DOUBLE),       Intent(IN)    :: ep(*)
        Integer(C_INT), VALUE               :: im1 
        Integer(C_INT), VALUE               :: im2 
        Integer(C_INT), VALUE               :: idim 
        Integer(C_INT), VALUE               :: ipar 
        Type(C_PTR),    VALUE               :: epar1
        Type(C_PTR),    VALUE               :: epar2
        Real(C_DOUBLE),       Intent(IN)    :: eeps(*)
        Integer(C_INT),       Intent(IN)    :: nend(*) 
        Integer(C_INT), VALUE               :: iopen1 
        Integer(C_INT), VALUE               :: iopen2 
        Real(C_DOUBLE),       Intent(IN)    :: edgeps(*)
        Real(C_DOUBLE), VALUE               :: afctol 
        Integer(C_INT), VALUE               :: iopt 
        Integer(C_INT), VALUE               :: itmax 
        Integer(C_INT), VALUE               :: ik1 
        Integer(C_INT), VALUE               :: ik2 
        Type(C_PTR),          Intent(INOUT) :: rs
        Real(C_DOUBLE),       Intent(INOUT) :: emxerr(*)
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1966
    End Interface

    c_im1    = im1 
    c_im2    = im2 
    c_idim   = idim
    c_ipar   = ipar 
    c_iopen1 = iopen1 
    c_iopen2 = iopen2 
    c_iopt   = iopt 
    c_itmax  = itmax 
    c_ik1    = ik1 
    c_ik2    = ik2 
    c_nend   = nend

    c_epar1_p = C_NULL_PTR
    c_epar2_p = C_NULL_PTR
    If (ipar==3) Then
      If(SIZE(epar1) > 0) c_epar1_p = C_LOC(epar1)
      If(SIZE(epar2) > 0) c_epar2_p = C_LOC(epar2)
    EndIf
  
    emxerr(1:idim) = 0.0_REAL64 
    Call c_s1966(ep, c_im1, c_im2, c_idim, c_ipar, c_epar1_p, c_epar2_p, eeps, &
                 c_nend, c_iopen1, c_iopen2, edgeps, afctol, c_iopt, c_itmax,  &
                 c_ik1, c_ik2, rs%cptr, emxerr, c_jstat)

    Call surfCtoF(rs)

    jstat = c_jstat

  End Subroutine s1966

!---------------------------------- s1967 -------------------------------------

  Subroutine s1967(ep, etang1, etang2, eder11, im1, im2, idim, ipar, epar1,    &
                   epar2, eeps, nend, iopen1, iopen2, edgeps, iopt, itmax, rs, &
                   emxerr, jstat)

!! PURPOSE
!!   s1967 - To compute a bicubic hermite spline-approximation to the position
!!           and derivative data given by ep,etang1,etang2 and eder11.

!! INTERFACE
!!   Subroutine s1967(ep, etang1, etang2, eder11, im1, im2, idim, ipar,        &
!!                    epar1, epar2, eeps, nend, iopen1, iopen2, edgeps, iopt,  &
!!                    itmax, rs, emxerr, jstat)
!!     Real(REAL64),    Intent(IN)    :: ep(*)
!!     Real(REAL64),    Intent(IN)    :: etang1(*)
!!     Real(REAL64),    Intent(IN)    :: etang2(*)
!!     Real(REAL64),    Intent(IN)    :: eder11(*)
!!     Integer,         Intent(IN)    :: im1 
!!     Integer,         Intent(IN)    :: im2 
!!     Integer,         Intent(IN)    :: idim 
!!     Integer,         Intent(IN)    :: ipar 
!!     Real(REAL64),    Intent(IN)    :: epar1(:)
!!     Real(REAL64),    Intent(IN)    :: epar2(:)
!!     Real(REAL64),    Intent(IN)    :: eeps(*)
!!     Integer,         Intent(IN)    :: nend(4)
!!     Integer,         Intent(IN)    :: iopen1 
!!     Integer,         Intent(IN)    :: iopen2 
!!     Real(REAL64),    Intent(IN)    :: edgeps(*) 
!!     Integer,         Intent(IN)    :: iopt 
!!     Integer,         Intent(IN)    :: itmax 
!!     Type(SISLsurf),  Intent(INOUT) :: rs 
!!     Real(REAL64),    Intent(INOUT) :: emxerr(*)
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Real(REAL64),            Intent(IN)    :: ep(*)
    Real(REAL64),            Intent(IN)    :: etang1(*)
    Real(REAL64),            Intent(IN)    :: etang2(*)
    Real(REAL64),            Intent(IN)    :: eder11(*)
    Integer,                 Intent(IN)    :: im1 
    Integer,                 Intent(IN)    :: im2 
    Integer,                 Intent(IN)    :: idim 
    Integer,                 Intent(IN)    :: ipar 
    Real(REAL64),    TARGET, Intent(IN)    :: epar1(:)
    Real(REAL64),    TARGET, Intent(IN)    :: epar2(:)
    Real(REAL64),            Intent(IN)    :: eeps(*)
    Integer,                 Intent(IN)    :: nend(4)
    Integer,                 Intent(IN)    :: iopen1 
    Integer,                 Intent(IN)    :: iopen2 
    Real(REAL64),            Intent(IN)    :: edgeps(*) 
    Integer,                 Intent(IN)    :: iopt 
    Integer,                 Intent(IN)    :: itmax 
    Type(SISLsurf),          Intent(INOUT) :: rs 
    Real(REAL64),            Intent(INOUT) :: emxerr(*)
    Integer,                 Intent(INOUT) :: jstat

    Integer(C_INT) :: c_im1, c_im2, c_idim, c_ipar, c_iopen1, c_iopen2,        &
                      c_iopt, c_itmax, c_jstat

    Integer(C_INT) :: c_nend(4)

    Type(C_PTR)    :: c_epar1_p, c_epar2_p

    Interface
      Subroutine c_s1967(ep, etang1, etang2, eder11, im1, im2, idim, ipar,     &
                         epar1, epar2, eeps, nend, iopen1, iopen2, edgeps,     &
                         iopt, itmax, rs, emxerr, jstat)                       &
                         BIND(C,name="s1967")

        IMPORT :: C_PTR, C_DOUBLE, C_INT
        Implicit NONE

        Real(C_DOUBLE),       Intent(IN)    :: ep(*)
        Real(C_DOUBLE),       Intent(IN)    :: etang1(*)
        Real(C_DOUBLE),       Intent(IN)    :: etang2(*)
        Real(C_DOUBLE),       Intent(IN)    :: eder11(*)
        Integer(C_INT), VALUE               :: im1 
        Integer(C_INT), VALUE               :: im2 
        Integer(C_INT), VALUE               :: idim 
        Integer(C_INT), VALUE               :: ipar 
        Type(C_PTR),    VALUE               :: epar1
        Type(C_PTR),    VALUE               :: epar2
        Real(C_DOUBLE),       Intent(IN)    :: eeps(*)
        Integer(C_INT),       Intent(IN)    :: nend(*) 
        Integer(C_INT), VALUE               :: iopen1 
        Integer(C_INT), VALUE               :: iopen2 
        Real(C_DOUBLE),       Intent(IN)    :: edgeps(*)
        Integer(C_INT), VALUE               :: iopt 
        Integer(C_INT), VALUE               :: itmax 
        Type(C_PTR),          Intent(INOUT) :: rs
        Real(C_DOUBLE),       Intent(INOUT) :: emxerr(*)
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1967
    End Interface

    c_im1    = im1 
    c_im2    = im2 
    c_idim   = idim
    c_ipar   = ipar 
    c_iopen1 = iopen1 
    c_iopen2 = iopen2 
    c_iopt   = iopt 
    c_itmax  = itmax 
    c_nend   = nend

    c_epar1_p = C_NULL_PTR
    c_epar2_p = C_NULL_PTR
    If (ipar==3) Then
      If(SIZE(epar1) > 0) c_epar1_p = C_LOC(epar1)
      If(SIZE(epar2) > 0) c_epar2_p = C_LOC(epar2)
    EndIf

    emxerr(1:idim) = 0.0_REAL64
   
    Call c_s1967(ep, etang1, etang2, eder11, c_im1, c_im2, c_idim, c_ipar,     &
                 c_epar1_p, c_epar2_p, eeps, c_nend, c_iopen1, c_iopen2,       &
                 edgeps, c_iopt, c_itmax, rs%cptr, emxerr, c_jstat)

    Call surfCtoF(rs)

    jstat = c_jstat

  End Subroutine s1967

!---------------------------------- s1968 -------------------------------------

  Subroutine s1968(ps, eeps, nend, iopen1, iopen2, edgeps, iopt, itmax, rs,    &
                   jstat)

!! PURPOSE
!!   s1968 - To compute a cubic tensor-product spline approximation to a
!!           given tensor product spline surface of arbitrary order, with er-
!!           ror less than eeps in each of the idim components. The error in
!!           continuity over the start and end of a closed or periodic surface
!!           is only guaranteed to be within edgeps.

!! INTERFACE
!!   Subroutine s1968(ps, eeps, nend, iopen1, iopen2, edgeps, iopt, itmax, rs,&
!!                    jstat)
!!     Type(SISLsurf),  Intent(IN)    :: ps
!!     Real(REAL64),    Intent(IN)    :: eeps(*)
!!     Integer,         Intent(IN)    :: nend(4) 
!!     Integer,         Intent(IN)    :: iopen1 
!!     Integer,         Intent(IN)    :: iopen2 
!!     Real(REAL64),    Intent(IN)    :: edgeps(*)
!!     Integer,         Intent(IN)    :: iopt 
!!     Integer,         Intent(IN)    :: itmax 
!!     Type(SISLsurf),  Intent(INOUT) :: rs
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLsurf),  Intent(IN)    :: ps
    Real(REAL64),    Intent(IN)    :: eeps(*)
    Integer,         Intent(IN)    :: nend(4) 
    Integer,         Intent(IN)    :: iopen1 
    Integer,         Intent(IN)    :: iopen2 
    Real(REAL64),    Intent(IN)    :: edgeps(*)
    Integer,         Intent(IN)    :: iopt 
    Integer,         Intent(IN)    :: itmax 
    Type(SISLsurf),  Intent(INOUT) :: rs
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_iopen1, c_iopen2, c_iopt, c_itmax, c_jstat
    Integer(C_INT) :: c_nend(4)

    Interface
      Subroutine c_s1968(ps, eeps, nend, iopen1, iopen2, edgeps, iopt, itmax,  &
                         rs, jstat)                                            &
                         BIND(C,name="s1968")

        IMPORT :: C_PTR, C_DOUBLE, C_INT
        Implicit NONE

        Type(C_PTR),    VALUE               :: ps
        Real(C_DOUBLE),       Intent(IN)    :: eeps(*)
        Integer(C_INT),       Intent(IN)    :: nend(*) 
        Integer(C_INT), VALUE               :: iopen1 
        Integer(C_INT), VALUE               :: iopen2 
        Real(C_DOUBLE),       Intent(IN)    :: edgeps(*)
        Integer(C_INT), VALUE               :: iopt 
        Integer(C_INT), VALUE               :: itmax 
        Type(C_PTR),          Intent(INOUT) :: rs
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1968
    End Interface

    c_nend    = nend
    c_iopen1  = iopen1
    c_iopen2  = iopen2
    c_iopt    = iopt 
    c_itmax   = itmax 
       
    Call c_s1968(ps%cptr, eeps, c_nend, c_iopen1, c_iopen2, edgeps, c_iopt, &
                 c_itmax, rs%cptr, c_jstat)

    Call surfCtoF(rs)

    jstat = c_jstat

  End Subroutine s1968

End Module Data_Reduction
