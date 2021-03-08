
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

Module Curve_Definition

!! Module Curve_Definition contains Modern Fortran C-interoperability routines
!! for the C routines described in Chapter 2 of version 4.4 of the SISL 
!! reference manual.

!! Written by: Richard Weed, Ph.D.
!! Version no. 1.0 - Feb. 2021
!!   Initial development version

!! Contact: rweedmsu@gmail.com

  USE forSISLdata, ONLY: REAL64, C_INT, C_DOUBLE, C_PTR, C_F_POINTER, c_free,  &
                         C_ASSOCIATED, SISLcurve, curveCtoF

  Implicit NONE

  PRIVATE :: REAL64, C_INT, C_DOUBLE, C_PTR, C_F_POINTER, c_free,              &
             C_ASSOCIATED, SISLcurve, curveCtoF 

Contains

!---------------------------------- s1602 -------------------------------------
 
  Subroutine s1602(startpt, endpt, order, dim, startpar, endpar, curve, stat)

!! PURPOSE
!!   s1602 - Creates a staight line represented as a B-spline curve between two
!!           points

!! INTERFACE

!!  Subroutine s1602(startpt, endpt, order, dim, startpar, endpar, curve, stat)
!!    Real(REAL64),    Intent(IN)    :: startpt(*)
!!    Real(REAL64),    Intent(IN)    :: endpt(*)
!!    Integer,         Intent(IN)    :: order 
!!    Integer,         Intent(IN)    :: dim
!!    Real(REAL64),    Intent(IN)    :: startpar 
!!    Real(REAL64),    Intent(INOUT) :: endpar
!!    Type(SISLcurve), Intent(INOUT) :: curve
!!    Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Real(REAL64),    Intent(IN)    :: startpt(*)
    Real(REAL64),    Intent(IN)    :: endpt(*)
    Integer,         Intent(IN)    :: order 
    Integer,         Intent(IN)    :: dim
    Real(REAL64),    Intent(IN)    :: startpar 
    Real(REAL64),    Intent(INOUT) :: endpar
    Type(SISLcurve), Intent(INOUT) :: curve
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_order, c_dim, c_stat

! C interface

    Interface
      Subroutine c_s1602(startpt, endpt, order, dim, startpar, endpar, curve,  &
                         stat) BIND(C,name="s1602")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Real(C_DOUBLE),       Intent(IN)    :: startpt(*)
        Real(C_DOUBLE),       Intent(IN)    :: endpt(*)
        Integer(C_INT), VALUE               :: order     
        Integer(C_INT), VALUE               :: dim     
        Real(C_DOUBLE), VALUE               :: startpar
        Real(C_DOUBLE),       Intent(INOUT) :: endpar
        Type(C_PTR),          Intent(INOUT) :: curve
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1602
    End Interface

    c_order = order
    c_dim   = dim

    Call c_s1602(startpt, endpt, c_order, c_dim, startpar, endpar,             &
                 curve%cptr, c_stat)

    stat = c_stat
    Call curveCtoF(curve)
    
  End Subroutine s1602

!---------------------------------- s1356 -------------------------------------

  Subroutine s1356(epoint, inbpnt, idim, nptyp, icnsta, icnend, iopen, ik,     &
                   astpar, cendpar, rc, gpar, jnbpar, jstat)

!! PURPOSE
!!   s1356 - Compute a curve interpolating a set of points. The points can be
!!           assigned a tangent (derivative). The parameterization of the curve
!!           will be generated and the curve can be open, closed non-periodic
!!           or periodic. If end-conditions are conflicting, the closed curve
!!           rules out other end conditions. The output will be represented as 
!!           a B-spline curve.  

!! INTERFACE
!!    Subroutine s1356(epoint, inbpnt, idim, nptyp, icnsta, icnend, iopen, ik, &
!!                     astpar, cendpar, rc, gpar, jnbpar, jstat)
!!      Real(REAL64),                   Intent(IN)    :: epoint(*)
!!      Integer,                        Intent(IN)    :: inbpnt
!!      Integer,                        Intent(IN)    :: idim
!!      Integer,                        Intent(IN)    :: nptyp(*)
!!      Integer,                        Intent(IN)    :: icnsta
!!      Integer,                        Intent(IN)    :: icnend
!!      Integer,                        Intent(IN)    :: iopen
!!      Integer,                        Intent(IN)    :: ik
!!      Real(REAL64),                   Intent(IN)    :: astpar
!!      Real(REAL64),                   Intent(INOUT) :: cendpar
!!      Type(SISLCURVE),                Intent(INOUT) :: rc
!!      Real(REAL64),    ALLOCATABLE,   Intent(INOUT) :: gpar(:)
!!      Integer,                        Intent(INOUT) :: jnbpar
!!      Integer,                        Intent(INOUT) :: jstat

    Implicit NONE

    Real(REAL64),                   Intent(IN)    :: epoint(*)
    Integer,                        Intent(IN)    :: inbpnt
    Integer,                        Intent(IN)    :: idim
    Integer,                        Intent(IN)    :: nptyp(*)
    Integer,                        Intent(IN)    :: icnsta 
    Integer,                        Intent(IN)    :: icnend 
    Integer,                        Intent(IN)    :: iopen 
    Integer,                        Intent(IN)    :: ik
    Real(REAL64),                   Intent(IN)    :: astpar
    Real(REAL64),                   Intent(INOUT) :: cendpar
    Type(SISLCURVE),                Intent(INOUT) :: rc
    Real(REAL64),    ALLOCATABLE,   Intent(INOUT) :: gpar(:)
    Integer,                        Intent(INOUT) :: jnbpar 
    Integer,                        Intent(INOUT) :: jstat
 
    Integer(C_INT) :: c_inbpnt, c_idim, c_icnsta, c_icnend, c_iopen, c_ik,     &
                      c_jnbpar, c_jstat
    Integer(C_INT)        :: c_nptyp(inbpnt)
    Type(C_PTR)           :: c_gpar_p
    Real(REAL64), Pointer :: gparp(:)

! C interface

    Interface
      Subroutine c_s1356(epoint, inbpnt, idim, nptyp, icnsta, icnend, iopen,   &
                         ik, astpar, cendpar, rc, gpar, jnbpar, jstat)         &
                         BIND(C,name="s1356")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Real(C_DOUBLE),       Intent(IN)    :: epoint(*)
        Integer(C_INT), VALUE               :: inbpnt       
        Integer(C_INT), VALUE               :: idim       
        Integer(C_INT),       Intent(IN)    :: nptyp(*)       
        Integer(C_INT), VALUE               :: icnsta       
        Integer(C_INT), VALUE               :: icnend       
        Integer(C_INT), VALUE               :: iopen       
        Integer(C_INT), VALUE               :: ik       
        Real(C_DOUBLE), VALUE               :: astpar 
        Real(C_DOUBLE),       Intent(INOUT) :: cendpar
        Type(C_PTR),          Intent(INOUT) :: rc
        Type(C_PTR),          Intent(INOUT) :: gpar 
        Integer(C_INT),       Intent(OUT)   :: jnbpar                
        Integer(C_INT),       Intent(OUT)   :: jstat

      End Subroutine c_s1356

    End Interface

    c_inbpnt = inbpnt
    c_idim   = idim
    c_icnsta = icnsta
    c_icnend = icnend
    c_iopen  = iopen
    c_ik     = ik

    c_nptyp(:) = nptyp(1:inbpnt)

    Call c_s1356(epoint, c_inbpnt, c_idim, c_nptyp, c_icnsta, c_icnend,        &
                 c_iopen, c_ik, astpar, cendpar, rc%cptr, c_gpar_p,            &
                 c_jnbpar, c_jstat)

! Unroll C pointer to gpar array returned by s1356 and copy it into
! ALLOCATABLE array. gpar allocated in C is deleted

    jnbpar = c_jnbpar
    If (C_ASSOCIATED(c_gpar_p)) Then
     Call C_F_Pointer(c_gpar_p, gparp,[jnbpar])
     If (ASSOCIATED(gparp)) Then
       If (ALLOCATED(gpar)) DEALLOCATE(gpar)
       ALLOCATE(gpar(jnbpar), SOURCE=0.0_REAL64)
       gpar(:) = gparp(:)
       NULLIFY(gparp)
       Call c_free(c_gpar_p)
     EndIf
   EndIf

    Call curveCtoF(rc)

    jstat = c_jstat

  End Subroutine s1356

!---------------------------------- s1357 -------------------------------------

  Subroutine s1357(epoint, inbpnt, idim, nptyp, epar, icnsta, icnend, iopen,   &
                   ik, astpar, cendpar, rc, gpar, jnbpar, jstat)

!! PURPOSE
!!   s1357 - Compute a curve interpolating a set of points. The points can be
!!           assigned a tangent (derivative). The curve can be open, closed
!!            or periodic. If end-conditions are conflicting, the closed
!!            curve rules out other end conditions. The parameterization is
!!            given by the array epar. The output will be represented as a
!!            B-spline curve.

!! INTERFACE
!!   Subroutine s1357(epoint, inbpnt, idim, nptyp, epar, icnsta, icnend, iopen,&
!!                    ik, astpar, cendpar, rc, gpar, jnbpar, jstat)
!!     Real(REAL64),                 Intent(IN)    :: epoint(*)
!!     Integer,                      Intent(IN)    :: inbpnt
!!     Integer,                      Intent(IN)    :: idim
!!     Integer,                      Intent(IN)    :: nptyp(*)
!!     Real(REAL64),                 Intent(IN)    :: epar(*)
!!     Integer,                      Intent(IN)    :: icnsta 
!!     Integer,                      Intent(IN)    :: icnend 
!!     Integer,                      Intent(IN)    :: iopen 
!!     Integer,                      Intent(IN)    :: ik
!!     Real(REAL64),                 Intent(IN)    :: astpar
!!     Real(REAL64),                 Intent(INOUT) :: cendpar
!!     Type(SISLCURVE),              Intent(INOUT) :: rc
!!     Real(REAL64),    ALLOCATABLE, Intent(INOUT) :: gpar(:)
!!     Integer,                      Intent(INOUT) :: jnbpar 
!!     Integer,                      Intent(INOUT) :: jstat
!!     Implicit NONE

    Real(REAL64),                 Intent(IN)    :: epoint(*)
    Integer,                      Intent(IN)    :: inbpnt
    Integer,                      Intent(IN)    :: idim
    Integer,                      Intent(IN)    :: nptyp(*)
    Real(REAL64),                 Intent(IN)    :: epar(*)
    Integer,                      Intent(IN)    :: icnsta 
    Integer,                      Intent(IN)    :: icnend 
    Integer,                      Intent(IN)    :: iopen 
    Integer,                      Intent(IN)    :: ik
    Real(REAL64),                 Intent(IN)    :: astpar
    Real(REAL64),                 Intent(INOUT) :: cendpar
    Type(SISLCURVE),              Intent(INOUT) :: rc
    Real(REAL64),    ALLOCATABLE, Intent(INOUT) :: gpar(:)
    Integer,                      Intent(INOUT) :: jnbpar 
    Integer,                      Intent(INOUT) :: jstat
 
    Integer(C_INT) :: c_inbpnt, c_idim, c_icnsta, c_icnend, c_iopen, c_ik,     &
                      c_jnbpar, c_jstat
    Integer(C_INT) :: c_nptyp(inbpnt)
    Type(C_PTR)           :: c_gpar_p
    Real(REAL64), Pointer :: gparp(:)

! C interface 

    Interface
      Subroutine c_s1357(epoint, inbpnt, idim, nptyp, epar, icnsta, icnend,    &
                         iopen, ik, astpar, cendpar, rc, gpar, jnbpar, jstat)  &
                         BIND(C,name="s1357")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Real(C_DOUBLE),       Intent(IN)    :: epoint(*)
        Integer(C_INT), VALUE               :: inbpnt       
        Integer(C_INT), VALUE               :: idim       
        Integer(C_INT),       Intent(IN)    :: nptyp(*)       
        Real(C_DOUBLE),       Intent(IN)    :: epar(*)
        Integer(C_INT), VALUE               :: icnsta       
        Integer(C_INT), VALUE               :: icnend       
        Integer(C_INT), VALUE               :: iopen       
        Integer(C_INT), VALUE               :: ik       
        Real(C_DOUBLE), VALUE               :: astpar 
        Real(C_DOUBLE),       Intent(INOUT) :: cendpar
        Type(C_PTR),          Intent(INOUT) :: rc
        Type(C_PTR),          Intent(INOUT) :: gpar
        Integer(C_INT),       Intent(INOUT) :: jnbpar                
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1357

    End Interface

    c_inbpnt = inbpnt
    c_idim   = idim
    c_icnsta = icnsta
    c_icnend = icnend
    c_iopen  = iopen
    c_ik     = ik

    c_nptyp(:) = nptyp(1:inbpnt)

    Call c_s1357(epoint, c_inbpnt, c_idim, c_nptyp, epar, c_icnsta, c_icnend,  &
                 c_iopen, c_ik, astpar, cendpar, rc%cptr, c_gpar_p, c_jnbpar,  &
                 c_jstat)

! Unroll C pointer to gpar array returned by s1357 and copy it into
! ALLOCATABLE array. gpar allocated in C is deleted

    jnbpar = c_jnbpar

    If (C_ASSOCIATED(c_gpar_p)) Then
     Call C_F_Pointer(c_gpar_p, gparp,[jnbpar])
     If (ASSOCIATED(gparp)) Then
       If (ALLOCATED(gpar)) DEALLOCATE(gpar)
       ALLOCATE(gpar(jnbpar), SOURCE=0.0_REAL64)
       gpar(:) = gparp(:)
       NULLIFY(gparp)
       Call c_free(c_gpar_p)
     EndIf
   EndIf

    Call curveCtoF(rc)
    jstat = c_jstat

  End Subroutine s1357

!---------------------------------- s1380 -------------------------------------

  Subroutine s1380(point, derivate, numpt, dim, typepar, curve, stat)

!! PURPOSE
!!   s1380 - Computes the cubic Hermite interpolant to the data given by the
!!   points point and the derivatives derivate. The output is represented as
!!   a B-spline curve.

!! INTERFACE
!!   Subroutine s1380(point, derivate, numpt, dim, typepar, curve, stat)
!!     Real(REAL64),    Intent(IN)    :: point(*)
!!     Real(REAL64),    Intent(IN)    :: derivate(*)
!!     Integer,         Intent(IN)    :: numpt 
!!     Integer,         Intent(IN)    :: dim
!!     Integer,         Intent(IN)    :: typepar 
!!     Type(SISLcurve), Intent(INOUT) :: curve
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Real(REAL64),    Intent(IN)    :: point(*)
    Real(REAL64),    Intent(IN)    :: derivate(*)
    Integer,         Intent(IN)    :: numpt 
    Integer,         Intent(IN)    :: dim
    Integer,         Intent(IN)    :: typepar
    Type(SISLcurve), Intent(INOUT) :: curve
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_numpt, c_dim, c_typepar, c_stat

! C interface

    Interface
      Subroutine c_s1380(point, derivate, numpt, dim, typepar, curve, stat)    &
                         BIND(C,name="s1380")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Real(C_DOUBLE),       Intent(IN)    :: point(*)
        Real(C_DOUBLE),       Intent(IN)    :: derivate(*)
        Integer(C_INT), VALUE               :: numpt     
        Integer(C_INT), VALUE               :: dim     
        Integer(C_INT), VALUE               :: typepar
        Type(C_PTR),          Intent(INOUT) :: curve
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1380
    End Interface

    c_numpt   = numpt 
    c_dim     = dim
    c_typepar = typepar

    Call c_s1380(point, derivate, c_numpt, c_dim, c_typepar, curve%cptr,       &
                 c_stat)

    stat = c_stat
    Call curveCtoF(curve)
    
  End Subroutine s1380

!---------------------------------- s1379 -------------------------------------

  Subroutine s1379(point, derivate, par, numpt, dim, curve, stat)

!! PURPOSE
!!   s1379 - Computes the cubic Hermite interpolant to the data given by the
!!           points point and the derivatives derivate and the parameterization
!!           par. The output is represented as a B-spline curve.

!! INTERFACE
!!   Subroutine s1379(point, derivate, par, numpt, dim, curve, stat)
!!     Real(REAL64),    Intent(IN)    :: point(*)
!!     Real(REAL64),    Intent(IN)    :: derivate(*)
!!     Real(REAL64),    Intent(IN)    :: par(*)
!!     Integer,         Intent(IN)    :: numpt 
!!     Integer,         Intent(IN)    :: dim
!!     Type(SISLcurve), Intent(INOUT) :: curve
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Real(REAL64),    Intent(IN)    :: point(*)
    Real(REAL64),    Intent(IN)    :: derivate(*)
    Real(REAL64),    Intent(IN)    :: par(*)
    Integer,         Intent(IN)    :: numpt 
    Integer,         Intent(IN)    :: dim
    Type(SISLcurve), Intent(INOUT) :: curve
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_numpt, c_dim, c_stat

! C interface

    Interface
      Subroutine c_s1379(point, derivate, par, numpt, dim, curve, stat)    &
                         BIND(C,name="s1379")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Real(C_DOUBLE),       Intent(IN)    :: point(*)
        Real(C_DOUBLE),       Intent(IN)    :: derivate(*)
        Real(C_DOUBLE),       Intent(IN)    :: par(*)
        Integer(C_INT), VALUE               :: numpt     
        Integer(C_INT), VALUE               :: dim     
        Type(C_PTR),          Intent(INOUT) :: curve
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1379
    End Interface

    c_numpt = numpt 
    c_dim   = dim

    Call c_s1379(point, derivate, par, c_numpt, c_dim, curve%cptr, c_stat)

    stat = c_stat
    Call curveCtoF(curve)
    
  End Subroutine s1379

!---------------------------------- s1607 -------------------------------------

  Subroutine s1607(curve1, curve2, epsge, end1, fillpar1, end2, fillpar2,      &
                   filltype, dim, order, newcurve, stat)

!! PURPOSE
!!   s1607 - Calculates a fillet curve between two curves. The start and end
!!           point for the fillet is given as one parameter value for each of 
!!           the curves. The output is represented as a B-spline curve.

!! INTERFACE
!!   Subroutine s1607(curve1, curve2, epsge, end1, fillpar1, end2, fillpar2,   &
!!                    filltype, dim, order, newcurve, stat)
!!     Type(SISLcurve), Intent(IN)    :: curve1 
!!     Type(SISLcurve), Intent(IN)    :: curve2 
!!     Real(REAL64),    Intent(IN)    :: epsge 
!!     Real(REAL64),    Intent(IN)    :: end1 
!!     Real(REAL64),    Intent(IN)    :: fillpar1 
!!     Real(REAL64),    Intent(IN)    :: end2 
!!     Real(REAL64),    Intent(IN)    :: fillpar2 
!!     Integer,         Intent(IN)    :: filltype 
!!     Integer,         Intent(IN)    :: dim
!!     Integer,         Intent(IN)    :: order 
!!     Type(SISLcurve), Intent(INOUT) :: newcurve
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: curve1 
    Type(SISLcurve), Intent(IN)    :: curve2 
    Real(REAL64),    Intent(IN)    :: epsge 
    Real(REAL64),    Intent(IN)    :: end1 
    Real(REAL64),    Intent(IN)    :: fillpar1 
    Real(REAL64),    Intent(IN)    :: end2 
    Real(REAL64),    Intent(IN)    :: fillpar2 
    Integer,         Intent(IN)    :: filltype 
    Integer,         Intent(IN)    :: dim
    Integer,         Intent(IN)    :: order 
    Type(SISLcurve), Intent(INOUT) :: newcurve
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_filltype, c_dim, c_order, c_stat

! C interface

    Interface
      Subroutine c_s1607(curve1, curve2, epsge, end1, fillpar1, end2,          &
                         fillpar2, filltype, dim, order, newcurve, stat)       &
                         BIND(C,name="s1607")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: curve1 
        Type(C_PTR),    VALUE               :: curve2 
        Real(C_DOUBLE), VALUE               :: epsge 
        Real(C_DOUBLE), VALUE               :: end1 
        Real(C_DOUBLE), VALUE               :: fillpar1 
        Real(C_DOUBLE), VALUE               :: end2 
        Real(C_DOUBLE), VALUE               :: fillpar2 
        Integer(C_INT), VALUE               :: filltype 
        Integer(C_INT), VALUE               :: dim
        Integer(C_INT), VALUE               :: order 
        Type(C_PTR),          Intent(INOUT) :: newcurve
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1607
    End Interface

    c_filltype = filltype
    c_dim      = dim
    c_order    = order

    Call c_s1607(curve1%cptr, curve2%cptr, epsge, end1, fillpar1,              &
                 end2, fillpar2, c_filltype, c_dim, c_order, newcurve%cptr,    &
                 c_stat)

    stat = c_stat
    Call curveCtoF(newcurve)
    
  End Subroutine s1607

!---------------------------------- s1608 -------------------------------------

  Subroutine s1608(curve1, curve2, epsge, point1, startpt1, point2, endpt2,    &
                   filltype, dim, order, newcurve, parpt1, parspt1, parpt2,    &
                   parept2, stat)

!! PURPOSE
!!   s1608 - Calculates a fillet curve between two curves. Points indicate be-
!!           tween which points on the input curve the fillet is to be produced.
!!           The output is represented as a B-spline curve.

!! INTERFACE
!!   Subroutine s1608(curve1, curve2, epsge, point1, startpt1, point2, endpt2, &
!!                    filltype, dim, order, newcurve, parpt1, parspt1, parpt2, &
!!                    parept2, stat)
!!     Type(SISLcurve), Intent(IN)    :: curve1 
!!     Type(SISLcurve), Intent(IN)    :: curve2 
!!     Real(REAL64),    Intent(IN)    :: epsge
!!     Real(REAL64),    Intent(IN)    :: point1(*) 
!!     Real(REAL64),    Intent(IN)    :: startpt1(*)
!!     Real(REAL64),    Intent(IN)    :: point2(*) 
!!     Real(REAL64),    Intent(IN)    :: endpt2(*) 
!!     Integer,         Intent(IN)    :: filltype 
!!     Integer,         Intent(IN)    :: dim
!!     Integer,         Intent(IN)    :: order 
!!     Type(SISLcurve), Intent(INOUT) :: newcurve
!!     Real(REAL64),    Intent(INOUT) :: parpt1
!!     Real(REAL64),    Intent(INOUT) :: parspt1
!!     Real(REAL64),    Intent(INOUT) :: parpt2
!!     Real(REAL64),    Intent(INOUT) :: parept2
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: curve1 
    Type(SISLcurve), Intent(IN)    :: curve2 
    Real(REAL64),    Intent(IN)    :: epsge
    Real(REAL64),    Intent(IN)    :: point1(*) 
    Real(REAL64),    Intent(IN)    :: startpt1(*)
    Real(REAL64),    Intent(IN)    :: point2(*) 
    Real(REAL64),    Intent(IN)    :: endpt2(*) 
    Integer,         Intent(IN)    :: filltype 
    Integer,         Intent(IN)    :: dim
    Integer,         Intent(IN)    :: order 
    Type(SISLcurve), Intent(INOUT) :: newcurve
    Real(REAL64),    Intent(INOUT) :: parpt1
    Real(REAL64),    Intent(INOUT) :: parspt1
    Real(REAL64),    Intent(INOUT) :: parpt2
    Real(REAL64),    Intent(INOUT) :: parept2
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_filltype, c_dim, c_order, c_stat

! C interface

    Interface
      Subroutine c_s1608(curve1, curve2, epsge, point1, startpt1, point2,      &
                         endpt2, filltype, dim, order,  newcurve, parpt1,      &
                         parspt1, parpt2, parept2, stat)                       &
                         BIND(C,name="s1608")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: curve1 
        Type(C_PTR),    VALUE               :: curve2 
        Real(C_DOUBLE), VALUE               :: epsge 
        Real(C_DOUBLE),       Intent(IN)    :: point1(*) 
        Real(C_DOUBLE),       Intent(IN)    :: startpt1(*) 
        Real(C_DOUBLE),       Intent(IN)    :: point2(*) 
        Real(C_DOUBLE),       Intent(IN)    :: endpt2(*) 
        Integer(C_INT), VALUE               :: filltype 
        Integer(C_INT), VALUE               :: dim
        Integer(C_INT), VALUE               :: order 
        Type(C_PTR),          Intent(INOUT) :: newcurve
        Real(C_DOUBLE),       Intent(INOUT) :: parpt1
        Real(C_DOUBLE),       Intent(INOUT) :: parspt1
        Real(C_DOUBLE),       Intent(INOUT) :: parpt2
        Real(C_DOUBLE),       Intent(INOUT) :: parept2
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1608
    End Interface

    c_filltype = filltype
    c_dim      = dim
    c_order    = order

    Call c_s1608(curve1%cptr, curve2%cptr, epsge, point1, startpt1,            &
                 point2, endpt2, c_filltype, c_dim, c_order, newcurve%cptr,    &
                 parpt1, parspt1, parpt2, parept2, c_stat)

    stat = c_stat
    Call curveCtoF(newcurve)
    
  End Subroutine s1608

!---------------------------------- s1609 -------------------------------------

  Subroutine s1609(curve1, curve2, epsge, point1, pointf, point2,  radius,     &
                   normal, filltype, dim, order, newcurve, parend1, parspt1,   &
                   parend2, parept2, stat)
!! PURPOSE
!!   s1609 - Calculates a constant radius fillet curve between two curves if
!!           possible. The output is represented as a B-spline curve.

!! INTERFACE
!!   Subroutine s1609(curve1, curve2, epsge, point1, pointf, point2,  radius,  &
!!                   normal, filltype, dim, order, newcurve, parend1, parspt1, &
!!                   parend2, parept2, stat)
!!     Type(SISLcurve), Intent(IN)    :: curve1 
!!     Type(SISLcurve), Intent(IN)    :: curve2 
!!     Real(REAL64),    Intent(IN)    :: epsge 
!!     Real(REAL64),    Intent(IN)    :: point1(*) 
!!     Real(REAL64),    Intent(IN)    :: pointf(*) 
!!     Real(REAL64),    Intent(IN)    :: point2(*) 
!!     Real(REAL64),    Intent(IN)    :: radius 
!!     Real(REAL64),    Intent(IN)    :: normal(*) 
!!     Integer,         Intent(IN)    :: filltype 
!!     Integer,         Intent(IN)    :: dim
!!     Integer,         Intent(IN)    :: order 
!!     Type(SISLcurve), Intent(INOUT) :: newcurve
!!     Real(REAL64),    Intent(INOUT) :: parend1
!!     Real(REAL64),    Intent(INOUT) :: parspt1
!!     Real(REAL64),    Intent(INOUT) :: parend2
!!     Real(REAL64),    Intent(INOUT) :: parept2
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: curve1 
    Type(SISLcurve), Intent(IN)    :: curve2 
    Real(REAL64),    Intent(IN)    :: epsge 
    Real(REAL64),    Intent(IN)    :: point1(*) 
    Real(REAL64),    Intent(IN)    :: pointf(*) 
    Real(REAL64),    Intent(IN)    :: point2(*) 
    Real(REAL64),    Intent(IN)    :: radius 
    Real(REAL64),    Intent(IN)    :: normal(*) 
    Integer,         Intent(IN)    :: filltype 
    Integer,         Intent(IN)    :: dim
    Integer,         Intent(IN)    :: order 
    Type(SISLcurve), Intent(INOUT) :: newcurve
    Real(REAL64),    Intent(INOUT) :: parend1
    Real(REAL64),    Intent(INOUT) :: parspt1
    Real(REAL64),    Intent(INOUT) :: parend2
    Real(REAL64),    Intent(INOUT) :: parept2
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_filltype, c_dim, c_order, c_stat

    Interface
      Subroutine c_s1609(curve1, curve2, epsge, point1, pointf, point2,      &
                         radius, normal, filltype, dim, order,  newcurve,    & 
                         parend1, parspt1, parend2, parept2, stat)           &
                         BIND(C,name="s1609")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: curve1 
        Type(C_PTR),    VALUE               :: curve2 
        Real(C_DOUBLE), VALUE               :: epsge 
        Real(C_DOUBLE),       Intent(IN)    :: point1(*) 
        Real(C_DOUBLE),       Intent(IN)    :: pointf(*) 
        Real(C_DOUBLE),       Intent(IN)    :: point2(*) 
        Real(C_DOUBLE),       Intent(IN)    :: radius 
        Real(C_DOUBLE),       Intent(IN)    :: normal(*) 
        Integer(C_INT), VALUE               :: filltype 
        Integer(C_INT), VALUE               :: dim
        Integer(C_INT), VALUE               :: order 
        Type(C_PTR),          Intent(INOUT) :: newcurve
        Real(C_DOUBLE),       Intent(INOUT) :: parend1
        Real(C_DOUBLE),       Intent(INOUT) :: parspt1
        Real(C_DOUBLE),       Intent(INOUT) :: parend2
        Real(C_DOUBLE),       Intent(INOUT) :: parept2
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1609
    End Interface

    c_filltype = filltype
    c_dim      = dim
    c_order    = order

    Call c_s1609(curve1%cptr, curve2%cptr, epsge, point1, pointf,              &
                 point2, radius, normal, c_filltype, c_dim, c_order,           &
                 newcurve%cptr, parend1, parspt1, parend2, parept2, c_stat)

    Call curveCtoF(newcurve)
    stat = c_stat
    
  End Subroutine s1609

!---------------------------------- s1014 -------------------------------------

  Subroutine s1014(pc1, circ_cen, circ_rad, aepsge, eps1, eps2, aradius,       &
                   parpt1, parpt2, centre, jstat) 

!! PURPOSE
!!   s1014 - Computes a circular fillet by iterating to the start and end points
!!           of a fillet between a 2D curve and a circle. The centre of the
!!           circular fillet is also calculated

!! INTERFACE
!!   Subroutine s1014(pc1, circ_cen, circ_rad, aepsge, eps1, eps2, aradius,    &
!!                     parpt1, parpt2, centre, jstat) 
!!     Type(SISLcurve), Intent(IN)    :: pc1 
!!     Real(REAL64),    Intent(IN)    :: circ_cen(*) 
!!     Real(REAL64),    Intent(IN)    :: circ_rad 
!!     Real(REAL64),    Intent(IN)    :: aepsge 
!!     Real(REAL64),    Intent(IN)    :: eps1(*) 
!!     Real(REAL64),    Intent(IN)    :: eps2(*) 
!!     Real(REAL64),    Intent(IN)    :: aradius 
!!     Real(REAL64),    Intent(INOUT) :: parpt1
!!     Real(REAL64),    Intent(INOUT) :: parpt2
!!     Real(REAL64),    Intent(INOUT) :: centre(*) 
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: pc1 
    Real(REAL64),    Intent(IN)    :: circ_cen(*) 
    Real(REAL64),    Intent(IN)    :: circ_rad 
    Real(REAL64),    Intent(IN)    :: aepsge 
    Real(REAL64),    Intent(IN)    :: eps1(*) 
    Real(REAL64),    Intent(IN)    :: eps2(*) 
    Real(REAL64),    Intent(IN)    :: aradius 
    Real(REAL64),    Intent(INOUT) :: parpt1
    Real(REAL64),    Intent(INOUT) :: parpt2
    Real(REAL64),    Intent(INOUT) :: centre(*) 
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_jstat

    Interface
      Subroutine c_s1014(pc1, circ_cen,circ_rad, aepsge, eps1, eps2, aradius,  &
                         parpt1, parpt2, centre, jstat)                        &
                         BIND(C,name="s1014")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: pc1 
        Real(C_DOUBLE),       Intent(IN)    :: circ_cen(*) 
        Real(C_DOUBLE), VALUE               :: circ_rad 
        Real(C_DOUBLE), VALUE               :: aepsge 
        Real(C_DOUBLE),       Intent(IN)    :: eps1(*) 
        Real(C_DOUBLE),       Intent(IN)    :: eps2(*) 
        Real(C_DOUBLE), VALUE               :: aradius
        Real(C_DOUBLE),       Intent(INOUT) :: parpt1
        Real(C_DOUBLE),       Intent(INOUT) :: parpt2
        Real(C_DOUBLE),       Intent(INOUT) :: centre(*) 
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1014
    End Interface

    centre(1:2) = 0.0_REAL64
    Call c_s1014(pc1%cptr, circ_cen, circ_rad, aepsge, eps1, eps2, aradius,    &
                 parpt1, parpt2, centre, c_jstat)

    jstat = c_jstat

  End Subroutine s1014

!---------------------------------- s1015 -------------------------------------

  Subroutine s1015(pc1, pc2,  aepsge, eps1, eps2, aradius, parpt1, parpt2,     &
                   centre, jstat) 

!! PURPOSE
!!   s1015 - Computes a fillet by iterating to the start and end points of a
!!           fillet between two 2D curves. The centre of the circular fillet is
!!           also calculated.

!! INTERFACE
!!   Subroutine s1015(pc1, pc2,  aepsge, eps1, eps2, aradius, parpt1, parpt2, &
!!                    centre, jstat) 
!!     Type(SISLcurve), Intent(IN)    :: pc1 
!!     Type(SISLcurve), Intent(IN)    :: pc2 
!!     Real(REAL64),    Intent(IN)    :: aepsge 
!!     Real(REAL64),    Intent(IN)    :: eps1(*) 
!!     Real(REAL64),    Intent(IN)    :: eps2(*) 
!!     Real(REAL64),    Intent(IN)    :: aradius 
!!     Real(REAL64),    Intent(INOUT) :: parpt1
!!     Real(REAL64),    Intent(INOUT) :: parpt2
!!     Real(REAL64),    Intent(INOUT) :: centre(*) 
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: pc1 
    Type(SISLcurve), Intent(IN)    :: pc2 
    Real(REAL64),    Intent(IN)    :: aepsge 
    Real(REAL64),    Intent(IN)    :: eps1(*) 
    Real(REAL64),    Intent(IN)    :: eps2(*) 
    Real(REAL64),    Intent(IN)    :: aradius 
    Real(REAL64),    Intent(INOUT) :: parpt1
    Real(REAL64),    Intent(INOUT) :: parpt2
    Real(REAL64),    Intent(INOUT) :: centre(*) 
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_jstat

    Interface
      Subroutine c_s1015(pc1, pc2, aepsge, eps1, eps2, aradius, parpt1,        &
                         parpt2, centre, jstat)                                &
                         BIND(C,name="s1015")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: pc1 
        Type(C_PTR),    VALUE               :: pc2 
        Real(C_DOUBLE), VALUE               :: aepsge 
        Real(C_DOUBLE),       Intent(IN)    :: eps1(*) 
        Real(C_DOUBLE),       Intent(IN)    :: eps2(*) 
        Real(C_DOUBLE), VALUE               :: aradius
        Real(C_DOUBLE),       Intent(INOUT) :: parpt1
        Real(C_DOUBLE),       Intent(INOUT) :: parpt2
        Real(C_DOUBLE),       Intent(INOUT) :: centre(*) 
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1015
    End Interface

    centre(1:2) = 0.0_REAL64
    Call c_s1015(pc1%cptr, pc2%cptr, aepsge, eps1, eps2, aradius, parpt1,      &
                 parpt2, centre, c_jstat)

    jstat = c_jstat

  End Subroutine s1015

!---------------------------------- s1016 -------------------------------------

  Subroutine s1016(pc1, point, normal, aepsge, eps1, eps2, aradius, parpt1,   &
                   parpt2, centre, jstat) 

!! PURPOSE
!!   s1016 - Compute the fillet by iterating to the start and end points of a
!!           fillet between a 2D curve and a 2D line. The centre of the circular
!!           fillet is also calculated

!! INTERFACE
!!   Subroutine s1016(pc1, point, normal,  aepsge, eps1, eps2, aradius, parpt1,&
!!                    parpt2, centre, jstat)
!!     Type(SISLcurve), Intent(IN)    :: pc1 
!!     Real(REAL64),    Intent(IN)    :: point(*) 
!!     Real(REAL64),    Intent(IN)    :: normal(*) 
!!     Real(REAL64),    Intent(IN)    :: aepsge 
!!     Real(REAL64),    Intent(IN)    :: eps1(*) 
!!     Real(REAL64),    Intent(IN)    :: eps2(*) 
!!     Real(REAL64),    Intent(IN)    :: aradius 
!!     Real(REAL64),    Intent(INOUT) :: parpt1
!!     Real(REAL64),    Intent(INOUT) :: parpt2
!!     Real(REAL64),    Intent(INOUT) :: centre(*) 
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: pc1 
    Real(REAL64),    Intent(IN)    :: point(*) 
    Real(REAL64),    Intent(IN)    :: normal(*) 
    Real(REAL64),    Intent(IN)    :: aepsge 
    Real(REAL64),    Intent(IN)    :: eps1(*) 
    Real(REAL64),    Intent(IN)    :: eps2(*) 
    Real(REAL64),    Intent(IN)    :: aradius 
    Real(REAL64),    Intent(INOUT) :: parpt1
    Real(REAL64),    Intent(INOUT) :: parpt2
    Real(REAL64),    Intent(INOUT) :: centre(*) 
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_jstat

    Interface
      Subroutine c_s1016(pc1, point, normal, aepsge, eps1, eps2, aradius,      &
                         parpt1, parpt2, centre, jstat)                        &
                         BIND(C,name="s1016")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: pc1 
        Real(C_DOUBLE),       Intent(IN)    :: point(*) 
        Real(C_DOUBLE),       Intent(IN)    :: normal(*) 
        Real(C_DOUBLE), VALUE               :: aepsge 
        Real(C_DOUBLE),       Intent(IN)    :: eps1(*) 
        Real(C_DOUBLE),       Intent(IN)    :: eps2(*) 
        Real(C_DOUBLE), VALUE               :: aradius
        Real(C_DOUBLE),       Intent(INOUT) :: parpt1
        Real(C_DOUBLE),       Intent(INOUT) :: parpt2
        Real(C_DOUBLE),       Intent(INOUT) :: centre(*) 
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1016
    End Interface

    centre(1:2) = 0.0_REAL64
    Call c_s1016(pc1%cptr, point, normal, aepsge, eps1, eps2, aradius,     &
                 parpt1, parpt2, centre, c_jstat)

    jstat = c_jstat

  End Subroutine s1016

!---------------------------------- s1606 -------------------------------------

  Subroutine s1606(curve1, curve2, epsge, point1, point2, blendtype, dim,      &
                   order, newcurve, stat)

!! PURPOSE
!!   s1606 - Computes a blending curve between two curves. Two points
!!           indicate between which ends the blend is to be produced. The
!!           blending curve is either a circle or an approximated conic section
!!           if this is possible, otherwise it is a quadratic polynomial spline
!!           curve. The output is represented as a B-spline curve.

!! INTERFACE
!!   Subroutine s1606(curve1, curve2, epsge, point1, point2, blendtype, dim,   &
!!                    order, newcurve, stat)
!!     Type(SISLcurve), Intent(IN)    :: curve1 
!!     Type(SISLcurve), Intent(IN)    :: curve2 
!!     Real(REAL64),    Intent(IN)    :: epsge 
!!     Real(REAL64),    Intent(IN)    :: point1(*) 
!!     Real(REAL64),    Intent(IN)    :: point2(*) 
!!     Integer,         Intent(IN)    :: blendtype 
!!     Integer,         Intent(IN)    :: dim
!!     Integer,         Intent(IN)    :: order 
!!     Type(SISLcurve), Intent(INOUT) :: newcurve
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: curve1 
    Type(SISLcurve), Intent(IN)    :: curve2 
    Real(REAL64),    Intent(IN)    :: epsge 
    Real(REAL64),    Intent(IN)    :: point1(*) 
    Real(REAL64),    Intent(IN)    :: point2(*) 
    Integer,         Intent(IN)    :: blendtype 
    Integer,         Intent(IN)    :: dim
    Integer,         Intent(IN)    :: order 
    Type(SISLcurve), Intent(INOUT) :: newcurve
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_blendtype, c_dim, c_order, c_stat

    Interface
      Subroutine c_s1606(curve1, curve2, epsge, point1, point2, blendtype,     &
                         dim, order, newcurve, stat)                           &
                         BIND(C,name="s1606")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: curve1 
        Type(C_PTR),    VALUE               :: curve2 
        Real(C_DOUBLE), VALUE               :: epsge 
        Real(C_DOUBLE),       Intent(IN)    :: point1(*) 
        Real(C_DOUBLE),       Intent(IN)    :: point2(*) 
        Integer(C_INT), VALUE               :: blendtype 
        Integer(C_INT), VALUE               :: dim
        Integer(C_INT), VALUE               :: order 
        Type(C_PTR),          Intent(INOUT) :: newcurve
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1606
    End Interface

    c_blendtype = blendtype
    c_dim       = dim
    c_order     = order

    Call c_s1606(curve1%cptr, curve2%cptr, epsge, point1, point2,              &
                 c_blendtype, c_dim, c_order, newcurve%cptr, c_stat)

    stat = c_stat
    Call curveCtoF(newcurve)
    
  End Subroutine s1606

!---------------------------------- s1303 -------------------------------------

  Subroutine s1303(startpt, epsge, angle, centrept, axis, dim, curve, stat)

!! PURPOSE
!!   s1303 - To create a curve approximating a circular arc around the axis
!!           defined by the centre point, an axis vector, a start point and a
!!           rotational angle. The maximal deviation between the true circular
!!           arc and the approximation to the arc is controlled by the geometric
!!           tolerance (epsge). The output will be represented as a B-spline
!!           curve.

!! INTERFACE
!!   Subroutine s1303(startpt, epsge, angle, centrept, axis, dim, curve, stat)
!!     Real(REAL64),    Intent(IN)    :: startpt(*)
!!     Real(REAL64),    Intent(IN)    :: epsge
!!     Real(REAL64),    Intent(IN)    :: angle 
!!     Real(REAL64),    Intent(IN)    :: centrept(*)
!!     Real(REAL64),    Intent(IN)    :: axis(*)
!!     Integer,         Intent(IN)    :: dim
!!     Type(SISLcurve), Intent(INOUT) :: curve
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Real(REAL64),    Intent(IN)    :: startpt(*)
    Real(REAL64),    Intent(IN)    :: epsge
    Real(REAL64),    Intent(IN)    :: angle 
    Real(REAL64),    Intent(IN)    :: centrept(*)
    Real(REAL64),    Intent(IN)    :: axis(*)
    Integer,         Intent(IN)    :: dim
    Type(SISLcurve), Intent(INOUT) :: curve
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_dim, c_stat

    Interface
      Subroutine c_s1303(startpt, epsge, angle, centrept, axis, dim, curve,    &
                         stat) BIND(C,name="s1303")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Real(C_DOUBLE),       Intent(IN)    :: startpt(*)
        Real(C_DOUBLE), VALUE               :: epsge
        Real(C_DOUBLE), VALUE               :: angle
        Real(C_DOUBLE),       Intent(IN)    :: centrept(*)
        Real(C_DOUBLE),       Intent(IN)    :: axis(*)
        Integer(C_INT), VALUE               :: dim     
        Type(C_PTR),          Intent(INOUT) :: curve
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1303
    End Interface

    c_dim   = dim

    Call c_s1303(startpt, epsge, angle, centrept, axis, c_dim,                 &
                 curve%cptr, c_stat)

    stat = c_stat
    Call curveCtoF(curve)
    
  End Subroutine s1303

!---------------------------------- s1611 -------------------------------------

  Subroutine s1611(point, numpt, dim, typept, iopen, order, startpar, epsge,   &
                   endpar, curve, stat) 

!! PURPOSE
!!  s1611 - approximate a conic arc with a curve in two or three dimen-
!!          sional space. If two points are given, a straight line is produced,
!!          if three an approximation of a circular arc, and if four or five a
!!          conic arc. The output will be represented as a B-spline curve.

!! INTERFACE
!!   Subroutine s1611(point, numpt, dim, typept, iopen, order, startpar, epsge,&
!!                   endpar, curve, stat) 
!!     Real(REAL64),    Intent(IN)    :: point(*)
!!     Integer,         Intent(IN)    :: numpt 
!!     Integer,         Intent(IN)    :: dim
!!     Real(REAL64),    Intent(IN)    :: typept(*)
!!     Integer,         Intent(IN)    :: iopen 
!!     Integer,         Intent(IN)    :: order 
!!     Real(REAL64),    Intent(IN)    :: startpar 
!!     Real(REAL64),    Intent(IN)    :: epsge
!!     Real(REAL64),    Intent(INOUT) :: endpar 
!!     Type(SISLcurve), Intent(INOUT) :: curve
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Real(REAL64),    Intent(IN)    :: point(*)
    Integer,         Intent(IN)    :: numpt 
    Integer,         Intent(IN)    :: dim
    Real(REAL64),    Intent(IN)    :: typept(*)
    Integer,         Intent(IN)    :: iopen 
    Integer,         Intent(IN)    :: order 
    Real(REAL64),    Intent(IN)    :: startpar 
    Real(REAL64),    Intent(IN)    :: epsge
    Real(REAL64),    Intent(INOUT) :: endpar 
    Type(SISLcurve), Intent(INOUT) :: curve
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_numpt, c_dim, c_open, c_order, c_stat

    Interface
      Subroutine c_s1611(point, numpt, dim , typept, iopen, order, startpar,   &
                         epsge, endpar, curve, stat)                           &
                         BIND(C,name="s1611")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Real(C_DOUBLE),       Intent(IN)    :: point(*)
        Integer(C_INT), VALUE               :: numpt     
        Integer(C_INT), VALUE               :: dim     
        Real(C_DOUBLE),       Intent(IN)    :: typept(*)
        Integer(C_INT), VALUE               :: iopen     
        Integer(C_INT), VALUE               :: order     
        Real(C_DOUBLE), VALUE               :: startpar 
        Real(C_DOUBLE), VALUE               :: epsge
        Real(C_DOUBLE),       Intent(INOUT) :: endpar 
        Type(C_PTR),          Intent(INOUT) :: curve
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1611
    End Interface

    c_numpt = numpt
    c_dim   = dim
    c_open  = iopen
    c_order = order

    Call c_s1611(point, c_numpt, c_dim, typept, c_open, c_order, startpar,     &
                 epsge, endpar, curve%cptr, c_stat)

    stat = c_stat
    Call curveCtoF(curve)
    
  End Subroutine s1611

!---------------------------------- s1630 -------------------------------------

  Subroutine s1630(epoint, inbpnt, astpar, iopen, idim, ik, rc, jstat) 

!! PURPOSE
!!   s1630 - To compute a curve using the input points as controlling vertices.
!!           The distances between the points are used as parametrization.
!!           The output will be represented as a B-spline curve.

!! INTERFACE
!!   Subroutine s1630(epoint, inbpnt, astpar, iopen, idim, ik, rc, jstat) 
!!     Real(REAL64),    Intent(IN)    :: epoint(*)
!!     Integer,         Intent(IN)    :: inbpnt 
!!     Real(REAL64),    Intent(IN)    :: astpar 
!!     Integer,         Intent(IN)    :: iopen 
!!     Integer,         Intent(IN)    :: idim 
!!     Integer,         Intent(IN)    :: ik
!!     Type(SISLcurve), Intent(INOUT) :: rc 
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Real(REAL64),    Intent(IN)    :: epoint(*)
    Integer,         Intent(IN)    :: inbpnt 
    Real(REAL64),    Intent(IN)    :: astpar 
    Integer,         Intent(IN)    :: iopen 
    Integer,         Intent(IN)    :: idim 
    Integer,         Intent(IN)    :: ik
    Type(SISLcurve), Intent(INOUT) :: rc 
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_inbpnt, c_idim, c_iopen, c_ik, c_jstat

    Interface
      Subroutine c_s1630(epoint, inbpnt, astpar, iopen, idim, ik, rc, jstat)   &
                         BIND(C,name="s1630")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Real(C_DOUBLE),       Intent(IN)    :: epoint(*)
        Integer(C_INT), VALUE               :: inbpnt     
        Real(C_DOUBLE), VALUE               :: astpar 
        Integer(C_INT), VALUE               :: iopen     
        Integer(C_INT), VALUE               :: idim     
        Integer(C_INT), VALUE               :: ik     
        Type(C_PTR),          Intent(INOUT) :: rc 
        Integer(C_INT),       Intent(INOUT) :: jstat 

      End Subroutine c_s1630
    End Interface

    c_inbpnt = inbpnt 
    c_iopen  = iopen
    c_idim   = idim
    c_ik     = ik

    Call c_s1630(epoint, c_inbpnt, astpar, c_iopen, c_idim, c_ik, rc%cptr,     &
                 c_jstat)

    jstat = c_jstat
    Call curveCtoF(rc)
    
  End Subroutine s1630

!---------------------------------- s1360 -------------------------------------

  Subroutine s1360(oldcurve, offset, epsge, norm, max, dim, newcurve, stat)

!! PURPOSE
!!   s1360 - To create a approximation of the offset to a curve within a toler-
!!           ance. The output will be represented as a B-spline curve.
!!           With an offset of zero, this routine can be used to approximate any
!!           any NURBS curve, within a tolerance, with a (non-rational) B-spline
!!           curve.

!! INTERFACE
!!   Subroutine s1360(oldcurve, offset, epsge, norm, max, dim, newcurve, stat)
!!     Type(SISLcurve), Intent(IN)    :: oldcurve 
!!     Real(REAL64),    Intent(IN)    :: offset 
!!     Real(REAL64),    Intent(IN)    :: epsge 
!!     Real(REAL64),    Intent(IN)    :: norm(*) 
!!     Real(REAL64),    Intent(IN)    :: max 
!!     Integer,         Intent(IN)    :: dim 
!!     Type(SISLcurve), Intent(INOUT) :: newcurve 
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: oldcurve 
    Real(REAL64),    Intent(IN)    :: offset 
    Real(REAL64),    Intent(IN)    :: epsge 
    Real(REAL64),    Intent(IN)    :: norm(*) 
    Real(REAL64),    Intent(IN)    :: max 
    Integer,         Intent(IN)    :: dim 
    Type(SISLcurve), Intent(INOUT) :: newcurve 
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_dim, c_stat

    Interface
      Subroutine c_s1360(oldcurve, offset, epsge, norm, max, dim, newcurve,    &
                         stat) BIND(C,name="s1360")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: oldcurve 
        Real(C_DOUBLE), VALUE               :: offset 
        Real(C_DOUBLE), VALUE               :: epsge 
        Real(C_DOUBLE),       Intent(IN)    :: norm(*) 
        Real(C_DOUBLE), VALUE               :: max
        Integer(C_INT), VALUE               :: dim 
        Type(C_PTR),          Intent(INOUT) :: newcurve 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1360
    End Interface

    c_dim = dim

    Call c_s1360(oldcurve%cptr, offset, epsge, norm, max, c_dim,               &
                 newcurve%cptr, c_stat)

    stat = c_stat
    Call curveCtoF(newcurve)

  End Subroutine s1360

!---------------------------------- s1613 -------------------------------------

  Subroutine s1613(curve, epsge, points, numpoints, stat)

!! PURPOSE
!!   1613 - To calculate a set of points on a curve. The straight lines between
!!          the points will not deviate more than epsge from the curve at any
!!          point. The generated points will have the same spatial dimension
!!          as the input curve.

!! INTERFACE
!!   Subroutine s1613(curve, epsge, points, numpoints, stat)
!!     Type(SISLcurve),              Intent(IN)    :: curve 
!!     Real(REAL64),                 Intent(IN)    :: epsge
!!     Real(REAL64),    ALLOCATABLE, Intent(INOUT) :: points(:)
!!     Integer,                      Intent(INOUT) :: numpoints 
!!     Integer,                      Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLcurve),              Intent(IN)    :: curve 
    Real(REAL64),                 Intent(IN)    :: epsge
    Real(REAL64),    ALLOCATABLE, Intent(INOUT) :: points(:)
    Integer,                      Intent(INOUT) :: numpoints 
    Integer,                      Intent(INOUT) :: stat

    Integer(C_INT) :: c_numpoints, c_stat

    Type(C_PTR)           :: c_points
    Real(REAL64), Pointer :: f_points(:)

    Interface
      Subroutine c_s1613(curve, epsge, points, numpoints, stat)                &
                         BIND(C,name="s1613")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: curve 
        Real(C_DOUBLE), VALUE               :: epsge 
        Type(C_PTR),          Intent(INOUT) :: points
        Integer(C_INT),       Intent(INOUT) :: numpoints 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1613
    End Interface

    Call c_s1613(curve%cptr, epsge, c_points, c_numpoints, c_stat)

! Unroll pointer to points array defined in s1613 and copy contents to 
! ALLOCATABLE array

    numpoints = c_numpoints
    If (C_ASSOCIATED(c_points)) Then
      Call C_F_Pointer(c_points, f_points, [curve%idim*numpoints])
      If (ALLOCATED(points)) DEALLOCATE(points)
      ALLOCATE(points(curve%idim*numpoints), SOURCE=0.0_REAL64)
      points(:) = f_points(:)
      NULLIFY(f_points)
      Call c_free(c_points)
    End If

    stat = c_stat

  End Subroutine s1613

!---------------------------------- s1600 -------------------------------------

  Subroutine s1600(oldcurve, point, normal, dim, newcurve, stat)

!! PURPOSE
!!   s1600 - To mirror a curve around a plane.

!! INTERFACE
!!   Subroutine s1600(oldcurve, point, normal, dim, newcurve, stat)
!!     Type(SISLcurve), Intent(IN)    :: oldcurve 
!!     Real(REAL64),    Intent(IN)    :: point(*)
!!     Real(REAL64),    Intent(IN)    :: normal(*)
!!     Integer,         Intent(IN)    :: dim 
!!     Type(SISLcurve), Intent(INOUT) :: newcurve 
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: oldcurve 
    Real(REAL64),    Intent(IN)    :: point(*)
    Real(REAL64),    Intent(IN)    :: normal(*)
    Integer,         Intent(IN)    :: dim 
    Type(SISLcurve), Intent(INOUT) :: newcurve 
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_dim, c_stat

    Interface
      Subroutine c_s1600(oldcurve, point, normal, dim, newcurve, stat)         &
                         BIND(C,name="s1600")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: oldcurve 
        Real(C_DOUBLE),       Intent(IN)    :: point(*)
        Real(C_DOUBLE),       Intent(IN)    :: normal(*)
        Integer(C_INT), VALUE               :: dim 
        Type(C_PTR),          Intent(INOUT) :: newcurve 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1600
    End Interface

    c_dim = dim
    Call c_s1600(oldcurve%cptr, point, normal, c_dim, newcurve%cptr, c_stat) 

    stat = c_stat
    Call curveCtoF(newcurve)

  End Subroutine s1600

!---------------------------------- s1389 -------------------------------------

  Subroutine s1389(curve, cubic, numcubic, dim, stat)

!! PURPOSE
!!   s1389 - Convert a curve of order up to 4 to a sequence of non-rational
!!           cubic segments with uniform parameterization.

!! INTERFACE
!!   Subroutine s1389(curve, cubic, numcubic, dim, stat)
!!     Type(SISLcurve),              Intent(IN)    :: curve 
!!     Real(REAL64),    ALLOCATABLE, Intent(INOUT) :: cubic(:)
!!     Integer,                      Intent(INOUT) :: numcubic
!!     Integer,                      Intent(INOUT) :: dim 
!!     Integer,                      Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLcurve),              Intent(IN)    :: curve 
    Real(REAL64),    ALLOCATABLE, Intent(INOUT) :: cubic(:)
    Integer,                      Intent(INOUT) :: numcubic
    Integer,                      Intent(INOUT) :: dim 
    Integer,                      Intent(INOUT) :: stat

    Integer(C_INT) :: c_numcubic, c_dim, c_stat

    Type(C_PTR)           :: c_cubic
    Real(REAL64), Pointer :: f_cubic(:)

    Interface
      Subroutine c_s1389(curve, cubic, numcubic, dim, stat)                    &
                         BIND(C,name="s1389")

        IMPORT :: C_PTR, C_INT
        Implicit NONE

        Type(C_PTR),    VALUE               :: curve 
        Type(C_PTR),          Intent(INOUT) :: cubic 
        Integer(C_INT),       Intent(INOUT) :: numcubic
        Integer(C_INT),       Intent(INOUT) :: dim 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1389
    End Interface

    Call c_s1389(curve%cptr, c_cubic, c_numcubic, c_dim, c_stat)
    numcubic = c_numcubic
    dim      = c_dim

! Unroll pointer to cubic array defined in s1389 and copy its contents to
! ALLOCATABLE array

    If (C_ASSOCIATED(c_cubic)) Then
      Call C_F_Pointer(c_cubic, f_cubic, [4*dim*numcubic])
      If (ALLOCATED(cubic)) DEALLOCATE(cubic)
      ALLOCATE(cubic(4*dim*numcubic), SOURCE=0.0_REAL64)
      cubic(:) = f_cubic(:)
      NULLIFY(f_cubic)
      Call c_free(c_cubic)
    End If

    stat = c_stat

  End Subroutine s1389

!---------------------------------- s1730 -------------------------------------

  Subroutine s1730(curve, newcurve, stat)

!! PURPOSE
!!   s1730 - To convert a curve to a sequence of Bezier curves. The Bezier
!!           curves are stored as one curve with all knots of multiplicity
!!           newcurve%ik (order of the curve). If the input curve is rational,
!!           the generated Bezier curves will be rational too (i.e. there will
!!           be rational weights in the representation of the Bezier curves).

!! INTERFACE
!!   Subroutine s1730(curve, newcurve, stat)
!!     Type(SISLcurve), Intent(IN)    :: curve 
!!     Type(SISLcurve), Intent(INOUT) :: newcurve 
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: curve 
    Type(SISLcurve), Intent(INOUT) :: newcurve 
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_stat

    Interface
      Subroutine c_s1730(curve, newcurve, stat)                                &
                         BIND(C,name="s1730")

        IMPORT :: C_PTR, C_INT
        Implicit NONE

        Type(C_PTR),    VALUE               :: curve 
        Type(C_PTR),          Intent(INOUT) :: newcurve 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1730
    End Interface

    Call c_s1730(curve%cptr, newcurve%cptr, c_stat)

    stat = c_stat
    Call curveCtoF(newcurve)

  End Subroutine s1730

!---------------------------------- s1732 -------------------------------------

  Subroutine s1732(curve, number, startpar, endpar, coef, stat)

!! PURPOSE
!!   s1732 - To pick out the next Bezier curve from a curve. This function re-
!!           quires a curve represented as the curve that is output from s1730.
!!           If the input curve is rational, the generated Bezier curves will be
!!           rational too (i.e. there will be rational weights in the 
!!           representation  of the Bezier curves).

!! INTERFACE
!!   Subroutine s1732(curve, number, startpar, endpar, coef, stat)
!!     Type(SISLcurve),              Intent(IN)    :: curve 
!!     Integer,                      Intent(IN)    :: number 
!!     Real(REAL64),                 Intent(INOUT) :: startpar 
!!     Real(REAL64),                 Intent(INOUT) :: endpar 
!!     Real(REAL64),                 Intent(INOUT) :: coef(*)
!!     Integer,                      Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLcurve),              Intent(IN)    :: curve 
    Integer,                      Intent(IN)    :: number 
    Real(REAL64),                 Intent(INOUT) :: startpar 
    Real(REAL64),                 Intent(INOUT) :: endpar 
    Real(REAL64),                 Intent(INOUT) :: coef(*)
    Integer,                      Intent(INOUT) :: stat

    Integer(C_INT) :: c_number, c_stat

    Interface
      Subroutine c_s1732(curve, number, startpar, endpar, coef, stat)          &
                         BIND(C,name="s1732")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: curve 
        Integer(C_INT), VALUE               :: number
        Real(C_DOUBLE),       Intent(INOUT) :: startpar 
        Real(C_DOUBLE),       Intent(INOUT) :: endpar 
        Real(C_DOUBLE),       Intent(INOUT) :: coef(*) 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1732
    End Interface

    c_number = number
    coef(1:((curve%idim+1)*curve%ik)) = 0.0_REAL64 
    Call c_s1732(curve%cptr, c_number, startpar, endpar, coef, c_stat)

    stat = c_stat

  End Subroutine s1732

!---------------------------------- s1750 -------------------------------------

  Subroutine s1750(curve, order, newcurve, stat)

!! PURPOSE
!!   s1720 - To express the “i”-th derivative of an open curve as a curve.

!! INTERFACE
!!   Subroutine s1750(curve, order, newcurve, stat)
!!     Type(SISLcurve), Intent(IN)    :: curve 
!!     Integer,         Intent(IN)    :: order
!!     Type(SISLcurve), Intent(INOUT) :: newcurve 
!!     Integer,         Intent(INOUT) :: stat


    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: curve 
    Integer,         Intent(IN)    :: order
    Type(SISLcurve), Intent(INOUT) :: newcurve 
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_order, c_stat

    Interface
      Subroutine c_s1750(curve, order, newcurve, stat)                         &
                         BIND(C,name="s1750")

        IMPORT :: C_PTR, C_INT
        Implicit NONE

        Type(C_PTR),    VALUE               :: curve 
        Integer(C_INT), VALUE               :: order 
        Type(C_PTR),          Intent(INOUT) :: newcurve 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1750
    End Interface
    
    c_order = order
    Call c_s1750(curve%cptr, c_order, newcurve%cptr, c_stat)

    stat = c_stat
    Call curveCtoF(newcurve)

  End Subroutine s1750

!---------------------------------- s1720 -------------------------------------

  Subroutine s1720(curve, derive, newcurve, stat)

!! PURPOSE
!!   s1720 - To express the “i”-th derivative of an open curve as a curve.

!! INTERFACE
!!  Subroutine s1720(curve, derive, newcurve, stat)
!!    Type(SISLcurve), Intent(IN)    :: curve 
!!    Integer,         Intent(IN)    :: derive 
!!    Type(SISLcurve), Intent(INOUT) :: newcurve 
!!    Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: curve 
    Integer,         Intent(IN)    :: derive 
    Type(SISLcurve), Intent(INOUT) :: newcurve 
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_derive, c_stat

    Interface
      Subroutine c_s1720(curve, derive, newcurve, stat)                        &
                         BIND(C,name="s1720")

        IMPORT :: C_PTR, C_INT
        Implicit NONE

        Type(C_PTR),    VALUE               :: curve 
        Integer(C_INT), VALUE               :: derive 
        Type(C_PTR),          Intent(INOUT) :: newcurve 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1720
    End Interface
    
    c_derive = derive
    Call c_s1720(curve%cptr, c_derive, newcurve%cptr, c_stat)

    stat = c_stat
    Call curveCtoF(newcurve)

  End Subroutine s1720

!---------------------------------- s1522 -------------------------------------

  Subroutine s1522(normal, centre, ellipaxis, ratio, dim, ellipse, stat)

!! PURPOSE
!!   s1522 - Convert a 2D or 3D analytical ellipse to a curve. The curve will
!!           be geometrically exact.

!! INTERFACE
!!   Subroutine s1522(normal, centre, ellipaxis, ratio, dim, ellipse, stat)
!!     Real(REAL64),    Intent(IN)    :: normal(*)
!!     Real(REAL64),    Intent(IN)    :: centre(*)
!!     Real(REAL64),    Intent(IN)    :: ellipaxis(*)
!!     Real(REAL64),    Intent(IN)    :: ratio 
!!     Integer,         Intent(IN)    :: dim 
!!     Type(SISLcurve), Intent(INOUT) :: ellipse 
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Real(REAL64),    Intent(IN)    :: normal(*)
    Real(REAL64),    Intent(IN)    :: centre(*)
    Real(REAL64),    Intent(IN)    :: ellipaxis(*)
    Real(REAL64),    Intent(IN)    :: ratio 
    Integer,         Intent(IN)    :: dim 
    Type(SISLcurve), Intent(INOUT) :: ellipse 
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_dim, c_stat

    Interface
      Subroutine c_s1522(normal, centre, ellipaxis, ratio, dim, ellipse, stat) &
                         BIND(C,name="s1522")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Real(C_DOUBLE),       Intent(IN)    :: normal(*)
        Real(C_DOUBLE),       Intent(IN)    :: centre(*)
        Real(C_DOUBLE),       Intent(IN)    :: ellipaxis(*)
        Real(C_DOUBLE), VALUE               :: ratio 
        Integer(C_INT), VALUE               :: dim 
        Type(C_PTR),          Intent(INOUT) :: ellipse 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1522
    End Interface

    c_dim = dim
    Call c_s1522(normal, centre, ellipaxis, ratio, c_dim, ellipse%cptr, c_stat)

    stat = c_stat
    Call curveCtoF(ellipse)

  End Subroutine s1522

!---------------------------------- s1011 -------------------------------------

  Subroutine s1011(start_pos, top_pos, end_pos, shape, dim, arc_seg, stat)

!! PURPOSE
!!   s1011 - Convert an analytic conic arc to a curve. The curve will be geo-
!!           metrically exact. The arc is given by position at start, shoulder
!!           point and end, and a shape factor.

!! INTERFACE
!!   Subroutine s1011(start_pos, top_pos, end_pos, shape, dim, arc_seg, stat)
!!     Real(REAL64),    Intent(IN)    :: start_pos(*)
!!     Real(REAL64),    Intent(IN)    :: top_pos(*)
!!     Real(REAL64),    Intent(IN)    :: end_pos(*)
!!     Real(REAL64),    Intent(IN)    :: shape 
!!     Integer,         Intent(IN)    :: dim 
!!     Type(SISLcurve), Intent(INOUT) :: arc_seg 
!!     Integer,         Intent(INOUT) :: stat
!!     Implicit NONE

    Real(REAL64),    Intent(IN)    :: start_pos(*)
    Real(REAL64),    Intent(IN)    :: top_pos(*)
    Real(REAL64),    Intent(IN)    :: end_pos(*)
    Real(REAL64),    Intent(IN)    :: shape 
    Integer,         Intent(IN)    :: dim 
    Type(SISLcurve), Intent(INOUT) :: arc_seg 
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_dim, c_stat

    Interface
      Subroutine c_s1011(start_pos, top_pos, end_pos, shape, dim, arc_seg,     &
                         stat) BIND(C,name="s1011")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Real(C_DOUBLE),       Intent(IN)    :: start_pos(*)
        Real(C_DOUBLE),       Intent(IN)    :: top_pos(*)
        Real(C_DOUBLE),       Intent(IN)    :: end_pos(*)
        Real(C_DOUBLE), VALUE               :: shape 
        Integer(C_INT), VALUE               :: dim 
        Type(C_PTR),          Intent(INOUT) :: arc_seg 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1011
    End Interface

    c_dim = dim
    Call c_s1011(start_pos, top_pos, end_pos, shape, c_dim, arc_seg%cptr,      &
                 c_stat)

    stat = c_stat
    Call curveCtoF(arc_seg)

  End Subroutine s1011

!---------------------------------- s1012 -------------------------------------

  Subroutine s1012(start_pos, axis_pos, axis_dir, frequency, numb_quad,        &
                   counter_clock, helix, stat)

!! PURPOSE
!!   s1012 - Convert an analytical truncated helix to a curve. The curve will
!!           be geometrically exact.

!! INTERFACE
!!   Subroutine s1012(start_pos, axis_pos, axis_dir, frequency, numb_quad,     &
!!                    counter_clock, helix, stat)
!!     Real(REAL64),    Intent(IN)    :: start_pos(*)
!!     Real(REAL64),    Intent(IN)    :: axis_pos(*)
!!     Real(REAL64),    Intent(IN)    :: axis_dir(*)
!!     Real(REAL64),    Intent(IN)    :: frequency 
!!     Integer,         Intent(IN)    :: numb_quad 
!!     Integer,         Intent(IN)    :: counter_clock 
!!     Type(SISLcurve), Intent(INOUT) :: helix 
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Real(REAL64),    Intent(IN)    :: start_pos(*)
    Real(REAL64),    Intent(IN)    :: axis_pos(*)
    Real(REAL64),    Intent(IN)    :: axis_dir(*)
    Real(REAL64),    Intent(IN)    :: frequency 
    Integer,         Intent(IN)    :: numb_quad 
    Integer,         Intent(IN)    :: counter_clock 
    Type(SISLcurve), Intent(INOUT) :: helix 
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_numb_quad, c_counter_clock, c_stat

    Interface
      Subroutine c_s1012(start_pos, axis_pos, axis_dir, frequency, numb_quad,  &
                 counter_clock, helix, stat)                                   &
                 BIND(C,name="s1012")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Real(C_DOUBLE),       Intent(IN)    :: start_pos(*)
        Real(C_DOUBLE),       Intent(IN)    :: axis_pos(*)
        Real(C_DOUBLE),       Intent(IN)    :: axis_dir(*)
        Real(C_DOUBLE), VALUE               :: frequency 
        Integer(C_INT), VALUE               :: numb_quad 
        Integer(C_INT), VALUE               :: counter_clock 
        Type(C_PTR),          Intent(INOUT) :: helix 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1012
    End Interface

    c_numb_quad     = numb_quad
    c_counter_clock = counter_clock
    Call c_s1012(start_pos, axis_pos, axis_dir, frequency, c_numb_quad,        &
                 c_counter_clock, helix%cptr, c_stat) 

    stat = c_stat
    Call curveCtoF(helix)

  End Subroutine s1012

End Module Curve_Definition
