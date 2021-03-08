
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

Module Curve_Interrogation
  
!! Module Surface_Interrogation contains Modern Fortran C-interoperability 
!! routines for C functions described in Chapter 3 of version 4.4 of the SISL
!! reference manual. Some of the functions support an option to enable graphics
!! as part of the function call. This feature is not supported

!! Written by: Richard Weed, Ph.D.
!! Version no. 1.0 - Feb. 2021
!!   Initial development version

!! Contact: rweedmsu@gmail.com

  USE forSISLdata, ONLY: REAL64, C_INT, C_DOUBLE, C_PTR, C_NULL_PTR, c_free,   &
                         C_ASSOCIATED, C_F_POINTER, SISLcurve, SISLIntcurve,   &
                         SISLbox, SISLdir, curveCtoF, IntCurveCtoF, boxCtoF,   &
                         dirCtoF 

  Implicit NONE

  PRIVATE :: REAL64, C_INT, C_DOUBLE, C_PTR, C_NULL_PTR, c_free,               &
             C_ASSOCIATED, C_F_POINTER, SISLcurve, SISLIntcurve, SISLbox,      &
             SISLdir, curveCtoF, IntCurveCtoF, boxCtoF, dirCtoF
 
Contains

!---------------------------------- s1871 -------------------------------------

  Subroutine s1871(pc1, pt1, idim, aepsge, jpt, gpar1, jcrv, wcurve, jstat)

!! PURPOSE
!!   s1871 - Find all the intersections between a curve and a point.

!! INTERFACE
!!   Subroutine s1871(pc1, pt1, idim, aepsge, jpt, gpar1, jcrv, wcurve, jstat)
!!     Type(SISLcurve),                 Intent(IN)    :: pc1
!!     Real(REAL64),                    Intent(IN)    :: pt1(*)
!!     Integer,                         Intent(IN)    :: idim
!!     Real(REAL64),                    Intent(IN)    :: aepsge 
!!     Integer,                         Intent(INOUT) :: jpt 
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: gpar1(:)
!!     Integer,                         Intent(INOUT) :: jcrv 
!!     Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: wcurve(:)
!!     Integer,                         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLcurve),                 Intent(IN)    :: pc1
    Real(REAL64),                    Intent(IN)    :: pt1(*)
    Integer,                         Intent(IN)    :: idim
    Real(REAL64),                    Intent(IN)    :: aepsge 
    Integer,                         Intent(INOUT) :: jpt 
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: gpar1(:)
    Integer,                         Intent(INOUT) :: jcrv 
    Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: wcurve(:)
    Integer,                         Intent(INOUT) :: jstat


    Integer :: i

    Integer(C_INT)        :: c_idim, c_jpt, c_jcrv, c_jstat
    Type(C_PTR)           :: c_wcurve_p, c_gpar1_p
    Type(C_PTR),  Pointer :: cwcurve(:)
    Real(REAL64), Pointer :: gpar1p(:)

    Interface
      Subroutine c_s1871(pc1, pt1, idim, aepsge, jpt, gpar1, jcrv, wcurve,     &
                         jstat) BIND(C,name="s1871")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: pc1
        Real(C_DOUBLE),       Intent(IN)    :: pt1(*)
        Integer(C_INT), VALUE               :: idim
        Real(C_DOUBLE), VALUE               :: aepsge
        Integer(C_INT),       Intent(INOUT) :: jpt 
        Type(C_PTR),          Intent(INOUT) :: gpar1
        Integer(C_INT),       Intent(INOUT) :: jcrv 
        Type(C_PTR),          Intent(INOUT) :: wcurve
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1871
    End Interface

    c_idim = idim

    Call c_s1871(pc1%cptr, pt1, c_idim, aepsge, c_jpt, c_gpar1_p, c_jcrv,      &
                 c_wcurve_p, c_jstat)

    jcrv = c_jcrv
    jpt  = c_jpt
    If (jpt > 0) Then
      If (C_ASSOCIATED(c_gpar1_p)) Then
        Call C_F_Pointer(c_gpar1_p, gpar1p,[jpt])
        If (ALLOCATED(gpar1)) DEALLOCATE(gpar1)
        ALLOCATE(gpar1(jpt), SOURCE=0.0_REAL64)
        gpar1(:) = gpar1p(:)
        NULLIFY(gpar1p)
        Call c_free(c_gpar1_p)
      EndIf
    EndIf
    If (jcrv > 0) Then
    
      If (C_ASSOCIATED(c_wcurve_p)) Then
        Call C_F_Pointer(c_wcurve_p, cwcurve, [jcrv])
        If (ALLOCATED(wcurve)) Deallocate(wcurve)
        ALLOCATE(wcurve(jcrv))
        Do i=1,jcrv
          If (C_ASSOCIATED(cwcurve(i))) Then
            wcurve(i)%cptr = cwcurve(i)
          Else
            wcurve(i)%cptr = C_NULL_PTR
          End If
        EndDo
        Call IntCurveCtoF(wcurve(i))
      EndIf
    EndIf

    If (ASSOCIATED(cwcurve)) NULLIFY(cwcurve)
    c_wcurve_p = C_NULL_PTR

    jstat = c_jstat

  End Subroutine s1871

!---------------------------------- s1850 -------------------------------------

  Subroutine s1850(curve, point, normal, dim, epsco, epsge, numintpt,          &
                   intpar, numintcu, intcurve, stat)

!! PURPOSE
!!   s1850 - Find all the intersections between a curve and a plane (if curve
!!           dimension and dim = 3) or a curve and a line (if curve dimension
!!           and dim = 2).

!! INTERFACE
!!   Subroutine s1850(curve, point, normal, dim, epsco, epsge, numintpt,       &
!!                    intpar, numintcu, intcurve, stat)
!!     Type(SISLcurve),                 Intent(IN)    :: curve
!!     Real(REAL64),                    Intent(IN)    :: point(*)
!!     Real(REAL64),                    Intent(IN)    :: normal(*)
!!     Integer,                         Intent(IN)    :: dim
!!     Real(REAL64),                    Intent(IN)    :: epsco 
!!     Real(REAL64),                    Intent(IN)    :: epsge 
!!     Integer,                         Intent(INOUT) :: numintpt 
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: intpar(:)
!!     Integer,                         Intent(INOUT) :: numintcu 
!!     Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurve(:)
!!     Integer,                         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLcurve),                 Intent(IN)    :: curve
    Real(REAL64),                    Intent(IN)    :: point(*)
    Real(REAL64),                    Intent(IN)    :: normal(*)
    Integer,                         Intent(IN)    :: dim
    Real(REAL64),                    Intent(IN)    :: epsco 
    Real(REAL64),                    Intent(IN)    :: epsge 
    Integer,                         Intent(INOUT) :: numintpt 
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: intpar(:)
    Integer,                         Intent(INOUT) :: numintcu 
    Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurve(:)
    Integer,                         Intent(INOUT) :: stat


    Integer :: i

    Integer(C_INT)        :: c_dim, c_numintpt, c_numintcu, c_stat
    Type(C_PTR)           :: c_intcurve_p, c_intpar_p
    Type(C_PTR),  Pointer :: cintcurve(:)
    Real(REAL64), Pointer :: intparp(:)

    Interface
      Subroutine c_s1850(curve, point, normal, dim, epsco, epsge, numintpt,    &
                   intpar, numintcu, intcurve, stat)                                   &
                   BIND(C,name="s1850")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: curve
        Real(C_DOUBLE),       Intent(IN)    :: point(*)
        Real(C_DOUBLE),       Intent(IN)    :: normal(*)
        Integer(C_INT), VALUE               :: dim
        Real(C_DOUBLE), VALUE               :: epsco
        Real(C_DOUBLE), VALUE               :: epsge
        Integer(C_INT),       Intent(INOUT) :: numintpt 
        Type(C_PTR),          Intent(INOUT) :: intpar 
        Integer(C_INT),       Intent(INOUT) :: numintcu 
        Type(C_PTR),          Intent(INOUT) :: intcurve
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1850
    End Interface

    c_dim = dim

    Call c_s1850(curve%cptr, point, normal, c_dim, epsco, epsge,               &
                 c_numintpt, c_intpar_p, c_numintcu,  c_intcurve_p, c_stat)

    numintcu = c_numintcu
    numintpt = c_numintpt
    If (numintpt > 0) Then
      If (C_ASSOCIATED(c_intpar_p)) Then
        Call C_F_Pointer(c_intpar_p, intparp,[numintpt])
        If (ALLOCATED(intpar)) DEALLOCATE(intpar)
        ALLOCATE(intpar(numintpt), SOURCE=0.0_REAL64)
        intpar(:) = intparp(:)
        NULLIFY(intparp)
        Call c_free(c_intpar_p)
      EndIf
    EndIf

    If (numintcu > 0) Then
    
      If (C_ASSOCIATED(c_intcurve_p)) Then
        Call C_F_Pointer(c_intcurve_p, cintcurve, [numintcu])
        If (ALLOCATED(intcurve)) Deallocate(intcurve)
        ALLOCATE(intcurve(numintcu))
        Do i=1,numintcu
          If (C_ASSOCIATED(cintcurve(i))) Then
            intcurve(i)%cptr = cintcurve(i)
          Else
            intcurve(i)%cptr = C_NULL_PTR
          End If
        EndDo
        Call IntCurveCtoF(intcurve(i))
      EndIf
    EndIf

    If (ASSOCIATED(cintcurve)) NULLIFY(cintcurve)
    c_intcurve_p = C_NULL_PTR

    stat = c_stat

  End Subroutine s1850

!---------------------------------- s1327 -------------------------------------

  Subroutine s1327(pcold, epoint, enorm1, enorm2, idim, rcnew, jstat)

!! PURPOSE
!!   1327 - Put the equation of the curve pointed at by pcold into two planes
!!          given by the point epoint and the normals enorm1 and enorm2.
!1          The result is an equation where the new two-dimensional curve
!!          rcnew is to be equal to origo.

!! INTERFACE
!!   Subroutine s1327(pcold, epoint, enorm1, enorm2, idim, rcnew, jstat)
!!     Type(SISLcurve), Intent(IN)    :: pcold 
!!     Real(REAL64),    Intent(IN)    :: epoint(*)
!!     Real(REAL64),    Intent(IN)    :: enorm1(*)
!!     Real(REAL64),    Intent(IN)    :: enorm2(*)
!!     Integer,         Intent(IN)    :: idim
!!     Type(SISLcurve), Intent(INOUT) :: rcnew 
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: pcold 
    Real(REAL64),    Intent(IN)    :: epoint(*)
    Real(REAL64),    Intent(IN)    :: enorm1(*)
    Real(REAL64),    Intent(IN)    :: enorm2(*)
    Integer,         Intent(IN)    :: idim
    Type(SISLcurve), Intent(INOUT) :: rcnew 
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_idim, c_jstat

    Interface
      Subroutine c_s1327(pcold, epoint, enorm1, enorm2, idim, rcnew, jstat)    &
                         BIND(C,name="s1327")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: pcold 
        Real(C_DOUBLE),       Intent(IN)    :: epoint(*)
        Real(C_DOUBLE),       Intent(IN)    :: enorm1(*)
        Real(C_DOUBLE),       Intent(IN)    :: enorm2(*)
        Integer(C_INT), VALUE               :: idim
        Type(C_PTR),          Intent(INOUT) :: rcnew 
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1327
    End Interface

    c_idim = idim
    Call c_s1327(pcold%cptr, epoint, enorm1, enorm2, c_idim, rcnew%cptr,       &
                 c_jstat)

    jstat = c_jstat
    Call curveCtoF(rcnew)

  End Subroutine s1327

!---------------------------------- s1371 -------------------------------------

  Subroutine s1371(curve, centre, radius, dim, epsco, epsge, numintpt, intpar, &
                   numintcu, intcurve, stat)

!! PURPOSE
!!   s1371 - Find all the intersections between a curve and a sphere (if curve
!!           dimension and dim = 3), or a curve and a circle (if curve dimension
!!           and dim = 2).

!! INTERFACE
!!   Subroutine s1371(curve, centre, radius, dim, epsco, epsge, numintpt,      &
!!                    intpar, numintcu, intcurve, stat)
!!     Type(SISLcurve),                 Intent(IN)    :: curve
!!     Real(REAL64),                    Intent(IN)    :: centre(*)
!!     Real(REAL64),                    Intent(IN)    :: radius 
!!     Integer,                         Intent(IN)    :: dim
!!     Real(REAL64),                    Intent(IN)    :: epsco 
!!     Real(REAL64),                    Intent(IN)    :: epsge 
!!     Integer,                         Intent(INOUT) :: numintpt 
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: intpar(:)
!!     Integer,                         Intent(INOUT) :: numintcu 
!!     Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurve(:)
!!     Integer,                         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLcurve),                 Intent(IN)    :: curve
    Real(REAL64),                    Intent(IN)    :: centre(*)
    Real(REAL64),                    Intent(IN)    :: radius 
    Integer,                         Intent(IN)    :: dim
    Real(REAL64),                    Intent(IN)    :: epsco 
    Real(REAL64),                    Intent(IN)    :: epsge 
    Integer,                         Intent(INOUT) :: numintpt 
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: intpar(:)
    Integer,                         Intent(INOUT) :: numintcu 
    Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurve(:)
    Integer,                         Intent(INOUT) :: stat

    Integer :: i

    Integer(C_INT)        :: c_dim, c_numintpt, c_numintcu, c_stat
    Type(C_PTR)           :: c_intcurve_p, c_intpar_p
    Type(C_PTR),  Pointer :: cintcurve(:)
    Real(REAL64), Pointer :: intparp(:)

    Interface
      Subroutine c_s1371(curve, centre, radius, dim, epsco, epsge, numintpt,   &
                   intpar, numintcu, intcurve, stat)                           &
                   BIND(C,name="s1371")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: curve
        Real(C_DOUBLE),       Intent(IN)    :: centre(*)
        Real(C_DOUBLE), VALUE               :: radius
        Integer(C_INT), VALUE               :: dim
        Real(C_DOUBLE), VALUE               :: epsco
        Real(C_DOUBLE), VALUE               :: epsge
        Integer(C_INT),       Intent(INOUT) :: numintpt 
        Type(C_PTR),          Intent(INOUT) :: intpar 
        Integer(C_INT),       Intent(INOUT) :: numintcu 
        Type(C_PTR),          Intent(INOUT) :: intcurve
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1371
    End Interface

    c_dim = dim

    Call c_s1371(curve%cptr, centre, radius, c_dim, epsco, epsge,              &
                 c_numintpt, c_intpar_p, c_numintcu,  c_intcurve_p, c_stat)

    numintcu = c_numintcu
    numintpt = c_numintpt
    If (numintpt > 0) Then
      If (C_ASSOCIATED(c_intpar_p)) Then
        Call C_F_Pointer(c_intpar_p, intparp,[numintpt])
        If (ALLOCATED(intpar)) DEALLOCATE(intpar)
        ALLOCATE(intpar(numintpt), SOURCE=0.0_REAL64)
        intpar(:) = intparp(:)
        NULLIFY(intparp)
        Call c_free(c_intpar_p)
      EndIf
    EndIf

    If (numintcu > 0) Then
      If (C_ASSOCIATED(c_intcurve_p)) Then
        Call C_F_Pointer(c_intcurve_p, cintcurve, [numintcu])
        If (ALLOCATED(intcurve)) Deallocate(intcurve)
        ALLOCATE(intcurve(numintcu))
        Do i=1,numintcu
          If (C_ASSOCIATED(cintcurve(i))) Then
            intcurve(i)%cptr = cintcurve(i)
          Else
            intcurve(i)%cptr = C_NULL_PTR
          End If
        EndDo
        Call IntCurveCtoF(intcurve(i))
      EndIf
    EndIf

    If (ASSOCIATED(cintcurve)) NULLIFY(cintcurve)
    c_intcurve_p = C_NULL_PTR

    stat = c_stat

  End Subroutine s1371

!---------------------------------- s1374 -------------------------------------

  Subroutine s1374(curve, conarray, dim, epsco, epsge, numintpt, intpar,      &
                   numintcu, intcurve, stat)

!! PURPOSE
!!   s1374 - Find all the intersections between a curve and a quadric curve, (if
!!           curve dimension and dim = 2), or a curve and a quadric surface,
!!           (if curve dimension and dim = 3).

!! INTERFACE
!!   Subroutine s1374(curve, conarray, dim, epsco, epsge, numintpt, intpar,    &
!!                    numintcu, intcurve, stat)
!!     Type(SISLcurve),                 Intent(IN)    :: curve
!!     Real(REAL64),                    Intent(IN)    :: conarray(*)
!!     Integer,                         Intent(IN)    :: dim
!!     Real(REAL64),                    Intent(IN)    :: epsco 
!!     Real(REAL64),                    Intent(IN)    :: epsge 
!!     Integer,                         Intent(INOUT) :: numintpt 
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: intpar(:)
!!     Integer,                         Intent(INOUT) :: numintcu 
!!     Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurve(:)
!!     Integer,                         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLcurve),                 Intent(IN)    :: curve
    Real(REAL64),                    Intent(IN)    :: conarray(*)
    Integer,                         Intent(IN)    :: dim
    Real(REAL64),                    Intent(IN)    :: epsco 
    Real(REAL64),                    Intent(IN)    :: epsge 
    Integer,                         Intent(INOUT) :: numintpt 
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: intpar(:)
    Integer,                         Intent(INOUT) :: numintcu 
    Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurve(:)
    Integer,                         Intent(INOUT) :: stat

    Integer :: i

    Integer(C_INT)        :: c_dim, c_numintpt, c_numintcu, c_stat
    Type(C_PTR)           :: c_intcurve_p, c_intpar_p
    Type(C_PTR),  Pointer :: cintcurve(:)
    Real(REAL64), Pointer :: intparp(:)

    Interface
      Subroutine c_s1374(curve, conarray, dim, epsco, epsge, numintpt,         &
                   intpar, numintcu, intcurve, stat)                           &
                   BIND(C,name="s1374")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: curve
        Real(C_DOUBLE),       Intent(IN)    :: conarray(*)
        Integer(C_INT), VALUE               :: dim
        Real(C_DOUBLE), VALUE               :: epsco
        Real(C_DOUBLE), VALUE               :: epsge
        Integer(C_INT),       Intent(INOUT) :: numintpt 
        Type(C_PTR),          Intent(INOUT) :: intpar 
        Integer(C_INT),       Intent(INOUT) :: numintcu 
        Type(C_PTR),          Intent(INOUT) :: intcurve
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1374
    End Interface

    c_dim = dim

    Call c_s1374(curve%cptr, conarray, c_dim, epsco, epsge, c_numintpt,        &
                 c_intpar_p, c_numintcu,  c_intcurve_p, c_stat)

    numintcu = c_numintcu
    numintpt = c_numintpt
    If (numintpt > 0) Then
      If (C_ASSOCIATED(c_intpar_p)) Then
        Call C_F_Pointer(c_intpar_p, intparp,[numintpt])
        If (ALLOCATED(intpar)) DEALLOCATE(intpar)
        ALLOCATE(intpar(numintpt), SOURCE=0.0_REAL64)
        intpar(:) = intparp(:)
        NULLIFY(intparp)
        Call c_free(c_intpar_p)
      EndIf
    EndIf

    If (numintcu > 0) Then
      If (C_ASSOCIATED(c_intcurve_p)) Then
        Call C_F_Pointer(c_intcurve_p, cintcurve, [numintcu])
        If (ALLOCATED(intcurve)) Deallocate(intcurve)
        ALLOCATE(intcurve(numintcu))
        Do i=1,numintcu
          If (C_ASSOCIATED(cintcurve(i))) Then
            intcurve(i)%cptr = cintcurve(i)
          Else
            intcurve(i)%cptr = C_NULL_PTR
          End If
          Call IntCurveCtoF(intcurve(i))
        EndDo
      EndIf
    EndIf

    If (ASSOCIATED(cintcurve)) NULLIFY(cintcurve)
    c_intcurve_p = C_NULL_PTR

    stat = c_stat

  End Subroutine s1374

!---------------------------------- s1857 -------------------------------------

  Subroutine s1857(curve1, curve2, epsco, epsge, numintpt, intpar1, intpar2,   &
                   numintcu, intcurve, stat)

!! PURPOSE
!!   s1857 - Find all the intersections between two curves.

!! INTERFACE
!!   Subroutine s1857(curve1, curve2, epsco, epsge, numintpt, intpar1, intpar2,&
!!                    numintcu, intcurve, stat)
!!     Type(SISLcurve),                 Intent(IN)    :: curve1
!!     Type(SISLcurve),                 Intent(IN)    :: curve2
!!     Real(REAL64),                    Intent(IN)    :: epsco 
!!     Real(REAL64),                    Intent(IN)    :: epsge 
!!     Integer,                         Intent(INOUT) :: numintpt 
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: intpar1(:)
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: intpar2(:)
!!     Integer,                         Intent(INOUT) :: numintcu 
!!     Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurve(:)
!!     Integer,                         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLcurve),                 Intent(IN)    :: curve1
    Type(SISLcurve),                 Intent(IN)    :: curve2
    Real(REAL64),                    Intent(IN)    :: epsco 
    Real(REAL64),                    Intent(IN)    :: epsge 
    Integer,                         Intent(INOUT) :: numintpt 
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: intpar1(:)
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: intpar2(:)
    Integer,                         Intent(INOUT) :: numintcu 
    Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurve(:)
    Integer,                         Intent(INOUT) :: stat

    Integer :: i

    Integer(C_INT)        :: c_numintpt, c_numintcu, c_stat
    Type(C_PTR)           :: c_intcurve_p, c_intpar1_p, c_intpar2_p
    Type(C_PTR),  Pointer :: cintcurve(:)
    Real(REAL64), Pointer :: intpar1p(:)
    Real(REAL64), Pointer :: intpar2p(:)

    Interface
      Subroutine c_s1857(curve1, curve2, epsco, epsge, numintpt, intpar1,      &
                   intpar2, numintcu, intcurve, stat)                          &
                   BIND(C,name="s1857")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: curve1
        Type(C_PTR),    VALUE               :: curve2
        Real(C_DOUBLE), VALUE               :: epsco
        Real(C_DOUBLE), VALUE               :: epsge
        Integer(C_INT),       Intent(INOUT) :: numintpt 
        Type(C_PTR),          Intent(INOUT) :: intpar1 
        Type(C_PTR),          Intent(INOUT) :: intpar2 
        Integer(C_INT),       Intent(INOUT) :: numintcu 
        Type(C_PTR),          Intent(INOUT) :: intcurve
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1857
    End Interface

    Call c_s1857(curve1%cptr, curve2%cptr, epsco, epsge, c_numintpt,           &
                 c_intpar1_p, c_intpar2_p, c_numintcu,  c_intcurve_p, c_stat)

    numintcu = c_numintcu
    numintpt = c_numintpt

    If (numintpt > 0) Then

      If (C_ASSOCIATED(c_intpar1_p)) Then
        Call C_F_Pointer(c_intpar1_p, intpar1p,[numintpt])
        If (ALLOCATED(intpar1)) DEALLOCATE(intpar1)
        ALLOCATE(intpar1(numintpt), SOURCE=0.0_REAL64)
        intpar1(:) = intpar1p(:)
        NULLIFY(intpar1p)
        Call c_free(c_intpar1_p)
      EndIf

      If (C_ASSOCIATED(c_intpar2_p)) Then
        Call C_F_Pointer(c_intpar2_p, intpar2p,[numintpt])
        If (ALLOCATED(intpar2)) DEALLOCATE(intpar2)
        ALLOCATE(intpar2(numintpt), SOURCE=0.0_REAL64)
        intpar2(:) = intpar2p(:)
        NULLIFY(intpar2p)
        Call c_free(c_intpar2_p)
      EndIf

    EndIf

    If (numintcu > 0) Then
      If (C_ASSOCIATED(c_intcurve_p)) Then
        Call C_F_Pointer(c_intcurve_p, cintcurve, [numintcu])
        If (ALLOCATED(intcurve)) Deallocate(intcurve)
        ALLOCATE(intcurve(numintcu))
        Do i=1,numintcu
          If (C_ASSOCIATED(cintcurve(i))) Then
            intcurve(i)%cptr = cintcurve(i)
          Else
            intcurve(i)%cptr = C_NULL_PTR
          End If
          Call IntCurveCtoF(intcurve(i))
        EndDo
      EndIf
    EndIf

    If (ASSOCIATED(cintcurve)) NULLIFY(cintcurve)
    c_intcurve_p = C_NULL_PTR

    stat = c_stat

  End Subroutine s1857

!---------------------------------- s1240 -------------------------------------

  Subroutine s1240(curve, epsge, length, stat)

!! PURPOSE
!!   s1240 - Compute the length of a curve. The length calculated will not
!!           deviate more than epsge divided by the calculated length, from
!!           the real length of the curve.

!! INTERFACE
!!   Subroutine s1240(curve, epsge, length, stat)
!!     Type(SISLcurve), Intent(IN)    :: curve
!!     Real(REAL64),    Intent(IN)    :: epsge 
!!     Real(REAL64),    Intent(INOUT) :: length 
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: curve
    Real(REAL64),    Intent(IN)    :: epsge 
    Real(REAL64),    Intent(INOUT) :: length 
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_stat

    Interface
      Subroutine c_s1240(curve, epsge, length, stat)                           &
                         BIND(C,name="s1240")

        IMPORT :: C_PTR, C_DOUBLE, C_INT
        Implicit NONE

        Type(C_PTR),    VALUE               :: curve
        Real(C_DOUBLE), VALUE               :: epsge 
        Real(C_DOUBLE),       Intent(INOUT) :: length 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1240
    End Interface

    Call c_s1240(curve%cptr, epsge, length, c_stat)

    stat = c_stat

  End Subroutine s1240

!---------------------------------- s1364 -------------------------------------

  Subroutine s1364(curve, epsge, stat)

!! PURPOSE
!!   s1364 - To check if a curve is closed, i.e. test if the distance between
!!           the end points of the curve is less than a given tolerance.

!! INTERFACE
!!   Subroutine s1364(curve, epsge, stat)
!!     Type(SISLcurve), Intent(IN)    :: curve
!!     Real(REAL64),    Intent(IN)    :: epsge 
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: curve
    Real(REAL64),    Intent(IN)    :: epsge 
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_stat

    Interface
      Subroutine c_s1364(curve, epsge, stat)                                   &
                         BIND(C,name="s1364")

        IMPORT :: C_PTR, C_DOUBLE, C_INT
        Implicit NONE

        Type(C_PTR),    VALUE               :: curve
        Real(C_DOUBLE), VALUE               :: epsge 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1364
    End Interface

    Call c_s1364(curve%cptr, epsge, c_stat)

    stat = c_stat

  End Subroutine s1364

!---------------------------------- s1451 -------------------------------------

  Subroutine s1451(pc1, aepsge, jdgen, jstat)

!! PURPOSE
!!   s1451 - To check if a curve is degenerated.

!! INTERFACE
!!   Subroutine s1451(pc1, aepsge, jdgen, jstat)
!!     Type(SISLcurve), Intent(IN)    :: pc1 
!!     Real(REAL64),    Intent(IN)    :: aepsge 
!!     Integer,         Intent(INOUT) :: jdgen 
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: pc1 
    Real(REAL64),    Intent(IN)    :: aepsge 
    Integer,         Intent(INOUT) :: jdgen 
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_jstat, c_jdgen

    Interface
      Subroutine c_s1451(pc1, aepsge, jdgen, jstat)                            &
                         BIND(C,name="s1451")

        IMPORT :: C_PTR, C_DOUBLE, C_INT
        Implicit NONE

        Type(C_PTR),    VALUE               :: pc1 
        Real(C_DOUBLE), VALUE               :: aepsge 
        Integer(C_INT),       Intent(INOUT) :: jdgen 
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1451
    End Interface

    Call c_s1451(pc1%cptr, aepsge, c_jdgen, c_jstat)
    
    jdgen = c_jdgen
    jstat = c_jstat

  End Subroutine s1451

!---------------------------------- s1363 -------------------------------------

  Subroutine s1363(curve, startpar, endpar, stat)

!! PURPOSE
!!   s1363 - To pick the parameter range of a curve.

!! INTERFACE
!!   Subroutine s1363(curve, startpar, endpar, stat)
!!     Type(SISLcurve), Intent(IN)    :: curve
!!     Real(REAL64),    Intent(INOUT) :: startpar 
!!     Real(REAL64),    Intent(INOUT) :: endpar 
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: curve
    Real(REAL64),    Intent(INOUT) :: startpar 
    Real(REAL64),    Intent(INOUT) :: endpar 
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_stat

    Interface
      Subroutine c_s1363(curve, startpar, endpar, stat)                        &
                         BIND(C,name="s1363")

        IMPORT :: C_PTR, C_DOUBLE, C_INT
        Implicit NONE

        Type(C_PTR),    VALUE               :: curve
        Real(C_DOUBLE),       Intent(INOUT) :: startpar 
        Real(C_DOUBLE),       Intent(INOUT) :: endpar 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1363
    End Interface

    Call c_s1363(curve%cptr, startpar, endpar, c_stat)

    stat = c_stat

  End Subroutine s1363

!---------------------------------- s1953 -------------------------------------

  Subroutine s1953(curve, point, dim, epsco, epsge, numintpt, intpar,         &
                   numintcu, intcurve, stat)

!! PURPOSE
!!   s1953 - Find the closest points between a curve and a point.

!! INTERFACE
!!   Subroutine s1953(curve, point, dim, epsco, epsge, numintpt, intpar,       &
!!                    numintcu, intcurve, stat)
!!     Type(SISLcurve),                 Intent(IN)    :: curve
!!     Real(REAL64),                    Intent(IN)    :: point(*)
!!     Integer,                         Intent(IN)    :: dim
!!     Real(REAL64),                    Intent(IN)    :: epsco 
!!     Real(REAL64),                    Intent(IN)    :: epsge 
!!     Integer,                         Intent(INOUT) :: numintpt 
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: intpar(:)
!!     Integer,                         Intent(INOUT) :: numintcu 
!!     Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurve(:)
!!     Integer,                         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLcurve),                 Intent(IN)    :: curve
    Real(REAL64),                    Intent(IN)    :: point(*)
    Integer,                         Intent(IN)    :: dim
    Real(REAL64),                    Intent(IN)    :: epsco 
    Real(REAL64),                    Intent(IN)    :: epsge 
    Integer,                         Intent(INOUT) :: numintpt 
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: intpar(:)
    Integer,                         Intent(INOUT) :: numintcu 
    Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurve(:)
    Integer,                         Intent(INOUT) :: stat


    Integer :: i

    Integer(C_INT)        :: c_dim, c_numintpt, c_numintcu, c_stat
    Type(C_PTR)           :: c_intcurve_p, c_intpar_p
    Type(C_PTR),  Pointer :: cintcurve(:)
    Real(REAL64), Pointer :: intparp(:)

    Interface
      Subroutine c_s1953(curve, point, dim, epsco, epsge, numintpt,         &
                   intpar, numintcu, intcurve, stat)                           &
                   BIND(C,name="s1953")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: curve
        Real(C_DOUBLE),       Intent(IN)    :: point(*)
        Integer(C_INT), VALUE               :: dim
        Real(C_DOUBLE), VALUE               :: epsco
        Real(C_DOUBLE), VALUE               :: epsge
        Integer(C_INT),       Intent(INOUT) :: numintpt 
        Type(C_PTR),          Intent(INOUT) :: intpar 
        Integer(C_INT),       Intent(INOUT) :: numintcu 
        Type(C_PTR),          Intent(INOUT) :: intcurve
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1953
    End Interface

    c_dim = dim

    Call c_s1953(curve%cptr, point, c_dim, epsco, epsge, c_numintpt,       &
                 c_intpar_p, c_numintcu,  c_intcurve_p, c_stat)

    numintcu = c_numintcu
    numintpt = c_numintpt
    If (numintpt > 0) Then
      If (C_ASSOCIATED(c_intpar_p)) Then
        Call C_F_Pointer(c_intpar_p, intparp,[numintpt])
        If (ALLOCATED(intpar)) DEALLOCATE(intpar)
        ALLOCATE(intpar(numintpt), SOURCE=0.0_REAL64)
        intpar(:) = intparp(:)
        NULLIFY(intparp)
        Call c_free(c_intpar_p)
      EndIf
    EndIf

    If (numintcu > 0) Then
      If (C_ASSOCIATED(c_intcurve_p)) Then
        Call C_F_Pointer(c_intcurve_p, cintcurve, [numintcu])
        If (ALLOCATED(intcurve)) Deallocate(intcurve)
        ALLOCATE(intcurve(numintcu))
        Do i=1,numintcu
          If (C_ASSOCIATED(cintcurve(i))) Then
            intcurve(i)%cptr = cintcurve(i)
          Else
            intcurve(i)%cptr = C_NULL_PTR
          End If
          Call IntCurveCtoF(intcurve(i))
        EndDo
      EndIf
    EndIf

    If (ASSOCIATED(cintcurve)) NULLIFY(cintcurve)
    c_intcurve_p = C_NULL_PTR

    stat = c_stat

  End Subroutine s1953

!---------------------------------- s1957 -------------------------------------

  Subroutine s1957(pcurve, epoint, idim, aepsco, aepsge, gpar, dist, jstat)

!! PURPOSE
!!   s1957 - Find the closest point between a curve and a point. The method is
!!           fast and should work well in clear cut cases but does not guarantee
!!           finding the right solution. As long as it doesn’t fail, it will
!!           find exactly one point. In other cases, use s1953().

!! INTERFACE
!!   Subroutine s1957(pcurve, epoint, idim, aepsco, aepsge, gpar, dist, jstat)
!!     Type(SISLcurve), Intent(IN)    :: pcurve
!!     Real(REAL64),    Intent(IN)    :: epoint(*)
!!     Integer,         Intent(IN)    :: idim 
!!     Real(REAL64),    Intent(IN)    :: aepsco 
!!     Real(REAL64),    Intent(IN)    :: aepsge
!!     Real(REAL64),    Intent(INOUT) :: gpar
!!     Real(REAL64),    Intent(INOUT) :: dist 
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: pcurve
    Real(REAL64),    Intent(IN)    :: epoint(*)
    Integer,         Intent(IN)    :: idim 
    Real(REAL64),    Intent(IN)    :: aepsco 
    Real(REAL64),    Intent(IN)    :: aepsge
    Real(REAL64),    Intent(INOUT) :: gpar
    Real(REAL64),    Intent(INOUT) :: dist 
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_idim, c_jstat

    Interface
      Subroutine c_s1957(pcurve, epoint, idim, aepsco, aepsge,gpar, dist,      &
                         jstat) BIND(C,name="s1957")

        IMPORT :: C_PTR, C_DOUBLE, C_INT
        Implicit NONE

        Type(C_PTR),    VALUE               :: pcurve
        Real(C_DOUBLE),       Intent(IN)    :: epoint(*)
        Integer(C_INT), VALUE               :: idim
        Real(C_DOUBLE), VALUE               :: aepsco 
        Real(C_DOUBLE), VALUE               :: aepsge 
        Real(C_DOUBLE),       Intent(INOUT) :: gpar 
        Real(C_DOUBLE),       Intent(INOUT) :: dist 
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1957
    End Interface

    c_idim = idim
    Call c_s1957(pcurve%cptr, epoint, c_idim, aepsco, aepsge, gpar, dist,      &
                 c_jstat)

    jstat = c_jstat

  End Subroutine s1957

!---------------------------------- s1774 -------------------------------------

  Subroutine s1774(crv, point, dim, epsge, start, end, guess, clpar, stat)

!! PURPOSE
!!   s1774 - Newton iteration on the distance function between a curve and a
!!           point, to find a closest point or an intersection point. If a bad
!!           choice for the guess parameter is given in, the iteration may end
!!           at a local, not global closest point.

!! INTERFACE
!!   Subroutine s1774(crv, point, dim, epsge, start, end, guess, clpar, stat)
!!     Type(SISLcurve), Intent(IN)    :: crv
!!     Real(REAL64),    Intent(IN)    :: point(*)
!!     Integer,         Intent(IN)    :: dim 
!!     Real(REAL64),    Intent(IN)    :: epsge
!!     Real(REAL64),    Intent(IN)    :: start 
!!     Real(REAL64),    Intent(IN)    :: end 
!!     Real(REAL64),    Intent(IN)    :: guess 
!!     Real(REAL64),    Intent(INOUT) :: clpar
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: crv
    Real(REAL64),    Intent(IN)    :: point(*)
    Integer,         Intent(IN)    :: dim 
    Real(REAL64),    Intent(IN)    :: epsge
    Real(REAL64),    Intent(IN)    :: start 
    Real(REAL64),    Intent(IN)    :: end 
    Real(REAL64),    Intent(IN)    :: guess 
    Real(REAL64),    Intent(INOUT) :: clpar
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_dim, c_stat

    Interface
      Subroutine c_s1774(crv, point, dim, epsge, start, end, guess, clpar,     &
                         stat) BIND(C,name="s1774")

        IMPORT :: C_PTR, C_DOUBLE, C_INT
        Implicit NONE

        Type(C_PTR),    VALUE               :: crv
        Real(C_DOUBLE),       Intent(IN)    :: point(*)
        Integer(C_INT), VALUE               :: dim
        Real(C_DOUBLE), VALUE               :: epsge 
        Real(C_DOUBLE), VALUE               :: start 
        Real(C_DOUBLE), VALUE               :: end 
        Real(C_DOUBLE), VALUE               :: guess 
        Real(C_DOUBLE),       Intent(INOUT) :: clpar 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1774
    End Interface

    c_dim = dim
    Call c_s1774(crv%cptr, point, c_dim, epsge, start, end, guess, clpar,      &
                 c_stat)

    stat = c_stat

  End Subroutine s1774

!---------------------------------- s1955 -------------------------------------

  Subroutine s1955(curve1, curve2, epsco, epsge, numintpt, intpar1, intpar2,   &
                   numintcu, intcurve, stat)

!! PURPOSE
!!   s1955 - Find the closest points between two curves.

!! INTERFACE
!!   Subroutine s1955(curve1, curve2, epsco, epsge, numintpt, intpar1,         &
!!                    intpar2, numintcu, intcurve, stat)
!!     Type(SISLcurve),                 Intent(IN)    :: curve1
!!     Type(SISLcurve),                 Intent(IN)    :: curve2
!!     Real(REAL64),                    Intent(IN)    :: epsco 
!!     Real(REAL64),                    Intent(IN)    :: epsge 
!!     Integer,                         Intent(INOUT) :: numintpt 
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: intpar1(:)
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: intpar2(:)
!!     Integer,                         Intent(INOUT) :: numintcu 
!!     Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurve(:)
!!     Integer,                         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLcurve),                 Intent(IN)    :: curve1
    Type(SISLcurve),                 Intent(IN)    :: curve2
    Real(REAL64),                    Intent(IN)    :: epsco 
    Real(REAL64),                    Intent(IN)    :: epsge 
    Integer,                         Intent(INOUT) :: numintpt 
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: intpar1(:)
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: intpar2(:)
    Integer,                         Intent(INOUT) :: numintcu 
    Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurve(:)
    Integer,                         Intent(INOUT) :: stat


    Integer :: i

    Integer(C_INT)        :: c_numintpt, c_numintcu, c_stat
    Type(C_PTR)           :: c_intcurve_p, c_intpar1_p, c_intpar2_p
    Type(C_PTR),  Pointer :: cintcurve(:)
    Real(REAL64), Pointer :: intpar1p(:)
    Real(REAL64), Pointer :: intpar2p(:)

    Interface
      Subroutine c_s1955(curve1, curve2, epsco, epsge, numintpt, intpar1,      &
                   intpar2, numintcu, intcurve, stat)                          &
                   BIND(C,name="s1955")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: curve1
        Type(C_PTR),    VALUE               :: curve2
        Real(C_DOUBLE), VALUE               :: epsco
        Real(C_DOUBLE), VALUE               :: epsge
        Integer(C_INT),       Intent(INOUT) :: numintpt 
        Type(C_PTR),          Intent(INOUT) :: intpar1 
        Type(C_PTR),          Intent(INOUT) :: intpar2 
        Integer(C_INT),       Intent(INOUT) :: numintcu 
        Type(C_PTR),          Intent(INOUT) :: intcurve
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1955
    End Interface

    Call c_s1955(curve1%cptr, curve2%cptr, epsco, epsge, c_numintpt,           &
                 c_intpar1_p, c_intpar2_p, c_numintcu,  c_intcurve_p, c_stat)

    numintcu = c_numintcu
    numintpt = c_numintpt
    If (numintpt > 0) Then

      If (C_ASSOCIATED(c_intpar1_p)) Then
        Call C_F_Pointer(c_intpar1_p, intpar1p,[numintpt])
        If (ALLOCATED(intpar1)) DEALLOCATE(intpar1)
        ALLOCATE(intpar1(numintpt), SOURCE=0.0_REAL64)
        intpar1(:) = intpar1p(:)
        NULLIFY(intpar1p)
        Call c_free(c_intpar1_p)
      EndIf

      If (C_ASSOCIATED(c_intpar2_p)) Then
        Call C_F_Pointer(c_intpar2_p, intpar2p,[numintpt])
        If (ALLOCATED(intpar2)) DEALLOCATE(intpar2)
        ALLOCATE(intpar2(numintpt), SOURCE=0.0_REAL64)
        intpar2(:) = intpar2p(:)
        NULLIFY(intpar2p)
        Call c_free(c_intpar2_p)
      EndIf

    EndIf

    If (numintcu > 0) Then
      If (C_ASSOCIATED(c_intcurve_p)) Then
        Call C_F_Pointer(c_intcurve_p, cintcurve, [numintcu])
        If (ALLOCATED(intcurve)) Deallocate(intcurve)
        ALLOCATE(intcurve(numintcu))
        Do i=1,numintcu
          If (C_ASSOCIATED(cintcurve(i))) Then
            intcurve(i)%cptr = cintcurve(i)
          Else
            intcurve(i)%cptr = C_NULL_PTR
          End If
          Call IntCurveCtoF(intcurve(i))
        EndDo
      EndIf
    EndIf

    If (ASSOCIATED(cintcurve)) NULLIFY(cintcurve)
    c_intcurve_p = C_NULL_PTR

    stat = c_stat

  End Subroutine s1955

!---------------------------------- s1013 -------------------------------------

  Subroutine s1013(pcurve, ang, ang_tol, guess_par, iter_par, jstat)

!! PURPOSE
!!   s1013 - Find a point on a 2D curve along a given direction.

!! INTERFACE
!!   Subroutine s1013(pcurve, ang, ang_tol, guess_par, iter_par, jstat)
!!     Type(SISLcurve), Intent(IN)    :: pcurve
!!     Real(REAL64),    Intent(IN)    :: ang 
!!     Real(REAL64),    Intent(IN)    :: ang_tol 
!!     Real(REAL64),    Intent(IN)    :: guess_par 
!!     Real(REAL64),    Intent(INOUT) :: iter_par 
!!     Integer,         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: pcurve
    Real(REAL64),    Intent(IN)    :: ang 
    Real(REAL64),    Intent(IN)    :: ang_tol 
    Real(REAL64),    Intent(IN)    :: guess_par 
    Real(REAL64),    Intent(INOUT) :: iter_par 
    Integer,         Intent(INOUT) :: jstat

    Integer(C_INT) :: c_jstat

    Interface
      Subroutine c_s1013(pcurve, ang, ang_tol, guess_par, iter_par, jstat)     &
                         BIND(C,name="s1013")

        IMPORT :: C_PTR, C_DOUBLE, C_INT
        Implicit NONE

        Type(C_PTR),    VALUE               :: pcurve
        Real(C_DOUBLE), VALUE               :: ang 
        Real(C_DOUBLE), VALUE               :: ang_tol 
        Real(C_DOUBLE), VALUE               :: guess_par 
        Real(C_DOUBLE),       Intent(INOUT) :: iter_par 
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1013
    End Interface

    Call c_s1013(pcurve%cptr, ang, ang_tol, guess_par, iter_par, c_jstat)

    jstat = c_jstat

  End Subroutine s1013

!---------------------------------- s1920 -------------------------------------

  Subroutine s1920(curve, dir, dim, epsco, epsge, numintpt, intpar,            &
                   numintcu, intcurve, stat)

!! PURPOSE
!!   s1920 - Find the absolute extremal points/intervals of a curve relative to
!!           a given direction.

!! INTERFACE
!!   Subroutine s1920(curve, dir, dim, epsco, epsge, numintpt, intpar,        &
!!                    numintcu, intcurve, stat)
!!     Type(SISLcurve),                 Intent(IN)    :: curve
!!     Real(REAL64),                    Intent(IN)    :: dir(*)
!!     Integer,                         Intent(IN)    :: dim
!!     Real(REAL64),                    Intent(IN)    :: epsco 
!!     Real(REAL64),                    Intent(IN)    :: epsge 
!!     Integer,                         Intent(INOUT) :: numintpt 
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: intpar(:)
!!     Integer,                         Intent(INOUT) :: numintcu 
!!     Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurve(:)
!!     Integer,                         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLcurve),                 Intent(IN)    :: curve
    Real(REAL64),                    Intent(IN)    :: dir(*)
    Integer,                         Intent(IN)    :: dim
    Real(REAL64),                    Intent(IN)    :: epsco 
    Real(REAL64),                    Intent(IN)    :: epsge 
    Integer,                         Intent(INOUT) :: numintpt 
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: intpar(:)
    Integer,                         Intent(INOUT) :: numintcu 
    Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurve(:)
    Integer,                         Intent(INOUT) :: stat


    Integer :: i

    Integer(C_INT)        :: c_dim, c_numintpt, c_numintcu, c_stat
    Type(C_PTR)           :: c_intcurve_p, c_intpar_p
    Type(C_PTR),  Pointer :: cintcurve(:)
    Real(REAL64), Pointer :: intparp(:)

    Interface
      Subroutine c_s1920(curve, dir, dim, epsco, epsge, numintpt, intpar,      &
                         numintcu, intcurve, stat)                             &
                   BIND(C,name="s1920")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: curve
        Real(C_DOUBLE),       Intent(IN)    :: dir(*)
        Integer(C_INT), VALUE               :: dim
        Real(C_DOUBLE), VALUE               :: epsco
        Real(C_DOUBLE), VALUE               :: epsge
        Integer(C_INT),       Intent(INOUT) :: numintpt 
        Type(C_PTR),          Intent(INOUT) :: intpar 
        Integer(C_INT),       Intent(INOUT) :: numintcu 
        Type(C_PTR),          Intent(INOUT) :: intcurve
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1920
    End Interface

    c_dim = dim

    Call c_s1920(curve%cptr, dir, c_dim, epsco, epsge, c_numintpt,             &
                 c_intpar_p, c_numintcu,  c_intcurve_p, c_stat)

    numintcu = c_numintcu
    numintpt = c_numintpt
    If (numintpt > 0) Then
      If (C_ASSOCIATED(c_intpar_p)) Then
        Call C_F_Pointer(c_intpar_p, intparp,[numintpt])
        If (ALLOCATED(intpar)) DEALLOCATE(intpar)
        ALLOCATE(intpar(numintpt), SOURCE=0.0_REAL64)
        intpar(:) = intparp(:)
        NULLIFY(intparp)
        Call c_free(c_intpar_p)
      EndIf
    EndIf

    If (numintcu > 0) Then
      If (C_ASSOCIATED(c_intcurve_p)) Then
        Call C_F_Pointer(c_intcurve_p, cintcurve, [numintcu])
        If (ALLOCATED(intcurve)) Deallocate(intcurve)
        ALLOCATE(intcurve(numintcu))
        Do i=1,numintcu
          If (C_ASSOCIATED(cintcurve(i))) Then
            intcurve(i)%cptr = cintcurve(i)
          Else
            intcurve(i)%cptr = C_NULL_PTR
          End If
          Call IntCurveCtoF(intcurve(i))
        EndDo
      EndIf
    EndIf

    If (ASSOCIATED(cintcurve)) NULLIFY(cintcurve)
    c_intcurve_p = C_NULL_PTR

    stat = c_stat

  End Subroutine s1920

!---------------------------------- s1241 -------------------------------------

  Subroutine s1241(pcurve, point, dim, epsge, area, stat)

!! PURPOSE
!!   s1241 - To calculate the area between a 2D curve and a 2D point. When
!!           the curve is rotating counter-clockwise around the point, the area
!!           contribution is positive. When the curve is rotating clockwise
!!           around the point, the area contribution is negative. If the curve
!!           is closed or periodic, the area calculated is independent of where
!!           the point is situated. The area is calculated exactly for B-spline
!!           curves, for NURBS the result is an approximation. This routine
!!           will only perform if the order of the curve is less than 7 (can
!!           easily be extended).

!! INTERFACE
!!   Subroutine s1241(pcurve, point, dim, epsge, area, stat)
!!     Type(SISLcurve), Intent(IN)    :: pcurve
!!     Real(REAL64),    Intent(IN)    :: point(*) 
!!     Integer,         Intent(IN)    :: dim 
!!     Real(REAL64),    Intent(IN)    :: epsge 
!!     Real(REAL64),    Intent(INOUT) :: area 
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: pcurve
    Real(REAL64),    Intent(IN)    :: point(*) 
    Integer,         Intent(IN)    :: dim 
    Real(REAL64),    Intent(IN)    :: epsge 
    Real(REAL64),    Intent(INOUT) :: area 
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_dim, c_stat

    Interface
      Subroutine c_s1241(pcurve, point, dim, epsge, area, stat)                &
                         BIND(C,name="s1241")

        IMPORT :: C_PTR, C_DOUBLE, C_INT
        Implicit NONE

        Type(C_PTR),    VALUE               :: pcurve
        Real(C_DOUBLE),       Intent(IN)    :: point(*)
        Integer(C_INT), VALUE               :: dim 
        Real(C_DOUBLE), VALUE               :: epsge 
        Real(C_DOUBLE),       Intent(INOUT) :: area 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1241
    End Interface

    c_dim = dim
    Call c_s1241(pcurve%cptr, point, dim, epsge, area, c_stat)

    stat = c_stat

  End Subroutine s1241

!---------------------------------- s1243 -------------------------------------

  Subroutine s1243(pcurve, point, dim, epsge, weight, area, moment, stat)

!! PURPOSE
!!   s1243 - To calculate the weight point and rotational momentum of an area
!!           between a 2D curve and a 2D point. The area is also calculated.
!!           When the curve is rotating counter-clockwise around the point, the
!!           area contribution is positive. When the curve is rotating clockwise
!!           around the point, the area contribution is negative. OBSERVE:
!!           FOR CALCULATION OF AREA ONLY, USE s1241().

!! INTERFACE
!!   Subroutine s1243(pcurve, point, dim, epsge, weight, area, moment, stat)
!!     Type(SISLcurve), Intent(IN)    :: pcurve
!!     Real(REAL64),    Intent(IN)    :: point(*) 
!!     Integer,         Intent(IN)    :: dim 
!!     Real(REAL64),    Intent(IN)    :: epsge 
!!     Real(REAL64),    Intent(INOUT) :: weight(*) 
!!     Real(REAL64),    Intent(INOUT) :: area 
!!     Real(REAL64),    Intent(INOUT) :: moment 
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLcurve), Intent(IN)    :: pcurve
    Real(REAL64),    Intent(IN)    :: point(*) 
    Integer,         Intent(IN)    :: dim 
    Real(REAL64),    Intent(IN)    :: epsge 
    Real(REAL64),    Intent(INOUT) :: weight(*) 
    Real(REAL64),    Intent(INOUT) :: area 
    Real(REAL64),    Intent(INOUT) :: moment 
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_dim, c_stat

    Interface
      Subroutine c_s1243(pcurve, point, dim, epsge, weight, area, moment,      &
                         stat) BIND(C,name="s1243")

        IMPORT :: C_PTR, C_DOUBLE, C_INT
        Implicit NONE

        Type(C_PTR),    VALUE               :: pcurve
        Real(C_DOUBLE),       Intent(IN)    :: point(*)
        Integer(C_INT), VALUE               :: dim 
        Real(C_DOUBLE), VALUE               :: epsge 
        Real(C_DOUBLE),       Intent(INOUT) :: weight(*)
        Real(C_DOUBLE),       Intent(INOUT) :: area 
        Real(C_DOUBLE),       Intent(INOUT) :: moment 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1243
    End Interface

    c_dim = dim
    Call c_s1243(pcurve%cptr, point, dim, epsge, weight, area, moment,     &
                 c_stat)

    stat = c_stat

  End Subroutine s1243

!---------------------------------- newbox ------------------------------------

  Subroutine newbox(idim, box)

!! PURPOSE
!!   newbox - Create and initialize a curve/surface bounding box instance.

!! INTERFACE
!!   Subroutine newbox(idim, box)
!!     Integer,       Intent(INOUT) :: idim 
!!     Type(SISLbox), Intent(INOUT) :: box

    Implicit NONE

    Integer,       Intent(INOUT) :: idim 
    Type(SISLbox), Intent(INOUT) :: box

    Integer(C_INT) :: c_idim

    Interface
      Function c_newbox(idim) BIND(C, name="newbox")

        IMPORT :: C_PTR, C_INT
        Implicit NONE

        Integer(C_INT), VALUE :: idim
        Type(C_PTR)           :: c_newbox

      End Function c_newbox
    End Interface

    c_idim   = idim
    box%cptr = c_newbox(c_idim)

    Call boxCtoF(idim, box)
  
  End Subroutine newbox

!---------------------------------- s1988 -------------------------------------

  Subroutine s1988(pc, emax, emin, jstat)

!! PURPOSE
!!   s1988 - Find the bounding box of a SISLCurve. NB. The geometric
!!           bounding box is returned also in the rational case, that is the
!!           box in homogenous coordinates is NOT computed

!! INTERFACE
!!   Subroutine s1988(pc, emax, emin, jstat)
!!     Type(SISLcurve),                 Intent(IN)    :: pc 
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: emax(:)
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: emin(:)
!!     Integer,                         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLcurve),                 Intent(IN)    :: pc 
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: emax(:)
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: emin(:)
    Integer,                         Intent(INOUT) :: jstat

    Integer(C_INT)        :: c_jstat
    Type(C_PTR)           :: c_emax_p, c_emin_p
    Real(REAL64), Pointer :: emaxp(:)
    Real(REAL64), Pointer :: eminp(:)

    Integer :: idim

    Interface
      Subroutine c_s1988(pc, emax, emin, jstat)                                &
                         BIND(C,name="s1988")

        IMPORT :: C_PTR, C_INT
        Implicit NONE

        Type(C_PTR),    VALUE               :: pc 
        Type(C_PTR),          Intent(INOUT) :: emax 
        Type(C_PTR),          Intent(INOUT) :: emin 
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1988
    End Interface

    Call c_s1988(pc%cptr, c_emax_p, c_emin_p, c_jstat) 

    idim = pc%idim

    If (C_ASSOCIATED(c_emax_p)) Then
      Call C_F_Pointer(c_emax_p, emaxp,[idim])
      If (ALLOCATED(emax)) DEALLOCATE(emax)
      ALLOCATE(emax(idim), SOURCE=0.0_REAL64)
      emax(:) = emaxp(:)
      NULLIFY(emaxp)
      Call c_free(c_emax_p)
    EndIf

    If (C_ASSOCIATED(c_emin_p)) Then
      Call C_F_Pointer(c_emin_p, eminp,[idim])
      If (ALLOCATED(emin)) DEALLOCATE(emin)
      ALLOCATE(emin(idim), SOURCE=0.0_REAL64)
      emin(:) = eminp(:)
      NULLIFY(eminp)
      Call c_free(c_emin_p)
    EndIf

    jstat = c_jstat

  End Subroutine s1988

!---------------------------------- newdir ------------------------------------
  Subroutine newdir(idim, dir)

!! PURPOSE
!!   newdir - Create and initialize a curve/surface direction instance

!! INTERFACE
!!   Subroutine newdir(idim, dir)
!!     Integer,       Intent(IN)    :: idim
!!     Type(SISLdir), Intent(INOUT) :: dir

    Implicit NONE

    Integer,       Intent(IN)    :: idim
    Type(SISLdir), Intent(INOUT) :: dir

    Integer(C_INT) :: c_idim

    Interface

      Function c_newdir(idim) BIND(C,name="newdir")

        IMPORT :: C_PTR, C_INT
        Implicit NONE

        Integer(C_INT), VALUE :: idim
        Type(C_PTR)           :: c_newdir

      End Function c_newdir 
    End Interface

    c_idim   = idim
    dir%cptr = c_newdir(idim)

    Call dirCtoF(idim, dir)

  End Subroutine newdir

!---------------------------------- s1986 -------------------------------------

  Subroutine s1986(pc, aepsge, jgtpi, gaxis, cang, jstat)

!! PURPOSE
!!   s1986 - Find the direction cone of a curve.

!! INTERFACE
!!   Subroutine s1986(pc, aepsge, jgtpi, gaxis, cang, jstat)
!!     Type(SISLcurve),                 Intent(IN)    :: pc 
!!     Real(REAL64),                    Intent(IN)    :: aepsge
!!     Integer,                         Intent(INOUT) :: jgtpi
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: gaxis(:)
!!     Real(REAL64),                    Intent(INOUT) :: cang 
!!     Integer,                         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLcurve),                 Intent(IN)    :: pc 
    Real(REAL64),                    Intent(IN)    :: aepsge
    Integer,                         Intent(INOUT) :: jgtpi
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: gaxis(:)
    Real(REAL64),                    Intent(INOUT) :: cang 
    Integer,                         Intent(INOUT) :: jstat

    Integer(C_INT)        :: c_jstat, c_jgtpi
    Type(C_PTR)           :: c_gaxis_p
    Real(REAL64), Pointer :: gaxisp(:)
    Real(C_DOUBLE)        :: c_cang

    Integer :: idim

    Interface
      Subroutine c_s1986(pc, aepsge, jgtpi, gaxis, cang, jstat)                &
                         BIND(C,name="s1986")

        IMPORT :: C_PTR, C_DOUBLE, C_INT
        Implicit NONE

        Type(C_PTR),    VALUE               :: pc 
        Real(C_DOUBLE), VALUE               :: aepsge
        Integer(C_INT),       Intent(INOUT) :: jgtpi
        Type(C_PTR),          Intent(INOUT) :: gaxis
        Real(C_DOUBLE),       Intent(INOUT) :: cang 
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1986
    End Interface

    Call c_s1986(pc%cptr, aepsge, c_jgtpi, c_gaxis_p, c_cang, c_jstat) 

    idim  = pc%idim
    jgtpi = c_jgtpi
    If (jgtpi == 0) Then
      If (C_ASSOCIATED(c_gaxis_p)) Then
        Call C_F_Pointer(c_gaxis_p, gaxisp,[idim])
        If (ALLOCATED(gaxis)) DEALLOCATE(gaxis)
        ALLOCATE(gaxis(idim), SOURCE=0.0_REAL64)
        gaxis(:) = gaxisp(:)
        NULLIFY(gaxisp)
        Call c_free(c_gaxis_p)
      EndIf
      cang = c_cang
    Else
      cang = 0.0_REAL64
    EndIf 

    jstat = c_jstat

  End Subroutine s1986

End Module Curve_Interrogation
