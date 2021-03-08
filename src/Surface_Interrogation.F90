   
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

Module Surface_Interrogation

!! Module Surface_Interrogation contains Modern Fortran C-interoperability
!! routines for the C functions described in Chapter 7 of version 4.4 of
!! the SISL reference manual

!! Written by: Richard Weed, Ph.D.
!! Version no. 1.0 - Feb. 2021
!!   Initial development version

!! Contact: rweedmsu@gmail.com

  USE forSISLdata, ONLY: REAL64, C_INT, C_DOUBLE, C_PTR, C_NULL_PTR, c_free,   &
                         C_ASSOCIATED, C_F_POINTER, SISLsurf, SISLcurve,       &
                         SISLIntcurve, IntCurveCtoF

  USE Curve_Utilities, ONLY: freeCurve

  Implicit NONE

  PRIVATE :: REAL64, C_INT, C_DOUBLE, C_PTR, C_NULL_PTR, c_free, C_ASSOCIATED, &
             C_F_POINTER, SISLsurf, SISLcurve, SISLIntcurve, freeCurve,        &
             IntCurveCtoF

Contains

!-------------------------------- newIntCurve ---------------------------------

  Subroutine newIntCurve(numgdpt, numpar1, numpar2, guidepar1, guidepar2,      &
                         itype, intCurve)

!! PURPOSE
!!   newIntcurve - Create and initialize a SISLIntcurve-instance. Note that thes
!!   arrays guidepar1 and guidepar2 will be freed by freeIntcurve. In most
!!   cases the SISLIntcurve objects will be generated internally in the
!!   SISL intersection routines.

!! INTERFACE
!!   Subroutine newIntCurve(numgdpt, numpar1, numpar2, guidepar1, guidepar2,   &
!!                          itype, IntCurve)
!!     Integer,                    Intent(IN)    :: numgdpt 
!!     Integer,                    Intent(IN)    :: numpar1
!!     Integer,                    Intent(IN)    :: numpar2
!!     Real(REAL64),               Intent(IN)    :: guidepar1(*)
!!     Real(REAL64),               Intent(IN)    :: guidepar2(*)
!!     Integer,                    Intent(IN)    :: itype 
!!     Type(SISLIntcurve), TARGET, Intent(INOUT) :: intCurve

    Implicit NONE

    Integer,                    Intent(IN)    :: numgdpt 
    Integer,                    Intent(IN)    :: numpar1
    Integer,                    Intent(IN)    :: numpar2
    Real(REAL64),               Intent(IN)    :: guidepar1(*)
    Real(REAL64),               Intent(IN)    :: guidepar2(*)
    Integer,                    Intent(IN)    :: itype 
    Type(SISLIntcurve), TARGET, Intent(INOUT) :: intCurve

    Integer(C_INT) :: c_numgdpt, c_numpar1, c_numpar2, c_itype

    Interface

      Function c_newIntcurve(numgdpt, numpar1, numpar2, guidepar1, guidepar2,  &
                             itype)                                            &
                             BIND(C,name="newIntcurve")

        IMPORT :: C_INT, C_DOUBLE, C_PTR

        Implicit NONE

        Integer(C_INT), VALUE             :: numgdpt 
        Integer(C_INT), VALUE             :: numpar1 
        Integer(C_INT), VALUE             :: numpar2 
        Real(C_DOUBLE),        Intent(IN) :: guidepar1(*)
        Real(C_DOUBLE),        Intent(IN) :: guidepar2(*)
        Integer(C_INT), VALUE             :: itype 
        Type(C_PTR)                        :: c_newIntcurve

      End Function c_newIntcurve
    End Interface

    c_numgdpt = numgdpt
    c_numpar1 = numpar1 
    c_numpar2 = numpar2 
    c_itype   = itype
    intCurve%cptr = c_newIntCurve(c_numgdpt, c_numpar1, c_numpar2,             &
                                  guidepar1, guidepar2, c_itype)

    Call IntcurveCtoF(intCurve)

  End Subroutine newIntcurve

!------------------------------- freeIntCurve ---------------------------------

  Subroutine freeIntCurve(intCurve)

!! PURPOSE
!!   freeIntcurve - Free the space occupied by a SISLIntcurve.
!!   Note that the arrays guidepar1 and guidepar2 will be freed as well.

!! INTERFACE
!!   Subroutine freeIntCurve(intCurve)
!!     Type(SISLIntCurve), Target, Intent(INOUT) :: intCurve

    Implicit NONE

    Type(SISLIntCurve), Target, Intent(INOUT) :: intCurve

    Interface
      Subroutine c_freeIntcurve(IntCurve) BIND(C, name="freeIntcurve")

        IMPORT :: C_PTR
        Implicit NONE

        Type(C_PTR), VALUE :: IntCurve

      End Subroutine
    End Interface

    If (C_ASSOCIATED(IntCurve%cptr)) Then

      IntCurve%ipoint = 0 
      IntCurve%ipar1  = 0 
      IntCurve%ipar2  = 0 
      IntCurve%itype  = 0 
      IntCurve%pretop = 0 

      If (ASSOCIATED(IntCurve%epar1)) NULLIFY(IntCurve%epar1) 
      If (ASSOCIATED(IntCurve%epar2)) NULLIFY(IntCurve%epar2) 
      Call freeCurve(IntCurve%pgeom, free_cptr=.FALSE.) 
      Call freeCurve(IntCurve%ppar1, free_cptr=.FALSE.) 
      Call freeCurve(IntCurve%ppar2, free_cptr=.FALSE.) 

      Call c_freeIntCurve(IntCurve%cptr)
    End If

  End Subroutine freeIntcurve

!------------------------------- freeIntCrvlist -------------------------------

  Subroutine freeIntCrvlist(intCurve, icrv)

!! PURPOSE
!!   freeIntcrvlist - Free a list of SISLIntcurve.

!! INTERFACE
!!   Subroutine freeIntCrvlist(IntCurve, icrv)
!!     Type(SISLIntCurve), Target, Intent(INOUT) :: IntCurve(*)
!!     Integer,                    Intent(IN)    :: icrv

    Implicit NONE

    Type(SISLIntCurve), Target, Intent(INOUT) :: intCurve(*)
    Integer,                    Intent(IN)    :: icrv

    Integer :: i

    Do i=1,icrv
      Call freeIntCurve(intCurve(i))
    EndDo

  End Subroutine freeIntcrvlist
! S1850 is same routine as in Curve_Interrogation module
! S1371 is same routine as in Curve_Interrogation module

!---------------------------------- s1372 -------------------------------------

  Subroutine s1372(curve, point, dir, radius, dim, epsco, epsge, numintpt,     &
                   intpar, numintcu, intcurve, stat)

!! PURPOSE
!!   s1372 - Find all the intersections between a curve and a cylinder.

!! INTERFACE
!!   Subroutine s1372(curve, point, dir, radius, dim, epsco, epsge, numintpt, &
!!                    intpar, numintcu, intcurve, stat)
!!     Type(SISLcurve),                 Intent(IN)    :: curve
!!     Real(REAL64),                    Intent(IN)    :: point(*)
!!     Real(REAL64),                    Intent(IN)    :: dir(*)
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
    Real(REAL64),                    Intent(IN)    :: point(*)
    Real(REAL64),                    Intent(IN)    :: dir(*)
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
      Subroutine c_s1372(curve, point, dir, radius, dim, epsco, epsge,         &
                   numintpt, intpar, numintcu, intcurve, stat)                 &
                   BIND(C,name="s1372")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: curve
        Real(C_DOUBLE),       Intent(IN)    :: point(*)
        Real(C_DOUBLE),       Intent(IN)    :: dir(*)
        Real(C_DOUBLE), VALUE               :: radius
        Integer(C_INT), VALUE               :: dim
        Real(C_DOUBLE), VALUE               :: epsco
        Real(C_DOUBLE), VALUE               :: epsge
        Integer(C_INT),       Intent(INOUT) :: numintpt
        Type(C_PTR),          Intent(INOUT) :: intpar
        Integer(C_INT),       Intent(INOUT) :: numintcu
        Type(C_PTR),          Intent(INOUT) :: intcurve
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1372
    End Interface

    c_dim = dim

    Call c_s1372(curve%cptr, point, dir, radius, c_dim, epsco, epsge,      &
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
          Call IntCurveCtoF(intcurve(i))
        EndDo
      EndIf
    EndIf

    If (ASSOCIATED(cintcurve)) NULLIFY(cintcurve)
    c_intcurve_p = C_NULL_PTR

    stat = c_stat

  End Subroutine s1372

!---------------------------------- s1373 -------------------------------------

  Subroutine s1373(curve, top, axispt, conept, dim, epsco, epsge, numintpt,    &
                   intpar, numintcu, intcurve, stat)

!! PURPOSE
!!   s1373 - Find all the intersections between a curve and a cone.

!! INTERFACE
!!   Subroutine s1373(curve, top, axispt, conept, dim, epsco, epsge, numintpt, &
!!                    intpar, numintcu, intcurve, stat)
!!     Type(SISLcurve),                 Intent(IN)    :: curve
!!     Real(REAL64),                    Intent(IN)    :: top(*)
!!     Real(REAL64),                    Intent(IN)    :: axispt(*)
!!     Real(REAL64),                    Intent(IN)    :: conept(*) 
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
    Real(REAL64),                    Intent(IN)    :: top(*)
    Real(REAL64),                    Intent(IN)    :: axispt(*)
    Real(REAL64),                    Intent(IN)    :: conept(*) 
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
      Subroutine c_s1373(curve, top, axispt, conept, dim, epsco, epsge,        &
                   numintpt, intpar, numintcu, intcurve, stat)                 &
                   BIND(C,name="s1373")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: curve
        Real(C_DOUBLE),       Intent(IN)    :: top(*)
        Real(C_DOUBLE),       Intent(IN)    :: axispt(*)
        Real(C_DOUBLE),       Intent(IN)    :: conept(*)
        Integer(C_INT), VALUE               :: dim
        Real(C_DOUBLE), VALUE               :: epsco
        Real(C_DOUBLE), VALUE               :: epsge
        Integer(C_INT),       Intent(INOUT) :: numintpt
        Type(C_PTR),          Intent(INOUT) :: intpar
        Integer(C_INT),       Intent(INOUT) :: numintcu
        Type(C_PTR),          Intent(INOUT) :: intcurve
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1373
    End Interface

    c_dim = dim

    Call c_s1373(curve%cptr, top, axispt, conept, c_dim, epsco, epsge,         &
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
          Call IntCurveCtoF(intcurve(i))
        EndDo
      EndIf
    EndIf

    If (ASSOCIATED(cintcurve)) NULLIFY(cintcurve)
    c_intcurve_p = C_NULL_PTR

    stat = c_stat

  End Subroutine s1373

!---------------------------------- s1502 -------------------------------------

  Subroutine s1502(curve, basept, normdir, ellipaxis, alpha, ratio, dim,       &
                   epsco, epsge, numintpt, intpar, numintcu, intcurve, stat)

!! PURPOSE
!!   s1502 - Find all the intersections between a curve and an elliptic cone.

!! INTERFACE
!!   Subroutine s1502(curve, basept, normdir, ellipaxis, alpha, ratio, dim,  &
!!                    epsco, epsge, numintpt, intpar, numintcu, intcurve,    &
!!                    stat)
!!     Type(SISLcurve),                 Intent(IN)    :: curve
!!     Real(REAL64),                    Intent(IN)    :: basept(*)
!!     Real(REAL64),                    Intent(IN)    :: normdir(*)
!!     Real(REAL64),                    Intent(IN)    :: ellipaxis(*) 
!!     Real(REAL64),                    Intent(IN)    :: alpha
!!     Real(REAL64),                    Intent(IN)    :: ratio
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
    Real(REAL64),                    Intent(IN)    :: basept(*)
    Real(REAL64),                    Intent(IN)    :: normdir(*)
    Real(REAL64),                    Intent(IN)    :: ellipaxis(*) 
    Real(REAL64),                    Intent(IN)    :: alpha
    Real(REAL64),                    Intent(IN)    :: ratio
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
      Subroutine c_s1502(curve, basept, normdir, ellipaxis, alpha, ratio,      &
                         dim, epsco, epsge, numintpt, intpar, numintcu,        &
                         intcurve, stat)                                       &
                         BIND(C,name="s1502")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: curve
        Real(C_DOUBLE),       Intent(IN)    :: basept(*)
        Real(C_DOUBLE),       Intent(IN)    :: normdir(*)
        Real(C_DOUBLE),       Intent(IN)    :: ellipaxis(*)
        Real(C_DOUBLE), VALUE               :: alpha
        Real(C_DOUBLE), VALUE               :: ratio
        Integer(C_INT), VALUE               :: dim
        Real(C_DOUBLE), VALUE               :: epsco
        Real(C_DOUBLE), VALUE               :: epsge
        Integer(C_INT),       Intent(INOUT) :: numintpt
        Type(C_PTR),          Intent(INOUT) :: intpar
        Integer(C_INT),       Intent(INOUT) :: numintcu
        Type(C_PTR),          Intent(INOUT) :: intcurve
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1502
    End Interface

    c_dim = dim

    Call c_s1502(curve%cptr, basept, normdir, ellipaxis, alpha, ratio,     &
                c_dim, epsco, epsge, c_numintpt, c_intpar_p, c_numintcu,       &
                c_intcurve_p, c_stat)

    numintcu = c_numintcu
    numintpt = c_numintpt
    If (numintpt > 0) Then
      If (C_ASSOCIATED(c_intpar_p)) Then
        Call C_F_Pointer(c_intpar_p, intparp, [numintpt])
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

  End Subroutine s1502

!---------------------------------- s1375 -------------------------------------

  Subroutine s1375(curve, centre, normal, centdis, rad, dim, epsco, epsge,     &
                   numintpt, intpar, numintcu, intcurve, stat)

!! PURPOSE
!!   s1375 - Find all the intersections between a curve and a torus

!! INTERFACE
!!   Subroutine s1375(curve, centre, normal, centdis, rad, dim, epsco, epsge, &
!!                    numintpt, intpar, numintcu, intcurve, stat)
!!     Type(SISLcurve),                 Intent(IN)    :: curve
!!     Real(REAL64),                    Intent(IN)    :: centre(*)
!!     Real(REAL64),                    Intent(IN)    :: normal(*)
!!     Real(REAL64),                    Intent(IN)    :: centdis
!!     Real(REAL64),                    Intent(IN)    :: rad
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
    Real(REAL64),                    Intent(IN)    :: normal(*)
    Real(REAL64),                    Intent(IN)    :: centdis
    Real(REAL64),                    Intent(IN)    :: rad
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
      Subroutine c_s1375(curve, centre, normal, centdis, rad, dim, epsco,      &
                         epsge, numintpt, intpar, numintcu, intcurve, stat)    &
                         BIND(C,name="s1375")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: curve
        Real(C_DOUBLE),       Intent(IN)    :: centre(*)
        Real(C_DOUBLE),       Intent(IN)    :: normal(*)
        Real(C_DOUBLE), VALUE               :: centdis
        Real(C_DOUBLE), VALUE               :: rad
        Integer(C_INT), VALUE               :: dim
        Real(C_DOUBLE), VALUE               :: epsco
        Real(C_DOUBLE), VALUE               :: epsge
        Integer(C_INT),       Intent(INOUT) :: numintpt
        Type(C_PTR),          Intent(INOUT) :: intpar
        Integer(C_INT),       Intent(INOUT) :: numintcu
        Type(C_PTR),          Intent(INOUT) :: intcurve
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1375
    End Interface

    c_dim = dim

    Call c_s1375(curve%cptr, centre, normal, centdis, rad, c_dim, epsco,   &
                 epsge, c_numintpt, c_intpar_p, c_numintcu, c_intcurve_p,      &
                 c_stat)

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

  End Subroutine s1375

!---------------------------------- s1870 -------------------------------------

  Subroutine s1870(ps1, pt1, idim, aepsge, jpt, gpar1, jcrv, wcurve, jstat)

!! PURPOSE
!!   s1870 - Find all intersections between a surface and a point.

!! INTERFACE
!!   Subroutine s1870(ps1, pt1, idim, aepsge, jpt, gpar1, jcrv, wcurve, jstat)
!!     Type(SISLsurf),                  Intent(IN)    :: ps1
!!     Real(REAL64),                    Intent(IN)    :: pt1(*)
!!     Integer,                         Intent(IN)    :: idim
!!     Real(REAL64),                    Intent(IN)    :: aepsge
!!     Integer,                         Intent(INOUT) :: jpt
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: gpar1(:)
!!     Integer,                         Intent(INOUT) :: jcrv
!!     Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: wcurve(:)
!!     Integer,                         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLsurf),                  Intent(IN)    :: ps1
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
      Subroutine c_s1870(ps1, pt1, idim, aepsge, jpt, gpar1, jcrv, wcurve,     &
                         jstat) BIND(C,name="s1870")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: ps1
        Real(C_DOUBLE),       Intent(IN)    :: pt1(*)
        Integer(C_INT), VALUE               :: idim
        Real(C_DOUBLE), VALUE               :: aepsge
        Integer(C_INT),       Intent(INOUT) :: jpt
        Type(C_PTR),          Intent(INOUT) :: gpar1
        Integer(C_INT),       Intent(INOUT) :: jcrv
        Type(C_PTR),          Intent(INOUT) :: wcurve
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1870
    End Interface

    c_idim = idim

    Call c_s1870(ps1%cptr, pt1, c_idim, aepsge, c_jpt, c_gpar1_p, c_jcrv,   &
                 c_wcurve_p, c_jstat)

    jcrv = c_jcrv
    jpt  = c_jpt
    If (jpt > 0) Then
      If (C_ASSOCIATED(c_gpar1_p)) Then
        Call C_F_Pointer(c_gpar1_p, gpar1p,[jpt*2])
        If (ALLOCATED(gpar1)) DEALLOCATE(gpar1)
        ALLOCATE(gpar1(jpt*2), SOURCE=0.0_REAL64)
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
          Call IntCurveCtoF(wcurve(i))
        EndDo
      EndIf
    EndIf

    If (ASSOCIATED(cwcurve)) NULLIFY(cwcurve)
    c_wcurve_p = C_NULL_PTR

    jstat = c_jstat

  End Subroutine s1870

!---------------------------------- s1856 -------------------------------------

  Subroutine s1856(surf, point, linedir, dim, epsco, epsge, numintpt,          &
                   pointpar, numintcr, intcurves, stat)

!! PURPOSE
!!   s1856 - Find all intersections between a tensor-product surface and an
!!           infinite straight line.

!! INTERFACE
!!   Subroutine s1856(surf, point, linedir, dim, epsco, epsge, numintpt,      &
!!                    pointpar, numintcr, intcurves, stat)
!!     Type(SISLsurf),                  Intent(IN)    :: surf 
!!     Real(REAL64),                    Intent(IN)    :: point(*)
!!     Real(REAL64),                    Intent(IN)    :: linedir(*)
!!     Integer,                         Intent(IN)    :: dim
!!     Real(REAL64),                    Intent(IN)    :: epsco
!!     Real(REAL64),                    Intent(IN)    :: epsge
!!     Integer,                         Intent(INOUT) :: numintpt
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: pointpar(:)
!!     Integer,                         Intent(INOUT) :: numintcr
!!     Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurves(:)
!!     Integer,                         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf),                  Intent(IN)    :: surf 
    Real(REAL64),                    Intent(IN)    :: point(*)
    Real(REAL64),                    Intent(IN)    :: linedir(*)
    Integer,                         Intent(IN)    :: dim
    Real(REAL64),                    Intent(IN)    :: epsco
    Real(REAL64),                    Intent(IN)    :: epsge
    Integer,                         Intent(INOUT) :: numintpt
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: pointpar(:)
    Integer,                         Intent(INOUT) :: numintcr
    Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurves(:)
    Integer,                         Intent(INOUT) :: stat

    Integer :: i

    Integer(C_INT)        :: c_dim, c_numintpt, c_numintcr, c_stat
    Type(C_PTR)           :: c_intcurves_p, c_pointpar_p
    Type(C_PTR),  Pointer :: cintcurves(:)
    Real(REAL64), Pointer :: pointparp(:)

    Interface
      Subroutine c_s1856(surf, point, linedir, dim, epsco, epsge, numintpt,    &
                         pointpar, numintcr, intcurves, stat)                  &
                         BIND(C,name="s1856")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf 
        Real(C_DOUBLE),       Intent(IN)    :: point(*)
        Real(C_DOUBLE),       Intent(IN)    :: linedir(*)
        Integer(C_INT), VALUE               :: dim
        Real(C_DOUBLE), VALUE               :: epsco
        Real(C_DOUBLE), VALUE               :: epsge
        Integer(C_INT),       Intent(INOUT) :: numintpt
        Type(C_PTR),          Intent(INOUT) :: pointpar
        Integer(C_INT),       Intent(INOUT) :: numintcr
        Type(C_PTR),          Intent(INOUT) :: intcurves
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1856
    End Interface

    c_dim = dim

    Call c_s1856(surf%cptr, point, linedir, c_dim, epsco, epsge,            &
                 c_numintpt, c_pointpar_p, c_numintcr,  c_intcurves_p, c_stat)

    numintcr = c_numintcr
    numintpt = c_numintpt
    If (numintpt > 0) Then
      If (C_ASSOCIATED(c_pointpar_p)) Then
        Call C_F_Pointer(c_pointpar_p, pointparp,[numintpt*2])
        If (ALLOCATED(pointpar)) DEALLOCATE(pointpar)
        ALLOCATE(pointpar(numintpt*2), SOURCE=0.0_REAL64)
        pointpar(:) = pointparp(:)
        NULLIFY(pointparp)
        Call c_free(c_pointpar_p)
      EndIf
    EndIf

    If (numintcr > 0) Then
      If (C_ASSOCIATED(c_intcurves_p)) Then
        Call C_F_Pointer(c_intcurves_p, cintcurves, [numintcr])
        If (ALLOCATED(intcurves)) Deallocate(intcurves)
        ALLOCATE(intcurves(numintcr))
        Do i=1,numintcr
          If (C_ASSOCIATED(cintcurves(i))) Then
            intcurves(i)%cptr = cintcurves(i)
          Else
            intcurves(i)%cptr = C_NULL_PTR
          End If
          Call IntCurveCtoF(intcurves(i))
        EndDo
      EndIf
    EndIf

    If (ASSOCIATED(cintcurves)) NULLIFY(cintcurves)
    c_intcurves_p = C_NULL_PTR

    stat = c_stat

  End Subroutine s1856

!---------------------------------- s1518 -------------------------------------

  Subroutine s1518(surf, point, dir, epsge, start, end, parin, parout, stat)

!! PURPOSE
!!   s1518 - Newton iteration on the intersection between a 3D NURBS surface
!!           and a line. If a good initial guess is given, the intersection will
!!           be found quickly. However if a bad initial guess is given, the 
!!           iteration might not converge. We only search in the rectangular
!!           subdomain specified by ”start” and ”end”. This can be the whole
!!           domain if desired.

!! INTERFACE
!!   Subroutine s1518(surf, point, dir, epsge, start, end, parin, parout, stat)
!!     Type(SISLsurf),  Intent(IN)    :: surf
!!     Real(REAL64),    Intent(IN)    :: point(*)
!!     Real(REAL64),    Intent(IN)    :: dir(*)
!!     Real(REAL64),    Intent(IN)    :: epsge
!!     Real(REAL64),    Intent(IN)    :: start(*)
!!     Real(REAL64),    Intent(IN)    :: end(*)
!!     Real(REAL64),    Intent(IN)    :: parin(*)
!!     Real(REAL64),    Intent(INOUT) :: parout(*) 
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf),  Intent(IN)    :: surf
    Real(REAL64),    Intent(IN)    :: point(*)
    Real(REAL64),    Intent(IN)    :: dir(*)
    Real(REAL64),    Intent(IN)    :: epsge
    Real(REAL64),    Intent(IN)    :: start(*)
    Real(REAL64),    Intent(IN)    :: end(*)
    Real(REAL64),    Intent(IN)    :: parin(*)
    Real(REAL64),    Intent(INOUT) :: parout(*) 
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_stat

    Interface
      Subroutine c_s1518(surf, point, dir, epsge, start, end, parin, parout,   &
                         stat) BIND(C,name="s1518")

        IMPORT :: C_PTR, C_DOUBLE, C_INT
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf
        Real(C_DOUBLE),       Intent(IN)    :: point(*)
        Real(C_DOUBLE),       Intent(IN)    :: dir(*)
        Real(C_DOUBLE), VALUE               :: epsge
        Real(C_DOUBLE),       Intent(IN)    :: start(*)
        Real(C_DOUBLE),       Intent(IN)    :: end(*)
        Real(C_DOUBLE),       Intent(IN)    :: parin(*)
        Real(C_DOUBLE),       Intent(INOUT) :: parout(*)
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1518
    End Interface

    parout(1:2) = 0.0_REAL64
    Call c_s1518(surf%cptr, point, dir, epsge, start, end, parin,           &
                 parout, c_stat)

    stat = c_stat

  End Subroutine s1518

!---------------------------------- 1328 -------------------------------------

  Subroutine s1328(psold, epoint, enorm1, enorm2, idim, rsnew, jstat)

!! PURPOSE
!!   s1328 - Put the equation of the surface pointed at by psold into two planes
!!           given by the point epoint and the normals enorm1 and enorm2.
!!           The result is an equation where the new two-dimensional surface
!!           rsnew is to be equal to origo.

!! INTERFACE
!!   Subroutine s1328(psold, epoint, enorm1, enorm2, idim, rsnew, jstat)
!!     Type(SISLsurf), Intent(IN)    :: psold
!!     Real(REAL64),   Intent(IN)    :: epoint(*)
!!     Real(REAL64),   Intent(IN)    :: enorm1(*)
!!     Real(REAL64),   Intent(IN)    :: enorm2(*)
!!     Integer,        Intent(IN)    :: idim
!!     Type(SISLsurf), Intent(INOUT) :: rsnew
!!     Integer,        Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLsurf), Intent(IN)    :: psold
    Real(REAL64),   Intent(IN)    :: epoint(*)
    Real(REAL64),   Intent(IN)    :: enorm1(*)
    Real(REAL64),   Intent(IN)    :: enorm2(*)
    Integer,        Intent(IN)    :: idim
    Type(SISLsurf), Intent(INOUT) :: rsnew
    Integer,        Intent(INOUT) :: jstat

    Integer(C_INT)  :: c_idim, c_jstat

    Interface
    Subroutine c_s1328(psold, epoint, enorm1, enorm2, idim, rsnew, jstat)      &
                       BIND(C,name="s1328")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: psold
        Real(C_DOUBLE),       Intent(IN)    :: epoint(*)
        Real(C_DOUBLE),       Intent(IN)    :: enorm1(*)
        Real(C_DOUBLE),       Intent(IN)    :: enorm2(*)
        Integer(C_INT), VALUE               :: idim
        Type(C_PTR),          Intent(INOUT) :: rsnew
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1328
    End Interface

    c_idim = idim
    Call c_s1328(psold%cptr, epoint, enorm1, enorm2, c_idim, rsnew%cptr,       &
                 c_jstat)

    jstat = c_jstat

  End Subroutine s1328

!---------------------------------- s1855 -------------------------------------

  Subroutine s1855(surf, centre, radius, normal, dim, epsco, epsge, numintpt,  &
                   pointpar, numintcr, intcurves, stat)

!! PURPOSE
!!   s1855 - Find all intersections between a tensor-product surface and a full
!!           circle.

!! INTERFACE
!!   Subroutine s1855(surf, centre, radius, normal, dim, epsco, epsge,         &
!!                    numintpt, pointpar, numintcr, intcurves, stat)
!!     Type(SISLsurf),                  Intent(IN)    :: surf 
!!     Real(REAL64),                    Intent(IN)    :: centre(*)
!!     Real(REAL64),                    Intent(IN)    :: radius 
!!     Real(REAL64),                    Intent(IN)    :: normal(*)
!!     Integer,                         Intent(IN)    :: dim
!!     Real(REAL64),                    Intent(IN)    :: epsco
!!     Real(REAL64),                    Intent(IN)    :: epsge
!!     Integer,                         Intent(INOUT) :: numintpt
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: pointpar(:)
!!     Integer,                         Intent(INOUT) :: numintcr
!!     Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurves(:)
!!     Integer,                         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf),                  Intent(IN)    :: surf 
    Real(REAL64),                    Intent(IN)    :: centre(*)
    Real(REAL64),                    Intent(IN)    :: radius 
    Real(REAL64),                    Intent(IN)    :: normal(*)
    Integer,                         Intent(IN)    :: dim
    Real(REAL64),                    Intent(IN)    :: epsco
    Real(REAL64),                    Intent(IN)    :: epsge
    Integer,                         Intent(INOUT) :: numintpt
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: pointpar(:)
    Integer,                         Intent(INOUT) :: numintcr
    Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurves(:)
    Integer,                         Intent(INOUT) :: stat

    Integer :: i 

    Integer(C_INT)        :: c_dim, c_numintpt, c_numintcr, c_stat
    Type(C_PTR)           :: c_intcurves_p, c_pointpar_p
    Type(C_PTR),  Pointer :: cintcurves(:)
    Real(REAL64), Pointer :: pointparp(:)

    Interface
      Subroutine c_s1855(surf, centre, radius, normal, dim, epsco, epsge,      &
                         numintpt, pointpar, numintcr, intcurves, stat)        &
                         BIND(C,name="s1855")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf 
        Real(C_DOUBLE),       Intent(IN)    :: centre(*)
        Real(C_DOUBLE), VALUE               :: radius 
        Real(C_DOUBLE),       Intent(IN)    :: normal(*)
        Integer(C_INT), VALUE               :: dim
        Real(C_DOUBLE), VALUE               :: epsco
        Real(C_DOUBLE), VALUE               :: epsge
        Integer(C_INT),       Intent(INOUT) :: numintpt
        Type(C_PTR),          Intent(INOUT) :: pointpar
        Integer(C_INT),       Intent(INOUT) :: numintcr
        Type(C_PTR),          Intent(INOUT) :: intcurves
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1855
    End Interface

    c_dim = dim

    Call c_s1855(surf%cptr, centre, radius, normal, c_dim, epsco, epsge,    &
                 c_numintpt, c_pointpar_p, c_numintcr,  c_intcurves_p, c_stat)

    numintcr = c_numintcr
    numintpt = c_numintpt
    If (numintpt > 0) Then
      If (C_ASSOCIATED(c_pointpar_p)) Then
        Call C_F_Pointer(c_pointpar_p, pointparp,[numintpt*2])
        If (ALLOCATED(pointpar)) DEALLOCATE(pointpar)
        ALLOCATE(pointpar(numintpt*2), SOURCE=0.0_REAL64)
        pointpar(:) = pointparp(:)
        NULLIFY(pointparp)
        Call c_free(c_pointpar_p)
      EndIf
    EndIf

    If (numintcr > 0) Then
      If (C_ASSOCIATED(c_intcurves_p)) Then
        Call C_F_Pointer(c_intcurves_p, cintcurves, [numintcr])
        If (ALLOCATED(intcurves)) Deallocate(intcurves)
        ALLOCATE(intcurves(numintcr))
        Do i=1,numintcr
          If (C_ASSOCIATED(cintcurves(i))) Then
            intcurves(i)%cptr = cintcurves(i)
          Else
            intcurves(i)%cptr = C_NULL_PTR
          End If
        EndDo
        Call IntCurveCtoF(intcurves(i))
      EndIf
    EndIf

    If (ASSOCIATED(cintcurves)) NULLIFY(cintcurves)
    c_intcurves_p = C_NULL_PTR

    stat = c_stat

  End Subroutine s1855

!---------------------------------- s1858 -------------------------------------

  Subroutine s1858(surf, curve, epsco, epsge, numintpt, pointpar1, pointpar2,  &
                   numintcr, intcurves, stat)

!! PURPOSE
!!   s1858 - Find all intersections between a surface and a curve. Intersection
!!           curves are described by guide points. To pick the intersection
!!           curves use s1712

!! INTERFACE
!!   Subroutine s1858(surf, curve, epsco, epsge, numintpt, pointpar1,         &
!!                    pointpar2, numintcr, intcurves, stat)
!!     Type(SISLsurf),                  Intent(IN)    :: surf 
!!     Type(SISLcurve),                 Intent(IN)    :: curve 
!!     Real(REAL64),                    Intent(IN)    :: epsco
!!     Real(REAL64),                    Intent(IN)    :: epsge
!!     Integer,                         Intent(INOUT) :: numintpt
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: pointpar1(:)
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: pointpar2(:)
!!     Integer,                         Intent(INOUT) :: numintcr
!!     Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurves(:)
!!     Integer,                         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf),                  Intent(IN)    :: surf 
    Type(SISLcurve),                 Intent(IN)    :: curve 
    Real(REAL64),                    Intent(IN)    :: epsco
    Real(REAL64),                    Intent(IN)    :: epsge
    Integer,                         Intent(INOUT) :: numintpt
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: pointpar1(:)
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: pointpar2(:)
    Integer,                         Intent(INOUT) :: numintcr
    Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurves(:)
    Integer,                         Intent(INOUT) :: stat


    Integer :: i

    Integer(C_INT)        :: c_numintpt, c_numintcr, c_stat
    Type(C_PTR)           :: c_intcurves_p, c_pointpar1_p, c_pointpar2_p
    Type(C_PTR),  Pointer :: cintcurves(:)
    Real(REAL64), Pointer :: pointparp1(:)
    Real(REAL64), Pointer :: pointparp2(:)

    Interface
      Subroutine c_s1858(surf, curve, epsco, epsge, numintpt, pointpar1,       &
                         pointpar2, numintcr, intcurves, stat)                 &
                         BIND(C,name="s1858")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf 
        Type(C_PTR),    VALUE               :: curve 
        Real(C_DOUBLE), VALUE               :: epsco
        Real(C_DOUBLE), VALUE               :: epsge
        Integer(C_INT),       Intent(INOUT) :: numintpt
        Type(C_PTR),          Intent(INOUT) :: pointpar1
        Type(C_PTR),          Intent(INOUT) :: pointpar2
        Integer(C_INT),       Intent(INOUT) :: numintcr
        Type(C_PTR),          Intent(INOUT) :: intcurves
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1858
    End Interface

    Call c_s1858(surf%cptr, curve%cptr, epsco, epsge, c_numintpt,              &
                 c_pointpar1_p, c_pointpar2_p, c_numintcr,  c_intcurves_p,     &
                 c_stat)

    numintcr = c_numintcr
    numintpt = c_numintpt
    If (numintpt > 0) Then

      If (C_ASSOCIATED(c_pointpar1_p)) Then
        Call C_F_Pointer(c_pointpar1_p, pointparp1,[numintpt*2])
        If (ALLOCATED(pointpar1)) DEALLOCATE(pointpar1)
        ALLOCATE(pointpar1(numintpt*2), SOURCE=0.0_REAL64)
        pointpar1(:) = pointparp1(:)
        NULLIFY(pointparp1)
        Call c_free(c_pointpar1_p)
      EndIf

      If (C_ASSOCIATED(c_pointpar2_p)) Then
        Call C_F_Pointer(c_pointpar2_p, pointparp2,[numintpt])
        If (ALLOCATED(pointpar2)) DEALLOCATE(pointpar2)
        ALLOCATE(pointpar2(numintpt), SOURCE=0.0_REAL64)
        pointpar2(:) = pointparp2(:)
        NULLIFY(pointparp2)
        Call c_free(c_pointpar2_p)
      EndIf

    EndIf

    If (numintcr > 0) Then

      If (C_ASSOCIATED(c_intcurves_p)) Then
        Call C_F_Pointer(c_intcurves_p, cintcurves, [numintcr])
        If (ALLOCATED(intcurves)) Deallocate(intcurves)
        ALLOCATE(intcurves(numintcr))
        Do i=1,numintcr
          If (C_ASSOCIATED(cintcurves(i))) Then
            intcurves(i)%cptr = cintcurves(i)
          Else
            intcurves(i)%cptr = C_NULL_PTR
          End If
          Call IntCurveCtoF(intcurves(i))
        EndDo
      EndIf
    EndIf

    If (ASSOCIATED(cintcurves)) NULLIFY(cintcurves)
    c_intcurves_p = C_NULL_PTR

    stat = c_stat

  End Subroutine s1858

!---------------------------------- s1851 -------------------------------------

  Subroutine s1851(surf, point, normal, dim, epsco, epsge, numintpt,          &
                   pointpar, numintcr, intcurves, stat)

!! PURPOSE
!!   s1851 - Find all intersections between a tensor-product surface and a
!!           plane. Intersection curves are described by guide points. To make
!!           the intersection curves use s1314.

!! INTERFACE
!!   Subroutine s1851(surf, point, normal, dim, epsco, epsge, numintpt,       &
!!                    pointpar, numintcr, intcurves, stat)
!!     Type(SISLsurf),                  Intent(IN)    :: surf 
!!     Real(REAL64),                    Intent(IN)    :: point(*)
!!     Real(REAL64),                    Intent(IN)    :: normal(*)
!!     Integer,                         Intent(IN)    :: dim
!!     Real(REAL64),                    Intent(IN)    :: epsco
!!     Real(REAL64),                    Intent(IN)    :: epsge
!!     Integer,                         Intent(INOUT) :: numintpt
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: pointpar(:)
!!     Integer,                         Intent(INOUT) :: numintcr
!!     Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurves(:)
!!     Integer,                         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf),                  Intent(IN)    :: surf 
    Real(REAL64),                    Intent(IN)    :: point(*)
    Real(REAL64),                    Intent(IN)    :: normal(*)
    Integer,                         Intent(IN)    :: dim
    Real(REAL64),                    Intent(IN)    :: epsco
    Real(REAL64),                    Intent(IN)    :: epsge
    Integer,                         Intent(INOUT) :: numintpt
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: pointpar(:)
    Integer,                         Intent(INOUT) :: numintcr
    Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurves(:)
    Integer,                         Intent(INOUT) :: stat

    Integer :: i

    Integer(C_INT)        :: c_dim, c_numintpt, c_numintcr, c_stat
    Type(C_PTR)           :: c_intcurves_p, c_pointpar_p
    Type(C_PTR),  Pointer :: cintcurves(:)
    Real(REAL64), Pointer :: pointparp(:)

    Interface
      Subroutine c_s1851(surf, point, normal, dim, epsco, epsge, numintpt,    &
                         pointpar, numintcr, intcurves, stat)                  &
                         BIND(C,name="s1851")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf 
        Real(C_DOUBLE),       Intent(IN)    :: point(*)
        Real(C_DOUBLE),       Intent(IN)    :: normal(*)
        Integer(C_INT), VALUE               :: dim
        Real(C_DOUBLE), VALUE               :: epsco
        Real(C_DOUBLE), VALUE               :: epsge
        Integer(C_INT),       Intent(INOUT) :: numintpt
        Type(C_PTR),          Intent(INOUT) :: pointpar
        Integer(C_INT),       Intent(INOUT) :: numintcr
        Type(C_PTR),          Intent(INOUT) :: intcurves
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1851
    End Interface

    c_dim = dim

    Call c_s1851(surf%cptr, point, normal, c_dim, epsco, epsge,                &
                 c_numintpt, c_pointpar_p, c_numintcr,  c_intcurves_p, c_stat)

    numintcr = c_numintcr
    numintpt = c_numintpt
    If (numintpt > 0) Then
      If (C_ASSOCIATED(c_pointpar_p)) Then
        Call C_F_Pointer(c_pointpar_p, pointparp,[numintpt*2])
        If (ALLOCATED(pointpar)) DEALLOCATE(pointpar)
        ALLOCATE(pointpar(numintpt*2), SOURCE=0.0_REAL64)
        pointpar(:) = pointparp(:)
        NULLIFY(pointparp)
        Call c_free(c_pointpar_p)
      EndIf
    EndIf

    If (numintcr > 0) Then
      If (C_ASSOCIATED(c_intcurves_p)) Then
        Call C_F_Pointer(c_intcurves_p, cintcurves, [numintcr])
        If (ALLOCATED(intcurves)) Deallocate(intcurves)
        ALLOCATE(intcurves(numintcr))
        Do i=1,numintcr
          If (C_ASSOCIATED(cintcurves(i))) Then
            intcurves(i)%cptr = cintcurves(i)
          Else
            intcurves(i)%cptr = C_NULL_PTR
          End If
          Call IntCurveCtoF(intcurves(i))
        EndDo
      EndIf
    EndIf

    If (ASSOCIATED(cintcurves)) NULLIFY(cintcurves)
    c_intcurves_p = C_NULL_PTR

    stat = c_stat

  End Subroutine s1851

!---------------------------------- s1852 -------------------------------------

  Subroutine s1852(surf, centre, radius, dim, epsco, epsge, numintpt,          &
                   pointpar, numintcr, intcurves, stat)

!! PURPOSE
!!   s1852 - Find all intersections between a tensor-product surface and a
!!           sphere. Intersection curves are described by guide points. To pro-
!!           duce the intersection curves use s1315.

!! INTERFACE
!!   Subroutine s1852(surf, centre, radius, dim, epsco, epsge, numintpt,      &
!!                    pointpar, numintcr, intcurves, stat)
!!     Type(SISLsurf),                  Intent(IN)    :: surf 
!!     Real(REAL64),                    Intent(IN)    :: centre(*)
!!     Real(REAL64),                    Intent(IN)    :: radius
!!     Integer,                         Intent(IN)    :: dim
!!     Real(REAL64),                    Intent(IN)    :: epsco
!!     Real(REAL64),                    Intent(IN)    :: epsge
!!     Integer,                         Intent(INOUT) :: numintpt
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: pointpar(:)
!!     Integer,                         Intent(INOUT) :: numintcr
!!     Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurves(:)
!!     Integer,                         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf),                  Intent(IN)    :: surf 
    Real(REAL64),                    Intent(IN)    :: centre(*)
    Real(REAL64),                    Intent(IN)    :: radius
    Integer,                         Intent(IN)    :: dim
    Real(REAL64),                    Intent(IN)    :: epsco
    Real(REAL64),                    Intent(IN)    :: epsge
    Integer,                         Intent(INOUT) :: numintpt
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: pointpar(:)
    Integer,                         Intent(INOUT) :: numintcr
    Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurves(:)
    Integer,                         Intent(INOUT) :: stat

    Integer :: i

    Integer(C_INT)        :: c_dim, c_numintpt, c_numintcr, c_stat
    Type(C_PTR)           :: c_intcurves_p, c_pointpar_p
    Type(C_PTR),  Pointer :: cintcurves(:)
    Real(REAL64), Pointer :: pointparp(:)

    Interface
      Subroutine c_s1852(surf, centre, radius, dim, epsco, epsge, numintpt,    &
                         pointpar, numintcr, intcurves, stat)                  &
                         BIND(C,name="s1852")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf 
        Real(C_DOUBLE),       Intent(IN)    :: centre(*)
        Real(C_DOUBLE), VALUE               :: radius
        Integer(C_INT), VALUE               :: dim
        Real(C_DOUBLE), VALUE               :: epsco
        Real(C_DOUBLE), VALUE               :: epsge
        Integer(C_INT),       Intent(INOUT) :: numintpt
        Type(C_PTR),          Intent(INOUT) :: pointpar
        Integer(C_INT),       Intent(INOUT) :: numintcr
        Type(C_PTR),          Intent(INOUT) :: intcurves
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1852
    End Interface

    c_dim = dim

    Call c_s1852(surf%cptr, centre, radius, c_dim, epsco, epsge,               &
                 c_numintpt, c_pointpar_p, c_numintcr,  c_intcurves_p, c_stat)

    numintcr = c_numintcr
    numintpt = c_numintpt
    If (numintpt > 0) Then
      If (C_ASSOCIATED(c_pointpar_p)) Then
        Call C_F_Pointer(c_pointpar_p, pointparp,[numintpt*2])
        If (ALLOCATED(pointpar)) DEALLOCATE(pointpar)
        ALLOCATE(pointpar(numintpt*2), SOURCE=0.0_REAL64)
        pointpar(:) = pointparp(:)
        NULLIFY(pointparp)
        Call c_free(c_pointpar_p)
      EndIf
    EndIf

    If (numintcr > 0) Then
      If (C_ASSOCIATED(c_intcurves_p)) Then
        Call C_F_Pointer(c_intcurves_p, cintcurves, [numintcr])
        If (ALLOCATED(intcurves)) Deallocate(intcurves)
        ALLOCATE(intcurves(numintcr))
        Do i=1,numintcr
          If (C_ASSOCIATED(cintcurves(i))) Then
            intcurves(i)%cptr = cintcurves(i)
          Else
            intcurves(i)%cptr = C_NULL_PTR
          End If
          Call IntCurveCtoF(intcurves(i))
        EndDo
      EndIf
    EndIf

    If (ASSOCIATED(cintcurves)) NULLIFY(cintcurves)
    c_intcurves_p = C_NULL_PTR

    stat = c_stat

  End Subroutine s1852

!---------------------------------- s1853 -------------------------------------

  Subroutine s1853(surf, point, cyldir, radius, dim, epsco, epsge, numintpt,   &
                   pointpar, numintcr, intcurve, stat)

!! PURPOSE
!!   s1853 - Find all intersections between a tensor-product surface and a
!!           cylinder. Intersection curves are described by guide points. To
!!           produce the intersection curves use s1316.

!! INTERFACE
!!   Subroutine s1853(surf, point, cyldir, radius, dim, epsco, epsge, numintpt,&
!!                    pointpar, numintcr, intcurve, stat)
!!     Type(SISLsurf),                  Intent(IN)    :: surf
!!     Real(REAL64),                    Intent(IN)    :: point(*)
!!     Real(REAL64),                    Intent(IN)    :: cyldir(*)
!!     Real(REAL64),                    Intent(IN)    :: radius 
!!     Integer,                         Intent(IN)    :: dim
!!     Real(REAL64),                    Intent(IN)    :: epsco
!!     Real(REAL64),                    Intent(IN)    :: epsge
!!     Integer,                         Intent(INOUT) :: numintpt
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: pointpar(:)
!!     Integer,                         Intent(INOUT) :: numintcr
!!     Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurve(:)
!!     Integer,                         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf),                  Intent(IN)    :: surf
    Real(REAL64),                    Intent(IN)    :: point(*)
    Real(REAL64),                    Intent(IN)    :: cyldir(*)
    Real(REAL64),                    Intent(IN)    :: radius 
    Integer,                         Intent(IN)    :: dim
    Real(REAL64),                    Intent(IN)    :: epsco
    Real(REAL64),                    Intent(IN)    :: epsge
    Integer,                         Intent(INOUT) :: numintpt
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: pointpar(:)
    Integer,                         Intent(INOUT) :: numintcr
    Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurve(:)
    Integer,                         Intent(INOUT) :: stat

    Integer :: i

    Integer(C_INT)        :: c_dim, c_numintpt, c_numintcr, c_stat
    Type(C_PTR)           :: c_intcurve_p, c_pointpar_p
    Type(C_PTR),  Pointer :: cintcurve(:)
    Real(REAL64), Pointer :: pointparp(:)

    Interface
      Subroutine c_s1853(surf, point, cyldir, radius, dim, epsco, epsge,       &
                   numintpt, pointpar, numintcr, intcurve, stat)               &
                   BIND(C,name="s1853")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf
        Real(C_DOUBLE),       Intent(IN)    :: point(*)
        Real(C_DOUBLE),       Intent(IN)    :: cyldir(*)
        Real(C_DOUBLE), VALUE               :: radius
        Integer(C_INT), VALUE               :: dim
        Real(C_DOUBLE), VALUE               :: epsco
        Real(C_DOUBLE), VALUE               :: epsge
        Integer(C_INT),       Intent(INOUT) :: numintpt
        Type(C_PTR),          Intent(INOUT) :: pointpar
        Integer(C_INT),       Intent(INOUT) :: numintcr
        Type(C_PTR),          Intent(INOUT) :: intcurve
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1853
    End Interface

    c_dim = dim

    Call c_s1853(surf%cptr, point, cyldir, radius, c_dim, epsco, epsge,     &
                 c_numintpt, c_pointpar_p, c_numintcr,  c_intcurve_p, c_stat)

    numintcr = c_numintcr
    numintpt = c_numintpt
    If (numintpt > 0) Then
      If (C_ASSOCIATED(c_pointpar_p)) Then
        Call C_F_Pointer(c_pointpar_p, pointparp,[numintpt*2])
        If (ALLOCATED(pointpar)) DEALLOCATE(pointpar)
        ALLOCATE(pointpar(numintpt*2), SOURCE=0.0_REAL64)
        pointpar(:) = pointparp(:)
        NULLIFY(pointparp)
        Call c_free(c_pointpar_p)
      EndIf
    EndIf

    If (numintcr > 0) Then
      If (C_ASSOCIATED(c_intcurve_p)) Then
        Call C_F_Pointer(c_intcurve_p, cintcurve, [numintcr])
        If (ALLOCATED(intcurve)) Deallocate(intcurve)
        ALLOCATE(intcurve(numintcr))
        Do i=1,numintcr
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

  End Subroutine s1853

!---------------------------------- s1854 -------------------------------------

  Subroutine s1854(surf, toppt, axispt, conept, dim, epsco, epsge, numintpt,   &
                   pointpar, numintcr, intcurve, stat)

!! PURPOSE
!!   s1854 - Find all intersections between a tensor-product surface and a cone.
!!           Intersection curves are described by guide points. To produce the
!!           intersection curves use s1317.

!! INTERFACE
!!   Subroutine s1854(surf, toppt, axispt, conept, dim, epsco, epsge, numintpt,&
!!                    pointpar, numintcr, intcurve, stat)
!!     Type(SISLsurf),                  Intent(IN)    :: surf
!!     Real(REAL64),                    Intent(IN)    :: toppt(*)
!!     Real(REAL64),                    Intent(IN)    :: axispt(*)
!!     Real(REAL64),                    Intent(IN)    :: conept(*) 
!!     Integer,                         Intent(IN)    :: dim
!!     Real(REAL64),                    Intent(IN)    :: epsco
!!     Real(REAL64),                    Intent(IN)    :: epsge
!!     Integer,                         Intent(INOUT) :: numintpt
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: pointpar(:)
!!     Integer,                         Intent(INOUT) :: numintcr
!!     Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurve(:)
!!     Integer,                         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf),                  Intent(IN)    :: surf
    Real(REAL64),                    Intent(IN)    :: toppt(*)
    Real(REAL64),                    Intent(IN)    :: axispt(*)
    Real(REAL64),                    Intent(IN)    :: conept(*) 
    Integer,                         Intent(IN)    :: dim
    Real(REAL64),                    Intent(IN)    :: epsco
    Real(REAL64),                    Intent(IN)    :: epsge
    Integer,                         Intent(INOUT) :: numintpt
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: pointpar(:)
    Integer,                         Intent(INOUT) :: numintcr
    Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurve(:)
    Integer,                         Intent(INOUT) :: stat

    Integer :: i

    Integer(C_INT)        :: c_dim, c_numintpt, c_numintcr, c_stat
    Type(C_PTR)           :: c_intcurve_p, c_pointpar_p
    Type(C_PTR),  Pointer :: cintcurve(:)
    Real(REAL64), Pointer :: pointparp(:)

    Interface
      Subroutine c_s1854(surf, toppt, axispt, conept, dim, epsco, epsge,       &
                   numintpt, pointpar, numintcr, intcurve, stat)               &
                   BIND(C,name="s1854")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf 
        Real(C_DOUBLE),       Intent(IN)    :: toppt(*)
        Real(C_DOUBLE),       Intent(IN)    :: axispt(*)
        Real(C_DOUBLE),       Intent(IN)    :: conept(*)
        Integer(C_INT), VALUE               :: dim
        Real(C_DOUBLE), VALUE               :: epsco
        Real(C_DOUBLE), VALUE               :: epsge
        Integer(C_INT),       Intent(INOUT) :: numintpt
        Type(C_PTR),          Intent(INOUT) :: pointpar
        Integer(C_INT),       Intent(INOUT) :: numintcr
        Type(C_PTR),          Intent(INOUT) :: intcurve
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1854
    End Interface

    c_dim = dim

    Call c_s1854(surf%cptr, toppt, axispt, conept, c_dim, epsco, epsge,     &
                 c_numintpt, c_pointpar_p, c_numintcr,  c_intcurve_p, c_stat)

    numintcr = c_numintcr
    numintpt = c_numintpt
    If (numintpt > 0) Then
      If (C_ASSOCIATED(c_pointpar_p)) Then
        Call C_F_Pointer(c_pointpar_p, pointparp,[numintpt*2])
        If (ALLOCATED(pointpar)) DEALLOCATE(pointpar)
        ALLOCATE(pointpar(numintpt*2), SOURCE=0.0_REAL64)
        pointpar(:) = pointparp(:)
        NULLIFY(pointparp)
        Call c_free(c_pointpar_p)
      EndIf
    EndIf

    If (numintcr > 0) Then
      If (C_ASSOCIATED(c_intcurve_p)) Then
        Call C_F_Pointer(c_intcurve_p, cintcurve, [numintcr])
        If (ALLOCATED(intcurve)) Deallocate(intcurve)
        ALLOCATE(intcurve(numintcr))
        Do i=1,numintcr
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

  End Subroutine s1854

!---------------------------------- s1503 -------------------------------------

  Subroutine s1503(surf, basept, normdir, ellipaxis, alpha, ratio, dim,        &
                   epsco, epsge, numintpt, pointpar, numintcr, intcurve, stat)

!! PURPOSE
!!   s1503 - Find all intersections between a tensor-product surface and an
!!           elliptic cone. Intersection curves are described by guide points.
!!           To produce the intersection curves use s1501.

!! INTERFACE
!!   Subroutine s1503(surf, basept, normdir, ellipaxis, alpha, ratio, dim,    &
!!                    epsco, epsge, numintpt, pointpar, numintcr, intcurve,   &
!!                    stat)
!!     Type(SISLsurf),                  Intent(IN)    :: surf 
!!     Real(REAL64),                    Intent(IN)    :: basept(*)
!!     Real(REAL64),                    Intent(IN)    :: normdir(*)
!!     Real(REAL64),                    Intent(IN)    :: ellipaxis(*) 
!!     Real(REAL64),                    Intent(IN)    :: alpha
!!     Real(REAL64),                    Intent(IN)    :: ratio
!!     Integer,                         Intent(IN)    :: dim
!!     Real(REAL64),                    Intent(IN)    :: epsco
!!     Real(REAL64),                    Intent(IN)    :: epsge
!!     Integer,                         Intent(INOUT) :: numintpt
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: pointpar(:)
!!     Integer,                         Intent(INOUT) :: numintcr
!!     Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurve(:)

    Implicit NONE

    Type(SISLsurf),                  Intent(IN)    :: surf 
    Real(REAL64),                    Intent(IN)    :: basept(*)
    Real(REAL64),                    Intent(IN)    :: normdir(*)
    Real(REAL64),                    Intent(IN)    :: ellipaxis(*) 
    Real(REAL64),                    Intent(IN)    :: alpha
    Real(REAL64),                    Intent(IN)    :: ratio
    Integer,                         Intent(IN)    :: dim
    Real(REAL64),                    Intent(IN)    :: epsco
    Real(REAL64),                    Intent(IN)    :: epsge
    Integer,                         Intent(INOUT) :: numintpt
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: pointpar(:)
    Integer,                         Intent(INOUT) :: numintcr
    Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurve(:)
    Integer,                         Intent(INOUT) :: stat


    Integer :: i

    Integer(C_INT)        :: c_dim, c_numintpt, c_numintcr, c_stat
    Type(C_PTR)           :: c_intcurve_p, c_pointpar_p
    Type(C_PTR),  Pointer :: cintcurve(:)
    Real(REAL64), Pointer :: pointparp(:)

    Interface
      Subroutine c_s1503(surf, basept, normdir, ellipaxis, alpha, ratio,       &
                         dim, epsco, epsge, numintpt, pointpar, numintcr,      &
                         intcurve, stat)                                       &
                         BIND(C,name="s1503")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf 
        Real(C_DOUBLE),       Intent(IN)    :: basept(*)
        Real(C_DOUBLE),       Intent(IN)    :: normdir(*)
        Real(C_DOUBLE),       Intent(IN)    :: ellipaxis(*)
        Real(C_DOUBLE), VALUE               :: alpha
        Real(C_DOUBLE), VALUE               :: ratio
        Integer(C_INT), VALUE               :: dim
        Real(C_DOUBLE), VALUE               :: epsco
        Real(C_DOUBLE), VALUE               :: epsge
        Integer(C_INT),       Intent(INOUT) :: numintpt
        Type(C_PTR),          Intent(INOUT) :: pointpar
        Integer(C_INT),       Intent(INOUT) :: numintcr
        Type(C_PTR),          Intent(INOUT) :: intcurve
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1503
    End Interface

    c_dim = dim

    Call c_s1503(surf%cptr, basept, normdir, ellipaxis, alpha, ratio,       &
                c_dim, epsco, epsge, c_numintpt, c_pointpar_p, c_numintcr,     &
                c_intcurve_p, c_stat)

    numintcr = c_numintcr
    numintpt = c_numintpt
    If (numintpt > 0) Then
      If (C_ASSOCIATED(c_pointpar_p)) Then
        Call C_F_Pointer(c_pointpar_p, pointparp,[numintpt*2])
        If (ALLOCATED(pointpar)) DEALLOCATE(pointpar)
        ALLOCATE(pointpar(numintpt*2), SOURCE=0.0_REAL64)
        pointpar(:) = pointparp(:)
        NULLIFY(pointparp)
        Call c_free(c_pointpar_p)
      EndIf
    EndIf

    If (numintcr > 0) Then
      If (C_ASSOCIATED(c_intcurve_p)) Then
        Call C_F_Pointer(c_intcurve_p, cintcurve, [numintcr])
        If (ALLOCATED(intcurve)) Deallocate(intcurve)
        ALLOCATE(intcurve(numintcr))
        Do i=1,numintcr
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

  End Subroutine s1503

!---------------------------------- s1369 -------------------------------------

  Subroutine s1369(surf, centre, normal, cendis, rad, dim, epsco, epsge,       &
                   numintpt, pointpar, numintcr, intcurve, stat)

!! PURPOSE
!!   s1369 - Find all intersections between a surface and a torus. Intersection
!!           curves are described by guide points. To produce the intersection
!!           curves use s1318.

!! INTERFACE
!!   Subroutine s1369(surf, centre, normal, cendis, rad, dim, epsco, epsge,   &
!!                    numintpt, pointpar, numintcr, intcurve, stat)
!!     Type(SISLsurf),                  Intent(IN)    :: surf 
!!     Real(REAL64),                    Intent(IN)    :: centre(*)
!!     Real(REAL64),                    Intent(IN)    :: normal(*)
!!     Real(REAL64),                    Intent(IN)    :: cendis
!!     Real(REAL64),                    Intent(IN)    :: rad
!!     Integer,                         Intent(IN)    :: dim
!!     Real(REAL64),                    Intent(IN)    :: epsco
!!     Real(REAL64),                    Intent(IN)    :: epsge
!!     Integer,                         Intent(INOUT) :: numintpt
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: pointpar(:)
!!     Integer,                         Intent(INOUT) :: numintcr
!!     Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurve(:)
!!     Integer,                         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf),                  Intent(IN)    :: surf 
    Real(REAL64),                    Intent(IN)    :: centre(*)
    Real(REAL64),                    Intent(IN)    :: normal(*)
    Real(REAL64),                    Intent(IN)    :: cendis
    Real(REAL64),                    Intent(IN)    :: rad
    Integer,                         Intent(IN)    :: dim
    Real(REAL64),                    Intent(IN)    :: epsco
    Real(REAL64),                    Intent(IN)    :: epsge
    Integer,                         Intent(INOUT) :: numintpt
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: pointpar(:)
    Integer,                         Intent(INOUT) :: numintcr
    Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurve(:)
    Integer,                         Intent(INOUT) :: stat


    Integer :: i

    Integer(C_INT)        :: c_dim, c_numintpt, c_numintcr, c_stat
    Type(C_PTR)           :: c_intcurve_p, c_pointpar_p
    Type(C_PTR),  Pointer :: cintcurve(:)
    Real(REAL64), Pointer :: pointparp(:)

    Interface
      Subroutine c_s1369(surf, centre, normal, cendis, rad, dim, epsco,        &
                         epsge, numintpt, pointpar, numintcr, intcurve, stat)  &
                         BIND(C,name="s1369")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf 
        Real(C_DOUBLE),       Intent(IN)    :: centre(*)
        Real(C_DOUBLE),       Intent(IN)    :: normal(*)
        Real(C_DOUBLE), VALUE               :: cendis
        Real(C_DOUBLE), VALUE               :: rad
        Integer(C_INT), VALUE               :: dim
        Real(C_DOUBLE), VALUE               :: epsco
        Real(C_DOUBLE), VALUE               :: epsge
        Integer(C_INT),       Intent(INOUT) :: numintpt
        Type(C_PTR),          Intent(INOUT) :: pointpar
        Integer(C_INT),       Intent(INOUT) :: numintcr
        Type(C_PTR),          Intent(INOUT) :: intcurve
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1369
    End Interface

    c_dim = dim

    Call c_s1369(surf%cptr, centre, normal, cendis, rad, c_dim, epsco,         &
                 epsge, c_numintpt, c_pointpar_p, c_numintcr, c_intcurve_p,    &
                 c_stat)

    numintcr = c_numintcr
    numintpt = c_numintpt
    If (numintpt > 0) Then
      If (C_ASSOCIATED(c_pointpar_p)) Then
        Call C_F_Pointer(c_pointpar_p, pointparp,[numintpt*2])
        If (ALLOCATED(pointpar)) DEALLOCATE(pointpar)
        ALLOCATE(pointpar(numintpt*2), SOURCE=0.0_REAL64)
        pointpar(:) = pointparp(:)
        NULLIFY(pointparp)
        Call c_free(c_pointpar_p)
      EndIf
    EndIf

    If (numintcr > 0) Then
      If (C_ASSOCIATED(c_intcurve_p)) Then
        Call C_F_Pointer(c_intcurve_p, cintcurve, [numintcr])
        If (ALLOCATED(intcurve)) Deallocate(intcurve)
        ALLOCATE(intcurve(numintcr))
        Do i=1,numintcr
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

  End Subroutine s1369

!---------------------------------- s1859 -------------------------------------

  Subroutine s1859(surf1, surf2, epsco, epsge, numintpt, pointpar1, pointpar2, &
                   numintcr, intcurves, stat)

!! PURPOSE
!!   s1859 - Find all intersections between two surfaces. Intersection curves
!!           are described by guide points. To produce the intersection curves
!!           use s1310.

!! INTERFACE
!!   Subroutine s1859(surf1, surf2, epsco, epsge, numintpt, pointpar1,        &
!!                    pointpar2, numintcr, intcurves, stat)
!!     Type(SISLsurf),                  Intent(IN)    :: surf1
!!     Type(SISLsurf),                  Intent(IN)    :: surf2 
!!     Real(REAL64),                    Intent(IN)    :: epsco
!!     Real(REAL64),                    Intent(IN)    :: epsge
!!     Integer,                         Intent(INOUT) :: numintpt
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: pointpar1(:)
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: pointpar2(:)
!!     Integer,                         Intent(INOUT) :: numintcr
!!     Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurves(:)
!!     Integer,                         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf),                  Intent(IN)    :: surf1
    Type(SISLsurf),                  Intent(IN)    :: surf2 
    Real(REAL64),                    Intent(IN)    :: epsco
    Real(REAL64),                    Intent(IN)    :: epsge
    Integer,                         Intent(INOUT) :: numintpt
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: pointpar1(:)
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: pointpar2(:)
    Integer,                         Intent(INOUT) :: numintcr
    Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: intcurves(:)
    Integer,                         Intent(INOUT) :: stat

    Integer :: i

    Integer(C_INT)        :: c_numintpt, c_numintcr, c_stat
    Type(C_PTR)           :: c_intcurves_p, c_pointpar1_p, c_pointpar2_p
    Type(C_PTR),  Pointer :: cintcurves(:)
    Real(REAL64), Pointer :: pointparp1(:)
    Real(REAL64), Pointer :: pointparp2(:)

    Interface
      Subroutine c_s1859(surf1, surf2, epsco, epsge, numintpt, pointpar1,      &
                         pointpar2, numintcr, intcurves, stat)                 &
                         BIND(C,name="s1859")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf1 
        Type(C_PTR),    VALUE               :: surf2 
        Real(C_DOUBLE), VALUE               :: epsco
        Real(C_DOUBLE), VALUE               :: epsge
        Integer(C_INT),       Intent(INOUT) :: numintpt
        Type(C_PTR),          Intent(INOUT) :: pointpar1
        Type(C_PTR),          Intent(INOUT) :: pointpar2
        Integer(C_INT),       Intent(INOUT) :: numintcr
        Type(C_PTR),          Intent(INOUT) :: intcurves
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1859
    End Interface

    c_numintpt = 0
    c_numintcr = 0
    c_pointpar1_p = C_NULL_PTR
    c_pointpar2_p = C_NULL_PTR
    c_intcurves_p = C_NULL_PTR

    Call c_s1859(surf1%cptr, surf2%cptr, epsco, epsge, c_numintpt,             &
                 c_pointpar1_p, c_pointpar2_p, c_numintcr,  c_intcurves_p,     &
                 c_stat)

    numintcr = c_numintcr
    numintpt = c_numintpt
    If (numintpt > 0) Then

      If (C_ASSOCIATED(c_pointpar1_p)) Then
        Call C_F_Pointer(c_pointpar1_p, pointparp1,[numintpt*2])
        If (ALLOCATED(pointpar1)) DEALLOCATE(pointpar1)
        ALLOCATE(pointpar1(numintpt*2), SOURCE=0.0_REAL64)
        pointpar1(:) = pointparp1(:)
        NULLIFY(pointparp1)
        Call c_free(c_pointpar1_p)
      EndIf

      If (C_ASSOCIATED(c_pointpar2_p)) Then
        Call C_F_Pointer(c_pointpar2_p, pointparp2,[numintpt*2])
        If (ALLOCATED(pointpar2)) DEALLOCATE(pointpar2)
        ALLOCATE(pointpar2(numintpt*2), SOURCE=0.0_REAL64)
        pointpar2(:) = pointparp2(:)
        NULLIFY(pointparp2)
        Call c_free(c_pointpar2_p)
      EndIf

    EndIf
    If (numintcr > 0) Then

      If (C_ASSOCIATED(c_intcurves_p)) Then
        Call C_F_Pointer(c_intcurves_p, cintcurves, [numintcr])
        If (ALLOCATED(intcurves)) Deallocate(intcurves)
        ALLOCATE(intcurves(numintcr))
        Do i=1,numintcr
          If (C_ASSOCIATED(cintcurves(i))) Then
            intcurves(i)%cptr = cintcurves(i)
          Else
            intcurves(i)%cptr = C_NULL_PTR
          End If
          Call IntCurveCtoF(intcurves(i))
        EndDo
      EndIf
    EndIf

    If (ASSOCIATED(cintcurves)) NULLIFY(cintcurves)
    c_intcurves_p = C_NULL_PTR

    stat = c_stat

  End Subroutine s1859

!---------------------------------- s1860 -------------------------------------

  Subroutine s1860(surf, viewdir, dim, epsco, epsge, numsilpt, pointpar,       &
                   numsilcr, silcurves, stat)

!! PURPOSE
!!   s1860 - Find the silhouette curves and points of a surface when the surface
!!           is viewed from a specific direction (i.e. parallel projection). In
!!           addition to the points and curves found by this routine, break 
!!           curves and edge-curves might be silhouette curves. Silhouette 
!!           curves are described by guide points. To produce the silhouette
!!           curves use s1319.

!! INTERFACE
!!   Subroutine s1860(surf, viewdir, dim, epsco, epsge, numsilpt, pointpar,   &
!!                    numsilcr, silcurves, stat)
!!     Type(SISLsurf),                  Intent(IN)    :: surf
!!     Real(REAL64),                    Intent(IN)    :: viewdir(*)
!!     Integer,                         Intent(IN)    :: dim
!!     Real(REAL64),                    Intent(IN)    :: epsco
!!     Real(REAL64),                    Intent(IN)    :: epsge
!!     Integer,                         Intent(INOUT) :: numsilpt
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: pointpar(:)
!!     Integer,                         Intent(INOUT) :: numsilcr
!!     Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: silcurves(:)
!!     Integer,                         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf),                  Intent(IN)    :: surf
    Real(REAL64),                    Intent(IN)    :: viewdir(*)
    Integer,                         Intent(IN)    :: dim
    Real(REAL64),                    Intent(IN)    :: epsco
    Real(REAL64),                    Intent(IN)    :: epsge
    Integer,                         Intent(INOUT) :: numsilpt
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: pointpar(:)
    Integer,                         Intent(INOUT) :: numsilcr
    Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: silcurves(:)
    Integer,                         Intent(INOUT) :: stat

    Integer :: i

    Integer(C_INT)        :: c_dim, c_numsilpt, c_numsilcr, c_stat
    Type(C_PTR)           :: c_silcurves_p, c_pointpar_p
    Type(C_PTR),  Pointer :: csilcurves(:)
    Real(REAL64), Pointer :: pointparp(:)

    Interface
      Subroutine c_s1860(surf, viewdir, dim, epsco, epsge, numsilpt, pointpar, &
                         numsilcr, silcurves, stat)                            &
                         BIND(C,name="s1860")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf 
        Real(C_DOUBLE),       Intent(IN)    :: viewdir(*)
        Integer(C_INT), VALUE               :: dim
        Real(C_DOUBLE), VALUE               :: epsco
        Real(C_DOUBLE), VALUE               :: epsge
        Integer(C_INT),       Intent(INOUT) :: numsilpt
        Type(C_PTR),          Intent(INOUT) :: pointpar
        Integer(C_INT),       Intent(INOUT) :: numsilcr
        Type(C_PTR),          Intent(INOUT) :: silcurves
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1860
    End Interface

    c_dim = dim

    Call c_s1860(surf%cptr, viewdir, c_dim, epsco, epsge, c_numsilpt,       &
                 c_pointpar_p, c_numsilcr, c_silcurves_p, c_stat) 

    numsilcr = c_numsilcr
    numsilpt = c_numsilpt
    If (numsilpt > 0) Then
      If (C_ASSOCIATED(c_pointpar_p)) Then
        Call C_F_Pointer(c_pointpar_p, pointparp,[numsilpt*2])
        If (ALLOCATED(pointpar)) DEALLOCATE(pointpar)
        ALLOCATE(pointpar(numsilpt*2), SOURCE=0.0_REAL64)
        pointpar(:) = pointparp(:)
        NULLIFY(pointparp)
        Call c_free(c_pointpar_p)
      EndIf
    EndIf

    If (numsilcr > 0) Then

      If (C_ASSOCIATED(c_silcurves_p)) Then
        Call C_F_Pointer(c_silcurves_p, csilcurves, [numsilcr])
        If (ALLOCATED(silcurves)) Deallocate(silcurves)
        ALLOCATE(silcurves(numsilcr))
        Do i=1,numsilcr
          If (C_ASSOCIATED(csilcurves(i))) Then
            silcurves(i)%cptr = csilcurves(i)
          Else
            silcurves(i)%cptr = C_NULL_PTR
          End If
          Call IntCurveCtoF(silcurves(i))
        EndDo
      EndIf
    EndIf

    If (ASSOCIATED(csilcurves)) NULLIFY(csilcurves)
    c_silcurves_p = C_NULL_PTR

    stat = c_stat

  End Subroutine s1860

!---------------------------------- s1510 -------------------------------------

  Subroutine s1510(ps, eyepoint, idim, aepsco, aepsge, jpt, gpar, jcrv,        &
                   wcurve, jstat)

!! PURPOSE

!!   s1510 - Find the silhouette curves and points of a surface when the surface
!!           is viewed perspectively from a specific eye point. In addition to 
!!           the points and curves found by this routine, break curves and edge-
!!           curves might be silhouette curves. To march out the silhouette
!!           curves, use s1514.

!! INTERFACE
!!   Subroutine s1510(ps, eyepoint, idim, aepsco, aepsge, jpt, gpar, jcrv,   &
!!                    wcurve, jstat)
!!     Type(SISLsurf),                  Intent(IN)    :: ps
!!     Real(REAL64),                    Intent(IN)    :: eyepoint(*)
!!     Integer,                         Intent(IN)    :: idim
!!     Real(REAL64),                    Intent(IN)    :: aepsco
!!     Real(REAL64),                    Intent(IN)    :: aepsge
!!     Integer,                         Intent(INOUT) :: jpt
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: gpar(:)
!!     Integer,                         Intent(INOUT) :: jcrv
!!     Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: wcurve(:)
!!     Integer,                         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLsurf),                  Intent(IN)    :: ps
    Real(REAL64),                    Intent(IN)    :: eyepoint(*)
    Integer,                         Intent(IN)    :: idim
    Real(REAL64),                    Intent(IN)    :: aepsco
    Real(REAL64),                    Intent(IN)    :: aepsge
    Integer,                         Intent(INOUT) :: jpt
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: gpar(:)
    Integer,                         Intent(INOUT) :: jcrv
    Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: wcurve(:)
    Integer,                         Intent(INOUT) :: jstat

    Integer :: i

    Integer(C_INT)        :: c_idim, c_jpt, c_jcrv, c_jstat
    Type(C_PTR)           :: c_wcurve_p, c_gpar_p
    Type(C_PTR),  Pointer :: cwcurve(:)
    Real(REAL64), Pointer :: gparp(:)

    Interface
      Subroutine c_s1510(ps, eyepoint, idim, aepsco, aepsge, jpt, gpar, jcrv,  &
                         wcurve, jstat)                                        &
                         BIND(C,name="s1510")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: ps
        Real(C_DOUBLE),       Intent(IN)    :: eyepoint(*)
        Integer(C_INT), VALUE               :: idim
        Real(C_DOUBLE), VALUE               :: aepsco
        Real(C_DOUBLE), VALUE               :: aepsge
        Integer(C_INT),       Intent(INOUT) :: jpt
        Type(C_PTR),          Intent(INOUT) :: gpar
        Integer(C_INT),       Intent(INOUT) :: jcrv
        Type(C_PTR),          Intent(INOUT) :: wcurve
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1510
    End Interface

    c_idim = idim

    Call c_s1510(ps%cptr, eyepoint, c_idim, aepsco, aepsge, c_jpt,             &
                 c_gpar_p, c_jcrv, c_wcurve_p, c_jstat)

    jcrv = c_jcrv
    jpt  = c_jpt
    If (jpt > 0) Then
      If (C_ASSOCIATED(c_gpar_p)) Then
        Call C_F_Pointer(c_gpar_p, gparp,[jpt*2])
        If (ALLOCATED(gpar)) DEALLOCATE(gpar)
        ALLOCATE(gpar(jpt*2), SOURCE=0.0_REAL64)
        gpar(:) = gparp(:)
        NULLIFY(gparp)
        Call c_free(c_gpar_p)
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
          Call IntCurveCtoF(wcurve(i))
        EndDo
      EndIf
    EndIf

    If (ASSOCIATED(cwcurve)) NULLIFY(cwcurve)
    c_wcurve_p = C_NULL_PTR

    jstat = c_jstat

  End Subroutine s1510

!---------------------------------- s1511 -------------------------------------

  Subroutine s1511(ps, qpoint, bvec, idim, aepsco, aepsge, jpt, gpar, jcrv,    &
                   wcurve, jstat)

!! PURPOSE
!!   s1511 - Find the circular silhouette curves and points of a surface. In
!!           addition to the points and curves found by this routine, break
!!           curves and edge-curves might be silhouette curves. To march out
!!           the silhouette curves use s1515.

!! INTERFACE
!!   Subroutine s1511(ps, qpoint, bvec, idim, aepsco, aepsge, jpt, gpar, jcrv, &
!!                    wcurve, jstat)
!!     Type(SISLsurf),                  Intent(IN)    :: ps
!!     Real(REAL64),                    Intent(IN)    :: qpoint(*)
!!     Real(REAL64),                    Intent(IN)    :: bvec(*)
!!     Integer,                         Intent(IN)    :: idim
!!     Real(REAL64),                    Intent(IN)    :: aepsco
!!     Real(REAL64),                    Intent(IN)    :: aepsge
!!     Integer,                         Intent(INOUT) :: jpt
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: gpar(:)
!!     Integer,                         Intent(INOUT) :: jcrv
!!     Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: wcurve(:)
!!     Integer,                         Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLsurf),                  Intent(IN)    :: ps
    Real(REAL64),                    Intent(IN)    :: qpoint(*)
    Real(REAL64),                    Intent(IN)    :: bvec(*)
    Integer,                         Intent(IN)    :: idim
    Real(REAL64),                    Intent(IN)    :: aepsco
    Real(REAL64),                    Intent(IN)    :: aepsge
    Integer,                         Intent(INOUT) :: jpt
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: gpar(:)
    Integer,                         Intent(INOUT) :: jcrv
    Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: wcurve(:)
    Integer,                         Intent(INOUT) :: jstat

    Integer :: i

    Integer(C_INT)        :: c_idim, c_jpt, c_jcrv, c_jstat
    Type(C_PTR)           :: c_wcurve_p, c_gpar_p
    Type(C_PTR),  Pointer :: cwcurve(:)
    Real(REAL64), Pointer :: gparp(:)

    Interface
      Subroutine c_s1511(ps, qpoint, bvec, idim, aepsco, aepsge, jpt, gpar,    &
                         jcrv, wcurve, jstat)                                  &
                         BIND(C,name="s1511")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: ps
        Real(C_DOUBLE),       Intent(IN)    :: qpoint(*)
        Real(C_DOUBLE),       Intent(IN)    :: bvec(*)
        Integer(C_INT), VALUE               :: idim
        Real(C_DOUBLE), VALUE               :: aepsco
        Real(C_DOUBLE), VALUE               :: aepsge
        Integer(C_INT),       Intent(INOUT) :: jpt
        Type(C_PTR),          Intent(INOUT) :: gpar
        Integer(C_INT),       Intent(INOUT) :: jcrv
        Type(C_PTR),          Intent(INOUT) :: wcurve
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1511
    End Interface

    c_idim = idim

    Call c_s1511(ps%cptr, qpoint, bvec, c_idim, aepsco, aepsge, c_jpt,      &
                 c_gpar_p, c_jcrv, c_wcurve_p, c_jstat)

    jcrv = c_jcrv
    jpt  = c_jpt
    If (jpt > 0) Then
      If (C_ASSOCIATED(c_gpar_p)) Then
        Call C_F_Pointer(c_gpar_p, gparp,[jpt*2])
        If (ALLOCATED(gpar)) DEALLOCATE(gpar)
        ALLOCATE(gpar(jpt*2), SOURCE=0.0_REAL64)
        gpar(:) = gparp(:)
        NULLIFY(gparp)
        Call c_free(c_gpar_p)
      EndIf
    EndIf

    If (jcrv > 0) Then

      If (C_ASSOCIATED(c_wcurve_p)) Then
        Call C_F_Pointer(c_wcurve_p, cwcurve, [jcrv])
        If (ALLOCATED(wcurve)) DEALLOCATE(wcurve)
        ALLOCATE(wcurve(jcrv))
        Do i=1,jcrv
          If (C_ASSOCIATED(cwcurve(i))) Then
            wcurve(i)%cptr = cwcurve(i)
          Else
            wcurve(i)%cptr = C_NULL_PTR
          End If
          Call IntCurveCtoF(wcurve(i))
        EndDo
      EndIf
    EndIf

    If (ASSOCIATED(cwcurve)) NULLIFY(cwcurve)
    c_wcurve_p = C_NULL_PTR

    jstat = c_jstat

  End Subroutine s1511

!---------------------------------- s1314 -------------------------------------

  Subroutine s1314(surf, point, normal, dim, epsco, epsge, maxstep, intcurve,  &
                   makecurv, graphic, stat)

!! PURPOSE
!!   s1314 - To march an intersection curve described by parameter pairs in an
!!           intersection curve object, a surface and a plane. The guide points
!!           are expected to be found by s1851. The generated geometric curves
!!           are represented as B-spline curves.

!! INTERFACE
!!   Subroutine s1314(surf, point, normal, dim, epsco, epsge, maxstep,        &
!!                    intcurve, makecurv, graphic, stat)
!!     Type(SISLsurf),                  Intent(IN)    :: surf 
!!     Real(REAL64),                    Intent(IN)    :: point(*)
!!     Real(REAL64),                    Intent(IN)    :: normal(*)
!!     Integer,                         Intent(IN)    :: dim
!!     Real(REAL64),                    Intent(IN)    :: epsco
!!     Real(REAL64),                    Intent(IN)    :: epsge
!!     Real(REAL64),                    Intent(IN)    :: maxstep
!!     Type(SISLIntcurve),              Intent(INOUT) :: intcurve
!!     Integer,                         Intent(IN)    :: makecurv
!!     Integer,                         Intent(IN)    :: graphic
!!     Integer,                         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf),                  Intent(IN)    :: surf 
    Real(REAL64),                    Intent(IN)    :: point(*)
    Real(REAL64),                    Intent(IN)    :: normal(*)
    Integer,                         Intent(IN)    :: dim
    Real(REAL64),                    Intent(IN)    :: epsco
    Real(REAL64),                    Intent(IN)    :: epsge
    Real(REAL64),                    Intent(IN)    :: maxstep
    Type(SISLIntcurve),              Intent(INOUT) :: intcurve
    Integer,                         Intent(IN)    :: makecurv
    Integer,                         Intent(IN)    :: graphic
    Integer,                         Intent(INOUT) :: stat

    Integer(C_INT) :: c_dim, c_makecurv, c_graphic, c_stat

    Interface
      Subroutine c_s1314(surf, point, normal, dim, epsco, epsge, maxstep,      &
                         intcurve, makecurv, graphic, stat)                    &
                         BIND(C,name="s1314")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf 
        Real(C_DOUBLE),       Intent(IN)    :: point(*)
        Real(C_DOUBLE),       Intent(IN)    :: normal(*)
        Integer(C_INT), VALUE               :: dim
        Real(C_DOUBLE), VALUE               :: epsco
        Real(C_DOUBLE), VALUE               :: epsge
        Real(C_DOUBLE), VALUE               :: maxstep 
        Type(C_PTR),    VALUE               :: intcurve
        Integer(C_INT), VALUE               :: makecurv
        Integer(C_INT), VALUE               :: graphic 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1314
    End Interface

    c_dim      = dim
    c_graphic  = 0
    c_makecurv = makecurv
    If (graphic > 0) Then
      Print *,' s1314 : graphic option not supported at this time '
    EndIf

    Call c_s1314(surf%cptr, point, normal, c_dim, epsco, epsge, maxstep,    &
                 intcurve%cptr, c_makecurv, c_graphic, c_stat)

    Call IntCurveCtoF(intcurve)

    stat = c_stat

  End Subroutine s1314

!---------------------------------- s1315 -------------------------------------

  Subroutine s1315(surf, centre, radius, dim, epsco, epsge, maxstep, intcurve, &
                   makecurv, graphic, stat)

!! PURPOSE
!!   s1315 - To march an intersection curve described by parameter pairs in an
!!           intersection curve object, a surface and a sphere. The guide points
!!           are expected to be found by s1852. The generated geometric curves
!!           are represented as B-spline curves.

!! INTERFACE
!!   Subroutine s1315(surf, centre, radius, dim, epsco, epsge, maxstep,       &
!!                    intcurve, makecurv, graphic, stat)
!!     Type(SISLsurf),                  Intent(IN)    :: surf
!!     Real(REAL64),                    Intent(IN)    :: centre(*)
!!     Real(REAL64),                    Intent(IN)    :: radius
!!     Integer,                         Intent(IN)    :: dim
!!     Real(REAL64),                    Intent(IN)    :: epsco
!!     Real(REAL64),                    Intent(IN)    :: epsge
!!     Real(REAL64),                    Intent(IN)    :: maxstep 
!!     Type(SISLIntcurve),              Intent(INOUT) :: intcurve
!!     Integer,                         Intent(IN)    :: makecurv
!!     Integer,                         Intent(IN)    :: graphic
!!     Integer,                         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf),                  Intent(IN)    :: surf
    Real(REAL64),                    Intent(IN)    :: centre(*)
    Real(REAL64),                    Intent(IN)    :: radius
    Integer,                         Intent(IN)    :: dim
    Real(REAL64),                    Intent(IN)    :: epsco
    Real(REAL64),                    Intent(IN)    :: epsge
    Real(REAL64),                    Intent(IN)    :: maxstep 
    Type(SISLIntcurve),              Intent(INOUT) :: intcurve
    Integer,                         Intent(IN)    :: makecurv
    Integer,                         Intent(IN)    :: graphic
    Integer,                         Intent(INOUT) :: stat

    Integer(C_INT)        :: c_dim, c_makecurv, c_graphic, c_stat

    Interface
      Subroutine c_s1315(surf, centre, radius, dim, epsco, epsge, maxstep,     &
                         intcurve, makecurv, graphic, stat)                    &
                         BIND(C,name="s1315")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf 
        Real(C_DOUBLE),       Intent(IN)    :: centre(*)
        Real(C_DOUBLE),       Intent(IN)    :: radius
        Integer(C_INT), VALUE               :: dim
        Real(C_DOUBLE), VALUE               :: epsco
        Real(C_DOUBLE), VALUE               :: epsge
        Real(C_DOUBLE), VALUE               :: maxstep 
        Type(C_PTR),    VALUE               :: intcurve
        Integer(C_INT), VALUE               :: makecurv
        Integer(C_INT), VALUE               :: graphic 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1315
    End Interface

    c_dim      = dim
    c_graphic  = 0
    c_makecurv = makecurv
    If (graphic > 0) Then
      Print *,' s1315 : graphic option not supported at this time'
    EndIf

    Call c_s1315(surf%cptr, centre, radius, c_dim, epsco, epsge, maxstep,   &
                 intcurve%cptr, c_makecurv, c_graphic, c_stat)

    Call IntCurveCtoF(intcurve)

    stat = c_stat

  End Subroutine s1315

!---------------------------------- s1316 -------------------------------------

  Subroutine s1316(surf, point, cyldir, radius, dim, epsco, epsge, maxstep,    &
                   intcurve, makecurv, graphic, stat)

!! PURPOSE
!!   1316 - To march an intersection curve described by parameter pairs in
!!          an intersection curve object, a surface and a cylinder. The guide
!!          points are expected to be found by s1853. The generated geometric
!!          curves are represented as B-spline curves.

!! INTERFACE
!!   Subroutine s1316(surf, point, cyldir, radius, dim, epsco, epsge,        &
!!                    maxstep, intcurve, makecurv, graphic, stat)
!!     Type(SISLsurf),                  Intent(IN)    :: surf 
!!     Real(REAL64),                    Intent(IN)    :: point(*)
!!     Real(REAL64),                    Intent(IN)    :: cyldir(*)
!!     Real(REAL64),                    Intent(IN)    :: radius
!!     Integer,                         Intent(IN)    :: dim
!!     Real(REAL64),                    Intent(IN)    :: epsco
!!     Real(REAL64),                    Intent(IN)    :: epsge
!!     Real(REAL64),                    Intent(IN)    :: maxstep 
!!     Type(SISLIntcurve),              Intent(INOUT) :: intcurve
!!     Integer,                         Intent(IN)    :: makecurv
!!     Integer,                         Intent(IN)    :: graphic
!!     Integer,                         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf),                  Intent(IN)    :: surf 
    Real(REAL64),                    Intent(IN)    :: point(*)
    Real(REAL64),                    Intent(IN)    :: cyldir(*)
    Real(REAL64),                    Intent(IN)    :: radius
    Integer,                         Intent(IN)    :: dim
    Real(REAL64),                    Intent(IN)    :: epsco
    Real(REAL64),                    Intent(IN)    :: epsge
    Real(REAL64),                    Intent(IN)    :: maxstep 
    Type(SISLIntcurve),              Intent(INOUT) :: intcurve
    Integer,                         Intent(IN)    :: makecurv
    Integer,                         Intent(IN)    :: graphic
    Integer,                         Intent(INOUT) :: stat

    Integer(C_INT)        :: c_dim, c_makecurv, c_graphic, c_stat

    Interface
      Subroutine c_s1316(surf, point, cyldir, radius, dim, epsco, epsge,       &
                         maxstep, intcurve, makecurv, graphic, stat)           &
                         BIND(C,name="s1316")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf 
        Real(C_DOUBLE),       Intent(IN)    :: point(*)
        Real(C_DOUBLE),       Intent(IN)    :: cyldir(*)
        Real(C_DOUBLE), VALUE               :: radius
        Integer(C_INT), VALUE               :: dim
        Real(C_DOUBLE), VALUE               :: epsco
        Real(C_DOUBLE), VALUE               :: epsge
        Real(C_DOUBLE), VALUE               :: maxstep 
        Type(C_PTR),    VALUE               :: intcurve
        Integer(C_INT), VALUE               :: makecurv
        Integer(C_INT), VALUE               :: graphic 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1316
    End Interface

    c_dim      = dim
    c_graphic  = 0
    c_makecurv = makecurv
    If (graphic > 0) Then
      Print *,' s1316 : graphic option not supported at this time'
    EndIf

    Call c_s1316(surf%cptr, point, cyldir, radius, c_dim, epsco, epsge,     &
                 maxstep, intcurve%cptr, c_makecurv, c_graphic, c_stat)

    Call IntCurveCtoF(intcurve)

    stat = c_stat

  End Subroutine s1316

!---------------------------------- s1317 -------------------------------------

  Subroutine s1317(surf, toppt, axispt, conept, dim, epsco, epsge, maxstep,    &
                   intcurve, makecurv, graphic, stat)

!! PURPOSE
!!   s1317 - To march an intersection curve described by parameter pairs in an
!!           intersection curve object, a surface and a cone. The guide points
!!           are expected to be found by s1854. The generated geometric
!!           curves are represented as B-spline curves.

!! INTERFACE
!!   Subroutine s1317(surf, toppt, axispt, conept, dim, epsco, epsge,         &
!!                    maxstep, intcurve, makecurv, graphic, stat)
!!     Type(SISLsurf),                  Intent(IN)    :: surf
!!     Real(REAL64),                    Intent(IN)    :: toppt(*)
!!     Real(REAL64),                    Intent(IN)    :: axispt(*)
!!     Real(REAL64),                    Intent(IN)    :: conept(*)
!!     Integer,                         Intent(IN)    :: dim
!!     Real(REAL64),                    Intent(IN)    :: epsco
!!     Real(REAL64),                    Intent(IN)    :: epsge
!!     Real(REAL64),                    Intent(IN)    :: maxstep 
!!     Type(SISLIntcurve),              Intent(INOUT) :: intcurve
!!     Integer,                         Intent(IN)    :: makecurv
!!     Integer,                         Intent(IN)    :: graphic
!!     Integer,                         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf),                  Intent(IN)    :: surf
    Real(REAL64),                    Intent(IN)    :: toppt(*)
    Real(REAL64),                    Intent(IN)    :: axispt(*)
    Real(REAL64),                    Intent(IN)    :: conept(*)
    Integer,                         Intent(IN)    :: dim
    Real(REAL64),                    Intent(IN)    :: epsco
    Real(REAL64),                    Intent(IN)    :: epsge
    Real(REAL64),                    Intent(IN)    :: maxstep 
    Type(SISLIntcurve),              Intent(INOUT) :: intcurve
    Integer,                         Intent(IN)    :: makecurv
    Integer,                         Intent(IN)    :: graphic
    Integer,                         Intent(INOUT) :: stat

    Integer(C_INT) :: c_dim, c_makecurv, c_graphic, c_stat

    Interface
      Subroutine c_s1317(surf, toppt, axispt, conept, dim, epsco, epsge,       &
                         maxstep, intcurve, makecurv, graphic, stat)           &
                         BIND(C,name="s1317")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf 
        Real(C_DOUBLE),       Intent(IN)    :: toppt(*)
        Real(C_DOUBLE),       Intent(IN)    :: axispt(*)
        Real(C_DOUBLE),       Intent(IN)    :: conept(*)
        Integer(C_INT), VALUE               :: dim
        Real(C_DOUBLE), VALUE               :: epsco
        Real(C_DOUBLE), VALUE               :: epsge
        Real(C_DOUBLE), VALUE               :: maxstep 
        Type(C_PTR),    VALUE               :: intcurve
        Integer(C_INT), VALUE               :: makecurv
        Integer(C_INT), VALUE               :: graphic 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1317
    End Interface

    c_dim      = dim
    c_graphic  = 0
    c_makecurv = makecurv
    If (graphic > 0) Then
      Print *,' s1317 : graphic option not supported at this time'
    EndIf

    Call c_s1317(surf%cptr, toppt, axispt, conept, c_dim, epsco, epsge,     &
                 maxstep, intcurve%cptr, c_makecurv, c_graphic, c_stat)

    Call IntCurveCtoF(intcurve)

    stat = c_stat

  End Subroutine s1317

!---------------------------------- s1501 -------------------------------------

  Subroutine s1501(surf, basept, normdir, ellipaxis, alpha, ratio, dim, epsco, &
                   epsge, maxstep, intcurve, makecurv, graphic, stat)

!! PURPOSE
!!   s1501 - To march an intersection curve described by parameter pairs in
!!           an intersection curve object, a surface and an elliptic cone. The
!!           guide points are expected to be found by s1503. The generated
!!           geometric curves are represented as B-spline curves.

!! INTERFACE
!!   Subroutine s1501(surf, basept, normdir, ellipaxis, alpha, ratio, dim,  &
!!                    epsco, epsge, maxstep, intcurve, makecurv, graphic,   &
!!                    stat)
!!     Type(SISLsurf),                  Intent(IN)    :: surf 
!!     Real(REAL64),                    Intent(IN)    :: basept(*)
!!     Real(REAL64),                    Intent(IN)    :: normdir(*)
!!     Real(REAL64),                    Intent(IN)    :: ellipaxis(*)
!!     Real(REAL64),                    Intent(IN)    :: alpha 
!!     Real(REAL64),                    Intent(IN)    :: ratio
!!     Integer,                         Intent(IN)    :: dim
!!     Real(REAL64),                    Intent(IN)    :: epsco
!!     Real(REAL64),                    Intent(IN)    :: epsge
!!     Real(REAL64),                    Intent(IN)    :: maxstep 
!!     Type(SISLIntcurve),              Intent(INOUT) :: intcurve
!!     Integer,                         Intent(IN)    :: makecurv
!!     Integer,                         Intent(IN)    :: graphic
!!     Integer,                         Intent(INOUT) :: stat
 
    Implicit NONE

    Type(SISLsurf),                  Intent(IN)    :: surf 
    Real(REAL64),                    Intent(IN)    :: basept(*)
    Real(REAL64),                    Intent(IN)    :: normdir(*)
    Real(REAL64),                    Intent(IN)    :: ellipaxis(*)
    Real(REAL64),                    Intent(IN)    :: alpha 
    Real(REAL64),                    Intent(IN)    :: ratio
    Integer,                         Intent(IN)    :: dim
    Real(REAL64),                    Intent(IN)    :: epsco
    Real(REAL64),                    Intent(IN)    :: epsge
    Real(REAL64),                    Intent(IN)    :: maxstep 
    Type(SISLIntcurve),              Intent(INOUT) :: intcurve
    Integer,                         Intent(IN)    :: makecurv
    Integer,                         Intent(IN)    :: graphic
    Integer,                         Intent(INOUT) :: stat

    Integer(C_INT) :: c_dim, c_makecurv, c_graphic, c_stat

    Interface
      Subroutine c_s1501(surf, basept, normdir, ellipaxis, alpha, ratio, dim,  &
                         epsco, epsge, maxstep, intcurve, makecurv, graphic,   &
                         stat) BIND(C,name="s1501")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf 
        Real(C_DOUBLE),       Intent(IN)    :: basept(*)
        Real(C_DOUBLE),       Intent(IN)    :: normdir(*)
        Real(C_DOUBLE),       Intent(IN)    :: ellipaxis(*)
        Real(C_DOUBLE), VALUE               :: alpha
        Real(C_DOUBLE), VALUE               :: ratio
        Integer(C_INT), VALUE               :: dim
        Real(C_DOUBLE), VALUE               :: epsco
        Real(C_DOUBLE), VALUE               :: epsge
        Real(C_DOUBLE), VALUE               :: maxstep 
        Type(C_PTR),    VALUE               :: intcurve
        Integer(C_INT), VALUE               :: makecurv
        Integer(C_INT), VALUE               :: graphic 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1501
    End Interface

    c_dim      = dim
    c_graphic  = 0
    c_makecurv = makecurv
    If (graphic > 0) Then
      Print *,' s1501 : graphic option not supported at this time'
    EndIf

    Call c_s1501(surf%cptr, basept, normdir, ellipaxis, alpha, ratio,          &
                 c_dim, epsco, epsge, maxstep, intcurve%cptr,                  &
                 c_makecurv, c_graphic, c_stat)

    Call IntCurveCtoF(intcurve)

    stat = c_stat

  End Subroutine s1501

!---------------------------------- s1318 -------------------------------------

  Subroutine s1318(surf, centre, normal, cendist, radius, dim, epsco, epsge,   &
                   maxstep, intcurve, makecurv, graphic, stat)

!! PURPOSE
!!   s1318 - To march an intersection curve described by parameter pairs in an
!!           intersection curve object, a surface and a torus. The guide points
!!           are expected to be found by s1369. The generated geometric
!!          curves are represented as B-spline curves.

!! INTERFACE
!!   Subroutine s1318(surf, centre, normal, cendist, radius, dim, epsco,       &
!!                    epsge, maxstep, intcurve, makecurv, graphic, stat)
!!     Type(SISLsurf),                  Intent(IN)    :: surf 
!!     Real(REAL64),                    Intent(IN)    :: centre(*)
!!     Real(REAL64),                    Intent(IN)    :: normal(*)
!!     Real(REAL64),                    Intent(IN)    :: cendist
!!     Real(REAL64),                    Intent(IN)    :: radius 
!!     Integer,                         Intent(IN)    :: dim
!!     Real(REAL64),                    Intent(IN)    :: epsco
!!     Real(REAL64),                    Intent(IN)    :: epsge
!!     Real(REAL64),                    Intent(IN)    :: maxstep 
!!     Type(SISLIntcurve),              Intent(INOUT) :: intcurve
!!     Integer,                         Intent(IN)    :: makecurv
!!     Integer,                         Intent(IN)    :: graphic
!!     Integer,                         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf),                  Intent(IN)    :: surf 
    Real(REAL64),                    Intent(IN)    :: centre(*)
    Real(REAL64),                    Intent(IN)    :: normal(*)
    Real(REAL64),                    Intent(IN)    :: cendist
    Real(REAL64),                    Intent(IN)    :: radius 
    Integer,                         Intent(IN)    :: dim
    Real(REAL64),                    Intent(IN)    :: epsco
    Real(REAL64),                    Intent(IN)    :: epsge
    Real(REAL64),                    Intent(IN)    :: maxstep 
    Type(SISLIntcurve),              Intent(INOUT) :: intcurve
    Integer,                         Intent(IN)    :: makecurv
    Integer,                         Intent(IN)    :: graphic
    Integer,                         Intent(INOUT) :: stat

    Integer(C_INT) :: c_dim, c_makecurv, c_graphic, c_stat

    Interface
      Subroutine c_s1318(surf, centre, normal, cendist, radius, dim, epsco,    &
                         epsge, maxstep, intcurve, makecurv, graphic, stat)    &
                         BIND(C,name="s1318")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf 
        Real(C_DOUBLE),       Intent(IN)    :: centre(*)
        Real(C_DOUBLE),       Intent(IN)    :: normal(*)
        Real(C_DOUBLE), VALUE               :: cendist
        Real(C_DOUBLE), VALUE               :: radius
        Integer(C_INT), VALUE               :: dim
        Real(C_DOUBLE), VALUE               :: epsco
        Real(C_DOUBLE), VALUE               :: epsge
        Real(C_DOUBLE), VALUE               :: maxstep 
        Type(C_PTR),    VALUE               :: intcurve
        Integer(C_INT), VALUE               :: makecurv
        Integer(C_INT), VALUE               :: graphic 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1318
    End Interface

    c_dim      = dim
    c_graphic  = 0
    c_makecurv = makecurv
    If (graphic > 0) Then
      Print *,' s1318 : graphic option not supported at this time'
    EndIf

    Call c_s1318(surf%cptr, centre, normal, cendist, radius, c_dim,        &
                 epsco, epsge, maxstep, intcurve%cptr, c_makecurv,       &
                 c_graphic, c_stat)

    Call IntCurveCtoF(intcurve)

    stat = c_stat

  End Subroutine s1318

!---------------------------------- s1310 -------------------------------------

  Subroutine s1310(surf1, surf2, intcurve, epsge, maxstep, makecurv, graphic,  &
                   stat)

!! PURPOSE
!!   s1310 - To march an intersection curve between two surfaces. The inter-
!!           section curve is described by guide parameter pairs stored in an
!!           intersection curve object. The guide points are expected to be
!!           found by s1859. The generated geometric curves are represented
!!           as B-spline curves.

!! INTERFACE
!!   Subroutine s1310(surf1, surf2, intcurve, epsge, maxstep, makecurv,        &
!!                    graphic, stat)
!!     Type(SISLsurf),     Intent(IN)    :: surf1
!!     Type(SISLsurf),     Intent(IN)    :: surf2 
!!     Type(SISLIntcurve), Intent(INOUT) :: intcurve
!!     Real(REAL64),       Intent(IN)    :: epsge
!!     Real(REAL64),       Intent(IN)    :: maxstep 
!!     Integer,            Intent(IN)    :: makecurv
!!     Integer,            Intent(IN)    :: graphic
!!     Integer,            Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf),     Intent(IN)    :: surf1
    Type(SISLsurf),     Intent(IN)    :: surf2 
    Type(SISLIntcurve), Intent(INOUT) :: intcurve
    Real(REAL64),       Intent(IN)    :: epsge
    Real(REAL64),       Intent(IN)    :: maxstep 
    Integer,            Intent(IN)    :: makecurv
    Integer,            Intent(IN)    :: graphic
    Integer,            Intent(INOUT) :: stat

    Integer(C_INT) :: c_makecurv, c_graphic, c_stat

    Interface
      Subroutine c_s1310(surf1, surf2, intcurve, epsge, maxstep, makecurv,     &
                         graphic, stat)                                        &
                         BIND(C,name="s1310")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf1 
        Type(C_PTR),    VALUE               :: surf2 
        Type(C_PTR),    VALUE               :: intcurve
        Real(C_DOUBLE), VALUE               :: epsge
        Real(C_DOUBLE), VALUE               :: maxstep 
        Integer(C_INT), VALUE               :: makecurv
        Integer(C_INT), VALUE               :: graphic 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1310
    End Interface

    c_graphic  = 0
    c_makecurv = makecurv
    If (graphic > 0) Then
      Print *,' s1310 : graphic option not supported at this time'
    EndIf

    Call c_s1310(surf1%cptr, surf2%cptr, intcurve%cptr, epsge,      &
                 maxstep, c_makecurv, c_graphic, c_stat)

    Call IntCurveCtoF(intcurve)

    stat = c_stat

  End Subroutine s1310

!---------------------------------- s1319 -------------------------------------

  Subroutine s1319(surf, viewdir, dim, epsco, epsge, maxstep, intcurve,        &
                   makecurv, graphic, stat)

!! PURPOSE
!!   s1319 - To march the silhouette curve described by an intersection curve
!!           object, a surface and a view direction (i.e. parallel projection).
!!           The guide points are expected to be found by s1860. The generated
!!           geometric curves are represented as B-spline curves.

!! INTERFACE
!!   Subroutine s1319(surf, viewdir, dim, epsco, epsge, maxstep, intcurve,    &
!!                    makecurv, graphic, stat)
!!     Type(SISLsurf),     Intent(IN)    :: surf
!!     Real(REAL64),       Intent(IN)    :: viewdir(*)
!!     Integer,            Intent(IN)    :: dim
!!     Real(REAL64),       Intent(IN)    :: epsco
!!     Real(REAL64),       Intent(IN)    :: epsge
!!     Real(REAL64),       Intent(IN)    :: maxstep 
!!     Type(SISLIntcurve), Intent(INOUT) :: intcurve
!!     Integer,            Intent(IN)    :: makecurv
!!     Integer,            Intent(IN)    :: graphic
!!     Integer,            Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf),     Intent(IN)    :: surf
    Real(REAL64),       Intent(IN)    :: viewdir(*)
    Integer,            Intent(IN)    :: dim
    Real(REAL64),       Intent(IN)    :: epsco
    Real(REAL64),       Intent(IN)    :: epsge
    Real(REAL64),       Intent(IN)    :: maxstep 
    Type(SISLIntcurve), Intent(INOUT) :: intcurve
    Integer,            Intent(IN)    :: makecurv
    Integer,            Intent(IN)    :: graphic
    Integer,            Intent(INOUT) :: stat

    Integer(C_INT) :: c_dim, c_makecurv, c_graphic, c_stat

    Interface
      Subroutine c_s1319(surf, viewdir, dim, epsco, epsge, maxstep, intcurve,  &
                         makecurv, graphic, stat)                              &
                         BIND(C,name="s1319")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf 
        Real(C_DOUBLE),       Intent(IN)    :: viewdir(*)
        Integer(C_INT), VALUE               :: dim
        Real(C_DOUBLE), VALUE               :: epsco
        Real(C_DOUBLE), VALUE               :: epsge
        Real(C_DOUBLE), VALUE               :: maxstep 
        Type(C_PTR),    VALUE               :: intcurve
        Integer(C_INT), VALUE               :: makecurv
        Integer(C_INT), VALUE               :: graphic 
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1319
    End Interface

    c_dim      = dim
    c_graphic  = 0
    c_makecurv = makecurv
    If (graphic > 0) Then
      Print *,' s1319 : graphic option not supported at this time'
    EndIf

    Call c_s1319(surf%cptr, viewdir, c_dim, epsco, epsge, maxstep,          &
                 intcurve%cptr, c_makecurv, c_graphic, c_stat)

    Call IntCurveCtoF(intcurve)

    stat = c_stat

  End Subroutine s1319

!---------------------------------- s1514 -------------------------------------

  Subroutine s1514(ps1, eyepoint, idim, aepsco, aepsge, amax, pinter, icur,    &
                   igraph, jstat)

!! PURPOSE
!!   s1514 - To march the perspective silhouette curve described by an inter-
!!           section curve object, a surface and an eye point. The generated
!!           geometric curves are represented as B-spline curves.

!! INTERFACE
!!   Subroutine s1514(ps1, eyepoint, idim, aepsco, aepsge, amax, pinter,     &
!!                    icur, igraph, jstat)
!!     Type(SISLsurf),     Intent(IN)    :: ps1
!!     Real(REAL64),       Intent(IN)    :: eyepoint(*)
!!     Integer,            Intent(IN)    :: idim
!!     Real(REAL64),       Intent(IN)    :: aepsco
!!     Real(REAL64),       Intent(IN)    :: aepsge
!!     Real(REAL64),       Intent(IN)    :: amax
!!     Type(SISLIntcurve), Intent(INOUT) :: pinter
!!     Integer,            Intent(IN)    :: icur
!!     Integer,            Intent(IN)    :: igraph
!!     Integer,            Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLsurf),     Intent(IN)    :: ps1
    Real(REAL64),       Intent(IN)    :: eyepoint(*)
    Integer,            Intent(IN)    :: idim
    Real(REAL64),       Intent(IN)    :: aepsco
    Real(REAL64),       Intent(IN)    :: aepsge
    Real(REAL64),       Intent(IN)    :: amax
    Type(SISLIntcurve), Intent(INOUT) :: pinter
    Integer,            Intent(IN)    :: icur
    Integer,            Intent(IN)    :: igraph
    Integer,            Intent(INOUT) :: jstat

    Integer(C_INT) :: c_idim, c_icur, c_igraph, c_jstat

    Interface
      Subroutine c_s1514(ps1, eyepoint, idim, aepsco, aepsge, amax, pinter,  &
                         icur, igraph, jstat)                                &
                         BIND(C,name="s1514")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: ps1 
        Real(C_DOUBLE),       Intent(IN)    :: eyepoint(*)
        Integer(C_INT), VALUE               :: idim
        Real(C_DOUBLE), VALUE               :: aepsco
        Real(C_DOUBLE), VALUE               :: aepsge
        Real(C_DOUBLE), VALUE               :: amax 
        Type(C_PTR),    VALUE               :: pinter
        Integer(C_INT), VALUE               :: icur
        Integer(C_INT), VALUE               :: igraph 
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1514
    End Interface

    c_idim   = idim
    c_igraph = 0
    c_icur   = icur
    If (igraph > 0) Then
      Print *,' s1514 : igraph option not supported at this time'
    EndIf

    Call c_s1514(ps1%cptr, eyepoint, c_idim, aepsco, aepsge, amax,          &
                 pinter%cptr, c_icur, c_igraph, c_jstat)

    Call IntCurveCtoF(pinter)

    jstat = c_jstat

  End Subroutine s1514

!---------------------------------- s1515 -------------------------------------

  Subroutine s1515(ps1, qpoint, bvec, idim, aepsco, aepsge, amax, pinter,      &
                   icur, igraph, jstat)

!! PURPOSE
!!   s1515 - To march the circular silhouette curve described by an intersection
!!           curve object, a surface, point Q and direction B i.e. solution of
!!           f(u, v) = N(u, v) × (P(u, v) − Q) · B.
!!           The generated geometric curves are represented as B-spline curves.

!! INTERFACE
!!   Subroutine s1515(ps1, qpoint, bvec, idim, aepsco, aepsge, amax, pinter,   &
!!                    icur, igraph, jstat)
!!     Type(SISLsurf),     Intent(IN)    :: ps1
!!     Real(REAL64),       Intent(IN)    :: qpoint(*)
!!     Real(REAL64),       Intent(IN)    :: bvec(*)
!!     Integer,            Intent(IN)    :: idim
!!     Real(REAL64),       Intent(IN)    :: aepsco
!!     Real(REAL64),       Intent(IN)    :: aepsge
!!     Real(REAL64),       Intent(IN)    :: amax 
!!     Type(SISLIntcurve), Intent(INOUT) :: pinter
!!     Integer,            Intent(IN)    :: icur
!!     Integer,            Intent(IN)    :: igraph
!!     Integer,            Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLsurf),     Intent(IN)    :: ps1
    Real(REAL64),       Intent(IN)    :: qpoint(*)
    Real(REAL64),       Intent(IN)    :: bvec(*)
    Integer,            Intent(IN)    :: idim
    Real(REAL64),       Intent(IN)    :: aepsco
    Real(REAL64),       Intent(IN)    :: aepsge
    Real(REAL64),       Intent(IN)    :: amax 
    Type(SISLIntcurve), Intent(INOUT) :: pinter
    Integer,            Intent(IN)    :: icur
    Integer,            Intent(IN)    :: igraph
    Integer,            Intent(INOUT) :: jstat

    Integer(C_INT) :: c_idim, c_icur, c_igraph, c_jstat

    Interface
      Subroutine c_s1515(ps1, qpoint, bvec, idim, aepsco, aepsge, amax,        &
                         pinter, icur, igraph, jstat)                          &
                         BIND(C,name="s1515")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: ps1 
        Real(C_DOUBLE),       Intent(IN)    :: qpoint(*)
        Real(C_DOUBLE),       Intent(IN)    :: bvec(*)
        Integer(C_INT), VALUE               :: idim
        Real(C_DOUBLE), VALUE               :: aepsco
        Real(C_DOUBLE), VALUE               :: aepsge
        Real(C_DOUBLE), VALUE               :: amax 
        Type(C_PTR),    VALUE               :: pinter
        Integer(C_INT), VALUE               :: icur
        Integer(C_INT), VALUE               :: igraph 
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1515
    End Interface

    c_idim   = idim
    c_igraph = 0
    c_icur   = icur
    If (igraph > 0) Then
      Print *,' s1515 : igraph option not supported at this time'
    EndIf

    Call c_s1515(ps1%cptr, qpoint, bvec, c_idim, aepsco, aepsge, amax,      &
                 pinter%cptr, c_icur, c_igraph, c_jstat)

    Call IntCurveCtoF(pinter)

    jstat = c_jstat

  End Subroutine s1515

!---------------------------------- s1450 -------------------------------------

  Subroutine s1450(surf, epsge, close1, close2, degen1, degen2, degen3,        &
                   degen4, stat)

!! PURPOSE
!!  s1450 - To check if a surface is closed or has degenerate boundaries. The
!!          edge numbers are ordered counter-clockwise with edge no 1 at the
!!          v=0 parameteric location

!! INTERFACE
!!   Subroutine s1450(surf, epsge, close1, close2, degen1, degen2, degen3,    &
!!                    degen4, stat)
!!     Type(SISLsurf), Intent(IN)    :: surf
!!     Real(REAL64),   Intent(IN)    :: epsge
!!     Integer,        Intent(INOUT) :: close1 
!!     Integer,        Intent(INOUT) :: close2 
!!     Integer,        Intent(INOUT) :: degen1 
!!     Integer,        Intent(INOUT) :: degen2 
!!     Integer,        Intent(INOUT) :: degen3 
!!     Integer,        Intent(INOUT) :: degen4 
!!     Integer,        Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf), Intent(IN)    :: surf
    Real(REAL64),   Intent(IN)    :: epsge
    Integer,        Intent(INOUT) :: close1 
    Integer,        Intent(INOUT) :: close2 
    Integer,        Intent(INOUT) :: degen1 
    Integer,        Intent(INOUT) :: degen2 
    Integer,        Intent(INOUT) :: degen3 
    Integer,        Intent(INOUT) :: degen4 
    Integer,        Intent(INOUT) :: stat

    Integer(C_INT) :: c_close1, c_close2, c_degen1, c_degen2, c_degen3,        &
                      c_degen4, c_stat

    Interface
      Subroutine c_s1450(surf, epsge, close1, close2, degen1, degen2, degen3,  &
                         degen4, stat) BIND(C,name="s1450")

        IMPORT :: C_PTR, C_DOUBLE, C_INT
        Implicit NONE

        Type(C_PTR),    VALUE                :: surf
        Real(C_DOUBLE), VALUE                :: epsge
        Integer(C_INT),        Intent(INOUT) :: close1 
        Integer(C_INT),        Intent(INOUT) :: close2 
        Integer(C_INT),        Intent(INOUT) :: degen1 
        Integer(C_INT),        Intent(INOUT) :: degen2 
        Integer(C_INT),        Intent(INOUT) :: degen3 
        Integer(C_INT),        Intent(INOUT) :: degen4 
        Integer(C_INT),        Intent(INOUT) :: stat

      End Subroutine c_s1450
    End Interface

    Call c_s1450(surf%cptr, epsge, c_close1, c_close2, c_degen1, c_degen2,  &
                 c_degen3, c_degen4, c_stat)

    close1 = c_close1
    close2 = c_close2
    degen1 = c_degen1
    degen2 = c_degen2
    degen3 = c_degen3
    degen4 = c_degen4

    stat = c_stat

  End Subroutine s1450

!---------------------------------- s1603 -------------------------------------

  Subroutine s1603(surf, min1, min2, max1, max2, stat) 

!! PURPOSE
!!   s1603 - To pick the parameter ranges of a surface

!! INTERFACE
!!   Subroutine s1603(surf, min1, min2, max1, max2, stat) 
!!     Type(SISLsurf),  Intent(IN)    :: surf
!!     Real(REAL64),    Intent(INOUT) :: min1 
!!     Real(REAL64),    Intent(INOUT) :: min2 
!!     Real(REAL64),    Intent(INOUT) :: max1 
!!     Real(REAL64),    Intent(INOUT) :: max2 
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf),  Intent(IN)    :: surf
    Real(REAL64),    Intent(INOUT) :: min1 
    Real(REAL64),    Intent(INOUT) :: min2 
    Real(REAL64),    Intent(INOUT) :: max1 
    Real(REAL64),    Intent(INOUT) :: max2 
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_stat

    Interface
      Subroutine c_s1603(surf, min1, min2, max1, max2, stat)                   &
                         BIND(C,name="s1603")

        IMPORT :: C_PTR, C_DOUBLE, C_INT
        Implicit NONE

        Type(C_PTR),    VALUE                :: surf
        Real(C_DOUBLE),        Intent(INOUT) :: min1 
        Real(C_DOUBLE),        Intent(INOUT) :: min2 
        Real(C_DOUBLE),        Intent(INOUT) :: max1 
        Real(C_DOUBLE),        Intent(INOUT) :: max2 
        Integer(C_INT),        Intent(INOUT) :: stat

      End Subroutine c_s1603
    End Interface

    Call c_s1603(surf%cptr, min1, min2, max1, max2, c_stat) 

    stat = c_stat

  End Subroutine s1603

!---------------------------------- s1954 -------------------------------------

  Subroutine s1954(surf, point, dim, epsco, epsge, numclopt, pointpar,       &
                   numclocr, clocurves, stat)

!! PURPOSE
!!   s1954 - Find the points on a surface lying closest to a given point.

!! INTERFACE
!!   Subroutine s1954(surf, point, dim, epsco, epsge, numclopt, pointpar,   &
!!                    numclocr, clocurves, stat)
!!     Type(SISLsurf),                  Intent(IN)    :: surf
!!     Real(REAL64),                    Intent(IN)    :: point(*)
!!     Integer,                         Intent(IN)    :: dim
!!     Real(REAL64),                    Intent(IN)    :: epsco
!!     Real(REAL64),                    Intent(IN)    :: epsge
!!     Integer,                         Intent(INOUT) :: numclopt
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: pointpar(:)
!!     Integer,                         Intent(INOUT) :: numclocr
!!     Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: clocurves(:)
!!     Integer,                         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf),                  Intent(IN)    :: surf
    Real(REAL64),                    Intent(IN)    :: point(*)
    Integer,                         Intent(IN)    :: dim
    Real(REAL64),                    Intent(IN)    :: epsco
    Real(REAL64),                    Intent(IN)    :: epsge
    Integer,                         Intent(INOUT) :: numclopt
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: pointpar(:)
    Integer,                         Intent(INOUT) :: numclocr
    Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: clocurves(:)
    Integer,                         Intent(INOUT) :: stat

    Integer :: i

    Integer(C_INT)        :: c_dim, c_numclopt, c_numclocr, c_stat
    Type(C_PTR)           :: c_clocurves_p, c_pointpar_p
    Type(C_PTR),  Pointer :: cclocurves(:)
    Real(REAL64), Pointer :: pointparp(:)

    Interface
      Subroutine c_s1954(surf, point, dim, epsco, epsge, numclopt, pointpar, &
                         numclocr, clocurves, stat)                            &
                         BIND(C,name="s1954")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf 
        Real(C_DOUBLE),       Intent(IN)    :: point(*)
        Integer(C_INT), VALUE               :: dim
        Real(C_DOUBLE), VALUE               :: epsco
        Real(C_DOUBLE), VALUE               :: epsge
        Integer(C_INT),       Intent(INOUT) :: numclopt
        Type(C_PTR),          Intent(INOUT) :: pointpar
        Integer(C_INT),       Intent(INOUT) :: numclocr
        Type(C_PTR),          Intent(INOUT) :: clocurves
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1954
    End Interface

    c_dim = dim

    Call c_s1954(surf%cptr, point, c_dim, epsco, epsge, c_numclopt,       &
                 c_pointpar_p, c_numclocr, c_clocurves_p, c_stat) 

    numclocr = c_numclocr
    numclopt = c_numclopt
    If (numclopt > 0) Then
      If (C_ASSOCIATED(c_pointpar_p)) Then
        Call C_F_Pointer(c_pointpar_p, pointparp,[numclopt*2])
        If (ALLOCATED(pointpar)) DEALLOCATE(pointpar)
        ALLOCATE(pointpar(numclopt*2), SOURCE=0.0_REAL64)
        pointpar(:) = pointparp(:)
        NULLIFY(pointparp)
        Call c_free(c_pointpar_p)
      EndIf
    EndIf

    If (numclocr > 0) Then

      If (C_ASSOCIATED(c_clocurves_p)) Then
        Call C_F_Pointer(c_clocurves_p, cclocurves, [numclocr])
        If (ALLOCATED(clocurves)) Deallocate(clocurves)
        ALLOCATE(clocurves(numclocr))
        Do i=1,numclocr
          If (C_ASSOCIATED(cclocurves(i))) Then
            clocurves(i)%cptr = cclocurves(i)
          Else
            clocurves(i)%cptr = C_NULL_PTR
          End If
          Call IntCurveCtoF(clocurves(i))
        EndDo
      EndIf
    EndIf

    If (ASSOCIATED(cclocurves)) NULLIFY(cclocurves)
    c_clocurves_p = C_NULL_PTR

    stat = c_stat

  End Subroutine s1954

!---------------------------------- s1958 -------------------------------------

  Subroutine s1958(psurf, epoint, idim, aepsco, aepsge, gpar, dist, jstat)

!! PURPOSE
!!   s1958 - Find the closest point between a surface and a point. The method
!!           is fast and should work well in clear cut cases, but there is no
!!           guarantee it will find the right solution. As long as it doesn’t
!!           fail, it will find exactly one point. In other cases, use s1954

!! INTERFACE
!!   Subroutine s1958(psurf, epoint, idim, aepsco, aepsge, gpar, dist, jstat)
!!     Type(SISLsurf),     Intent(IN)    :: psurf 
!!     Real(REAL64),       Intent(IN)    :: epoint(*)
!!     Integer,            Intent(IN)    :: idim
!!     Real(REAL64),       Intent(IN)    :: aepsco
!!     Real(REAL64),       Intent(IN)    :: aepsge
!!     Real(REAL64),       Intent(INOUT) :: gpar(2) 
!!     Real(REAL64),       Intent(INOUT) :: dist
!!     Integer,            Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLsurf),     Intent(IN)    :: psurf 
    Real(REAL64),       Intent(IN)    :: epoint(*)
    Integer,            Intent(IN)    :: idim
    Real(REAL64),       Intent(IN)    :: aepsco
    Real(REAL64),       Intent(IN)    :: aepsge
    Real(REAL64),       Intent(INOUT) :: gpar(2) 
    Real(REAL64),       Intent(INOUT) :: dist
    Integer,            Intent(INOUT) :: jstat

    Integer(C_INT) :: c_idim, c_jstat

    Interface
      Subroutine c_s1958(psurf, epoint, idim, aepsco, aepsge, gpar, dist,      &
                         jstat) BIND(C,name="s1958")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: psurf 
        Real(C_DOUBLE),       Intent(IN)    :: epoint(*)
        Integer(C_INT), VALUE               :: idim
        Real(C_DOUBLE), VALUE               :: aepsco
        Real(C_DOUBLE), VALUE               :: aepsge
        Real(C_DOUBLE),       Intent(INOUT) :: gpar(2) 
        Real(C_DOUBLE),       Intent(INOUT) :: dist 
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1958
    End Interface

    c_idim    = idim
    gpar(1:2) = 0.0_REAL64
    Call c_s1958(psurf%cptr, epoint, c_idim, aepsco, aepsge, gpar,          &
                 dist, c_jstat)

    jstat = c_jstat

  End Subroutine s1958

!---------------------------------- s1775 -------------------------------------

  Subroutine s1775(surf, point, dim, epsge, start, end, guess, clpar, stat)

!! PURPOSE
!!   s1775 - Newton iteration on the distance function between a surface and
!!           a point, to find a closest point or an intersection point. If a bad
!!           choice for the guess parameters is given in, the iteration may end
!!           at a local, not global closest point.

!! INTERFACE
!!   Subroutine s1775(surf, point, dim, epsge, start, end, guess, clpar, stat)
!!     Type(SISLsurf),  Intent(IN)    :: surf
!!     Real(REAL64),    Intent(IN)    :: point(*)
!!     Integer,         Intent(IN)    :: dim 
!!     Real(REAL64),    Intent(IN)    :: epsge
!!     Real(REAL64),    Intent(IN)    :: start(*)
!!     Real(REAL64),    Intent(IN)    :: end(*)
!!     Real(REAL64),    Intent(IN)    :: guess(*)
!!     Real(REAL64),    Intent(INOUT) :: clpar(*) 
!!     Integer,         Intent(INOUT) :: stat

    Implicit NONE

    Type(SISLsurf),  Intent(IN)    :: surf
    Real(REAL64),    Intent(IN)    :: point(*)
    Integer,         Intent(IN)    :: dim 
    Real(REAL64),    Intent(IN)    :: epsge
    Real(REAL64),    Intent(IN)    :: start(*)
    Real(REAL64),    Intent(IN)    :: end(*)
    Real(REAL64),    Intent(IN)    :: guess(*)
    Real(REAL64),    Intent(INOUT) :: clpar(*) 
    Integer,         Intent(INOUT) :: stat

    Integer(C_INT) :: c_stat, c_dim

    Interface
      Subroutine c_s1775(surf, point, dim, epsge, start, end, guess, clpar,   &
                         stat) BIND(C,name="s1775")

        IMPORT :: C_PTR, C_DOUBLE, C_INT
        Implicit NONE

        Type(C_PTR),    VALUE               :: surf
        Real(C_DOUBLE),       Intent(IN)    :: point(*)
        Integer(C_INT), VALUE               :: dim 
        Real(C_DOUBLE), VALUE               :: epsge
        Real(C_DOUBLE),       Intent(IN)    :: start(*)
        Real(C_DOUBLE),       Intent(IN)    :: end(*)
        Real(C_DOUBLE),       Intent(IN)    :: guess(*)
        Real(C_DOUBLE),       Intent(INOUT) :: clpar(*)
        Integer(C_INT),       Intent(INOUT) :: stat

      End Subroutine c_s1775
    End Interface

    c_dim      = dim
    clpar(1:2) = 0.0_REAL64
    Call c_s1775(surf%cptr, point, c_dim, epsge, start, end, guess, clpar,    &
                 c_stat)

    stat = c_stat

  End Subroutine s1775

!---------------------------------- s1921 -------------------------------------

  Subroutine s1921(ps, edir, idim, aepsco, aepsge, jpt, gpar, jcrv, wcurve,    &
                   jstat)

!! PURPOSE
!!   s1921 - Find the absolute extremal points/curves of a surface along a given
!!           direction

!! INTERFACE
!!   Subroutine s1921(ps, edir, idim, aepsco, aepsge, jpt, gpar, jcrv,        &
!!                    wcurve, jstat)
!!     Type(SISLsurf),                  Intent(IN)    :: ps
!!     Real(REAL64),                    Intent(IN)    :: edir(*)
!!     Integer,                         Intent(IN)    :: idim
!!     Real(REAL64),                    Intent(IN)    :: aepsco
!!     Real(REAL64),                    Intent(IN)    :: aepsge
!!     Integer,                         Intent(INOUT) :: jpt
!!     Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: gpar(:)
!!     Integer,                         Intent(INOUT) :: jcrv
!!     Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: wcurve(:)
!!     Integer,                         Intent(INOUT) :: jstat
  
    Implicit NONE

    Type(SISLsurf),                  Intent(IN)    :: ps
    Real(REAL64),                    Intent(IN)    :: edir(*)
    Integer,                         Intent(IN)    :: idim
    Real(REAL64),                    Intent(IN)    :: aepsco
    Real(REAL64),                    Intent(IN)    :: aepsge
    Integer,                         Intent(INOUT) :: jpt
    Real(REAL64),       ALLOCATABLE, Intent(INOUT) :: gpar(:)
    Integer,                         Intent(INOUT) :: jcrv
    Type(SISLIntcurve), ALLOCATABLE, Intent(INOUT) :: wcurve(:)
    Integer,                         Intent(INOUT) :: jstat

    Integer :: i

    Integer(C_INT)        :: c_idim, c_jpt, c_jcrv, c_jstat
    Type(C_PTR)           :: c_wcurve_p, c_gpar_p
    Type(C_PTR),  Pointer :: cwcurve(:)
    Real(REAL64), Pointer :: gparp(:)

    Interface
      Subroutine c_s1921(ps, edir, idim, aepsco, aepsge, jpt, gpar, jcrv,      &
                         wcurve, jstat)                                        &
                         BIND(C,name="s1921")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: ps
        Real(C_DOUBLE),       Intent(IN)    :: edir(*)
        Integer(C_INT), VALUE               :: idim
        Real(C_DOUBLE), VALUE               :: aepsco
        Real(C_DOUBLE), VALUE               :: aepsge
        Integer(C_INT),       Intent(INOUT) :: jpt
        Type(C_PTR),          Intent(INOUT) :: gpar
        Integer(C_INT),       Intent(INOUT) :: jcrv
        Type(C_PTR),          Intent(INOUT) :: wcurve
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1921
    End Interface

    c_idim = idim

    Call c_s1921(ps%cptr, edir, c_idim, aepsco, aepsge, c_jpt,                 &
                 c_gpar_p, c_jcrv, c_wcurve_p, c_jstat)

    jcrv = c_jcrv
    jpt  = c_jpt
    If (jpt > 0) Then
      If (C_ASSOCIATED(c_gpar_p)) Then
        Call C_F_Pointer(c_gpar_p, gparp,[jpt*2])
        If (ALLOCATED(gpar)) DEALLOCATE(gpar)
        ALLOCATE(gpar(jpt*2), SOURCE=0.0_REAL64)
        gpar(:) = gparp(:)
        NULLIFY(gparp)
        Call c_free(c_gpar_p)
      EndIf
    EndIf
    If (jcrv > 0) Then

      If (C_ASSOCIATED(c_wcurve_p)) Then
        Call C_F_Pointer(c_wcurve_p, cwcurve, [jcrv])
        If (ALLOCATED(wcurve)) DEALLOCATE(wcurve)
        ALLOCATE(wcurve(jcrv))
        Do i=1,jcrv
          If (C_ASSOCIATED(cwcurve(i))) Then
            wcurve(i)%cptr = cwcurve(i)
          Else
            wcurve(i)%cptr = C_NULL_PTR
          End If
          Call IntCurveCtoF(wcurve(i))
        EndDo
      EndIf
    EndIf

    If (ASSOCIATED(cwcurve)) NULLIFY(cwcurve)
    c_wcurve_p = C_NULL_PTR

    jstat = c_jstat

  End Subroutine s1921

! newbox defined in Curve_Interrogation

!---------------------------------- s1989 -------------------------------------

  Subroutine s1989(ps, emax, emin, jstat)

!! PURPOSE
!!   s1989 - Find the bounding box of a surface.
!!           NOTE: The geometric bounding box is returned also in the ra-
!!           tional case, that is the box in homogeneous coordinates is NOT
!!           computed.

!! INTERFACE
!!   Subroutine s1989(ps, emax, emin, jstat)
!!     Type(SISLsurf),              Intent(IN)    :: ps 
!!     Real(REAL64),   ALLOCATABLE, Intent(INOUT) :: emax(:)
!!     Real(REAL64),   ALLOCATABLE, Intent(INOUT) :: emin(:)
!!     Integer,                     Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLsurf),              Intent(IN)    :: ps 
    Real(REAL64),   ALLOCATABLE, Intent(INOUT) :: emax(:)
    Real(REAL64),   ALLOCATABLE, Intent(INOUT) :: emin(:)
    Integer,                     Intent(INOUT) :: jstat

    Integer                 :: idim
    Integer(C_INT)          :: c_jstat
    Type(C_PTR)             :: c_emax_p, c_emin_p
    Real(C_DOUBLE), Pointer :: emaxp(:)
    Real(C_DOUBLE), Pointer :: eminp(:)
    Interface
      Subroutine c_s1989(ps, emax, emin, jstat)                                &
                         BIND(C,name="s1989")

        IMPORT :: C_PTR, C_INT
        Implicit NONE

        Type(C_PTR),    VALUE               :: ps 
        Type(C_PTR),          Intent(INOUT) :: emax
        Type(C_PTR),          Intent(INOUT) :: emin
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1989
    End Interface

    Call c_s1989(ps%cptr, c_emax_p, c_emin_p, c_jstat) 

    idim = ps%idim

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

  End Subroutine s1989

! newdir defined in Curve_Interrogation

!---------------------------------- s1987 -------------------------------------

  Subroutine s1987(ps, aepsge, jgtpi, gaxis, cang, jstat)

!! PURPOSE
!!   s1987 - Find the direction cone of a surface.

!! INTERFACE
!!   Subroutine s1987(ps, aepsge, jgtpi, gaxis, cang, jstat)
!!     Type(SISLsurf),             Intent(IN)    :: ps 
!!     Real(REAL64),               Intent(IN)    :: aepsge 
!!     Integer,                    Intent(INOUT) :: jgtpi 
!!     Real(REAL64),  ALLOCATABLE, Intent(INOUT) :: gaxis(:)
!!     Real(REAL64),               Intent(INOUT) :: cang
!!     Integer,                    Intent(INOUT) :: jstat

    Implicit NONE

    Type(SISLsurf),             Intent(IN)    :: ps 
    Real(REAL64),               Intent(IN)    :: aepsge 
    Integer,                    Intent(INOUT) :: jgtpi 
    Real(REAL64),  ALLOCATABLE, Intent(INOUT) :: gaxis(:)
    Real(REAL64),               Intent(INOUT) :: cang
    Integer,                    Intent(INOUT) :: jstat

    Integer                 :: idim
    Integer(C_INT)          :: c_jgtpi,c_jstat
    Type(C_PTR)             :: c_gaxis_p
    Real(C_DOUBLE), Pointer :: gaxisp(:)
    Interface
      Subroutine c_s1987(ps, aepsge, jgtpi, gaxis, cang, jstat)                &
                         BIND(C,name="s1987")

        IMPORT :: C_PTR, C_INT, C_DOUBLE
        Implicit NONE

        Type(C_PTR),    VALUE               :: ps
        Real(C_DOUBLE), VALUE               :: aepsge 
        Integer(C_INT),       Intent(INOUT) :: jgtpi
        Type(C_PTR),          Intent(INOUT) :: gaxis
        Real(C_DOUBLE),       Intent(INOUT) :: cang  
        Integer(C_INT),       Intent(INOUT) :: jstat

      End Subroutine c_s1987
    End Interface

    Call c_s1987(ps%cptr, aepsge, c_jgtpi, c_gaxis_p, cang, c_jstat) 

    idim = ps%idim
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
      cang = 0.0_REAL64
    EndIf

    jstat = c_jstat

  End Subroutine s1987

End Module Surface_Interrogation
