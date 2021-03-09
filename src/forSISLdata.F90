   
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


Module forSISLdata

!! Module forSISLdata contains definitions of the derived type "objects" for 
!! SISL curve, surface, interface curve, bounding box, and direction cone
!! entities. In addition, subroutines are proviced for mapping data in
!! the C objects to their Fortran equivalents. Each Fortran version of
!! the C object contains a C pointer (type(C_PTR) to the C object. All
!! arrays etc in the C object are mapped to Fortran pointer arrays. All
!! scalar data is copied. All instances of other objects such as the
!! box and direction objects contained in a parent object are instances
!! of the Fortran version of the object. Enumeration data etc. defined
!! the the C header files (sisl.h etc) are redefined here


  USE, Intrinsic :: ISO_FORTRAN_ENV
  USE, Intrinsic :: ISO_C_BINDING

  Implicit NONE

  Enum, BIND(C)
    Enumerator :: NO_SEG_TYPE=0,                                               &
                  TANGENTIAL_BELT_LEFT,                                        &
                  TANGENTIAL_BELT_RIGHT,                                       &
                  LIMITING_SEG
  End Enum 

  Enum, BIND(C)
    Enumerator :: NO_SURFACE_TYPE=0,                                           &
                  TANGENTIAL_BELT
  End Enum 
  
  Enum, BIND(C)
    ENUMERATOR :: SI_RIGHT=1, SI_LEFT
  End Enum
  Enum, BIND(C)
    ENUMERATOR :: SI_UNDEF, SI_IN, SI_OUT, SI_ON, SI_AT

  End Enum

  Integer(C_INT), PARAMETER :: SISL_CRV_PERIODIC = -1_C_INT
  Integer(C_INT), PARAMETER :: SISL_CRV_OPEN     =  1_C_INT
  Integer(C_INT), PARAMETER :: SISL_CRV_CLOSED   =  0_C_INT
 
  Integer(C_INT), PARAMETER :: SISL_SURF_PERIODIC = -1_C_INT
  Integer(C_INT), PARAMETER :: SISL_SURF_OPEN     =  1_C_INT
  Integer(C_INT), PARAMETER :: SISL_SURF_CLOSED   =  0_C_INT

! C structures for interfacing with SISL functions
 
  Type, BIND(C) :: c_SISLdir  ! C direction cone object

    Integer(C_INT) :: igtpi   = 0_C_INT
    Type(C_PTR)    :: ecoef   = C_NULL_PTR
    Real(C_DOUBLE) :: aang    = 0.0_C_DOUBLE
    Type(C_PTR)    :: esmooth = C_NULL_PTR
  End Type

  Type, BIND(C) :: c_SISLbox  ! C bounding box object

    Type(C_PTR)    :: emax      = C_NULL_PTR
    Type(C_PTR)    :: emin      = C_NULL_PTR
    Integer(C_INT) :: imin      = 0_C_INT
    Integer(C_INT) :: imax      = 0_C_INT
    Type(C_PTR)    :: e2max(3)  = [C_NULL_PTR, C_NULL_PTR, C_NULL_PTR]
    Type(C_PTR)    :: e2min(3)  = [C_NULL_PTR, C_NULL_PTR, C_NULL_PTR]
    Real(C_DOUBLE) :: etol(3)   = [0.0_C_DOUBLE, 0.0_C_DOUBLE, 0.0_C_DOUBLE]
  End Type

  Type, BIND(C) :: c_SISLSegmentation ! C segmentation object

    Type(C_PTR)    :: seg_val  = C_NULL_PTR
    Type(C_PTR)    :: seg_type = C_NULL_PTR
    Integer(C_INT) :: num_seg  = 0_C_INT

  End Type

  Type, BIND(C) :: c_SISLCurve  ! C curve object

    Integer(C_INT) :: ik     = 0_C_INT
    Integer(C_INT) :: in     = 0_C_INT
    Type(C_PTR)    :: et     = C_NULL_PTR
    Type(C_PTR)    :: ecoef  = C_NULL_PTR
    Type(C_PTR)    :: rcoef  = C_NULL_PTR
    Integer(C_INT) :: ikind  = 0_C_INT
    Integer(C_INT) :: idim   = 0_C_INT
    Integer(C_INT) :: icopy  = 0_C_INT
    Type(C_PTR)    :: pdir   = C_NULL_PTR
    Type(C_PTR)    :: pbox   = C_NULL_PTR
    Integer(C_INT) :: cuopen = 0_C_INT

  End Type

  Type, BIND(C) :: c_SISLsurf   ! C surf object

    Integer(C_INT) :: ik1       = 0_C_INT
    Integer(C_INT) :: ik2       = 0_C_INT
    Integer(C_INT) :: in1       = 0_C_INT
    Integer(C_INT) :: in2       = 0_C_INT
    Type(C_PTR)    :: et1       = C_NULL_PTR
    Type(C_PTR)    :: et2       = C_NULL_PTR
    Type(C_PTR)    :: ecoef     = C_NULL_PTR
    Type(C_PTR)    :: rcoef     = C_NULL_PTR
    Integer(C_INT) :: ikind     = 0_C_INT
    Integer(C_INT) :: idim      = 0_C_INT
    Integer(C_INT) :: icopy     = 0_C_INT
    Type(C_PTR)    :: pdir      = C_NULL_PTR
    Type(C_PTR)    :: pbox      = C_NULL_PTR
    Integer(C_INT) :: use_count = 0_C_INT
    Integer(C_INT) :: cuopen_1   = 0_C_INT
    Integer(C_INT) :: cuopen_2   = 0_C_INT
    Type(C_PTR)    :: seg1      = C_NULL_PTR
    Type(C_PTR)    :: seg2      = C_NULL_PTR
    Integer(C_INT) :: sf_type   = 0_C_INT

  End Type

  Type, BIND(C) :: c_SISLIntCurve ! C interface curve object

    Integer(C_INT) :: ipoint    = 0_C_INT 
    Integer(C_INT) :: ipar1     = 0_C_INT 
    Integer(C_INT) :: ipar2     = 0_C_INT
    Type(C_PTR)    :: epar1     = C_NULL_PTR
    Type(C_PTR)    :: epar2     = C_NULL_PTR
    Type(C_PTR)    :: pgeom     = C_NULL_PTR
    Type(C_PTR)    :: ppar1     = C_NULL_PTR
    Type(C_PTR)    :: ppar2     = C_NULL_PTR
    Integer(C_INT) :: itype     = 0_C_INT 
    Integer(C_INT) :: pretop(4) = [0_C_INT, 0_C_INT, 0_C_INT, 0_C_INT]

  End Type

  Type :: R64_p  ! Local container to hold a Fortran REAL64 pointer instance
    Real(REAL64), Pointer :: ptr(:) => NULL()
  End Type


  Type :: SISLdir  ! Fortran direction cone object

    Integer               :: igtpi      = 0 
    Real(REAL64)          :: aang       = 0.0_REAL64
    Real(REAL64), Pointer :: ecoef(:)   => NULL() 
    Real(REAL64), Pointer :: esmooth(:) => NULL()
    Type(C_PTR)           :: cptr       = C_NULL_PTR
  End Type

  Type  :: SISLbox ! Fortran bounding box object

    Integer               :: imin     = 0
    Integer               :: imax     = 0
    Real(REAL64), Pointer :: emax(:)  => NULL()  
    Real(REAL64), Pointer :: emin(:)  => NULL()
    Type(R64_p)           :: e2max(3)
    Type(R64_p)           :: e2min(3)
    Real(REAL64)          :: etol(3)  = [0.0_REAL64, 0.0_REAL64, 0.0_REAL64]
    Type(C_PTR)           :: cptr     = C_NULL_PTR
  End Type

  Type :: SISLSegmentation ! Fortran segmentation object

    Real(REAL64), Pointer  :: seg_val(:) => NULL()
    Integer,      Pointer  :: seg_type(:) => NULL() 
    Integer                :: num_seg    = 0
    Type(C_PTR)            :: cptr       = C_NULL_PTR
  End Type

  Type :: SISLCurve   ! Fortran curve object

    Integer                :: ik        = 0
    Integer                :: in        = 0
    Real(REAL64), Pointer  :: et(:)     => NULL()
    Real(REAL64), Pointer  :: ecoef(:)  => NULL() 
    Real(REAL64), Pointer  :: rcoef(:)  => NULL()
    Integer                :: ikind     = 0
    Integer                :: idim      = 0
    Integer                :: icopy     = 0
    Type(SISLdir)          :: pdir      
    Type(SISLbox)          :: pbox        
    Integer                :: cuopen    = 0
    Type(C_PTR)            :: cptr      = C_NULL_PTR
  End Type

  Type :: SISLsurf   ! Fortran surf object

    Integer                 :: ik1      = 0
    Integer                 :: ik2      = 0
    Integer                 :: in1      = 0
    Integer                 :: in2      = 0
    Real(REAL64), Pointer   :: et1(:)   => NULL()
    Real(REAL64), Pointer   :: et2(:)   => NULL()
    Real(REAL64), Pointer   :: ecoef(:) => NULL() 
    Real(REAL64), Pointer   :: rcoef(:) => NULL()
    Integer                 :: ikind    = 0
    Integer                 :: idim     = 0
    Integer                 :: icopy    = 0
    Type(SISLdir)           :: pdir
    Type(SISLbox)           :: pbox
    Integer                 :: use_count = 0
    Integer                 :: cuopen_1  = 0
    Integer                 :: cuopen_2  = 0
    Type(SISLSegmentation)  :: seg1
    Type(SISLSegmentation)  :: seg2
    Integer                 :: sf_type  = 0
    Type(C_PTR)             :: cptr  = C_NULL_PTR

  End Type

  Type :: SISLIntCurve   ! Fortran interface curve object

    Integer                  :: ipoint    = 0 
    Integer                  :: ipar1     = 0 
    Integer                  :: ipar2     = 0
    Real(REAL64),    Pointer :: epar1(:)  => NULL() 
    Real(REAL64),    Pointer :: epar2(:)  => NULL()
    Type(SISLCurve)          :: pgeom
    Type(SISLCurve)          :: ppar1
    Type(SISLCurve)          :: ppar2
    Integer                  :: itype     = 0 
    Integer                  :: pretop(4) = [0, 0, 0, 0]
    Type(C_PTR)              :: cptr = C_NULL_PTR 
  End Type


   Real(REAL64) :: NULL_ARRAY(0) ! Zero length array used to mimic a C NULL

  Interface  ! interface to a wrapper around the C free function. In C_utils.c

    Subroutine c_free(ptr) BIND(C,name="c_free")
      IMPORT :: C_PTR
      Implicit NONE
      Type(C_PTR), VALUE :: ptr
    End Subroutine c_free

  End Interface

Contains

  Subroutine curveCtoF(curve)

!! PURPOSE
!!   curveCtoF maps data from the C curve object to Fortran curve object

!! INTERFACE
!!   Subroutine curveCtoF(curve)
!!     Type(SISLcurve), Intent(INOUT) :: curve

    Implicit NONE

    Type(SISLcurve), Intent(INOUT) :: curve

    Integer :: i, knum

    Type(c_SISLCurve), Pointer :: ccurve
    Type(c_SISLdir),   Pointer :: cpdir
    Type(c_SISLbox),   Pointer :: cpbox
 
    ccurve => NULL()
    cpdir  => NULL()
    cpbox  => NULL()

    If (ASSOCIATED(curve%et))           NULLIFY(curve%et)
    If (ASSOCIATED(curve%ecoef))        NULLIFY(curve%ecoef)
    If (ASSOCIATED(curve%rcoef))        NULLIFY(curve%rcoef)
    If (ASSOCIATED(curve%pdir%ecoef))   NULLIFY(curve%pdir%ecoef)
    If (ASSOCIATED(curve%pdir%esmooth)) NULLIFY(curve%pdir%esmooth)
    If (ASSOCIATED(curve%pbox%emax))    NULLIFY(curve%pbox%emax)
    If (ASSOCIATED(curve%pbox%emin))    NULLIFY(curve%pbox%emin)
    Do i=1,3
      If (ASSOCIATED(curve%pbox%e2max(i)%ptr))                         &
              NULLIFY(curve%pbox%e2max(i)%ptr)
      If (ASSOCIATED(curve%pbox%e2min(i)%ptr))                         &
              NULLIFY(curve%pbox%e2min(i)%ptr)
    EndDo

    If (C_ASSOCIATED(curve%cptr)) Then

      Call C_F_POINTER(curve%cptr, ccurve)
      curve%in     = ccurve%in
      curve%ik     = ccurve%ik
      curve%ikind  = ccurve%ikind
      curve%idim   = ccurve%idim
      curve%icopy  = ccurve%icopy
      curve%cuopen = ccurve%cuopen

      If (C_ASSOCIATED(ccurve%et)) Then
        Call C_F_Pointer(ccurve%et, curve%et,[curve%in+curve%ik])
      EndIf

      If (C_ASSOCIATED(ccurve%ecoef)) Then
        Call C_F_Pointer(ccurve%ecoef, curve%ecoef,[curve%in*curve%idim])
         
      EndIf

      If (C_ASSOCIATED(ccurve%rcoef)) Then
        Call C_F_Pointer(ccurve%rcoef, curve%rcoef, [curve%in*(curve%idim+1)])
      EndIf

      If (C_ASSOCIATED(ccurve%pdir)) Then
        Call C_F_Pointer(ccurve%pdir, cpdir)
        curve%pdir%igtpi = cpdir%igtpi
        curve%pdir%aang  = cpdir%aang
        If (C_ASSOCIATED(cpdir%ecoef)) Then
          Call C_F_Pointer(cpdir%ecoef, curve%pdir%ecoef, [curve%idim])
        End If  
        If (C_ASSOCIATED(cpdir%esmooth)) Then
          Call C_F_Pointer(cpdir%esmooth, curve%pdir%esmooth, [curve%idim])
        EndIf

      EndIf

      If (C_ASSOCIATED(ccurve%pbox)) Then
        Call C_F_POINTER(ccurve%pbox, cpbox)
        curve%pbox%etol = cpbox%etol
        curve%pbox%imax = cpbox%imax
        curve%pbox%imin = cpbox%imin
        If (curve%idim == 3) Then
          knum = 12  ! for some reason this is 12 in c newbox.
        ElseIf (curve%idim == 2) Then
          knum = 4
        Else
          knum = curve%idim
        EndIf
        If (C_ASSOCIATED(cpbox%emax)) Then
          Call C_F_POINTER(cpbox%emax, curve%pbox%emax, [knum])
        EndIf
        If (C_ASSOCIATED(cpbox%emin)) Then
          Call C_F_POINTER(cpbox%emin, curve%pbox%emin, [knum])
        EndIf

        Do i=1,3
          If (C_ASSOCIATED(cpbox%e2max(i))) Then
            Call C_F_POINTER(cpbox%e2max(i), curve%pbox%e2max(i)%ptr, [knum])
          EndIf
          If (C_ASSOCIATED(cpbox%e2min(i))) Then
            Call C_F_POINTER(cpbox%e2min(i), curve%pbox%e2min(i)%ptr, [knum])
          EndIf
        EndDo

      EndIf 

    EndIf

    If (ASSOCIATED(cpdir))  NULLIFY(cpdir)
    If (ASSOCIATED(cpbox))  NULLIFY(cpbox)
    If (ASSOCIATED(ccurve)) NULLIFY(ccurve)

  End Subroutine curveCtoF
  
  Subroutine dirCtoF(idim, dir)

!! PURPOSE
!!   dirCtoF maps data from the C direction cone object to Fortran dir object

!! INTERFACE
!!   Subroutine dirCtoF(idim, dir)
!!     Integer,       Intent(IN)    :: idim
!!     Type(SISLdir), Intent(INOUT) :: dir

    Implicit NONE

    Integer,       Intent(IN)    :: idim
    Type(SISLdir), Intent(INOUT) :: dir

    Type(c_SISLdir), Pointer :: cdir

    cdir => NULL()

    If (ASSOCIATED(dir%esmooth)) NULLIFY(dir%esmooth)
    If (ASSOCIATED(dir%ecoef))   NULLIFY(dir%ecoef)

    If (C_ASSOCIATED(dir%cptr)) Then
      Call C_F_POINTER(dir%cptr, cdir)
      dir%igtpi  = cdir%igtpi 
      dir%aang   = cdir%aang
      If (C_ASSOCIATED(cdir%ecoef)) Then
        Call C_F_POINTER(cdir%ecoef, dir%ecoef, [idim])
      EndIf 
      If (C_ASSOCIATED(cdir%esmooth)) Then
        Call C_F_POINTER(cdir%esmooth, dir%esmooth, [idim])
      EndIf
    End If

    If (ASSOCIATED(cdir)) NULLIFY(cdir)

  End Subroutine dirCtoF
 
  Subroutine boxCtoF(idim, box)

!! PURPOSE
!!   boxCtoF maps data from the C bounding box object to Fortran box object

!! INTERFACE
!!   Subroutine boxCtoF(idim, box)
!!     Integer,       Intent(IN)    :: idim
!!     Type(SISLbox), Intent(INOUT) :: box

    Implicit NONE

    Integer,       Intent(IN)    :: idim
    Type(SISLbox), Intent(INOUT) :: box
   
    Integer :: knum, i
    Type(c_SISLbox), Pointer :: cbox

    cbox => NULL()

    If (ASSOCIATED(box%emax)) NULLIFY(box%emax)
    If (ASSOCIATED(box%emin)) NULLIFY(box%emin)
    Do i=1,3
      If (ASSOCIATED(box%e2max(i)%ptr)) NULLIFY(box%e2max(i)%ptr)
      If (ASSOCIATED(box%e2min(i)%ptr)) NULLIFY(box%e2min(i)%ptr)
    EndDo

    If (C_ASSOCIATED(box%cptr)) Then
      Call C_F_POINTER(box%cptr, cbox)
      box%etol = cbox%etol
      box%imin = cbox%imin
      box%imax = cbox%imax
      If (idim == 3) Then
        knum = 12  ! for some reason this is 12 in c newbox.
      ElseIf (idim == 2) Then
        knum = 4
      Else
        knum = idim
      EndIf
      If (C_ASSOCIATED(cbox%emax)) Then
        Call C_F_POINTER(cbox%emax, box%emax, [knum])
      EndIf
      If (C_ASSOCIATED(cbox%emin)) Then
        Call C_F_POINTER(cbox%emin, box%emin, [knum])
      EndIf
      Do i=1,3
        If (C_ASSOCIATED(cbox%e2max(i))) Then
          Call C_F_POINTER(cbox%e2max(i), box%e2max(i)%ptr, [knum])
        EndIf
        If (C_ASSOCIATED(cbox%e2min(i))) Then
          Call C_F_POINTER(cbox%e2min(i), box%e2min(i)%ptr, [knum])
        EndIf
      EndDo
    End If 

    If (ASSOCIATED(cbox)) NULLIFY(cbox)

  End Subroutine boxCtoF 

  Subroutine IntCurveCtoF(intCurve)

!! PURPOSE
!!   IntCurveCtoF maps data from C intersection curve object to Fortran IntCurve
!!   object

!! INTERFACE
!!   Subroutine IntCurveCtoF(intCurve)
!!     Type(SISLIntCurve), Target, Intent(INOUT) :: intCurve

    Implicit NONE

    Type(SISLIntCurve), Target, Intent(INOUT) :: intCurve

    Integer :: ipoint, ipar1, ipar2
    Type(c_SISLIntCurve), Pointer :: cicurve

    cicurve => NULL()
    If (ASSOCIATED(intCurve%epar1)) NULLIFY(intCurve%epar1)
    If (ASSOCIATED(intCurve%epar2)) NULLIFY(intCurve%epar2)

    If (C_ASSOCIATED(intCurve%cptr)) Then

       Call C_F_Pointer(intCurve%cptr, cicurve)

       intCurve%ipoint = cicurve%ipoint
       intCurve%ipar1  = cicurve%ipar1
       intCurve%ipar2  = cicurve%ipar2
       intCurve%itype  = cicurve%itype
       intCurve%pretop = cicurve%pretop
       ipoint                     = intCurve%ipoint
       ipar1                      = intCurve%ipar1
       ipar2                      = intCurve%ipar2

       If (C_ASSOCIATED(cicurve%epar1)) Then
         Call C_F_Pointer(cicurve%epar1, intCurve%epar1, [ipoint*ipar1])
       EndIf 
       If (C_ASSOCIATED(cicurve%epar2)) Then
         Call C_F_Pointer(cicurve%epar2, intCurve%epar2, [ipoint*ipar2])
       EndIf
       If (C_ASSOCIATED(cicurve%pgeom)) Then
         intCurve%pgeom%cptr = cicurve%pgeom
         Call curveCtoF(intCurve%pgeom)
       EndIf
       If (C_ASSOCIATED(cicurve%ppar1)) Then
         intCurve%ppar1%cptr = cicurve%ppar1
         Call curveCtoF(intCurve%ppar1)
       EndIf
       If (C_ASSOCIATED(cicurve%ppar2)) Then
         intCurve%ppar2%cptr = cicurve%ppar2
         Call curveCtoF(intCurve%ppar2)
       EndIf

     End If
     If (ASSOCIATED(cicurve)) NULLIFY(cicurve)

  End Subroutine IntCurveCtoF 

  Subroutine surfCtoF(surf)

!! PURPOSE
!!   surfCtoF maps data from C surf object to Fortran surf object

!! INTERFACE
!!   Subroutine surfCtoF(surf)
!!     Type(SISLsurf), Intent(INOUT) :: surf

    Implicit NONE

    Type(SISLsurf), Intent(INOUT) :: surf

    Integer :: i, knum

    Type(c_SISLSurf),         Pointer :: csurf
    Type(c_SISLdir) ,         Pointer :: cpdir
    Type(c_SISLbox) ,         Pointer :: cpbox
    Type(c_SISLsegmentation), Pointer :: cseg1
    Type(c_SISLsegmentation), Pointer :: cseg2


    csurf => NULL() 
    cpdir => NULL() 
    cpbox => NULL() 
    cseg1 => NULL() 
    cseg2 => NULL()
 
    If (ASSOCIATED(surf%et1))          NULLIFY(surf%et1)
    If (ASSOCIATED(surf%et2))          NULLIFY(surf%et2)
    If (ASSOCIATED(surf%ecoef))        NULLIFY(surf%ecoef)
    If (ASSOCIATED(surf%rcoef))        NULLIFY(surf%rcoef)
    If (ASSOCIATED(surf%pdir%ecoef))   NULLIFY(surf%pdir%ecoef)
    If (ASSOCIATED(surf%pdir%esmooth)) NULLIFY(surf%pdir%esmooth)
    If (ASSOCIATED(surf%pbox%emax))    NULLIFY(surf%pbox%emax)
    If (ASSOCIATED(surf%pbox%emin))    NULLIFY(surf%pbox%emin)

    Do i=1,3
      If (ASSOCIATED(surf%pbox%e2max(i)%ptr))                          &
          NULLIFY(surf%pbox%e2max(i)%ptr)
      If (ASSOCIATED(surf%pbox%e2min(i)%ptr))                          &
          NULLIFY(surf%pbox%e2min(i)%ptr)
    EndDo
    If (ASSOCIATED(surf%seg1%seg_val))  NULLIFY(surf%seg1%seg_val)
    If (ASSOCIATED(surf%seg1%seg_type)) NULLIFY(surf%seg1%seg_type)
    If (ASSOCIATED(surf%seg2%seg_val))  NULLIFY(surf%seg2%seg_val)
    If (ASSOCIATED(surf%seg2%seg_type)) NULLIFY(surf%seg2%seg_type)

    If (C_ASSOCIATED(surf%cptr)) Then

      Call C_F_POINTER(surf%cptr, csurf)
      surf%in1       = csurf%in1
      surf%in2       = csurf%in2
      surf%ik1       = csurf%ik1
      surf%ik2       = csurf%ik2
      surf%ikind     = csurf%ikind
      surf%idim      = csurf%idim
      surf%icopy     = csurf%icopy
      surf%use_count = csurf%use_count
      surf%cuopen_1  = csurf%cuopen_1
      surf%cuopen_2  = csurf%cuopen_2
      surf%sf_type   = csurf%sf_type
      If (C_ASSOCIATED(csurf%et1)) Then
        Call C_F_Pointer(csurf%et1, surf%et1,[surf%in1+surf%ik1])
      EndIf
      If (C_ASSOCIATED(csurf%et2)) Then
        Call C_F_Pointer(csurf%et2, surf%et2,[surf%in2+surf%ik2])
      EndIf
      If (C_ASSOCIATED(csurf%ecoef)) Then
        Call C_F_Pointer(csurf%ecoef, surf%ecoef,[surf%in1*surf%in2*surf%idim])
      EndIf

      If (C_ASSOCIATED(csurf%rcoef)) Then
        Call C_F_Pointer(csurf%rcoef, surf%rcoef, [surf%in1*surf%in2*(surf%idim+1)])
      EndIf

      If (C_ASSOCIATED(csurf%pdir)) Then
        Call C_F_Pointer(csurf%pdir, cpdir)
        surf%pdir%igtpi = cpdir%igtpi
        surf%pdir%aang  = cpdir%aang
        If (C_ASSOCIATED(cpdir%ecoef)) Then
          Call C_F_Pointer(cpdir%ecoef, surf%pdir%ecoef, [surf%idim])
        End If  
        If (C_ASSOCIATED(cpdir%esmooth)) Then
          Call C_F_Pointer(cpdir%esmooth, surf%pdir%esmooth, [surf%idim])
        EndIf
!        surf%pdir%cptr = csurf%pdir
      EndIf

      If (C_ASSOCIATED(csurf%pbox)) Then
        Call C_F_POINTER(csurf%pbox, cpbox)
        surf%pbox%etol = cpbox%etol
        If (surf%idim == 3) Then
          knum = 12  ! for some reason this is 12 in c newbox.
        ElseIf (surf%idim == 2) Then
          knum = 4
        Else
          knum = surf%idim
        EndIf
        If (C_ASSOCIATED(cpbox%emax)) Then
          Call C_F_POINTER(cpbox%emax, surf%pbox%emax, [knum])
        EndIf
        If (C_ASSOCIATED(cpbox%emin)) Then
          Call C_F_POINTER(cpbox%emin, surf%pbox%emin, [knum])
        EndIf
        Do i=1,3
          If (C_ASSOCIATED(cpbox%e2max(i))) Then
            Call C_F_POINTER(cpbox%e2max(i), surf%pbox%e2max(i)%ptr, [knum])
          EndIf
          If (C_ASSOCIATED(cpbox%e2min(i))) Then
            Call C_F_POINTER(cpbox%e2min(i), surf%pbox%e2min(i)%ptr, [knum])
          EndIf
        EndDo

      EndIf 

      If (C_ASSOCIATED(csurf%seg1)) Then
        Call C_F_Pointer(csurf%seg1, cseg1)
        surf%seg1%num_seg = cseg1%num_seg
        If (C_ASSOCIATED(cseg1%seg_val)) Then
          Call C_F_Pointer(cseg1%seg_val, surf%seg1%seg_val, [cseg1%num_seg])
        End If  
        If (C_ASSOCIATED(cseg1%seg_type)) Then
          Call C_F_Pointer(cseg1%seg_type, surf%seg1%seg_type, [cseg1%num_seg])
        End If  
!        surf%seg1%cptr = csurf%seg1
      EndIf

      If (C_ASSOCIATED(csurf%seg2)) Then
        Call C_F_Pointer(csurf%seg2, cseg2)
        surf%seg2%num_seg = cseg2%num_seg
        If (C_ASSOCIATED(cseg2%seg_val)) Then
          Call C_F_Pointer(cseg2%seg_val, surf%seg2%seg_val, [cseg2%num_seg])
        End If  
        If (C_ASSOCIATED(cseg2%seg_type)) Then
          Call C_F_Pointer(cseg2%seg_type, surf%seg2%seg_type, [cseg2%num_seg])
        End If  
!        surf%seg2%cptr = csurf%seg2
      EndIf

    EndIf
    
    If (ASSOCIATED(cseg1))  NULLIFY(cseg1)
    If (ASSOCIATED(cseg2))  NULLIFY(cseg2)
    If (ASSOCIATED(cpdir))  NULLIFY(cpdir)
    If (ASSOCIATED(cpbox))  NULLIFY(cpbox)
    If (ASSOCIATED(csurf))  NULLIFY(csurf)

  End Subroutine surfCtoF

End Module forSISLdata 
