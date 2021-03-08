
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

Module forSISLio

!! Module forSISLio provides routines to read and write Curve and Surface object
!! data in standard Fortran sequential formatted data format. It mimics the
!! GoReadWrite routines found in the SISL examples/streaming directory but makes
!! no attempt to duplicate the C++ output exactly. These routines define a
!! basic IO capability for the Fortran Curve and Surface objects.

  USE forSISLdata,     ONLY: REAL64, C_ASSOCIATED, SISLcurve, SISLsurf
  USE forSISLcurves,   ONLY: newCurve
  USE forSISLsurfaces, ONLY: newSurf

  Implicit NONE

  Integer, PARAMETER :: HEADER_SIZE              = 4
  Integer, PARAMETER :: CURVE_INSTANCE_TYPE      = 100 
  Integer, PARAMETER :: SURFACE_INSTANCE_TYPE    = 200 
  Integer, PARAMETER :: POINTCLOUD_INSTANCE_TYPE = 400 
  Integer, PARAMETER :: MAJOR_VERSION            = 1 
  Integer, PARAMETER :: MINOR_VERSION            = 0

  PRIVATE :: REAL64, C_ASSOCIATED, SISLcurve, SISLsurf

Contains

  Function determine_SISL_instance_type(is) RESULT(itype)

!! PURPOSE 
!!   Reads header record of input files to determine in file contains the
!!   appropriate data type for curve or surface io

!! INTERFACE
!!   Function determine_SISL_instance_type(is) RESULT(itype)
!!     Integer, Intent(IN) :: is
!!     Integer             :: itype 

    Implicit NONE

    Integer, Intent(IN) :: is
    Integer             :: itype 

    Read(is, *) itype 

  End Function determine_SISL_instance_type

  Subroutine read_basis(is, n, k, knots)

!! PURPOSE
!!   Reads knot vector information for curve or surface objects. All io is done
!!   using Fortran list directed IO

!! INTERFACE
!!   Subroutine read_basis(is, n, k, knots)
!!     Integer,                    Intent(IN)    :: is
!!     Integer,                    Intent(INOUT) :: n, k
!!     Real(REAL64),  ALLOCATABLE, Intent(INOUT) :: knots(:)

    Implicit NONE

    Integer,                    Intent(IN)    :: is
    Integer,                    Intent(INOUT) :: n, k
    Real(REAL64),  ALLOCATABLE, Intent(INOUT) :: knots(:)

    Read(is,*) n, k
    If (ALLOCATED(knots)) DEALLOCATE(knots)
    ALLOCATE(knots(n+k), SOURCE=0.0_REAL64)
    Read(is,*) knots(:)

  End Subroutine read_basis

  Subroutine write_basis(os, n, k, knots)

!! PURPOSE
!!   Writes knot vector data for curve and surface objects. Integers are
!!   written using "i0,' ',i0" format. Reals are written using "500(g0.7,' ')"
!!   format

!! INTERFACE
!!   Subroutine write_basis(os, n, k, knots)
!!     Integer,        Intent(IN) :: os
!!     Integer,        Intent(IN) :: n, k
!!     Real(REAL64),   Intent(IN) :: knots(:)

    Implicit NONE

    Integer,        Intent(IN) :: os
    Integer,        Intent(IN) :: n, k
    Real(REAL64),   Intent(IN) :: knots(:)

    Write(os,'(i0," ",i0)') n, k
    Write(os,'(500(g0.7," "))') knots(1:n+k)

  End Subroutine write_basis

  Subroutine readSISLSurface(SISL_file, surf)

!! PURPOSE
!!   Read SISL surface object data

!! INTERFACE
!!   Subroutine readSISLSurface(SISL_file, surf)
!!     Integer,        Intent(IN)    :: SISL_file
!!     Type(SISLsurf), Intent(INOUT) :: surf
 
    Implicit NONE

    Integer,        Intent(IN)    :: SISL_file
    Type(SISLsurf), Intent(INOUT) :: surf
 
    Integer :: ikind, idim, rational, num_coefs_1, num_coefs_2, order_1,      &
               order_2, coef_size, copy

    Real(REAL64), ALLOCATABLE :: knots_1(:)
    Real(REAL64), ALLOCATABLE :: knots_2(:)
    Real(REAL64), ALLOCATABLE :: coef(:)

    If (determine_SISL_instance_type(SISL_file) /= SURFACE_INSTANCE_TYPE) Then
      Print *,''
      STOP "file given to readSISLSurface() does not contain surface type"
    End If
  
!! read knot vector data
   
    Read(SISL_file, *) idim, rational
    ikind = MERGE(2,1,(rational > 0)) 
    Call read_basis(SISL_file, num_coefs_1, order_1, knots_1) 
    Call read_basis(SISL_file, num_coefs_2, order_2, knots_2)

!! read coefficient data

    coef_size = num_coefs_1 * num_coefs_2 * MERGE(idim+1, idim, (rational > 0))
    ALLOCATE(coef(coef_size), SOURCE = 0.0_REAL64)
    Read(SISL_file, *) coef(:)

    copy = 1

!! Call newSurf to create a Fortran surface object

    Call newSurf(num_coefs_1, num_coefs_2, order_1, order_2, knots_1, knots_2, &
                 coef, ikind, idim, copy, surf) 

  End Subroutine readSISLSurface

  Subroutine writeSISLSurface(surf, SISL_file)

!! PURPOSE
!!   Write SISL surface object data

!! INTERFACE
!!   Subroutine writeSISLSurface(surf, SISL_file)
!!     Type(SISLsurf), Intent(IN) :: surf
!!     Integer,        Intent(IN) :: SISL_file
 
    Implicit NONE

    Type(SISLsurf), TARGET, Intent(IN) :: surf
    Integer,                Intent(IN) :: SISL_file
 
    Integer :: idim, rational, coef_size 

!! Check to see if surface C ptr is associated

    If (.NOT. C_ASSOCIATED(surf%cptr)) Then
      Print *,''
      STOP " surface given to writeSISLSurface is not defined"
    End If

!!  Write instance type, version numbers etc.

    Write(SISL_file,'(i0," ", i0," ", i0," 0")') SURFACE_INSTANCE_TYPE,      &
          MAJOR_VERSION, MINOR_VERSION

!! Write knot vector info

    idim = surf%idim
    rational = MERGE(1,0,(MOD(surf%ikind,2) == 0)) 
    Write(SISL_file, '(i0," ",i0)') idim, rational
    Call write_basis(SISL_file, surf%in1, surf%ik1, surf%et1) 
    Call write_basis(SISL_file, surf%in2, surf%ik2, surf%et2) 

!! Write coefficient info

    coef_size = surf%in1 * surf%in2 * MERGE(idim+1, idim, (rational > 0))
    If (rational == 0) Then
      Write(SISL_file, '(500(g0.7," "))') surf%ecoef(1:coef_size)
    Else
      Write(SISL_file, '(500(g0.7," "))') surf%rcoef(1:coef_size)
    EndIf
    FLUSH(SISL_file)

  End Subroutine writeSISLSurface

  Subroutine writeSISLCurve(curve, SISL_file)

!! PURPOSE
!!   Write SISL curve object data

!! INTERFACE
!!   Subroutine writeSISLCurve(curve, SISL_file)
!!     Type(SISLcurve), Intent(IN) :: curve 
!!     Integer,         Intent(IN) :: SISL_file
 
    Implicit NONE

    Type(SISLcurve), TARGET, Intent(IN) :: curve
    Integer,         Intent(IN) :: SISL_file
 
    Integer :: idim, rational, coef_size 

!! Check if curve C pointer is associated

    If (.NOT. C_ASSOCIATED(curve%cptr)) Then
      Print *,''
      STOP " curve given to writeSISLCurve is not defined"
    End If

!! Write instance type, version numbers etc

    Write(SISL_file,'(i0," ",i0," ",i0," 0")') CURVE_INSTANCE_TYPE,            &
          MAJOR_VERSION, MINOR_VERSION

!! Write knot data

    idim = curve%idim
    rational = MERGE(1,0,(MOD(curve%ikind,2) == 0)) 
    Write(SISL_file, '(i0," ",i0)') idim, rational
    Call write_basis(SISL_file, curve%in, curve%ik, curve%et) 

!! Write coefficient data

    coef_size = curve%in * MERGE(idim+1, idim, (rational > 0))
    If (rational == 0) Then
      Write(SISL_file, '(500(g0.7," "))') curve%ecoef(1:coef_size)
    Else
      Write(SISL_file, '(500(g0.7," "))') curve%rcoef(1:coef_size)
    EndIf
    FLUSH(SISL_file)

  End Subroutine writeSISLCurve

  Subroutine readSISLCurve(SISL_file, curve)

!! PURPOSE
!!   Read SISL curve object data

!! INTERFACE
!!   Subroutine readSISLcurve(SISL_file, curve)
!!     Integer,         Intent(IN)     :: SISL_file
!!     Type(SISLcurve), Intent(INOUT) :: curve 
 

    Implicit NONE

    Integer,         Intent(IN)    :: SISL_file
    Type(SISLcurve), Intent(INOUT) :: curve
 
    Integer :: ikind, idim, rational, num_coefs, order, coef_size, copy

    Real(REAL64), ALLOCATABLE :: knots(:)
    Real(REAL64), ALLOCATABLE :: coef(:)

    If (determine_SISL_instance_type(SISL_file) /= CURVE_INSTANCE_TYPE) Then
      Print *,''
      STOP "file given to readSISLCurve() does not contain curve type"
    End If

!! Read knot vector data
     
    Read(SISL_file, *) idim, rational
    ikind = MERGE(2,1,(rational > 0))
    Call read_basis(SISL_file, num_coefs, order, knots) 

!! Read coefficient data
    coef_size = num_coefs * MERGE(idim+1, idim, (rational > 0))
    ALLOCATE(coef(coef_size), SOURCE = 0.0_REAL64)
    Read(SISL_file, *) coef(:)
    copy = 1

!! Create new curve object

    Call newCurve(num_coefs, order, knots, coef, ikind, idim, copy, curve) 

  End Subroutine readSISLcurve

  Subroutine writeSISLPoints(num_points, coords, SISL_file)

!! PURPOSE
!!   Write point data generated by SISL routines

!! INTERFACE
!!   Subroutine WriteSISLPoints(SISL_file, coords)
!!     Integer,      Intent(IN)         :: num_points
!!     Real(REAL64), TARGET, Intent(IN) :: coords(:)
!!     Integer,              Intent(IN) :: SISL_file

    Implicit NONE

    Integer,              Intent(IN) :: num_points
    Real(REAL64), TARGET, Intent(IN) :: coords(:) 
    Integer,              Intent(IN) :: SISL_file

    If (SIZE(coords) /= 3*num_points) Then
      Print *,''
      STOP " dimension of point array given to writeSISLPoints is not 3*num_points"
    End If

    Write(SISL_file,'(i0," ", i0," ", i0," 4 255 255 0 255")')               &
          POINTCLOUD_INSTANCE_TYPE, MAJOR_VERSION, MINOR_VERSION

    Write(SISL_file, '(i0)') num_points
    Write(SISL_file, '(3(g0.7," "))') coords(:)
    FLUSH(SISL_file)

  End Subroutine writeSISLPoints

  Subroutine readSISLPoints(coords, SISL_file)

!! PURPOSE
!!   Read point data generated by SISL routines

!! INTERFACE
!!   Subroutine readSISLPoints(coords, SISL_file)
!!     Real(REAL64), ALLOCATABLE, Intent(INOUT) :: coords(:)
!!     Integer,                   Intent(IN)    :: SISL_file

    Implicit NONE

    Real(REAL64), ALLOCATABLE, Intent(INOUT) :: coords(:)
    Integer,                   Intent(IN)    :: SISL_file
 
    Integer :: i, num_points, dummy

    If (determine_SISL_instance_type(SISL_file) /= POINTCLOUD_INSTANCE_TYPE) Then
      Print *,''
      STOP "file given to readSISLCurve() does not contain pointcloud type"
    End If
    
    Do i=1,4
      Read(SISL_file, *) dummy
    EndDo
 
    Read(SISL_file, *) num_points
    If (ALLOCATED(coords)) DEALLOCATE(coords) 
    ALLOCATE(coords(num_points*3), SOURCE = 0.0_REAL64)
    Read(SISL_file, *) coords(:)

  End Subroutine readSISLPoints

End Module forSISLio
