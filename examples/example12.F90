
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
!  (https://github.com/SINTEF-Geometry/SISL). It is assumed that users
!  will download and install the SISL software separate from this code.
!  Users are required to honor the SISL license clauses that cover usage
!  for both commercial and non-commercial applications. See the copy of 
!  the SISL license information and copyright that occompanies this software.

 
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

!! Original SISL License since this file is a derived work

!  Copyright (C) 1998, 2000-2007, 2010, 2011, 2012, 2013 SINTEF ICT,
!  Applied Mathematics, Norway.
! 
!  Contact information: E-mail: tor.dokken@sintef.no                      
!  SINTEF ICT, Department of Applied Mathematics,                         
!  P.O. Box 124 Blindern,                                                 
!  0314 Oslo, Norway.                                                     
! 
!  This file is part of SISL.
! 
!  SISL is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation, either version 3 of the
!  License, or (at your option) any later version. 
! 
!  SISL is distributed in the hope that it will be useful,        
!  but WITHOUT ANY WARRANTY; without even the implied warranty of         
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          
!  GNU Affero General Public License for more details.
! 
!  You should have received a copy of the GNU Affero General Public
!  License along with SISL. If not, see
!  <http://www.gnu.org/licenses/>.
! 
!  In accordance with Section 7(b) of the GNU Affero General Public
!  License, a covered work must retain the producer line in every data
!  file that is created or manipulated using SISL.
! 
!  Other Usage
!  You can be released from the requirements of the license by purchasing
!  a commercial license. Buying such a license is mandatory as soon as you
!  develop commercial activities involving the SISL library without
!  disclosing the source code of your own applications.
! 
!  This file may be used in accordance with the terms contained in a
!  written agreement between you and SINTEF ICT. 
! 

Program example12

!  This program computes all intersection points between a
!  curve and a surface using s1227 and s1858.  The curve and surface
!  where generated in previous examples
 
  USE forSISLdata
  USE forSISL,    ONLY: s1227, s1858, freeCurve, freeSurf, freeIntCurve,       &
                        readSISLcurve, readSISLsurface, writeSISLpoints

  Implicit NONE

  Integer :: i, is_cv, is_sf, os, num_int_curves, num_int_points, temp,        &
             jstat

  Real(REAL64)                      :: epsco
  Real(REAL64)                      :: epsge
  Real(REAL64), ALLOCATABLE         :: intpar_surf(:) 
  Real(REAL64), ALLOCATABLE         :: intpar_curve(:) 
  Real(REAL64), ALLOCATABLE, TARGET :: point_coords_3D(:,:) 
  Real(REAL64), Pointer, Contiguous :: point_ptr(:)

  Type(SISLcurve)                 :: curve 
  Type(SISLsurf)                  :: surf 
  Type(SISLIntcurve), ALLOCATABLE :: intcurve(:) 

  Character(LEN=:), ALLOCATABLE :: IN_FILE_CURVE
  Character(LEN=:), ALLOCATABLE :: IN_FILE_SURFACE
  Character(LEN=:), ALLOCATABLE :: OUT_FILE_POINT
  Character(LEN=1)              :: ans


  IN_FILE_CURVE   = "example04_curve.g2"
  IN_FILE_SURFACE = "example10_surf.g2"
  OUT_FILE_POINT  = "example12_isectpoints.g2"

  ans = " "
  Print *,''
  Write(OUTPUT_UNIT, '(" Example12: ")')  
  Print *,''
  Write(OUTPUT_UNIT, '(A)')                                                    &
    "  This program computes all intersection points between a ",              &
    "  curve and a surface.  The curve and surface in question has ",          &
    "  been generated by earlier example programs (example4 and ",             &
    "  example10), so you should run these first.  The resulting ",            &
    "  points will be written to the file "//OUT_FILE_POINT

 
  Print *,''
  Write(OUTPUT_UNIT, '(" To proceed press enter, or q to quit : ")',           &
        ADVANCE="NO")
  Read(*,'(A)') ans
  If (ans == "q") Then
    Print *,''
    Print *,' Adios !!! '
  End If

  Open(newunit=is_cv, FILE=IN_FILE_CURVE, FORM="FORMATTED",                    &
       STATUS="UNKNOWN")
  Open(newunit=is_sf, FILE=IN_FILE_SURFACE, FORM="FORMATTED",                  &
       STATUS="UNKNOWN")
  Open(newunit=os, FILE=OUT_FILE_POINT, FORM="FORMATTED",                      &
       STATUS="UNKNOWN")

! read surface and curve from file

  Call readSISLsurface(is_sf, surf)
  Call readSISLcurve(is_cv,  curve)
  
  epsco     = 1.0E-15_REAL64
  epsge     = 1.0E-5_REAL64
  num_int_points  = 0
  num_int_curves = 0
  jstat          = 0

! Find all intersections between surf and curve

  Call s1858(surf,           & ! the surface
             curve,          & ! the curve
             epsco,          & ! computational resolution
             epsge,          & ! geometry resolution
             num_int_points, & ! number of single intersection points
             intpar_surf,    & ! pointer to array of parameter values for the surface
             intpar_curve,   & !          for the curve
             num_int_curves, & ! number of detected intersection curves
             intcurve,       & ! pointer to array of detected intersection curves.
             jstat)             ! status variable

  If (jstat<0) Then
    Print *,''
    STOP " Error occured inside call to SISL routine (s1858) "
  Else If(jstat>0) Then
    Print *,''
    Print *,' WARNING: warning occured inside call to SISL routine (s1858)'
  End If

  jstat = 0

  Print *,''
  Write(OUTPUT_UNIT,'(" Number of intersection points detected: ", i0)')   &
        num_int_points
  Print *,''
  Write(OUTPUT_UNIT,'(" Number of intersection curves detected: ", i0)')     &
        num_int_curves
  Print *,''

  ALLOCATE(point_coords_3D(3,num_int_points), SOURCE=0.0_REAL64)

! Get intersection points and first derivative 

  Do i=1, num_int_points
    jstat = 0
    temp  = 0
    Call s1227(curve,           & ! we evaluate on the first curve
               0,               & ! calculate no derivatives
               intpar_curve(i), & ! parameter value on which to evaluate
               temp,            & ! not used for our purposes (gives parameter interval)
               point_coords_3D(:,i), & ! result written here
               jstat)     


    If (jstat<0) Then
      Print *,''
      STOP " Error occured inside call to SISL routine (s1227) "
    Else If(jstat>0) Then
      Print *,''
      Print *,' WARNING: warning occured inside call to SISL routine (s1227)'
    EndIf

  End Do

! output intersection points

  point_ptr(1:3*num_int_points) => point_coords_3D(:,:) ! flatten 3D array
  Call writeSISLPoints(num_int_points, point_ptr, os)

! free surf and curve

  Call freeSurf(surf)
  Call freeCurve(curve)

  If(ALLOCATED(IN_FILE_CURVE))   DEALLOCATE(IN_FILE_CURVE)
  If(ALLOCATED(IN_FILE_SURFACE)) DEALLOCATE(IN_FILE_SURFACE) 
  If(ALLOCATED(OUT_FILE_POINT))  DEALLOCATE(OUT_FILE_POINT)
 
  Close(is_cv)
  Close(is_sf)
  Close(os)

! DEALLOCATE curve and surface parameter arrays etc.

  If (ALLOCATED(intpar_curve)) DEALLOCATE(intpar_curve) 
  If (ALLOCATED(intpar_surf))  DEALLOCATE(intpar_surf)
  NULLIFY(point_ptr)
  If (ALLOCATED(point_coords_3D)) DEALLOCATE(point_coords_3D)
 
  If (ALLOCATED(intcurve)) Then
    Do i=1, num_int_curves
      Call freeIntCurve(intcurve(i))
    EndDo
    DEALLOCATE(intcurve)
  End If

  STOP
 
End Program example12 
