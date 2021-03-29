
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

Program example13

!  This program computes the intersection curves between two 
!  surfaces using s1310 and s1859. The two surfaces used where
!  generated in previous examples 

  USE forSISLdata
  USE forSISL,    ONLY: s1859, s1310, freeSurf, freeIntCurve, readSISLsurface, &
                        writeSISLsurface, writeSISLcurve

  Implicit NONE

  Integer :: i, is_sf1, is_sf2, os, num_int_curves, num_int_points, jstat 

  Real(REAL64)              :: epsco
  Real(REAL64)              :: epsge

  Real(REAL64), ALLOCATABLE :: intpar_surf_1(:) 
  Real(REAL64), ALLOCATABLE :: intpar_surf_2(:) 

  Type(SISLsurf) :: surf_1 
  Type(SISLsurf) :: surf_2
 
  Type(SISLIntcurve), ALLOCATABLE :: intcurve(:) 

  Character(LEN=:), ALLOCATABLE :: IN_FILE_SURFACE_1
  Character(LEN=:), ALLOCATABLE :: IN_FILE_SURFACE_2
  Character(LEN=:), ALLOCATABLE :: OUT_FILE_CURVE
  Character(LEN=1)              :: ans


  IN_FILE_SURFACE_1 = "example10_surf.g2"
  IN_FILE_SURFACE_2 = "example11_surf.g2"
  OUT_FILE_CURVE    = "example13_isectcurves.g2"

  ans = " "
  Print *,''
  Write(OUTPUT_UNIT, '(" Example13: ")')  
  Print *,''
  Write(OUTPUT_UNIT, '(A)')                                                    &
    "  This program computes the intersection curves between two ",            &
    "  surfaces.  The two surfaces used have been generated by ",              &
    "  earlier example programs (example10 and example11), so you",            &
    "  should run these first. The filenames for these surfaces ",             &
    "  are "// IN_FILE_SURFACE_1//" and "//IN_FILE_SURFACE_2//".",             &
    "  The resulting curves will be written to the file "//OUT_FILE_CURVE

 
  Print *,''
  Write(OUTPUT_UNIT, '(" To proceed press enter, or q to quit : ")',           &
        ADVANCE="NO")
  Read(*,'(A)') ans
  If (ans == "q") Then
    Print *,''
    STOP ' Adios !!! '
  End If

  Open(newunit=is_sf1, FILE=IN_FILE_SURFACE_1, FORM="FORMATTED",               &
       STATUS="UNKNOWN")
  Open(newunit=is_sf2, FILE=IN_FILE_SURFACE_2, FORM="FORMATTED",               &
       STATUS="UNKNOWN")
  Open(newunit=os, FILE=OUT_FILE_CURVE, FORM="FORMATTED",                      &
       STATUS="UNKNOWN")

! Read surf_1 and surf_2

  Call readSISLsurface(is_sf1, surf_1)
  Call readSISLsurface(is_sf2, surf_2)
  
  epsco          = 1.0E-15_REAL64
  epsge          = 1.0E-5_REAL64
  num_int_points = 0
  num_int_curves = 0
  jstat          = 0

! Find all intersection curves between surf_1 and surf_2

  Call s1859(surf_1,         & ! the first surface
             surf_2,         & ! the second surface 
             epsco,          & ! computational resolution
             epsge,          & ! geometry resolution
             num_int_points, & ! number of single intersection points
             intpar_surf_1,  & ! pointer to array of parameter values for the surface 1
             intpar_surf_2,  & !           for the surface 2
             num_int_curves, & ! number of detected intersection curves
             intcurve,       & ! pointer to array of detected intersection curves.
             jstat)             ! status variable
  If (jstat<0) Then
    Print *,''
    STOP " Error occured inside call to SISL routine (s1859) "
  Else If(jstat>0) Then
    Print *,''
    Print *,' WARNING: warning occured inside call to SISL routine (s1859)'
  End If

  jstat = 0

  Print *,''
  Write(OUTPUT_UNIT,'(" Number of intersection points detected: ", i0)')   &
        num_int_points
  Write(OUTPUT_UNIT,'(" Number of intersection curves detected: ", i0)')     &
        num_int_curves
  Print *,''

! March intersection points between surfaces

  Do i=1, num_int_curves
    jstat = 0
    Call s1310(surf_1,      & ! the first surface
               surf_2,      & ! the second surface
               intcurve(i), & ! pointer to the intersection curve object 
               epsge,       & ! geometric tolerance
               0.0_REAL64,  & ! maximum step size (ignored if <= 0)
               1,           & ! make only 3D curve (no 2D curve in parametric domain)
               0,           & ! don't draw the curve
               jstat)
    If (jstat<0) Then
      Print *,''
      STOP " Error occured inside call to SISL routine (s1310) "
    Else If(jstat>0) Then
      Print *,''
      Print *,' WARNING: warning occured inside call to SISL routine (s1310)'
    EndIf

! Output intersection curves

    Call writeSISLcurve(intcurve(i)%pgeom, os)

  End Do


! Clean up

  If (ALLOCATED(intcurve)) Then
    Do i=1, num_int_curves
      Call freeIntCurve(intcurve(i))
    EndDo
    DEALLOCATE(intcurve)
  End If

  If (ALLOCATED(intpar_surf_2)) DEALLOCATE(intpar_surf_2)
  If (ALLOCATED(intpar_surf_1)) DEALLOCATE(intpar_surf_1) 

  Call freeSurf(surf_2)
  Call freeSurf(surf_1)

  If (ALLOCATED(OUT_FILE_CURVE))    DEALLOCATE(OUT_FILE_CURVE)
  If (ALLOCATED(IN_FILE_SURFACE_2)) DEALLOCATE(IN_FILE_SURFACE_2) 
  If (ALLOCATED(IN_FILE_SURFACE_1)) DEALLOCATE(IN_FILE_SURFACE_1) 

  Close(is_sf1)
  Close(is_sf2)
  Close(os)

  STOP
 
End Program example13
