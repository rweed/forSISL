
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

Program example10

! This program generates a series of curves, and then
! generates a lofted surface through these curves using s1538

  USE forSISLdata
  USE forSISL,     ONLY: newCurve, s1538, freeCurve, freeSurf,                 &
                         writeSISLsurface, writeSISLcurve

  Implicit NONE

  Integer :: jstat, num_curves, num_control_points, curve_order, loft_order,   &
             i, c, os_curves, os_surf
 
  Integer :: cv_type(16)

  Real(REAL64)                      :: x_dilate 
  Real(REAL64)                      :: y_increment 
  Real(REAL64)                      :: z_increment 
  Real(REAL64)                      :: curve_coef(12)
  Real(REAL64)                      :: curve_kvec(8)
  Real(REAL64), ALLOCATABLE, TARGET :: gpar(:)

  Type(SISLcurve)                   :: curves(16) 
  Type(SISLsurf)                    :: result_surf

  Character(LEN=:), ALLOCATABLE :: OUT_FILE_CURVES
  Character(LEN=:), ALLOCATABLE :: OUT_FILE_SURFACE
  Character(LEN=1)              :: ans


  OUT_FILE_CURVES   = "example10_curves.g2"
  OUT_FILE_SURFACE  = "example10_surf.g2"

  ans = " "
  Print *,''
  Write(OUTPUT_UNIT, '(" Exampl10: ")')  
  Print *,''
  Write(OUTPUT_UNIT, '(A)')                                                    &
    "  This program generates a series of curves, and then  ",                 &
    "  generates a lofted surface through these curves.  The ",                &
    "  routine used is s1538.  The resulting curves and surfaces ",            &
    "  are written respectively to "//OUT_FILE_CURVES//" and ",                &
     OUT_FILE_SURFACE

  Print *,''
  Write(OUTPUT_UNIT, '(" To proceed press enter, or q to quit : ")',           &
        ADVANCE="NO")
  Read(*,'(A)') ans
  If (ans == "q") Then
    Print *,''
    Print *,' Adios !!! '
  End If

  Open(newunit=os_curves, FILE=OUT_FILE_CURVES, FORM="FORMATTED",              &
       STATUS="UNKNOWN")
  Open(newunit=os_surf, FILE=OUT_FILE_SURFACE, FORM="FORMATTED",               &
       STATUS="UNKNOWN")

  num_curves         = 16
  num_control_points = 4
  curve_order        = 4
  loft_order         = 4

  curve_coef = REAL([0, 0, 0,                                                  &
                     1, 2, 0,                                                  &
                     2, 3, 0,                                                  &
                     3, 0, 0], REAL64)

  curve_kvec = REAL([0, 0, 0, 0, 1, 1, 1, 1], REAL64)

  x_dilate    = 1.15_REAL64
  y_increment = 2.0_REAL64
  z_increment = 2.0_REAL64

! Generate a series of curves and incrementally change control points

  Do c=1,num_curves

    Call newCurve(num_control_points, & ! number of curve control points
                  curve_order,        & ! the order of the curve 
                  curve_kvec,         & ! pointer to the knotvector
                  curve_coef,         & ! pointer to the control points,
                  1,                  & ! kind = 1: polynomial spline curve
                  3,                  & ! dimension of Euclidean space
                  1,                  & ! copy input arrays
                  curves(c))

    If (C_ASSOCIATED(curves(c)%cptr) .EQV. .FALSE.) Then
      Print *,''
      STOP " Unable to generate curve"
    End If

! change control points and output curves

    Call incrementally_change_control_points()
    Call writeSISLcurve(curves(c), os_curves)

  End Do

  cv_type = 1

! generate a lofted surface using B-spline curves in curves(:)

  Call s1538(num_curves,  & ! the number of curves that are used to generate the lofted surf.
             curves,      & ! pointer to the array of input curves
             cv_type,     & ! indicate the type of the input curves
             0.0_REAL64,  & ! start parameter for lofting direction
             1,           & ! flag telling that the surface should be open
             loft_order,  & ! order of the surface in the lofting direction
             0,           & ! do not adjust tangents in the derivative curve
             result_surf, & ! the resulting surface
             gpar,        & ! get the parametrization along the lofted dir 
             jstat)         ! status variable

  If (jstat < 0) Then
    Print *,''
    STOP " Error occured inside call to SISL routine (s1538) "
  Else If(jstat > 0) Then
    Print *,''
    Print *,' WARNING: warning occured inside call to SISL routine (s1538)'
  EndIf

! Write surface and free curves

  Call writeSISLsurface( result_surf, os_surf)

  If (ALLOCATED(gpar)) DEALLOCATE(gpar) 
  Do i=1, num_curves
    Call freeCurve(curves(i))
  EndDo

! Free surface

  Call freeSurf(result_surf)

  If(ALLOCATED(OUT_FILE_CURVES))  DEALLOCATE(OUT_FILE_CURVES)
  If(ALLOCATED(OUT_FILE_SURFACE)) DEALLOCATE(OUT_FILE_SURFACE)
  Close(os_surf)
  Close(os_curves)

  STOP

Contains

  Subroutine incrementally_change_control_points()

    Implicit NONE

    Integer :: p

    Do p=1,num_control_points

      curve_coef(3*(p-1) + 1) = curve_coef(3*(p-1) + 1) * x_dilate
      curve_coef(3*(p-1) + 3) = curve_coef(3*(p-1) + 3) + z_increment
 
    End Do

    curve_coef(5) = curve_coef(5) - 0.1_REAL64*y_increment 
    curve_coef(8) = curve_coef(8) + y_increment 

  End Subroutine incrementally_change_control_points

End Program example10 
