
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

Program example09

! This program generates a series of surfaces that 
! each interpolate an array of (internally defined) points in
! 3D space using routine s1537

  USE forSISLdata
  USE forSISL,     ONLY: s1537, freeSurf, writeSISLsurface, writeSISLpoints

  Implicit NONE

  Integer :: i, os_surf, os_pts, num_points_u, num_points_v, idim, num_surf,  &
             jstat
  Integer :: order_u(4)
  Integer :: order_v(4)

  Real(REAL64) :: points(75) 
  Real(REAL64) :: u_par(5) 
  Real(REAL64) :: v_par(5) 

  Type(SISLsurf) :: result_surf 

  Character(LEN=:), ALLOCATABLE :: OUT_FILE_SURFACE
  Character(LEN=:), ALLOCATABLE :: OUT_FILE_POINTS
  Character(LEN=1)              :: ans

  OUT_FILE_SURFACE = "example09_surf.g2"
  OUT_FILE_POINTS  = "example09_points.g2"

  ans = " "
  Print *,''
  Write(OUTPUT_UNIT, '(" Example09: ")')  
  Print *,''
  Write(OUTPUT_UNIT, '(A)')                                                    &
    "  This program generates a series of surfaces that ",                     &
    "  each interpolate an array of (internally defined) points in ",          &
    "  3D space.  The results will be written to file in forSISL format.",     &
    "  The generated surfaces will be written to the file ",                   &
       OUT_FILE_SURFACE//". The interpolated points will be ",                 &
    "  written to the file "// OUT_FILE_POINTS// ".  The ",                    &
    "  interpolating routine used is s1534.  When inspecting the ",            &
    "  result, note the effect of the varying order of the surfaces. ",        &
    "  Note also the 'artificial' bumps generated due to the ",                &
    "  interpolation criterion"
 
  Print *,''
  Write(OUTPUT_UNIT, '(" To proceed press enter, or q to quit : ")',           &
        ADVANCE="NO")
  Read(*,'(A)') ans
  If (ans == "q") Then
    Print *,''
    Print *,' Adios !!! '
  End If

  Open(newunit=os_surf, FILE=OUT_FILE_SURFACE, FORM="FORMATTED",               &
       STATUS="UNKNOWN")
  Open(newunit=os_pts, FILE=OUT_FILE_POINTs, FORM="FORMATTED",                 &
       STATUS="UNKNOWN")


  num_points_u = 5
  num_points_v = 5

  points = REAL([ 0, 0, 0,  1, 0, 0,  2, 0, 0,  3, 0, 0,  4, 0, 0,             &
                  0, 1, 0,  1, 1, 0,  2, 1, 0,  3, 1, 0,  4, 1, 0,             &
                  0, 2, 0,  1, 2, 0,  2, 2, 1,  3, 2, 0,  4, 2, 0,             &
                  0, 3, 0,  1, 3, 0,  2, 3, 0,  3, 3, 0,  4, 3, 0,             &
                  0, 4, 0,  1, 4, 0,  2, 4, 0,  3, 4, 0,  4, 4, 0], REAL64)
   
  u_par  = REAL([0, 1, 2, 3, 4], REAL64)
  v_par  = REAL([0, 1, 2, 3, 4], REAL64)

  idim     = 3
  num_surf = 4
  
  order_u = [2, 3, 4, 4]
  order_v = [2, 3, 4, 2]

  jstat          = 0

! Generate tensor product surfaces of different orders

  Do i=1, num_surf
    jstat = 0
    Call s1537(points,       & ! pointer to the array of points to interpolate
               num_points_u, & ! number of interpolating points along the 'u' parameter
               num_points_v, & ! number of interpolating points along the 'v' parameter
               idim,         & ! dimension of the Euclidean space
               u_par,        & ! pointer to the 'u' parameter values of the points
               v_par,        & ! pointer to the 'v' parameter values of the points
               0,            & ! no additional condition along edge 1
               0,            & ! no additional condition along edge 2
               0,            & ! no additional condition along edge 3
               0,            & ! no additional condition along edge 4
               order_u(i),   & ! the order of the generated surface in the 'u' parameter
               order_v(i),   & ! the order of the generated surface in the 'v' parameter
               1,            & ! open surface in the u direction
               1,            & ! open surface in the v direction 
               result_surf,  & ! the generated surface
               jstat)          ! status variable
    If (C_ASSOCIATED(result_surf%cptr) .EQV. .FALSE.) Then
      Print *,''
      Print *,' i surf = ', i
      STOP " result_surf%cptr is not associated"
    EndIf
    If (jstat<0) Then
      Print *,''
      STOP " Error occured inside call to SISL routine (s1537) "
    Else If(jstat>0) Then
      Print *,''
      Print *,' WARNING: warning occured inside call to SISL routine (s1537)'
    End If

! output surfaces and free result_surf

    Call writeSISLsurface(result_surf, os_surf)
    Call freeSurf(result_surf)
   
  End Do

! Write out surface points

  Call writeSISLpoints(num_points_u*num_points_v, points, os_pts)

  If (ALLOCATED(OUT_FILE_SURFACE)) DEALLOCATE(OUT_FILE_SURFACE) 
  If (ALLOCATED(OUT_FILE_POINTS))  DEALLOCATE(OUT_FILE_POINTS)

  Close(os_surf) 
  Close(os_pts) 

  STOP
 
End Program example09
