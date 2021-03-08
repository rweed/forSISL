
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

Program example08

! This program generates two non-intersecting curves (from
! internal data), and computes their mutual closest points
! using SISL routine s1955 and s1227.

  USE forSISLdata
  USE forSISL,    ONLY: newCurve, freeCurve, freeIntCurve, s1955, s1227,       &
                        writeSISLcurve, writeSISLpoints

  Implicit NONE

  Integer :: i, os_cv1, os_cv2, os_pts, c1_number, c1_order, c2_number,        &
             c2_order, idim, temp, num_int_curves, num_cl_points, jstat,       &
             jstat1, jstat2

  Real(REAL64)                      :: epsco
  Real(REAL64)                      :: epsge
  Real(REAL64)                      :: c1_knots(8) 
  Real(REAL64)                      :: c1_coef(12)
  Real(REAL64)                      :: c2_knots(8)
  Real(REAL64)                      :: c2_coef(12)
  Real(REAL64), ALLOCATABLE         :: intpar1(:) 
  Real(REAL64), ALLOCATABLE         :: intpar2(:) 
  Real(REAL64), ALLOCATABLE, TARGET :: point_coords_3D(:,:,:) 
  Real(REAL64), Pointer, Contiguous :: point_ptr(:)

  Type(SISLcurve)                 :: c1 
  Type(SISLcurve)                 :: c2
  Type(SISLIntcurve), ALLOCATABLE :: intcurve(:) 

  Character(LEN=:), ALLOCATABLE :: OUT_FILE_CURVE_1
  Character(LEN=:), ALLOCATABLE :: OUT_FILE_CURVE_2
  Character(LEN=:), ALLOCATABLE :: OUT_FILE_POINT
  Character(LEN=1)              :: ans


  OUT_FILE_CURVE_1 = "example08_curve_1.g2"
  OUT_FILE_CURVE_2 = "example08_curve_2.g2"
  OUT_FILE_POINT   = "example08_closestpoints.g2"

  ans = " "
  Print *,''
  Write(OUTPUT_UNIT, '(" Example08: ")')  
  Print *,''
  Write(OUTPUT_UNIT, '(A)')                                                    &
    "  This program generates two non-intersecting curves (from ",             &
    "  internal data), and computes their mutual closest points, ",            &
    "  using SISL routine s1955.  The curves will be saved to the ",           &
    "  two files "// OUT_FILE_CURVE_1 // " and "//OUT_FILE_CURVE_2,            &
    "  The closest points will be saved to the file "//OUT_FILE_POINT
 
  Print *,''
  Write(OUTPUT_UNIT, '(" To proceed press enter, or q to quit : ")',           &
        ADVANCE="NO")
  Read(*,'(A)') ans
  If (ans == "q") Then
    Print *,''
    Print *,' Adios !!! '
  End If

  Open(newunit=os_cv1, FILE=OUT_FILE_CURVE_1, FORM="FORMATTED",               &
       STATUS="UNKNOWN")
  Open(newunit=os_cv2, FILE=OUT_FILE_CURVE_2, FORM="FORMATTED",               &
       STATUS="UNKNOWN")
  Open(newunit=os_pts, FILE=OUT_FILE_POINT, FORM="FORMATTED",               &
       STATUS="UNKNOWN")

  idim      = 3

  c1_number = 4
  c1_order  = 4

  c1_coef = REAL([0, 0, 0,                                                    &
                  0, 1, 0,                                                    &
                  1, 1, 0,                                                    &
                  1, 0, 0], REAL64)

  c1_knots = REAL([0, 0, 0, 0, 1, 1, 1, 1], REAL64)

  c2_number = 4
  c2_order  = 4

  c2_coef = REAL( [-2.,  0., 0.,                                               &
                    0., 1.2, 0.,                                               &
                    1., 1.2, 0.,                                               &
                    3.,  0., 0.], REAL64)

  c2_knots = REAL([0, 0, 0, 0, 1, 1, 1, 1], REAL64)

! generating curves from internal data

  Call newCurve(c1_number, & ! num. of control points
                c1_order,  & ! spline order
                c1_knots,  & ! knotvector
                c1_coef,   & ! control points
                1,         & ! kind = polynomial B-spline curve
                idim,      & ! dimension of space (3D)
                1,         & ! copy input arrays
                c1)

   Call newCurve(c2_number, & ! num. of control points
                 c2_order,  & ! spline order
                 c2_knots,  & ! knotvector
                 c2_coef,   & ! control points
                 1,         & ! kind = polynomial B-spline curve
                 idim,      & ! dimension of space (3D)
                 1,         & ! copy input arrays
                 c2)

  If (C_ASSOCIATED(c1%cptr) .EQV. .FALSE. .OR.                            &
      C_ASSOCIATED(c2%cptr) .EQV. .FALSE.) Then
    Print *,''
    STOP " Error occured while generating curves."
  End If

  epsco     = 1.0E-15_REAL64
  epsge     = 1.0E-5_REAL64
  num_cl_points  = 0
  num_int_curves = 0
  jstat          = 0
    

! Find closet points between c1 and c2

  Call s1955(c1,              & ! first curve 
             c2,              & ! second curve
             epsco,           & ! computational resolution
             epsge,           & ! geometry resolution
             num_cl_points,   & ! number of single closest points
             intpar1,         & ! pointer to array of parameter values curve1
             intpar2,         & ! pointer to array of parameter values curve2
             num_int_curves,  & ! number of detected intersection curves
             intcurve,        & ! pointer to array of detected intersection curves.
             jstat)

  If (jstat<0) Then
    Print *,''
    STOP " Error occured inside call to SISL routine (s1955) "
  Else If(jstat>0) Then
    Print *,''
    Print *,' WARNING: warning occured inside call to SISL routine (s1955)'
  End If

  jstat = 0

  Print *,''
  Write(OUTPUT_UNIT,'(" Number of pairs of closest points detected: ", i0)')   &
        num_cl_points
  Print *,''
  Write(OUTPUT_UNIT,'(" Number of intersection curves detected: ", i0)')     &
        num_int_curves
  Print *,''

  ALLOCATE(point_coords_3D(3,num_cl_points,2), SOURCE=0.0_REAL64)

! Get intersection points

  Do i=1, num_cl_points
    jstat1 = 0
    jstat2 = 0
    temp   = 0
    Call s1227(c1,         & ! we evaluate on the first curve
               0,          & ! calculate no derivatives
               intpar1(i), & ! parameter value on which to evaluate
               temp,       & ! not used for our purposes (gives parameter interval)
               point_coords_3D(:,i,1), & ! result written here
               jstat1)     

    Call s1227(c2,         & ! we evaluate on the first curve
               0,          & ! calculate no derivatives
               intpar2(i), & ! parameter value on which to evaluate
               temp,       & ! not used for our purposes (gives parameter interval)
               point_coords_3D(:,i,2), & ! result written here
               jstat1)     

    If (jstat1<0 .OR. jstat2<0) Then
      Print *,''
      STOP " Error occured inside call to SISL routine (s1227) "
    Else If(jstat1>0 .OR. jstat2>0) Then
      Print *,''
      Print *,' WARNING: warning occured inside call to SISL routine (s1227)'
    EndIf

  End Do

! output curves and points to files

  Call writeSISLCurve(c1, os_cv1)
  Call writeSISLCurve(c2, os_cv2)
  point_ptr(1:3*num_cl_points*2) => point_coords_3D(:,:,:) ! flatten 3D array
  Call writeSISLPoints(num_cl_points*2, point_ptr, os_pts)

! Free curves, close files, and DEALLOCATE parameter arrays etc.

  Call freeCurve(c1)
  Call freeCurve(c2)

! Deallocate these to make valgrind happy with gfortran 9
  If (ALLOCATED(OUT_FILE_CURVE_1)) DEALLOCATE(OUT_FILE_CURVE_1)
  If (ALLOCATED(OUT_FILE_CURVE_2)) DEALLOCATE(OUT_FILE_CURVE_2) 
  If (ALLOCATED(OUT_FILE_POINT))   DEALLOCATE(OUT_FILE_POINT)

  Close(os_cv1)
  Close(os_cv2)
  Close(os_pts)

  If (ALLOCATED(intpar1)) DEALLOCATE(intpar1) 
  If (ALLOCATED(intpar2)) DEALLOCATE(intpar2)
  NULLIFY(point_ptr)
  If (ALLOCATED(point_coords_3D)) DEALLOCATE(point_coords_3D)
 
  If (ALLOCATED(intcurve)) Then
    Do i=1, num_int_curves
      Call freeIntCurve(intcurve(i))
    EndDo
    DEALLOCATE(intcurve)
  End If

  STOP
 
End Program example08 
