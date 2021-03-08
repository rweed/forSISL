   
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

Program example02

!  This program will generate a SISL spline curve object by 
!  interpolating a set of given points.

  USE forSISLdata
  USE forSISL,    ONLY: s1356, freeCurve, writeSISLPoints, writeSISLcurve

  Implicit NONE

  Integer                       :: num_points, jnbpar, jstat, os_points,       &
                                   os_curve
  Real(REAL64)                  :: cstartpar, cendpar
  Integer                       :: itype(6)
  Real(REAL64)                  :: points(18)
  Real(REAL64),     ALLOCATABLE :: gpar(:)
  Type(SISLcurve)               :: result_curve
  Character(LEN=:), ALLOCATABLE :: OUT_FILE_POINTS
  Character(LEN=:), ALLOCATABLE :: OUT_FILE_CURVE
  Character(LEN=1)              :: ans


  OUT_FILE_POINTS = "example02_points.g2"
  OUT_FILE_CURVE  = "example02_curve.g2"

  ans = " "
  Print *,''
  Write(OUTPUT_UNIT, '(" Example02: ")')  
  Print *,''
  Write(OUTPUT_UNIT, '(A)')                                                    &
    " This program will generate a SISL spline curve object by ",              &
    " interpolating a set of given points.  The parametrization ",             &
    " will be calculated automatically.  No derivative ",                      &
    " information is provided.  The routine used is s1356. The ",              &
    " points will be written in forSISL format to the file: "//OUT_FILE_POINTS,&
    " The resulting curve will be written in forSISL format to file : "//OUT_FILE_CURVE

  Print *,''
  Write(OUTPUT_UNIT, '(" To proceed press enter, or q to quit : ")',           &
        ADVANCE="NO")
  Read(*,'(A)') ans
  If (ans == "q") Then
    Print *,''
    Print *,' Adios !!! '
  End If

  num_points = 6
  cstartpar  = 0.0_REAL64
  itype      = [1, 1, 1, 1, 1, 1 ]
  points     = REAL( [0, 0, 0,                                                 &
                      1, 1, 0,                                                 &
                      2, -1, 0,                                                &
                      3, 0, 0,                                                 &
                      4, 1, 1,                                                 &
                      3, 0, 4], REAL64)

! Generate curve that interpolates points

  Call s1356(points,        &! pointer to where the point coordinates are stored
             num_points,    &! number of points to be interpolated
             3,             &! the dimension
             itype,         &! what type of information is stored at a particular point
             0,             &! no additional condition at start point
             0,             &! no additional condition at end point
             1,             &! open curve
             4,             &! order of the spline curve to be produced
             cstartpar,     &! parameter value to be used at start of curve
             cendpar,       &! parameter value at the end of the curve (to be determined)
             result_curve,  &! the resulting spline curve (to be determined)
             gpar,          &! array of parameter values of the points in the curve
                             ! (to be determined)
              jnbpar,       &! number of unique parameter values (to be determined)
              jstat)        ! status message

  If (jstat < 0) Then
    Print *,''
    STOP " Error occured inside call to SISL routine (s1356) "
  Else If(jstat > 0) Then
    Print *,''
    Print *,' WARNING: warning occured inside call to SISL routine (s1356)'
  EndIf

  Print *,''
  Write (OUTPUT_UNIT, '(" Total parameter interval of curve : [ ",g0.7," ", g0.7," ]")') cstartpar, cendpar
  Print *,''
  Write(OUTPUT_UNIT, '(" Point parameter values was decided to be: ")')
  Print *,''
  Write(OUTPUT_UNIT, '(80(g0.7," "))') gpar(:)

  Print *,''

  Open (newunit=os_points, FILE=OUT_FILE_POINTS, FORM="FORMATTED",             &
        STATUS="UNKNOWN")    
  Open (newunit=os_curve,  FILE=OUT_FILE_CURVE,  FORM="FORMATTED",             &
        STATUS="UNKNOWN")

! output curve and points

  Call writeSISLPoints(num_points, points, os_points)
  Call writeSISLCurve( result_curve, os_curve)

! free curve and gpar

  Call freeCurve(result_curve)
  If (ALLOCATED(gpar)) DEALLOCATE(gpar)

  If (ALLOCATED(OUT_FILE_POINTS)) DEALLOCATE(OUT_FILE_POINTS) 
  If (ALLOCATED(OUT_FILE_CURVE))  DEALLOCATE(OUT_FILE_CURVE)
 
  Close(os_points)
  Close(os_curve)

  STOP

End Program example02 
