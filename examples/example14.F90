
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

Program example14

! This program demonstrates one of the data reduction
! capabilities in SISL.  It generates a dense point set by
! sampling from a predefined curve, and then try to generate a new
! curve with reduced points. The program uses s1227 and s1961 

  USE forSISLdata
  USE forSISL,    ONLY: newCurve, s1227, s1961, freeCurve, writeSISLcurve,     &
                        writeSISLpoints

  Implicit NONE

  Integer :: number, order, kind, dim, copy, os_cv, os_pts, num_samples,       &
             jstat, temp, max_iterations, i
  
  Real(REAL64)         :: tolerance
  Real(REAL64)         :: afctol 
  Real(REAL64)         :: param 
  Real(REAL64)         :: coef(9)
  Real(REAL64)         :: knots(6)
  Real(REAL64), TARGET :: sampled_point_coords(3,500) 

! These two arrays are dimensioned to num_samples (500) in C version. Should
! only be idim (3). Not sure why C overdimensions them

  Real(REAL64)         :: tolerance_vec(3)  
  Real(REAL64)         :: emxerr(3)

  Type(SISLcurve) :: sample_curve
  Type(SISLcurve) :: result_curve
  Character(LEN=:), ALLOCATABLE :: OUT_FILE_CURVE
  Character(LEN=:), ALLOCATABLE :: OUT_FILE_POINTS
  Character(LEN=1) :: ans

  OUT_FILE_CURVE  = "example14_curve.g2"
  OUT_FILE_POINTS = "example14_points.g2"

  ans = " "
  Print *,""
  Write(OUTPUT_UNIT,'(" Example14:")')
  Print *,""
  Write( OUTPUT_UNIT, '(A)')                                                   &
   "  This program demonstrates one of the data reduction ",                   &
   "  capabilities in SISL.  It generates a dense point set by ",              &
   "  sampling from a predefined curve, and then try to generate ",            &
   "  a curve that fits closely to to these samples, using as few ",           &
   "  control points as possible.  (In our case, since we know that ",         &
   "  the points were sampled from a simple curve, we already ",               &
   "  have knowledge about the ideal solution to this problem). ",             &
   "  The sampled points will be saved to the file "//OUT_FILE_POINTS,         &
   "  and the generated curve will be saved to the file ",                     &
   "  "//OUT_FILE_CURVE


  Print *,""
  Write(OUTPUT_UNIT, '("To proceed press enter, or q to quit : ")',            &
        ADVANCE="NO")
  Read(*,'(A)') ans
  If (ans == "q") Then
    Print *,""
    STOP " Adios!!! "
  End If
 
  number = 3 
  order  = 3
  kind   = 1
  dim    = 3
  copy   = 0

  knots  = REAL([0, 0, 0,1, 1, 1],REAL64)
  coef   = REAL([ 0, 0, 0,                                                     &
                  1, 1, 0,                                                     &
                  2, 0, 0], REAL64)

  num_samples = 500

  Open(newunit=os_cv, FILE=OUT_FILE_CURVE, FORM="FORMATTED",              &
       STATUS="UNKNOWN")
  Open(newunit=os_pts, FILE=OUT_FILE_POINTS, FORM="FORMATTED",            &
       STATUS="UNKNOWN")

! Define sample_curve

  Call newCurve(number, order, knots, coef, 1, 3, 0, sample_curve)
  If (C_ASSOCIATED(sample_curve%cptr) .EQV. .FALSE.) Then
    Print *,''
    STOP " Error occured while generating curve. "
  End If

! generate sample points

  Do i=1, num_samples
    param = REAL(i-1,REAL64)/REAL(num_samples-1, REAL64)
    temp = 0
    jstat = 0

    Call s1227(sample_curve, & ! the curve to sample from
               0,            & ! calculate no derivatives
               param,        & ! sample in this parameter
               temp,         & ! not used for our purposes (returns parameter interval)
               sampled_point_coords(:,i), & ! sampled point written here
               jstat)         ! status
     If (jstat < 0) Then
       Print *,'' 
       STOP "Error occured inside call to SISL routine s1227."
     ElseIf (jstat > 0) Then
       Print *,"WARNING: warning occured inside call to SISL routine 1227."
     EndIf
  End Do

  tolerance      = 1.0E-5_REAL64
  afctol         = 0.1_REAL64
  max_iterations = 150
  tolerance_vec  = tolerance
  jstat = 0

! Generate a B-spline curve approximation to point data

  Call s1961(RESHAPE(sampled_point_coords,SHAPE=[1500]), & ! array of points to be approximated
             num_samples,       & ! number of sample points
             3,                 & ! dimension of Euclidean space
             2,                 & ! flag indicating uniform parametrization
             NULL_ARRAY,        & ! we do not use this argument
             tolerance_vec,     & ! the pointwise tolerance(eeps)
             0,                 & ! number of fixed derivatives at left
             0,                 & ! number of fixed derivatives at right
             1,                 & ! flag indicating that we want an open curve
             afctol,            & ! distribution of tolerance
             max_iterations,    & ! maximum number of iterations in the routine
             3,                 & ! polynomial order of approximation
             result_curve,      & ! pointer to the generated curve
             emxerr,            & ! reporting the max deviation in each point
              jstat)
 
  If (jstat < 0) Then
    Print *,'' 
    STOP "Error occured inside call to SISL routine s1961."
  ElseIf (jstat > 0) Then
    Print *,"WARNING: warning occured inside call to SISL routine 1961."
  EndIf
  
  Print *,'' 
  Write(OUTPUT_UNIT,'(" Number of initial data points: ", i0)') num_samples
  Write(OUTPUT_UNIT,'(" Number of control points in final approximation: ", i0)') result_curve%in
  Write(OUTPUT_UNIT,'(" Max pointwise error is: ", g0.7)') MAXVAL(emxerr)
  Print *,''

! Write curves and points

  Call writeSISLpoints(num_samples, RESHAPE(sampled_point_coords, [1500]), os_pts)
  Call writeSISLcurve(result_curve, os_cv)

! Clean up

  Call freeCurve(result_curve)
  Call freeCurve(sample_curve)

! Valgrind shows these aren't being deallcated for gfortran 9

  If (ALLOCATED(OUT_FILE_CURVE)) DEALLOCATE(OUT_FILE_CURVE)
  If (ALLOCATED(OUT_FILE_POINTS)) DEALLOCATE(OUT_FILE_POINTS)

  Close(os_pts)
  Close(os_cv)
 
  STOP 
 
End Program example14 
