
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

Program example03

! This program will create a 'blend curve' to connect the 
! endpoints of the two curves generated in the two previous
! example programs.  The routines used are s1227 and s1606

  USE forSISLdata
  USE forSISL,    ONLY: s1606, s1227, freeCurve, readSISLcurve,  writeSISLcurve

  Implicit NONE

  Integer                       :: jstat, jstat1, jstat2, stream_1,  stream_2, &
                                   os, idim, blendtype, order, temp
  Real(REAL64)                  :: epsge
  Real(REAL64)                  :: c1_endpar 
  Real(REAL64)                  :: c2_endpar 
  Real(REAL64)                  :: c1_endpoint(3)
  Real(REAL64)                  :: c2_endpoint(3)
  Type(SISLcurve)               :: c1 
  Type(SISLcurve)               :: c2 
  Type(SISLcurve)               :: blend_curve
  Character(LEN=:), ALLOCATABLE :: IN_FILE_CURVE_1
  Character(LEN=:), ALLOCATABLE :: IN_FILE_CURVE_2
  Character(LEN=:), ALLOCATABLE :: OUT_FILE_CURVE
  Character(LEN=1)              :: ans


  IN_FILE_CURVE_1 = "example01_curve.g2"
  IN_FILE_CURVE_2 = "example02_curve.g2"
  OUT_FILE_CURVE  = "example03_curve.g2"

  ans = " "
  Print *,''
  Write(OUTPUT_UNIT, '(" Example03: ")')  
  Print *,''
  Write(OUTPUT_UNIT, '(A)')                                                    &
    "This program will create a 'blend curve' to connect the ",                &
    "endpoints of the two curves generated in the two previous",               &
    "example programs.  The routines used are s1227 and s1606. ",              &
    "Input: " //IN_FILE_CURVE_1//" and "//IN_FILE_CURVE_2,                     &
    "Output: "// OUT_FILE_CURVE 
  Print *,''
  Write(OUTPUT_UNIT, '(" To proceed press enter, or q to quit : ")',           &
        ADVANCE="NO")
  Read(*,'(A)') ans
  If (ans == "q") Then
    Print *,''
    Print *,' Adios !!! '
  End If

  Open(newunit=stream_1, FILE=IN_FILE_CURVE_1, FORM="FORMATTED",               &
       STATUS="UNKNOWN")
  Open(newunit=stream_2, FILE=IN_FILE_CURVE_2, FORM="FORMATTED",               &
       STATUS="UNKNOWN")

  Call readSISLcurve(stream_1, c1)
  Call readSISLcurve(stream_2, c2)

  epsge     = 1.0E-5_REAL64
  blendtype = 0
  idim      = 3
  order     = 4
  c1_endpar = c1%et(c1%in+1)
  c2_endpar = c2%et(c2%in+1)

  c1_endpoint = 0.0_REAL64
  c2_endpoint = 0.0_REAL64

! compute endpoint positions of both curves

  temp = 0
  Call s1227(c1,           & ! input curve
              0,           & ! evaluate position only (no derivatives)
              c1_endpar,   & ! end parameter
              temp,        & ! indicates param. interval (not interesting for our purposes)
              c1_endpoint, & ! this is what we want to calculate (3D position)
              jstat1)        ! status variable (0 if everything all right)

  temp = 0
  Call s1227(c2,           & ! input curve
              0,           & ! evaluate position only (no derivatives)
              c2_endpar,   & ! end parameter
              temp,        & ! indicates param. interval (not interesting for our purposes)
              c2_endpoint, & ! this is what we want to calculate (3D position)
              jstat2)        ! status variable (0 if everything all right)


  If (jstat1<0 .OR. jstat2<0) Then
    Print *,''
    STOP " Error occured inside call to SISL routine (s1227) "
  Else If(jstat1>0 .OR. jstat2>0) Then
    Print *,''
    Print *,' WARNING: warning occured inside call to SISL routine (s1227)'
  EndIf


! compute blend curve

  Call s1606(c1,           & ! the first input curve
             c2,           & ! the second input curve
             epsge,        & ! geometric tolerance
             c1_endpoint,  & ! endpoint of curve 1 (geometric)
             c2_endpoint,  & ! endpoint of curve 2 (geometric)
             blendtype,    & ! type of blend curve (circle, conic, polynomial)
             idim,         & ! dimension (3D)
             order,        & ! order of generated spline curve
             blend_curve,  & ! the generated curve
             jstat)          ! status message

  If (jstat<0) Then
    Print *,''
    ERROR STOP " Error occured inside call to SISL routine (s1606) "
  ElseIf (jstat>0) Then
    Print *,''
    Print *,' WARNING: warning occured inside call to SISL routine (s1606)'
  EndIf

  Open (newunit=os,  FILE=OUT_FILE_CURVE,  FORM="FORMATTED",                  &
        STATUS="UNKNOWN")

! output blend_curve

  Call writeSISLCurve( blend_curve, os)

! cleanup

  Call freeCurve(blend_curve)
  Call freeCurve(c1)
  Call freeCurve(c2)

  If (ALLOCATED(IN_FILE_CURVE_1)) DEALLOCATE(IN_FILE_CURVE_1)
  If (ALLOCATED(IN_FILE_CURVE_2)) DEALLOCATE(IN_FILE_CURVE_2) 
  If (ALLOCATED(OUT_FILE_CURVE))  DEALLOCATE(OUT_FILE_CURVE) 

  Close(os)
  Close(stream_1)
  Close(stream_2)

  STOP

End Program example03 
