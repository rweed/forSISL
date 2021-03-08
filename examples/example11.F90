
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

Program example11

!  This program generates a cylindrical surface using s1021

  USE forSISLdata
  USE forSISL,     ONLY: s1021, freeSurf, writeSISLsurface

  Implicit NONE

  Integer                       :: jstat, os_surf
  Real(REAL64)                  :: height 
  Real(REAL64)                  :: ellipse_ratio
  Real(REAL64)                  :: bottom_pos(3)
  Real(REAL64)                  :: bottom_axis(3)
  Real(REAL64)                  :: axis_dir(3)
  Type(SISLsurf)                :: result_surf 
  Character(LEN=:), ALLOCATABLE :: OUT_FILE_SURFACE
  Character(LEN=1)              :: ans


  OUT_FILE_SURFACE = "example11_surf.g2"

  ans = " "
  Print *,''
  Write(OUTPUT_UNIT, '(" Example11: ")')  
  Print *,''
  Write(OUTPUT_UNIT, '(A)')                                                    &
    "  This program generates a cylindrical surface and saves it ",            &
    "  to the file "//OUT_FILE_SURFACE
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

  bottom_pos    = REAL([10, 1, -1], REAL64) 
  bottom_axis   = REAL([5,  0,  0], REAL64)
  ellipse_ratio = 1.2_REAL64 
  axis_dir      = REAL([0,  0,  1], REAL64)
  height        = 35.0_REAL64 
  jstat  = 0

! generate a surface representing a truncated cylinder

  Call s1021(bottom_pos,      & ! bottom position of the cylinder
             bottom_axis,     & ! the axis of the base of the cylinder
             ellipse_ratio,   & ! the ellipticalness of the base of the cylinder
             axis_dir,        & ! direction of the main axis
             height,          & ! height of the cylinder
             result_surf,     & ! resulting surface
             jstat)             ! status variable
 
  If (jstat < 0) Then
    Print *,''
    STOP " Error occured inside call to SISL routine (s1024) "
  Else If(jstat > 0) Then
    Print *,''
    Print *,' WARNING: warning occured inside call to SISL routine (s1024)'
  EndIf

! output surface and free result_surf

  Call writeSISLsurface(result_surf, os_surf)

  Call freeSurf(result_surf)
  If(ALLOCATED(OUT_FILE_SURFACE)) DEALLOCATE(OUT_FILE_SURFACE)
 
  Close (os_surf)

  STOP

End Program example11
