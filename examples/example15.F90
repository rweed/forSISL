
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

Program example15

! This program will do a simple ray-tracing of a bspline
! surface.  From one point P in space, we will extend lines
! ('rays') towards the surface, and calculate the point where
! the line intersects with the surface.  All points obtained
! in this way thus describe how the surface is 'visible' from
! P. The ray tracing routines are contained in this program
! as internal subroutines at the bottom of this file

! SISL routines s1518, s1421, s1856, and s1424 are used in the ray tracers

! Note that in this program, two ray-tracing routines are
! provided; (1) one slow and robust one, which would always
! work, and one quick but more fragile one (2), which bases
! its point estimates on previously obtained results, and
! thus is likely to fail when some parts of a surface shadows
! other parts.  The default method is (1); however the user
! can invoke the quick method by running the program with the
! additional argument 'q' (quick).  An examination of the
! obtained point cloud will in this case reveal erroneous
! behaviour where the examined surface 'folds over' itself.
! The user will notice, though, that the execution time is
! significantly shorter.  This method is therefore useful in
! some cases where surfaces are not self-obstructing.

! *** WARNING - only the robust version of the ray tracer produces the same
!               results as the C version. For some reason, the quick version
!               produces 5 fewer points than the C version. I'm still
!               trying to debug the cause of this anomoly - RW Feb 21

  USE forSISLdata
  USE forSISL,    ONLY: s1518, s1421, s1856, s1424, freeSurf, freeIntCurve,    &
                        readSISLsurface, writeSISLpoints

  Implicit NONE

  Integer :: is, os, HRES, VRES, varnum, arglen

  Real(REAL64) :: EPSCO
  Real(REAL64) :: EPSGE
  Real(REAL64) :: startTime
  Real(REAL64) :: endTime
  Real(REAL64) :: start_dom(2)
  Real(REAL64) :: end_dom(2)
  Real(REAL64) :: P(3)
  Real(REAL64) :: WIN_LL(3)
  Real(REAL64) :: WIN_LR(3)
  Real(REAL64) :: WIN_UL(3)
  Real(REAL64) :: WIN_UR(3)

  Real(REAL64), ALLOCATABLE :: points(:)

  Type(SISLSurf), TARGET  :: surf
  Type(SISLSurf), Pointer :: cached_surf

  Character(LEN=:), ALLOCATABLE :: IN_FILE_SURFACE
  Character(LEN=:), ALLOCATABLE :: OUT_FILE_POINTS
  Character(LEN=:), ALLOCATABLE :: arg1
  Character(LEN=1)              :: ans

  Logical :: USE_LOCAL_METHOD

  IN_FILE_SURFACE = "example10_surf.g2"
  OUT_FILE_POINTS = "example15_points.g2"

  ans = " "
  Print *,''
  Write(OUTPUT_UNIT, '(" Example15: ")')  
  Print *,''
  Write(OUTPUT_UNIT, '(A)')                                                    &
   "  This program will do a simple ray-tracing of a bspline ",                &
   "  surface.  From one point P in space, we will extend lines ",             &
   "  ('rays') towards the surface, and calculate the point where ",           &
   "  the line intersects with the surface.  All points obtained ",            &
   "  in this way thus describe how the surface is 'visible' from ",           &
   "  P.  They will be stored to the file "//OUT_FILE_POINTS,                  &
   "  The surface used is the one generated by 'example10', namely ",          &
   " "// IN_FILE_SURFACE
    Print *,''
    Write(OUTPUT_UNIT,'(A)')                                                   &
    "  Note that in this program, two ray-tracing routines are ",              &
    "  provided; (1) one slow and robust one, which would always ",            &
    "  work, and one quick but more fragile one (2), which bases ",            &
    "  its point estimates on previously obtained results, and ",              &
    "  thus is likely to fail when some parts of a surface shadows ",          &
    "  other parts.  The default method is (1); however the user ",            &
    "  can invoke the quick method by running the program with the ",          &
    "  additional argument 'q' (quick).  An examination of the ",              &
    "  obtained point cloud will in this case reveal erroneous ",              &
    "  behaviour where the examined surface 'folds over' itself.",             &
    "  The user will notice, though, that the execution time is ",             &
    "  significantly shorter.  This method is therefore useful in",            &
    "  some cases where surfaces are not self-obstructing."

  P = REAL([0, 0, -30], REAL64)

  WIN_LL = REAL([-5, -5, -10], REAL64) 
  WIN_LR = REAL([ 5, -5, -10], REAL64) 
  WIN_UL = REAL([-5,  5, -10], REAL64) 
  WIN_UR = REAL([ 5,  5, -10], REAL64) 

  HRES = 200
  VRES = 200

  EPSGE = 1.0E-5_REAL64
  EPSCO = 1.0E-15_REAL64

  USE_LOCAL_METHOD = .FALSE.

  Print *,''
  Write(OUTPUT_UNIT, '(" To proceed press enter, or q to quit : ")',           &
        ADVANCE="NO")
  Read(*,'(A)') ans
  If (ans == "q") Then
    Print *,''
    Print *,' Adios !!! '
  End If

  varnum = COMMAND_ARGUMENT_COUNT()

  If (varnum >= 1) Then
    Call GET_COMMAND_ARGUMENT(1, LENGTH=argLen)
    If (ALLOCATED(arg1)) DEALLOCATE(arg1)
    ALLOCATE(Character(LEN=arglen) :: arg1)
    arg1 = REPEAT(" ",arglen)
    Call GET_COMMAND_ARGUMENT(1, arg1)
    If (SCAN(arg1,"qQ") > 0) Then
      USE_LOCAL_METHOD = .TRUE.
    Else
      Print *,''
      Print *," Invalid command argument '"//TRIM(ADJUSTL(arg1))//" ' specified." 
      STOP
    EndIf
  EndIf

  Open(newunit=is, FILE=IN_FILE_SURFACE, FORM="FORMATTED",                     &
       STATUS="UNKNOWN")
  Open(newunit=os, FILE=OUT_FILE_POINTS, FORM="FORMATTED",                     &
       STATUS="UNKNOWN")

! read in target surface surf

  Call readSISLsurface(is, surf)

! Call desired ray tracer and output points

  startTime = clock() 
  If (USE_LOCAL_METHOD .EQV. .FALSE.) Then
    Call raytracer_simple(surf, points) 
  Else
    Call raytracer_advanced(surf, points)
  EndIf 
  endTime = clock()
  Write(*,"(' Time used =  ', g0.7)") (endtime-starttime)
  Print *,''
  Call writeSISLpoints(SIZE(points)/3, points, os)

! Clean up

  Call freeSurf(surf)

! Valgrind showed these weren't being deallocated on exit by gfortran-9

  If (ALLOCATED(OUT_FILE_POINTS)) DEALLOCATE(OUT_FILE_POINTS)
  If (ALLOCATED(IN_FILE_SURFACE)) DEALLOCATE(IN_FILE_SURFACE)

  Close(is)
  Close(os)

  If (ALLOCATED(points)) DEALLOCATE(points)

  STOP

Contains
  
  Subroutine raytracer_simple(surf, points)

!! slow robust raytracer. Scan view line by line and find all
!! intersections

    Implicit NONE

    Type(SISLsurf),              Intent(IN)    :: surf
    Real(REAL64),   ALLOCATABLE, Intent(INOUT) :: points(:)

    Integer      :: vscan, hscan
    Real(REAL64) :: dir(3)
    Real(REAL64) :: param_val(2)
    Logical      :: found

    If (ALLOCATED(points)) DEALLOCATE(points)
    ALLOCATE(points(0))

    Do vscan = 0,VRES-1
      Do hscan = 0,HRES-1
        Call calculate_direction(vscan, hscan, dir)
        found = trace_ray_robust(surf, param_val, dir)
        If (found) Then
          points = [points(1:SIZE(points)), dir(1:3)]
        EndIf
      EndDo
    EndDo

  End Subroutine raytracer_simple

  Subroutine raytracer_advanced(surf, points)

! Quick raytracer

    Implicit NONE

    Type(SISLsurf),              Intent(IN)    :: surf
    Real(REAL64),   ALLOCATABLE, Intent(INOUT) :: points(:)

    Integer      :: vscan, hscan, start, istop, inc
    Real(REAL64) :: dir(3)
    Real(REAL64) :: param_val(2)
    Logical      :: found_last
    Logical      :: hscan_forwards

    found_last     = .FALSE.
    hscan_forwards = .TRUE.
    param_val(1) = surf%et1(surf%ik1) - 1.0_REAL64
    param_val(2) = surf%et2(surf%ik2) - 1.0_REAL64
    If (ALLOCATED(points)) DEALLOCATE(points)
    ALLOCATE(points(0))

    Do vscan = 0,VRES-1

      start  = MERGE(0, HRES,    (hscan_forwards .EQV. .TRUE.))
      istop  = MERGE(HRES-1, -1, (hscan_forwards .EQV. .TRUE.))
      inc    = MERGE(1, -1,      (hscan_forwards .EQV. .TRUE.))
      hscan_forwards = (.NOT.hscan_forwards)
      hscan = start

     HSCAN_LOOP: Do
        If (hscan == istop) EXIT HSCAN_LOOP
        Call calculate_direction(vscan, hscan, dir)
        If (found_last) Then
          found_last = trace_ray_rapid(surf, param_val, dir)
        Else
          found_last = trace_ray_robust(surf, param_val, dir)
        End If
        If (found_last) Then
          points = [points(:), dir(:)]
        EndIf
        hscan = hscan + inc
      EndDo HSCAN_LOOP

    EndDo

  End Subroutine raytracer_advanced

  Subroutine calculate_direction(vscan, hscan, dir)

! Compute scan direction

    Implicit NONE

    Integer,      Intent(IN)    :: vscan, hscan
    Real(REAL64), Intent(INOUT) :: dir(3)

    Real(REAL64) :: v_ratio, h_ratio, vr1, hr1

    v_ratio = REAL(vscan, REAL64)/REAL(VRES-1, REAL64) 
    h_ratio = REAL(hscan, REAL64)/REAL(HRES-1, REAL64)

    vr1 = (1.0_REAL64-v_ratio)
    hr1 = (1.0_REAL64-h_ratio)
    dir(1:3) = hr1     * (vr1*WIN_LL(:) + v_ratio*WIN_LR(:)) +                 &
          h_ratio * (vr1*WIN_UL(:) + v_ratio*WIN_UR(:)) - P(:) 
 
  End Subroutine calculate_direction

  Function trace_ray_rapid(surf, init_guess, dir) RESULT(result)

! perform ray trace with rapid method

    Implicit NONE

    Type(SISLsurf), TARGET, Intent(IN)    :: surf
    Real(REAL64),           Intent(INOUT) :: init_guess(2)
    Real(REAL64),           Intent(INOUT) :: dir(3)
    Logical                               :: result

    Integer      :: jstat, temp1, temp2
    Real(REAL64) :: isect_uv(2)
    Real(REAL64) :: point(9), n(3)
    Real(REAL64) :: scalar_product_1, scalar_product_2

    If (ASSOCIATED(cached_surf, surf) .EQV. .FALSE.) Then

      start_dom(1) = surf%et1(surf%ik1)
      start_dom(2) = surf%et2(surf%ik2)
      end_dom(1)   = surf%et1(surf%in1+1)
      end_dom(2)   = surf%et2(surf%in2+1)
      cached_surf => surf
    End If

    If (init_guess(1) < start_dom(1)) Then
      result = trace_ray_robust(surf, init_guess, dir)
      RETURN
    End If

! Perform Newton interaction to find intersection between NURBS surface and line

    isect_uv = 0.0_REAL64 
    jstat = 0
    Call s1518(surf,        & ! the surface to 'raytrace'
               P,           & ! observer's position
               dir,         & ! direction of ray 
               EPSGE,       & ! geometric resolution
               start_dom,   & ! lower limit of search rectangle (umin, vmin)
               end_dom,     & ! upper limit of search rectangle (umax, vmax)
               init_guess,  & ! initial guess - we can use the last found value
               isect_uv,    & ! the intersection point - if found!
               jstat)         ! status

     If (jstat < 0) Then
       Print *,''
       STOP " Error occured in SISL routine s1518."
     ElseIf (jstat /= 1) Then ! no point found - Use robust technique
       result = trace_ray_robust(surf, init_guess, dir)
       RETURN
     EndIf

! Now evaluate surface at parameter value

     temp1 = 0
     temp2 = 0
     Call s1421(surf, & ! the surface in question
                1,    & ! we need at least one derivative in order to calculate normal
                init_guess, & ! parameter value
                temp1,      & ! uninteresting for our purposes - parameter interval
                temp2,     & ! uninteresting for our purposes - parameter interval
                point,     & ! storage for the point and its derivatives
                n,         & ! storage for the normal
                jstat) 

     If (jstat < 0) Then
       Print *,''
       STOP " Error occured in SISL routine s1412."
     ElseIf (jstat > 0) Then
       Print *,''
       Print *," WARNING: warning occured inside call to SISL routine s1421"
       Print *,'' 
     EndIf

     scalar_product_1 = DOT_PRODUCT(n,dir(1:3))

! Evaluate surface at new parameter

     Call s1421(surf, & ! the surface in question
                1,    & ! we need at least one derivative in order to calculate normal
                isect_uv, & ! parameter value
                temp1,    & ! uninteresting for our purposes - parameter interval
                temp2,    & ! uninteresting for our purposes - parameter interval
                point,   & ! storage for the point and its derivatives
                n,       & ! storage for the normal
                jstat) 

     If (jstat < 0) Then
       Print *,''
       STOP " Error occured in SISL routine s1412."
     ElseIf (jstat > 0) Then
       Print *,''
       Print *," WARNING: warning occured inside call to SISL routine s1421"
       Print *,'' 
     EndIf

     scalar_product_2 = DOT_PRODUCT(n,dir(1:3))

! Fall back on robust ray tracer if scalar products are of opposite signs

     If (scalar_product_1*scalar_product_2 < 0.0_REAL64) Then
       result = trace_ray_robust(surf, init_guess, dir)
       RETURN
     EndIf

     dir(1:3)        = point(1:3)
     init_guess(1:2) = isect_uv(1:2)
     result          = .TRUE.

  End Function trace_ray_rapid

  Function trace_ray_robust(surf, param_val, dir) Result(result)

! perform ray trace with robust method

    Implicit NONE

    Type(SISLsurf), Intent(IN)    :: surf
    Real(REAL64),   Intent(INOUT) :: param_val(2)    
    Real(REAL64),   Intent(INOUT) :: dir(3)
    Logical                       :: result

    Integer :: i, is, ie, jstat, num_intpoints, num_intcurves, temp1, temp2
    Real(REAL64)                    :: dist, cur_dist 
    Real(REAL64)                    :: temp_pos(3), tmp(3)
    Real(REAL64),       ALLOCATABLE :: pointpar(:)
    Type(SISLintCurve), ALLOCATABLE :: intcurves(:)

! Find all intersections between tensor-product surface and line (ray)

    Call s1856(surf, & ! the surface to intersect with
               P,    & ! a point on the line (in our case: the viewpoint)
               dir,  & ! the directional vector of the line
               3,             & ! dimension of the Euclidean space
               EPSCO,         & ! machine (computational) tolerance
               EPSGE,         & ! geometric tolerance
               num_intpoints, & ! reports the number of intersection points found
               pointpar,      & ! pointer to array containing the surface parameters for the 
                                ! found intersection points
               num_intcurves, & ! returns the number of intersection curves found (usually none)
               intcurves,     & ! pointer to an array containing the intersection curves found
               jstat)

    If (jstat < 0) Then
       Print *,''
       STOP " Error occured in SISL routine s1856."
     ElseIf (jstat > 0) Then
       Print *,''
       Print *," WARNING: warning occured inside call to SISL routine s1856"
       Print *,''
     EndIf

     If (num_intpoints == 0) Then
       If (ALLOCATED(pointpar)) DEALLOCATE(pointpar)

       If (ALLOCATED(intcurves)) Then
         Do i=1,num_intcurves
           Call freeIntcurve(intcurves(i))
          End Do
         DEALLOCATE(intcurves)
       End If

       result = .FALSE.
       RETURN
     EndIf

! Loop through intersection points and evaluate surface at parameter values

     dist = HUGE(1.0_REAL64)
     Do i=1, num_intpoints
       jstat    = 0
       temp1    = 0
       temp2    = 0
       temp_pos = 0.0_REAL64
       is = 2*i-1
       ie = is+1
       Call s1424(surf,      & ! input surface
              0,             & ! evaluate position only (no derivatives in u-param)
              0,             & ! evaluate position only (no derivatives in v-param)
              pointpar(is:ie), & ! parameter values
              temp1,         & ! unused for our purposes (returns used u-interval)
              temp2,         & ! unused for our purposes (returns used v-interval)
              temp_pos,      & ! the calculated position
              jstat)           ! status variable

      If (jstat < 0) Then
        Print *,''
        STOP " Error occured in SISL routine s1424."
      ElseIf (jstat > 0) Then
        Print *,''
        Print *," WARNING: warning occured inside call to SISL routine s1424"
        Print *,''
      EndIf

      tmp      = (temp_pos(:) - P(:))
      cur_dist = DOT_PRODUCT(tmp,tmp)

      If (cur_dist < dist) Then
        dist           = cur_dist
        param_val(1:2) = pointpar(is:ie)
        dir(1:3)       = temp_pos(1:3)
      End If

    End Do

    If (ALLOCATED(pointpar)) DEALLOCATE(pointpar)

    If (ALLOCATED(intcurves)) Then
      Do i=1,num_intcurves
        Call freeIntcurve(intcurves(i))
      End Do
      DEALLOCATE(intcurves)
    End If

    result = .TRUE.

  End Function trace_ray_robust 

  Function clock() Result(time_now)

! Provides a wrapper function around Fortran intrinsic SYSTEM_CLOCK subroutine
! SYSTEM_CLOCK returns current number of clock_ticks and clock_rate in
! ticks/second. Time is clock_ticks/clock_rate

    Implicit NONE

    Real(REAL64) :: time_now

    Integer(INT64) :: counts
    Real(REAL64)   :: count_rate
    Integer(INT64) :: count_max

    Call SYSTEM_CLOCK(COUNT=counts, COUNT_RATE=count_rate,            &
                      COUNT_MAX=count_max)

    time_now = REAL(counts,REAL64)/count_rate

  End Function clock

End Program example15
