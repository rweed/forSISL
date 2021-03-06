
/*   
*  Copyright (C) 2021 Richard Weed.
*  All rights reserved.
  
*  Redistribution and use in source and binary forms, with or without 
*  modification, are permitted provided that the following conditions are met:
  
*  1. Redistributions of source code, in whole or in part, must retain the  
*  above copyright notice, this list of conditions and the following 
*  disclaimer.
  
*  2. Redistributions in binary form, in whole or in part, must reproduce the 
*  above copyright notice, this list of conditions and the following disclaimer 
*  in the documentation and/or other materials provided with the distribution.
  
*  3. The names of the contributors may not be used to endorse or promote from 
*  products derived from this software without specific prior written 
*  permission.

*  4. Redistributions of this software, in whole or in part, in any form, 
*  must be freely available and licensed under this original License. The 
*  U.S. Government may add additional restrictions to their modified and 
*  redistributed software as required by Law. However, these restrictions 
*  do not apply to the original software distribution.
 
*  5. Redistribution of this source code, including any modifications, may 
*  not be intentionally obfuscated.
  
*  6. Other code may make use of this software, in whole or in part, without 
*  restriction, provided that it does not apply any restriction to this 
*  software other than outlined above.

*  7. This software requires a version of the SINTEF SISL library
*     (https://github.com/SINTEF-Geometry/SISL). It is assumed that users
*     will download and install this software separate from this code.
*     Users are required to honor the SISL license clauses that cover usage
*     for both commercial and non-commercial applications. See the copy of 
*     the SISL license information and copyright that occompanies this software.

 
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
*  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
*  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
*  PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND
*  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, 
*  EXEMPLARARY OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
*  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; 
*  OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
*  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
*  OTHERWISE), ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
*  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


/* Some C utilities used by Fortran interfaces */ 

#include <sys/types.h>

/* Get standard I/O library definitions */
#include <stdio.h>

/* Get standard library definitions (includes malloc defs) */
#include <stdlib.h>

/* Get  string library definitions (includes memcpy defs) */
#include <string.h>

/* Get system specific limits */
#include <limits.h>
/* Get floating point limits */
#include <float.h>

/* Get math library routines definition. */
#include <math.h>

extern void c_free(void *ptr)
{
  free(ptr);
}

