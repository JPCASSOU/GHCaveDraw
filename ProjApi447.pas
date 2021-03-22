//------------------------------------------------------------------------------
// Proj API for PROJ 4.4.7
//------------------------------------------------------------------------------
{
@abstract(Application Interface to Frank Warmerdams PROJ 4.4.7)
@author  (Alexander Weidauer <alex.weidauer@huckfinn.de)
@created (December 2001)
@lastmod (March 2003)
The Unit  delivers an application interface to Frank Warmerdams
geographic projection tool proj for the use under delphi.
One change was made by handling the initialization code for
_pj_init_plus_path. These routine work static with
the environment variable PROJ_LIB=$DIR with a complex handling
to home directories or enviromental setted directories.
I modified the code to set the directory for the
transformation datas directly.

@@1. Function  _pj_init is not implemented yet because
it's senseless  translate projPJ pj_init(int, char **,const char * ) now
more for command versions;

@@2. There is a great brumborium around system variables and so on
to find the coordinate table in the HOME directory or the globale
directory in the UNIX code. I dont break this codebut I' give the
possebility to set the path to the coordinate and geodatic files
explicit so I' do not use this mechanism. If you set the path variable to
NIL or in C to NULL it works like the origin code.
}
Unit ProjApi447;

{******************************************************************************
 * $Id: proj_api.h,v 1.8 2003/03/31 14:52:38 warmerda Exp $
 *
 * Project:  PROJ.4
 * Purpose:  Public (application) include file for PROJ.4 API, and constants.
 * Author:   Frank Warmerdam, <warmerdam@pobox.com>
 *
 ******************************************************************************
 * Copyright (c) 2001, Frank Warmerdam <warmerdam@pobox.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 ******************************************************************************
 *
 * $Log: proj_api.h,v $
 * Revision 1.8  2003/03/31 14:52:38  warmerda
 * updated to 4.4.7
 *
 * Revision 1.7  2002/12/14 20:14:35  warmerda
 * added geocentric support
 *
 * Revision 1.6  2002/06/11 18:08:25  warmerda
 * Added the pj_get_def() function
 *
 * Revision 1.5  2002/01/09 14:36:22  warmerda
 * updated to 4.4.5
 *
 * Revision 1.4  2001/09/15 22:55:28  warmerda
 * final prep for 4.4.4 release
 *
 * Revision 1.3  2001/08/23 20:25:55  warmerda
 * added pj_set_finder function
 *
 * Revision 1.2  2001/06/02 03:35:36  warmerda
 * added pj_get_errno_ref()
 *
 * Revision 1.1  2001/04/06 01:24:22  warmerda
 * New
 *
 ******************************************************************************}

Interface

Uses SysUtils;
//------------------------------------------------------------------------------
Const
 {DLL}
 LibName='proj447.dll';

 { Scaling value radiant to decimal degree }
 RAD_TO_DEG   =	57.29577951308232;
 { Scaling value decimal degree to radiant }
 DEG_TO_RAD   =	0.0174532925199432958;

//------------------------------------------------------------------------------
Type
 { Point record and analoga }
 TProjUV = Record U,V:Double End;
 { Point record and analoga }
 TProjXY = TProjUV;
 { Point record and analoga }
 TProjLP = TProjUV;
 { Pointer to a projection }
 PProjPJ = Pointer;
 { Pointer to a double array like double *mat; in C}
 PDoubleArray  = ^TDoubleArray;
 { Array like double *vec; in C}
 TDoubleArray  = Array [0..0] Of Double;
 { Pointer to a character array like char *chr; in C}
 PCharArray    = Array Of Pointer;
 { Array like char *chr; in C}
 TCharArray    = Array [0..0] Of PChar;
 { 32 bit word }
 TLong         = LongWord;
//------------------------------------------------------------------------------
{ Initialize a projection set via plus item
  defined file defined projection.
  exsample:
   PJ := _pj_init_plus_path('+init=world:gk4-d','..\nad\');
  loades the projection from the file ..\nad\world
  item:
  ...
  <gk4-d> # Gauss Krueger Grid for Germany
        proj=tmerc ellps=bessel lon_0=12d0E lat_0=0
        x_0=500000
        no_defs<>

There is a great brumborium around system variables and so on
to find the coordinate table in the HOME directory or the globale
directory in the UNIX code. I dont break this codebut I' give the
possebility to set the path to the coordinate and geodatic files
explicit so I' do not use this mechanism. If you set the path variable to
NIL or in C to NULL it works like the origin code.
}
Function  _pj_init_plus_path(const Args: PChar; const Path: PChar):
          PProjPJ ;cdecl;external LibName;

{ forward projection normally Longitude Latitude to plain xy Coordinates }
Function  _pj_fwd(ProjLP:TProjLP; projPJ:PProjPJ):TProjXY;
          cdecl;external LibName;

{ inverse projection normally plain xy coordinates to longitude latitude coordinates }
Function  _pj_inv(ProjXY:TProjXY; projPJ:PProjPJ):TProjLP;
          cdecl;external LibName;

{**** never tested this ****.
 The  pj_transform function may be used to transform points
 between the two provided coordinate systems.  In  addition
 to  converting between cartographic projection coordinates
 and geographic coordinates, this function also takes  care
 of  datum shifts if possible between the source and desti-
 nation coordinate system.  Unlike pj_fwd and pj_inv it  is
 also  allowable  for the coordinate system definitions (PJ
 *)  to  be  geographic  coordinate  systems  (defined   as
 +proj=latlong).   The  x, y and z arrays contain the input
 values of the points, and are  replaced  with  the  output
 values.   The function returns zero on success, or the er-
 ror number (also in pj_errno) on failure.
}
Function  _pj_transform(src,dst:PProjPJ;point_count:TLong;point_offset:Integer;
          x,y,z:PDoubleArray):Integer;cdecl;external LibName;

{..compare two datums }
Function  _pj_compare_datums(srcdefn,dstdefn:PProjPJ ):Integer
          ;cdecl;external LibName;
{ Free the allocated projections }
Function  _pj_free(projPJ:PProjPJ):Integer;cdecl;external LibName;

{ Get the error code }
Function  _pj_strerrno(errno:Integer):PChar;cdecl;external LibName;

{ Get the error code number }
Function  _pj_get_errno_ref:Integer;cdecl;external LibName;


//------------------------------------------------------------------------------
Implementation

End.
