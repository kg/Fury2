/*
SoftFX (Software graphics manipulation library)
Copyright (C) 2003 Kevin Gadd

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

extern ScalerFunction *DefaultSampleFunction;

void  SampleRow_Bilinear(Image *Source, int X, int Y, int XW, int YW, int XI, int YI, int XWI, int YWI, int Count, Pixel *Dest);
void  SampleRow_Linear(Image *Source, int X, int Y, int XW, int YW, int XI, int YI, int XWI, int YWI, int Count, Pixel *Dest);
void  SampleRow_Bilinear_Rolloff(Image *Source, int X, int Y, int XW, int YW, int XI, int YI, int XWI, int YWI, int Count, Pixel *Dest);
void  SampleRow_Linear_Rolloff(Image *Source, int X, int Y, int XW, int YW, int XI, int YI, int XWI, int YWI, int Count, Pixel *Dest);

#define resample_included