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

Pixel* GenerateGradientTable(Pixel StartColor, Pixel EndColor, int Size);
Pixel* GenerateGradientTable(Pixel StartColor, Pixel EndColor, int Size, int Offset);
Pixel* GenerateGradientTable(Pixel* Table, Pixel StartColor, Pixel EndColor, int Size);
Pixel* GenerateGradientTable(Pixel* Table, Pixel StartColor, Pixel EndColor, int Size, int Offset);

Pixel* GenerateGradientTableFast(Pixel StartColor, Pixel EndColor, int Size);
Pixel* GenerateGradientTableFast(Pixel StartColor, Pixel EndColor, int Size, int Offset);
Pixel* GenerateGradientTableFast(Pixel* Table, Pixel StartColor, Pixel EndColor, int Size);
Pixel* GenerateGradientTableFast(Pixel* Table, Pixel StartColor, Pixel EndColor, int Size, int Offset);
