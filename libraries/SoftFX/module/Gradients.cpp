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

#include "../header/SoftFX Main.hpp"
#include "../header/Blend.hpp"

Pixel* GenerateGradientTable(Pixel StartColor, Pixel EndColor, int Size, int Offset) {

    if (Size < 1) Size = 1;

//    Pixel *pTable = AllocateArray(Pixel, Size);
    Pixel *pTable = LookupAllocate<Pixel>(Size + 1);

    if (!pTable) return Null;

    if (Size == 1) {
        pTable[0] = Premultiply(StartColor);
        return pTable;
    }

    if (Offset < 0) Offset = 0;

    float w = 0, winc = ((float)255 / (float)(Size - 1));
    int weight = 0, ci = 0;
    for (int i = 0; i < Size; i++) {
        ci = i - Offset;

        w += winc;
        if (((ci) >= 0) && (ci < Size)) {
            weight = ClipByte(w);

            // set the alpha for this column (inverted)
            pTable[ci][::Alpha] = (((StartColor[::Alpha] * (weight ^ 0xFF)) + (EndColor[::Alpha] * weight)) / 255) ^ 0xFF;
            
            // set the color for this column (premultiplied for more speed)
            pTable[ci][::Blue] = (((StartColor[::Blue] * (weight ^ 0xFF)) + (EndColor[::Blue] * weight)) / 255) * (pTable[ci][::Alpha] ^ 0xFF) / 255;
            pTable[ci][::Green] = (((StartColor[::Green] * (weight ^ 0xFF)) + (EndColor[::Green] * weight)) / 255) * (pTable[ci][::Alpha] ^ 0xFF) / 255;
            pTable[ci][::Red] = (((StartColor[::Red] * (weight ^ 0xFF)) + (EndColor[::Red] * weight)) / 255) * (pTable[ci][::Alpha] ^ 0xFF) / 255;
        }

    }

    pTable[0] = Premultiply(StartColor);
    pTable[0][::Alpha] = pTable[0][::Alpha] ^ 0xFF;
    pTable[Size - 1] = Premultiply(EndColor);
    pTable[Size - 1][::Alpha] = pTable[Size - 1][::Alpha] ^ 0xFF;

    return pTable;
    
}

Pixel* GenerateGradientTable(Pixel StartColor, Pixel EndColor, int Size) {
    return GenerateGradientTable(StartColor, EndColor, Size, 0);
}

Pixel* GenerateGradientTable(Pixel* Table, Pixel StartColor, Pixel EndColor, int Size, int Offset) {

    if (Size < 1) Size = 1;

    Pixel *pTable = Table;

    if (!pTable) return Null;

    if (Size == 1) {
        pTable[0] = Premultiply(StartColor);
        return pTable;
    }
  
    if (Offset < 0) Offset = 0;

    float w = 0, winc = ((float)255 / (float)(Size - 1));
    int weight = 0, ci = 0;
    for (int i = 0; i < Size; i++) {
        ci = i - Offset;

        w += winc;
        if (((ci) >= 0) && (ci < Size)) {
            weight = ClipByte(w);

            // set the alpha for this column (inverted)
            pTable[ci][::Alpha] = (((StartColor[::Alpha] * (weight ^ 0xFF)) + (EndColor[::Alpha] * weight)) / 255) ^ 0xFF;
            
            // set the color for this column (premultiplied for more speed)
            pTable[ci][::Blue] = (((StartColor[::Blue] * (weight ^ 0xFF)) + (EndColor[::Blue] * weight)) / 255) * (pTable[ci][::Alpha] ^ 0xFF) / 255;
            pTable[ci][::Green] = (((StartColor[::Green] * (weight ^ 0xFF)) + (EndColor[::Green] * weight)) / 255) * (pTable[ci][::Alpha] ^ 0xFF) / 255;
            pTable[ci][::Red] = (((StartColor[::Red] * (weight ^ 0xFF)) + (EndColor[::Red] * weight)) / 255) * (pTable[ci][::Alpha] ^ 0xFF) / 255;
        }

    }

    if (Offset == 0) pTable[0] = Premultiply(StartColor);
    pTable[Size - 1] = Premultiply(EndColor);

    return pTable;
    
}

Pixel* GenerateGradientTable(Pixel* Table, Pixel StartColor, Pixel EndColor, int Size) {
    return GenerateGradientTable(Table, StartColor, EndColor, Size, 0);
}

Pixel* GenerateGradientTableFast(Pixel* Table, Pixel StartColor, Pixel EndColor, int Size, int Offset) {

    if (Size < 1) Size = 1;

    Pixel *pTable = Table;

    if (!pTable) return Null;

    if (Size == 1) {
        pTable[0] = StartColor;
        return pTable;
    }

    if (Offset < 0) Offset = 0;

    float w = 0, winc = ((float)255 / (float)(Size - 1));
    int weight = 0, ci = 0;
    for (int i = 0; i < Size; i++) {
        ci = i - Offset;

        w += winc;
        if (((ci) >= 0) && (ci < Size)) {
            weight = ClipByte(w);
            
            // set the color for this column
            pTable[ci][::Alpha] = (((StartColor[::Alpha] * (weight ^ 0xFF)) + (EndColor[::Alpha] * weight)) / 255);
            pTable[ci][::Blue] = (((StartColor[::Blue] * (weight ^ 0xFF)) + (EndColor[::Blue] * weight)) / 255);
            pTable[ci][::Green] = (((StartColor[::Green] * (weight ^ 0xFF)) + (EndColor[::Green] * weight)) / 255);
            pTable[ci][::Red] = (((StartColor[::Red] * (weight ^ 0xFF)) + (EndColor[::Red] * weight)) / 255);
        }

    }

    if (Offset == 0) pTable[0] = StartColor;
    pTable[Size - 1] = EndColor;

    return pTable;
    
}

Pixel* GenerateGradientTableFast(Pixel* Table, Pixel StartColor, Pixel EndColor, int Size) {
    return GenerateGradientTableFast(Table, StartColor, EndColor, Size, 0);
}

Pixel* GenerateGradientTableFast(Pixel StartColor, Pixel EndColor, int Size, int Offset) {

    if (Size < 1) Size = 1;

//    Pixel *pTable = AllocateArray(Pixel, Size);
    Pixel *pTable = LookupAllocate<Pixel>(Size);

    if (!pTable) return Null;

    if (Size == 1) {
        pTable[0] = StartColor;
        return pTable;
    }

    if (Offset < 0) Offset = 0;

    float w = 0, winc = ((float)255 / (float)(Size - 1));
    int weight = 0, ci = 0;
    for (int i = 0; i < Size; i++) {
        ci = i - Offset;

        w += winc;
        if (((ci) >= 0) && (ci < Size)) {
            weight = ClipByte(w);

            // set the color for this column
            pTable[ci][::Alpha] = (((StartColor[::Alpha] * (weight ^ 0xFF)) + (EndColor[::Alpha] * weight)) / 255);
            pTable[ci][::Blue] = (((StartColor[::Blue] * (weight ^ 0xFF)) + (EndColor[::Blue] * weight)) / 255);
            pTable[ci][::Green] = (((StartColor[::Green] * (weight ^ 0xFF)) + (EndColor[::Green] * weight)) / 255);
            pTable[ci][::Red] = (((StartColor[::Red] * (weight ^ 0xFF)) + (EndColor[::Red] * weight)) / 255);
        }

    }

    if (Offset == 0) pTable[0] = StartColor;
    pTable[Size - 1] = EndColor;

    return pTable;
    
}

Pixel* GenerateGradientTableFast(Pixel StartColor, Pixel EndColor, int Size) {
    return GenerateGradientTableFast(StartColor, EndColor, Size, 0);
}
