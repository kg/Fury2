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
AlphaLevel *aColor[2], *aSource;

    if (Size < 1) Size = 1;

//    Pixel *pTable = AllocateArray(Pixel, Size);
    Pixel *pTable = LookupAllocate<Pixel>(Size + 1);

    if (!pTable) return Null;

    if (Size == 1) {
        pTable[0] = Premultiply(StartColor);
        return pTable;
    }

    float w = 0, winc = ((float)255 / (float)(Size - 1));
    int weight = 0, ci = 0;
    for (int i = 0; i < Size; i++) {
        ci = i - Offset;

        w += winc;
        if ((ci) >= 0) {
            weight = ClipByte(w);
            // initialize the two lookup pointers
            aColor[0] = AlphaLevelLookup(weight ^ 0xFF);
            aColor[1] = AlphaLevelLookup(weight);
        
            // set the alpha for this column (inverted)
            pTable[ci][::Alpha] = AlphaFromLevel2(aColor[0], StartColor[::Alpha], aColor[1], EndColor[::Alpha]) ^ 0xFF;
        
            // initialize the other two lookup pointers using the alpha of this column
            aSource = AlphaLevelLookup( pTable[ci][::Alpha] ^ 0xFF );
    
            // set the color for this column (premultiplied for more speed)
            pTable[ci][::Blue] = AlphaFromLevel(aSource, AlphaFromLevel2(aColor[0], StartColor[::Blue], aColor[1], EndColor[::Blue]));
            pTable[ci][::Green] = AlphaFromLevel(aSource, AlphaFromLevel2(aColor[0], StartColor[::Green], aColor[1], EndColor[::Green]));
            pTable[ci][::Red] = AlphaFromLevel(aSource, AlphaFromLevel2(aColor[0], StartColor[::Red], aColor[1], EndColor[::Red]));
        }

    }

    if (Offset == 0) pTable[0] = Premultiply(StartColor);
    pTable[Size - (1 + Offset)] = Premultiply(EndColor);

    return pTable;
    
}

Pixel* GenerateGradientTable(Pixel StartColor, Pixel EndColor, int Size) {
    return GenerateGradientTable(StartColor, EndColor, Size, 0);
}

Pixel* GenerateGradientTable(Pixel* Table, Pixel StartColor, Pixel EndColor, int Size, int Offset) {
AlphaLevel *aColor[2], *aSource;

    if (Size < 1) Size = 1;

    Pixel *pTable = Table;

    if (!pTable) return Null;

    if (Size == 1) {
        pTable[0] = Premultiply(StartColor);
        return pTable;
    }

    float w = 0, winc = ((float)255 / (float)(Size - 1));
    int weight = 0, ci = 0;
    for (int i = 0; i < Size; i++) {
        ci = i - Offset;

        w += winc;
        if ((ci) >= 0) {
            weight = ClipByte(w);
            // initialize the two lookup pointers
            aColor[0] = AlphaLevelLookup(weight ^ 0xFF);
            aColor[1] = AlphaLevelLookup(weight);
        
            // set the alpha for this column (inverted)
            pTable[ci][::Alpha] = AlphaFromLevel2(aColor[0], StartColor[::Alpha], aColor[1], EndColor[::Alpha]) ^ 0xFF;
        
            // initialize the other two lookup pointers using the alpha of this column
            aSource = AlphaLevelLookup( pTable[ci][::Alpha] ^ 0xFF );
    
            // set the color for this column (premultiplied for more speed)
            pTable[ci][::Blue] = AlphaFromLevel(aSource, AlphaFromLevel2(aColor[0], StartColor[::Blue], aColor[1], EndColor[::Blue]));
            pTable[ci][::Green] = AlphaFromLevel(aSource, AlphaFromLevel2(aColor[0], StartColor[::Green], aColor[1], EndColor[::Green]));
            pTable[ci][::Red] = AlphaFromLevel(aSource, AlphaFromLevel2(aColor[0], StartColor[::Red], aColor[1], EndColor[::Red]));
        }

    }

    if (Offset == 0) pTable[0] = Premultiply(StartColor);
    pTable[Size - (1 + Offset)] = Premultiply(EndColor);

    return pTable;
    
}

Pixel* GenerateGradientTable(Pixel* Table, Pixel StartColor, Pixel EndColor, int Size) {
    return GenerateGradientTable(Table, StartColor, EndColor, Size, 0);
}

Pixel* GenerateGradientTableFast(Pixel* Table, Pixel StartColor, Pixel EndColor, int Size, int Offset) {
AlphaLevel *aColor[2];

    if (Size < 1) Size = 1;

    Pixel *pTable = Table;

    if (!pTable) return Null;

    if (Size == 1) {
        pTable[0] = StartColor;
        return pTable;
    }

    float w = 0, winc = ((float)255 / (float)(Size - 1));
    int weight = 0, ci = 0;
    for (int i = 0; i < Size; i++) {
        ci = i - Offset;

        w += winc;
        if ((ci) >= 0) {
            weight = ClipByte(w);
            // initialize the two lookup pointers
            aColor[0] = AlphaLevelLookup(weight ^ 0xFF);
            aColor[1] = AlphaLevelLookup(weight);       
        
            // set the color for this column
            pTable[ci][::Blue] = AlphaFromLevel2(aColor[0], StartColor[::Blue], aColor[1], EndColor[::Blue]);
            pTable[ci][::Green] = AlphaFromLevel2(aColor[0], StartColor[::Green], aColor[1], EndColor[::Green]);
            pTable[ci][::Red] = AlphaFromLevel2(aColor[0], StartColor[::Red], aColor[1], EndColor[::Red]);
            pTable[ci][::Alpha] = AlphaFromLevel2(aColor[0], StartColor[::Alpha], aColor[1], EndColor[::Alpha]);
        }

    }

    if (Offset == 0) pTable[0] = StartColor;
    pTable[Size - (1 + Offset)] = EndColor;

    return pTable;
    
}

Pixel* GenerateGradientTableFast(Pixel* Table, Pixel StartColor, Pixel EndColor, int Size) {
    return GenerateGradientTableFast(Table, StartColor, EndColor, Size, 0);
}

Pixel* GenerateGradientTableFast(Pixel StartColor, Pixel EndColor, int Size, int Offset) {
AlphaLevel *aColor[2];

    if (Size < 1) Size = 1;

//    Pixel *pTable = AllocateArray(Pixel, Size);
    Pixel *pTable = LookupAllocate<Pixel>(Size);

    if (!pTable) return Null;

    if (Size == 1) {
        pTable[0] = StartColor;
        return pTable;
    }

    float w = 0, winc = ((float)255 / (float)(Size - 1));
    int weight = 0, ci = 0;
    for (int i = 0; i < Size; i++) {
        ci = i - Offset;

        w += winc;
        if ((ci) >= 0) {
            weight = ClipByte(w);
            // initialize the two lookup pointers
            aColor[0] = AlphaLevelLookup(weight ^ 0xFF);
            aColor[1] = AlphaLevelLookup(weight);       
        
            // set the color for this column
            pTable[ci][::Blue] = AlphaFromLevel2(aColor[0], StartColor[::Blue], aColor[1], EndColor[::Blue]);
            pTable[ci][::Green] = AlphaFromLevel2(aColor[0], StartColor[::Green], aColor[1], EndColor[::Green]);
            pTable[ci][::Red] = AlphaFromLevel2(aColor[0], StartColor[::Red], aColor[1], EndColor[::Red]);
            pTable[ci][::Alpha] = AlphaFromLevel2(aColor[0], StartColor[::Alpha], aColor[1], EndColor[::Alpha]);
        }

    }

    if (Offset == 0) pTable[0] = StartColor;
    pTable[Size - (1 + Offset)] = EndColor;

    return pTable;
    
}

Pixel* GenerateGradientTableFast(Pixel StartColor, Pixel EndColor, int Size) {
    return GenerateGradientTableFast(StartColor, EndColor, Size, 0);
}
