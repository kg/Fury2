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

#define TILEBLITTERSIMPLE_SIGNATURE(name)                       \
                                                                \
    Export int BlitTile_##name (Image *Dest, Image *Source,     \
     Rectangle *Rect

#define TILEBLITTERSIMPLE_BEGIN                                 \
                                                                \
    if (!Dest || !Source) {                                     \
        return Failure;                                         \
    }                                                           \
                                                                \
    Rectangle rCoordinates, rCopy;                              \
    rCoordinates = Dest->ClipRectangle;                         \
    rCopy = *Rect;                                              \
    Dest->setClipRectangle(rCopy);                               \
                                                                \
    if (rCopy.empty()) {                                       \
        return Trivial_Success;                                 \
    }                                                           \
                                                                \
    unsigned int iCX = 0, iMX = (rCopy.Width / Source->Width),  \
        iCY = (rCopy.Height / Source->Height);                  \
                                                                \
    if (rCopy.Width % Source->Width) iMX++;                     \
    if (rCopy.Height % Source->Height) iCY++;                   \
                                                                \
    unsigned int iDX = Rect->Left,                              \
        iDY = Rect->Top;                                        \
                                                                \
    Rectangle rDest;                                            \
        rDest = rCopy;                                           \

#define TILEBLITTERSIMPLE_LOOPBEGIN                             \
                                                                \
    while (iCY--) {                                             \
                                                                \
        iCX = iMX;                                              \
        iDX = rCopy.Left;                                       \
                                                                \
        while (iCX--) {                                         \
                                                                \
            rDest.Left = iDX;                                   \
            rDest.Top = iDY;                                    \
            rDest.Width = Source->Width;                        \
            rDest.Height = Source->Height;                      \
            
#define TILEBLITTERSIMPLE_LOOPEND                               \
                                                                \
            iDX += Source->Width;                               \
                                                                \
        }                                                       \
                                                                \
        iDY += Source->Height;                                  \
                                                                \
    }

#define TILEBLITTERSIMPLE_END                                   \
                                                                \
    Dest->ClipRectangle = rCoordinates;                         \
                                                                \
    return Success;                                             \
                                                                \
}