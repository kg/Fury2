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

#define FILTERSIMPLE_SIGNATURE(name)                            \
                                                                \
    Export int FilterSimple_##name (Image *Image,               \
     Rectangle* Area 

#define FILTERSIMPLE_INIT                                       \
    if (!Image) {                                               \
        return Failure;                                         \
    }                                                           \
    if (!Image->initialized()) {                                \
        return Failure;                                         \
    }                                                           

#define FILTERSIMPLE_BEGIN                                      \
    ImageLockManager ilImage(lockingMode, Image);               \
    if (!ilImage.performUnlock()) {                             \
        return Failure;                                         \
    }                                                           \
                                                                \
    int CropOffsets[4] = {0,0,0,0};                             \
    Rectangle rCoordinates;                                     \
        if (!Area) {                                            \
            rCoordinates = Image->ClipRectangle;                \
        } else {                                                \
            rCoordinates = *Area;                               \
        }                                                       \
        Image->clipRectangle(&rCoordinates, CropOffsets);       \
                                                                \
    if (rCoordinates.empty()) {                                 \
        return Trivial_Success;                                 \
    }                                                           \
                                                                \
    Image->dirty();                                \
                                                                \
    Pixel *pCurrent = Image->pointer(                           \
        rCoordinates.Left, rCoordinates.Top);                   \
    *pCurrent = *pCurrent;                                      \
                                                                \
    if ((!pCurrent)) return Failure;                            \
                                                                \
    int iCX = 0 , iCY = rCoordinates.Height;                    \
                                                                \
    int iRowOffset =                                            \
        (Image->Width - rCoordinates.Width) + Image->Pitch;     
               
#define _FOS(name, xpc)                                         \
    {                                                           \
    int overrideresult =                                        \
    Override::EnumOverrides(                                              \
    Override::##name,                                                      \
    2 + xpc, Image, Area

#ifdef OVERRIDES
#define _FOE                                                    \
    );                                                          \
    if (overrideresult != 0) return overrideresult;             \
    }
#else
#define _FOE                                                    \
    );                                                          \
    }
#endif

#define FILTERSIMPLE_INITPRIMITIVE                              \
                                                                \
    if (!Image) {                                               \
        return Failure;                                         \
    }                                                           \
    if (!Image->initialized()) {                                \
        return Failure;                                         \
    }                                                           

#define FILTERSIMPLE_BEGINPRIMITIVE                             \
    ImageLockManager ilImage(lockingMode, Image);               \
    if (!ilImage.performUnlock()) {                             \
        return Failure;                                         \
    }                                                           \
                                                                \
    Rectangle rCoordinates;                                     \
        if (!Area) {                                            \
            rCoordinates = Image->getRectangle();               \
        } else {                                                \
            rCoordinates = *Area;                               \
        }                                                       \
                                                                \

#define FILTERSIMPLE_ROW                                        \
                                                                \
    while (iCY--) {                                             \
                                                                \
        iCX = rCoordinates.Width;                               

#define FILTERSIMPLE_COL                                        \
                                                                \
        while (iCX--) {
            
#define FILTERSIMPLE_COLEND                                     \
                                                                \
            pCurrent++;                                         \
                                                                \
        }                                                                                                                      
          
#define FILTERSIMPLE_ROWEND                                     \
                                                                \
        pCurrent += iRowOffset;                                 \
                                                                \
    }

#define FILTERSIMPLE_LOOPBEGIN                                  \
                                                                \
    while (iCY--) {                                             \
                                                                \
        iCX = rCoordinates.Width;                               \
                                                                \
        while (iCX--) {
            
#define FILTERSIMPLE_LOOPEND                                    \
                                                                \
            pCurrent++;                                         \
                                                                \
        }                                                       \
                                                                \
        pCurrent += iRowOffset;                                 \
                                                                \
    }

#define FILTERSIMPLE_END                                        \
                                                                \
    return Success;                                             \
                                                                \
}