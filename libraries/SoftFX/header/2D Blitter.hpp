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

#define BLITTERSIMPLE_SIGNATURE(name)                           \
                                                                \
    Export int BlitSimple_##name (Image *Dest, Image *Source,   \
        Rectangle *Rect, Coordinate SX, Coordinate SY

#define BLITTERSIMPLE_INIT                                      \
                                                                \
    if (!Dest || !Source) {                                     \
        return Failure;                                         \
    }                                                           \
    if (!Dest->initialized()) {                                 \
        return Failure;                                         \
    }                                                           \
    if (!Source->initialized()) {                               \
        return Failure;                                         \
    }                                                           

#define _BOS(name, xpc)                                         \
    {                                                           \
    int overrideresult =                                        \
    Override::EnumOverrides(                                              \
    Override::##name,                                                      \
    5 + xpc, Dest, Source, Rect, SX, SY

#ifdef OVERRIDES
#define _BOE                                                    \
    );                                                          \
    if (overrideresult != 0) return overrideresult;             \
    }
#else
#define _BOE                                                    \
    );                                                          \
    }
#endif

#define BLITTERSIMPLE_BEGIN                                     \
                                                                \
    ImageLockManager ilDest(lockingMode, Dest);                 \
    ImageLockManager ilSource(lockingMode, Source);             \
    if (!ilDest.performUnlock())                              \
        return Failure;                                         \
    if (!ilSource.performUnlock())                            \
        return Failure;                                         \
    int DSX = SX, DSY = SY;                                     \
                                                                \
    Rectangle rCoordinates;                                     \
        if (!Clip2D_SimpleRect(&rCoordinates, Dest, Source,     \
         Rect, DSX, DSY)) return Trivial_Success;               \
                                                                \
    Pixel *pSource = Source->pointer(DSX, DSY),                 \
        *pDest = Dest->pointer(                                 \
        rCoordinates.Left, rCoordinates.Top);                   \
                                                                \
    /* clipping sucks */                                        \
    if ((!pSource) || (!pDest)) return Failure;                 \
                                                                \
    Dest->dirty();                                              \
                                                                \
    DoubleWord iCX = 0 , iCY = rCoordinates.Height;             \
                                                                \
    DoubleWord iSourceRowOffset =                               \
        (Source->Width - rCoordinates.Width) + Source->Pitch;   \
    DoubleWord iDestRowOffset =                                 \
        (Dest->Width - rCoordinates.Width) + Dest->Pitch;

#define BLITTERSIMPLE_ROW                                       \
                                                                \
    while (iCY--) {                                             \
                                                                \
        iCX = rCoordinates.Width;                               \

#define BLITTERSIMPLE_COL                                       \
                                                                \
        while (iCX--) {                                         
            

#define BLITTERSIMPLE_COLEND                                    \
                                                                \
            ++pSource;                                          \
            ++pDest;                                            \
                                                                \
        }                                                       \

#define BLITTERSIMPLE_ROWEND                                    \
                                                                \
        pSource += iSourceRowOffset;                            \
        pDest += iDestRowOffset;                                \
                                                                \
    }

#define BLITTERSIMPLE_LOOPBEGIN                                 \
                                                                \
    while (iCY--) {                                             \
                                                                \
        iCX = rCoordinates.Width;                               \
                                                                \
        while (iCX--) {
            
#define BLITTERSIMPLE_LOOPEND                                   \
                                                                \
            ++pSource;                                          \
            ++pDest;                                            \
                                                                \
        }                                                       \
                                                                \
        pSource += iSourceRowOffset;                            \
        pDest += iDestRowOffset;                                \
                                                                \
    }

#define BLITTERSIMPLE_END                                       \
                                                                \
    return Success;                                             \
                                                                \
}

#define BLITTERRESAMPLE_SIGNATURE(name)                         \
                                                                \
    Export int BlitResample_##name (Image *Dest, Image *Source, \
        Rectangle *DestRect, Rectangle *SourceRect,             \
        ScalerFunction *Scaler                                  \


#define _RBOS(name, xpc)                                         \
    {                                                           \
    int overrideresult =                                        \
    Override::EnumOverrides(                                              \
    Override::##name,                                                      \
    5 + xpc, Dest, Source, DestRect, SourceRect, Scaler

#ifdef OVERRIDES
#define _RBOE                                                    \
    );                                                          \
    if (overrideresult != 0) return overrideresult;             \
    }
#else
#define _RBOE                                                    \
    );                                                          \
    }
#endif

#define BLITTERRESAMPLE_INIT                                                        \
                                                                \
    if (!Dest || !Source) {                                     \
        return Failure;                                         \
    }                                                           \
    if (!Dest->initialized()) {                                 \
        return Failure;                                         \
    }                                                           \
    if (!Source->initialized()) {                               \
        return Failure;                                         \
    }                                                           

#define BLITTERRESAMPLE_BEGIN                                   \
                                                                \
    ImageLockManager ilDest(lockingMode, Dest);                 \
    ImageLockManager ilSource(lockingMode, Source);             \
    if (!ilDest.performUnlock()) {                              \
        return Failure;                                         \
    }                                                           \
    if (!ilSource.performUnlock()) {                            \
        return Failure;                                         \
    }                                                           \
    int CropOffsets[4] = {0,0,0,0};                             \
    bool vflip = false;                                         \
    if (SourceRect->Height < 0) vflip = true;                   \
    SourceRect->normalize();                                    \
    Rectangle rCoordinates, rSourceCoordinates;                 \
        if (!Clip2D_PairedRect(&rCoordinates,                   \
         &rSourceCoordinates, Dest, Source, DestRect,           \
         SourceRect, CropOffsets)) return Trivial_Success;      \
                                                                \
    Pixel *pDest = Dest->pointer(                               \
        rCoordinates.Left, rCoordinates.Top);                   \
                                                                \
    /* clipping sucks */                                        \
    if ((!pDest)) return Failure;                               \
                                                                \
    Dest->dirty();                                              \
                                                                \
    int iRDW = DestRect->Width;                                 \
    int iRDH = DestRect->Height;                                \
    int iRSW = rSourceCoordinates.Width;                        \
    int iRSH = rSourceCoordinates.Height;                       \
    float fIX = abs((float)iRSW / (float)iRDW);                 \
    float fIY = abs((float)iRSH / (float)iRDH);                 \
    float fSX = (rSourceCoordinates.Left +                      \
      CropOffsets[0] * fIX),                                    \
      fSY = (rSourceCoordinates.Top +                           \
      CropOffsets[1] * fIY);                                    \
    if (vflip) fSY = (rSourceCoordinates.bottom() - CropOffsets[1] * fIY); \
    if (vflip) fIY = -fIY;                                      \
    Pixel S;                                                    \
    Pixel *pS = &S;                                             \
    *pS = *pS;                                                  \
    DoubleWord iDX = 0, iDY = rCoordinates.Height;              \
    DoubleWord iMX = rCoordinates.Width;                        \
    Pixel *rowTable = StaticAllocate<Pixel>(BlitterBuffer, iMX);\
    iDY = abs(rCoordinates.Height);                             \
    bool noChange = (rSourceCoordinates.Height == 1);           \
                                                                \
    DoubleWord iDestRowOffset =                                 \
        (Dest->Width - rCoordinates.Width) + Dest->Pitch;       \
                                                                \
    if (noChange)                                               \
      Scaler(Source,                                            \
        fSX,                                                    \
        fSY,                                                    \
        (int)(fSX * 65536.0) % 65536,                           \
        (int)(fSY * 65536.0) % 65536,                           \
        (fIX), 0,                                               \
        (int)(fIX * 65536.0) % 65536, 0,                        \
        iMX,                                                    \
        rowTable );

#define BLITTERRESAMPLE_LOOPBEGIN                               \
                                                                \
    while (iDY--) {                                             \
                                                                \
        iDX = 0;                                                \
                                                                \
        if (!noChange)                                          \
          Scaler(Source,                                        \
            fSX,                                                \
            fSY,                                                \
            (int)(fSX * 65536.0) % 65536,                       \
            (int)(fSY * 65536.0) % 65536,                       \
            (fIX), 0,                                           \
            (int)(fIX * 65536.0) % 65536, 0,                    \
            iMX,                                                \
            rowTable );                                         \
                                                                \
        while (iDX < iMX) {                                     \
                                                                \
            S = rowTable[iDX];                                  \
            
#define BLITTERRESAMPLE_LOOPEND                                 \
                                                                \
            pDest++;                                            \
            iDX++;                                              \
                                                                \
        }                                                       \
                                                                \
        fSY += fIY;                                             \
        pDest += iDestRowOffset;                                \
                                                                \
    }

#define BLITTERRESAMPLE_END                                     \
                                                                \
    return Success;                                             \
                                                                \
}