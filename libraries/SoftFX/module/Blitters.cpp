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
// #include "../header/Blend_Template.hpp"
#include "../header/Blend.hpp"
#include "../header/Clip.hpp"
#include "../header/2D Blitter.hpp"
#include "../header/Tile Blitter.hpp"
#include "../header/Resample.hpp"
#include "../header/Convolve.hpp"
#include "../header/Filters.hpp"

BLITTERSIMPLE_SIGNATURE(Automatic_SourceAlpha_Opacity) , int Opacity);

BLITTERSIMPLE_SIGNATURE(Normal)
    ) {
BLITTERSIMPLE_INIT
    if (Source->OptimizeData.solidColor) {
      return FilterSimple_Fill(Dest, Rect, Source->getPixel(0, 0));
    }
    _BOS(BlitSimple_Normal, 0) _BOE
BLITTERSIMPLE_BEGIN
    if ((Rect->Left == 0) && (Rect->Top == 0) && (Rect->Width == Dest->Width) && (Rect->Height == Dest->Height) && (Rect->Width == Source->Width) && (Rect->Height == Source->Height)) {
        // muahahaha
        _Copy<Pixel>(Dest->fast_pointer(0, 0), Source->fast_pointer(0, 0), Dest->Width * Dest->Height);
        return Success;
    }
    iCX = iCX;
    while (iCY--) {
        _Copy<Pixel>(pDest, pSource, rCoordinates.Width);
        pSource += iSourceRowOffset + rCoordinates.Width;
        pDest += iDestRowOffset + rCoordinates.Width;
    }
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Channel)
    , int DestChannel, int SourceChannel) {
BLITTERSIMPLE_INIT
    _BOS(BlitSimple_Channel, 2) , DestChannel, SourceChannel _BOE
BLITTERSIMPLE_BEGIN
BLITTERSIMPLE_LOOPBEGIN
    (*pDest)[DestChannel] = (*pSource)[SourceChannel];
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Normal_Opacity)
    , int Opacity) {
BLITTERSIMPLE_INIT
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return BlitSimple_Normal(Dest, Source, Rect, SX, SY);
    if (Source->OptimizeData.solidColor) {
      return FilterSimple_Fill_Opacity(Dest, Rect, Source->getPixel(0, 0), Opacity);
    }
    _BOS(BlitSimple_Normal_Opacity, 1) , Opacity _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource, *aDest;
    aSource = AlphaLevelLookup( ClipByte(Opacity) );
    aDest = AlphaLevelLookup( ClipByte(Opacity) ^ 0xFF );
BLITTERSIMPLE_LOOPBEGIN
    BLENDPIXEL_ALPHA_OPACITY(pDest, pDest, pSource, aDest, aSource)
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Normal_Tint)
    , Pixel Tint) {
BLITTERSIMPLE_INIT
    if (Tint[::Alpha] == 0) return BlitSimple_Normal(Dest, Source, Rect, SX, SY);
    _BOS(BlitSimple_Normal_Tint, 1) , Tint _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource, *aDest;
    int tR, tG, tB;
    aSource = AlphaLevelLookup( Tint[::Alpha] );
    aDest = AlphaLevelLookup( Tint[::Alpha] ^ 0xFF );
    tB = AlphaFromLevel(aSource, Tint[::Blue]);
    tG = AlphaFromLevel(aSource, Tint[::Green]);
    tR = AlphaFromLevel(aSource, Tint[::Red]);
BLITTERSIMPLE_LOOPBEGIN
    (*pDest)[::Blue] = AlphaFromLevel(aDest, (*pSource)[::Blue]) + tB;
    (*pDest)[::Green] = AlphaFromLevel(aDest, (*pSource)[::Green]) + tG;
    (*pDest)[::Red] = AlphaFromLevel(aDest, (*pSource)[::Red]) + tR;
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Normal_Tint_Opacity)
    , Pixel Tint, int Opacity) {
BLITTERSIMPLE_INIT
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return BlitSimple_Normal_Tint(Dest, Source, Rect, SX, SY, Tint);
    if (Tint[::Alpha] == 0) return BlitSimple_Normal_Opacity(Dest, Source, Rect, SX, SY, Opacity);
    _BOS(BlitSimple_Normal_Tint_Opacity, 2) , Tint, Opacity _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource, *aDest, *aTint;
    int tR, tG, tB;
    aSource = AlphaLevelLookup(Tint[::Alpha]);
    aTint = AlphaLevelLookup(Tint[::Alpha] ^ 0xFF);
    tB = AlphaFromLevel(aSource, Tint[::Blue]);
    tG = AlphaFromLevel(aSource, Tint[::Green]);
    tR = AlphaFromLevel(aSource, Tint[::Red]);
    aSource = AlphaLevelLookup( ClipByte(Opacity) );
    aDest = AlphaLevelLookup( ClipByte(Opacity) ^ 0xFF );
BLITTERSIMPLE_LOOPBEGIN
    (*pDest)[::Blue] = AlphaFromLevel2(aDest, (*pDest)[::Blue], aSource, AlphaFromLevel(aTint, (*pSource)[::Blue]) + tB);
    (*pDest)[::Green] = AlphaFromLevel2(aDest, (*pDest)[::Green], aSource, AlphaFromLevel(aTint, (*pSource)[::Green]) + tG);
    (*pDest)[::Red] = AlphaFromLevel2(aDest, (*pDest)[::Red], aSource, AlphaFromLevel(aTint, (*pSource)[::Red]) + tR);
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Normal_Gamma)
    , int Gamma) {
BLITTERSIMPLE_INIT
    _BOS(BlitSimple_Normal_Gamma, 1) , Gamma _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aGamma;
    aGamma = AlphaLevelLookup( ClipValue(Gamma, 0, 511) );
BLITTERSIMPLE_LOOPBEGIN
    (*pDest)[::Blue] = AlphaFromLevel(aGamma, (*pSource)[::Blue]);
    (*pDest)[::Green] = AlphaFromLevel(aGamma, (*pSource)[::Green]);
    (*pDest)[::Red] = AlphaFromLevel(aGamma, (*pSource)[::Red]);
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Normal_Gamma_Opacity)
    , int Gamma, int Opacity) {
BLITTERSIMPLE_INIT
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return BlitSimple_Normal_Gamma(Dest, Source, Rect, SX, SY, Gamma);
    _BOS(BlitSimple_Normal_Gamma_Opacity, 2) , Gamma, Opacity _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource, *aDest;
    aSource = AlphaLevelLookup( ClipByte(Opacity) );
    aDest = AlphaLevelLookup( ClipByte(Opacity) ^ 0xFF );
BLITTERSIMPLE_LOOPBEGIN
    BLENDPIXEL_ALPHA_OPACITY(pDest, pDest, pSource, aDest, aSource)
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Matte)
    ) {
BLITTERSIMPLE_INIT
    if (Source->OptimizeData.solidColor) {
      Pixel V = Source->getPixel(0, 0);
      if (V != Source->MatteColor) {
        return FilterSimple_Fill(Dest, Rect, V);
      } else {
        return Trivial_Success;
      }
    }
    _BOS(BlitSimple_Matte, 0) _BOE
    Pixel MaskColor = Source->MatteColor;
BLITTERSIMPLE_BEGIN
    Pixel* MaskArray[2];
BLITTERSIMPLE_LOOPBEGIN
    MaskArray[0] = pDest;
    MaskArray[1] = pSource;
    *pDest = *(MaskArray[pSource->V != MaskColor.V]);
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Matte_Solid_Tint)
    , Pixel Tint) {
BLITTERSIMPLE_INIT
    _BOS(BlitSimple_Matte_Solid_Tint, 1) , Tint _BOE
    Pixel MaskColor = Source->MatteColor;
BLITTERSIMPLE_BEGIN
    Pixel* MaskArray[2];
    MaskArray[1] = &Tint;
BLITTERSIMPLE_LOOPBEGIN
    MaskArray[0] = pDest;
    *pDest = *(MaskArray[pSource->V != MaskColor.V]);
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Matte_Tint)
    , Pixel Tint) {
BLITTERSIMPLE_INIT
    if (Tint[::Alpha] == 0) return BlitSimple_Matte(Dest, Source, Rect, SX, SY);
    if (Tint[::Alpha] == 255) return BlitSimple_Matte_Solid_Tint(Dest, Source, Rect, SX, SY, Tint);
    _BOS(BlitSimple_Matte_Tint, 1) , Tint _BOE
    Pixel MaskColor = Source->MatteColor;
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource, *aDest;
    int tR, tG, tB;
    aSource = AlphaLevelLookup( Tint[::Alpha] );
    aDest = AlphaLevelLookup( Tint[::Alpha] ^ 0xFF );
    tB = AlphaFromLevel(aSource, Tint[::Blue]);
    tG = AlphaFromLevel(aSource, Tint[::Green]);
    tR = AlphaFromLevel(aSource, Tint[::Red]);
BLITTERSIMPLE_LOOPBEGIN
    if (pSource->V != MaskColor.V) {
        (*pDest)[::Blue] = AlphaFromLevel(aDest, (*pSource)[::Blue]) + tB;
        (*pDest)[::Green] = AlphaFromLevel(aDest, (*pSource)[::Green]) + tG;
        (*pDest)[::Red] = AlphaFromLevel(aDest, (*pSource)[::Red]) + tR;
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Matte_Opacity)
    , int Opacity) {
BLITTERSIMPLE_INIT
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return BlitSimple_Matte(Dest, Source, Rect, SX, SY);
    if (Source->OptimizeData.solidColor) {
      Pixel V = Source->getPixel(0, 0);
      if (V != Source->MatteColor) {
        return FilterSimple_Fill_Opacity(Dest, Rect, V, Opacity);
      } else {
        return Trivial_Success;
      }
    }
    _BOS(BlitSimple_Matte_Opacity, 1) , Opacity _BOE
    Pixel MaskColor = Source->MatteColor;
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource, *aDest;
    aSource = AlphaLevelLookup( ClipByte(Opacity) );
    aDest = AlphaLevelLookup( ClipByte(Opacity) ^ 0xFF );
BLITTERSIMPLE_LOOPBEGIN
    if (pSource->V != MaskColor.V) {
      BLENDPIXEL_ALPHA_OPACITY(pDest, pDest, pSource, aDest, aSource)
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Matte_Tint_Opacity)
    , Pixel Tint, int Opacity) {
BLITTERSIMPLE_INIT
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return BlitSimple_Matte_Tint(Dest, Source, Rect, SX, SY, Tint);
    if (Tint[::Alpha] == 0) return BlitSimple_Matte_Opacity(Dest, Source, Rect, SX, SY, Opacity);
    _BOS(BlitSimple_Matte_Tint_Opacity, 2) , Tint, Opacity _BOE
    Pixel MaskColor = Source->MatteColor;
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource, *aDest, *aTint;
    int tR, tG, tB;
    aSource = AlphaLevelLookup(Tint[::Alpha]);
    aTint = AlphaLevelLookup(Tint[::Alpha] ^ 0xFF);
    tB = AlphaFromLevel(aSource, Tint[::Blue]);
    tG = AlphaFromLevel(aSource, Tint[::Green]);
    tR = AlphaFromLevel(aSource, Tint[::Red]);
    aSource = AlphaLevelLookup( ClipByte(Opacity) );
    aDest = AlphaLevelLookup( ClipByte(Opacity) ^ 0xFF );
BLITTERSIMPLE_LOOPBEGIN
    if (pSource->V != MaskColor.V) {
        (*pDest)[::Blue] = AlphaFromLevel2(aDest, (*pDest)[::Blue], aSource, AlphaFromLevel(aTint, (*pSource)[::Blue]) + tB);
        (*pDest)[::Green] = AlphaFromLevel2(aDest, (*pDest)[::Green], aSource, AlphaFromLevel(aTint, (*pSource)[::Green]) + tG);
        (*pDest)[::Red] = AlphaFromLevel2(aDest, (*pDest)[::Red], aSource, AlphaFromLevel(aTint, (*pSource)[::Red]) + tR);
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Additive)
    ) {
BLITTERSIMPLE_INIT
    _BOS(BlitSimple_Additive, 0) _BOE
BLITTERSIMPLE_BEGIN
BLITTERSIMPLE_LOOPBEGIN
    if (pSource->V != 0) {
        BLENDPIXEL_ADDITIVE(pDest, pDest, pSource)
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Additive_Opacity)
    , int Opacity) {
BLITTERSIMPLE_INIT
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return BlitSimple_Additive(Dest, Source, Rect, SX, SY);
    _BOS(BlitSimple_Additive_Opacity, 1) , Opacity _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource;
    aSource = AlphaLevelLookup( ClipByte(Opacity) );
BLITTERSIMPLE_LOOPBEGIN
    if (pSource->V != 0) {
        BLENDPIXEL_ADDITIVE_OPACITY(pDest, pDest, pSource, aSource)
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Additive_SourceAlpha)
    ) {
BLITTERSIMPLE_INIT
    _BOS(BlitSimple_Additive_SourceAlpha, 0) _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource;
BLITTERSIMPLE_LOOPBEGIN
    if (pSource->V != 0) {
		aSource = AlphaLevelLookup((*pSource)[::Alpha]);
		(*pDest)[::Blue] = ClipByteHigh((*pDest)[::Blue] + AlphaFromLevel(aSource, (*pSource)[::Blue]));
		(*pDest)[::Green] = ClipByteHigh((*pDest)[::Green] + AlphaFromLevel(aSource, (*pSource)[::Green]));
		(*pDest)[::Red] = ClipByteHigh((*pDest)[::Red] + AlphaFromLevel(aSource, (*pSource)[::Red]));
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Additive_SourceAlpha_Opacity)
    , int Opacity) {
BLITTERSIMPLE_INIT
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return BlitSimple_Additive_SourceAlpha(Dest, Source, Rect, SX, SY);
    _BOS(BlitSimple_Additive_SourceAlpha_Opacity, 1) , Opacity _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource, *aScale;
	aScale = AlphaLevelLookup(Opacity);
BLITTERSIMPLE_LOOPBEGIN
    if (pSource->V != 0) {
		aSource = AlphaLevelLookup(AlphaFromLevel(aScale, (*pSource)[::Alpha]));
		(*pDest)[::Blue] = ClipByteHigh((*pDest)[::Blue] + AlphaFromLevel(aSource, (*pSource)[::Blue]));
		(*pDest)[::Green] = ClipByteHigh((*pDest)[::Green] + AlphaFromLevel(aSource, (*pSource)[::Green]));
		(*pDest)[::Red] = ClipByteHigh((*pDest)[::Red] + AlphaFromLevel(aSource, (*pSource)[::Red]));
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Screen)
    ) {
BLITTERSIMPLE_INIT
    _BOS(BlitSimple_Screen, 0) _BOE
BLITTERSIMPLE_BEGIN
BLITTERSIMPLE_LOOPBEGIN
    (*pDest)[::Blue] = AlphaLookup((*pDest)[::Blue] ^ 0xFF, (*pSource)[::Blue] ^ 0xFF) ^ 0xFF;
    (*pDest)[::Green] = AlphaLookup((*pDest)[::Green] ^ 0xFF, (*pSource)[::Green] ^ 0xFF) ^ 0xFF;
    (*pDest)[::Red] = AlphaLookup((*pDest)[::Red] ^ 0xFF, (*pSource)[::Red] ^ 0xFF) ^ 0xFF;
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Screen_Opacity)
    , int Opacity) {
BLITTERSIMPLE_INIT
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return BlitSimple_Screen(Dest, Source, Rect, SX, SY);
    _BOS(BlitSimple_Screen_Opacity, 1) , Opacity _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource;
    aSource = AlphaLevelLookup( ClipByte(Opacity) );
BLITTERSIMPLE_LOOPBEGIN
    (*pDest)[::Blue] = AlphaLookup((*pDest)[::Blue] ^ 0xFF, AlphaFromLevel(aSource, (*pSource)[::Blue]) ^ 0xFF) ^ 0xFF;
    (*pDest)[::Green] = AlphaLookup((*pDest)[::Green] ^ 0xFF, AlphaFromLevel(aSource, (*pSource)[::Green]) ^ 0xFF) ^ 0xFF;
    (*pDest)[::Red] = AlphaLookup((*pDest)[::Red] ^ 0xFF, AlphaFromLevel(aSource, (*pSource)[::Red]) ^ 0xFF) ^ 0xFF;
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Multiply)
    ) {
BLITTERSIMPLE_INIT
    _BOS(BlitSimple_Multiply, 0) _BOE
BLITTERSIMPLE_BEGIN
BLITTERSIMPLE_LOOPBEGIN
    if (pSource->V != 0) {
      if (pSource->V == 0xFFFFFFFF) {
      } else {
        (*pDest)[::Blue] = AlphaLookup((*pSource)[::Blue], (*pDest)[::Blue]);
        (*pDest)[::Green] = AlphaLookup((*pSource)[::Green], (*pDest)[::Green]);
        (*pDest)[::Red] = AlphaLookup((*pSource)[::Red], (*pDest)[::Red]);
      }
    } else {
      pDest->V = 0;
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Multiply_Opacity)
    , int Opacity) {
BLITTERSIMPLE_INIT
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return BlitSimple_Multiply(Dest, Source, Rect, SX, SY);
    _BOS(BlitSimple_Multiply_Opacity, 1) , Opacity _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource, *aDest;
    aSource = AlphaLevelLookup( ClipByte(Opacity) );
    aDest = AlphaLevelLookup( ClipByte(Opacity) ^ 0xFF );
BLITTERSIMPLE_LOOPBEGIN
    if (pSource->V != 0) {
      if (pSource->V == 0xFFFFFFFF) {
      } else {
        (*pDest)[::Blue] = AlphaFromLevel(aSource, AlphaLookup((*pSource)[::Blue], (*pDest)[::Blue])) + AlphaFromLevel(aDest, (*pDest)[::Blue]);
        (*pDest)[::Green] = AlphaFromLevel(aSource, AlphaLookup((*pSource)[::Green], (*pDest)[::Green])) + AlphaFromLevel(aDest, (*pDest)[::Green]);
        (*pDest)[::Red] = AlphaFromLevel(aSource, AlphaLookup((*pSource)[::Red], (*pDest)[::Red])) + AlphaFromLevel(aDest, (*pDest)[::Red]);
      }
    } else {
      (*pDest)[::Blue] = AlphaFromLevel(aDest, (*pDest)[::Blue]);
      (*pDest)[::Green] = AlphaFromLevel(aDest, (*pDest)[::Green]);
      (*pDest)[::Red] = AlphaFromLevel(aDest, (*pDest)[::Red]);
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Dodge)
    ) {
BLITTERSIMPLE_INIT
    _BOS(BlitSimple_Dodge, 0) _BOE
BLITTERSIMPLE_BEGIN
BLITTERSIMPLE_LOOPBEGIN
    if ((*pSource)[::Blue] == 255) {
	  if ((*pDest)[::Blue] != 0) {
		(*pDest)[::Blue] = 255;
	  } else {
		(*pDest)[::Blue] = 0;
	  }
    } else {
      (*pDest)[::Blue] = ClipByteHigh(((*pDest)[::Blue] * 255) / ((*pSource)[::Blue] ^ 0xFF));
    }
    if ((*pSource)[::Green] == 255) {
	  if ((*pDest)[::Green] != 0) {
		(*pDest)[::Green] = 255;
	  } else {
		(*pDest)[::Green] = 0;
	  }
    } else {
      (*pDest)[::Green] = ClipByteHigh(((*pDest)[::Green] * 255) / ((*pSource)[::Green] ^ 0xFF));
    }
    if ((*pSource)[::Red] == 255) {
	  if ((*pDest)[::Red] != 0) {
		(*pDest)[::Red] = 255;
	  } else {
		(*pDest)[::Red] = 0;
	  }
    } else {
      (*pDest)[::Red] = ClipByteHigh(((*pDest)[::Red] * 255) / ((*pSource)[::Red] ^ 0xFF));
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Dodge_Opacity)
    , int Opacity) {
BLITTERSIMPLE_INIT
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return BlitSimple_Dodge(Dest, Source, Rect, SX, SY);
    _BOS(BlitSimple_Dodge_Opacity, 1) , Opacity _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource, *aDest;
	Pixel color, *pColor;
    aSource = AlphaLevelLookup( ClipByte(Opacity) );
	aDest = AlphaLevelLookup( ClipByte(Opacity) ^ 0xFF );
	pColor = &color;
BLITTERSIMPLE_LOOPBEGIN
    if ((*pSource)[::Blue] == 255) {
	  if ((*pDest)[::Blue] != 0) {
		color[::Blue] = 255;
	  } else {
		color[::Blue] = 0;
	  }
    } else {
      color[::Blue] = ClipByteHigh(((*pDest)[::Blue] * 255) / ((*pSource)[::Blue] ^ 0xFF));
    }
    if ((*pSource)[::Green] == 255) {
	  if ((*pDest)[::Green] != 0) {
		color[::Green] = 255;
	  } else {
		color[::Green] = 0;
	  }
    } else {
      color[::Green] = ClipByteHigh(((*pDest)[::Green] * 255) / ((*pSource)[::Green] ^ 0xFF));
    }
    if ((*pSource)[::Red] == 255) {
	  if ((*pDest)[::Red] != 0) {
		color[::Red] = 255;
	  } else {
		color[::Red] = 0;
	  }
    } else {
      color[::Red] = ClipByteHigh(((*pDest)[::Red] * 255) / ((*pSource)[::Red] ^ 0xFF));
    }
	BLENDPIXEL_ALPHA_OPACITY(pDest, pDest, pColor, aDest, aSource)
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Burn)
    ) {
BLITTERSIMPLE_INIT
    _BOS(BlitSimple_Burn, 0) _BOE
BLITTERSIMPLE_BEGIN
BLITTERSIMPLE_LOOPBEGIN
    if ((*pSource)[::Blue] == 255) {
	  if ((*pDest)[::Blue] != 255) {
		(*pDest)[::Blue] = 0;
	  } else {
		(*pDest)[::Blue] = 255;
	  }
    } else {
      (*pDest)[::Blue] = ClipByteHigh((((*pDest)[::Blue] ^ 0xFF) * 255) / ((*pSource)[::Blue] ^ 0xFF)) ^ 0xFF;
    }
    if ((*pSource)[::Green] == 255) {
	  if ((*pDest)[::Green] != 255) {
		(*pDest)[::Green] = 0;
	  } else {
		(*pDest)[::Green] = 255;
	  }
    } else {
      (*pDest)[::Green] = ClipByteHigh((((*pDest)[::Green] ^ 0xFF) * 255) / ((*pSource)[::Green] ^ 0xFF)) ^ 0xFF;
    }
    if ((*pSource)[::Red] == 255) {
	  if ((*pDest)[::Red] != 255) {
		(*pDest)[::Red] = 0;
	  } else {
		(*pDest)[::Red] = 255;
	  }
    } else {
      (*pDest)[::Red] = ClipByteHigh((((*pDest)[::Red] ^ 0xFF) * 255) / ((*pSource)[::Red] ^ 0xFF)) ^ 0xFF;
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Burn_Opacity)
    , int Opacity) {
BLITTERSIMPLE_INIT
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return BlitSimple_Burn(Dest, Source, Rect, SX, SY);
    _BOS(BlitSimple_Burn_Opacity, 1) , Opacity _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource, *aDest;
	Pixel color, *pColor;
    aSource = AlphaLevelLookup( ClipByte(Opacity) );
	aDest = AlphaLevelLookup( ClipByte(Opacity) ^ 0xFF );
	pColor = &color;
BLITTERSIMPLE_LOOPBEGIN
    if ((*pSource)[::Blue] == 255) {
	  if ((*pDest)[::Blue] != 255) {
		color[::Blue] = 0;
	  } else {
		color[::Blue] = 255;
	  }
    } else {
      color[::Blue] = ClipByteHigh((((*pDest)[::Blue] ^ 0xFF) * 255) / ((*pSource)[::Blue] ^ 0xFF)) ^ 0xFF;
    }
    if ((*pSource)[::Green] == 255) {
	  if ((*pDest)[::Green] != 255) {
		color[::Green] = 0;
	  } else {
		color[::Green] = 255;
	  }
    } else {
      color[::Green] = ClipByteHigh((((*pDest)[::Green] ^ 0xFF) * 255) / ((*pSource)[::Green] ^ 0xFF)) ^ 0xFF;
    }
    if ((*pSource)[::Red] == 255) {
	  if ((*pDest)[::Red] != 255) {
		color[::Red] = 0;
	  } else {
		color[::Red] = 255;
	  }
    } else {
      color[::Red] = ClipByteHigh((((*pDest)[::Red] ^ 0xFF) * 255) / ((*pSource)[::Red] ^ 0xFF)) ^ 0xFF;
    }
	BLENDPIXEL_ALPHA_OPACITY(pDest, pDest, pColor, aDest, aSource)
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Subtractive)
    ) {
BLITTERSIMPLE_INIT
    _BOS(BlitSimple_Subtractive, 0) _BOE
BLITTERSIMPLE_BEGIN
BLITTERSIMPLE_LOOPBEGIN
    if (pSource->V != 0) {
        BLENDPIXEL_SUBTRACTIVE(pDest, pDest, pSource)
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Subtractive_Opacity)
    , int Opacity) {
BLITTERSIMPLE_INIT
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return BlitSimple_Subtractive(Dest, Source, Rect, SX, SY);
    _BOS(BlitSimple_Subtractive_Opacity, 1) , Opacity _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource;
    aSource = AlphaLevelLookup( ClipByte(Opacity) );
BLITTERSIMPLE_LOOPBEGIN
    if (pSource->V != 0) {
        BLENDPIXEL_SUBTRACTIVE_OPACITY(pDest, pDest, pSource, aSource)
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Subtractive_SourceAlpha)
    ) {
BLITTERSIMPLE_INIT
    _BOS(BlitSimple_Subtractive_SourceAlpha, 0) _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource;
BLITTERSIMPLE_LOOPBEGIN
    if (pSource->V != 0) {
		aSource = AlphaLevelLookup((*pSource)[::Alpha]);
		(*pDest)[::Blue] = ClipByteLow((*pDest)[::Blue] - AlphaFromLevel(aSource, (*pSource)[::Blue]));
		(*pDest)[::Green] = ClipByteLow((*pDest)[::Green] - AlphaFromLevel(aSource, (*pSource)[::Green]));
		(*pDest)[::Red] = ClipByteLow((*pDest)[::Red] - AlphaFromLevel(aSource, (*pSource)[::Red]));
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Subtractive_SourceAlpha_Opacity)
    , int Opacity) {
BLITTERSIMPLE_INIT
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return BlitSimple_Subtractive_SourceAlpha(Dest, Source, Rect, SX, SY);
    _BOS(BlitSimple_Subtractive_SourceAlpha_Opacity, 1) , Opacity _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource, *aScale;
	aScale = AlphaLevelLookup(Opacity);
BLITTERSIMPLE_LOOPBEGIN
    if (pSource->V != 0) {
		aSource = AlphaLevelLookup(AlphaFromLevel(aScale, (*pSource)[::Alpha]));
		(*pDest)[::Blue] = ClipByteLow((*pDest)[::Blue] - AlphaFromLevel(aSource, (*pSource)[::Blue]));
		(*pDest)[::Green] = ClipByteLow((*pDest)[::Green] - AlphaFromLevel(aSource, (*pSource)[::Green]));
		(*pDest)[::Red] = ClipByteLow((*pDest)[::Red] - AlphaFromLevel(aSource, (*pSource)[::Red]));
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Erase)
    ) {
BLITTERSIMPLE_INIT
    _BOS(BlitSimple_Erase, 0) _BOE
BLITTERSIMPLE_BEGIN
BLITTERSIMPLE_LOOPBEGIN
    if (pSource->V != 0) {
        (*pDest)[::Alpha] = ClipByteLow((*pDest)[::Alpha] - (*pSource)[::Red]);
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Erase_Opacity)
    , int Opacity) {
BLITTERSIMPLE_INIT
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return BlitSimple_Erase(Dest, Source, Rect, SX, SY);
    _BOS(BlitSimple_Erase_Opacity, 1) , Opacity _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource;
    aSource = AlphaLevelLookup( ClipByte(Opacity) );
BLITTERSIMPLE_LOOPBEGIN
    if (pSource->V != 0) {
        (*pDest)[::Alpha] = ClipByteLow((*pDest)[::Alpha] - AlphaFromLevel(aSource, (*pSource)[::Red]));
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Unerase)
    ) {
BLITTERSIMPLE_INIT
    _BOS(BlitSimple_Unerase, 0) _BOE
BLITTERSIMPLE_BEGIN
BLITTERSIMPLE_LOOPBEGIN
    if (pSource->V != 0) {
        (*pDest)[::Alpha] = ClipByteHigh((*pDest)[::Alpha] + (*pSource)[::Red]);
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Unerase_Opacity)
    , int Opacity) {
BLITTERSIMPLE_INIT
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return BlitSimple_Unerase(Dest, Source, Rect, SX, SY);
    _BOS(BlitSimple_Unerase_Opacity, 1) , Opacity _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource;
    aSource = AlphaLevelLookup( ClipByte(Opacity) );
BLITTERSIMPLE_LOOPBEGIN
    if (pSource->V != 0) {
        (*pDest)[::Alpha] = ClipByteHigh((*pDest)[::Alpha] + AlphaFromLevel(aSource, (*pSource)[::Red]));
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(SourceAlpha)
    ) {
BLITTERSIMPLE_INIT
    if (Source->OptimizeData.solidColor) {
      return FilterSimple_Fill_SourceAlpha(Dest, Rect, Source->getPixel(0, 0));
    }
    if (Source->OptimizeData.transparentOnly) return Trivial_Success;
    _BOS(BlitSimple_SourceAlpha, 0) _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource, *aDest;
BLITTERSIMPLE_LOOPBEGIN
    if ((*pSource)[::Alpha]) {
      if ((*pSource)[::Alpha] == 255) {
        (*pDest)[::Blue] = (*pSource)[::Blue];
        (*pDest)[::Green] = (*pSource)[::Green];
        (*pDest)[::Red] = (*pSource)[::Red];
      } else {
        aSource = AlphaLevelLookup( (*pSource)[::Alpha] );
        aDest = AlphaLevelLookup( (*pSource)[::Alpha] ^ 0xFF );
        (*pDest)[::Blue] = AlphaFromLevel2(aDest, (*pDest)[::Blue], aSource, (*pSource)[::Blue]);
        (*pDest)[::Green] = AlphaFromLevel2(aDest, (*pDest)[::Green], aSource, (*pSource)[::Green]);
        (*pDest)[::Red] = AlphaFromLevel2(aDest, (*pDest)[::Red], aSource, (*pSource)[::Red]);
      }
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(SourceAlpha_Opacity)
    , int Opacity) {
BLITTERSIMPLE_INIT
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return BlitSimple_SourceAlpha(Dest, Source, Rect, SX, SY);
    if (Source->OptimizeData.solidColor) {
      return FilterSimple_Fill_SourceAlpha_Opacity(Dest, Rect, Source->getPixel(0, 0), Opacity);
    }
    if (Source->OptimizeData.transparentOnly) return Trivial_Success;
    _BOS(BlitSimple_SourceAlpha_Opacity, 1) , Opacity _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource, *aDest, *aScale, *aScale2;
    aScale = AlphaLevelLookup( ClipByte(Opacity) );
    aScale2 = AlphaLevelLookup( ClipByte(Opacity) ^ 0xFF );
BLITTERSIMPLE_LOOPBEGIN
    if ((*pSource)[::Alpha]) {
      if ((*pSource)[::Alpha] == 255) {
        (*pDest)[::Blue] = AlphaFromLevel2(aScale2, (*pDest)[::Blue], aScale, (*pSource)[::Blue]);
        (*pDest)[::Green] = AlphaFromLevel2(aScale2, (*pDest)[::Green], aScale, (*pSource)[::Green]);
        (*pDest)[::Red] = AlphaFromLevel2(aScale2, (*pDest)[::Red], aScale, (*pSource)[::Red]);
      } else {
        aSource = AlphaLevelLookup( AlphaFromLevel(aScale, (*pSource)[::Alpha]) );
        aDest = AlphaLevelLookup( AlphaFromLevel(aScale, (*pSource)[::Alpha]) ^ 0xFF );
        (*pDest)[::Blue] = AlphaFromLevel2(aDest, (*pDest)[::Blue], aSource, (*pSource)[::Blue]);
        (*pDest)[::Green] = AlphaFromLevel2(aDest, (*pDest)[::Green], aSource, (*pSource)[::Green]);
        (*pDest)[::Red] = AlphaFromLevel2(aDest, (*pDest)[::Red], aSource, (*pSource)[::Red]);
      }
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(BlitBehind)
    ) {
BLITTERSIMPLE_INIT
    _BOS(BlitSimple_BlitBehind, 0) _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource, *aDest;
BLITTERSIMPLE_LOOPBEGIN
    if ((*pDest)[::Alpha] < 255) {
      if ((*pDest)[::Alpha] == 0) {
        pDest->V = pSource->V;
      } else {
        aSource = AlphaLevelLookup( (*pDest)[::Alpha] ^ 0xFF );
        aDest = AlphaLevelLookup( (*pDest)[::Alpha] );
        (*pDest)[::Blue] = AlphaFromLevel2(aDest, (*pDest)[::Blue], aSource, (*pSource)[::Blue]);
        (*pDest)[::Green] = AlphaFromLevel2(aDest, (*pDest)[::Green], aSource, (*pSource)[::Green]);
        (*pDest)[::Red] = AlphaFromLevel2(aDest, (*pDest)[::Red], aSource, (*pSource)[::Red]);
      }
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(BlitBehind_Opacity)
    , int Opacity) {
BLITTERSIMPLE_INIT
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return BlitSimple_BlitBehind(Dest, Source, Rect, SX, SY);
    _BOS(BlitSimple_BlitBehind_Opacity, 1) , Opacity _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource, *aDest, *aScale, *aScale2;
    aScale = AlphaLevelLookup( ClipByte(Opacity) );
    aScale2 = AlphaLevelLookup( ClipByte(Opacity) ^ 0xFF );
BLITTERSIMPLE_LOOPBEGIN
    if ((*pDest)[::Alpha] < 255) {
      if ((*pDest)[::Alpha] == 0) {
        (*pDest)[::Blue] = AlphaFromLevel(aScale, (*pSource)[::Blue]);
        (*pDest)[::Green] = AlphaFromLevel(aScale, (*pSource)[::Green]);
        (*pDest)[::Red] = AlphaFromLevel(aScale, (*pSource)[::Red]);
      } else {
        aSource = AlphaLevelLookup( AlphaFromLevel(aScale, (*pDest)[::Alpha] ^ 0xFF) );
        aDest = AlphaLevelLookup( (*pDest)[::Alpha] );
        (*pDest)[::Blue] = AlphaFromLevel2(aDest, (*pDest)[::Blue], aSource, (*pSource)[::Blue]);
        (*pDest)[::Green] = AlphaFromLevel2(aDest, (*pDest)[::Green], aSource, (*pSource)[::Green]);
        (*pDest)[::Red] = AlphaFromLevel2(aDest, (*pDest)[::Red], aSource, (*pSource)[::Red]);
      }
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Merge)
    ) {
BLITTERSIMPLE_INIT
    if (Source->OptimizeData.transparentOnly) return Trivial_Success;
    _BOS(BlitSimple_Merge, 0) _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource, *aDest;
BLITTERSIMPLE_LOOPBEGIN
    if ((*pSource)[::Alpha]) {
      if ((*pSource)[::Alpha] == 255) {
        pDest->V = pSource->V;
      } else {
        aSource = AlphaLevelLookup( ClipByteHigh((*pSource)[::Alpha] + ( (*pDest)[::Alpha] ^ 0xFF )) );
        aDest = AlphaLevelLookup( ClipByteHigh(((*pSource)[::Alpha] + ( (*pDest)[::Alpha] ^ 0xFF ))) ^ 0xFF );
        (*pDest)[::Blue] = AlphaFromLevel2(aDest, (*pDest)[::Blue], aSource, (*pSource)[::Blue]);
        (*pDest)[::Green] = AlphaFromLevel2(aDest, (*pDest)[::Green], aSource, (*pSource)[::Green]);
        (*pDest)[::Red] = AlphaFromLevel2(aDest, (*pDest)[::Red], aSource, (*pSource)[::Red]);
        (*pDest)[::Alpha] = ClipByteHigh((*pDest)[::Alpha] + AlphaFromLevel(aSource, (*pSource)[::Alpha]));
      }
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Merge_Opacity)
    , int Opacity) {
BLITTERSIMPLE_INIT
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return BlitSimple_Merge(Dest, Source, Rect, SX, SY);
    if (Source->OptimizeData.transparentOnly) return Trivial_Success;
    _BOS(BlitSimple_Merge_Opacity, 1) , Opacity _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource, *aDest, *aScale, *aScale2;
    aScale = AlphaLevelLookup( ClipByte(Opacity) );
    aScale2 = AlphaLevelLookup( ClipByte(Opacity) ^ 0xFF );
BLITTERSIMPLE_LOOPBEGIN
    if ((*pSource)[::Alpha]) {
      if ((*pSource)[::Alpha] == 255) {
        (*pDest)[::Blue] = AlphaFromLevel2(aScale2, (*pDest)[::Blue], aScale, (*pSource)[::Blue]);
        (*pDest)[::Green] = AlphaFromLevel2(aScale2, (*pDest)[::Green], aScale, (*pSource)[::Green]);
        (*pDest)[::Red] = AlphaFromLevel2(aScale2, (*pDest)[::Red], aScale, (*pSource)[::Red]);
        (*pDest)[::Alpha] = ClipByteHigh((*pDest)[::Alpha] + AlphaFromLevel(aScale, Opacity));
//        (*pDest)[::Alpha] = ClipByteHigh((*pDest)[::Alpha] + Opacity);
      } else {
        aSource = AlphaLevelLookup( AlphaFromLevel(aScale, ClipByteHigh((*pSource)[::Alpha] + ( (*pDest)[::Alpha] ^ 0xFF ))));
        aDest = AlphaLevelLookup( AlphaFromLevel(aScale, ClipByteHigh(((*pSource)[::Alpha] + ( (*pDest)[::Alpha] ^ 0xFF )))) ^ 0xFF );
        (*pDest)[::Blue] = AlphaFromLevel2(aDest, (*pDest)[::Blue], aSource, (*pSource)[::Blue]);
        (*pDest)[::Green] = AlphaFromLevel2(aDest, (*pDest)[::Green], aSource, (*pSource)[::Green]);
        (*pDest)[::Red] = AlphaFromLevel2(aDest, (*pDest)[::Red], aSource, (*pSource)[::Red]);
        (*pDest)[::Alpha] = ClipByteHigh((*pDest)[::Alpha] + AlphaFromLevel(aScale, AlphaFromLevel(aScale, (*pSource)[::Alpha])));
//        (*pDest)[::Alpha] = ClipByteHigh((*pDest)[::Alpha] + AlphaFromLevel(aScale, (*pSource)[::Alpha]));
      }
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(SourceAlpha_Solid_Tint)
    , Pixel Tint) {
BLITTERSIMPLE_INIT
    _BOS(BlitSimple_SourceAlpha_Solid_Tint, 1) , Tint _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource, *aDest;
BLITTERSIMPLE_LOOPBEGIN
    if ((*pSource)[::Alpha]) {
        aSource = AlphaLevelLookup( (*pSource)[::Alpha] );
        aDest = AlphaLevelLookup( (*pSource)[::Alpha] ^ 0xFF );
        (*pDest)[::Blue] = AlphaFromLevel2(aDest, (*pDest)[::Blue], aSource, Tint[::Blue]);
        (*pDest)[::Green] = AlphaFromLevel2(aDest, (*pDest)[::Green], aSource, Tint[::Green]);
        (*pDest)[::Red] = AlphaFromLevel2(aDest, (*pDest)[::Red], aSource, Tint[::Red]);
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(SourceAlpha_Tint)
    , Pixel Tint) {
BLITTERSIMPLE_INIT
    if (Tint[::Alpha] == 0) return BlitSimple_Automatic_SourceAlpha_Opacity(Dest, Source, Rect, SX, SY, 255);
    if (Tint[::Alpha] == 255) return BlitSimple_SourceAlpha_Solid_Tint(Dest, Source, Rect, SX, SY, Tint);
    _BOS(BlitSimple_SourceAlpha_Tint, 1) , Tint _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource, *aDest, *aTint;
    int tR, tG, tB;
    aSource = AlphaLevelLookup(Tint[::Alpha]);
    aTint = AlphaLevelLookup(Tint[::Alpha] ^ 0xFF);
    tB = AlphaFromLevel(aSource, Tint[::Blue]);
    tG = AlphaFromLevel(aSource, Tint[::Green]);
    tR = AlphaFromLevel(aSource, Tint[::Red]);
BLITTERSIMPLE_LOOPBEGIN
    if ((*pSource)[::Alpha]) {
        aSource = AlphaLevelLookup( (*pSource)[::Alpha] );
        aDest = AlphaLevelLookup( (*pSource)[::Alpha] ^ 0xFF );
        (*pDest)[::Blue] = AlphaFromLevel2(aDest, (*pDest)[::Blue], aSource, AlphaFromLevel(aTint, (*pSource)[::Blue]) + tB);
        (*pDest)[::Green] = AlphaFromLevel2(aDest, (*pDest)[::Green], aSource, AlphaFromLevel(aTint, (*pSource)[::Green]) + tG);
        (*pDest)[::Red] = AlphaFromLevel2(aDest, (*pDest)[::Red], aSource, AlphaFromLevel(aTint, (*pSource)[::Red]) + tR);
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(SourceAlpha_ColorMask)
    , Pixel Mask) {
BLITTERSIMPLE_INIT
    if (Mask[::Alpha] == 0) return Trivial_Success;
    if (Mask.V == 0xFFFFFFFF) return BlitSimple_SourceAlpha(Dest, Source, Rect, SX, SY);
    if ((Mask[::Red] == 255) && (Mask[::Green] == 255) && (Mask[::Blue] == 255)) return BlitSimple_SourceAlpha_Opacity(Dest, Source, Rect, SX, SY, Mask[::Alpha]);
    _BOS(BlitSimple_SourceAlpha_ColorMask, 1) , Mask _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aMask[4], *aSource, *aDest;
    for (int i = 0; i < 4; i++) {
      aMask[i] = AlphaLevelLookup(Mask[i]);
    }
BLITTERSIMPLE_LOOPBEGIN
    if ((*pSource)[::Alpha]) {
        Byte a = AlphaFromLevel(aMask[::Alpha], (*pSource)[::Alpha]);
        aSource = AlphaLevelLookup( a );
        aDest = AlphaLevelLookup( a ^ 0xFF );
        (*pDest)[::Blue] = AlphaFromLevel2(aDest, (*pDest)[::Blue], aSource, AlphaFromLevel(aMask[::Blue], (*pSource)[::Blue]));
        (*pDest)[::Green] = AlphaFromLevel2(aDest, (*pDest)[::Green], aSource, AlphaFromLevel(aMask[::Green], (*pSource)[::Green]));
        (*pDest)[::Red] = AlphaFromLevel2(aDest, (*pDest)[::Red], aSource, AlphaFromLevel(aMask[::Red], (*pSource)[::Red]));
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(SourceAlpha_Tint_Opacity)
    , Pixel Tint, int Opacity) {
BLITTERSIMPLE_INIT
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return BlitSimple_SourceAlpha_Tint(Dest, Source, Rect, SX, SY, Tint);
    if (Tint[::Alpha] == 0) return BlitSimple_Automatic_SourceAlpha_Opacity(Dest, Source, Rect, SX, SY, Opacity);
    _BOS(BlitSimple_SourceAlpha_Tint_Opacity, 2) , Tint, Opacity _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource, *aDest, *aScale, *aTint;
    int tR, tG, tB;
    aSource = AlphaLevelLookup(Tint[::Alpha]);
    aTint = AlphaLevelLookup(Tint[::Alpha] ^ 0xFF);
    tB = AlphaFromLevel(aSource, Tint[::Blue]);
    tG = AlphaFromLevel(aSource, Tint[::Green]);
    tR = AlphaFromLevel(aSource, Tint[::Red]);
    aScale = AlphaLevelLookup( ClipByte(Opacity) );
BLITTERSIMPLE_LOOPBEGIN
    if ((*pSource)[::Alpha]) {
        aSource = AlphaLevelLookup( AlphaFromLevel(aScale, (*pSource)[::Alpha]) );
        aDest = AlphaLevelLookup( AlphaFromLevel(aScale, (*pSource)[::Alpha]) ^ 0xFF );
        (*pDest)[::Blue] = AlphaFromLevel2(aDest, (*pDest)[::Blue], aSource, AlphaFromLevel(aTint, (*pSource)[::Blue]) + tB);
        (*pDest)[::Green] = AlphaFromLevel2(aDest, (*pDest)[::Green], aSource, AlphaFromLevel(aTint, (*pSource)[::Green]) + tG);
        (*pDest)[::Red] = AlphaFromLevel2(aDest, (*pDest)[::Red], aSource, AlphaFromLevel(aTint, (*pSource)[::Red]) + tR);
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(SourceAlphaMatte)
    ) {
BLITTERSIMPLE_INIT
  Pixel* MaskArray[2];
  if (Source->OptimizeData.solidColor) {
    return FilterSimple_Fill_SourceAlpha(Dest, Rect, Source->getPixel(0, 0));
  }
  _BOS(BlitSimple_SourceAlphaMatte, 0) _BOE
BLITTERSIMPLE_BEGIN
BLITTERSIMPLE_LOOPBEGIN
    MaskArray[0] = pDest;
    MaskArray[1] = pSource;
    *pDest = *(MaskArray[((*pSource)[::Alpha] > 0)]);
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(SourceAlphaMatte_Opacity)
    , int Opacity) {
BLITTERSIMPLE_INIT
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return BlitSimple_SourceAlphaMatte(Dest, Source, Rect, SX, SY);
    if (Source->OptimizeData.solidColor) {
      return FilterSimple_Fill_SourceAlpha_Opacity(Dest, Rect, Source->getPixel(0, 0), Opacity);
    }
    _BOS(BlitSimple_SourceAlphaMatte_Opacity, 1) , Opacity _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource, *aDest;
    aSource = AlphaLevelLookup( ClipByte(Opacity) );
    aDest = AlphaLevelLookup( ClipByte(Opacity) ^ 0xFF );
BLITTERSIMPLE_LOOPBEGIN
    if ((*pSource)[::Alpha]) {
          BLENDPIXEL_ALPHA_OPACITY(pDest, pDest, pSource, aDest, aSource)
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Font_SourceAlpha)
    , Pixel Color) {
BLITTERSIMPLE_INIT
    if (Source->OptimizeData.transparentOnly) return Trivial_Success;
    _BOS(BlitSimple_Font_SourceAlpha, 1) , Color _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource, *aDest, *aScale, *aValue;
    aScale = AlphaLevelLookup( Color[::Alpha] );
BLITTERSIMPLE_LOOPBEGIN
    if ((*pSource)[::Alpha]) {
        aSource = AlphaLevelLookup( AlphaFromLevel(aScale, (*pSource)[::Alpha]) );
        aDest = AlphaLevelLookup( AlphaFromLevel(aScale, (*pSource)[::Alpha]) ^ 0xFF );
        aValue = AlphaLevelLookup( (*pSource)[::Red] );
        (*pDest)[::Blue] = AlphaFromLevel2(aDest, (*pDest)[::Blue], aSource, AlphaFromLevel(aValue, Color[::Blue]));
        (*pDest)[::Green] = AlphaFromLevel2(aDest, (*pDest)[::Green], aSource, AlphaFromLevel(aValue, Color[::Green]));
        (*pDest)[::Red] = AlphaFromLevel2(aDest, (*pDest)[::Red], aSource, AlphaFromLevel(aValue, Color[::Red]));
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Font_SourceAlpha_Opacity)
    , Pixel Color, int Opacity) {
BLITTERSIMPLE_INIT
    if (Source->OptimizeData.transparentOnly) return Trivial_Success;
    _BOS(BlitSimple_Font_SourceAlpha_Opacity, 2) , Color, Opacity _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource, *aDest, *aScale, *aValue;
    aScale = AlphaLevelLookup( ScaleAlpha(Color, Opacity)[::Alpha] );
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return BlitSimple_Font_SourceAlpha(Dest, Source, Rect, SX, SY, Color);
BLITTERSIMPLE_LOOPBEGIN
    if ((*pSource)[::Alpha]) {
        aSource = AlphaLevelLookup( AlphaFromLevel(aScale, (*pSource)[::Alpha]) );
        aDest = AlphaLevelLookup( AlphaFromLevel(aScale, (*pSource)[::Alpha]) ^ 0xFF );
        aValue = AlphaLevelLookup( (*pSource)[::Red] );
        (*pDest)[::Blue] = AlphaFromLevel2(aDest, (*pDest)[::Blue], aSource, AlphaFromLevel(aValue, Color[::Blue]));
        (*pDest)[::Green] = AlphaFromLevel2(aDest, (*pDest)[::Green], aSource, AlphaFromLevel(aValue, Color[::Green]));
        (*pDest)[::Red] = AlphaFromLevel2(aDest, (*pDest)[::Red], aSource, AlphaFromLevel(aValue, Color[::Red]));
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Font)
    , Pixel Color) {
BLITTERSIMPLE_INIT
    _BOS(BlitSimple_Font, 1) , Color _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource, *aDest, *aScale;
    aScale = AlphaLevelLookup( Color[::Alpha] );
BLITTERSIMPLE_LOOPBEGIN
    if (pSource) {
        aSource = AlphaLevelLookup( AlphaFromLevel(aScale, (*pSource)[::Red]) );
        aDest = AlphaLevelLookup( AlphaFromLevel(aScale, (*pSource)[::Red]) ^ 0xFF );
        (*pDest)[::Blue] = AlphaFromLevel2(aDest, (*pDest)[::Blue], aSource, Color[::Blue]);
        (*pDest)[::Green] = AlphaFromLevel2(aDest, (*pDest)[::Green], aSource, Color[::Green]);
        (*pDest)[::Red] = AlphaFromLevel2(aDest, (*pDest)[::Red], aSource, Color[::Red]);
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Font_Opacity)
    , Pixel Color, int Opacity) {
BLITTERSIMPLE_INIT
    _BOS(BlitSimple_Font_Opacity, 2) , Color, Opacity _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource, *aDest, *aScale;
    aScale = AlphaLevelLookup( AlphaLookup(Color[::Alpha], ClipByte(Opacity)) );
BLITTERSIMPLE_LOOPBEGIN
    if (pSource) {
        aSource = AlphaLevelLookup( AlphaFromLevel(aScale, (*pSource)[::Red]) );
        aDest = AlphaLevelLookup( AlphaFromLevel(aScale, (*pSource)[::Red]) ^ 0xFF );
        (*pDest)[::Blue] = AlphaFromLevel2(aDest, (*pDest)[::Blue], aSource, Color[::Blue]);
        (*pDest)[::Green] = AlphaFromLevel2(aDest, (*pDest)[::Green], aSource, Color[::Green]);
        (*pDest)[::Red] = AlphaFromLevel2(aDest, (*pDest)[::Red], aSource, Color[::Red]);
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Font_SourceAlpha_RGB)
    , Pixel Color) {
BLITTERSIMPLE_INIT
    if (Source->OptimizeData.transparentOnly) return Trivial_Success;
    _BOS(BlitSimple_Font_SourceAlpha_RGB, 1) , Color _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource, *aDest, *aScale;
    AlphaLevel *aRed, *aGreen, *aBlue;
    aScale = AlphaLevelLookup( Color[::Alpha] );
    if (Source->OptimizeData.grayscaleOnly) return BlitSimple_Font_SourceAlpha(Dest, Source, Rect, SX, SY, Color);
BLITTERSIMPLE_LOOPBEGIN
    if ((*pSource)[::Alpha]) {
        aSource = AlphaLevelLookup( AlphaFromLevel(aScale, (*pSource)[::Alpha]) );
        aDest = AlphaLevelLookup( AlphaFromLevel(aScale, (*pSource)[::Alpha]) ^ 0xFF );
        aBlue = AlphaLevelLookup( (*pSource)[::Blue] );
        aGreen = AlphaLevelLookup( (*pSource)[::Green] );
        aRed = AlphaLevelLookup( (*pSource)[::Red] );
        (*pDest)[::Blue] = AlphaFromLevel2(aDest, (*pDest)[::Blue], aSource, AlphaFromLevel(aBlue, Color[::Blue]));
        (*pDest)[::Green] = AlphaFromLevel2(aDest, (*pDest)[::Green], aSource, AlphaFromLevel(aGreen, Color[::Green]));
        (*pDest)[::Red] = AlphaFromLevel2(aDest, (*pDest)[::Red], aSource, AlphaFromLevel(aRed, Color[::Red]));
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Font_SourceAlpha_RGB_Opacity)
    , Pixel Color, int Opacity) {
BLITTERSIMPLE_INIT
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return BlitSimple_Font_SourceAlpha_RGB(Dest, Source, Rect, SX, SY, Color);
    if (Source->OptimizeData.grayscaleOnly) return BlitSimple_Font_SourceAlpha_Opacity(Dest, Source, Rect, SX, SY, Color, Opacity);
    if (Source->OptimizeData.transparentOnly) return Trivial_Success;
    _BOS(BlitSimple_Font_SourceAlpha_RGB_Opacity, 2) , Color, Opacity _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource, *aDest, *aScale;
    AlphaLevel *aRed, *aGreen, *aBlue;
    aScale = AlphaLevelLookup( ScaleAlpha(Color, Opacity)[::Alpha] );
BLITTERSIMPLE_LOOPBEGIN
    if ((*pSource)[::Alpha]) {
        aSource = AlphaLevelLookup( AlphaFromLevel(aScale, (*pSource)[::Alpha]) );
        aDest = AlphaLevelLookup( AlphaFromLevel(aScale, (*pSource)[::Alpha]) ^ 0xFF );
        aBlue = AlphaLevelLookup( (*pSource)[::Blue] );
        aGreen = AlphaLevelLookup( (*pSource)[::Green] );
        aRed = AlphaLevelLookup( (*pSource)[::Red] );
        (*pDest)[::Blue] = AlphaFromLevel2(aDest, (*pDest)[::Blue], aSource, AlphaFromLevel(aBlue, Color[::Blue]));
        (*pDest)[::Green] = AlphaFromLevel2(aDest, (*pDest)[::Green], aSource, AlphaFromLevel(aGreen, Color[::Green]));
        (*pDest)[::Red] = AlphaFromLevel2(aDest, (*pDest)[::Red], aSource, AlphaFromLevel(aRed, Color[::Red]));
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Font_Merge_RGB)
    , Pixel Color) {
BLITTERSIMPLE_INIT
    if (Source->OptimizeData.transparentOnly) return Trivial_Success;
    _BOS(BlitSimple_Font_Merge_RGB, 1) , Color _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource, *aDest, *aScale;
    AlphaLevel *aRed, *aGreen, *aBlue;
    aScale = AlphaLevelLookup( Color[::Alpha] );
BLITTERSIMPLE_LOOPBEGIN
    if ((*pSource)[::Alpha]) {
        aSource = AlphaLevelLookup( AlphaFromLevel(aScale, ClipByteHigh((*pSource)[::Alpha] + ( (*pDest)[::Alpha] ^ 0xFF ))) );
        aDest = AlphaLevelLookup( AlphaFromLevel(aScale, ClipByteHigh((*pSource)[::Alpha] + ( (*pDest)[::Alpha] ^ 0xFF ))) ^ 0xFF );
        aBlue = AlphaLevelLookup( (*pSource)[::Blue] );
        aGreen = AlphaLevelLookup( (*pSource)[::Green] );
        aRed = AlphaLevelLookup( (*pSource)[::Red] );
        (*pDest)[::Blue] = AlphaFromLevel2(aDest, (*pDest)[::Blue], aSource, AlphaFromLevel(aBlue, Color[::Blue]));
        (*pDest)[::Green] = AlphaFromLevel2(aDest, (*pDest)[::Green], aSource, AlphaFromLevel(aGreen, Color[::Green]));
        (*pDest)[::Red] = AlphaFromLevel2(aDest, (*pDest)[::Red], aSource, AlphaFromLevel(aRed, Color[::Red]));
        (*pDest)[::Alpha] = ClipByteHigh((*pDest)[::Alpha] + AlphaFromLevel(aScale, AlphaFromLevel(aScale, (*pSource)[::Alpha])));
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Font_Merge_RGB_Opacity)
    , Pixel Color, int Opacity) {
BLITTERSIMPLE_INIT
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return BlitSimple_Font_Merge_RGB(Dest, Source, Rect, SX, SY, Color);
    if (Source->OptimizeData.transparentOnly) return Trivial_Success;
    _BOS(BlitSimple_Font_Merge_RGB_Opacity, 2) , Color, Opacity _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aSource, *aDest, *aScale;
    AlphaLevel *aRed, *aGreen, *aBlue;
    aScale = AlphaLevelLookup( ScaleAlpha(Color, Opacity)[::Alpha] );
BLITTERSIMPLE_LOOPBEGIN
    if ((*pSource)[::Alpha]) {
        aSource = AlphaLevelLookup( AlphaFromLevel(aScale, ClipByteHigh((*pSource)[::Alpha] + ( (*pDest)[::Alpha] ^ 0xFF ))) );
        aDest = AlphaLevelLookup( AlphaFromLevel(aScale, ClipByteHigh((*pSource)[::Alpha] + ( (*pDest)[::Alpha] ^ 0xFF ))) ^ 0xFF );
        aBlue = AlphaLevelLookup( (*pSource)[::Blue] );
        aGreen = AlphaLevelLookup( (*pSource)[::Green] );
        aRed = AlphaLevelLookup( (*pSource)[::Red] );
        (*pDest)[::Blue] = AlphaFromLevel2(aDest, (*pDest)[::Blue], aSource, AlphaFromLevel(aBlue, Color[::Blue]));
        (*pDest)[::Green] = AlphaFromLevel2(aDest, (*pDest)[::Green], aSource, AlphaFromLevel(aGreen, Color[::Green]));
        (*pDest)[::Red] = AlphaFromLevel2(aDest, (*pDest)[::Red], aSource, AlphaFromLevel(aRed, Color[::Red]));
        (*pDest)[::Alpha] = ClipByteHigh((*pDest)[::Alpha] + AlphaFromLevel(aScale, AlphaFromLevel(aScale, (*pSource)[::Alpha])));
    }
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Lightmap)
    ) {
BLITTERSIMPLE_INIT
    _BOS(BlitSimple_Lightmap, 0) _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aLookup[256], *aScale;
    for (int i = 0; i < 256; i++) {
        aLookup[i] = AlphaLevelLookup(ClipValue(i*2,0,511));
    }
BLITTERSIMPLE_LOOPBEGIN
    aScale = aLookup[(*pSource)[::Red]];
    (*pDest)[::Blue] = AlphaFromLevel(aScale, (*pDest)[::Blue]);
    (*pDest)[::Green] = AlphaFromLevel(aScale, (*pDest)[::Green]);
    (*pDest)[::Red] = AlphaFromLevel(aScale, (*pDest)[::Red]);
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Lightmap_RGB)
    ) {
BLITTERSIMPLE_INIT
    if (Source->OptimizeData.grayscaleOnly) return BlitSimple_Lightmap(Dest, Source, Rect, SX, SY);
    _BOS(BlitSimple_Lightmap_RGB, 0) _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aLookup[256];
    for (int i = 0; i < 256; i++) {
        aLookup[i] = AlphaLevelLookup(ClipValue(i*2,0,511));
    }
BLITTERSIMPLE_LOOPBEGIN
    (*pDest)[::Blue] = AlphaFromLevel(aLookup[(*pSource)[::Blue]], (*pDest)[::Blue]);
    (*pDest)[::Green] = AlphaFromLevel(aLookup[(*pSource)[::Green]], (*pDest)[::Green]);
    (*pDest)[::Red] = AlphaFromLevel(aLookup[(*pSource)[::Red]], (*pDest)[::Red]);
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Lightmap_Opacity)
    , int Opacity) {
BLITTERSIMPLE_INIT
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return BlitSimple_Lightmap(Dest, Source, Rect, SX, SY);
    _BOS(BlitSimple_Lightmap_Opacity, 1) , Opacity _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aLookup[256], *aScale, *aSource, *aDest;
    for (int i = 0; i < 256; i++) {
        aLookup[i] = AlphaLevelLookup(ClipValue(i*2,0,511));
    }
    aSource = AlphaLevelLookup( ClipByte(Opacity) );
    aDest = AlphaLevelLookup( ClipByte(Opacity) ^ 0xFF );
BLITTERSIMPLE_LOOPBEGIN
    aScale = aLookup[(*pSource)[::Red]];
    (*pDest)[::Blue] = AlphaFromLevel2(aDest, (*pDest)[::Blue], aSource, AlphaFromLevel(aScale, (*pDest)[::Blue]));
    (*pDest)[::Green] = AlphaFromLevel2(aDest, (*pDest)[::Green], aSource, AlphaFromLevel(aScale, (*pDest)[::Green]));
    (*pDest)[::Red] = AlphaFromLevel2(aDest, (*pDest)[::Red], aSource, AlphaFromLevel(aScale, (*pDest)[::Red]));
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Lightmap_RGB_Opacity)
    , int Opacity) {
BLITTERSIMPLE_INIT
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return BlitSimple_Lightmap_RGB(Dest, Source, Rect, SX, SY);
    if (Source->OptimizeData.grayscaleOnly) return BlitSimple_Lightmap_Opacity(Dest, Source, Rect, SX, SY, Opacity);
    _BOS(BlitSimple_Lightmap_RGB_Opacity, 1) , Opacity _BOE
BLITTERSIMPLE_BEGIN
    AlphaLevel *aLookup[256], *aSource, *aDest;
    for (int i = 0; i < 256; i++) {
        aLookup[i] = AlphaLevelLookup(ClipValue(i*2,0,511));
    }
    aSource = AlphaLevelLookup( ClipByte(Opacity) );
    aDest = AlphaLevelLookup( ClipByte(Opacity) ^ 0xFF );
BLITTERSIMPLE_LOOPBEGIN
    (*pDest)[::Blue] = AlphaFromLevel2(aDest, (*pDest)[::Blue], aSource, AlphaFromLevel(aLookup[(*pSource)[::Blue]], (*pDest)[::Blue]));
    (*pDest)[::Green] = AlphaFromLevel2(aDest, (*pDest)[::Green], aSource, AlphaFromLevel(aLookup[(*pSource)[::Green]], (*pDest)[::Green]));
    (*pDest)[::Red] = AlphaFromLevel2(aDest, (*pDest)[::Red], aSource, AlphaFromLevel(aLookup[(*pSource)[::Red]], (*pDest)[::Red]));
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(AND)
    ) {
BLITTERSIMPLE_INIT
    _BOS(BlitSimple_AND, 0) _BOE
BLITTERSIMPLE_BEGIN
BLITTERSIMPLE_LOOPBEGIN
    (*pDest)[::Blue] &= (*pSource)[::Blue];
    (*pDest)[::Green] &= (*pSource)[::Green];
    (*pDest)[::Red] &= (*pSource)[::Red];
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(OR)
    ) {
BLITTERSIMPLE_INIT
    _BOS(BlitSimple_OR, 0) _BOE
BLITTERSIMPLE_BEGIN
BLITTERSIMPLE_LOOPBEGIN
    (*pDest)[::Blue] |= (*pSource)[::Blue];
    (*pDest)[::Green] |= (*pSource)[::Green];
    (*pDest)[::Red] |= (*pSource)[::Red];
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(XOR)
    ) {
BLITTERSIMPLE_INIT
    _BOS(BlitSimple_XOR, 0) _BOE
BLITTERSIMPLE_BEGIN
BLITTERSIMPLE_LOOPBEGIN
    (*pDest)[::Blue] ^= (*pSource)[::Blue];
    (*pDest)[::Green] ^= (*pSource)[::Green];
    (*pDest)[::Red] ^= (*pSource)[::Red];
BLITTERSIMPLE_LOOPEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Dither)
    ) {
BLITTERSIMPLE_INIT
    _BOS(BlitSimple_Dither, 0) _BOE
BLITTERSIMPLE_BEGIN
    bool b, r = false;
BLITTERSIMPLE_ROW
    b = r;
    r = !r;
BLITTERSIMPLE_COL
    if (b) *pDest = *pSource;
    b = !b;
BLITTERSIMPLE_COLEND
BLITTERSIMPLE_ROWEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Dither_Opacity)
    , int Opacity) {
BLITTERSIMPLE_INIT
    _BOS(BlitSimple_Dither_Opacity, 1) , Opacity _BOE
BLITTERSIMPLE_BEGIN
    bool b, r = false;
    AlphaLevel *aSource, *aDest;
    aSource = AlphaLevelLookup( ClipByte(Opacity) );
    aDest = AlphaLevelLookup( ClipByte(Opacity) ^ 0xFF );
BLITTERSIMPLE_ROW
    b = r;
    r = !r;
BLITTERSIMPLE_COL
    if (b) {
        BLENDPIXEL_ALPHA_OPACITY(pDest, pDest, pSource, aDest, aSource)
    }
    b = !b;
BLITTERSIMPLE_COLEND
BLITTERSIMPLE_ROWEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Dither_DualOpacity)
    , int Opacity, int Opacity2) {
BLITTERSIMPLE_INIT
    _BOS(BlitSimple_Dither_DualOpacity, 2) , Opacity, Opacity2 _BOE
BLITTERSIMPLE_BEGIN
    bool b, r = false;
    AlphaLevel *aSource, *aDest;
    aSource = AlphaLevelLookup( ClipByte(Opacity) );
    aDest = AlphaLevelLookup( ClipByte(Opacity) ^ 0xFF );
    AlphaLevel *aSource2, *aDest2;
    aSource2 = AlphaLevelLookup( ClipByte(Opacity2) );
    aDest2 = AlphaLevelLookup( ClipByte(Opacity2) ^ 0xFF );
BLITTERSIMPLE_ROW
    b = r;
    r = !r;
BLITTERSIMPLE_COL
    if (b) {
        BLENDPIXEL_ALPHA_OPACITY(pDest, pDest, pSource, aDest, aSource)
    } else {
        BLENDPIXEL_ALPHA_OPACITY(pDest, pDest, pSource, aDest2, aSource2)
    }
    b = !b;
BLITTERSIMPLE_COLEND
BLITTERSIMPLE_ROWEND
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Automatic_SourceAlpha_Opacity) 
  , int Opacity) {
BLITTERSIMPLE_INIT
  if (Source->OptimizeData.transparentOnly) {
    return Trivial_Success;
  } else if (Source->OptimizeData.opaqueOnly) {
    return BlitSimple_Normal_Opacity(Dest, Source, Rect, SX, SY, Opacity);
  } else if (Source->OptimizeData.maskOnly) {
    return BlitSimple_SourceAlphaMatte_Opacity(Dest, Source, Rect, SX, SY, Opacity);
  } else {
    return BlitSimple_SourceAlpha_Opacity(Dest, Source, Rect, SX, SY, Opacity);
  }
BLITTERSIMPLE_END

BLITTERSIMPLE_SIGNATURE(Automatic_Matte_Opacity) 
  , int Opacity) {
BLITTERSIMPLE_INIT
  if (Source->OptimizeData.noMask) {
    return BlitSimple_Normal_Opacity(Dest, Source, Rect, SX, SY, Opacity);
  } else {
    return BlitSimple_Matte_Opacity(Dest, Source, Rect, SX, SY, Opacity);
  }
BLITTERSIMPLE_END

TILEBLITTERSIMPLE_SIGNATURE(Normal)
    ) {
TILEBLITTERSIMPLE_BEGIN
    if (Source->OptimizeData.solidColor) {
      return FilterSimple_Fill(Dest, Rect, Source->getPixel(0, 0));
    }
TILEBLITTERSIMPLE_LOOPBEGIN
    BlitSimple_Normal(Dest, Source, &rDest, 0, 0);
TILEBLITTERSIMPLE_LOOPEND
TILEBLITTERSIMPLE_END

TILEBLITTERSIMPLE_SIGNATURE(Normal_Opacity)
    , int Opacity) {
TILEBLITTERSIMPLE_BEGIN
    if (Source->OptimizeData.solidColor) {
      return FilterSimple_Fill_Opacity(Dest, Rect, Source->getPixel(0, 0), Opacity);
    }
TILEBLITTERSIMPLE_LOOPBEGIN
    BlitSimple_Normal_Opacity(Dest, Source, &rDest, 0, 0, Opacity);
TILEBLITTERSIMPLE_LOOPEND
TILEBLITTERSIMPLE_END

TILEBLITTERSIMPLE_SIGNATURE(Matte)
    ) {
TILEBLITTERSIMPLE_BEGIN
    if (Source->OptimizeData.solidColor) {
      Pixel V = Source->getPixel(0, 0);
      if (V != Source->MatteColor) {
        return FilterSimple_Fill(Dest, Rect, V);
      } else {
        return Trivial_Success;
      }
    }
TILEBLITTERSIMPLE_LOOPBEGIN
    BlitSimple_Automatic_Matte_Opacity(Dest, Source, &rDest, 0, 0, 255);
TILEBLITTERSIMPLE_LOOPEND
TILEBLITTERSIMPLE_END

TILEBLITTERSIMPLE_SIGNATURE(Matte_Opacity)
    , int Opacity) {
TILEBLITTERSIMPLE_BEGIN
    if (Source->OptimizeData.solidColor) {
      Pixel V = Source->getPixel(0, 0);
      if (V != Source->MatteColor) {
        return FilterSimple_Fill_Opacity(Dest, Rect, V, Opacity);
      } else {
        return Trivial_Success;
      }
    }
TILEBLITTERSIMPLE_LOOPBEGIN
    BlitSimple_Automatic_Matte_Opacity(Dest, Source, &rDest, 0, 0, Opacity);
TILEBLITTERSIMPLE_LOOPEND
TILEBLITTERSIMPLE_END

TILEBLITTERSIMPLE_SIGNATURE(Matte_Tint)
    , Pixel Tint) {
TILEBLITTERSIMPLE_BEGIN
TILEBLITTERSIMPLE_LOOPBEGIN
    BlitSimple_Matte_Tint(Dest, Source, &rDest, 0, 0, Tint);
TILEBLITTERSIMPLE_LOOPEND
TILEBLITTERSIMPLE_END

TILEBLITTERSIMPLE_SIGNATURE(Matte_Tint_Opacity)
    , Pixel Tint, int Opacity) {
TILEBLITTERSIMPLE_BEGIN
TILEBLITTERSIMPLE_LOOPBEGIN
    BlitSimple_Matte_Tint_Opacity(Dest, Source, &rDest, 0, 0, Tint, Opacity);
TILEBLITTERSIMPLE_LOOPEND
TILEBLITTERSIMPLE_END

TILEBLITTERSIMPLE_SIGNATURE(SourceAlpha_Tint_Opacity)
    , Pixel Tint, int Opacity) {
TILEBLITTERSIMPLE_BEGIN
TILEBLITTERSIMPLE_LOOPBEGIN
    BlitSimple_SourceAlpha_Tint_Opacity(Dest, Source, &rDest, 0, 0, Tint, Opacity);
TILEBLITTERSIMPLE_LOOPEND
TILEBLITTERSIMPLE_END

TILEBLITTERSIMPLE_SIGNATURE(SourceAlpha_Tint)
    , Pixel Tint) {
TILEBLITTERSIMPLE_BEGIN
TILEBLITTERSIMPLE_LOOPBEGIN
    BlitSimple_SourceAlpha_Tint(Dest, Source, &rDest, 0, 0, Tint);
TILEBLITTERSIMPLE_LOOPEND
TILEBLITTERSIMPLE_END

TILEBLITTERSIMPLE_SIGNATURE(SourceAlpha_Opacity)
    , int Opacity) {
TILEBLITTERSIMPLE_BEGIN
    if (Source->OptimizeData.transparentOnly) return Trivial_Success;
    if (Source->OptimizeData.solidColor) {
      return FilterSimple_Fill_SourceAlpha_Opacity(Dest, Rect, Source->getPixel(0, 0), Opacity);
    }
TILEBLITTERSIMPLE_LOOPBEGIN
    BlitSimple_Automatic_SourceAlpha_Opacity(Dest, Source, &rDest, 0, 0, Opacity);
TILEBLITTERSIMPLE_LOOPEND
TILEBLITTERSIMPLE_END

TILEBLITTERSIMPLE_SIGNATURE(SourceAlpha)
    ) {
TILEBLITTERSIMPLE_BEGIN
    if (Source->OptimizeData.transparentOnly) return Trivial_Success;
    if (Source->OptimizeData.solidColor) {
      return FilterSimple_Fill_SourceAlpha(Dest, Rect, Source->getPixel(0, 0));
    }
TILEBLITTERSIMPLE_LOOPBEGIN
    BlitSimple_Automatic_SourceAlpha_Opacity(Dest, Source, &rDest, 0, 0, 255);
TILEBLITTERSIMPLE_LOOPEND
TILEBLITTERSIMPLE_END

TILEBLITTERSIMPLE_SIGNATURE(Multiply_Opacity)
    , int Opacity) {
TILEBLITTERSIMPLE_BEGIN
TILEBLITTERSIMPLE_LOOPBEGIN
    BlitSimple_Multiply_Opacity(Dest, Source, &rDest, 0, 0, Opacity);
TILEBLITTERSIMPLE_LOOPEND
TILEBLITTERSIMPLE_END

TILEBLITTERSIMPLE_SIGNATURE(Multiply)
    ) {
TILEBLITTERSIMPLE_BEGIN
TILEBLITTERSIMPLE_LOOPBEGIN
    BlitSimple_Multiply(Dest, Source, &rDest, 0, 0);
TILEBLITTERSIMPLE_LOOPEND
TILEBLITTERSIMPLE_END

TILEBLITTERSIMPLE_SIGNATURE(Merge)
    ) {
TILEBLITTERSIMPLE_BEGIN
    if (Source->OptimizeData.transparentOnly) return Trivial_Success;
TILEBLITTERSIMPLE_LOOPBEGIN
    BlitSimple_Merge(Dest, Source, &rDest, 0, 0);
TILEBLITTERSIMPLE_LOOPEND
TILEBLITTERSIMPLE_END

TILEBLITTERSIMPLE_SIGNATURE(Merge_Opacity)
    , int Opacity) {
TILEBLITTERSIMPLE_BEGIN
    if (Source->OptimizeData.transparentOnly) return Trivial_Success;
TILEBLITTERSIMPLE_LOOPBEGIN
    BlitSimple_Merge_Opacity(Dest, Source, &rDest, 0, 0, Opacity);
TILEBLITTERSIMPLE_LOOPEND
TILEBLITTERSIMPLE_END

TILEBLITTERSIMPLE_SIGNATURE(Additive)
    ) {
TILEBLITTERSIMPLE_BEGIN
TILEBLITTERSIMPLE_LOOPBEGIN
    BlitSimple_Additive(Dest, Source, &rDest, 0, 0);
TILEBLITTERSIMPLE_LOOPEND
TILEBLITTERSIMPLE_END

TILEBLITTERSIMPLE_SIGNATURE(Subtractive)
    ) {
TILEBLITTERSIMPLE_BEGIN
TILEBLITTERSIMPLE_LOOPBEGIN
    BlitSimple_Subtractive(Dest, Source, &rDest, 0, 0);
TILEBLITTERSIMPLE_LOOPEND
TILEBLITTERSIMPLE_END

TILEBLITTERSIMPLE_SIGNATURE(Additive_Opacity)
    , int Opacity) {
TILEBLITTERSIMPLE_BEGIN
TILEBLITTERSIMPLE_LOOPBEGIN
    BlitSimple_Additive_Opacity(Dest, Source, &rDest, 0, 0, Opacity);
TILEBLITTERSIMPLE_LOOPEND
TILEBLITTERSIMPLE_END

TILEBLITTERSIMPLE_SIGNATURE(Subtractive_Opacity)
    , int Opacity) {
TILEBLITTERSIMPLE_BEGIN
TILEBLITTERSIMPLE_LOOPBEGIN
    BlitSimple_Subtractive_Opacity(Dest, Source, &rDest, 0, 0, Opacity);
TILEBLITTERSIMPLE_LOOPEND
TILEBLITTERSIMPLE_END

TILEBLITTERSIMPLE_SIGNATURE(Font_Merge_Opacity)
    , Pixel Tint, int Opacity) {
TILEBLITTERSIMPLE_BEGIN
TILEBLITTERSIMPLE_LOOPBEGIN
    BlitSimple_Font_Merge_RGB_Opacity(Dest, Source, &rDest, 0, 0, Tint, Opacity);
TILEBLITTERSIMPLE_LOOPEND
TILEBLITTERSIMPLE_END

TILEBLITTERSIMPLE_SIGNATURE(Font_SourceAlpha_Opacity)
    , Pixel Tint, int Opacity) {
TILEBLITTERSIMPLE_BEGIN
TILEBLITTERSIMPLE_LOOPBEGIN
    BlitSimple_Font_SourceAlpha_Opacity(Dest, Source, &rDest, 0, 0, Tint, Opacity);
TILEBLITTERSIMPLE_LOOPEND
TILEBLITTERSIMPLE_END

BLITTERRESAMPLE_SIGNATURE(Normal)
    ) {
BLITTERRESAMPLE_INIT
    if (Source->OptimizeData.solidColor) {
      return FilterSimple_Fill(Dest, DestRect, Source->getPixel(0, 0));
    }
    _RBOS(BlitResample_Normal, 0) _RBOE
BLITTERRESAMPLE_BEGIN
BLITTERRESAMPLE_LOOPBEGIN
    *pDest = S;
BLITTERRESAMPLE_LOOPEND
BLITTERRESAMPLE_END

BLITTERRESAMPLE_SIGNATURE(Normal_Opacity)
    , int Opacity) {
BLITTERRESAMPLE_INIT
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return BlitResample_Normal(Dest, Source, DestRect, SourceRect, Scaler);
    if (Source->OptimizeData.solidColor) {
      return FilterSimple_Fill_Opacity(Dest, DestRect, Source->getPixel(0, 0), Opacity);
    }
    _RBOS(BlitResample_Normal_Opacity, 1) , Opacity _RBOE
BLITTERRESAMPLE_BEGIN
    AlphaLevel *aSource, *aDest;
    aSource = AlphaLevelLookup( ClipByte(Opacity) );
    aDest = AlphaLevelLookup( ClipByte(Opacity) ^ 0xFF );
BLITTERRESAMPLE_LOOPBEGIN
      BLENDPIXEL_ALPHA_OPACITY(pDest, pDest, pS, aDest, aSource)
BLITTERRESAMPLE_LOOPEND
BLITTERRESAMPLE_END

BLITTERRESAMPLE_SIGNATURE(Multiply)
    ) {
BLITTERRESAMPLE_INIT
    _RBOS(BlitResample_Multiply, 0) _RBOE
BLITTERRESAMPLE_BEGIN
BLITTERRESAMPLE_LOOPBEGIN
    if (pS->V != 0) {
      if (pS->V == 0xFFFFFFFF) {
      } else {
        (*pDest)[::Blue] = AlphaLookup((*pS)[::Blue], (*pDest)[::Blue]);
        (*pDest)[::Green] = AlphaLookup((*pS)[::Green], (*pDest)[::Green]);
        (*pDest)[::Red] = AlphaLookup((*pS)[::Red], (*pDest)[::Red]);
      }
    } else {
      pDest->V = 0;
    }
BLITTERRESAMPLE_LOOPEND
BLITTERRESAMPLE_END

BLITTERRESAMPLE_SIGNATURE(Multiply_Opacity)
    , int Opacity) {
BLITTERRESAMPLE_INIT
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return BlitResample_Multiply(Dest, Source, DestRect, SourceRect, Scaler);
    _RBOS(BlitResample_Multiply_Opacity, 1) , Opacity _RBOE
BLITTERRESAMPLE_BEGIN
    AlphaLevel *aSource, *aDest;
    aSource = AlphaLevelLookup( ClipByte(Opacity) );
    aDest = AlphaLevelLookup( ClipByte(Opacity) ^ 0xFF );
BLITTERRESAMPLE_LOOPBEGIN
    if (pS->V != 0) {
      if (pS->V == 0xFFFFFFFF) {
      } else {
        (*pDest)[::Blue] = AlphaFromLevel(aSource, AlphaLookup((*pS)[::Blue], (*pDest)[::Blue])) + AlphaFromLevel(aDest, (*pDest)[::Blue]);
        (*pDest)[::Green] = AlphaFromLevel(aSource, AlphaLookup((*pS)[::Green], (*pDest)[::Green])) + AlphaFromLevel(aDest, (*pDest)[::Green]);
        (*pDest)[::Red] = AlphaFromLevel(aSource, AlphaLookup((*pS)[::Red], (*pDest)[::Red])) + AlphaFromLevel(aDest, (*pDest)[::Red]);
      }
    } else {
      (*pDest)[::Blue] = AlphaFromLevel(aDest, (*pDest)[::Blue]);
      (*pDest)[::Green] = AlphaFromLevel(aDest, (*pDest)[::Green]);
      (*pDest)[::Red] = AlphaFromLevel(aDest, (*pDest)[::Red]);
    }
BLITTERRESAMPLE_LOOPEND
BLITTERRESAMPLE_END

BLITTERRESAMPLE_SIGNATURE(SourceAlpha)
    ) {
BLITTERRESAMPLE_INIT
    if (Source->OptimizeData.transparentOnly) return Trivial_Success;
    if (Source->OptimizeData.solidColor) {
      return FilterSimple_Fill_SourceAlpha(Dest, DestRect, Source->getPixel(0, 0));
    }
    _RBOS(BlitResample_SourceAlpha, 0) _RBOE
BLITTERRESAMPLE_BEGIN
    AlphaLevel *aSource, *aDest;
BLITTERRESAMPLE_LOOPBEGIN
    if (S[::Alpha]) {
      if (S[::Alpha] == 255) {
        *pDest = S;
      } else {
        aSource = AlphaLevelLookup( S[::Alpha] );
        aDest = AlphaLevelLookup( S[::Alpha] ^ 0xFF );
        (*pDest)[::Blue] = AlphaFromLevel2(aDest, (*pDest)[::Blue], aSource, S[::Blue]);
        (*pDest)[::Green] = AlphaFromLevel2(aDest, (*pDest)[::Green], aSource, S[::Green]);
        (*pDest)[::Red] = AlphaFromLevel2(aDest, (*pDest)[::Red], aSource, S[::Red]);
      }
    }
BLITTERRESAMPLE_LOOPEND
BLITTERRESAMPLE_END

BLITTERRESAMPLE_SIGNATURE(SourceAlpha_Opacity)
    , int Opacity) {
BLITTERRESAMPLE_INIT
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return BlitResample_SourceAlpha(Dest, Source, DestRect, SourceRect, Scaler);
    if (Source->OptimizeData.transparentOnly) return Trivial_Success;
    if (Source->OptimizeData.solidColor) {
      return FilterSimple_Fill_SourceAlpha_Opacity(Dest, DestRect, Source->getPixel(0, 0), Opacity);
    }
    _RBOS(BlitResample_SourceAlpha_Opacity, 1) , Opacity _RBOE
BLITTERRESAMPLE_BEGIN
    AlphaLevel *aSource, *aDest, *aScale, *aScale2;
    aScale = AlphaLevelLookup( ClipByte(Opacity) );
    aScale2 = AlphaLevelLookup( ClipByte(Opacity) ^ 0xFF );
BLITTERRESAMPLE_LOOPBEGIN
    if (S[::Alpha]) {
      if (S[::Alpha] == 255) {
        (*pDest)[::Blue] = AlphaFromLevel2(aScale2, (*pDest)[::Blue], aScale, S[::Blue]);
        (*pDest)[::Green] = AlphaFromLevel2(aScale2, (*pDest)[::Green], aScale, S[::Green]);
        (*pDest)[::Red] = AlphaFromLevel2(aScale2, (*pDest)[::Red], aScale, S[::Red]);
      } else {
        aSource = AlphaLevelLookup( AlphaFromLevel(aScale, S[::Alpha]) );
        aDest = AlphaLevelLookup( AlphaFromLevel(aScale, S[::Alpha]) ^ 0xFF );
        (*pDest)[::Blue] = AlphaFromLevel2(aDest, (*pDest)[::Blue], aSource, S[::Blue]);
        (*pDest)[::Green] = AlphaFromLevel2(aDest, (*pDest)[::Green], aSource, S[::Green]);
        (*pDest)[::Red] = AlphaFromLevel2(aDest, (*pDest)[::Red], aSource, S[::Red]);
      }
    }
BLITTERRESAMPLE_LOOPEND
BLITTERRESAMPLE_END

BLITTERRESAMPLE_SIGNATURE(Lightmap)
    ) {
BLITTERRESAMPLE_INIT
    _RBOS(BlitResample_Lightmap, 0) _RBOE
BLITTERRESAMPLE_BEGIN
    AlphaLevel *aLookup[256], *aScale;
    for (int i = 0; i < 256; i++) {
        aLookup[i] = AlphaLevelLookup(ClipValue(i*2,0,511));
    }
BLITTERRESAMPLE_LOOPBEGIN
    aScale = aLookup[S[::Red]];
    (*pDest)[::Blue] = AlphaFromLevel(aScale, (*pDest)[::Blue]);
    (*pDest)[::Green] = AlphaFromLevel(aScale, (*pDest)[::Green]);
    (*pDest)[::Red] = AlphaFromLevel(aScale, (*pDest)[::Red]);
BLITTERRESAMPLE_LOOPEND
BLITTERRESAMPLE_END

BLITTERRESAMPLE_SIGNATURE(Lightmap_RGB)
    ) {
BLITTERRESAMPLE_INIT
    if (Source->OptimizeData.grayscaleOnly) return BlitResample_Lightmap(Dest, Source, DestRect, SourceRect, Scaler);
    _RBOS(BlitResample_Lightmap_RGB, 0) _RBOE
BLITTERRESAMPLE_BEGIN
    AlphaLevel *aLookup[256];
    for (int i = 0; i < 256; i++) {
        aLookup[i] = AlphaLevelLookup(ClipValue(i*2,0,511));
    }
BLITTERRESAMPLE_LOOPBEGIN
    (*pDest)[::Blue] = AlphaFromLevel(aLookup[S[::Blue]], (*pDest)[::Blue]);
    (*pDest)[::Green] = AlphaFromLevel(aLookup[S[::Green]], (*pDest)[::Green]);
    (*pDest)[::Red] = AlphaFromLevel(aLookup[S[::Red]], (*pDest)[::Red]);
BLITTERRESAMPLE_LOOPEND
BLITTERRESAMPLE_END

BLITTERRESAMPLE_SIGNATURE(Merge)
    ) {
BLITTERRESAMPLE_INIT
    _RBOS(BlitResample_Merge, 0) _RBOE
BLITTERRESAMPLE_BEGIN
    AlphaLevel *aSource, *aDest;
BLITTERRESAMPLE_LOOPBEGIN
    if (S[::Alpha]) {
      if (S[::Alpha] == 255) {
        pDest->V = S.V;
      } else {
        aSource = AlphaLevelLookup( ClipByteHigh(S[::Alpha] + ( (*pDest)[::Alpha] ^ 0xFF )) );
        aDest = AlphaLevelLookup( ClipByteHigh((S[::Alpha] + ( (*pDest)[::Alpha] ^ 0xFF ))) ^ 0xFF );
        (*pDest)[::Blue] = AlphaFromLevel2(aDest, (*pDest)[::Blue], aSource, S[::Blue]);
        (*pDest)[::Green] = AlphaFromLevel2(aDest, (*pDest)[::Green], aSource, S[::Green]);
        (*pDest)[::Red] = AlphaFromLevel2(aDest, (*pDest)[::Red], aSource, S[::Red]);
        (*pDest)[::Alpha] = ClipByteHigh((*pDest)[::Alpha] + AlphaFromLevel(aSource, S[::Alpha]));
      }
    }
BLITTERRESAMPLE_LOOPEND
BLITTERRESAMPLE_END

BLITTERRESAMPLE_SIGNATURE(Merge_Opacity)
    , int Opacity) {
BLITTERRESAMPLE_INIT
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return BlitResample_Merge(Dest, Source, DestRect, SourceRect, Scaler);
    _RBOS(BlitResample_Merge_Opacity, 1) , Opacity _RBOE
BLITTERRESAMPLE_BEGIN
    AlphaLevel *aSource, *aDest, *aScale, *aScale2;
    aScale = AlphaLevelLookup( ClipByte(Opacity) );
    aScale2 = AlphaLevelLookup( ClipByte(Opacity) ^ 0xFF );
BLITTERRESAMPLE_LOOPBEGIN
    if (S[::Alpha]) {
      if (S[::Alpha] == 255) {
        (*pDest)[::Blue] = AlphaFromLevel2(aScale2, (*pDest)[::Blue], aScale, S[::Blue]);
        (*pDest)[::Green] = AlphaFromLevel2(aScale2, (*pDest)[::Green], aScale, S[::Green]);
        (*pDest)[::Red] = AlphaFromLevel2(aScale2, (*pDest)[::Red], aScale, S[::Red]);
        (*pDest)[::Alpha] = ClipByteHigh((*pDest)[::Alpha] + AlphaFromLevel(aScale, Opacity));
      } else {
        aSource = AlphaLevelLookup( AlphaFromLevel(aScale, ClipByteHigh(S[::Alpha] + ( (*pDest)[::Alpha] ^ 0xFF ))));
        aDest = AlphaLevelLookup( AlphaFromLevel(aScale, ClipByteHigh((S[::Alpha] + ( (*pDest)[::Alpha] ^ 0xFF )))) ^ 0xFF );
        (*pDest)[::Blue] = AlphaFromLevel2(aDest, (*pDest)[::Blue], aSource, S[::Blue]);
        (*pDest)[::Green] = AlphaFromLevel2(aDest, (*pDest)[::Green], aSource, S[::Green]);
        (*pDest)[::Red] = AlphaFromLevel2(aDest, (*pDest)[::Red], aSource, S[::Red]);
        (*pDest)[::Alpha] = ClipByteHigh((*pDest)[::Alpha] + AlphaFromLevel(aScale, AlphaFromLevel(aScale, S[::Alpha])));
      }
    }
BLITTERRESAMPLE_LOOPEND
BLITTERRESAMPLE_END

BLITTERRESAMPLE_SIGNATURE(Additive)
    ) {
BLITTERRESAMPLE_INIT
    _RBOS(BlitResample_Additive, 0) _RBOE
BLITTERRESAMPLE_BEGIN
BLITTERRESAMPLE_LOOPBEGIN
    if (S.V != 0) {
          BLENDPIXEL_ADDITIVE(pDest, pDest, pS)
    }
BLITTERRESAMPLE_LOOPEND
BLITTERRESAMPLE_END

BLITTERRESAMPLE_SIGNATURE(Additive_Opacity)
    , int Opacity) {
BLITTERRESAMPLE_INIT
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return BlitResample_Additive(Dest, Source, DestRect, SourceRect, Scaler);
    _RBOS(BlitResample_Additive_Opacity, 1) , Opacity _RBOE
BLITTERRESAMPLE_BEGIN
    AlphaLevel *aSource, *aDest;
    aSource = AlphaLevelLookup( ClipByte(Opacity) );
    aDest = AlphaLevelLookup( ClipByte(Opacity) ^ 0xFF );
BLITTERRESAMPLE_LOOPBEGIN
    if (S.V != 0) {
          BLENDPIXEL_ADDITIVE_OPACITY(pDest, pDest, pS, aSource)
    }
BLITTERRESAMPLE_LOOPEND
BLITTERRESAMPLE_END

BLITTERRESAMPLE_SIGNATURE(Subtractive)
    ) {
BLITTERRESAMPLE_INIT
    _RBOS(BlitResample_Subtractive, 0) _RBOE
BLITTERRESAMPLE_BEGIN
BLITTERRESAMPLE_LOOPBEGIN
    if (S.V != 0) {
          BLENDPIXEL_SUBTRACTIVE(pDest, pDest, pS)
    }
BLITTERRESAMPLE_LOOPEND
BLITTERRESAMPLE_END

BLITTERRESAMPLE_SIGNATURE(Subtractive_Opacity)
    , int Opacity) {
BLITTERRESAMPLE_INIT
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return BlitResample_Subtractive(Dest, Source, DestRect, SourceRect, Scaler);
    _RBOS(BlitResample_Subtractive_Opacity, 1) , Opacity _RBOE
BLITTERRESAMPLE_BEGIN
    AlphaLevel *aSource, *aDest;
    aSource = AlphaLevelLookup( ClipByte(Opacity) );
    aDest = AlphaLevelLookup( ClipByte(Opacity) ^ 0xFF );
BLITTERRESAMPLE_LOOPBEGIN
    if (S.V != 0) {
          BLENDPIXEL_SUBTRACTIVE_OPACITY(pDest, pDest, pS, aSource)
    }
BLITTERRESAMPLE_LOOPEND
BLITTERRESAMPLE_END

BLITTERRESAMPLE_SIGNATURE(SourceAlpha_Tint)
    , Pixel Tint) {
BLITTERRESAMPLE_INIT
    if (Tint[::Alpha] = 0) return BlitResample_SourceAlpha(Dest, Source, DestRect, SourceRect, Scaler);
    _RBOS(BlitResample_SourceAlpha_Tint, 1) , Tint _RBOE
BLITTERRESAMPLE_BEGIN
    AlphaLevel *aSource, *aDest, *aTint;
    int tR, tG, tB;
    aSource = AlphaLevelLookup(Tint[::Alpha]);
    aTint = AlphaLevelLookup(Tint[::Alpha] ^ 0xFF);
    tB = AlphaFromLevel(aSource, Tint[::Blue]);
    tG = AlphaFromLevel(aSource, Tint[::Green]);
    tR = AlphaFromLevel(aSource, Tint[::Red]);
BLITTERRESAMPLE_LOOPBEGIN
    if (S[::Alpha]) {
        aSource = AlphaLevelLookup( S[::Alpha] );
        aDest = AlphaLevelLookup( S[::Alpha] ^ 0xFF );
        (*pDest)[::Blue] = AlphaFromLevel2(aDest, (*pDest)[::Blue], aSource, AlphaFromLevel(aTint, S[::Blue]) + tB);
        (*pDest)[::Green] = AlphaFromLevel2(aDest, (*pDest)[::Green], aSource, AlphaFromLevel(aTint, S[::Green]) + tG);
        (*pDest)[::Red] = AlphaFromLevel2(aDest, (*pDest)[::Red], aSource, AlphaFromLevel(aTint, S[::Red]) + tR);
    }
BLITTERRESAMPLE_LOOPEND
BLITTERRESAMPLE_END

BLITTERRESAMPLE_SIGNATURE(SourceAlpha_Tint_Opacity)
    , Pixel Tint, int Opacity) {
BLITTERRESAMPLE_INIT
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return BlitResample_SourceAlpha_Tint(Dest, Source, DestRect, SourceRect, Scaler, Tint);
    _RBOS(BlitResample_SourceAlpha_Tint_Opacity, 2) , Tint, Opacity _RBOE
BLITTERRESAMPLE_BEGIN
    AlphaLevel *aSource, *aDest, *aScale, *aTint;
    int tR, tG, tB;
    aSource = AlphaLevelLookup(Tint[::Alpha]);
    aTint = AlphaLevelLookup(Tint[::Alpha] ^ 0xFF);
    tB = AlphaFromLevel(aSource, Tint[::Blue]);
    tG = AlphaFromLevel(aSource, Tint[::Green]);
    tR = AlphaFromLevel(aSource, Tint[::Red]);
    aScale = AlphaLevelLookup( ClipByte(Opacity) );
BLITTERRESAMPLE_LOOPBEGIN
    if (S[::Alpha]) {
        aSource = AlphaLevelLookup( AlphaFromLevel(aScale, S[::Alpha]) );
        aDest = AlphaLevelLookup( AlphaFromLevel(aScale, S[::Alpha]) ^ 0xFF );
        (*pDest)[::Blue] = AlphaFromLevel2(aDest, (*pDest)[::Blue], aSource, AlphaFromLevel(aTint, S[::Blue]) + tB);
        (*pDest)[::Green] = AlphaFromLevel2(aDest, (*pDest)[::Green], aSource, AlphaFromLevel(aTint, S[::Green]) + tG);
        (*pDest)[::Red] = AlphaFromLevel2(aDest, (*pDest)[::Red], aSource, AlphaFromLevel(aTint, S[::Red]) + tR);
    }
BLITTERRESAMPLE_LOOPEND
BLITTERRESAMPLE_END

Export int BlitMask_Merge_Opacity(Image *Dest, Image *Source, Image *Mask,
                          Rectangle *Rect, Coordinate SX, Coordinate SY, Coordinate MX, Coordinate MY, int Opacity)
{
    if (!Dest || !Source || !Mask) {
        return Failure;
    }
    if (!Dest->initialized()) {
        return Failure;
    }
    if (!Source->initialized()) {
        return Failure;
    }
    if (!Mask->initialized()) {
        return Failure;
    }

    if (Opacity == 0) {
      return Trivial_Success;
    }

    {
        int overrideresult = Override::EnumOverrides(Override::BlitMask_Merge_Opacity, 9, Dest, Source, Mask, Rect, SX, SY, MX, MY, Opacity);
#ifdef OVERRIDES
        if (overrideresult != 0) return overrideresult;
#endif
    }

    if (!Dest->Unlocked) {
        return Failure;
    }
    if (!Source->Unlocked) {
        return Failure;
    }
    if (!Mask->Unlocked) {
        return Failure;
    }

    int DSX = SX, DSY = SY, MSX = MX, MSY = MY;

    Rectangle rCoordinates;
    if (!Clip2D_SimpleRect(&rCoordinates, Dest, Source,
        Rect, DSX, DSY)) {
          return Trivial_Success;
      }

    rCoordinates.Width = ClipValue(rCoordinates.Width, 0, Mask->Width - MX);
    rCoordinates.Height = ClipValue(rCoordinates.Height, 0, Mask->Height - MY);

    Pixel *pSource = Source->pointer(DSX, DSY),
        *pDest = Dest->pointer(
        rCoordinates.Left, rCoordinates.Top),
        *pMask = Mask->pointer(MSX, MSY);
    if ((!pSource) || (!pDest) || (!pMask)) {
      return Failure;
    }
    
    Dest->dirty();
    
    DoubleWord iCX = 0 , iCY = rCoordinates.Height;
    
    DoubleWord iMaskRowOffset =
        (Mask->Width - rCoordinates.Width) + Mask->Pitch;
    DoubleWord iSourceRowOffset =
        (Source->Width - rCoordinates.Width) + Source->Pitch;
    DoubleWord iDestRowOffset =
        (Dest->Width - rCoordinates.Width) + Dest->Pitch;
    
    AlphaLevel *aSource, *aDest, *aScale, *aScale2, *aMask, *aMask2;
    aScale = AlphaLevelLookup( ClipByte(Opacity) );
    aScale2 = AlphaLevelLookup( ClipByte(Opacity) ^ 0xFF );

    while (iCY--) {
        iCX = (DoubleWord)rCoordinates.Width;
        while (iCX--) {
            if ((*pMask)[::Red]) {
                if ((*pSource)[::Alpha]) {
                    if ((*pMask)[::Red] == 255) {
                        if ((*pSource)[::Alpha] == 255) {
                            (*pDest)[::Blue] = AlphaFromLevel2(aScale2, (*pDest)[::Blue], aScale, (*pSource)[::Blue]);
                            (*pDest)[::Green] = AlphaFromLevel2(aScale2, (*pDest)[::Green], aScale, (*pSource)[::Green]);
                            (*pDest)[::Red] = AlphaFromLevel2(aScale2, (*pDest)[::Red], aScale, (*pSource)[::Red]);
                            (*pDest)[::Alpha] = ClipByteHigh((*pDest)[::Alpha] + AlphaFromLevel(aScale, Opacity));
                            // (*pDest)[::Alpha] = ClipByteHigh((*pDest)[::Alpha] + Opacity);
                        } else {
                            aSource = AlphaLevelLookup( AlphaFromLevel(aScale, (*pSource)[::Alpha]) );
                            aDest = AlphaLevelLookup( AlphaFromLevel(aScale, (*pSource)[::Alpha]) ^ 0xFF );
                            (*pDest)[::Blue] = AlphaFromLevel2(aDest, (*pDest)[::Blue], aSource, (*pSource)[::Blue]);
                            (*pDest)[::Green] = AlphaFromLevel2(aDest, (*pDest)[::Green], aSource, (*pSource)[::Green]);
                            (*pDest)[::Red] = AlphaFromLevel2(aDest, (*pDest)[::Red], aSource, (*pSource)[::Red]);
                            (*pDest)[::Alpha] = ClipByteHigh((*pDest)[::Alpha] + AlphaFromLevel(aScale, AlphaFromLevel(aScale, (*pSource)[::Alpha])));
                            // (*pDest)[::Alpha] = ClipByteHigh((*pDest)[::Alpha] + AlphaFromLevel(aScale, (*pSource)[::Alpha]));
                        }
                    } else {
                        aMask = AlphaLevelLookup(AlphaFromLevel(aScale, (*pMask)[::Red]));
                        if ((*pSource)[::Alpha] == 255) {
                            aMask2 = AlphaLevelLookup(AlphaFromLevel(aScale, (*pMask)[::Red]) ^ 0xFF);
                            (*pDest)[::Blue] = AlphaFromLevel2(aMask2, (*pDest)[::Blue], aMask, (*pSource)[::Blue]);
                            (*pDest)[::Green] = AlphaFromLevel2(aMask2, (*pDest)[::Green], aMask, (*pSource)[::Green]);
                            (*pDest)[::Red] = AlphaFromLevel2(aMask2, (*pDest)[::Red], aMask, (*pSource)[::Red]);
                            (*pDest)[::Alpha] = ClipByteHigh((*pDest)[::Alpha] + AlphaFromLevel(aScale, Opacity));
                            // (*pDest)[::Alpha] = ClipByteHigh((*pDest)[::Alpha] + Opacity);
                        } else {
                            aSource = AlphaLevelLookup( AlphaFromLevel(aMask, (*pSource)[::Alpha]) );
                            aDest = AlphaLevelLookup( AlphaFromLevel(aMask, (*pSource)[::Alpha]) ^ 0xFF );
                            (*pDest)[::Blue] = AlphaFromLevel2(aDest, (*pDest)[::Blue], aSource, (*pSource)[::Blue]);
                            (*pDest)[::Green] = AlphaFromLevel2(aDest, (*pDest)[::Green], aSource, (*pSource)[::Green]);
                            (*pDest)[::Red] = AlphaFromLevel2(aDest, (*pDest)[::Red], aSource, (*pSource)[::Red]);
                            (*pDest)[::Alpha] = ClipByteHigh((*pDest)[::Alpha] + AlphaFromLevel(aScale, AlphaFromLevel(aScale, (*pSource)[::Alpha])));
                            // (*pDest)[::Alpha] = ClipByteHigh((*pDest)[::Alpha] + AlphaFromLevel(aScale, (*pSource)[::Alpha]));
                        }
                    }
                }
            }
            pMask++;
            pSource++;
            pDest++;
        }
        pMask += iMaskRowOffset;
        pSource += iSourceRowOffset;
        pDest += iDestRowOffset;
    }

    return Success;
}

Export int BlitConvolve_Normal(Image *Dest, Image *Source, Convolution::Filter *Filter,
                          Rectangle *Rect, Coordinate SX, Coordinate SY)
{
    if (!Dest || !Source || !Filter) {
      return Failure;
    }
    if (!Dest->initialized()) {
      return Failure;
    }
    if (!Source->initialized()) {
      return Failure;
    }
    if (Filter->Divisor < 1) return Failure; // Can't have a divisor lower than 1
    if ((Filter->Width < 3) || (Filter->Height < 3)) return Failure; // Can't have a convolution filter smaller than 3x3
    if (((Filter->Width % 2) == 0) || ((Filter->Height % 2) == 0)) return Failure; // Can't have an even numbered size

    int DSX = SX, DSY = SY;

    Rectangle rCoordinates;
    if (!Clip2D_SimpleRect(&rCoordinates, Dest, Source,
         Rect, DSX, DSY)) return Trivial_Success;

    Pixel *pDest = Dest->pointer(
        rCoordinates.Left, rCoordinates.Top);
    Pixel pSource;
    if ((!pDest)) return Failure;
    
short **cellTables;
DoubleWord cells = 0;
signed char *cxo, *cyo;

    for (int y = 0; y < Filter->Height; y++) {
      for (int x = 0; x < Filter->Width; x++) {
        if (Filter->Weights[(y * Filter->Width) + x]) {
          cells++;
        }
      }
    }

//    cxo = AllocateArray(signed char, cells);
//    cyo = AllocateArray(signed char, cells);
//    cellTables = AllocateArray(short*, cells);

    cxo = LookupAllocate<signed char>(cells);
    cyo = LookupAllocate<signed char>(cells);
    cellTables = LookupAllocate<short*>(cells);

    {
      float w;
      int i = 0;
      for (int y = 0; y < Filter->Height; y++) {
        for (int x = 0; x < Filter->Width; x++) {
          w = Filter->Weights[(y * Filter->Width) + x] / Filter->Divisor;
          if (w) {
            cxo[i] = x + Filter->XOffset;
            cyo[i] = y + Filter->YOffset;
            // cellTables[i] = AllocateArray(short, 256);
            cellTables[i] = LookupAllocate<short>(256);
            for (int v = 0; v < 256; v++) {
              cellTables[i][v] = floor((v * w));
            }
            i++;
          }
        }
      }
    }

    Dest->dirty();

    SX = DSX; SY = DSY;
    
    DoubleWord iCX = 0 , iCY = rCoordinates.Height;
    
    DoubleWord iDestRowOffset =
        (Dest->Width - rCoordinates.Width) + Dest->Pitch;
    
    short b = 0, g = 0, r = 0, a = 0;

    while (iCY--) {
      iCX = (DoubleWord)rCoordinates.Width;
      DSX = SX;
      while (iCX--) {
        b = g = r = a = 0;
        for (DoubleWord i = 0; i < cells; i++) {
            pSource = Source->getPixelClip(DSX + cxo[i], DSY + cyo[i]);
            b += cellTables[i][pSource[::Blue]];
            g += cellTables[i][pSource[::Green]];
            r += cellTables[i][pSource[::Red]];
            a += cellTables[i][pSource[::Alpha]];
        }
        (*pDest)[::Blue] = ClipByte(b);
        (*pDest)[::Green] = ClipByte(g);
        (*pDest)[::Red] = ClipByte(r);
        (*pDest)[::Alpha] = ClipByte(a);
        pDest++;
        DSX++;
      }
      pDest += iDestRowOffset;
      DSY++;
    }

    for (DoubleWord i = 0; i < cells; i++) {
//      DeleteArray(cellTables[i]);
      LookupDeallocate(cellTables[i]);
    }
//    DeleteArray(cellTables);
//    DeleteArray(cxo);
//    DeleteArray(cyo);
    LookupDeallocate(cellTables);
    LookupDeallocate(cxo);
    LookupDeallocate(cyo);

    return Success;
}

Export int BlitConvolve_Additive(Image *Dest, Image *Source, Convolution::Filter *Filter,
                          Rectangle *Rect, Coordinate SX, Coordinate SY)
{
    if (!Dest || !Source || !Filter) {
      return Failure;
    }
    if (!Dest->initialized()) {
      return Failure;
    }
    if (!Source->initialized()) {
      return Failure;
    }
    if (Filter->Divisor < 1) return Failure; // Can't have a divisor lower than 1
    if ((Filter->Width < 3) || (Filter->Height < 3)) return Failure; // Can't have a convolution filter smaller than 3x3
    if (((Filter->Width % 2) == 0) || ((Filter->Height % 2) == 0)) return Failure; // Can't have an even numbered size

    int DSX = SX, DSY = SY;

    Rectangle rCoordinates;
    if (!Clip2D_SimpleRect(&rCoordinates, Dest, Source,
         Rect, DSX, DSY)) return Trivial_Success;

    Pixel *pDest = Dest->pointer(
        rCoordinates.Left, rCoordinates.Top);
    Pixel pSource;
    if ((!pDest)) return Failure;
    
short **cellTables;
DoubleWord cells = 0;
signed char *cxo, *cyo;

    for (int y = 0; y < Filter->Height; y++) {
      for (int x = 0; x < Filter->Width; x++) {
        if (Filter->Weights[(y * Filter->Width) + x]) {
          cells++;
        }
      }
    }

//    cxo = AllocateArray(signed char, cells);
//    cyo = AllocateArray(signed char, cells);
//    cellTables = AllocateArray(short*, cells);

    cxo = LookupAllocate<signed char>(cells);
    cyo = LookupAllocate<signed char>(cells);
    cellTables = LookupAllocate<short*>(cells);

    {
      float w;
      int i = 0;
      for (int y = 0; y < Filter->Height; y++) {
        for (int x = 0; x < Filter->Width; x++) {
          w = Filter->Weights[(y * Filter->Width) + x] / Filter->Divisor;
          if (w) {
            cxo[i] = x + Filter->XOffset;
            cyo[i] = y + Filter->YOffset;
            // cellTables[i] = AllocateArray(short, 256);
            cellTables[i] = LookupAllocate<short>(256);
            for (int v = 0; v < 256; v++) {
              cellTables[i][v] = floor((v * w));
            }
            i++;
          }
        }
      }
    }

    Dest->dirty();

    SX = DSX; SY = DSY;
    
    DoubleWord iCX = 0 , iCY = rCoordinates.Height;
    
    DoubleWord iDestRowOffset =
        (Dest->Width - rCoordinates.Width) + Dest->Pitch;
    
    short b = 0, g = 0, r = 0;

    while (iCY--) {
      iCX = (DoubleWord)rCoordinates.Width;
      DSX = SX;
      while (iCX--) {
        b = g = r = 0;
        for (DoubleWord i = 0; i < cells; i++) {
            pSource = Source->getPixelClip(DSX + cxo[i], DSY + cyo[i]);
            b += cellTables[i][pSource[::Blue]];
            g += cellTables[i][pSource[::Green]];
            r += cellTables[i][pSource[::Red]];
        }
        (*pDest)[::Blue] = ClipByteHigh((*pDest)[::Blue] + ClipByte(b));
        (*pDest)[::Green] = ClipByteHigh((*pDest)[::Green] + ClipByte(g));
        (*pDest)[::Red] = ClipByteHigh((*pDest)[::Red] + ClipByte(r));
        pDest++;
        DSX++;
      }
      pDest += iDestRowOffset;
      DSY++;
    }

    for (DoubleWord i = 0; i < cells; i++) {
//      DeleteArray(cellTables[i]);
      LookupDeallocate(cellTables[i]);
    }
//    DeleteArray(cellTables);
//    DeleteArray(cxo);
//    DeleteArray(cyo);
    LookupDeallocate(cellTables);
    LookupDeallocate(cxo);
    LookupDeallocate(cyo);

    return Success;
}

Export int BlitConvolve_Subtractive(Image *Dest, Image *Source, Convolution::Filter *Filter,
                          Rectangle *Rect, Coordinate SX, Coordinate SY)
{
    if (!Dest || !Source || !Filter) {
      return Failure;
    }
    if (!Dest->initialized()) {
      return Failure;
    }
    if (!Source->initialized()) {
      return Failure;
    }
    if (Filter->Divisor < 1) return Failure; // Can't have a divisor lower than 1
    if ((Filter->Width < 3) || (Filter->Height < 3)) return Failure; // Can't have a convolution filter smaller than 3x3
    if (((Filter->Width % 2) == 0) || ((Filter->Height % 2) == 0)) return Failure; // Can't have an even numbered size

    int DSX = SX, DSY = SY;

    Rectangle rCoordinates;
    if (!Clip2D_SimpleRect(&rCoordinates, Dest, Source,
         Rect, DSX, DSY)) return Trivial_Success;

    Pixel *pDest = Dest->pointer(
        rCoordinates.Left, rCoordinates.Top);
    Pixel pSource;
    if ((!pDest)) return Failure;
    
short **cellTables;
DoubleWord cells = 0;
signed char *cxo, *cyo;

    for (int y = 0; y < Filter->Height; y++) {
      for (int x = 0; x < Filter->Width; x++) {
        if (Filter->Weights[(y * Filter->Width) + x]) {
          cells++;
        }
      }
    }

//    cxo = AllocateArray(signed char, cells);
//    cyo = AllocateArray(signed char, cells);
//    cellTables = AllocateArray(short*, cells);

    cxo = LookupAllocate<signed char>(cells);
    cyo = LookupAllocate<signed char>(cells);
    cellTables = LookupAllocate<short*>(cells);

    {
      float w;
      int i = 0;
      for (int y = 0; y < Filter->Height; y++) {
        for (int x = 0; x < Filter->Width; x++) {
          w = Filter->Weights[(y * Filter->Width) + x] / Filter->Divisor;
          if (w) {
            cxo[i] = x + Filter->XOffset;
            cyo[i] = y + Filter->YOffset;
            // cellTables[i] = AllocateArray(short, 256);
            cellTables[i] = LookupAllocate<short>(256);
            for (int v = 0; v < 256; v++) {
              cellTables[i][v] = floor((v * w));
            }
            i++;
          }
        }
      }
    }

    Dest->dirty();

    SX = DSX; SY = DSY;
    
    DoubleWord iCX = 0 , iCY = rCoordinates.Height;
    
    DoubleWord iDestRowOffset =
        (Dest->Width - rCoordinates.Width) + Dest->Pitch;
    
    short b = 0, g = 0, r = 0;

    while (iCY--) {
      iCX = (DoubleWord)rCoordinates.Width;
      DSX = SX;
      while (iCX--) {
        b = g = r = 0;
        for (DoubleWord i = 0; i < cells; i++) {
          if (cellTables[i][255]) {
            pSource = Source->getPixelClip(DSX + cxo[i], DSY + cyo[i]);
            b += cellTables[i][pSource[::Blue]];
            g += cellTables[i][pSource[::Green]];
            r += cellTables[i][pSource[::Red]];
          }
        }
        (*pDest)[::Blue] = ClipByteLow((*pDest)[::Blue] + ClipByte(b));
        (*pDest)[::Green] = ClipByteLow((*pDest)[::Green] + ClipByte(g));
        (*pDest)[::Red] = ClipByteLow((*pDest)[::Red] + ClipByte(r));
        pDest++;
        DSX++;
      }
      pDest += iDestRowOffset;
      DSY++;
    }

    for (DoubleWord i = 0; i < cells; i++) {
//      DeleteArray(cellTables[i]);
      LookupDeallocate(cellTables[i]);
    }
//    DeleteArray(cellTables);
//    DeleteArray(cxo);
//    DeleteArray(cyo);
    LookupDeallocate(cellTables);
    LookupDeallocate(cxo);
    LookupDeallocate(cyo);

    return Success;
}


Export int BlitConvolve_Shadow(Image *Dest, Image *Source, Convolution::Filter *Filter,
                          Rectangle *Rect, Coordinate SX, Coordinate SY, Pixel ShadowColor)
{
    if (!Dest || !Source || !Filter) {
      return Failure;
    }
    if (!Dest->initialized()) {
      return Failure;
    }
    if (!Source->initialized()) {
      return Failure;
    }
    if (Source->OptimizeData.transparentOnly) return Trivial_Success;
    if (Filter->Divisor < 1) return Failure; // Can't have a divisor lower than 1
    if ((Filter->Width < 3) || (Filter->Height < 3)) return Failure; // Can't have a convolution filter smaller than 3x3
    if (((Filter->Width % 2) == 0) || ((Filter->Height % 2) == 0)) return Failure; // Can't have an even numbered size

    int DSX = SX, DSY = SY;

    Rectangle rCoordinates;
    if (!Clip2D_SimpleRect(&rCoordinates, Dest, Source,
         Rect, DSX, DSY)) return Trivial_Success;

    Pixel *pDest = Dest->pointer(
        rCoordinates.Left, rCoordinates.Top);
    Pixel pSource;
    if ((!pDest)) return Failure;
    
short **cellTables;
DoubleWord cells = 0;
signed char *cxo, *cyo;

    for (int y = 0; y < Filter->Height; y++) {
      for (int x = 0; x < Filter->Width; x++) {
        if (Filter->Weights[(y * Filter->Width) + x]) {
          cells++;
        }
      }
    }

//    cxo = AllocateArray(signed char, cells);
//    cyo = AllocateArray(signed char, cells);
//    cellTables = AllocateArray(short*, cells);

    cxo = LookupAllocate<signed char>(cells);
    cyo = LookupAllocate<signed char>(cells);
    cellTables = LookupAllocate<short*>(cells);

    {
      float w;
      int i = 0;
      for (int y = 0; y < Filter->Height; y++) {
        for (int x = 0; x < Filter->Width; x++) {
          w = Filter->Weights[(y * Filter->Width) + x] / Filter->Divisor;
          if (w) {
            cxo[i] = x + Filter->XOffset;
            cyo[i] = y + Filter->YOffset;
            // cellTables[i] = AllocateArray(short, 256);
            cellTables[i] = LookupAllocate<short>(256);
            for (int v = 0; v < 256; v++) {
              cellTables[i][v] = floor((v * w));
            }
            i++;
          }
        }
      }
    }

    Dest->dirty();

    SX = DSX; SY = DSY;

    Byte R = ShadowColor[::Red], G = ShadowColor[::Green], B = ShadowColor[::Blue];
    
    DoubleWord iCX = 0 , iCY = rCoordinates.Height;
    
    DoubleWord iDestRowOffset =
        (Dest->Width - rCoordinates.Width) + Dest->Pitch;
    
    short a = 0;
    AlphaLevel *aScale, *aSource, *aDest;
    aScale = AlphaLevelLookup( ShadowColor[::Alpha] );

    while (iCY--) {
      iCX = (DoubleWord)rCoordinates.Width;
      DSX = SX;
      while (iCX--) {
        a = 0;
        for (DoubleWord i = 0; i < cells; i++) {
          if (cellTables[i][255]) {
            pSource = Source->getPixelClip(DSX + cxo[i], DSY + cyo[i]);
            a += cellTables[i][pSource[::Alpha]];
          }
        }
        a = AlphaFromLevel(aScale, ClipByte(a));
        aDest = AlphaLevelLookup( a ^ 0xFF );
        aSource = AlphaLevelLookup( a );
        (*pDest)[::Blue] = AlphaFromLevel2(aDest, (*pDest)[::Blue], aSource, B);
        (*pDest)[::Green] = AlphaFromLevel2(aDest, (*pDest)[::Green], aSource, G);
        (*pDest)[::Red] = AlphaFromLevel2(aDest, (*pDest)[::Red], aSource, R);
        pDest++;
        DSX++;
      }
      pDest += iDestRowOffset;
      DSY++;
    }

    for (DoubleWord i = 0; i < cells; i++) {
//      DeleteArray(cellTables[i]);
      LookupDeallocate(cellTables[i]);
    }
//    DeleteArray(cellTables);
//    DeleteArray(cxo);
//    DeleteArray(cyo);
    LookupDeallocate(cellTables);
    LookupDeallocate(cxo);
    LookupDeallocate(cyo);

    return Success;
}

Export int BlitConvolve_SourceAlpha(Image *Dest, Image *Source, Convolution::Filter *Filter,
                          Rectangle *Rect, Coordinate SX, Coordinate SY)
{
    if (!Dest || !Source || !Filter) {
      return Failure;
    }
    if (!Dest->initialized()) {
      return Failure;
    }
    if (!Source->initialized()) {
      return Failure;
    }
    if (Filter->Divisor < 1) return Failure; // Can't have a divisor lower than 1
    if ((Filter->Width < 3) || (Filter->Height < 3)) return Failure; // Can't have a convolution filter smaller than 3x3
    if (((Filter->Width % 2) == 0) || ((Filter->Height % 2) == 0)) return Failure; // Can't have an even numbered size

    int DSX = SX, DSY = SY;

    Rectangle rCoordinates;
    if (!Clip2D_SimpleRect(&rCoordinates, Dest, Source,
         Rect, DSX, DSY)) return Trivial_Success;

    Pixel *pDest = Dest->pointer(
        rCoordinates.Left, rCoordinates.Top);
    Pixel pSource;
    if ((!pDest)) return Failure;
    
short **cellTables;
DoubleWord cells = 0;
signed char *cxo, *cyo;

    for (int y = 0; y < Filter->Height; y++) {
      for (int x = 0; x < Filter->Width; x++) {
        if (Filter->Weights[(y * Filter->Width) + x]) {
          cells++;
        }
      }
    }

//    cxo = AllocateArray(signed char, cells);
//    cyo = AllocateArray(signed char, cells);
//    cellTables = AllocateArray(short*, cells);

    cxo = LookupAllocate<signed char>(cells);
    cyo = LookupAllocate<signed char>(cells);
    cellTables = LookupAllocate<short*>(cells);

    {
      float w;
      int i = 0;
      for (int y = 0; y < Filter->Height; y++) {
        for (int x = 0; x < Filter->Width; x++) {
          w = Filter->Weights[(y * Filter->Width) + x] / Filter->Divisor;
          if (w) {
            cxo[i] = x + Filter->XOffset;
            cyo[i] = y + Filter->YOffset;
            // cellTables[i] = AllocateArray(short, 256);
            cellTables[i] = LookupAllocate<short>(256);
            for (int v = 0; v < 256; v++) {
              cellTables[i][v] = floor((v * w));
            }
            i++;
          }
        }
      }
    }

    Dest->dirty();

    SX = DSX; SY = DSY;
    
    DoubleWord iCX = 0 , iCY = rCoordinates.Height;
    
    DoubleWord iDestRowOffset =
        (Dest->Width - rCoordinates.Width) + Dest->Pitch;
    
    short b = 0, g = 0, r = 0, a = 0;
    AlphaLevel *aSource, *aDest;

    while (iCY--) {
      iCX = (DoubleWord)rCoordinates.Width;
      DSX = SX;
      while (iCX--) {
        b = g = r = a = 0;
        for (DoubleWord i = 0; i < cells; i++) {
            pSource = Source->getPixelClip(DSX + cxo[i], DSY + cyo[i]);
            b += cellTables[i][pSource[::Blue]];
            g += cellTables[i][pSource[::Green]];
            r += cellTables[i][pSource[::Red]];
            a += cellTables[i][pSource[::Alpha]];
        }
        a = ClipByte(a);
        if (a == 255) {
          (*pDest)[::Blue] = ClipByte(b);
          (*pDest)[::Green] = ClipByte(g);
          (*pDest)[::Red] = ClipByte(r);
        } else if (a > 0) {
          aSource = AlphaLevelLookup(a);
          aDest = AlphaLevelLookup(a ^ 0xFF);
          (*pDest)[::Blue] = AlphaFromLevel2(aDest, (*pDest)[::Blue], aSource, ClipByte(b));
          (*pDest)[::Green] = AlphaFromLevel2(aDest, (*pDest)[::Green], aSource, ClipByte(g));
          (*pDest)[::Red] = AlphaFromLevel2(aDest, (*pDest)[::Red], aSource, ClipByte(r));
        }
        pDest++;
        DSX++;
      }
      pDest += iDestRowOffset;
      DSY++;
    }

    for (DoubleWord i = 0; i < cells; i++) {
//      DeleteArray(cellTables[i]);
      LookupDeallocate(cellTables[i]);
    }
//    DeleteArray(cellTables);
//    DeleteArray(cxo);
//    DeleteArray(cyo);
    LookupDeallocate(cellTables);
    LookupDeallocate(cxo);
    LookupDeallocate(cyo);

    return Success;
}
