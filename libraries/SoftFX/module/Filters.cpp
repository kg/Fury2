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
#include "../header/Clip.hpp"
#include "../header/2D Filter.hpp"
#include "../header/MersenneTwister.h"
#include "../header/Blitters.hpp"
#include "../header/Hue.hpp"
#include <sys/timeb.h>
#include <queue>

FILTERSIMPLE_SIGNATURE(Invert)
    ) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Invert, 0) _FOE
FILTERSIMPLE_BEGIN
FILTERSIMPLE_LOOPBEGIN
    BLENDPIXEL_INVERT(pCurrent)
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Invert_Color)
    ) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Invert_Color, 0) _FOE
FILTERSIMPLE_BEGIN
FILTERSIMPLE_LOOPBEGIN
    BLENDPIXEL_INVERT_COLOR(pCurrent)
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Invert_Channel)
    , int Channel) {
FILTERSIMPLE_INIT
	  ColorChannels ch = (ColorChannels)ClipValue(Channel, 3);
    _FOS(FilterSimple_Invert_Channel, 1) , Channel _FOE
FILTERSIMPLE_BEGIN
FILTERSIMPLE_LOOPBEGIN
    (*pCurrent)[ch] = ~(*pCurrent)[ch];
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Replace)
    , Pixel Find, Pixel Replace) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Replace, 2) , Find, Replace _FOE
FILTERSIMPLE_BEGIN
FILTERSIMPLE_LOOPBEGIN
    if (pCurrent->V == Find.V) {
        pCurrent->V = Replace.V;
    }
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Fill_Channel)
    , int Channel, int Value) {
FILTERSIMPLE_INIT
	ColorChannels ch = (ColorChannels)ClipValue(Channel, 3);
    _FOS(FilterSimple_Fill_Channel, 2) , Channel, Value _FOE
FILTERSIMPLE_BEGIN
    Byte value = ClipByte(Value);
FILTERSIMPLE_LOOPBEGIN
    (*pCurrent)[ch] = value;
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Fill)
    , Pixel Value) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Fill, 1) , Value _FOE
FILTERSIMPLE_BEGIN
    iCX = iCX;
    if ((rCoordinates.Left == 0) && (rCoordinates.Top == 0) && (rCoordinates.Width == Image->Width) && (rCoordinates.Height == Image->Height)) {
        // muahahaha
        _Fill<Pixel>(Image->fast_pointer(0, 0), Value, Image->Width * Image->Height);
        return Success;
    }
    while (iCY--) {
        _Fill<Pixel>(pCurrent, Value, rCoordinates.Width);
        pCurrent += iRowOffset + rCoordinates.Width;
    }
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Fill_Opacity)
    , Pixel Value, int Opacity) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Fill_Opacity, 2) , Value, Opacity _FOE
FILTERSIMPLE_BEGIN
    Byte redTable[256], greenTable[256], blueTable[256];
    {
      AlphaLevel *aSource, *aDest;
      Byte r, g, b;
      aSource = AlphaLevelLookup( Opacity );
      aDest = AlphaLevelLookup( Opacity ^ 0xFF );
      r = AlphaFromLevel(aSource, Value[::Red]);
      g = AlphaFromLevel(aSource, Value[::Green]);
      b = AlphaFromLevel(aSource, Value[::Blue]);
      for (int i = 0; i < 256; i++) {
        redTable[i] = ClipByteHigh(AlphaFromLevel(aDest, i) + r);
        greenTable[i] = ClipByteHigh(AlphaFromLevel(aDest, i) + g);
        blueTable[i] = ClipByteHigh(AlphaFromLevel(aDest, i) + b);
      }
    }
FILTERSIMPLE_LOOPBEGIN
    (*pCurrent)[::Blue] = blueTable[(*pCurrent)[::Blue]];
    (*pCurrent)[::Green] = greenTable[(*pCurrent)[::Green]];
    (*pCurrent)[::Red] = redTable[(*pCurrent)[::Red]];
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Fill_SourceAlpha)
    , Pixel Value) {
FILTERSIMPLE_INIT
    if (Value[::Alpha] == 0) return Trivial_Success;
//    if (Value[::Alpha] == 255) return FilterSimple_Fill(Image, Area, Value);
    _FOS(FilterSimple_Fill_SourceAlpha, 1) , Value _FOE
FILTERSIMPLE_BEGIN
  if (Value[::Alpha] == 255) {
    FILTERSIMPLE_LOOPBEGIN
        (*pCurrent)[::Blue] = Value[::Blue];
        (*pCurrent)[::Green] = Value[::Green];
        (*pCurrent)[::Red] = Value[::Red];
    FILTERSIMPLE_LOOPEND
  } else {
    Byte redTable[256], greenTable[256], blueTable[256];
    {
      AlphaLevel *aSource, *aDest;
      Byte r, g, b;
      aSource = AlphaLevelLookup( Value[::Alpha] );
      aDest = AlphaLevelLookup( Value[::Alpha] ^ 0xFF );
      r = AlphaFromLevel(aSource, Value[::Red]);
      g = AlphaFromLevel(aSource, Value[::Green]);
      b = AlphaFromLevel(aSource, Value[::Blue]);
      for (int i = 0; i < 256; i++) {
        redTable[i] = ClipByteHigh(AlphaFromLevel(aDest, i) + r);
        greenTable[i] = ClipByteHigh(AlphaFromLevel(aDest, i) + g);
        blueTable[i] = ClipByteHigh(AlphaFromLevel(aDest, i) + b);
      }
    }
    FILTERSIMPLE_LOOPBEGIN
        (*pCurrent)[::Blue] = blueTable[(*pCurrent)[::Blue]];
        (*pCurrent)[::Green] = greenTable[(*pCurrent)[::Green]];
        (*pCurrent)[::Red] = redTable[(*pCurrent)[::Red]];
    FILTERSIMPLE_LOOPEND
  }
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Fill_SourceAlpha_Opacity)
    , Pixel Value, int Opacity) {
FILTERSIMPLE_INIT
    if (Opacity == 0) return Trivial_Success;
    if (Opacity == 255) return FilterSimple_Fill_SourceAlpha(Image, Area, Value);
    _FOS(FilterSimple_Fill_SourceAlpha_Opacity, 2) , Value, Opacity _FOE
FILTERSIMPLE_BEGIN
    Byte redTable[256], greenTable[256], blueTable[256];
    {
      AlphaLevel *aSource, *aDest, *aScale;
      Byte r, g, b;
      aScale = AlphaLevelLookup( ClipByte(Opacity) );
      aSource = AlphaLevelLookup( AlphaFromLevel(aScale, Value[::Alpha]) );
      aDest = AlphaLevelLookup( AlphaFromLevel(aScale, Value[::Alpha]) ^ 0xFF );
      r = AlphaFromLevel(aSource, Value[::Red]);
      g = AlphaFromLevel(aSource, Value[::Green]);
      b = AlphaFromLevel(aSource, Value[::Blue]);
      for (int i = 0; i < 256; i++) {
        redTable[i] = ClipByteHigh(AlphaFromLevel(aDest, i) + r);
        greenTable[i] = ClipByteHigh(AlphaFromLevel(aDest, i) + g);
        blueTable[i] = ClipByteHigh(AlphaFromLevel(aDest, i) + b);
      }
    }
FILTERSIMPLE_LOOPBEGIN
    (*pCurrent)[::Blue] = blueTable[(*pCurrent)[::Blue]];
    (*pCurrent)[::Green] = greenTable[(*pCurrent)[::Green]];
    (*pCurrent)[::Red] = redTable[(*pCurrent)[::Red]];
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Fill_Additive)
    , Pixel Value) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Fill_Additive, 1) , Value _FOE
FILTERSIMPLE_BEGIN
    Byte redTable[256], greenTable[256], blueTable[256];
    {
      Byte r, g, b;
      r = Value[::Red];
      g = Value[::Green];
      b = Value[::Blue];
      for (int i = 0; i < 256; i++) {
        redTable[i] = ClipByteHigh(i + r);
        greenTable[i] = ClipByteHigh(i + g);
        blueTable[i] = ClipByteHigh(i + b);
      }
    }
FILTERSIMPLE_LOOPBEGIN
    (*pCurrent)[::Blue] = blueTable[(*pCurrent)[::Blue]];
    (*pCurrent)[::Green] = greenTable[(*pCurrent)[::Green]];
    (*pCurrent)[::Red] = redTable[(*pCurrent)[::Red]];
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Fill_Additive_Opacity)
    , Pixel Value, int Opacity) {
FILTERSIMPLE_INIT
    if (Opacity == 0) return Trivial_Success;
    if (Opacity == 255) return FilterSimple_Fill_Additive(Image, Area, Value);
    _FOS(FilterSimple_Fill_Additive_Opacity, 2) , Value, Opacity _FOE
FILTERSIMPLE_BEGIN
    Byte redTable[256], greenTable[256], blueTable[256];
    {
      AlphaLevel *aScale;
      Byte r, g, b;
      aScale = AlphaLevelLookup( ClipByte(Opacity) );
      r = AlphaFromLevel(aScale, Value[::Red]);
      g = AlphaFromLevel(aScale, Value[::Green]);
      b = AlphaFromLevel(aScale, Value[::Blue]);
      for (int i = 0; i < 256; i++) {
        redTable[i] = ClipByteHigh(i + r);
        greenTable[i] = ClipByteHigh(i + g);
        blueTable[i] = ClipByteHigh(i + b);
      }
    }
FILTERSIMPLE_LOOPBEGIN
    (*pCurrent)[::Blue] = blueTable[(*pCurrent)[::Blue]];
    (*pCurrent)[::Green] = greenTable[(*pCurrent)[::Green]];
    (*pCurrent)[::Red] = redTable[(*pCurrent)[::Red]];
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Fill_Subtractive)
    , Pixel Value) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Fill_Subtractive, 1) , Value _FOE
FILTERSIMPLE_BEGIN
    Byte redTable[256], greenTable[256], blueTable[256];
    {
      Byte r, g, b;
      r = Value[::Red];
      g = Value[::Green];
      b = Value[::Blue];
      for (int i = 0; i < 256; i++) {
        redTable[i] = ClipByteLow(i - r);
        greenTable[i] = ClipByteLow(i - g);
        blueTable[i] = ClipByteLow(i - b);
      }
    }
FILTERSIMPLE_LOOPBEGIN
    (*pCurrent)[::Blue] = blueTable[(*pCurrent)[::Blue]];
    (*pCurrent)[::Green] = greenTable[(*pCurrent)[::Green]];
    (*pCurrent)[::Red] = redTable[(*pCurrent)[::Red]];
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Fill_Subtractive_Opacity)
    , Pixel Value, int Opacity) {
FILTERSIMPLE_INIT
    if (Opacity == 0) return Trivial_Success;
    if (Opacity == 255) return FilterSimple_Fill_Subtractive(Image, Area, Value);
    _FOS(FilterSimple_Fill_Subtractive_Opacity, 2) , Value, Opacity _FOE
FILTERSIMPLE_BEGIN
    Byte redTable[256], greenTable[256], blueTable[256];
    {
      AlphaLevel *aScale;
      Byte r, g, b;
      aScale = AlphaLevelLookup( ClipByte(Opacity) );
      r = AlphaFromLevel(aScale, Value[::Red]);
      g = AlphaFromLevel(aScale, Value[::Green]);
      b = AlphaFromLevel(aScale, Value[::Blue]);
      for (int i = 0; i < 256; i++) {
        redTable[i] = ClipByteLow(i - r);
        greenTable[i] = ClipByteLow(i - g);
        blueTable[i] = ClipByteLow(i - b);
      }
    }
FILTERSIMPLE_LOOPBEGIN
    (*pCurrent)[::Blue] = blueTable[(*pCurrent)[::Blue]];
    (*pCurrent)[::Green] = greenTable[(*pCurrent)[::Green]];
    (*pCurrent)[::Red] = redTable[(*pCurrent)[::Red]];
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Swap_Channels)
    , int Channel1, int Channel2) {
FILTERSIMPLE_INIT
	ColorChannels ch1 = (ColorChannels)ClipValue(Channel1, 3);
	ColorChannels ch2 = (ColorChannels)ClipValue(Channel2, 3);
    _FOS(FilterSimple_Swap_Channels, 2) , Channel1, Channel2 _FOE
FILTERSIMPLE_BEGIN
FILTERSIMPLE_LOOPBEGIN
    _Swap<Byte>((*pCurrent)[ch1], (*pCurrent)[ch2]);
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Composite)
    , Pixel Value) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Composite, 1) , Value _FOE
FILTERSIMPLE_BEGIN
    AlphaLevel *aSource, *aDest;
FILTERSIMPLE_LOOPBEGIN
    aSource = AlphaLevelLookup( (*pCurrent)[::Alpha] );
    aDest = AlphaLevelLookup( (*pCurrent)[::Alpha] ^ 0xFF );
    (*pCurrent)[::Blue] = AlphaFromLevel2(aSource, (*pCurrent)[::Blue], aDest, Value[::Blue]);
    (*pCurrent)[::Green] = AlphaFromLevel2(aSource, (*pCurrent)[::Green], aDest, Value[::Green]);
    (*pCurrent)[::Red] = AlphaFromLevel2(aSource, (*pCurrent)[::Red], aDest, Value[::Red]);
    (*pCurrent)[::Alpha] = 255;
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Premultiply)
    ) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Premultiply, 0)  _FOE
FILTERSIMPLE_BEGIN
    AlphaLevel *aSource;
FILTERSIMPLE_LOOPBEGIN
    aSource = AlphaLevelLookup( (*pCurrent)[::Alpha] );
    (*pCurrent)[::Blue] = AlphaFromLevel(aSource, (*pCurrent)[::Blue]);
    (*pCurrent)[::Green] = AlphaFromLevel(aSource, (*pCurrent)[::Green]);
    (*pCurrent)[::Red] = AlphaFromLevel(aSource, (*pCurrent)[::Red]);
FILTERSIMPLE_LOOPEND
    Image->OptimizeData.premultiplied = true;
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Blur_Horizontal)
    , int Radius) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Blur_Horizontal, 1) , Radius _FOE
    if (Radius < 0) return Failure;
    if (Radius == 0) return Trivial_Success;
FILTERSIMPLE_BEGIN
    int rSum = 0, gSum = 0, bSum = 0;
    int factor = (Radius * 2) + 1;
    std::queue<Pixel> queue;
FILTERSIMPLE_ROW
    int y = rCoordinates.bottom_exclusive() - iCY;
    int x = rCoordinates.Left;
    rSum = gSum = bSum = 0;
    while (!queue.empty())
      queue.pop();
    Pixel l = Image->getPixelClipNO(rCoordinates.Left, y);
    for (int f = 0; f < Radius; f++) {
      rSum += l[::Red];
      gSum += l[::Green];
      bSum += l[::Blue];
      queue.push(l);
    }
    for (int f = 0; f <= Radius; f++) {
      Pixel c = Image->getPixelClipNO(rCoordinates.Left + f, y);
      rSum += c[::Red];
      gSum += c[::Green];
      bSum += c[::Blue];
      queue.push(c);
    }
FILTERSIMPLE_COL
    (*pCurrent)[::Blue] = (bSum / factor);
    (*pCurrent)[::Green] = (gSum / factor);
    (*pCurrent)[::Red] = (rSum / factor);
    Pixel add = Image->getPixelClipNO(x + Radius + 1, y);
//    Pixel remove = Image->getPixelClipNO(x - Radius, y);
    Pixel remove = queue.front();
    bSum += add[::Blue];
    gSum += add[::Green];
    rSum += add[::Red];
    bSum -= remove[::Blue];
    gSum -= remove[::Green];
    rSum -= remove[::Red];
    x++;
    queue.pop();
    queue.push(add);
FILTERSIMPLE_COLEND
FILTERSIMPLE_ROWEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Blur_Vertical)
    , int Radius) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Blur_Vertical, 1) , Radius _FOE
    if (Radius < 0) return Failure;
    if (Radius == 0) return Trivial_Success;
FILTERSIMPLE_BEGIN
    int rSum = 0, gSum = 0, bSum = 0;
    int factor = (Radius * 2) + 1;
    std::queue<Pixel> queue;
FILTERSIMPLE_INVROW
    int x = rCoordinates.right_exclusive() - iCX;
    int y = rCoordinates.Top;
    rSum = gSum = bSum = 0;
    while (!queue.empty())
      queue.pop();
    Pixel l = Image->getPixelClipNO(x, rCoordinates.Top);
    for (int f = 0; f < Radius; f++) {
      rSum += l[::Red];
      gSum += l[::Green];
      bSum += l[::Blue];
      queue.push(l);
    }
    for (int f = 0; f <= Radius; f++) {
      Pixel c = Image->getPixelClipNO(x, rCoordinates.Top + f);
      rSum += c[::Red];
      gSum += c[::Green];
      bSum += c[::Blue];
      queue.push(c);
    }
FILTERSIMPLE_INVCOL
    (*pCurrent)[::Blue] = (bSum / factor);
    (*pCurrent)[::Green] = (gSum / factor);
    (*pCurrent)[::Red] = (rSum / factor);
    Pixel add = Image->getPixelClipNO(x, y + Radius + 1);
//    Pixel remove = Image->getPixelClipNO(x, y - Radius);
    Pixel remove = queue.front();
    bSum += add[::Blue];
    gSum += add[::Green];
    rSum += add[::Red];
    bSum -= remove[::Blue];
    gSum -= remove[::Green];
    rSum -= remove[::Red];
    y++;
    queue.pop();
    queue.push(add);
FILTERSIMPLE_INVCOLEND
FILTERSIMPLE_INVROWEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Blur)
    , int XRadius, int YRadius) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Blur, 2) , XRadius, YRadius _FOE
    if ((XRadius < 0) || (YRadius < 0)) return Failure;
    if ((XRadius == 0) && (YRadius == 0)) return Trivial_Success;
FILTERSIMPLE_BEGIN
    FilterSimple_Blur_Horizontal(Image, Area, XRadius);
    FilterSimple_Blur_Vertical(Image, Area, YRadius);
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(PrepareNormals)
    ) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_PrepareNormals, 0)  _FOE
FILTERSIMPLE_BEGIN
    FPoint3 normal;
FILTERSIMPLE_LOOPBEGIN
    normal.X = ((*pCurrent)[::Red] - 127) / 127.0f;
    normal.Y = ((*pCurrent)[::Green] - 127) / 127.0f;
    normal.Z = ((*pCurrent)[::Blue] - 127) / 127.0f;
    if (abs(1.0 - normal.length()) >= NormalAccuracyThreshold) {
      normal.normalize();
      *pCurrent = Pixel(Round(normal.X * 127.0f) + 127, Round(normal.Y * 127.0f) + 127, Round(normal.Z * 127.0f) + 127, (*pCurrent)[::Alpha]);
    }
FILTERSIMPLE_LOOPEND
    Image->OptimizeData.normalsPrepared = true;
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Grid_SourceAlpha)
    , Pixel Value, int Width, int Height, int XOffset, int YOffset) {
FILTERSIMPLE_INIT
    if (Width < 1) return Failure;
    if (Height < 1) return Failure;
    _FOS(FilterSimple_Grid_SourceAlpha, 5) , Value, Width, Height, XOffset, YOffset _FOE
FILTERSIMPLE_BEGIN
AlphaLevel *aSource, *aDest;
Pixel *pRow = Image->pointer(0,0);
Pixel *pCol = pRow;
Pixel *pValue = &Value;
    iRowOffset = Image->pointer(0,1) - Image->pointer(0,0);
    aSource = AlphaLevelLookup( Value[::Alpha] );
    aDest = AlphaLevelLookup( Value[::Alpha] ^ 0xFF );

    iCY = YOffset + rCoordinates.Top;
    while (iCY < rCoordinates.bottom()) {
      if (iCY >= rCoordinates.Top) {
        // row
        pRow = Image->pointer(rCoordinates.Left, iCY);
        pCol = pRow;
        iCX = rCoordinates.Left;
        while(iCX < rCoordinates.right()) {
          if (iCX >= rCoordinates.Left) {
            BLENDPIXEL_ALPHA_OPACITY(pCol, pCol, pValue, aDest, aSource)
            pCol++;
          }
          iCX++;
        }
      }
      iCY += Height;
    }

    iCX = XOffset + rCoordinates.Left;
    while(iCX < rCoordinates.right()) {
      if (iCX >= rCoordinates.Left) {
        // column
        pCol = Image->pointer(iCX, rCoordinates.Top);
        pRow = pCol;
        iCY = rCoordinates.Top;
        while(iCY < rCoordinates.bottom()) {
          if (iCY >= rCoordinates.Top) {
            BLENDPIXEL_ALPHA_OPACITY(pRow, pRow, pValue, aDest, aSource)
            pRow += iRowOffset;
          }
          iCY++;
        }
      }
      iCX += Width;
    }

FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Mirror)
    ) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Mirror, 0) _FOE
    if (!Image->Unlocked) {
        return Failure;
    }
                                                                
    Rectangle rCoordinates;                                     
        if (!Area) {                                            
            rCoordinates = Image->ClipRectangle;               
        } else {                                                
            rCoordinates = *Area;                               
        }                                                       
        Image->clipRectangle(&rCoordinates);                    
                                                                
    if (rCoordinates.empty()) {                                 
        return Trivial_Success;                                 
    }                                                           
                                                                
    Pixel *pRow, *pCol, *pCol2;
    Pixel temp;
                                                                
    unsigned int iCX = 0 , iCY = rCoordinates.Height;
    // We are only going to loop through the first half of each scanline
    unsigned int iWidth = rCoordinates.Width / 2;

    while (iCY--) {                                             

        iCX = iWidth;
        pRow = Image->pointer(rCoordinates.Left, (rCoordinates.Height-1) - iCY + rCoordinates.Top);
                                                                
        while (iCX--) {
            
            // I'd do iteration here but I'm too lazy to get the math right
            pCol = pRow + (iCX);
            pCol2 = pRow + ((rCoordinates.Width-1) - iCX);
            // For every pixel in the first half of the scanline, swap it with its counterpart in the other half
            temp = *pCol;
            *pCol = *pCol2;
            *pCol2 = temp;
                                                                
        }                                                       
                                                                
    }

FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Flip)
    ) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Flip, 0) _FOE
    if (!Image->Unlocked) {
        return Failure;
    }
                                                                
    Rectangle rCoordinates;                                     
        if (!Area) {                                            
            rCoordinates = Image->ClipRectangle;               
        } else {                                                
            rCoordinates = *Area;                               
        }                                                       
        Image->clipRectangle(&rCoordinates);                    
                                                                
    if (rCoordinates.empty()) {                                 
        return Trivial_Success;                                 
    }  
    
    // Instead of using a temporary buffer for the swapping, I do in-place swapping.
    // In-place swapping is a bit faster in my tests, but I'm not sure if it's faster in general, so...
    // To enable using a temporary buffer just uncomment the necessary lines.
                                                                
    Pixel *pRow, *pRow2;
//    Pixel *pTemp;

    // Allocate a temporary buffer for swapping the scanlines
//    pTemp = AllocateArray(Pixel, rCoordinates.Width);
                                                                
    unsigned int iCY = rCoordinates.Height / 2;           

    while (iCY--) {                                             
                
        // I could probably use iteration here but I'm too lazy to get the math right
        pRow = Image->pointer(rCoordinates.Left, (rCoordinates.Height-1) - iCY + rCoordinates.Top);
        pRow2 = Image->pointer(rCoordinates.Left, iCY + rCoordinates.Top);
        // Swap the rows
        _Swap<Pixel>(pRow, pRow2, rCoordinates.Width);

//        _Copy<Pixel>(pTemp, pRow, rCoordinates.Width);
//        _Copy<Pixel>(pRow, pRow2, rCoordinates.Width);
//        _Copy<Pixel>(pRow2, pTemp, rCoordinates.Width);

                                                                
    }

//    DeleteArray(pTemp);

FILTERSIMPLE_END

Export int FilterSimple_Rotate90(Image *Image) {
    if (!Image) {
        return Failure;
    }
    if (!Image->initialized()) { 
        return Failure;
    }                                                           

    {
        int overrideresult = Override::EnumOverrides(Override::FilterSimple_Rotate90, 1, Image);
#ifdef OVERRIDES
        if (overrideresult != 0) return overrideresult;
#endif
    }
    
    ImageLockManager ilImage(lockingMode, Image);
    if (!ilImage.performUnlock()) {
        return Failure;
    }
                                                                                                                                                                                           
    int iCX = 0, iCY = 0;
    class Image *OldImage = new class Image(Image);
    Image->resize(Image->Height, Image->Width);

    Rectangle rCoordinates;                                     
    rCoordinates = Image->ClipRectangle;               
                                                                
    if (rCoordinates.empty()) {                                 
        return Trivial_Success;                                 
    }

    while (iCY < rCoordinates.Height) {

        iCX = 0;
                                                                
        while (iCX < rCoordinates.Width) {

            Image->setPixel(iCX, iCY, OldImage->getPixel(iCY, iCX));
            
            iCX++;
                                                                
        }                                                       
                                                                
        iCY++;

    }

    delete OldImage;
    return Success;

}

FILTERSIMPLE_SIGNATURE(Grayscale)
    ) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Grayscale, 0) _FOE
FILTERSIMPLE_BEGIN
    int g;
    Word bt[256], gt[256], rt[256];
    for (int i = 0; i < 256; i++) {
        // These three multipliers are used to calculate the luminance of each pixel.
        // Since the multipliers are constant, generating a lookup table beforehand results in better speed
        rt[i] = i * 30;
        gt[i] = i * 59;
        bt[i] = i * 11;
    }
FILTERSIMPLE_LOOPBEGIN
    // Feed the three color channels through the multiplier tables, and then divide by 100 to get a single value
    g = (bt[(*pCurrent)[::Blue]] + gt[(*pCurrent)[::Green]] + rt[(*pCurrent)[::Red]]) / 100; 
    (*pCurrent)[::Blue] = (*pCurrent)[::Green] = (*pCurrent)[::Red] = g;
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(ColorFilter)
    , ColorFilter *Filter) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_ColorFilter, 1) , Filter _FOE
FILTERSIMPLE_BEGIN
    if (!Filter) return Failure;
    if (Filter->Length < 768) return Failure;
FILTERSIMPLE_LOOPBEGIN
    // Apply the color filter to every pixel
    (*pCurrent)[::Blue] = Filter->Blue[(*pCurrent)[::Blue]];
    (*pCurrent)[::Green] = Filter->Green[(*pCurrent)[::Green]];
    (*pCurrent)[::Red] = Filter->Red[(*pCurrent)[::Red]];
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Noise)
    ) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Noise, 0) _FOE
FILTERSIMPLE_BEGIN
    // Seed the random number generator using the current time
    MTRand mersenne = MTRand();
FILTERSIMPLE_LOOPBEGIN
    pCurrent->V = mersenne.randInt();
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Noise_Grayscale)
    ) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Noise_Grayscale, 0) _FOE
FILTERSIMPLE_BEGIN
    // Use a lookup table to create a simple repeating sequence without branching
    Byte iteratorTable[4] = {1, 2, 3, 0}, i = 0;
    Pixel v;
    // Seed the random number generator using the current time
    MTRand mersenne = MTRand();
FILTERSIMPLE_LOOPBEGIN
    // Since we're only generating grayscale, we can use each byte of the random numbers we get
    // This means we should only generate a new number every 4 pixels
    if (i == 0) v.V = mersenne.randInt();
    (*pCurrent)[::Blue] = (*pCurrent)[::Green] = (*pCurrent)[::Red] = v[(ColorChannels)i];
    i = iteratorTable[i];
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Noise_Grayscale_Opacity)
    , int Opacity) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Noise_Grayscale_Opacity, 1) , Opacity _FOE
FILTERSIMPLE_BEGIN
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return FilterSimple_Noise_Grayscale(Image, Area);
    // Use a lookup table to create a simple repeating sequence without branching
    AlphaLevel *aSource = AlphaLevelLookup(ClipByte(Opacity)), *aDest = AlphaLevelLookup(ClipByte(Opacity) ^ 0xFF);
    Byte iteratorTable[4] = {1, 2, 3, 0}, i = 0, cv = 0;
    Pixel v;
    // Seed the random number generator using the current time
    MTRand mersenne = MTRand();
FILTERSIMPLE_LOOPBEGIN
    // Since we're only generating grayscale, we can use each byte of the random numbers we get
    // This means we should only generate a new number every 4 pixels
    if (i == 0) v.V = mersenne.randInt();
    cv = AlphaFromLevel(aSource, v[(ColorChannels)i]);
    (*pCurrent)[::Blue] = AlphaFromLevel(aDest, (*pCurrent)[::Blue]) + cv;
    (*pCurrent)[::Green] = AlphaFromLevel(aDest, (*pCurrent)[::Green]) + cv;
    (*pCurrent)[::Red] = AlphaFromLevel(aDest, (*pCurrent)[::Red]) + cv;
    i = iteratorTable[i];
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END


FILTERSIMPLE_SIGNATURE(Noise_Grayscale_Subtractive)
    ) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Noise_Grayscale_Subtractive, 0) _FOE
FILTERSIMPLE_BEGIN
    // Use a lookup table to create a simple repeating sequence without branching
    Byte iteratorTable[4] = {1, 2, 3, 0}, i = 0;
    Pixel v;
    // Seed the random number generator using the current time
    MTRand mersenne = MTRand();
FILTERSIMPLE_LOOPBEGIN
    // Since we're only generating grayscale, we can use each byte of the random numbers we get
    // This means we should only generate a new number every 4 pixels
    if (i == 0) v.V = mersenne.randInt();
    (*pCurrent)[::Blue] = ClipByteLow((*pCurrent)[::Blue] - v[(ColorChannels)i]);
    (*pCurrent)[::Green] = ClipByteLow((*pCurrent)[::Green] - v[(ColorChannels)i]);
    (*pCurrent)[::Red] = ClipByteLow((*pCurrent)[::Red] - v[(ColorChannels)i]);
    i = iteratorTable[i];
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Noise_Grayscale_Subtractive_Opacity)
    , int Opacity) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Noise_Grayscale_Subtractive_Opacity, 1) , Opacity _FOE
FILTERSIMPLE_BEGIN
    if (Opacity <= 0) return Trivial_Success;
    if (Opacity >= 255) return FilterSimple_Noise_Grayscale_Subtractive(Image, Area);
    // Use a lookup table to create a simple repeating sequence without branching
    AlphaLevel *aSource = AlphaLevelLookup(ClipByte(Opacity));
    Byte iteratorTable[4] = {1, 2, 3, 0}, i = 0, cv = 0;
    Pixel v;
    // Seed the random number generator using the current time
    MTRand mersenne = MTRand();
FILTERSIMPLE_LOOPBEGIN
    // Since we're only generating grayscale, we can use each byte of the random numbers we get
    // This means we should only generate a new number every 4 pixels
    if (i == 0) v.V = mersenne.randInt();
    cv = AlphaFromLevel(aSource, v[(ColorChannels)i]);
    (*pCurrent)[::Blue] = ClipByteLow((*pCurrent)[::Blue] - cv);
    (*pCurrent)[::Green] = ClipByteLow((*pCurrent)[::Green] - cv);
    (*pCurrent)[::Red] = ClipByteLow((*pCurrent)[::Red] - cv);
    i = iteratorTable[i];
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Noise_Channel)
    , int Channel) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Noise_Channel, 1) , Channel _FOE
FILTERSIMPLE_BEGIN
    // Use a lookup table to create a simple repeating sequence without branching
    Byte iteratorTable[4] = {1, 2, 3, 0}, i = 0;
    Pixel v;
    // Seed the random number generator using the current time
    MTRand mersenne = MTRand();
	ColorChannels ch = (ColorChannels)ClipValue(Channel, 3);
FILTERSIMPLE_LOOPBEGIN
    // Since we're only generating grayscale, we can use each byte of the random numbers we get
    // This means we should only generate a new number every 4 pixels
    if (i == 0) v.V = mersenne.randInt();
    (*pCurrent)[ch] = v[(ColorChannels)i];
    i = iteratorTable[i];
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Decay)
    , int Strength) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Decay, 1) , Strength _FOE
FILTERSIMPLE_BEGIN
    // Use a lookup table to create a simple repeating sequence without branching
    Byte iteratorTable[4] = {1, 2, 3, 0}, i = 0;
    Byte scaleTable[256];
    Pixel v;
    DoubleWord d;
    // Seed the random number generator using the current time
    MTRand mersenne = MTRand();
    for (int s = 0; s < 256; ++s) {
      scaleTable[s] = ClipByteHigh((s * Strength) / 255);
    }
FILTERSIMPLE_LOOPBEGIN
    // Since we're only generating grayscale, we can use each byte of the random numbers we get
    // This means we should only generate a new number every 4 pixels
    if (i == 0) v.V = mersenne.randInt();
    d = scaleTable[v[(ColorChannels)i]];
    if (d) {
      (*pCurrent)[::Blue] = ((*pCurrent)[::Blue] / d) * d;
      (*pCurrent)[::Green] = ((*pCurrent)[::Green] / d) * d;
      (*pCurrent)[::Red] = ((*pCurrent)[::Red] / d) * d;
    }
    i = iteratorTable[i];
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Solarize)
    , int Strength) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Solarize, 1) , Strength _FOE
FILTERSIMPLE_BEGIN
    Byte lookupTable[256];
    for (int i = 0; i < 256; i++) {
      lookupTable[i] = ClipByteHigh((i / Strength) * Strength);
    }
FILTERSIMPLE_LOOPBEGIN
    (*pCurrent)[::Blue] = lookupTable[(*pCurrent)[::Blue]];
    (*pCurrent)[::Green] = lookupTable[(*pCurrent)[::Green]];
    (*pCurrent)[::Red] = lookupTable[(*pCurrent)[::Red]];
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Adjust)
    , int Amount) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Adjust, 1) , Amount _FOE
FILTERSIMPLE_BEGIN
    Byte lookupTable[256];
    for (int i = 0; i < 256; i++) {
        lookupTable[i] = ClipByte(i + Amount);
    }
FILTERSIMPLE_LOOPBEGIN
    (*pCurrent)[::Blue] = lookupTable[(*pCurrent)[::Blue]];
    (*pCurrent)[::Green] = lookupTable[(*pCurrent)[::Green]];
    (*pCurrent)[::Red] = lookupTable[(*pCurrent)[::Red]];
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Adjust_RGB)
    , int RedAmount, int GreenAmount, int BlueAmount) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Adjust_RGB, 3) , RedAmount, GreenAmount, BlueAmount _FOE
FILTERSIMPLE_BEGIN
    Byte redTable[256], greenTable[256], blueTable[256];
    for (int i = 0; i < 256; i++) {
        redTable[i] = ClipByte(i + RedAmount);
        greenTable[i] = ClipByte(i + GreenAmount);
        blueTable[i] = ClipByte(i + BlueAmount);
    }
FILTERSIMPLE_LOOPBEGIN
    (*pCurrent)[::Blue] = blueTable[(*pCurrent)[::Blue]];
    (*pCurrent)[::Green] = greenTable[(*pCurrent)[::Green]];
    (*pCurrent)[::Red] = redTable[(*pCurrent)[::Red]];
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Adjust_HSV)
    , int HueAmount, int SaturationAmount, int ValueAmount) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Adjust_HSV, 3) , HueAmount, SaturationAmount, ValueAmount _FOE
FILTERSIMPLE_BEGIN
    //HSVA HSVA;
FILTERSIMPLE_LOOPBEGIN
    //HSVA.setPixel(*pCurrent);
    //HSVA.setValuesFast(HSVA.Hue + HueAmount, HSVA.Saturation + SaturationAmount, HSVA.Value + ValueAmount, HSVA.Alpha);
    //*pCurrent = HSVA.getPixel();
    *pCurrent = adjustHSV(*pCurrent, HueAmount, SaturationAmount, ValueAmount);
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Adjust_Channel)
    , int Channel, int Amount) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Adjust_Channel, 2) , Channel, Amount _FOE
FILTERSIMPLE_BEGIN
    Byte lookupTable[256];
    for (int i = 0; i < 256; i++) {
        lookupTable[i] = ClipByte(i + Amount);
    }
	ColorChannels ch = (ColorChannels)ClipValue(Channel, 3);
FILTERSIMPLE_LOOPBEGIN
    (*pCurrent)[ch] = lookupTable[(*pCurrent)[ch]];
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Gamma)
    , float Gamma) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Gamma, 1) , va_float(Gamma) _FOE
FILTERSIMPLE_BEGIN
    Byte lookupTable[256];
    float g = 1.0f / Gamma;
    float f = 0, fi = 1 / 255.0f;
    for (int i = 0; i < 256; i++) {
        lookupTable[i] = ClipByte(pow(f, g) * 255.0f);
        f += fi;
    }
FILTERSIMPLE_LOOPBEGIN
    (*pCurrent)[::Blue] = lookupTable[(*pCurrent)[::Blue]];
    (*pCurrent)[::Green] = lookupTable[(*pCurrent)[::Green]];
    (*pCurrent)[::Red] = lookupTable[(*pCurrent)[::Red]];
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Gamma_RGB)
    , float RedGamma, float GreenGamma, float BlueGamma) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Gamma_RGB, 3) , va_float(RedGamma), va_float(GreenGamma), va_float(BlueGamma) _FOE
FILTERSIMPLE_BEGIN
    Byte redTable[256], greenTable[256], blueTable[256];
    float rg = 1.0f / RedGamma;
    float gg = 1.0f / GreenGamma;
    float bg = 1.0f / BlueGamma;
    float f = 0, fi = 1 / 255.0f;
    for (int i = 0; i < 256; i++) {
        redTable[i] = ClipByte(pow(f, rg) * 255.0f);
        greenTable[i] = ClipByte(pow(f, gg) * 255.0f);
        blueTable[i] = ClipByte(pow(f, bg) * 255.0f);
        f += fi;
    }
FILTERSIMPLE_LOOPBEGIN
    (*pCurrent)[::Blue] = blueTable[(*pCurrent)[::Blue]];
    (*pCurrent)[::Green] = greenTable[(*pCurrent)[::Green]];
    (*pCurrent)[::Red] = redTable[(*pCurrent)[::Red]];
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Gamma_Channel)
    , int Channel, float Gamma) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Gamma_Channel, 2) , Channel, va_float(Gamma) _FOE
FILTERSIMPLE_BEGIN
    Byte lookupTable[256];
    float g = 1.0f / Gamma;
    float f = 0, fi = 1 / 255.0f;
    for (int i = 0; i < 256; i++) {
        lookupTable[i] = ClipByte(pow(f, g) * 255.0f);
        f += fi;
    }
  	ColorChannels ch = (ColorChannels)ClipValue(Channel, 3);
FILTERSIMPLE_LOOPBEGIN
    (*pCurrent)[ch] = lookupTable[(*pCurrent)[ch]];
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Multiply)
    , float Multiply) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Multiply, 1) , va_float(Multiply) _FOE
FILTERSIMPLE_BEGIN
    Byte lookupTable[256];
    for (int i = 0; i < 256; i++) {
        lookupTable[i] = ClipByte(i * Multiply);
    }
FILTERSIMPLE_LOOPBEGIN
    (*pCurrent)[::Blue] = lookupTable[(*pCurrent)[::Blue]];
    (*pCurrent)[::Green] = lookupTable[(*pCurrent)[::Green]];
    (*pCurrent)[::Red] = lookupTable[(*pCurrent)[::Red]];
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Multiply_RGB)
    , float RedMultiply, float GreenMultiply, float BlueMultiply) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Multiply_RGB, 3) , va_float(RedMultiply), va_float(GreenMultiply), va_float(BlueMultiply) _FOE
FILTERSIMPLE_BEGIN
    Byte redTable[256], greenTable[256], blueTable[256];
    for (int i = 0; i < 256; i++) {
        redTable[i] = ClipByte(i * RedMultiply);
        greenTable[i] = ClipByte(i * GreenMultiply);
        blueTable[i] = ClipByte(i * BlueMultiply);
    }
FILTERSIMPLE_LOOPBEGIN
    (*pCurrent)[::Blue] = blueTable[(*pCurrent)[::Blue]];
    (*pCurrent)[::Green] = greenTable[(*pCurrent)[::Green]];
    (*pCurrent)[::Red] = redTable[(*pCurrent)[::Red]];
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Multiply_Channel)
    , int Channel, float Multiply) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Multiply_Channel, 2) , Channel, va_float(Multiply) _FOE
FILTERSIMPLE_BEGIN
    Byte lookupTable[256];
    for (int i = 0; i < 256; i++) {
        lookupTable[i] = ClipByte(i * Multiply);
    }
  	ColorChannels ch = (ColorChannels)ClipValue(Channel, 3);
FILTERSIMPLE_LOOPBEGIN
    (*pCurrent)[ch] = lookupTable[(*pCurrent)[ch]];
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Depalettize)
    , Byte *Data, Pixel *Palette, int Length) {
FILTERSIMPLE_INIT
    if (!Data) return Failure;
    if (!Palette) return Failure;
    if (Length < 1) return Trivial_Success;
    _FOS(FilterSimple_Depalettize, 3) , Data, Palette, Length _FOE
FILTERSIMPLE_BEGIN
    int i = 0;
FILTERSIMPLE_LOOPBEGIN
    pCurrent->V = Palette[*Data].V;
    Data++;
    i++;
    if (i >= Length) return Success;
FILTERSIMPLE_LOOPEND
FILTERSIMPLE_END

