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

#define BLENDPIXEL_ALPHA_OPACITY(BlendTo, BlendDest, BlendSource, TableDest, TableSource)    \
  (*BlendTo)[::Blue] = AlphaFromLevel2(TableDest, (*BlendDest)[::Blue], TableSource, (*BlendSource)[::Blue]);    \
  (*BlendTo)[::Green] = AlphaFromLevel2(TableDest, (*BlendDest)[::Green], TableSource, (*BlendSource)[::Green]);    \
  (*BlendTo)[::Red] = AlphaFromLevel2(TableDest, (*BlendDest)[::Red], TableSource, (*BlendSource)[::Red]);

#define BLENDPIXEL_ALPHA_PREMULTIPLIED_OPACITY(BlendTo, BlendDest, BlendSource, TableDest)    \
  (*BlendTo)[::Blue] = (AlphaFromLevel(TableDest, (*BlendDest)[::Blue]) + (*BlendSource)[::Blue]) & 0xFF;        \
  (*BlendTo)[::Green] = (AlphaFromLevel(TableDest, (*BlendDest)[::Green]) + (*BlendSource)[::Green]) & 0xFF;        \
  (*BlendTo)[::Red] = (AlphaFromLevel(TableDest, (*BlendDest)[::Red]) + (*BlendSource)[::Red]) & 0xFF;

#define BLENDPIXEL_ADDITIVE_FAST(BlendTo, BlendDest, BlendSource)                \
  (*BlendTo)[::Blue] = ((*BlendDest)[::Blue] + (*BlendSource)[::Blue]);                        \
  (*BlendTo)[::Green] = ((*BlendDest)[::Green] + (*BlendSource)[::Green]);                        \
  (*BlendTo)[::Red] = ((*BlendDest)[::Red] + (*BlendSource)[::Red]);                        

#define BLENDPIXEL_ADDITIVE_OPACITY(BlendTo, BlendDest, BlendSource, TableSource)        \
  (*BlendTo)[::Blue] = ClipByteHigh((*BlendDest)[::Blue] + AlphaFromLevel(TableSource, (*BlendSource)[::Blue]));    \
  (*BlendTo)[::Green] = ClipByteHigh((*BlendDest)[::Green] + AlphaFromLevel(TableSource, (*BlendSource)[::Green]));    \
  (*BlendTo)[::Red] = ClipByteHigh((*BlendDest)[::Red] + AlphaFromLevel(TableSource, (*BlendSource)[::Red]));

#define BLENDPIXEL_ADDITIVE(BlendTo, BlendDest, BlendSource)                    \
  (*BlendTo)[::Blue] = ClipByteHigh((*BlendDest)[::Blue] + (*BlendSource)[::Blue]);                    \
  (*BlendTo)[::Green] = ClipByteHigh((*BlendDest)[::Green] + (*BlendSource)[::Green]);                    \
  (*BlendTo)[::Red] = ClipByteHigh((*BlendDest)[::Red] + (*BlendSource)[::Red]);                    

#define BLENDPIXEL_SUBTRACTIVE_OPACITY(BlendTo, BlendDest, BlendSource, TableSource)        \
  (*BlendTo)[::Blue] = ClipByteLow((*BlendDest)[::Blue] - AlphaFromLevel(TableSource, (*BlendSource)[::Blue]));    \
  (*BlendTo)[::Green] = ClipByteLow((*BlendDest)[::Green] - AlphaFromLevel(TableSource, (*BlendSource)[::Green]));    \
  (*BlendTo)[::Red] = ClipByteLow((*BlendDest)[::Red] - AlphaFromLevel(TableSource, (*BlendSource)[::Red]));

#define BLENDPIXEL_SUBTRACTIVE(BlendTo, BlendDest, BlendSource)              \
  (*BlendTo)[::Blue] = ClipByteLow((*BlendDest)[::Blue] - (*BlendSource)[::Blue]);                    \
  (*BlendTo)[::Green] = ClipByteLow((*BlendDest)[::Green] - (*BlendSource)[::Green]);                    \
  (*BlendTo)[::Red] = ClipByteLow((*BlendDest)[::Red] - (*BlendSource)[::Red]);                                

#define BLENDPIXEL_INVERT_COLOR(BlendTo)                        \
  BlendTo->V ^= 0xFFFFFF;

#define BLENDPIXEL_INVERT(BlendTo)                            \
  BlendTo->V ^= 0xFFFFFFFF;

inline Pixel Premultiply(Pixel Color) {
  AlphaLevel *aSource = AlphaLevelLookup( Color[::Alpha] );
  Color[::Blue] = AlphaFromLevel(aSource, Color[::Blue]);
  Color[::Green] = AlphaFromLevel(aSource, Color[::Green]);
  Color[::Red] = AlphaFromLevel(aSource, Color[::Red]);
  return Color;
}

inline Pixel TableBlend(Pixel Color1, AlphaLevel* Level1, Pixel Color2, AlphaLevel* Level2) {
  Color1[::Blue] = AlphaFromLevel(Level1, Color1[::Blue]) + AlphaFromLevel(Level2, Color2[::Blue]);
  Color1[::Green] = AlphaFromLevel(Level1, Color1[::Green]) + AlphaFromLevel(Level2, Color2[::Green]);
  Color1[::Red] = AlphaFromLevel(Level1, Color1[::Red]) + AlphaFromLevel(Level2, Color2[::Red]);
  return Color1;
}