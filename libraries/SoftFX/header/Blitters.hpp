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

#define SIGNATURE(name) \
    Export int BlitSimple_##name (Image *Dest, Image *Source,   \
     Rectangle *R, Size CX, Size CY 
#define END_SIGNATURE );

SIGNATURE(Normal) END_SIGNATURE
SIGNATURE(Normal_Opacity) , int Opacity END_SIGNATURE
SIGNATURE(Normal_Tint) , Pixel Tint END_SIGNATURE
SIGNATURE(Normal_Tint_Opacity) , Pixel Tint, int Opacity END_SIGNATURE
SIGNATURE(Matte) END_SIGNATURE
SIGNATURE(Matte_Tint_Opacity) , Pixel Tint, int Opacity END_SIGNATURE
SIGNATURE(Matte_Opacity) , int Opacity END_SIGNATURE
SIGNATURE(SourceAlpha) END_SIGNATURE
SIGNATURE(SourceAlpha_Tint_Opacity) , Pixel Tint, int Opacity END_SIGNATURE
SIGNATURE(SourceAlpha_Opacity) , int Opacity END_SIGNATURE
SIGNATURE(Merge) END_SIGNATURE
SIGNATURE(Merge_Opacity) , int Opacity END_SIGNATURE
SIGNATURE(Additive) END_SIGNATURE
SIGNATURE(Additive_Opacity) , int Opacity END_SIGNATURE
SIGNATURE(Screen) END_SIGNATURE
SIGNATURE(Screen_Opacity) , int Opacity END_SIGNATURE
SIGNATURE(Multiply) END_SIGNATURE
SIGNATURE(Multiply_Opacity) , int Opacity END_SIGNATURE
SIGNATURE(Lightmap) END_SIGNATURE
SIGNATURE(Lightmap_Opacity) , int Opacity END_SIGNATURE
SIGNATURE(Lightmap_RGB) END_SIGNATURE
SIGNATURE(Lightmap_RGB_Opacity) , int Opacity END_SIGNATURE
SIGNATURE(Subtractive) END_SIGNATURE
SIGNATURE(Subtractive_Opacity) , int Opacity END_SIGNATURE
SIGNATURE(Font_SourceAlpha_RGB) , Pixel Color END_SIGNATURE
SIGNATURE(Font_SourceAlpha_RGB_Opacity) , Pixel Color, int Opacity END_SIGNATURE
SIGNATURE(Font_Merge_RGB) , Pixel Color END_SIGNATURE
SIGNATURE(Font_Merge_RGB_Opacity) , Pixel Color, int Opacity END_SIGNATURE
SIGNATURE(Automatic_Matte_Opacity) , int Opacity END_SIGNATURE
SIGNATURE(Automatic_SourceAlpha_Opacity) , int Opacity END_SIGNATURE

#define TSIGNATURE(name) \
    Export int BlitTile_##name (Image *Dest, Image *Source,   \
     Rectangle *R 
#define END_TSIGNATURE );

enum SFX_BlitModes {
    BlitMode_Default = -1,
    BlitMode_Normal = 0,
    BlitMode_Matte = 1,
    BlitMode_SourceAlpha = 2,
    BlitMode_Additive = 3,
    BlitMode_Subtractive = 4,
    BlitMode_AND = 5,
    BlitMode_OR = 6,
    BlitMode_XOR = 7,
    BlitMode_Lightmap = 8,
    BlitMode_Lightmap_RGB = 9,
    BlitMode_Matte_Tint = 10,
    BlitMode_SourceAlpha_Tint = 11,
    BlitMode_Font = 12,
    BlitMode_Font_SourceAlpha = 13,
    BlitMode_Dither = 14,
    BlitMode_Screen = 15,
    BlitMode_Multiply = 16,
    BlitMode_Merge = 17
};

TSIGNATURE(Normal) END_TSIGNATURE
TSIGNATURE(Normal_Opacity) , int Opacity END_TSIGNATURE
TSIGNATURE(Matte_Opacity) , int Opacity END_TSIGNATURE
TSIGNATURE(Matte_Tint_Opacity) , Pixel Tint, int Opacity END_TSIGNATURE
TSIGNATURE(SourceAlpha_Opacity) , int Opacity END_TSIGNATURE
TSIGNATURE(SourceAlpha_Tint_Opacity) , Pixel Tint, int Opacity END_TSIGNATURE
TSIGNATURE(Additive_Opacity) , int Opacity END_TSIGNATURE
TSIGNATURE(Subtractive_Opacity) , int Opacity END_TSIGNATURE
TSIGNATURE(Merge_Opacity) , int Opacity END_TSIGNATURE

#define RSIGNATURE(name) \
    Export int BlitResample_##name (Image *Dest, Image *Source, \
        Rectangle *DestRect, Rectangle *SourceRect,             \
        ScalerFunction *Scaler
#define END_RSIGNATURE );

RSIGNATURE(Normal) END_RSIGNATURE
RSIGNATURE(Normal_Opacity) , int Opacity END_RSIGNATURE
RSIGNATURE(SourceAlpha) END_RSIGNATURE
RSIGNATURE(SourceAlpha_Opacity) , int Opacity END_RSIGNATURE
RSIGNATURE(SourceAlpha_Tint) , Pixel Tint END_RSIGNATURE
RSIGNATURE(SourceAlpha_Tint_Opacity) , Pixel Tint , int Opacity END_RSIGNATURE
RSIGNATURE(Additive) END_RSIGNATURE
RSIGNATURE(Additive_Opacity) , int Opacity END_RSIGNATURE
RSIGNATURE(Subtractive) END_RSIGNATURE
RSIGNATURE(Subtractive_Opacity) , int Opacity END_RSIGNATURE

inline int ModedBlit(SFX_BlitModes Mode, Image *Dest, Image *Source, Rectangle *Area, int SX, int SY, int Opacity) {
    switch(Mode) {
    case BlitMode_Additive:
        return BlitSimple_Additive_Opacity(Dest, Source, Area, SX, SY, Opacity);
        break;
    case BlitMode_Subtractive:
        return BlitSimple_Subtractive_Opacity(Dest, Source, Area, SX, SY, Opacity);
        break;
    case BlitMode_SourceAlpha:
        return BlitSimple_Automatic_SourceAlpha_Opacity(Dest, Source, Area, SX, SY, Opacity);
        break;
    case BlitMode_Matte:
        return BlitSimple_Automatic_Matte_Opacity(Dest, Source, Area, SX, SY, Opacity);
        break;
    case BlitMode_Normal:
        return BlitSimple_Normal_Opacity(Dest, Source, Area, SX, SY, Opacity);
        break;
    case BlitMode_Lightmap:
        return BlitSimple_Lightmap_Opacity(Dest, Source, Area, SX, SY, Opacity);
        break;
    case BlitMode_Lightmap_RGB:
        return BlitSimple_Lightmap_RGB_Opacity(Dest, Source, Area, SX, SY, Opacity);
        break;
    case BlitMode_Screen:
        return BlitSimple_Screen_Opacity(Dest, Source, Area, SX, SY, Opacity);
        break;
    case BlitMode_Multiply:
        return BlitSimple_Multiply_Opacity(Dest, Source, Area, SX, SY, Opacity);
        break;
    case BlitMode_Merge:
        return BlitSimple_Merge_Opacity(Dest, Source, Area, SX, SY, Opacity);
        break;
    default:
    case BlitMode_Default:
        return Failure;
        break;
    }
}

inline int ModedBlit(SFX_BlitModes Mode, Image *Dest, Image *Source, Rectangle *Area, int SX, int SY, Pixel Color, int Opacity) {
    switch(Mode) {
    case BlitMode_Additive:
        return BlitSimple_Additive_Opacity(Dest, Source, Area, SX, SY, Opacity);
        break;
    case BlitMode_Subtractive:
        return BlitSimple_Subtractive_Opacity(Dest, Source, Area, SX, SY, Opacity);
        break;
    case BlitMode_SourceAlpha:
        return BlitSimple_SourceAlpha_Tint_Opacity(Dest, Source, Area, SX, SY, Color, Opacity);
        break;
    case BlitMode_Matte:
        return BlitSimple_Matte_Tint_Opacity(Dest, Source, Area, SX, SY, Color, Opacity);
        break;
    case BlitMode_Normal:
        return BlitSimple_Normal_Opacity(Dest, Source, Area, SX, SY, Opacity);
        break;
    case BlitMode_Lightmap:
        return BlitSimple_Lightmap_Opacity(Dest, Source, Area, SX, SY, Opacity);
        break;
    case BlitMode_Lightmap_RGB:
        return BlitSimple_Lightmap_RGB_Opacity(Dest, Source, Area, SX, SY, Opacity);
        break;
    case BlitMode_Screen:
        return BlitSimple_Screen_Opacity(Dest, Source, Area, SX, SY, Opacity);
        break;
    case BlitMode_Multiply:
        return BlitSimple_Multiply_Opacity(Dest, Source, Area, SX, SY, Opacity);
        break;
    case BlitMode_Merge:
        return BlitSimple_Merge_Opacity(Dest, Source, Area, SX, SY, Opacity);
        break;
    default:
    case BlitMode_Default:
        return Failure;
        break;
    }
}

#ifdef resample_included

  inline int ModedResampleBlit(SFX_BlitModes Mode, Image *Dest, Image *Source, Rectangle *DestRect, Rectangle *SourceRect, int Opacity) {
      switch(Mode) {
      case BlitMode_Additive:
          return BlitResample_Additive_Opacity(Dest, Source, DestRect, SourceRect, DefaultSampleFunction, Opacity);
          break;
      case BlitMode_Subtractive:
          return BlitResample_Subtractive_Opacity(Dest, Source, DestRect, SourceRect, DefaultSampleFunction, Opacity);
          break;
      case BlitMode_SourceAlpha:
          return BlitResample_SourceAlpha_Opacity(Dest, Source, DestRect, SourceRect, DefaultSampleFunction, Opacity);
          break;
      case BlitMode_Normal:
          return BlitResample_Normal_Opacity(Dest, Source, DestRect, SourceRect, DefaultSampleFunction, Opacity);
          break;
      default:
      case BlitMode_Default:
          return Failure;
          break;
      }
  }

  inline int ModedResampleBlit(SFX_BlitModes Mode, Image *Dest, Image *Source, Rectangle *DestRect, Rectangle *SourceRect, Pixel Color, int Opacity) {
      switch(Mode) {
      case BlitMode_Additive:
          return BlitResample_Additive_Opacity(Dest, Source, DestRect, SourceRect, DefaultSampleFunction, Opacity);
          break;
      case BlitMode_Subtractive:
          return BlitResample_Subtractive_Opacity(Dest, Source, DestRect, SourceRect, DefaultSampleFunction, Opacity);
          break;
      case BlitMode_SourceAlpha:
          return BlitResample_SourceAlpha_Tint_Opacity(Dest, Source, DestRect, SourceRect, DefaultSampleFunction, Color, Opacity);
          break;
      case BlitMode_Normal:
          return BlitResample_Normal_Opacity(Dest, Source, DestRect, SourceRect, DefaultSampleFunction, Opacity);
          break;
      default:
      case BlitMode_Default:
          return Failure;
          break;
      }
  }

#endif

inline int ModedTiledBlit(SFX_BlitModes Mode, Image *Dest, Image *Source, Rectangle *Area, int Opacity) {
    switch(Mode) {
    case BlitMode_Additive:
        return BlitTile_Additive_Opacity(Dest, Source, Area, Opacity);
        break;
    case BlitMode_Subtractive:
        return BlitTile_Subtractive_Opacity(Dest, Source, Area, Opacity);
        break;
    case BlitMode_SourceAlpha:
        return BlitTile_SourceAlpha_Opacity(Dest, Source, Area, Opacity);
        break;
    case BlitMode_Normal:
        return BlitTile_Normal_Opacity(Dest, Source, Area, Opacity);
        break;
    case BlitMode_Merge:
        return BlitTile_Merge_Opacity(Dest, Source, Area, Opacity);
        break;
    default:
    case BlitMode_Default:
        return Failure;
        break;
    }
}

inline int ModedTiledBlit(SFX_BlitModes Mode, Image *Dest, Image *Source, Rectangle *Area, Pixel Tint, int Opacity) {
    switch(Mode) {
    case BlitMode_Additive:
        return BlitTile_Additive_Opacity(Dest, Source, Area, Opacity);
        break;
    case BlitMode_Subtractive:
        return BlitTile_Subtractive_Opacity(Dest, Source, Area, Opacity);
        break;
    case BlitMode_SourceAlpha:
        return BlitTile_SourceAlpha_Tint_Opacity(Dest, Source, Area, Tint, Opacity);
        break;
    case BlitMode_Merge:
        return BlitTile_Merge_Opacity(Dest, Source, Area, Opacity);
        break;
    case BlitMode_Normal:
        return BlitTile_Normal_Opacity(Dest, Source, Area, Opacity);
        break;
    default:
    case BlitMode_Default:
        return Failure;
        break;
    }
}