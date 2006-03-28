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

#define defOverrides \
  defStart \
/* OVERRIDES GO HERE */ \
  defItem(Allocate) \
  defItem(Deallocate) \
  defItem(Lock) \
  defItem(Unlock) \
  defItem(Clear) \
  defItem(Fill) \
  defItem(GetPixel) \
  defItem(GetPixelAA) \
  defItem(SetPixel) \
  defItem(SetPixelAA) \
  defItem(GetPixels) \
  defItem(Copy) \
  defItem(CopyEx) \
  defItem(Flip) \
  defItem(Mirror) \
  defItem(Rotate90) \
  defItem(Rotate) \
  defItem(Resample) \
  defItem(ResampleImage_Half) \
  defItem(ResampleImage_Double) \
  defItem(ResampleImage_Linear) \
  defItem(ResampleImage_BiLinear) \
  defItem(SetImageMatteColor) \
  defItem(SetImageClipRectangle) \
  defItem(FilterSimple_Circle) \
  defItem(FilterSimple_Circle_SourceAlpha) \
  defItem(FilterSimple_FilledCircle) \
  defItem(FilterSimple_FilledCircle_SourceAlpha) \
  defItem(FilterSimple_Noise) \
  defItem(FilterSimple_Noise_Grayscale) \
  defItem(FilterSimple_Noise_Grayscale_Subtractive) \
  defItem(FilterSimple_Adjust) \
  defItem(FilterSimple_Adjust_Channel) \
  defItem(FilterSimple_Adjust_RGB) \
  defItem(FilterSimple_Adjust_HSV) \
  defItem(FilterSimple_Box) \
  defItem(FilterSimple_Box_SourceAlpha) \
  defItem(FilterSimple_ColorFilter) \
  defItem(FilterSimple_Composite) \
  defItem(FilterSimple_Premultiply) \
  defItem(FilterSimple_PrepareNormals) \
  defItem(FilterSimple_ConvexPolygon) \
  defItem(FilterSimple_ConvexPolygon_Gradient) \
  defItem(FilterSimple_ConvexPolygon_Textured) \
  defItem(FilterSimple_ConvexPolygon_AntiAlias) \
  defItem(FilterSimple_ConvexPolygon_Textured_AntiAlias) \
  defItem(FilterSimple_Decay) \
  defItem(FilterSimple_Depalettize) \
  defItem(FilterSimple_Blur) \
  defItem(FilterSimple_Blur_Horizontal) \
  defItem(FilterSimple_Blur_Vertical) \
  defItem(FilterSimple_Fill) \
  defItem(FilterSimple_Fill_Opacity) \
  defItem(FilterSimple_Fill_Additive) \
  defItem(FilterSimple_Fill_Additive_Opacity) \
  defItem(FilterSimple_Fill_Channel) \
  defItem(FilterSimple_Fill_Subtractive) \
  defItem(FilterSimple_Fill_Subtractive_Opacity) \
  defItem(FilterSimple_Fill_SourceAlpha) \
  defItem(FilterSimple_Fill_SourceAlpha_Opacity) \
  defItem(FilterSimple_Flip) \
  defItem(FilterSimple_Gamma) \
  defItem(FilterSimple_Gamma_Channel) \
  defItem(FilterSimple_Gamma_RGB) \
  defItem(FilterSimple_Multiply) \
  defItem(FilterSimple_Multiply_Channel) \
  defItem(FilterSimple_Multiply_RGB) \
  defItem(FilterSimple_Gradient_4Point) \
  defItem(FilterSimple_Gradient_4Point_SourceAlpha) \
  defItem(FilterSimple_Gradient_Horizontal) \
  defItem(FilterSimple_Gradient_Horizontal_SourceAlpha) \
  defItem(FilterSimple_Gradient_Vertical) \
  defItem(FilterSimple_Gradient_Vertical_SourceAlpha) \
  defItem(FilterSimple_Gradient_Radial) \
  defItem(FilterSimple_Gradient_Radial_SourceAlpha) \
  defItem(FilterSimple_Grayscale) \
  defItem(FilterSimple_Grid_SourceAlpha) \
  defItem(FilterSimple_Invert) \
  defItem(FilterSimple_Invert_Channel) \
  defItem(FilterSimple_Invert_Color) \
  defItem(FilterSimple_Line) \
  defItem(FilterSimple_Line_Additive) \
  defItem(FilterSimple_Line_SourceAlpha) \
  defItem(FilterSimple_Line_Subtractive) \
  defItem(FilterSimple_Line_Gradient) \
  defItem(FilterSimple_Line_Gradient_SourceAlpha) \
  defItem(FilterSimple_Line_AA) \
  defItem(FilterSimple_Line_Gradient_AA) \
  defItem(FilterSimple_Mirror) \
  defItem(FilterSimple_Noise_Channel) \
  defItem(FilterSimple_Noise_Grayscale_Opacity) \
  defItem(FilterSimple_Noise_Grayscale_Subtractive_Opacity) \
  defItem(FilterSimple_Replace) \
  defItem(FilterSimple_Rotate90) \
  defItem(FilterSimple_Solarize) \
  defItem(FilterSimple_Swap_Channels) \
  defItem(FilterSimple_RenderStroke) \
  defItem(FilterSimple_) \
  defItem(BlitSimple_Additive) \
  defItem(BlitSimple_Additive_Opacity) \
  defItem(BlitSimple_Additive_SourceAlpha) \
  defItem(BlitSimple_Additive_SourceAlpha_Opacity) \
  defItem(BlitSimple_AND) \
  defItem(BlitSimple_BlitBehind) \
  defItem(BlitSimple_BlitBehind_Opacity) \
  defItem(BlitSimple_Burn) \
  defItem(BlitSimple_Burn_Opacity) \
  defItem(BlitSimple_Channel) \
  defItem(BlitSimple_Compare) \
  defItem(BlitSimple_Difference) \
  defItem(BlitSimple_Dither) \
  defItem(BlitSimple_Dither_Opacity) \
  defItem(BlitSimple_Dither_DualOpacity) \
  defItem(BlitSimple_Dodge) \
  defItem(BlitSimple_Dodge_Opacity) \
  defItem(BlitSimple_Erase) \
  defItem(BlitSimple_Erase_Opacity) \
  defItem(BlitSimple_Font) \
  defItem(BlitSimple_Font_Opacity) \
  defItem(BlitSimple_Font_Merge_RGB) \
  defItem(BlitSimple_Font_Merge_RGB_Opacity) \
  defItem(BlitSimple_Font_SourceAlpha) \
  defItem(BlitSimple_Font_SourceAlpha_Opacity) \
  defItem(BlitSimple_Font_SourceAlpha_RGB) \
  defItem(BlitSimple_Font_SourceAlpha_RGB_Opacity) \
  defItem(BlitSimple_Lightmap) \
  defItem(BlitSimple_Lightmap_Opacity) \
  defItem(BlitSimple_Lightmap_RGB) \
  defItem(BlitSimple_Lightmap_RGB_Opacity) \
  defItem(BlitSimple_Matte) \
  defItem(BlitSimple_Matte_Opacity) \
  defItem(BlitSimple_Matte_Tint) \
  defItem(BlitSimple_Matte_Tint_Opacity) \
  defItem(BlitSimple_Matte_Solid_Tint) \
  defItem(BlitSimple_Merge) \
  defItem(BlitSimple_Merge_Opacity) \
  defItem(BlitSimple_Multiply) \
  defItem(BlitSimple_Multiply_Opacity) \
  defItem(BlitSimple_Normal) \
  defItem(BlitSimple_Normal_Opacity) \
  defItem(BlitSimple_Normal_Gamma) \
  defItem(BlitSimple_Normal_Gamma_Opacity) \
  defItem(BlitSimple_Normal_Tint) \
  defItem(BlitSimple_Normal_Tint_Opacity) \
  defItem(BlitSimple_OR) \
  defItem(BlitSimple_Screen) \
  defItem(BlitSimple_Screen_Opacity) \
  defItem(BlitSimple_SourceAlpha) \
  defItem(BlitSimple_SourceAlpha_Opacity) \
  defItem(BlitSimple_SourceAlpha_Solid_Tint) \
  defItem(BlitSimple_SourceAlpha_Tint) \
  defItem(BlitSimple_SourceAlpha_Tint_Opacity) \
  defItem(BlitSimple_SourceAlpha_ColorMask) \
  defItem(BlitSimple_SourceAlphaMatte) \
  defItem(BlitSimple_SourceAlphaMatte_Opacity) \
  defItem(BlitSimple_SourceAlpha_Premultiplied) \
  defItem(BlitSimple_SourceAlpha_Premultiplied_Opacity) \
  defItem(BlitSimple_Subtractive) \
  defItem(BlitSimple_Subtractive_Opacity) \
  defItem(BlitSimple_Subtractive_SourceAlpha) \
  defItem(BlitSimple_Subtractive_SourceAlpha_Opacity) \
  defItem(BlitSimple_Unerase) \
  defItem(BlitSimple_Unerase_Opacity) \
  defItem(BlitSimple_XOR) \
  defItem(BlitSimple_NormalMap) \
  defItem(BlitSimple_NormalMap_Additive) \
  defItem(BlitSimple_NormalMap_SourceAlpha) \
  defItem(BlitSimple_NormalMap_Additive_SourceAlpha) \
  defItem(BlitSimple_) \
  defItem(BlitResample_Additive) \
  defItem(BlitResample_Additive_Opacity) \
  defItem(BlitResample_Lightmap) \
  defItem(BlitResample_Lightmap_RGB) \
  defItem(BlitResample_Merge) \
  defItem(BlitResample_Merge_Opacity) \
  defItem(BlitResample_Multiply) \
  defItem(BlitResample_Multiply_Opacity) \
  defItem(BlitResample_Normal) \
  defItem(BlitResample_Normal_Opacity) \
  defItem(BlitResample_SourceAlpha) \
  defItem(BlitResample_SourceAlpha_Opacity) \
  defItem(BlitResample_SourceAlpha_Tint) \
  defItem(BlitResample_SourceAlpha_Tint_Opacity) \
  defItem(BlitResample_Subtractive) \
  defItem(BlitResample_Subtractive_Opacity) \
  defItem(BlitResample_) \
  defItem(BlitMask_Normal_Opacity) \
  defItem(BlitMask_SourceAlpha_Opacity) \
  defItem(BlitMask_Merge_Opacity) \
  defItem(BlitMask_) \
  defItem(BlitConvolve) \
  defItem(BlitConvolveMask) \
  defItem(BlitDeform) \
  defItem(BlitDeformMask) \
  defItem(RenderTilemapLayer) \
  defItem(RenderWindow) \
  defItem(RenderText) \
  defItem(GetScanlineRenderer) \
/* OVERRIDES GO HERE */ \
  defEnd

/* CONSTANT LIST */
#ifndef OVERRIDE_ENUM
#define OVERRIDE_ENUM
#define defStart \
  const enum OverrideIndex { \
    _none,
#define defItem(n) \
    n,
#define defEnd \
    _count \
  };

namespace Override {
  defOverrides
};
#endif

#undef defStart
#undef defItem
#undef defEnd

#ifdef TRANSLATION_TABLE

/* TRANSLATION TABLE INITIALIZER */
#define defStart \
  template <class t1, class t2> inline void InitOverrideTranslationTable(t1& stiTable, t2* itsTable) {
#define defItem(n) \
    stiTable[#n] = Override::n; \
    itsTable[Override::n] = std::string(#n);
#define defEnd \
  }

defOverrides

#endif

#undef defStart
#undef defItem
#undef defEnd

#undef defOverrides
