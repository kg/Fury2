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

#define FSIGNATURE(name) \
    Export int FilterSimple_##name (Image *Dest, Rectangle *Area
#define END_FSIGNATURE );

FSIGNATURE(Fill) , Pixel Color END_FSIGNATURE
FSIGNATURE(Fill_Opacity) , Pixel Color, int Opacity END_FSIGNATURE
FSIGNATURE(Fill_SourceAlpha) , Pixel Color END_FSIGNATURE
FSIGNATURE(Fill_SourceAlpha_Opacity) , Pixel Color, int Opacity END_FSIGNATURE

FSIGNATURE(Gradient_4Point) , Pixel Color1, Pixel Color2, Pixel Color3, Pixel Color4 END_FSIGNATURE
FSIGNATURE(Gradient_4Point_SourceAlpha) , Pixel Color1, Pixel Color2, Pixel Color3, Pixel Color4 END_FSIGNATURE
FSIGNATURE(Gradient_4Edge) , ::Image *Edge1, ::Image *Edge2, ::Image *Edge3, ::Image *Edge4 END_FSIGNATURE

FSIGNATURE(Box) , Pixel Color END_FSIGNATURE

FSIGNATURE(Gradient_Radial) , Pixel Color1, Pixel Color2 END_FSIGNATURE

Export extern int FilterSimple_Line_AA(Image *Image, float X1, float Y1, float X2, float Y2, Pixel Color);
#ifdef _POLYGON_HPP_
Export extern int FilterSimple_ConvexPolygon(Image *Image, SimplePolygon *InPoly, Pixel Color, RenderFunction *Renderer, DoubleWord RenderArgument);
Export extern int FilterSimple_ConvexPolygon_Textured(Image *Dest, Image *Texture, TexturedPolygon *InPoly, ScalerFunction *Scaler, RenderFunction *Renderer, DoubleWord RenderArgument);
Export extern int FilterSimple_ConvexPolygon_Gradient(Image *Image, GradientPolygon *InPoly, RenderFunction *Renderer, DoubleWord RenderArgument);
extern void RenderFunction_SourceAlpha(Pixel *Dest, Pixel *Source, int Count, Pixel SolidColor, DoubleWord Argument);
extern void RenderFunction_Merge(Pixel *Dest, Pixel *Source, int Count, Pixel SolidColor, DoubleWord Argument);
extern void RenderFunction_Additive(Pixel *Dest, Pixel *Source, int Count, Pixel SolidColor, DoubleWord Argument);
extern void RenderFunction_Subtractive(Pixel *Dest, Pixel *Source, int Count, Pixel SolidColor, DoubleWord Argument);
extern void RenderFunction_Shadow(Pixel *Dest, Pixel *Source, int Count, Pixel SolidColor, DoubleWord Argument);
extern void RenderFunction_Screen(Pixel *Dest, Pixel *Source, int Count, Pixel SolidColor, DoubleWord Argument);
#endif
