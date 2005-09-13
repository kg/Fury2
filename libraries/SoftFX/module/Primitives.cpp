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
#include "../header/Filters.hpp"
#include "../header/Gradients.hpp"
#include "../header/Primitives.hpp"
#include "../header/Polygon.hpp"
#include "../header/Blitters.hpp"

enum csRegions {
    csNone        = 0,
    csTop         = 1,
    csBottom      = 2,
    csRight       = 4,
    csLeft        = 8,
    csAll         = 15
};

Export int GetPolygonVertexCount(SimplePolygon* Poly) {
  if (Poly) {
    return Poly->Count();
  } else {
    return 0;
  }
}

Export int GetGradientPolygonVertexCount(GradientPolygon* Poly) {
  if (Poly) {
    return Poly->Count();
  } else {
    return 0;
  }
}

Export int GetTexturedPolygonVertexCount(TexturedPolygon* Poly) {
  if (Poly) {
    return Poly->Count();
  } else {
    return 0;
  }
}

Export void* GetPolygonVertexPointer(SimplePolygon* Poly, int Vertex) {
  if (Poly) {
    return Poly->Vertexes + Vertex;
  } else {
    return 0;
  }
}

Export void* GetGradientPolygonVertexPointer(GradientPolygon* Poly, int Vertex) {
  if (Poly) {
    return Poly->Vertexes + Vertex;
  } else {
    return 0;
  }
}

Export void* GetTexturedPolygonVertexPointer(TexturedPolygon* Poly, int Vertex) {
  if (Poly) {
    return Poly->Vertexes + Vertex;
  } else {
    return 0;
  }
}

int PointRegionCode(FPoint *pt, Rectangle *rgn) {
int value = 0;
    if (pt->X < rgn->Left) {
        value |= csLeft;
    }
    else if (pt->X > rgn->right_exclusive()) {
        value |= csRight;
    }
    if (pt->Y < rgn->Top) {
        value |= csBottom;
    }
    else if (pt->Y > rgn->bottom_exclusive()) {
        value |= csTop;
    }
    return value;
}

int ClipLine(Rectangle *Rect, ILine *Line) {
int c[2], C;
FLine fLine = FLine(Line);
FPoint *pcur[2];
float x, y;
float xb[2], yb[2];
    if (!Rect) return 0;
    if (!Line) return 0;
    pcur[0] = &(fLine.Start);
    pcur[1] = &(fLine.End);
    xb[0] = Rect->Left;
    xb[1] = Rect->right_exclusive();
    yb[0] = Rect->Top;
    yb[1] = Rect->bottom_exclusive();
    c[0] = PointRegionCode(&(fLine.Start), Rect);
    c[1] = PointRegionCode(&(fLine.End), Rect);
    while(true) {
        if ((c[0] | c[1]) == 0) {
            // trivial accept
            *Line = ILine(&fLine);
            return 1;
        } else if ((c[0] & c[1]) != 0) {
            // trivial reject
            *Line = ILine(&fLine);
            return 0;
        } else {
            C = c[0] ? c[0] : c[1];

            // nontrivial
            if ((C & csTop) == csTop) {
                // top
                x = pcur[0]->X + ((pcur[1]->X - pcur[0]->X) * (yb[1] - pcur[0]->Y) / (pcur[1]->Y - pcur[0]->Y));
                y = yb[1];
            } else if ((C & csBottom) == csBottom) {
                // bottom
                x = pcur[0]->X + ((pcur[1]->X - pcur[0]->X) * (yb[0] - pcur[0]->Y) / (pcur[1]->Y - pcur[0]->Y));
                y = yb[0];
            } else if ((C & csRight) == csRight) {
                // right
                x = xb[1];
                y = pcur[0]->Y + ((pcur[1]->Y - pcur[0]->Y) * (xb[1] - pcur[0]->X) / (pcur[1]->X - pcur[0]->X));
            } else {
                // left
                x = xb[0];
                y = pcur[0]->Y + ((pcur[1]->Y - pcur[0]->Y) * (xb[0] - pcur[0]->X) / (pcur[1]->X - pcur[0]->X));
            }
            if (C == c[0]) {
                fLine.Start.X = x;
                fLine.Start.Y = y;
                c[0] = PointRegionCode(&(fLine.Start), Rect);
            } else {
                fLine.End.X = x;
                fLine.End.Y = y;
                c[1] = PointRegionCode(&(fLine.End), Rect);
            }
        }
    }
}

Export int ClipLine(Image *Image, ILine *Line) {
  if (!Image) return 0;
  return ClipLine(&(Image->ClipRectangle), Line);
}

int ClipFloatLine(Rectangle *Rect, FLine *Line) {
int c[2], C;
FLine fLine = *Line;
FPoint *pcur[2];
float x, y;
float xb[2], yb[2];
    if (!Rect) return 0;
    if (!Line) return 0;
    pcur[0] = &(fLine.Start);
    pcur[1] = &(fLine.End);
    xb[0] = Rect->Left;
    xb[1] = Rect->right_exclusive();
    yb[0] = Rect->Top;
    yb[1] = Rect->bottom_exclusive();
    c[0] = PointRegionCode(&(fLine.Start), Rect);
    c[1] = PointRegionCode(&(fLine.End), Rect);
    while(true) {
        if ((c[0] | c[1]) == 0) {
            // trivial accept
            *Line = fLine;
            return 1;
        } else if ((c[0] & c[1]) != 0) {
            // trivial reject
            *Line = fLine;
            return 0;
        } else {
            C = c[0] ? c[0] : c[1];

            // nontrivial
            if ((C & csTop) == csTop) {
                // top
                x = pcur[0]->X + ((pcur[1]->X - pcur[0]->X) * (yb[1] - pcur[0]->Y) / (pcur[1]->Y - pcur[0]->Y));
                y = yb[1];
            } else if ((C & csBottom) == csBottom) {
                // bottom
                x = pcur[0]->X + ((pcur[1]->X - pcur[0]->X) * (yb[0] - pcur[0]->Y) / (pcur[1]->Y - pcur[0]->Y));
                y = yb[0];
            } else if ((C & csRight) == csRight) {
                // right
                x = xb[1];
                y = pcur[0]->Y + ((pcur[1]->Y - pcur[0]->Y) * (xb[1] - pcur[0]->X) / (pcur[1]->X - pcur[0]->X));
            } else {
                // left
                x = xb[0];
                y = pcur[0]->Y + ((pcur[1]->Y - pcur[0]->Y) * (xb[0] - pcur[0]->X) / (pcur[1]->X - pcur[0]->X));
            }
            if (C == c[0]) {
                fLine.Start.X = x;
                fLine.Start.Y = y;
                c[0] = PointRegionCode(&(fLine.Start), Rect);
            } else {
                fLine.End.X = x;
                fLine.End.Y = y;
                c[1] = PointRegionCode(&(fLine.End), Rect);
            }
        }
    }
}

Export int ClipFloatLine(Image *Image, FLine *Line) {
  if (!Image) return 0;
  return ClipFloatLine(&(Image->ClipRectangle), Line);
}

Export int PointInsidePolygon(Polygon<FPoint> *Poly, FPoint *Point) {
	FLine edge, trace;
	FPoint who_cares;
	trace.Start = trace.End = *Point;
	trace.End.X = 9999999;
	int c = 0;
	for (int i = 0; i < Poly->VertexCount; i++)
	{
		edge.Start = Poly->Vertexes[i];
		edge.End = Poly->Vertexes[WrapValue(i + 1, 0, Poly->VertexCount - 1)];
		if (trace.intersect(edge, who_cares)) {
			c++;
		}
	}
	return (c % 2) == 1;
}

Export int FilterSimple_Multiple_Line_AA(Image *Image, FLine *Lines, Pixel Color, int Count, float XOffset, float YOffset) {
  if (!Image) return Failure;
  if (!Lines) return Failure;
  if (Count < 1) return Failure;
  FLine Line;
  for (int l = 0; l < Count; l++) {
    Line = Lines[l];
    Line.Start.X -= XOffset;
    Line.Start.Y -= YOffset;
    Line.End.X -= XOffset;
    Line.End.Y -= YOffset;
    if (ClipFloatLine(&(Image->ClipRectangle), &Line)) {
      int x_distance = Line.End.X - Line.Start.X, y_distance = Line.End.Y - Line.Start.Y;
      int pixel_count = (_Max(abs(x_distance), abs(y_distance)));
      float x_increment = (x_distance / (float)pixel_count), y_increment = (y_distance / (float)pixel_count);
      float current_x = Line.Start.X, current_y = Line.Start.Y;
      for (int i = 0; i <= pixel_count; i++) {
        Image->setPixelAA(current_x, current_y, Color);
        current_x += x_increment;
        current_y += y_increment;
      }
    }
  }
  return Success;
}

Export int FilterSimple_Line_AA(Image *Image, float X1, float Y1, float X2, float Y2, Pixel Color) {
  if (!Image) return Failure;
  {
    int overrideresult = Override::EnumOverrides(Override::FilterSimple_Line_AA, 6, Image, va_float(X1), va_float(Y1), va_float(X2), va_float(Y2), Color);
    #ifdef OVERRIDES
      if (overrideresult != 0) return overrideresult;
    #endif
  }
  ImageLockManager ilImage(lockingMode, Image);
  if (!ilImage.performUnlock()) {
      return Failure;
  }
  FLine Line;
  Line.Start.X = X1;
  Line.Start.Y = Y1;
  Line.End.X = X2;
  Line.End.Y = Y2;
  if (ClipFloatLine(&(Image->ClipRectangle), &Line)) {
    int x_distance = X2 - X1, y_distance = Y2 - Y1;
    int pixel_count = (_Max(abs(x_distance), abs(y_distance)));
    float x_increment = ((X2 - X1) / (float)pixel_count), y_increment = ((Y2 - Y1) / (float)pixel_count);
    float current_x = X1, current_y = Y1;
    for (int i = 0; i <= pixel_count; i++) {
      Image->setPixelAA(current_x, current_y, Color);
      current_x += x_increment;
      current_y += y_increment;
    }
    return Success;
  } else {
    return Trivial_Success;
  }
}

Export int FilterSimple_Line_Gradient_AA(Image *Image, float X1, float Y1, float X2, float Y2, Pixel StartColor, Pixel EndColor) {
  if (!Image) return Failure;
  {
    int overrideresult = Override::EnumOverrides(Override::FilterSimple_Line_Gradient_AA, 7, Image, va_float(X1), va_float(Y1), va_float(X2), va_float(Y2), StartColor, EndColor);
    #ifdef OVERRIDES
      if (overrideresult != 0) return overrideresult;
    #endif
  }
  ImageLockManager ilImage(lockingMode, Image);
  if (!ilImage.performUnlock()) {
      return Failure;
  }
  FLine Line;
  Line.Start.X = X1;
  Line.Start.Y = Y1;
  Line.End.X = X2;
  Line.End.Y = Y2;
  if (ClipFloatLine(&(Image->ClipRectangle), &Line)) {
    int x_distance = X2 - X1, y_distance = Y2 - Y1;
    int pixel_count = (_Max(abs(x_distance), abs(y_distance)));
    float x_increment = ((X2 - X1) / (float)pixel_count), y_increment = ((Y2 - Y1) / (float)pixel_count);
    float w_increment = (255.0f / (float)pixel_count);
    float w = 0;
    float current_x = X1, current_y = Y1;
    for (int i = 0; i <= pixel_count; i++) {
      Image->setPixelAA(current_x, current_y, Pixel(StartColor, EndColor, w));
      current_x += x_increment;
      current_y += y_increment;
      w += w_increment;
    }
    return Success;
  } else {
    return Trivial_Success;
  }
}

#define CircleSetPixel(x, y) i = y - ystart; \
if ((i >= 0) && (i < tableLength)) { \
  if (x < table[i].S) table[i].S = x; \
  if (x > table[i].E) table[i].E = x; \
}

Export int FilterSimple_FilledCircle(Image *Image, int X, int Y, int Radius, Pixel Color) {
  if (!Image) {
      return Failure;
  }
  if (!Image->initialized()) {
      return Failure;
  }
  {
    int overrideresult = Override::EnumOverrides(Override::FilterSimple_FilledCircle, 5, Image, X, Y, Radius, Color);
    #ifdef OVERRIDES
      if (overrideresult != 0) return overrideresult;
    #endif
  }
  ImageLockManager ilImage(lockingMode, Image);
  if (!ilImage.performUnlock()) {
      return Failure;
  }
  int d = 3 - (2 * Radius);
  int ox = 0, oy = Radius, w = 0;

  ISpan *table = Null;
  Pixel *ptr = Null;
  int tableLength = (Radius * 2) + 2;
  table = LookupAllocate<ISpan>(tableLength);
  int ystart = Y - Radius, i = 0, yend = ystart + tableLength;
  if ((yend - ystart + 1) > tableLength) {
    yend = (ystart + tableLength - 1);
  }

  for (i = 0; i < tableLength; ++i) {
    table[i].S = 999999;
    table[i].E = 0;
  }

  while (ox <= oy) {
    CircleSetPixel(X + ox, Y + oy);
    CircleSetPixel(X - ox, Y + oy);
    CircleSetPixel(X + ox, Y - oy);
    CircleSetPixel(X - ox, Y - oy);
    CircleSetPixel(X + oy, Y + ox);
    CircleSetPixel(X - oy, Y + ox);
    CircleSetPixel(X + oy, Y - ox);
    CircleSetPixel(X - oy, Y - ox);
    if (d < 0) {
      d += 4 * ox + 6;
    } else {
      d += 4 * ( ox - oy ) + 10;
      oy--;
    }
    ox++;
  }


  for (i = 0; i < tableLength; ++i) {
    if (table[i].E >= table[i].S) {
      w = (table[i].E - table[i].S) + 1;
      ox = table[i].S;
      if (ox < Image->ClipRectangle.Left) {
        w -= (Image->ClipRectangle.Left - ox);
        ox = Image->ClipRectangle.Left;
      }
      if (ox >= Image->ClipRectangle.right()) {
        w = 0;
      } else if ((ox + w) >= Image->ClipRectangle.right()) {
        w -= ((ox + w) - Image->ClipRectangle.right());
      }
      if (w) {
        if (((i + ystart) >= Image->ClipRectangle.Top) && ((i + ystart) < Image->ClipRectangle.bottom())) {
          ptr = Image->fast_pointer(ox, i + ystart);
          while (w--) {
            *ptr = Color;
            ++ptr;
          }
        }
      }
    }
  }

  LookupDeallocate<ISpan>(table);
  return Success;
}

#undef CircleSetPixel

#define CircleSetPixel(x, y) i = y - ystart; \
if ((i >= 0) && (i < tableLength)) { \
  if (x < table[i].S) table[i].S = x; \
  if (x > table[i].E) table[i].E = x; \
}

Export int FilterSimple_FilledCircle_SourceAlpha(Image *Image, int X, int Y, int Radius, Pixel Color) {
  if (!Image) {
      return Failure;
  }
  if (!Image->initialized()) {
      return Failure;
  }
  if (Color[::Alpha] == 255) return FilterSimple_FilledCircle(Image, X, Y, Radius, Color);
  if (Color[::Alpha] == 0) return Trivial_Success;
  {
    int overrideresult = Override::EnumOverrides(Override::FilterSimple_FilledCircle_SourceAlpha, 5, Image, X, Y, Radius, Color);
    #ifdef OVERRIDES
      if (overrideresult != 0) return overrideresult;
    #endif
  }
  ImageLockManager ilImage(lockingMode, Image);
  if (!ilImage.performUnlock()) {
      return Failure;
  }
  int d = 3 - (2 * Radius);
  int ox = 0, oy = Radius, w = 0;

  ISpan *table = Null;
  Pixel *ptr = Null;
  int tableLength = (Radius * 2) + 2;
  table = LookupAllocate<ISpan>(tableLength);
  int ystart = Y - Radius, i = 0, yend = ystart + tableLength;
  if ((yend - ystart + 1) > tableLength) {
    yend = (ystart + tableLength - 1);
  }
  tableLength = (yend - ystart + 1);

  for (i = 0; i < tableLength; ++i) {
    table[i].S = 999999;
    table[i].E = 0;
  }

  while (ox <= oy) {
    CircleSetPixel(X + ox, Y + oy);
    CircleSetPixel(X - ox, Y + oy);
    CircleSetPixel(X + ox, Y - oy);
    CircleSetPixel(X - ox, Y - oy);
    CircleSetPixel(X + oy, Y + ox);
    CircleSetPixel(X - oy, Y + ox);
    CircleSetPixel(X + oy, Y - ox);
    CircleSetPixel(X - oy, Y - ox);
    if (d < 0) {
      d += 4 * ox + 6;
    } else {
      d += 4 * ( ox - oy ) + 10;
      oy--;
    }
    ox++;
  }

  AlphaLevel *aSource = AlphaLevelLookup( Color[::Alpha] );
  AlphaLevel *aDest = AlphaLevelLookup( Color[::Alpha] ^ 0xFF );
  Pixel *pColor = &Color;
  for (i = 0; i < tableLength; ++i) {
    if (table[i].E >= table[i].S) {
      w = (table[i].E - table[i].S) + 1;
      ox = table[i].S;
      if (ox < Image->ClipRectangle.Left) {
        w -= (Image->ClipRectangle.Left - ox);
        ox = Image->ClipRectangle.Left;
      }
      if (ox >= Image->ClipRectangle.right()) {
        w = 0;
      } else if ((ox + w) >= Image->ClipRectangle.right()) {
        w -= ((ox + w) - Image->ClipRectangle.right());
      }
      if (w) {
        if (((i + ystart) >= Image->ClipRectangle.Top) && ((i + ystart) < Image->ClipRectangle.bottom())) {
          ptr = Image->fast_pointer(ox, i + ystart);
          while (w--) {
            BLENDPIXEL_ALPHA_OPACITY(ptr, ptr, pColor, aDest, aSource);
            ++ptr;
          }
        }
      }
    }
  }

  LookupDeallocate<ISpan>(table);
  return Success;
}

#undef CircleSetPixel

#define CircleSetPixel(x, y) if ((x >= Image->ClipRectangle.Left) && (y >= Image->ClipRectangle.Top) && (x < Image->ClipRectangle.right()) && (y < Image->ClipRectangle.bottom())) { \
  Image->setPixelFast(x, y, Color); \
}

Export int FilterSimple_Circle(Image *Image, int X, int Y, int Radius, Pixel Color) {
  if (!Image) {
      return Failure;
  }
  if (!Image->initialized()) {
      return Failure;
  }
  {
    int overrideresult = Override::EnumOverrides(Override::FilterSimple_Circle, 5, Image, X, Y, Radius, Color);
    #ifdef OVERRIDES
      if (overrideresult != 0) return overrideresult;
    #endif
  }
  ImageLockManager ilImage(lockingMode, Image);
  if (!ilImage.performUnlock()) {
      return Failure;
  }
  int d = 3 - (2 * Radius);
  int ox = 0, oy = Radius;

  while (ox <= oy) {
    CircleSetPixel(X + ox, Y + oy);
    CircleSetPixel(X - ox, Y + oy);
    CircleSetPixel(X + ox, Y - oy);
    CircleSetPixel(X - ox, Y - oy);
    CircleSetPixel(X + oy, Y + ox);
    CircleSetPixel(X - oy, Y + ox);
    CircleSetPixel(X + oy, Y - ox);
    CircleSetPixel(X - oy, Y - ox);
    if (d < 0) {
      d += 4 * ox + 6;
    } else {
      d += 4 * ( ox - oy ) + 10;
      oy--;
    }
    ox++;
  }
  return Success;
}

#undef CircleSetPixel

#define CircleSetPixel(x, y) if ((x >= Image->ClipRectangle.Left) && (y >= Image->ClipRectangle.Top) && (x < Image->ClipRectangle.right()) && (y < Image->ClipRectangle.bottom())) { \
  pCurrent = Image->fast_pointer(x, y); \
  BLENDPIXEL_ALPHA_OPACITY(pCurrent, pCurrent, pColor, aDest, aSource); \
}

Export int FilterSimple_Circle_SourceAlpha(Image *Image, int X, int Y, int Radius, Pixel Color) {
  if (!Image) {
      return Failure;
  }
  if (!Image->initialized()) {
      return Failure;
  }
  if (Color[::Alpha] == 255) return FilterSimple_Circle(Image, X, Y, Radius, Color);
  if (Color[::Alpha] == 0) return Trivial_Success;
  {
    int overrideresult = Override::EnumOverrides(Override::FilterSimple_Circle_SourceAlpha, 5, Image, X, Y, Radius, Color);
    #ifdef OVERRIDES
      if (overrideresult != 0) return overrideresult;
    #endif
  }
  ImageLockManager ilImage(lockingMode, Image);
  if (!ilImage.performUnlock()) {
      return Failure;
  }
  int d = 3 - (2 * Radius);
  int ox = 0, oy = Radius;

  AlphaLevel *aSource = AlphaLevelLookup( Color[::Alpha] );
  AlphaLevel *aDest = AlphaLevelLookup( Color[::Alpha] ^ 0xFF );
  Pixel *pColor = &Color, *pCurrent = Null;
  while (ox <= oy) {
    CircleSetPixel(X + ox, Y + oy);
    CircleSetPixel(X - ox, Y + oy);
    CircleSetPixel(X + ox, Y - oy);
    CircleSetPixel(X - ox, Y - oy);
    CircleSetPixel(X + oy, Y + ox);
    CircleSetPixel(X - oy, Y + ox);
    CircleSetPixel(X + oy, Y - ox);
    CircleSetPixel(X - oy, Y - ox);
    if (d < 0) {
      d += 4 * ox + 6;
    } else {
      d += 4 * ( ox - oy ) + 10;
      oy--;
    }
    ox++;
  }
  return Success;
}

#undef CircleSetPixel

FILTERSIMPLE_SIGNATURE(Line)
    , Pixel Color) {
FILTERSIMPLE_INITPRIMITIVE
    _FOS(FilterSimple_Line, 1) , Color _FOE
FILTERSIMPLE_BEGINPRIMITIVE

    ILine Line = ILine(&rCoordinates);

    DRAWLINE_INIT(Image, Line)
    DRAWLINE_BEGIN
        *pCurrent = Color;
    DRAWLINE_END

FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Line_Additive)
    , Pixel Color) {
FILTERSIMPLE_INITPRIMITIVE
    _FOS(FilterSimple_Line_Additive, 1) , Color _FOE
FILTERSIMPLE_BEGINPRIMITIVE

    ILine Line = ILine(&rCoordinates);

    DRAWLINE_INIT(Image, Line)
        Pixel *pColor = &Color;
    DRAWLINE_BEGIN
        BLENDPIXEL_ADDITIVE(pCurrent, pCurrent, pColor)
    DRAWLINE_END

FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Line_Subtractive)
    , Pixel Color) {
FILTERSIMPLE_INITPRIMITIVE
    _FOS(FilterSimple_Line_Subtractive, 1) , Color _FOE
FILTERSIMPLE_BEGINPRIMITIVE

    ILine Line = ILine(&rCoordinates);

    DRAWLINE_INIT(Image, Line)
        Pixel *pColor = &Color;
    DRAWLINE_BEGIN
        BLENDPIXEL_SUBTRACTIVE(pCurrent, pCurrent, pColor)
    DRAWLINE_END

FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Line_SourceAlpha)
    , Pixel Color) {
FILTERSIMPLE_INITPRIMITIVE
    _FOS(FilterSimple_Line_SourceAlpha, 1) , Color _FOE
FILTERSIMPLE_BEGINPRIMITIVE

    ILine Line = ILine(&rCoordinates);

    DRAWLINE_INIT(Image, Line)
        AlphaLevel *aSource = AlphaLevelLookup( Color[::Alpha] );
        AlphaLevel *aDest = AlphaLevelLookup( Color[::Alpha] ^ 0xFF );
        Pixel *pColor = &Color;
    DRAWLINE_BEGIN
        BLENDPIXEL_ALPHA_OPACITY(pCurrent, pCurrent, pColor, aDest, aSource)
    DRAWLINE_END

FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Line_Gradient)
    , Pixel StartColor, Pixel EndColor) {
FILTERSIMPLE_INITPRIMITIVE
    _FOS(FilterSimple_Line_Gradient, 2) , StartColor, EndColor _FOE
FILTERSIMPLE_BEGINPRIMITIVE

    ILine Line = ILine(&rCoordinates);

    Pixel *pColorTable = Null;

    DRAWLINE_INIT(Image, Line)
        int iLength = _Max(abs(rCoordinates.Width), abs(rCoordinates.Height)) + 1;
        int iXOffset = (Line.Start.X > rCoordinates.Left) ? Line.Start.X - rCoordinates.Left : 0;
        int iYOffset = (Line.Start.Y > rCoordinates.Top) ? Line.Start.Y - rCoordinates.Top : 0;
        int iOffset = ClipValue(abs(_Distance<int>(iXOffset, iYOffset)), 0, iLength);
        iXOffset = (Line.Start.X < rCoordinates.Left) ? rCoordinates.Left - Line.Start.X : 0;
        iYOffset = (Line.Start.Y < rCoordinates.Top) ? rCoordinates.Top - Line.Start.Y : 0;
        iLength -= ClipValue(abs(_Distance<int>(iXOffset, iYOffset)), 0, iLength - numpixels);
        pColorTable = GenerateGradientTable(StartColor, EndColor, iLength, iOffset);
    DRAWLINE_BEGIN
        *pCurrent = pColorTable[i];
    DRAWLINE_END

    LookupDeallocate(pColorTable);

FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Line_Gradient_SourceAlpha)
    , Pixel StartColor, Pixel EndColor) {
FILTERSIMPLE_INITPRIMITIVE
    _FOS(FilterSimple_Line_Gradient_SourceAlpha, 2) , StartColor, EndColor _FOE
FILTERSIMPLE_BEGINPRIMITIVE

    ILine Line = ILine(&rCoordinates);

    Pixel *pColorTable = Null;

    DRAWLINE_INIT(Image, Line)
        int iLength = _Max(abs(rCoordinates.Width), abs(rCoordinates.Height)) + 1;
        if (iLength == 1) {
          Image->setPixel(Line.Start.X, Line.Start.Y, EndColor);
          return Success;
        }
        if (iLength < 1) return Trivial_Success;
        int iXOffset = (Line.Start.X > rCoordinates.Left) ? Line.Start.X - rCoordinates.Left : 0;
        int iYOffset = (Line.Start.Y > rCoordinates.Top) ? Line.Start.Y - rCoordinates.Top : 0;
        int iOffset = ClipValue(abs(_Distance<int>(iXOffset, iYOffset)), 0, iLength);
        iXOffset = (Line.Start.X < rCoordinates.Left) ? rCoordinates.Left - Line.Start.X : 0;
        iYOffset = (Line.Start.Y < rCoordinates.Top) ? rCoordinates.Top - Line.Start.Y : 0;
        iLength -= ClipValue(abs(_Distance<int>(iXOffset, iYOffset)), 0, iLength - numpixels);
        pColorTable = GenerateGradientTable(StartColor, EndColor, iLength, iOffset);
        AlphaLevel *aDest;
    DRAWLINE_BEGIN
        aDest = AlphaLevelLookup( pColorTable[i][::Alpha] );
        BLENDPIXEL_ALPHA_PREMULTIPLIED_OPACITY(pCurrent, pCurrent, &pColorTable[i], aDest)
    DRAWLINE_END

    LookupDeallocate(pColorTable);

FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Box_SourceAlpha)
    , Pixel Color) {
FILTERSIMPLE_INITPRIMITIVE
    _FOS(FilterSimple_Box_SourceAlpha, 1) , Color _FOE
FILTERSIMPLE_BEGINPRIMITIVE
Rectangle rLine;

    if ((rCoordinates.Width == 0) || (rCoordinates.Height == 0)) return Trivial_Success;

    rLine.setValues(rCoordinates.Left, rCoordinates.Top, rCoordinates.Width - 1, 0);
    FilterSimple_Line_SourceAlpha(Image, &rLine, Color);
    rLine.setValues(rCoordinates.Left, rCoordinates.bottom_exclusive(), rCoordinates.Width - 1, 0);
    FilterSimple_Line_SourceAlpha(Image, &rLine, Color);
    rLine.setValues(rCoordinates.Left, rCoordinates.Top + 1, 0, rCoordinates.Height - 3);
    FilterSimple_Line_SourceAlpha(Image, &rLine, Color);
    rLine.setValues(rCoordinates.right_exclusive(), rCoordinates.Top + 1, 0, rCoordinates.Height - 3);
    FilterSimple_Line_SourceAlpha(Image, &rLine, Color);

FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Box)
    , Pixel Color) {
FILTERSIMPLE_INITPRIMITIVE
    _FOS(FilterSimple_Box, 1) , Color _FOE
FILTERSIMPLE_BEGINPRIMITIVE
Rectangle rLine;

    if ((rCoordinates.Width == 0) || (rCoordinates.Height == 0)) return Trivial_Success;

    rLine.setValues(rCoordinates.Left, rCoordinates.Top, rCoordinates.Width - 1, 0);
    FilterSimple_Line(Image, &rLine, Color);
    rLine.setValues(rCoordinates.Left, rCoordinates.bottom_exclusive(), rCoordinates.Width - 1, 0);
    FilterSimple_Line(Image, &rLine, Color);
    rLine.setValues(rCoordinates.Left, rCoordinates.Top + 1, 0, rCoordinates.Height - 3);
    FilterSimple_Line(Image, &rLine, Color);
    rLine.setValues(rCoordinates.right_exclusive(), rCoordinates.Top + 1, 0, rCoordinates.Height - 3);
    FilterSimple_Line(Image, &rLine, Color);

FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Gradient_Horizontal)
    , Pixel Color1, Pixel Color2) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Gradient_Horizontal, 2) , Color1, Color2 _FOE
FILTERSIMPLE_BEGIN

    Byte* table = StaticAllocate<Byte>(XBuffer, rCoordinates.Width);
    for (int x = 0; x < rCoordinates.Width; x++) {
        table[x] = (((rCoordinates.Width - x - 1) + CropOffsets[0]) * 255) / (Area->Width);
    }

    AlphaLevel *aColor1, *aColor2;

FILTERSIMPLE_ROW
FILTERSIMPLE_COL
        aColor1 = AlphaLevelLookup(table[iCX]);
        aColor2 = AlphaLevelLookup(table[iCX] ^ 0xFF);
        (*pCurrent)[::Alpha] = AlphaFromLevel2(aColor1, Color1[::Alpha], aColor2, Color2[::Alpha]);
        (*pCurrent)[::Blue] = AlphaFromLevel2(aColor1, Color1[::Blue], aColor2, Color2[::Blue]);
        (*pCurrent)[::Green] = AlphaFromLevel2(aColor1, Color1[::Green], aColor2, Color2[::Green]);
        (*pCurrent)[::Red] = AlphaFromLevel2(aColor1, Color1[::Red], aColor2, Color2[::Red]);
FILTERSIMPLE_COLEND
FILTERSIMPLE_ROWEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Gradient_Vertical)
    , Pixel Color1, Pixel Color2) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Gradient_Vertical, 2) , Color1, Color2 _FOE
FILTERSIMPLE_BEGIN

    Byte* table = StaticAllocate<Byte>(XBuffer, rCoordinates.Height);
    for (int y = 0; y < rCoordinates.Height; y++) {
        table[y] = (((rCoordinates.Height - y - 1) + CropOffsets[1]) * 255) / (Area->Height);
    }

    AlphaLevel *aColor1, *aColor2;
    Pixel color;

FILTERSIMPLE_ROW
        aColor1 = AlphaLevelLookup(table[iCY]);
        aColor2 = AlphaLevelLookup(table[iCY] ^ 0xFF);
        color[::Alpha] = AlphaFromLevel2(aColor1, Color1[::Alpha], aColor2, Color2[::Alpha]);
        color[::Blue] = AlphaFromLevel2(aColor1, Color1[::Blue], aColor2, Color2[::Blue]);
        color[::Green] = AlphaFromLevel2(aColor1, Color1[::Green], aColor2, Color2[::Green]);
        color[::Red] = AlphaFromLevel2(aColor1, Color1[::Red], aColor2, Color2[::Red]);
FILTERSIMPLE_COL
        pCurrent->V = color.V;
FILTERSIMPLE_COLEND
FILTERSIMPLE_ROWEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Gradient_4Point)
    , Pixel Color1, Pixel Color2, Pixel Color3, Pixel Color4) {
FILTERSIMPLE_INIT    
    // just in case, 'cause it's faster and all
    if ((Color1.V == Color2.V) && (Color3.V == Color4.V) && (Color3.V == Color1.V) && (Color4.V == Color2.V)) return FilterSimple_Fill(Image, Area, Color1);
    if ((Color1.V == Color2.V) && (Color3.V == Color4.V)) return FilterSimple_Gradient_Vertical(Image, Area, Color3, Color1);
    if ((Color1.V == Color3.V) && (Color2.V == Color4.V)) return FilterSimple_Gradient_Horizontal(Image, Area, Color2, Color1);
    _FOS(FilterSimple_Gradient_4Point, 4) , Color1, Color2, Color3, Color4 _FOE
FILTERSIMPLE_BEGIN

    AlphaLevel *aColor1, *aColor2, *aColor3, *aColor4;
    Byte *xTable, *yTable;
    Pixel color;
    int xWeight, yWeight, xWeight2, yWeight2;
    int weights[4];

    // allocate two lookup tables
    xTable = StaticAllocate<Byte>(XBuffer, rCoordinates.Width);
    yTable = StaticAllocate<Byte>(YBuffer, rCoordinates.Height);

    // fill 'em
    for (int x = 0; x < rCoordinates.Width; x++) {
        xTable[x] = (((rCoordinates.Width - x - 1) + CropOffsets[0]) * 255) / (Area->Width);
    }
    for (int y = 0; y < rCoordinates.Height; y++) {
        yTable[y] = (((rCoordinates.Height - y - 1) + CropOffsets[1]) * 255) / (Area->Height);
    }

FILTERSIMPLE_ROW
    // calculate the weight/opposite weight for this row
    yWeight2 = yTable[iCY];
    yWeight = yWeight2 ^ 0xFF;
FILTERSIMPLE_COL
    // calculate the weight/opposite weight for this column
    xWeight2 = xTable[iCX];
    xWeight = xWeight2 ^ 0xFF;
    // calculate the four weights for the source colors
    weights[0] = (xWeight * yWeight) / 255;
    weights[1] = (xWeight2 * yWeight) / 255;
    weights[2] = (xWeight * yWeight2) / 255;
    weights[3] = (weights[0] + weights[1] + weights[2]) ^ 0xFF;
    // retrieve the lookup pointers
    aColor1 = AlphaLevelLookup( weights[0] );
    aColor2 = AlphaLevelLookup( weights[1] );
    aColor3 = AlphaLevelLookup( weights[2] );
    aColor4 = AlphaLevelLookup( weights[3] );
    color[::Alpha] = AlphaFromLevel(aColor1, Color1[::Alpha]) + AlphaFromLevel(aColor2, Color2[::Alpha]) + AlphaFromLevel(aColor3, Color3[::Alpha]) + AlphaFromLevel(aColor4, Color4[::Alpha]);
    color[::Blue] = (AlphaFromLevel(aColor1, Color1[::Blue]) + AlphaFromLevel(aColor2, Color2[::Blue]) + AlphaFromLevel(aColor3, Color3[::Blue]) + AlphaFromLevel(aColor4, Color4[::Blue]));
    color[::Green] = (AlphaFromLevel(aColor1, Color1[::Green]) + AlphaFromLevel(aColor2, Color2[::Green]) + AlphaFromLevel(aColor3, Color3[::Green]) + AlphaFromLevel(aColor4, Color4[::Green]));
    color[::Red] = (AlphaFromLevel(aColor1, Color1[::Red]) + AlphaFromLevel(aColor2, Color2[::Red]) + AlphaFromLevel(aColor3, Color3[::Red]) + AlphaFromLevel(aColor4, Color4[::Red]));
    // finally set the stupid pixel
    pCurrent->V = color.V;
FILTERSIMPLE_COLEND
FILTERSIMPLE_ROWEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Gradient_4Edge)
    , ::Image *Edge1, ::Image *Edge2, ::Image *Edge3, ::Image *Edge4) {
FILTERSIMPLE_INIT
FILTERSIMPLE_BEGIN
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Gradient_Horizontal_SourceAlpha)
    , Pixel Color1, Pixel Color2) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Gradient_Horizontal_SourceAlpha, 2) , Color1, Color2 _FOE
FILTERSIMPLE_BEGIN

    Byte* table = StaticAllocate<Byte>(XBuffer, rCoordinates.Width);
    for (int x = 0; x < rCoordinates.Width; x++) {
        table[x] = (((rCoordinates.Width - x - 1) + CropOffsets[0]) * 255) / (Area->Width);
    }

    AlphaLevel *aColor1, *aColor2;
    AlphaLevel *aSource, *aDest;
    Pixel color;
    Pixel *pColor = &color;

FILTERSIMPLE_ROW
FILTERSIMPLE_COL
        aColor1 = AlphaLevelLookup(table[iCX]);
        aColor2 = AlphaLevelLookup(table[iCX] ^ 0xFF);
        color[::Alpha] = AlphaFromLevel2(aColor1, Color1[::Alpha], aColor2, Color2[::Alpha]);
        color[::Blue] = AlphaFromLevel2(aColor1, Color1[::Blue], aColor2, Color2[::Blue]);
        color[::Green] = AlphaFromLevel2(aColor1, Color1[::Green], aColor2, Color2[::Green]);
        color[::Red] = AlphaFromLevel2(aColor1, Color1[::Red], aColor2, Color2[::Red]);
        aSource = AlphaLevelLookup(color[::Alpha]);
        aDest = AlphaLevelLookup(color[::Alpha] ^ 0xFF);
        BLENDPIXEL_ALPHA_OPACITY(pCurrent, pCurrent, pColor, aDest, aSource);
FILTERSIMPLE_COLEND
FILTERSIMPLE_ROWEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Gradient_Vertical_SourceAlpha)
    , Pixel Color1, Pixel Color2) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Gradient_Vertical_SourceAlpha, 2) , Color1, Color2 _FOE
FILTERSIMPLE_BEGIN

    Byte* table = StaticAllocate<Byte>(XBuffer, rCoordinates.Height);
    for (int y = 0; y < rCoordinates.Height; y++) {
        table[y] = (((rCoordinates.Height - y - 1) + CropOffsets[1]) * 255) / (Area->Height);
    }

    AlphaLevel *aColor1, *aColor2;
    AlphaLevel *aSource, *aDest;
    Pixel color;
    Pixel *pColor = &color;

FILTERSIMPLE_ROW
        aColor1 = AlphaLevelLookup(table[iCY]);
        aColor2 = AlphaLevelLookup(table[iCY] ^ 0xFF);
        color[::Alpha] = AlphaFromLevel2(aColor1, Color1[::Alpha], aColor2, Color2[::Alpha]);
        color[::Blue] = AlphaFromLevel2(aColor1, Color1[::Blue], aColor2, Color2[::Blue]);
        color[::Green] = AlphaFromLevel2(aColor1, Color1[::Green], aColor2, Color2[::Green]);
        color[::Red] = AlphaFromLevel2(aColor1, Color1[::Red], aColor2, Color2[::Red]);
        aSource = AlphaLevelLookup(color[::Alpha]);
        aDest = AlphaLevelLookup(color[::Alpha] ^ 0xFF);
FILTERSIMPLE_COL
        BLENDPIXEL_ALPHA_OPACITY(pCurrent, pCurrent, pColor, aDest, aSource);
FILTERSIMPLE_COLEND
FILTERSIMPLE_ROWEND
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Gradient_4Point_SourceAlpha)
    , Pixel Color1, Pixel Color2, Pixel Color3, Pixel Color4) {
FILTERSIMPLE_INIT
    // just in case, 'cause it's faster and all
    if ((Color1.V == Color2.V) && (Color3.V == Color4.V) && (Color3.V == Color1.V) && (Color4.V == Color2.V)) return FilterSimple_Fill_SourceAlpha(Image, Area, Color1);
    if ((Color1[::Alpha] == 255) && (Color2[::Alpha] == 255) && (Color3[::Alpha] == 255) && (Color4[::Alpha] == 255)) return FilterSimple_Gradient_4Point(Image, Area, Color1, Color2, Color3, Color4);
    if ((Color1.V == Color2.V) && (Color3.V == Color4.V)) return FilterSimple_Gradient_Vertical_SourceAlpha(Image, Area, Color3, Color1);
    if ((Color1.V == Color3.V) && (Color2.V == Color4.V)) return FilterSimple_Gradient_Horizontal_SourceAlpha(Image, Area, Color2, Color1);
    _FOS(FilterSimple_Gradient_4Point_SourceAlpha, 4) , Color1, Color2, Color3, Color4 _FOE
FILTERSIMPLE_BEGIN

    AlphaLevel *aSource, *aDest;
    AlphaLevel *aColor1, *aColor2, *aColor3, *aColor4;
    Byte *xTable, *yTable;
    Pixel color;
    int xWeight, yWeight, xWeight2, yWeight2;
    int weights[4];

    // allocate two lookup tables
    xTable = StaticAllocate<Byte>(XBuffer, rCoordinates.Width);
    yTable = StaticAllocate<Byte>(YBuffer, rCoordinates.Height);

    // fill 'em
    for (int x = 0; x < rCoordinates.Width; x++) {
        xTable[x] = (((rCoordinates.Width - x - 1) + CropOffsets[0]) * 255) / (Area->Width);
    }
    for (int y = 0; y < rCoordinates.Height; y++) {
        yTable[y] = (((rCoordinates.Height - y - 1) + CropOffsets[1]) * 255) / (Area->Height);
    }

FILTERSIMPLE_ROW
    // calculate the weight/opposite weight for this row
    yWeight2 = yTable[iCY];
    yWeight = yWeight2 ^ 0xFF;
FILTERSIMPLE_COL
    // calculate the weight/opposite weight for this column
    xWeight2 = xTable[iCX];
    xWeight = xWeight2 ^ 0xFF;
    // calculate the four weights for the source colors
    weights[0] = (xWeight * yWeight) / 255;
    weights[1] = (xWeight2 * yWeight) / 255;
    weights[2] = (xWeight * yWeight2) / 255;
    weights[3] = (weights[0] + weights[1] + weights[2]) ^ 0xFF;
    // retrieve the lookup pointers
    aColor1 = AlphaLevelLookup( weights[0] );
    aColor2 = AlphaLevelLookup( weights[1] );
    aColor3 = AlphaLevelLookup( weights[2] );
    aColor4 = AlphaLevelLookup( weights[3] );
    // mix the alpha
    color[::Alpha] = AlphaFromLevel(aColor1, Color1[::Alpha]) + AlphaFromLevel(aColor2, Color2[::Alpha]) + AlphaFromLevel(aColor3, Color3[::Alpha]) + AlphaFromLevel(aColor4, Color4[::Alpha]);
    // retrieve the two lookup pointers for the actual drawing
    aSource = AlphaLevelLookup( color[::Alpha] );
    aDest = AlphaLevelLookup( color[::Alpha] ^ 0xFF );
    // mix the blue, green, and red
    color[::Blue] = AlphaFromLevel(aSource, (AlphaFromLevel(aColor1, Color1[::Blue]) + AlphaFromLevel(aColor2, Color2[::Blue]) + AlphaFromLevel(aColor3, Color3[::Blue]) + AlphaFromLevel(aColor4, Color4[::Blue])));
    color[::Green] = AlphaFromLevel(aSource, (AlphaFromLevel(aColor1, Color1[::Green]) + AlphaFromLevel(aColor2, Color2[::Green]) + AlphaFromLevel(aColor3, Color3[::Green]) + AlphaFromLevel(aColor4, Color4[::Green])));
    color[::Red] = AlphaFromLevel(aSource, (AlphaFromLevel(aColor1, Color1[::Red]) + AlphaFromLevel(aColor2, Color2[::Red]) + AlphaFromLevel(aColor3, Color3[::Red]) + AlphaFromLevel(aColor4, Color4[::Red])));
    // finally set the stupid pixel
    (*pCurrent)[::Blue] = AlphaFromLevel(aDest, (*pCurrent)[::Blue]) + color[::Blue];
    (*pCurrent)[::Green] = AlphaFromLevel(aDest, (*pCurrent)[::Green]) + color[::Green];
    (*pCurrent)[::Red] = AlphaFromLevel(aDest, (*pCurrent)[::Red]) + color[::Red];
FILTERSIMPLE_COLEND
FILTERSIMPLE_ROWEND

    // aren't we glad it's over
//    DeleteArray(xTable);
//    DeleteArray(yTable);
    //LookupDeallocate(xTable);
    //LookupDeallocate(yTable);
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Gradient_Radial)
    , Pixel Color1, Pixel Color2) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Gradient_Radial, 2) , Color1, Color2 _FOE
FILTERSIMPLE_BEGIN
    
    Byte *xCTable, *yCTable;
    Pixel colorTable[256];
//    xCTable = AllocateArray(Byte, rCoordinates.Width);
//    yCTable = AllocateArray(Byte, rCoordinates.Height);
//    xCTable = LookupAllocate<Byte>(rCoordinates.Width);
//    yCTable = LookupAllocate<Byte>(rCoordinates.Height);
    xCTable = StaticAllocate<Byte>(XBuffer, rCoordinates.Width);
    yCTable = StaticAllocate<Byte>(YBuffer, rCoordinates.Height);
    {
      float xMul = ((255.0 / ((Area->Width) / 2))), yMul = ((255.0 / ((Area->Height) / 2)));
      int xOffset = (Area->Width / 2) - CropOffsets[2], yOffset = (Area->Height / 2) - CropOffsets[3];
      AlphaLevel *aColor1, *aColor2;
      for (int i = 0; i < rCoordinates.Width; i++) {
        xCTable[i] = abs(i - xOffset) * xMul;
      }
      for (int i = 0; i < rCoordinates.Height; i++) {
        yCTable[i] = abs(i - yOffset) * yMul;
      }
      for (int i = 0; i < 256; i++) {
        aColor2 = AlphaLevelLookup( i );
        aColor1 = AlphaLevelLookup( i ^ 0xFF );
        colorTable[i][::Blue] = AlphaFromLevel2(aColor1, Color1[::Blue], aColor2, Color2[::Blue]);
        colorTable[i][::Green] = AlphaFromLevel2(aColor1, Color1[::Green], aColor2, Color2[::Green]);
        colorTable[i][::Red] = AlphaFromLevel2(aColor1, Color1[::Red], aColor2, Color2[::Red]);
        colorTable[i][::Alpha] = AlphaFromLevel2(aColor1, Color1[::Alpha], aColor2, Color2[::Alpha]);
      }
    }

    Byte Weight;

FILTERSIMPLE_ROW
FILTERSIMPLE_COL
    Weight = PythagorasLookup(xCTable[iCX], yCTable[iCY]);
    *pCurrent = colorTable[Weight];
FILTERSIMPLE_COLEND
FILTERSIMPLE_ROWEND
//    DeleteArray(xCTable);
//    DeleteArray(yCTable);
//    LookupDeallocate(xCTable);
//    LookupDeallocate(yCTable);
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Gradient_Radial_SourceAlpha)
    , Pixel Color1, Pixel Color2) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Gradient_Radial_SourceAlpha, 2) , Color1, Color2 _FOE
FILTERSIMPLE_BEGIN
    
    AlphaLevel *aDest;
    Byte *xCTable, *yCTable;
    Pixel colorTable[256], currentColor;
//    xCTable = AllocateArray(Byte, rCoordinates.Width);
//    yCTable = AllocateArray(Byte, rCoordinates.Height);
    //xCTable = LookupAllocate<Byte>(rCoordinates.Width);
    //yCTable = LookupAllocate<Byte>(rCoordinates.Height);
    xCTable = StaticAllocate<Byte>(XBuffer, rCoordinates.Width);
    yCTable = StaticAllocate<Byte>(YBuffer, rCoordinates.Height);
    {
      AlphaLevel *aColor1, *aColor2;
      float xMul = ((255.0 / ((Area->Width) / 2))), yMul = ((255.0 / ((Area->Height) / 2)));
      int xOffset = (Area->Width / 2) - CropOffsets[2], yOffset = (Area->Height / 2) - CropOffsets[3];
      for (int i = 0; i < rCoordinates.Width; i++) {
        xCTable[i] = abs(i - xOffset) * xMul;
      }
      for (int i = 0; i < rCoordinates.Height; i++) {
        yCTable[i] = abs(i - yOffset) * yMul;
      }
      for (int i = 0; i < 256; i++) {
        aColor2 = AlphaLevelLookup( i );
        aColor1 = AlphaLevelLookup( i ^ 0xFF );
        colorTable[i][::Alpha] = AlphaFromLevel2(aColor1, Color1[::Alpha], aColor2, Color2[::Alpha]);
        colorTable[i][::Blue] = AlphaLookup(colorTable[i][::Alpha], AlphaFromLevel2(aColor1, Color1[::Blue], aColor2, Color2[::Blue]));
        colorTable[i][::Green] = AlphaLookup(colorTable[i][::Alpha], AlphaFromLevel2(aColor1, Color1[::Green], aColor2, Color2[::Green]));
        colorTable[i][::Red] = AlphaLookup(colorTable[i][::Alpha], AlphaFromLevel2(aColor1, Color1[::Red], aColor2, Color2[::Red]));
        colorTable[i][::Alpha] = colorTable[i][::Alpha] ^ 0xFF;
      }
    }

    Byte Weight;

FILTERSIMPLE_ROW
FILTERSIMPLE_COL
    Weight = PythagorasLookup(xCTable[iCX], yCTable[iCY]);
    currentColor = colorTable[Weight];
    // retrieve render lookup pointers
    aDest = AlphaLevelLookup( currentColor[::Alpha] );
    // render pixel
    (*pCurrent)[::Blue] = AlphaFromLevel(aDest, (*pCurrent)[::Blue]) + currentColor[::Blue];
    (*pCurrent)[::Green] = AlphaFromLevel(aDest, (*pCurrent)[::Green]) + currentColor[::Green];
    (*pCurrent)[::Red] = AlphaFromLevel(aDest, (*pCurrent)[::Red]) + currentColor[::Red];
FILTERSIMPLE_COLEND
FILTERSIMPLE_ROWEND
//    DeleteArray(xCTable);
//    DeleteArray(yCTable);
//    LookupDeallocate(xCTable);
//    LookupDeallocate(yCTable);
FILTERSIMPLE_END

Export int FilterSimple_ConvexPolygon(Image *Image, SimplePolygon *InPoly, Pixel Color, RenderFunction *Renderer, DoubleWord RenderArgument) {
    if (!Image) {
        return Failure;
    }
    if (!Image->initialized()) {
        return Failure;
    }
    if (!InPoly) {
        return Failure;
    }

    {
        int overrideresult = Override::EnumOverrides(Override::FilterSimple_ConvexPolygon, 5, Image, InPoly, Color, Renderer, RenderArgument);
#ifdef OVERRIDES
        if (overrideresult != 0) return overrideresult;
#endif
    }

    ImageLockManager ilImage(lockingMode, Image);
    if (!ilImage.performUnlock()) {
        return Failure;
    }

    SimplePolygon *Poly;
    
    if (InPoly->VertexCount < 2) {
      return Trivial_Success;
    }

    Poly = ClipPolygon(InPoly, &(Image->ClipRectangle));

    if (Poly == Null) {
      return Trivial_Success;
    }
    if (Poly->Count() < 2) {
      delete Poly;
      return Trivial_Success;
    }

    FRect Bounds;
    Bounds.X1 = Poly->MinimumX();
    Bounds.Y1 = Poly->MinimumY();
    Bounds.X2 = Poly->MaximumX();
    Bounds.Y2 = Poly->MaximumY();

    FLine *Edges = StaticAllocate<FLine>(EdgeBuffer, Poly->VertexCount);
    for (int e = 0; e < Poly->VertexCount; e++) {
      Edges[e].Start = Poly->Vertexes[e];
      Edges[e].End = Poly->Vertexes[WrapValue(e + 1,0,Poly->VertexCount - 1)];
    }

    int SpanCount = ceil(abs(Bounds.Y2 - Bounds.Y1)) + 1;
    int cy = 0;
    if (SpanCount < 1) {
      delete Poly;
      return Trivial_Success;
    }

    ISpan *Spans = StaticAllocate<ISpan>(PolyBuffer, SpanCount);
    for (int i = 0; i < SpanCount; i++) {
        Spans[i].S = 99999;
        Spans[i].E = -99999;
    }

    int YOffset = _Min(Bounds.Y1, Bounds.Y2);

    for (int l = 0; l < Poly->VertexCount; l++) {
        TRACELINE_INIT(Edges[l])
        TRACELINE_BEGIN
          cy = y - YOffset;
          if (i < numpixels) {
              if ((cy >= 0) && (cy < SpanCount)) {
                  Spans[cy].S = _Min(Spans[cy].S, x);
                  Spans[cy].E = _Max(Spans[cy].E, x);
              }
          }
        TRACELINE_END
    }

    Pixel *Pointer;
    int X, Y;

    for (int i = 0; i < SpanCount; i++) {
        Y = i + YOffset;
        if ((Y >= Image->ClipRectangle.Top) && (Y < Image->ClipRectangle.bottom())) {
            if (Spans[i].E >= Spans[i].S) {
                X = ClipValue(Spans[i].E, Image->ClipRectangle.Left, Image->ClipRectangle.right() - 1) - ClipValue(Spans[i].S, Image->ClipRectangle.Left, Image->ClipRectangle.right() - 1);
                if (X) {
                    X++;
                    Pointer = Image->fast_pointer(ClipValue(Spans[i].S, Image->ClipRectangle.Left, Image->ClipRectangle.right() - 1), Y);
                    if (Renderer != Null) {
                      Renderer(Pointer, Null, X, Color, RenderArgument);
                    } else {
                      _Fill<Pixel>(Pointer, Color, X);
                    }
                }
            }
        }
    }

    delete Poly;

    Image->dirty();

    return Success;
}

void RenderFunction_SourceAlpha(Pixel *Dest, Pixel *Source, int Count, Pixel SolidColor, DoubleWord Argument) {
    AlphaLevel *aSource, *aDest;
	  Pixel arg = Pixel(Argument);
    int i = Count;
    Byte a = 0;
    if (Source) {
      if (arg[::Alpha]) {
        AlphaLevel *aTint;
        int tR, tG, tB;
        aSource = AlphaLevelLookup(arg[::Alpha]);
        aTint = AlphaLevelLookup(arg[::Alpha] ^ 0xFF);
        tB = AlphaFromLevel(aSource, arg[::Blue]);
        tG = AlphaFromLevel(aSource, arg[::Green]);
        tR = AlphaFromLevel(aSource, arg[::Red]);
        while (i--) {
          a = (*Source)[::Alpha];
          if (a) {
            if (a == 255) {
              (*Dest)[::Blue] = AlphaFromLevel(aTint, (*Source)[::Blue]) + tB;
              (*Dest)[::Green] = AlphaFromLevel(aTint, (*Source)[::Green]) + tG;
              (*Dest)[::Red] = AlphaFromLevel(aTint, (*Source)[::Red]) + tR;
            } else {
              aSource = AlphaLevelLookup( a );
              aDest = AlphaLevelLookup( a ^ 0xFF );
              (*Dest)[::Blue] = AlphaFromLevel2(aDest, (*Dest)[::Blue], aSource, AlphaFromLevel(aTint, (*Source)[::Blue]) + tB);
              (*Dest)[::Green] = AlphaFromLevel2(aDest, (*Dest)[::Green], aSource, AlphaFromLevel(aTint, (*Source)[::Green]) + tG);
              (*Dest)[::Red] = AlphaFromLevel2(aDest, (*Dest)[::Red], aSource, AlphaFromLevel(aTint, (*Source)[::Red]) + tR);
            }
          }
          Dest++;
          Source++;
        }
      } else {
        while (i--) {
          a = (*Source)[::Alpha];
          if (a) {
            if (a == 255) {
              (*Dest)[::Blue] = (*Source)[::Blue];
              (*Dest)[::Green] = (*Source)[::Green];
              (*Dest)[::Red] = (*Source)[::Red];
            } else {
              aSource = AlphaLevelLookup( a );
              aDest = AlphaLevelLookup( a ^ 0xFF );
              (*Dest)[::Blue] = AlphaFromLevel2(aDest, (*Dest)[::Blue], aSource, (*Source)[::Blue]);
              (*Dest)[::Green] = AlphaFromLevel2(aDest, (*Dest)[::Green], aSource, (*Source)[::Green]);
              (*Dest)[::Red] = AlphaFromLevel2(aDest, (*Dest)[::Red], aSource, (*Source)[::Red]);
            }
          }
          Dest++;
          Source++;
        }
      }
    } else {
      if (arg[::Alpha]) {
        AlphaLevel *aArg, *aColor;
        aArg = AlphaLevelLookup(arg[::Alpha]);
        aColor = AlphaLevelLookup(arg[::Alpha] ^ 0xFF);
        SolidColor[::Blue] = AlphaFromLevel(aColor, SolidColor[::Blue]) + AlphaFromLevel(aArg, arg[::Blue]);
        SolidColor[::Green] = AlphaFromLevel(aColor, SolidColor[::Green]) + AlphaFromLevel(aArg, arg[::Green]);
        SolidColor[::Red] = AlphaFromLevel(aColor, SolidColor[::Red]) + AlphaFromLevel(aArg, arg[::Red]);
      }
      a = SolidColor[::Alpha];
      if (a) {
        if (a == 255) {
          _Fill<Pixel>(Dest, SolidColor, Count);
        } else {          
          aSource = AlphaLevelLookup( a );
          aDest = AlphaLevelLookup( a ^ 0xFF );
          while (i--) {
            (*Dest)[::Blue] = AlphaFromLevel(aDest, (*Dest)[::Blue]) + SolidColor[::Blue];
            (*Dest)[::Green] = AlphaFromLevel(aDest, (*Dest)[::Green]) + SolidColor[::Green];
            (*Dest)[::Red] = AlphaFromLevel(aDest, (*Dest)[::Red]) + SolidColor[::Red];
            Dest++;
          }
        }
      }
    }
}

void RenderFunction_Font_SourceAlpha(Pixel *Dest, Pixel *Source, int Count, Pixel SolidColor, DoubleWord Argument) {
    AlphaLevel *aSource, *aDest, *aScale;
    AlphaLevel *aRed, *aGreen, *aBlue;
	  Pixel arg = Pixel(Argument);
    aBlue = AlphaLevelLookup( arg[::Blue] );
    aGreen = AlphaLevelLookup( arg[::Green] );
    aRed = AlphaLevelLookup( arg[::Red] );
    int i = Count;
    Byte a = 0;
    if (Source) {
      if (arg[::Alpha] == 0) return;
      aScale = AlphaLevelLookup(arg[::Alpha]);
      while (i--) {
        a = AlphaFromLevel(aScale, (*Source)[::Alpha]);
        if (a) {
          aSource = AlphaLevelLookup( a );
          aDest = AlphaLevelLookup( a ^ 0xFF );
          (*Dest)[::Blue] = AlphaFromLevel2(aDest, (*Dest)[::Blue], aSource, AlphaFromLevel(aBlue, (*Source)[::Blue]));
          (*Dest)[::Green] = AlphaFromLevel2(aDest, (*Dest)[::Green], aSource, AlphaFromLevel(aGreen, (*Source)[::Green]));
          (*Dest)[::Red] = AlphaFromLevel2(aDest, (*Dest)[::Red], aSource, AlphaFromLevel(aRed, (*Source)[::Red]));
        }
        Dest++;
        Source++;
      }
    } else {
      SolidColor[::Blue] = AlphaFromLevel(aBlue, SolidColor[::Blue]);
      SolidColor[::Green] = AlphaFromLevel(aGreen, SolidColor[::Green]);
      SolidColor[::Red] = AlphaFromLevel(aRed, SolidColor[::Red]);
      SolidColor[::Alpha] = SolidColor[::Alpha] * arg[::Alpha] / 255;
      a = SolidColor[::Alpha];
      if (a) {
        if (a == 255) {
          _Fill<Pixel>(Dest, SolidColor, Count);
        } else {          
          aSource = AlphaLevelLookup( a );
          aDest = AlphaLevelLookup( a ^ 0xFF );
          while (i--) {
            (*Dest)[::Blue] = AlphaFromLevel(aDest, (*Dest)[::Blue]) + SolidColor[::Blue];
            (*Dest)[::Green] = AlphaFromLevel(aDest, (*Dest)[::Green]) + SolidColor[::Green];
            (*Dest)[::Red] = AlphaFromLevel(aDest, (*Dest)[::Red]) + SolidColor[::Red];
            Dest++;
          }
        }
      }
    }
}

void RenderFunction_Merge(Pixel *Dest, Pixel *Source, int Count, Pixel SolidColor, DoubleWord Argument) {
    AlphaLevel *aSource, *aDest;
    int i = Count;
    Byte a = 0;
    if (Source) {
      while (i--) {
        a = (*Source)[::Alpha];
        if (a) {
          if (a == 255) {
            *Dest = *Source;
          } else if (a == 255) {
            aSource = AlphaLevelLookup( a );
            aDest = AlphaLevelLookup( a ^ 0xFF );
            (*Dest)[::Blue] = AlphaFromLevel2(aDest, (*Dest)[::Blue], aSource, (*Source)[::Blue]);
            (*Dest)[::Green] = AlphaFromLevel2(aDest, (*Dest)[::Green], aSource, (*Source)[::Green]);
            (*Dest)[::Red] = AlphaFromLevel2(aDest, (*Dest)[::Red], aSource, (*Source)[::Red]);
          } else {
            a = ClipByteHigh(a + ( (*Dest)[::Alpha] ^ 0xFF ));
            aSource = AlphaLevelLookup( a );
            aDest = AlphaLevelLookup( a ^ 0xFF );
            (*Dest)[::Blue] = AlphaFromLevel2(aDest, (*Dest)[::Blue], aSource, (*Source)[::Blue]);
            (*Dest)[::Green] = AlphaFromLevel2(aDest, (*Dest)[::Green], aSource, (*Source)[::Green]);
            (*Dest)[::Red] = AlphaFromLevel2(aDest, (*Dest)[::Red], aSource, (*Source)[::Red]);
            (*Dest)[::Alpha] = ClipByteHigh((*Dest)[::Alpha] + (*Source)[::Alpha]);
          }
        }
        Dest++;
        Source++;
      }
    }
}

void RenderFunction_Screen(Pixel *Dest, Pixel *Source, int Count, Pixel SolidColor, DoubleWord Argument) {
    int i = Count;
    if (Source) {
      while (i--) {
          (*Dest)[::Blue] = AlphaLookup((*Dest)[::Blue] ^ 0xFF, (*Source)[::Blue] ^ 0xFF) ^ 0xFF;
          (*Dest)[::Green] = AlphaLookup((*Dest)[::Green] ^ 0xFF, (*Source)[::Green] ^ 0xFF) ^ 0xFF;
          (*Dest)[::Red] = AlphaLookup((*Dest)[::Red] ^ 0xFF, (*Source)[::Red] ^ 0xFF) ^ 0xFF;
          Dest++;
          Source++;
      }
    } else {
      SolidColor[::Blue] = SolidColor[::Blue] ^ 0xFF;
      SolidColor[::Green] = SolidColor[::Green] ^ 0xFF;
      SolidColor[::Red] = SolidColor[::Red] ^ 0xFF;
      while (i--) {
          (*Dest)[::Blue] = AlphaLookup((*Dest)[::Blue] ^ 0xFF, SolidColor[::Blue]) ^ 0xFF;
          (*Dest)[::Green] = AlphaLookup((*Dest)[::Green] ^ 0xFF, SolidColor[::Green]) ^ 0xFF;
          (*Dest)[::Red] = AlphaLookup((*Dest)[::Red] ^ 0xFF, SolidColor[::Red]) ^ 0xFF;
          Dest++;
      }
    }
}

void RenderFunction_Additive(Pixel *Dest, Pixel *Source, int Count, Pixel SolidColor, DoubleWord Argument) {
    int i = Count;
    if (Source) {
      while (i--) {
          BLENDPIXEL_ADDITIVE(Dest, Dest, Source);
          Dest++;
          Source++;
      }
    } else {
      while (i--) {
        (*Dest)[::Blue] = ClipByteHigh((*Dest)[::Blue] + (SolidColor[::Blue]));
        (*Dest)[::Green] = ClipByteHigh((*Dest)[::Green] + (SolidColor[::Green]));
        (*Dest)[::Red] = ClipByteHigh((*Dest)[::Red] + (SolidColor[::Red]));
        Dest++;
      }
    }
}

void RenderFunction_Subtractive(Pixel *Dest, Pixel *Source, int Count, Pixel SolidColor, DoubleWord Argument) {
    int i = Count;
    if (Source) {
      while (i--) {
          BLENDPIXEL_SUBTRACTIVE(Dest, Dest, Source);
          Dest++;
          Source++;
      }
    } else {
      while (i--) {
        (*Dest)[::Blue] = ClipByteLow((*Dest)[::Blue] - (SolidColor[::Blue]));
        (*Dest)[::Green] = ClipByteLow((*Dest)[::Green] - (SolidColor[::Green]));
        (*Dest)[::Red] = ClipByteLow((*Dest)[::Red] - (SolidColor[::Red]));
        Dest++;
      }
    }
}

void RenderFunction_Additive_SourceAlpha(Pixel *Dest, Pixel *Source, int Count, Pixel SolidColor, DoubleWord Argument) {
    AlphaLevel *aSource;
    int i = Count;
    if (Source) {
      while (i--) {
        aSource = AlphaLevelLookup((*Source)[::Alpha]);
        BLENDPIXEL_ADDITIVE_OPACITY(Dest, Dest, Source, aSource);
        Dest++;
        Source++;
      }
    } else {
      SolidColor = Premultiply(SolidColor);
      while (i--) {
        (*Dest)[::Blue] = ClipByteHigh((*Dest)[::Blue] + (SolidColor[::Blue]));
        (*Dest)[::Green] = ClipByteHigh((*Dest)[::Green] + (SolidColor[::Green]));
        (*Dest)[::Red] = ClipByteHigh((*Dest)[::Red] + (SolidColor[::Red]));
        Dest++;
      }
    }
}

void RenderFunction_Subtractive_SourceAlpha(Pixel *Dest, Pixel *Source, int Count, Pixel SolidColor, DoubleWord Argument) {
    AlphaLevel *aSource;
    int i = Count;
    if (Source) {
      while (i--) {
        aSource = AlphaLevelLookup((*Source)[::Alpha]);
        BLENDPIXEL_SUBTRACTIVE_OPACITY(Dest, Dest, Source, aSource);
        Dest++;
        Source++;
      }
    } else {
      SolidColor = Premultiply(SolidColor);
      while (i--) {
        (*Dest)[::Blue] = ClipByteLow((*Dest)[::Blue] - (SolidColor[::Blue]));
        (*Dest)[::Green] = ClipByteLow((*Dest)[::Green] - (SolidColor[::Green]));
        (*Dest)[::Red] = ClipByteLow((*Dest)[::Red] - (SolidColor[::Red]));
        Dest++;
      }
    }
}

void RenderFunction_Shadow(Pixel *Dest, Pixel *Source, int Count, Pixel SolidColor, DoubleWord Argument) {
    int i = Count;
    AlphaLevel *alpha;
    if (Source) {
      while (i--) {
          alpha = AlphaLevelLookup((*Source)[::Alpha] ^ 0xFF);
          (*Dest)[::Blue] = AlphaFromLevel(alpha, (*Dest)[::Blue]);
          (*Dest)[::Green] = AlphaFromLevel(alpha, (*Dest)[::Green]);
          (*Dest)[::Red] = AlphaFromLevel(alpha, (*Dest)[::Red]);
          Dest++;
          Source++;
      }
    } else {
      alpha = AlphaLevelLookup(SolidColor[::Alpha] ^ 0xFF);
      while (i--) {
          (*Dest)[::Blue] = AlphaFromLevel(alpha, (*Dest)[::Blue]);
          (*Dest)[::Green] = AlphaFromLevel(alpha, (*Dest)[::Green]);
          (*Dest)[::Red] = AlphaFromLevel(alpha, (*Dest)[::Red]);
          Dest++;
      }
    }
}

Export int GetMergeRenderer() {
  return (int)RenderFunction_Merge;
}

Export int GetSourceAlphaRenderer() {
  return (int)RenderFunction_SourceAlpha;
}

Export int GetFontSourceAlphaRenderer() {
  return (int)RenderFunction_Font_SourceAlpha;
}

Export int GetScreenRenderer() {
  return (int)RenderFunction_Screen;
}

Export int GetAdditiveRenderer() {
  return (int)RenderFunction_Additive;
}

Export int GetSubtractiveRenderer() {
  return (int)RenderFunction_Subtractive;
}

Export int GetAdditiveSourceAlphaRenderer() {
  return (int)RenderFunction_Additive_SourceAlpha;
}

Export int GetSubtractiveSourceAlphaRenderer() {
  return (int)RenderFunction_Subtractive_SourceAlpha;
}

Export int GetShadowRenderer() {
  return (int)RenderFunction_Shadow;
}

Export int FilterSimple_ConvexPolygon_Textured(Image *Dest, Image *Texture, TexturedPolygon *InPoly, ScalerFunction *Scaler, RenderFunction *Renderer, DoubleWord RenderArgument) {
    if (!InPoly) {
        return Failure;
    }
    if (!Dest) {
        return Failure;
    }
    if (!Dest->initialized()) {
        return Failure;
    }
    if (!Texture) {
        return Failure;
    }
    if (!Texture->initialized()) {
        return Failure;
    }
    if (!Scaler) {
        return Failure;
    }

    if (InPoly->VertexCount == 4) {
      // check for rectangle
      if ((InPoly->Vertexes[0].X == InPoly->Vertexes[3].X) && (InPoly->Vertexes[1].X == InPoly->Vertexes[2].X)
            && (InPoly->Vertexes[0].Y == InPoly->Vertexes[1].Y) && (InPoly->Vertexes[2].Y == InPoly->Vertexes[3].Y) &&
            (InPoly->Vertexes[1].U >= InPoly->Vertexes[0].U) && (InPoly->Vertexes[2].U >= InPoly->Vertexes[3].U) &&
            (InPoly->Vertexes[2].V >= InPoly->Vertexes[1].V) && (InPoly->Vertexes[0].V <= InPoly->Vertexes[3].V)) {
        Rectangle DRect, SRect;
        DRect.Left = InPoly->Vertexes[0].X;
        DRect.Top = InPoly->Vertexes[0].Y;
        DRect.Width = InPoly->Vertexes[1].X - InPoly->Vertexes[0].X + 1;
        DRect.Height = InPoly->Vertexes[2].Y - InPoly->Vertexes[0].Y + 1;
        SRect.Left = InPoly->Vertexes[0].U;
        SRect.Top = InPoly->Vertexes[0].V;
        SRect.Width = InPoly->Vertexes[2].U - InPoly->Vertexes[0].U + 1;
        SRect.Height = InPoly->Vertexes[2].V - InPoly->Vertexes[0].V + 1;
        if (Renderer == Null) {
          return BlitResample_Normal(Dest, Texture, &DRect, &SRect, Scaler);
        }
      }
    }

    {
        int overrideresult = Override::EnumOverrides(Override::FilterSimple_ConvexPolygon_Textured, 6, Dest, Texture, InPoly, Scaler, Renderer, RenderArgument);
#ifdef OVERRIDES
        if (overrideresult != 0) return overrideresult;
#endif
    }

    ImageLockManager ilDest(lockingMode, Dest);
    if (!ilDest.performUnlock()) {
        return Failure;
    }
    ImageLockManager ilTexture(lockingMode, Texture);
    if (!ilTexture.performUnlock()) {
        return Failure;
    }

    TexturedPolygon *Poly;
    
    if (InPoly->VertexCount < 2) {
      return Trivial_Success;
    }

    Poly = ClipPolygon(InPoly, &(Dest->ClipRectangle));

    if (Poly == Null) {
      return Trivial_Success;
    }
    if (Poly->VertexCount < 2) {
      delete Poly;
      return Trivial_Success;
    }

    FRect Bounds;
    Bounds.X1 = Poly->MinimumX();
    Bounds.Y1 = Poly->MinimumY();
    Bounds.X2 = Poly->MaximumX();
    Bounds.Y2 = Poly->MaximumY();

//    FLine *Edges = new FLine[Poly->VertexCount];
    FLine *Edges = StaticAllocate<FLine>(EdgeBuffer, Poly->VertexCount);
    for (int e = 0; e < Poly->VertexCount; e++) {
      Edges[e].Start.X = Poly->Vertexes[e].X;
      Edges[e].Start.Y = Poly->Vertexes[e].Y;
      Edges[e].End.X = Poly->Vertexes[WrapValue(e + 1,0,Poly->VertexCount-1)].X;
      Edges[e].End.Y = Poly->Vertexes[WrapValue(e + 1,0,Poly->VertexCount-1)].Y; 
    }

    int SpanCount = ceil(abs(Bounds.Y2 - Bounds.Y1)) + 1;
    int cy = 0;
    int max_span_width = 0;
    if (SpanCount < 1) {
      delete Poly;
//      DeleteArray(Edges);
      return Trivial_Success;
    }

//    ISpan *Spans = AllocateArray(ISpan, SpanCount);
    ISpan *Spans = StaticAllocate<ISpan>(PolyBuffer, SpanCount);
    TextureCoordinateF *StartCoords = StaticAllocate<TextureCoordinateF>(XBuffer, SpanCount);
    TextureCoordinateF *EndCoords = StaticAllocate<TextureCoordinateF>(YBuffer, SpanCount);
    for (int i = 0; i < SpanCount; i++) {
        Spans[i].S = 99999;
        Spans[i].E = -99999;
        StartCoords[i].U = (0);
        StartCoords[i].V = (0);
        EndCoords[i].U = (0);
        EndCoords[i].V = (0);
    }

    int YOffset = _Min(Bounds.Y1, Bounds.Y2);

    for (int l = 0; l < Poly->VertexCount; l++) {
        int ln = WrapValue(l + 1, 0, Poly->VertexCount-1);
        float U = 0, V = 0;
        float UI = 0, VI = 0;
        TRACELINE_INIT(Edges[l])
            U = (Poly->Vertexes[l].U);
            V = (Poly->Vertexes[l].V);
            UI = ((Poly->Vertexes[ln].U - Poly->Vertexes[l].U) / (numpixels-1));
            VI = ((Poly->Vertexes[ln].V - Poly->Vertexes[l].V) / (numpixels-1));
        TRACELINE_BEGIN
            cy = y - YOffset;
            if ((cy >= 0) && (cy < SpanCount)) {
              if (x < Spans[cy].S) {
                Spans[cy].S = x;
                StartCoords[cy].U = (U);
                StartCoords[cy].V = (V);
              }
              if (x > Spans[cy].E) {
                Spans[cy].E = x;
                EndCoords[cy].U = (U);
                EndCoords[cy].V = (V);
              }
              max_span_width = _Max(max_span_width, (Spans[cy].E - Spans[cy].S) + 1);
            }
            U += UI;
            V += VI;
        TRACELINE_END
    }

    Pixel *Pointer;
    Pixel *CurrentPixel;
    Pixel *ScanlineTable = Null;
    float fXI, fYI;
    int X, Y, L;

    //ScanlineTable = AllocateArray(Pixel, max_span_width + 2);
    ScanlineTable = StaticAllocate<Pixel>(TextureBuffer, max_span_width + 2);
    for (int i = 0; i < SpanCount; i++) {
        Y = i + YOffset;
        if ((Y >= Dest->ClipRectangle.Top) && (Y < Dest->ClipRectangle.bottom())) {
            if (Spans[i].E >= Spans[i].S) {
                L = Spans[i].E - Spans[i].S;
                X = ClipValue(Spans[i].E, Dest->ClipRectangle.Left, Dest->ClipRectangle.right() - 1) - ClipValue(Spans[i].S, Dest->ClipRectangle.Left, Dest->ClipRectangle.right() - 1);
                if (X) {
                    X++;
                    Pointer = Dest->fast_pointer(ClipValue(Spans[i].S, Dest->ClipRectangle.Left, Dest->ClipRectangle.right() - 1), Y);
                    float fL = (float)L;
                    fXI = (EndCoords[i].U - StartCoords[i].U) / fL;
                    fYI = (EndCoords[i].V - StartCoords[i].V) / fL;
                    int ifXI = (int)(fXI * 65535.0f);
                    int ifYI = (int)(fYI * 65535.0f);
                    int ifX = (int)(StartCoords[i].U * 65535.0f);
                    int ifY = (int)(StartCoords[i].V * 65535.0f);
                    Scaler(Texture, ifX / 65535, ifY / 65535,
                    ifX % 65535, ifY % 65535,
                    ifXI / 65535, ifYI / 65535, ifXI % 65535, ifYI % 65535,
                    L+1, ScanlineTable);
                    CurrentPixel = ScanlineTable;
                    if (Spans[i].S < Dest->ClipRectangle.Left) {
                        CurrentPixel += Dest->ClipRectangle.Left - Spans[i].S;
                    }
                    if (Renderer != Null) {
                        Renderer(Pointer, CurrentPixel, X, 0, RenderArgument);
                    } else {
                        while (X--) {
                            *Pointer = *CurrentPixel;
                            CurrentPixel++;
                            Pointer++;
                        }
                    }
                }
            }
        }
    }

//    DeleteArray(StartCoords);
//    DeleteArray(EndCoords);
//    DeleteArray(ScanlineTable);
//    DeleteArray(Spans);
//    DeleteArray(Edges);

    delete Poly;

    Dest->dirty();

    return Success;
}

Export int FilterSimple_ConvexPolygon_Gradient(Image *Dest, GradientPolygon *InPoly, RenderFunction *Renderer, DoubleWord RenderArgument) {
    if (!InPoly) {
        return Failure;
    }
    if (!Dest) {
        return Failure;
    }
    if (!Dest->initialized()) {
        return Failure;
    }

    {
        int overrideresult = Override::EnumOverrides(Override::FilterSimple_ConvexPolygon_Gradient, 3, Dest, InPoly, Renderer);
#ifdef OVERRIDES
        if (overrideresult != 0) return overrideresult;
#endif
    }

    ImageLockManager ilDest(lockingMode, Dest);
    if (!ilDest.performUnlock()) {
        return Failure;
    }

    GradientPolygon *Poly;
    
    if (InPoly->VertexCount < 2) {
      return Trivial_Success;
    }

    Poly = ClipPolygon(InPoly, &(Dest->ClipRectangle));

    if (Poly == Null) {
      return Trivial_Success;
    }
    if (Poly->VertexCount < 2) {
      delete Poly;
      return Trivial_Success;
    }

    FRect Bounds;
    Bounds.X1 = Poly->MinimumX();
    Bounds.Y1 = Poly->MinimumY();
    Bounds.X2 = Poly->MaximumX();
    Bounds.Y2 = Poly->MaximumY();

//    FLine *Edges = AllocateArray(FLine, Poly->VertexCount);
    FLine *Edges = StaticAllocate<FLine>(EdgeBuffer, Poly->VertexCount);
    for (int e = 0; e < Poly->VertexCount; e++) {
      Edges[e].Start.X = Poly->Vertexes[e].X;
      Edges[e].Start.Y = Poly->Vertexes[e].Y;
      Edges[e].End.X = Poly->Vertexes[WrapValue(e + 1,0,Poly->VertexCount-1)].X;
      Edges[e].End.Y = Poly->Vertexes[WrapValue(e + 1,0,Poly->VertexCount-1)].Y; 
    }

    int SpanCount = ceil(abs(Bounds.Y2 - Bounds.Y1)) + 1;
    int cy = 0;
    int max_span_width = 0;
    if (SpanCount < 1) {
      delete Poly;
//      DeleteArray(Edges);
      return Trivial_Success;
    }

//    ISpan *Spans = AllocateArray(ISpan, SpanCount);
    ISpan *Spans = StaticAllocate<ISpan>(PolyBuffer, SpanCount);
//    Pixel *StartColor = LookupAllocate<Pixel>(SpanCount);
//    Pixel *EndColor = LookupAllocate<Pixel>(SpanCount);
    Pixel *StartColor = StaticAllocate<Pixel>(XBuffer, SpanCount);
    Pixel *EndColor = StaticAllocate<Pixel>(YBuffer, SpanCount);
    for (int i = 0; i < SpanCount; i++) {
        Spans[i].S = 99999;
        Spans[i].E = -99999;
        StartColor[i] = 0;
        EndColor[i] = 0;
    }

    int YOffset = _Min(Bounds.Y1, Bounds.Y2);

    for (int l = 0; l < Poly->VertexCount; l++) {
        int ln = WrapValue(l + 1, 0, Poly->VertexCount-1);
        float R = 0, G = 0, B = 0, A = 0;
        float RI = 0, GI = 0, BI = 0, AI = 0;
        TRACELINE_INIT(Edges[l])
            B = Poly->Vertexes[l].color()[::Blue];
            G = Poly->Vertexes[l].color()[::Green];
            R = Poly->Vertexes[l].color()[::Red];
            A = Poly->Vertexes[l].color()[::Alpha];
            BI = (Poly->Vertexes[ln].color()[::Blue] - B) / (numpixels-1);
            GI = (Poly->Vertexes[ln].color()[::Green] - G) / (numpixels-1);
            RI = (Poly->Vertexes[ln].color()[::Red] - R) / (numpixels-1);
            AI = (Poly->Vertexes[ln].color()[::Alpha] - A) / (numpixels-1);
        TRACELINE_BEGIN
            cy = y - YOffset;
            if ((cy >= 0) && (cy < SpanCount)) {
              if (x <= Spans[cy].S) {
                Spans[cy].S = x;
                StartColor[cy] = Pixel(R, G, B, A);
              }
              if (x >= Spans[cy].E) {
                Spans[cy].E = x;
                EndColor[cy] = Pixel(R, G, B, A);
              }
              max_span_width = _Max(max_span_width, (Spans[cy].E - Spans[cy].S) + 1);
            }
            B += BI;
            G += GI;
            R += RI;
            A += AI;
        TRACELINE_END
    }

    Pixel *Pointer;
    Pixel *CurrentPixel;
    Pixel *ScanlineTable = Null;
    int X, Y, L; // O, LO;

//    ScanlineTable = LookupAllocate<Pixel>(max_span_width + 2);
    ScanlineTable = StaticAllocate<Pixel>(TextureBuffer, max_span_width + 2);
    for (int i = 0; i < SpanCount; i++) {
        Y = i + YOffset;
        if ((Y >= Dest->ClipRectangle.Top) && (Y < Dest->ClipRectangle.bottom())) {
            if (Spans[i].E >= Spans[i].S) {
                L = Spans[i].E - Spans[i].S;
                X = ClipValue(Spans[i].E, Dest->ClipRectangle.Left, Dest->ClipRectangle.right() - 1) - ClipValue(Spans[i].S, Dest->ClipRectangle.Left, Dest->ClipRectangle.right() - 1);
                if (X) {
                    X++;
                    Pointer = Dest->fast_pointer(ClipValue(Spans[i].S, Dest->ClipRectangle.Left, Dest->ClipRectangle.right() - 1), Y);
                    CurrentPixel = ScanlineTable;
/*
                    if (Spans[i].S < Dest->ClipRectangle.Left) {
                        O = Dest->ClipRectangle.Left - Spans[i].S;
//                        CurrentPixel += O;   
                    } else {
                        O = 0;
                    }
                    if (Spans[i].E > Dest->ClipRectangle.right()) {
                        LO = Spans[i].E - Dest->ClipRectangle.right();
                    } else {
                        LO = 0;
                    }
                    GenerateGradientTable(ScanlineTable, StartColor[i], EndColor[i], X + O + LO, O);
*/
                    GenerateGradientTable(ScanlineTable, StartColor[i], EndColor[i], X, 0);
                    if (Renderer != Null) {
                        Renderer(Pointer, CurrentPixel, X, 0, RenderArgument);
                    } else {
                        while (X--) {
                            *Pointer = *CurrentPixel;
                            CurrentPixel++;
                            Pointer++;
                        }
                    }
                }
            }
        }
    }

//    LookupDeallocate<Pixel>(ScanlineTable);
//    LookupDeallocate<Pixel>(StartColor);
//    LookupDeallocate<Pixel>(EndColor);

    Dest->dirty();
    delete Poly;

    return Success;
}

/*
Bresenham's circle algorithm calculates the locations of the pixels in the first 45 degrees. It assumes that the circle is centered on the origin. So for every pixel (x,y) it calculates we draw a pixel in each of the 8 octants of the circle :
PutPixel(CenterX + X, Center Y + Y)
PutPixel(CenterX + X, Center Y - Y)
PutPixel(CenterX - X, Center Y + Y)
PutPixel(CenterX - X, Center Y - Y)
PutPixel(CenterX + Y, Center Y + X)
PutPixel(CenterX + Y, Center Y - X)
PutPixel(CenterX - Y, Center Y + X)
PutPixel(CenterX - Y, Center Y - X)

So let's get into the actual algorithm. Given a radius for the circle we perform this initialisation:
d := 3 - (2 * RADIUS)
x := 0
y := RADIUS

Now for each pixel we do the following operations:
Draw the 8 circle pixels
if d < 0 then
    d := d + (4 * x) + 6
else
  begin
    d := d + 4 * (x - y) + 10
    y := y - 1;
  end;
*/

Export int FilterSimple_ConvexPolygon_AntiAlias(Image *Image, SimplePolygon *InPoly, Pixel Color, RenderFunction *Renderer, DoubleWord RenderArgument) {
    if (!Image) {
        return Failure;
    }
    if (!Image->initialized()) {
        return Failure;
    }
    if (!InPoly) {
        return Failure;
    }

    {
        int overrideresult = Override::EnumOverrides(Override::FilterSimple_ConvexPolygon_AntiAlias, 5, Image, InPoly, Color, Renderer, RenderArgument);
#ifdef OVERRIDES
        if (overrideresult != 0) return overrideresult;
#endif
    }

    SimplePolygon *Poly;
    
    if (InPoly->VertexCount < 2) {
      return Trivial_Success;
    }

    Poly = ClipPolygon(InPoly, &(Image->ClipRectangle));

    if (Poly == Null) {
      return Trivial_Success;
    }
    if (Poly->Count() < 2) {
      delete Poly;
      return Trivial_Success;
    }

    FRect Bounds;
    Bounds.X1 = Poly->MinimumX();
    Bounds.Y1 = Poly->MinimumY();
    Bounds.X2 = Poly->MaximumX();
    Bounds.Y2 = Poly->MaximumY();

    FLine *Edges = StaticAllocate<FLine>(EdgeBuffer, Poly->VertexCount);
    for (int e = 0; e < Poly->VertexCount; e++) {
      Edges[e].Start = Poly->Vertexes[e];
      Edges[e].End = Poly->Vertexes[WrapValue(e + 1,0,Poly->VertexCount - 1)];
    }

    int SpanCount = ceil(abs(Bounds.Y2 - Bounds.Y1)) + 1;
    int XSpanCount = ceil(abs(Bounds.X2 - Bounds.X1)) + 1;
    int cy = 0, cx = 0;
    if ((SpanCount < 1) || (XSpanCount < 1)) {
      delete Poly;
      return Trivial_Success;
    }

    AASpan *Spans = StaticAllocate<AASpan>(PolyBuffer, SpanCount);
    AAColSpan *XSpans = StaticAllocate<AAColSpan>(EdgeBuffer1, XSpanCount);
    for (int i = 0; i < SpanCount; i++) {
        Spans[i].S = 99999;
        Spans[i].E = -99999;
        Spans[i].A1 = 0;
        Spans[i].A2 = 0;
    }
    for (int i = 0; i < XSpanCount; i++) {
        XSpans[i].S = 99999;
        XSpans[i].E = -99999;
        XSpans[i].A1 = 0;
        XSpans[i].A2 = 0;
    }

    int max_span_width = 0;
    int XOffset = _Min(Bounds.X1, Bounds.X2);
    int YOffset = _Min(Bounds.Y1, Bounds.Y2);
    float fa = 0, xo, yo, xo2;
    int flx, fly;

    for (int l = 0; l < Poly->VertexCount; l++) {
        TRACELINEFP_INIT(Edges[l], 2)
        TRACELINEFP_BEGIN
            flx = floor(x);
            fly = floor(y);
            cx = flx - XOffset;
            cy = fly - YOffset;
            if ((cy >= 0) && (cy < SpanCount)) {
                xo = x - flx;
                xo2 = 1.0f - xo;
                if (flx < Spans[cy].S) {
                    Spans[cy].S = flx;
                    Spans[cy].A1 = xo2 * 255;
                } else if (flx == Spans[cy].S) {
                    Spans[cy].A1 = ((xo2 * 255) + Spans[cy].A1) / 2;
                }
                if (flx > Spans[cy].E) {
                    Spans[cy].E = flx;
                    Spans[cy].A2 = xo * 255;
                } else if (flx == Spans[cy].E) {
                    Spans[cy].A2 = ((xo * 255) + Spans[cy].A2) / 2;
                }
                max_span_width = _Max(max_span_width, (Spans[cy].E - Spans[cy].S) + 1);
            }
            if ((cx >= 0) && (cx < XSpanCount)) {
                yo = y - fly;
                if (fly <= XSpans[cx].S) {
                    XSpans[cx].S = fly;
                    fa = 1.0f - yo;
                    XSpans[cx].A1 = fa * 255;
                }
                if (fly >= XSpans[cx].E) {
                    XSpans[cx].E = fly;
                    XSpans[cx].A2 = yo * 255;
                }
            }
        TRACELINEFP_END
    }

    Pixel *Pointer = Null;
    Pixel *ScanlineTable = Null;
    Pixel *CurrentPixel = Null;
    int X, Y, L, s, a, ya, oa = 0;
    AlphaLevel *ScaleTable = AlphaLevelLookup(0);

    ScanlineTable = StaticAllocate<Pixel>(TextureBuffer, max_span_width + 2);

    LockedRenderFunction *lRenderer = Null;
    bool needLock = true;
    {
        int overrideresult = Override::EnumOverrides(Override::GetScanlineRenderer, 4, Image, Renderer, max_span_width + 2, RenderArgument);
  #ifdef OVERRIDES
        if (overrideresult != 0) {
          lRenderer = reinterpret_cast<LockedRenderFunction*>(overrideresult);
          needLock = false;
        }
  #endif
    }

    ImageLockManager ilDest(lockingMode, Image);
    if (needLock) {
      if (!ilDest.performUnlock()) {
          return Failure;
      }
    }

    for (int i = 0; i < SpanCount; i++) {
        Y = i + YOffset;
        if ((Y >= Image->ClipRectangle.Top) && (Y < Image->ClipRectangle.bottom())) {
            if (Spans[i].E >= Spans[i].S) {
                L = Spans[i].E - Spans[i].S;
                X = ClipValue(Spans[i].E, Image->ClipRectangle.Left, Image->ClipRectangle.right() - 1) - ClipValue(Spans[i].S, Image->ClipRectangle.Left, Image->ClipRectangle.right() - 1);
                if (X) {
                    X++;
                    Pointer = Image->fast_pointer(ClipValue(Spans[i].S, Image->ClipRectangle.Left, Image->ClipRectangle.right() - 1), Y);
                    CurrentPixel = ScanlineTable;
                    for (s = Spans[i].S; s <= Spans[i].E; s++) {
                      cx = s - XOffset;
                      cy = i + YOffset;
                      if ((cx >= 0) && (cx < XSpanCount)) {
                        if (XSpans[cx].S > XSpans[cx].E) {
                          ya = 0;
                        } else if (cy == XSpans[cx].S) {
                          ya = XSpans[cx].A1;
                        } else if (cy == XSpans[cx].E) {
                          ya = XSpans[cx].A2;
                        } else if ((cy > XSpans[cx].S) && (cy < XSpans[cx].E)) {
                          ya = 255;
                        } else {
                          ya = 0;
                        }
                      } else {
                        ya = 0;
                      }
                      if (s == Spans[i].S) {
                        a = (ya * Spans[i].A1) / 255;
                      } else if (s == Spans[i].E) {
                        a = (ya * Spans[i].A2) / 255;
                      } else if ((s > Spans[i].S) && (s < Spans[i].E)) {
                        a = ya;
                      } else {
                        a = 0;
                      }
                      if (a) {
                        if (a != 255) {
                          if (a != oa) {
                            oa = a;
                            ScaleTable = AlphaLevelLookup(a);
                          }
                          *CurrentPixel = Color;
                          (*CurrentPixel)[::Alpha] = AlphaFromLevel(ScaleTable, Color[::Alpha]);
                        } else {
                          *CurrentPixel = Color;
                        }
                      } else {
                        *CurrentPixel = 0;
                      }
                      CurrentPixel++;
                    }
                    CurrentPixel = ScanlineTable;
                    if (Spans[i].S < Image->ClipRectangle.Left) {
                        CurrentPixel += Image->ClipRectangle.Left - Spans[i].S;
                    }
                    if (needLock) {
                      if (Renderer != Null) {
                          Renderer(Pointer, CurrentPixel, X, 0, RenderArgument);
                      } else {
                          while (X--) {
                              *Pointer = *CurrentPixel;
                              CurrentPixel++;
                              Pointer++;
                          }
                      }
                    } else {
                      if (lRenderer != Null) {
                        lRenderer(Image, ClipValue(Spans[i].S, Image->ClipRectangle.Left, Image->ClipRectangle.right() - 1), Y, CurrentPixel, X);
                      }
                    }
                }
            }
        }
    }

    delete Poly;

    Image->dirty();
    return Success;
}

Export int FilterSimple_FilledCircle_AntiAlias(Image *Image, float X, float Y, float XRadius, float YRadius, Pixel Color, RenderFunction *Renderer, float Accuracy) {
  SimplePolygon Poly = SimplePolygon();
  int v = ClipValue(ceil(2.0f * Pi * sqrt(pow(XRadius / 2.0f, 2.0f) + pow(YRadius / 2.0f, 2.0f)) * Accuracy), 3, 2048);
  Poly.Allocate(v);
  float i = Radians((360.0f) / (float)(v));
  float x, y, r = 0;
  for (int s = 0; s < v; s++) {
    x = (sin(r) * XRadius) + X;
    y = (-cos(r) * YRadius) + Y;
    Poly.Append(FPoint(x, y));
    r += i;
  }
  return FilterSimple_ConvexPolygon_AntiAlias(Image, &Poly, Color, Renderer, 0);
}

Export int FilterSimple_FilledPie_AntiAlias(Image *Image, float X, float Y, float XRadius, float YRadius, float Start, float End, Pixel Color, RenderFunction *Renderer, float Accuracy) {
  if (abs(End - Start) > 180) {
    float d = (End - Start);
    FilterSimple_FilledPie_AntiAlias(Image, X, Y, XRadius, YRadius, Start, Start + (d / 2.0f) + 10.0f, Color, Renderer, Accuracy);
    return FilterSimple_FilledPie_AntiAlias(Image, X, Y, XRadius, YRadius, Start + (d / 2.0f) - 10.0f, End, Color, Renderer, Accuracy);
  }
  SimplePolygon Poly = SimplePolygon();
  int v = ClipValue(ceil(2.0f * Pi * sqrt(pow(XRadius / 2.0f, 2.0f) + pow(YRadius / 2.0f, 2.0f)) * (abs(End-Start) / 360.0f) * Accuracy), 3, 2048);
  Poly.Allocate(v + 3);
  Poly.Append(FPoint(X, Y));
  float i = Radians((End-Start) / (float)(v));
  float x, y, r = Radians(Start);
  x = (sin(r) * XRadius) + X;
  y = (-cos(r) * YRadius) + Y;
  Poly.Append(FPoint(x, y));
  for (int s = 0; s < v; s++) {
    x = (sin(r) * XRadius) + X;
    y = (-cos(r) * YRadius) + Y;
    Poly.Append(FPoint(x, y));
    r += i;
  }
  r = Radians(End);
  x = (sin(r) * XRadius) + X;
  y = (-cos(r) * YRadius) + Y;
  Poly.Append(FPoint(x, y));
  return FilterSimple_ConvexPolygon_AntiAlias(Image, &Poly, Color, Renderer, 0);
}

Export int FilterSimple_FilledPie(Image *Image, float X, float Y, float XRadius, float YRadius, float Start, float End, Pixel Color, RenderFunction *Renderer, float Accuracy) {
  if (abs(End - Start) > 180) {
    float d = (End - Start);
    FilterSimple_FilledPie(Image, X, Y, XRadius, YRadius, Start, Start + ceil(d / 2), Color, Renderer, Accuracy);
    return FilterSimple_FilledPie(Image, X, Y, XRadius, YRadius, Start + floor(d / 2), End, Color, Renderer, Accuracy);
  }
  SimplePolygon Poly = SimplePolygon();
  int v = ClipValue(ceil(2.0f * Pi * sqrt(pow(XRadius / 2.0f, 2.0f) + pow(YRadius / 2.0f, 2.0f)) * (abs(End-Start) / 360.0f) * Accuracy), 3, 2048);
  Poly.Allocate(v + 3);
  Poly.Append(FPoint(X, Y));
  float i = Radians((End-Start) / (float)(v));
  float x, y, r = Radians(Start);
  x = (sin(r) * XRadius) + X;
  y = (-cos(r) * YRadius) + Y;
  Poly.Append(FPoint(x, y));
  for (int s = 0; s < v; s++) {
    x = (sin(r) * XRadius) + X;
    y = (-cos(r) * YRadius) + Y;
    Poly.Append(FPoint(x, y));
    r += i;
  }
  r = Radians(End);
  x = (sin(r) * XRadius) + X;
  y = (-cos(r) * YRadius) + Y;
  Poly.Append(FPoint(x, y));
  return FilterSimple_ConvexPolygon(Image, &Poly, Color, Renderer, 0);
}

Export int FilterSimple_ConvexPolygon_Textured_AntiAlias(Image *Dest, Image *Texture, TexturedPolygon *InPoly, ScalerFunction *Scaler, RenderFunction *Renderer, DoubleWord RenderArgument) {
    if (!InPoly) {
        return Failure;
    }
    if (!Dest) {
        return Failure;
    }
    if (!Dest->initialized()) {
        return Failure;
    }
    if (!Texture) {
        return Failure;
    }
    if (!Texture->initialized()) {
        return Failure;
    }
    if (!Scaler) {
        return Failure;
    }

    {
        int overrideresult = Override::EnumOverrides(Override::FilterSimple_ConvexPolygon_Textured_AntiAlias, 6, Dest, Texture, InPoly, Scaler, Renderer, RenderArgument);
#ifdef OVERRIDES
        if (overrideresult != 0) return overrideresult;
#endif
    }

    ImageLockManager ilTexture(lockingMode, Texture);
    if (!ilTexture.performUnlock()) {
        return Failure;
    }

    TexturedPolygon *Poly;
    
    if (InPoly->VertexCount < 2) {
      return Trivial_Success;
    }

    Poly = ClipPolygon(InPoly, &(Dest->ClipRectangle));

    if (Poly == Null) {
      return Trivial_Success;
    }
    if (Poly->VertexCount < 2) {
      delete Poly;
      return Trivial_Success;
    }

    FRect Bounds;
    Bounds.X1 = Poly->MinimumX();
    Bounds.Y1 = Poly->MinimumY();
    Bounds.X2 = Poly->MaximumX();
    Bounds.Y2 = Poly->MaximumY();

//    FLine *Edges = new FLine[Poly->VertexCount];
    FLine *Edges = StaticAllocate<FLine>(EdgeBuffer, Poly->VertexCount);
    for (int e = 0; e < Poly->VertexCount; e++) {
      Edges[e].Start.X = Poly->Vertexes[e].X;
      Edges[e].Start.Y = Poly->Vertexes[e].Y;
      Edges[e].End.X = Poly->Vertexes[WrapValue(e + 1,0,Poly->VertexCount-1)].X;
      Edges[e].End.Y = Poly->Vertexes[WrapValue(e + 1,0,Poly->VertexCount-1)].Y; 
    }

    int SpanCount = ceil(abs(Bounds.Y2 - Bounds.Y1)) + 1;
    int XSpanCount = ceil(abs(Bounds.X2 - Bounds.X1)) + 1;
    int cy = 0, cx = 0;
    if ((SpanCount < 1) || (XSpanCount < 1)) {
      delete Poly;
      return Trivial_Success;
    }

//    ISpan *Spans = AllocateArray(ISpan, SpanCount);
    AASpan *Spans = StaticAllocate<AASpan>(PolyBuffer, SpanCount);
    AAColSpan *XSpans = StaticAllocate<AAColSpan>(EdgeBuffer1, XSpanCount);
    TextureCoordinate *StartCoords = StaticAllocate<TextureCoordinate>(XBuffer, SpanCount);
    TextureCoordinate *EndCoords = StaticAllocate<TextureCoordinate>(YBuffer, SpanCount);
    for (int i = 0; i < SpanCount; i++) {
        Spans[i].S = 99999;
        Spans[i].E = -99999;
        Spans[i].A1 = 0;
        Spans[i].A2 = 0;
        StartCoords[i].U.setF(0);
        StartCoords[i].V.setF(0);
        EndCoords[i].U.setF(0);
        EndCoords[i].V.setF(0);
    }
    for (int i = 0; i < XSpanCount; i++) {
        XSpans[i].S = 99999;
        XSpans[i].E = -99999;
        XSpans[i].A1 = 0;
        XSpans[i].A2 = 0;
    }

    int max_span_width = 0;
    int XOffset = _Min(Bounds.X1, Bounds.X2);
    int YOffset = _Min(Bounds.Y1, Bounds.Y2);
    float fa = 0;
    int flx, fly;

    for (int l = 0; l < Poly->VertexCount; l++) {
        int ln = WrapValue(l + 1, 0, Poly->VertexCount-1);
        float U = 0, V = 0;
        float UI = 0, VI = 0;
        TRACELINEFP_INIT(Edges[l], 2)
            U = Poly->Vertexes[l].U;
            V = Poly->Vertexes[l].V;
            UI = (Poly->Vertexes[ln].U - U) / (numpixels);
            VI = (Poly->Vertexes[ln].V - V) / (numpixels);
        TRACELINEFP_BEGIN
            flx = floor(x);
            fly = floor(y);
            cy = fly - YOffset;
            if ((cy >= 0) && (cy < SpanCount)) {
                if (flx < Spans[cy].S) {
                    Spans[cy].S = flx;
                    StartCoords[cy].U.setF(U);
                    StartCoords[cy].V.setF(V);
                    fa = 1.0f - (x - flx);
                    Spans[cy].A1 = fa * 255;
                } else if (flx == Spans[cy].S) {
                    fa = 1.0f - (x - flx);
                    Spans[cy].A1 = ((fa * 255) + Spans[cy].A1) / 2;
                }
                if (flx > Spans[cy].E) {
                    Spans[cy].E = flx;
                    EndCoords[cy].U.setF(U);
                    EndCoords[cy].V.setF(V);
                    fa = (x - flx);
                    Spans[cy].A2 = fa * 255;
                } else if (flx == Spans[cy].E) {
                    fa = (x - flx);
                    Spans[cy].A2 = ((fa * 255) + Spans[cy].A2) / 2;
                }
                max_span_width = _Max(max_span_width, (Spans[cy].E - Spans[cy].S) + 1);
            }
            cx = flx - XOffset;
            if ((cx >= 0) && (cx < XSpanCount)) {
                if (fly <= XSpans[cx].S) {
                    XSpans[cx].S = fly;
                    fa = 1.0f - (y - fly);
                    XSpans[cx].A1 = fa * 255;
                }
                if (fly >= XSpans[cx].E) {
                    XSpans[cx].E = fly;
                    fa = (y - fly);
                    XSpans[cx].A2 = fa * 255;
                }
            }
            U += UI;
            V += VI;
        TRACELINEFP_END
    }

    Pixel *Pointer;
    Pixel *CurrentPixel;
    Pixel *ScanlineTable = Null;
    float fXI, fYI;
    int X, Y, L, s, ya, a, oa = 0;
    AlphaLevel* ScaleTable = AlphaLevelLookup(0);

    //ScanlineTable = AllocateArray(Pixel, max_span_width + 2);
    ScanlineTable = StaticAllocate<Pixel>(TextureBuffer, max_span_width + 2);

    LockedRenderFunction *lRenderer = Null;
    bool needLock = true;
    {
        int overrideresult = Override::EnumOverrides(Override::GetScanlineRenderer, 4, Dest, Renderer, max_span_width + 2, RenderArgument);
  #ifdef OVERRIDES
        if (overrideresult != 0) {
          lRenderer = reinterpret_cast<LockedRenderFunction*>(overrideresult);
          needLock = false;
        }
  #endif
    }

    ImageLockManager ilDest(lockingMode, Dest);
    if (needLock) {
      if (!ilDest.performUnlock()) {
          return Failure;
      }
    }

    for (int i = 0; i < SpanCount; i++) {
        Y = i + YOffset;
        if ((Y >= Dest->ClipRectangle.Top) && (Y < Dest->ClipRectangle.bottom())) {
            if (Spans[i].E >= Spans[i].S) {
                L = Spans[i].E - Spans[i].S;
                X = ClipValue(Spans[i].E, Dest->ClipRectangle.Left, Dest->ClipRectangle.right() - 1) - ClipValue(Spans[i].S, Dest->ClipRectangle.Left, Dest->ClipRectangle.right() - 1);
                if (X) {
                    X++;
                    Pointer = Dest->fast_pointer(ClipValue(Spans[i].S, Dest->ClipRectangle.Left, Dest->ClipRectangle.right() - 1), Y);
                    fXI = (EndCoords[i].U.F() - StartCoords[i].U.F()) / L;
                    fYI = (EndCoords[i].V.F() - StartCoords[i].V.F()) / L;
                    Scaler(Texture, StartCoords[i].U.H, StartCoords[i].V.H,
                    StartCoords[i].U.L, StartCoords[i].V.L,
                    (fXI), (fYI), (int)(fXI * 65535.0) % 65535, (int)(fYI * 65535.0) % 65535,
                    L + 1, ScanlineTable);
                    CurrentPixel = ScanlineTable;
                    for (s = Spans[i].S; s <= Spans[i].E; s++) {
                      cx = s - XOffset;
                      cy = i + YOffset;
                      if ((cx >= 0) && (cx < XSpanCount)) {
                        if (cy == XSpans[cx].S) {
                          ya = XSpans[cx].A1;
                        } else if (cy == XSpans[cx].E) {
                          ya = XSpans[cx].A2;
                        } else if ((cy > XSpans[cx].S) && (cy < XSpans[cx].E)) {
                          ya = 255;
                        } else {
                          ya = 0;
                        }
                      } else {
                        ya = 0;
                      }
                      if (s == Spans[i].S) {
                        a = (ya * Spans[i].A1) / 255;
                      } else if (s == Spans[i].E) {
                        a = (ya * Spans[i].A2) / 255;
                      } else if ((s > Spans[i].S) && (s < Spans[i].E)) {
                        a = ya;
                      } else {
                        a = 0;
                      }
                      if (a) {
                        if (a != 255) {
                          if (a != oa) {
                            oa = a;
                            ScaleTable = AlphaLevelLookup(a);
                          }
                          (*CurrentPixel)[::Alpha] = AlphaFromLevel(ScaleTable, (*CurrentPixel)[::Alpha]);
                        }
                      } else {
                        *CurrentPixel = 0;
                      }
                      CurrentPixel++;
                    }
                    CurrentPixel = ScanlineTable;
                    if (Spans[i].S < Dest->ClipRectangle.Left) {
                        CurrentPixel += Dest->ClipRectangle.Left - Spans[i].S;
                    }
                    if (needLock) {
                      if (Renderer != Null) {
                          Renderer(Pointer, CurrentPixel, X, 0, RenderArgument);
                      } else {
                          while (X--) {
                              *Pointer = *CurrentPixel;
                              CurrentPixel++;
                              Pointer++;
                          }
                      }
                    } else {
                      if (lRenderer != Null) {
                        lRenderer(Dest, ClipValue(Spans[i].S, Dest->ClipRectangle.Left, Dest->ClipRectangle.right() - 1), Y, CurrentPixel, X);
                      }
                    }
                }
            }
        }
    }

//    DeleteArray(StartCoords);
//    DeleteArray(EndCoords);
//    DeleteArray(ScanlineTable);
//    DeleteArray(Spans);
//    DeleteArray(Edges);

    delete Poly;

    Dest->dirty();
    return Success;
}

Export int FilterSimple_RenderStroke(Image *Dest, Stroke *TheStroke, RenderFunction *Renderer, DoubleWord RenderArgument) {
  if (!Dest) {
      return Failure;
  }
  if (!Dest->initialized()) {
      return Failure;
  }
  if (!TheStroke) {
      return Failure;
  }

  {
      int overrideresult = Override::EnumOverrides(Override::FilterSimple_RenderStroke, 4, Dest, TheStroke, Renderer, RenderArgument);
#ifdef OVERRIDES
      if (overrideresult != 0) return overrideresult;
#endif
  }

  FRect Bounds;
  Bounds.X1 = TheStroke->MinimumX();
  Bounds.Y1 = TheStroke->MinimumY();
  Bounds.X2 = TheStroke->MaximumX();
  Bounds.Y2 = TheStroke->MaximumY();

  float xd, yd;
  float cx, cy;
  float ix, iy;
  float u;
  float pt;

  int x1 = ClipValue(floor(Bounds.X1), Dest->ClipRectangle.Left, Dest->ClipRectangle.right_exclusive());
  int y1 = ClipValue(floor(Bounds.Y1), Dest->ClipRectangle.Top, Dest->ClipRectangle.bottom_exclusive());
  int x2 = ClipValue(ceil(Bounds.X2), Dest->ClipRectangle.Left, Dest->ClipRectangle.right_exclusive());
  int y2 = ClipValue(ceil(Bounds.Y2), Dest->ClipRectangle.Left, Dest->ClipRectangle.bottom_exclusive());

  int r = 0, g = 0, b = 0, a = 0;
  int d;
  int pi;
  float mt = 0;

  Pixel* dest;
  Pixel* rowTable;
  rowTable = StaticAllocate<Pixel>(TextureBuffer, x2 - x1 + 1);
  Pixel pc;

  LockedRenderFunction *lRenderer = Null;
  bool needLock = true;
  {
        int overrideresult = Override::EnumOverrides(Override::GetScanlineRenderer, 4, Dest, Renderer, x2 - x1 + 1, RenderArgument);
#ifdef OVERRIDES
      if (overrideresult != 0) {
        lRenderer = reinterpret_cast<LockedRenderFunction*>(overrideresult);
        needLock = false;
      }
#endif
  }

  ImageLockManager ilDest(lockingMode, Dest);
  if (needLock) {
    if (!ilDest.performUnlock()) {
        return Failure;
    }
  }

  if (TheStroke->PointCount < 2) return Trivial_Success;

  int segmentCount = TheStroke->Loop ? TheStroke->PointCount : TheStroke->PointCount - 1;
  StrokeSegment* segments = StaticAllocate<StrokeSegment>(EdgeBuffer, segmentCount + 1);
  if (!segments) return Failure;
  for (int i = 0; i < segmentCount; i++) {
    segments[i].Start = &(TheStroke->Points[i]);
    segments[i].End = &(TheStroke->Points[WrapValue(i + 1, 0, TheStroke->PointCount - 1)]);
    segments[i].XL = segments[i].End->X - segments[i].Start->X;
    segments[i].YL = segments[i].End->Y - segments[i].Start->Y;
    segments[i].L = (segments[i].XL * segments[i].XL) + (segments[i].YL * segments[i].YL);
    if (TheStroke->Points[i].Thickness > mt) mt = TheStroke->Points[i].Thickness;
    if (TheStroke->Points[WrapValue(i + 1, 0, TheStroke->PointCount - 1)].Thickness > mt) mt = TheStroke->Points[WrapValue(i + 1, 0, TheStroke->PointCount - 1)].Thickness;
  }

  if (mt <= 0) return Trivial_Success;

  ISpan *Spans = StaticAllocate<ISpan>(PolyBuffer, y2 - y1 + 1);
  for (int i = y1; i <= y2; i++) {
    Spans[i - y1].S = 999999;
    Spans[i - y1].E = -999999;
  }
  for (int s = 0; s < segmentCount; s++) {
    FPoint pt[4];
    FPoint ps, pe;
    FLine edge;
    float t = _Max(segments[s].Start->Thickness, segments[s].End->Thickness) + 1.0f;
    ps.X = segments[s].Start->X;
    ps.Y = segments[s].Start->Y;
    pe.X = segments[s].End->X;
    pe.Y = segments[s].End->Y;
    float a = Radians(AngleBetween(ps, pe));
    float sa = sin(a) * t, ca = -cos(a) * t;
    ps.X -= sa; ps.Y -= ca;
    pe.X += sa; pe.Y += ca;
    a -= Radians(90);
    pt[0].X = ps.X + sin(a) * t; pt[0].Y = ps.Y + -cos(a) * t;
    a += Radians(180);
    sa = sin(a) * t; ca = -cos(a) * t;
    pt[1].X = ps.X + sa; pt[1].Y = ps.Y + ca;
    pt[2].X = pe.X + sa; pt[2].Y = pe.Y + ca;
    a -= Radians(180);
    pt[3].X = pe.X + sin(a) * t; pt[3].Y = pe.Y + -cos(a) * t;
    for (int e = 0; e < 4; e++) {
      edge.Start = pt[e];
      edge.End = pt[WrapValue(e+1, 0, 3)];
      TRACELINE_INIT(edge)
      TRACELINE_BEGIN
        if ((y >= y1) && (y <= y2)) {
            Spans[y - y1].S = _Min(Spans[y - y1].S, x - 1);
            Spans[y - y1].E = _Max(Spans[y - y1].E, x + 1);
        }
      TRACELINE_END
    }
  }
  for (int i = y1; i <= y2; i++) {
    Spans[i - y1].S = ClipValue(Spans[i - y1].S, x1, x2);
    Spans[i - y1].E = ClipValue(Spans[i - y1].E, x1, x2);
  }

  Byte SoftnessRamp[256];
  for (int i = 0; i < 256; i++) {
    SoftnessRamp[i] = ClipByte(ceil(pow(i / 255.0f, TheStroke->Softness) * 255.0f));
  }

  int SectorWidth = 16, SectorHeight = 16;
  while (mt >= (SectorWidth / 2)) {
    SectorWidth *= 2;
    SectorHeight *= 2;
  }
  int XSectors = ceil((Bounds.X2 - Bounds.X1) / ((float)SectorWidth));
  int YSectors = ceil((Bounds.Y2 - Bounds.Y1) / ((float)SectorHeight));
  if (ceil(abs(Bounds.X2 - Bounds.X1)) <= 24) {
    XSectors = 1;
    SectorWidth = x2 - x1 + 1;
  }
  if (ceil(abs(Bounds.Y2 - Bounds.Y1)) <= 24) {
    YSectors = 1;
    SectorHeight = y2 - y1 + 1;
  }
  StrokeSector* Sectors = StaticAllocate<StrokeSector>(YBuffer, (XSectors * YSectors));
  if (!Sectors) return Failure;
  int* Buf = StaticAllocate<int>(XBuffer, (segmentCount) * ((XSectors * YSectors)));
  if (!Buf) return Failure;
  for (int y = 0; y < YSectors; y++) {
    for (int x = 0; x < XSectors; x++) {
      int s = (y * XSectors) + x;
      Sectors[s].Segments = Buf;
      Buf += segmentCount;
      Sectors[s].SegmentCount = 0;
      Rectangle rct, ln;
      rct.Left = (x-1) * SectorWidth + x1;
      rct.Top = (y-1) * SectorHeight + y1;
      rct.Width = SectorWidth * 3;
      rct.Height = SectorHeight * 3;
      for (int i = 0; i < segmentCount; i++) {
        ln.Left = _Min(segments[i].Start->X, segments[i].End->X);
        ln.Top = _Min(segments[i].Start->Y, segments[i].End->Y);
        ln.setRight(_Max(segments[i].Start->X, segments[i].End->X));
        ln.setBottom(_Max(segments[i].Start->Y, segments[i].End->Y));
        if (ln.intersect(rct)) {
          Sectors[s].Segments[Sectors[s].SegmentCount] = i;
          Sectors[s].SegmentCount++;
        }
      }
    }
  }

  cy = y1;
  int ys, xs, ysi, xsi;
  ys = 0;
  ysi = 0;
  for (int y = y1; y <= y2; y++) {
    if (Spans[y - y1].E >= Spans[y - y1].S) {
      cx = Spans[y - y1].S;
      xs = floor((cx - x1) / (float)SectorWidth);
      xsi = cx - (xs * SectorWidth) - x1;
      dest = rowTable;
      int firstx = 999999, lastx = 0;
      for (int x = Spans[y - y1].S; x <= Spans[y - y1].E; x++) {
        pi = 0;
        r = g = b = a = 0;
        int s = (ys * XSectors) + xs;
        if (Sectors[s].SegmentCount) {
          for(int i = 0; i < Sectors[s].SegmentCount; i++) {
            StrokeSegment *p = &(segments[Sectors[s].Segments[i]]);
            if (p->L == 0) { 
              u = 0;
            } else {
              u = (((cx - p->Start->X) * p->XL) + ((cy - p->Start->Y) * p->YL)) / p->L;
              if (u < 0) u = 0;
              if (u > 1) u = 1;
            }
            ix = p->Start->X + (u * p->XL);
            iy = p->Start->Y + (u * p->YL);
            xd = abs(cx - ix);
            yd = abs(cy - iy);
            pt = p->Start->Thickness + ((p->End->Thickness - p->Start->Thickness) * u);
            d = 255 - ClipByte((sqrt(xd*xd + yd*yd) / pt) * 255.0f);
            if (d) {
              if (d >= pi) {
                if (firstx > x) {
                  firstx = x;
                }
                if (lastx < x) {
                  lastx = x;
                }
                if (p->Start->Color == p->End->Color) {
                  pc = p->Start->Color;
                } else {
                  pc = Pixel(Pixel(p->Start->Color), Pixel(p->End->Color), u);
                }
                pi = d;
                r = pc[::Red];
                g = pc[::Green];
                b = pc[::Blue];
                a = pc[::Alpha] * SoftnessRamp[d] / 255;
              }
            }
            ++p;
          }
          (*dest)[::Red] = ClipByteHigh(r);
          (*dest)[::Green] = ClipByteHigh(g);
          (*dest)[::Blue] = ClipByteHigh(b);
          (*dest)[::Alpha] = ClipByteHigh(a);
        } else {
          dest->V = 0;
        }
        dest++;
        cx += 1.0f;
        xsi++;
        if (xsi > SectorWidth) {
          xsi = 0;
          xs++;
        }
      }
      
      if (needLock) {
        if (Renderer != Null) {
            Renderer(Dest->pointer(Spans[y - y1].S, y), rowTable, Spans[y - y1].E - Spans[y - y1].S + 1, 0, RenderArgument);
        } else {
            int i = Spans[y - y1].E - Spans[y - y1].S + 1;
            Pixel *ptr = Dest->pointer(Spans[y - y1].S, y), *src = rowTable;
            while (i--) {
                *ptr = *src;
                src++;
                ptr++;
            }
        }
      } else {
        if (lRenderer != Null) {
          lRenderer(Dest, Spans[y - y1].S, y, rowTable, Spans[y - y1].E - Spans[y - y1].S + 1);
        }
      }
      
    }
    cy += 1.0f;
    ysi++;
    if (ysi > SectorHeight) {
      ysi = 0;
      ys++;
    }
  }

  Dest->dirty();
  return Success;
}

Export int FilterSimple_RenderStroke_Bezier(Image *Dest, Stroke *TheStroke, RenderFunction *Renderer, DoubleWord RenderArgument, float Accuracy) {
  if (!Dest) {
      return Failure;
  }
  if (!Dest->initialized()) {
      return Failure;
  }
  if (!TheStroke) {
      return Failure;
  }
  if (TheStroke->PointCount < 4) {
      return Failure;
  }

  Stroke NewStroke;
  float d;
  int count = 0;
  StrokePoint s, e;
  s = TheStroke->Points[0];
  for (int i = 3; i < TheStroke->PointCount; i += 3) {
    e = TheStroke->Points[i];
    d = sqrt(pow(e.Y - s.Y, 2) + pow(e.X - s.X, 2));
    count += ClipValue(ceil(d * Accuracy), 2, 999);
    s = e;
  }

  NewStroke.PointCount = count;
  NewStroke.Softness = TheStroke->Softness;
  NewStroke.Loop = 0;
  NewStroke.Points = AllocateArray(StrokePoint, count);
  s = TheStroke->Points[0];
  int c, o;
  float t, a, a2, a3, b, b2, b3;
  StrokePoint pt, c1, c2;
  Pixel cN, cA, cB, cC, cD;
  o = 0;
  for (int i = 3; i < TheStroke->PointCount; i += 3) {
    c1 = TheStroke->Points[i-2];
    c2 = TheStroke->Points[i-1];
    e = TheStroke->Points[i];
    d = sqrt(pow(e.Y - s.Y, 2) + pow(e.X - s.X, 2));
    c = ClipValue(ceil(d * Accuracy), 2, 999);
    for (int p = 0; p < c; p++) {
      t = (p / (float)(c - 1));
      a = t;
      a2 = a * a;
      a3 = a2 * a;
      b = 1 - t;
      b2 = b * b;
      b3 = b2 * b;
      cA.V = s.Color; cB.V = c1.Color; cC.V = c2.Color; cD.V = e.Color;
      pt.X = (s.X * b3) + (3.0f * c1.X * b2 * a) + (3.0f * c2.X * b * a2) + (e.X * a3);
      pt.Y = (s.Y * b3) + (3.0f * c1.Y * b2 * a) + (3.0f * c2.Y * b * a2) + (e.Y * a3);
      /*
      pt.Thickness = (s.Thickness * b3) + (3.0f * c1.Thickness * b2 * a) + (3.0f * c2.Thickness * b * a2) + (e.Thickness * a3);
      cN[::Blue] = (cA[::Blue] * b3) + (3.0f * cB[::Blue] * b2 * a) + (3.0f * cC[::Blue] * b * a2) + (cD[::Blue] * a3);
      cN[::Green] = (cA[::Green] * b3) + (3.0f * cB[::Green] * b2 * a) + (3.0f * cC[::Green] * b * a2) + (cD[::Green] * a3);
      cN[::Red] = (cA[::Red] * b3) + (3.0f * cB[::Red] * b2 * a) + (3.0f * cC[::Red] * b * a2) + (cD[::Red] * a3);
      cN[::Alpha] = (cA[::Alpha] * b3) + (3.0f * cB[::Alpha] * b2 * a) + (3.0f * cC[::Alpha] * b * a2) + (cD[::Alpha] * a3);
      */
      pt.Thickness = (s.Thickness * b) + (e.Thickness * a);
      cN = Pixel(cA, cD, a);
      pt.Color = cN.V;
      NewStroke.Points[p + o] = pt;
    }
    s = e;
    o += c;
  }

  int result = FilterSimple_RenderStroke(Dest, &NewStroke, Renderer, RenderArgument);

  DeleteArray(NewStroke.Points);
  return result;
}

Export int FilterSimple_Circle_AntiAlias(Image *Image, float X, float Y, float XRadius, float YRadius, Pixel Color, RenderFunction *Renderer, float Thickness, float Softness, float Accuracy) {
  Stroke Stroke;
  Stroke.Softness = Softness;
  int v = ClipValue(ceil(2.0f * Pi * sqrt(pow(XRadius / 2.0f, 2.0f) + pow(YRadius / 2.0f, 2.0f)) * Accuracy), 3, 2048);
  StrokePoint *Points = AllocateArray(StrokePoint, v);
  Stroke.Points = Points;
  Stroke.PointCount = v;
  Stroke.Loop = 1;
  float i = Radians((360.0f) / (float)(v));
  float x, y, r = 0;
  for (int s = 0; s < v; s++) {
    x = (sin(r) * XRadius) + X;
    y = (-cos(r) * YRadius) + Y;
    Points[s].X = x;
    Points[s].Y = y;
    Points[s].Color = Color.V;
    Points[s].Thickness = Thickness;
    r += i;
  }
  int result = FilterSimple_RenderStroke(Image, &Stroke, Renderer, 0);
  DeleteArray(Points);
  return result;
}

Export int FilterSimple_Pie_AntiAlias(Image *Image, float X, float Y, float XRadius, float YRadius, float Start, float End, Pixel Color, RenderFunction *Renderer, float Thickness, float Softness, float Accuracy) {
  Stroke Stroke;
  Stroke.Softness = Softness;
  int v = ClipValue(ceil(2.0f * Pi * sqrt(pow(XRadius / 2.0f, 2.0f) + pow(YRadius / 2.0f, 2.0f)) * (abs(End-Start) / 360.0f) * Accuracy), 3, 2048);
  StrokePoint *Points = AllocateArray(StrokePoint, v+3);
  Stroke.Points = Points;
  Stroke.PointCount = v+3;
  Stroke.Loop = 1;
  Stroke.Points[0].X = X;
  Stroke.Points[0].Y = Y;
  Stroke.Points[0].Color = Color.V;
  Stroke.Points[0].Thickness = Thickness;
  float i = Radians((End-Start) / (float)(v));
  float x, y, r = Radians(Start);
  x = (sin(r) * XRadius) + X;
  y = (-cos(r) * YRadius) + Y;
  Stroke.Points[1].X = x;
  Stroke.Points[1].Y = y;
  Stroke.Points[1].Color = Color.V;
  Stroke.Points[1].Thickness = Thickness;
  for (int s = 0; s < v; s++) {
    x = (sin(r) * XRadius) + X;
    y = (-cos(r) * YRadius) + Y;
    Stroke.Points[s+2].X = x;
    Stroke.Points[s+2].Y = y;
    Stroke.Points[s+2].Color = Color.V;
    Stroke.Points[s+2].Thickness = Thickness;
    r += i;
  }
  r = Radians(End);
  x = (sin(r) * XRadius) + X;
  y = (-cos(r) * YRadius) + Y;
  Stroke.Points[v+2].X = x;
  Stroke.Points[v+2].Y = y;
  Stroke.Points[v+2].Color = Color.V;
  Stroke.Points[v+2].Thickness = Thickness;
  int result = FilterSimple_RenderStroke(Image, &Stroke, Renderer, 0);
  DeleteArray(Points);
  return result;
}

Export int FilterSimple_Arc_AntiAlias(Image *Image, float X, float Y, float XRadius, float YRadius, float Start, float End, Pixel Color, RenderFunction *Renderer, float Thickness, float Softness, float Accuracy) {
  Stroke Stroke;
  Stroke.Softness = Softness;
  int v = ClipValue(ceil(2.0f * Pi * sqrt(pow(XRadius / 2.0f, 2.0f) + pow(YRadius / 2.0f, 2.0f)) * (abs(End-Start) / 360.0f) * Accuracy), 3, 2048);
  StrokePoint *Points = AllocateArray(StrokePoint, v);
  Stroke.Points = Points;
  Stroke.PointCount = v;
  Stroke.Loop = 0;
  float i = Radians((End-Start) / (float)(v - 1));
  float x, y, r = Radians(Start);
  for (int s = 0; s < v; s++) {
    x = (sin(r) * XRadius) + X;
    y = (-cos(r) * YRadius) + Y;
    Stroke.Points[s].X = x;
    Stroke.Points[s].Y = y;
    Stroke.Points[s].Color = Color.V;
    Stroke.Points[s].Thickness = Thickness;
    r += i;
  }
  int result = FilterSimple_RenderStroke(Image, &Stroke, Renderer, 0);
  DeleteArray(Points);
  return result;
}
