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
  FLine Line;
  Line.Start.X = X1;
  Line.Start.Y = Y1;
  Line.End.X = X2;
  Line.End.Y = Y2;
  if (ClipFloatLine(&(Image->ClipRectangle), &Line)) {
    int x_distance = X2 - X1, y_distance = Y2 - Y1;
    int pixel_count = (_Max(abs(x_distance), abs(y_distance)));
    float x_increment = (x_distance / (float)pixel_count), y_increment = (y_distance / (float)pixel_count);
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
    Pixel *colorTable = StaticAllocate<Pixel>(XColorBuffer, abs(Area->Width) + 2);
    if (!colorTable) return Failure;

    GenerateGradientTableFast(colorTable, Color1, Color2, abs(Area->Width) + 2);

    if (Color1[::Alpha] != Color2[::Alpha]) {

FILTERSIMPLE_ROW
FILTERSIMPLE_COL
        pCurrent->V = colorTable[iCX + 1].V;
FILTERSIMPLE_COLEND
FILTERSIMPLE_ROWEND

    } else {

FILTERSIMPLE_ROW
FILTERSIMPLE_COL
        pCurrent->V = colorTable[iCX + 1].V;
FILTERSIMPLE_COLEND
FILTERSIMPLE_ROWEND

    }

//    LookupDeallocate(colorTable);
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Gradient_Vertical)
    , Pixel Color1, Pixel Color2) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Gradient_Vertical, 2) , Color1, Color2 _FOE
FILTERSIMPLE_BEGIN
    Pixel *colorTable = StaticAllocate<Pixel>(YColorBuffer, abs(Area->Height) + 2);
    if (!colorTable) return Failure;

    GenerateGradientTableFast(colorTable, Color1, Color2, abs(Area->Height) + 2);

    if (Color1[::Alpha] != Color2[::Alpha]) {

FILTERSIMPLE_ROW
FILTERSIMPLE_COL
        // set the pixel using the row's entry in the lookup table
        pCurrent->V = colorTable[iCY + 1].V;
FILTERSIMPLE_COLEND
FILTERSIMPLE_ROWEND

    } else {

FILTERSIMPLE_ROW
FILTERSIMPLE_COL
        pCurrent->V = colorTable[iCY + 1].V;
FILTERSIMPLE_COLEND
FILTERSIMPLE_ROWEND

    }

    //LookupDeallocate(colorTable);
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Gradient_4Point)
    , Pixel Color1, Pixel Color2, Pixel Color3, Pixel Color4) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Gradient_4Point, 4) , Color1, Color2, Color3, Color4 _FOE
FILTERSIMPLE_BEGIN
    
    // just in case, 'cause it's faster and all
    if ((Color1.V == Color2.V) && (Color3.V == Color4.V) && (Color3.V == Color1.V) && (Color4.V == Color2.V)) return FilterSimple_Fill(Image, Area, Color1);
    if ((Color1.V == Color2.V) && (Color3.V == Color4.V)) return FilterSimple_Gradient_Vertical(Image, Area, Color3, Color1);
    if ((Color1.V == Color3.V) && (Color2.V == Color4.V)) return FilterSimple_Gradient_Horizontal(Image, Area, Color2, Color1);

    AlphaLevel *aSource;
    AlphaLevel *aColor1, *aColor2, *aColor3, *aColor4;
    Byte *xTable, *yTable;
    Pixel color;
    int xWeight, yWeight, xWeight2, yWeight2;
    int weights[4];

    // allocate two lookup tables
//    xTable = AllocateArray(Byte, rCoordinates.Width);
//    yTable = AllocateArray(Byte, rCoordinates.Height);
//    xTable = LookupAllocate<Byte>(rCoordinates.Width);
//    yTable = LookupAllocate<Byte>(rCoordinates.Height);
    xTable = StaticAllocate<Byte>(XBuffer, rCoordinates.Width);
    yTable = StaticAllocate<Byte>(YBuffer, rCoordinates.Height);

    // fill 'em
    for (int x = 0; x < rCoordinates.Width; x++) {
        xTable[x] = ((x) * 255) / (rCoordinates.Width - 1);
    }
    for (int y = 0; y < rCoordinates.Height; y++) {
        yTable[y] = ((y) * 255) / (rCoordinates.Height - 1);
    }

FILTERSIMPLE_ROW
    // calculate the weight/opposite weight for this row
    yWeight = yTable[iCY];
    yWeight2 = yWeight ^ 0xFF;
FILTERSIMPLE_COL
    // calculate the weight/opposite weight for this column
    xWeight = xTable[iCX];
    xWeight2 = xWeight ^ 0xFF;
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
//    aDest = AlphaLevelLookup( color[::Alpha] ^ 0xFF );
    // mix the blue, green, and red
    color[::Blue] = AlphaFromLevel(aSource, (AlphaFromLevel(aColor1, Color1[::Blue]) + AlphaFromLevel(aColor2, Color2[::Blue]) + AlphaFromLevel(aColor3, Color3[::Blue]) + AlphaFromLevel(aColor4, Color4[::Blue])));
    color[::Green] = AlphaFromLevel(aSource, (AlphaFromLevel(aColor1, Color1[::Green]) + AlphaFromLevel(aColor2, Color2[::Green]) + AlphaFromLevel(aColor3, Color3[::Green]) + AlphaFromLevel(aColor4, Color4[::Green])));
    color[::Red] = AlphaFromLevel(aSource, (AlphaFromLevel(aColor1, Color1[::Red]) + AlphaFromLevel(aColor2, Color2[::Red]) + AlphaFromLevel(aColor3, Color3[::Red]) + AlphaFromLevel(aColor4, Color4[::Red])));
    // finally set the stupid pixel
    pCurrent->V = color.V;
FILTERSIMPLE_COLEND
FILTERSIMPLE_ROWEND

    // aren't we glad it's over
//    DeleteArray(xTable);
//    DeleteArray(yTable);
    //LookupDeallocate(xTable);
    //LookupDeallocate(yTable);
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Gradient_4Edge)
    , ::Image *Edge1, ::Image *Edge2, ::Image *Edge3, ::Image *Edge4) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Gradient_4Edge, 4) , Edge1, Edge2, Edge3, Edge4 _FOE
FILTERSIMPLE_BEGIN
/*
    ProfileStart("FilterSimple_Gradient_4Edge");
    
    AlphaLevel *aColor1, *aColor2, *aColor3, *aColor4;
    Byte *xTable, *yTable;
    Pixel Color1, Color2, Color3, Color4;
    Pixel column, row;
    Pixel color;
    int xWeight, yWeight, xWeight2, yWeight2;
    int weights[4];

    // allocate two lookup tables
//    xTable = AllocateArray(Byte, rCoordinates.Width);
//    yTable = AllocateArray(Byte, rCoordinates.Height);
//    xTable = LookupAllocate<Byte>(rCoordinates.Width);
//    yTable = LookupAllocate<Byte>(rCoordinates.Height);
    xTable = StaticAllocate<Byte>(XBuffer, rCoordinates.Width);
    yTable = StaticAllocate<Byte>(YBuffer, rCoordinates.Height);

    // fill 'em
    for (int x = 0; x < rCoordinates.Width; x++) {
        xTable[x] = ((x) * 255) / (rCoordinates.Width - 1);
    }
    for (int y = 0; y < rCoordinates.Height; y++) {
        yTable[y] = ((y) * 255) / (rCoordinates.Height - 1);
    }

FILTERSIMPLE_ROW
    // calculate the weight/opposite weight for this row
    yWeight = yTable[iCY];
    yWeight2 = yWeight ^ 0xFF;
FILTERSIMPLE_COL
    // calculate the weight/opposite weight for this column
    xWeight = xTable[iCX];
    xWeight2 = xWeight ^ 0xFF;
    // calculate the four weights for the source colors
    weights[0] = yWeight2;
    weights[1] = xWeight;
    weights[2] = yWeight;
    weights[3] = xWeight2;
    // retrieve the lookup pointers
    aColor1 = AlphaLevelLookup( weights[0] );
    aColor2 = AlphaLevelLookup( weights[1] );
    aColor3 = AlphaLevelLookup( weights[2] );
    aColor4 = AlphaLevelLookup( weights[3] );
    Color1 = Edge3->getPixelClip(iCX, 0);
    Color2 = Edge4->getPixelClip(0, iCY);
    Color3 = Edge1->getPixelClip(iCX, 0);
    Color4 = Edge2->getPixelClip(0, iCY);
    // mix it up yo
    color = TableBlend(color, aColor3, Color1, aColor1);
    color = TableBlend(color, aColor4, Color2, aColor2);
    color = TableBlend(color, aColor1, Color3, aColor3);
    color = TableBlend(color, aColor2, Color4, aColor4);
    // finally set the stupid pixel
    pCurrent->V = color.V;
FILTERSIMPLE_COLEND
FILTERSIMPLE_ROWEND

    // aren't we glad it's over
//    DeleteArray(xTable);
//    DeleteArray(yTable);
    //LookupDeallocate(xTable);
    //LookupDeallocate(yTable);
    ProfileStop("FilterSimple_Gradient_4Edge");
*/
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Gradient_Horizontal_SourceAlpha)
    , Pixel Color1, Pixel Color2) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Gradient_Horizontal_SourceAlpha, 2) , Color1, Color2 _FOE
FILTERSIMPLE_BEGIN
    AlphaLevel *aDest;
    Pixel *colorTable = StaticAllocate<Pixel>(XColorBuffer, abs(Area->Width) + 2);
    if (!colorTable) return Failure;

    GenerateGradientTable(colorTable, Color1, Color2, abs(Area->Width) + 2);

    if (Color1[::Alpha] != Color2[::Alpha]) {

FILTERSIMPLE_ROW
FILTERSIMPLE_COL
        // initialize the lookup pointer for the destination pixel
        aDest = AlphaLevelLookup( colorTable[iCX + 1][::Alpha] );
        // set the pixel using the column's entry in the lookup table
        BLENDPIXEL_ALPHA_PREMULTIPLIED_OPACITY(pCurrent, pCurrent, &colorTable[iCX + 1], aDest)
FILTERSIMPLE_COLEND
FILTERSIMPLE_ROWEND

    } else {

        // we can initialize the lookup pointer before the loop since the alpha does not change between the two colors
        aDest = AlphaLevelLookup( Color1[::Alpha] ^ 0xFF );

FILTERSIMPLE_ROW
FILTERSIMPLE_COL
        // set the pixel using the column's entry in the lookup table
        BLENDPIXEL_ALPHA_PREMULTIPLIED_OPACITY(pCurrent, pCurrent, &colorTable[iCX + 1], aDest)
FILTERSIMPLE_COLEND
FILTERSIMPLE_ROWEND

    }

    //LookupDeallocate(colorTable);
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Gradient_Vertical_SourceAlpha)
    , Pixel Color1, Pixel Color2) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Gradient_Vertical_SourceAlpha, 2) , Color1, Color2 _FOE
FILTERSIMPLE_BEGIN
    AlphaLevel *aDest;
    Pixel *colorTable = StaticAllocate<Pixel>(YColorBuffer, abs(Area->Height) + 2);
    if (!colorTable) return Failure;

    GenerateGradientTable(colorTable, Color1, Color2, abs(Area->Height) + 2);

    if (Color1[::Alpha] != Color2[::Alpha]) {

FILTERSIMPLE_ROW
        // initialize the lookup pointer for the destination pixel
        aDest = AlphaLevelLookup( colorTable[iCY + 1][::Alpha] );
FILTERSIMPLE_COL
        // set the pixel using the row's entry in the lookup table
        BLENDPIXEL_ALPHA_PREMULTIPLIED_OPACITY(pCurrent, pCurrent, &colorTable[iCY + 1], aDest)
FILTERSIMPLE_COLEND
FILTERSIMPLE_ROWEND

    } else {

        // we can initialize the lookup pointer before the loop since the alpha does not change between the two colors
        aDest = AlphaLevelLookup( Color1[::Alpha] ^ 0xFF );

FILTERSIMPLE_ROW
FILTERSIMPLE_COL
        // set the pixel using the row's entry in the lookup table
        BLENDPIXEL_ALPHA_PREMULTIPLIED_OPACITY(pCurrent, pCurrent, &colorTable[iCY + 1], aDest)
FILTERSIMPLE_COLEND
FILTERSIMPLE_ROWEND

    }

    //LookupDeallocate(colorTable);
FILTERSIMPLE_END

FILTERSIMPLE_SIGNATURE(Gradient_4Point_SourceAlpha)
    , Pixel Color1, Pixel Color2, Pixel Color3, Pixel Color4) {
FILTERSIMPLE_INIT
    _FOS(FilterSimple_Gradient_4Point_SourceAlpha, 4) , Color1, Color2, Color3, Color4 _FOE
FILTERSIMPLE_BEGIN
    
    // just in case, 'cause it's faster and all
    if ((Color1.V == Color2.V) && (Color3.V == Color4.V) && (Color3.V == Color1.V) && (Color4.V == Color2.V)) return FilterSimple_Fill_SourceAlpha(Image, Area, Color1);
    if ((Color1.V == Color2.V) && (Color3.V == Color4.V)) return FilterSimple_Gradient_Vertical_SourceAlpha(Image, Area, Color3, Color1);
    if ((Color1.V == Color3.V) && (Color2.V == Color4.V)) return FilterSimple_Gradient_Horizontal_SourceAlpha(Image, Area, Color2, Color1);

    AlphaLevel *aSource, *aDest;
    AlphaLevel *aColor1, *aColor2, *aColor3, *aColor4;
    Byte *xTable, *yTable;
    Pixel color;
    int xWeight, yWeight, xWeight2, yWeight2;
    int weights[4];

    // allocate two lookup tables
//    xTable = AllocateArray(Byte, rCoordinates.Width);
//    yTable = AllocateArray(Byte, rCoordinates.Height);
//    xTable = LookupAllocate<Byte>(rCoordinates.Width);
//    yTable = LookupAllocate<Byte>(rCoordinates.Height);
    xTable = StaticAllocate<Byte>(XBuffer, rCoordinates.Width);
    yTable = StaticAllocate<Byte>(YBuffer, rCoordinates.Height);

    // fill 'em
    for (int x = 0; x < rCoordinates.Width; x++) {
        xTable[x] = ((x) * 255) / (rCoordinates.Width - 1);
    }
    for (int y = 0; y < rCoordinates.Height; y++) {
        yTable[y] = ((y) * 255) / (rCoordinates.Height - 1);
    }

FILTERSIMPLE_ROW
    // calculate the weight/opposite weight for this row
    yWeight = yTable[iCY];
    yWeight2 = yWeight ^ 0xFF;
FILTERSIMPLE_COL
    // calculate the weight/opposite weight for this column
    xWeight = xTable[iCX];
    xWeight2 = xWeight ^ 0xFF;
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
        int overrideresult = Override::EnumOverrides(Override::FilterSimple_ConvexPolygon, 3, Image, InPoly, Color);
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

    ProfileStart("ConvexPolygon()");
    Poly = ClipPolygon(InPoly, &(Image->ClipRectangle));

    if (Poly == Null) {
      ProfileStop("ConvexPolygon()");
      return Trivial_Success;
    }
    if (Poly->Count() < 2) {
      delete Poly;
      ProfileStop("ConvexPolygon()");
      return Trivial_Success;
    }

    FRect Bounds;
    Bounds.X1 = Poly->MinimumX();
    Bounds.Y1 = Poly->MinimumY();
    Bounds.X2 = Poly->MaximumX();
    Bounds.Y2 = Poly->MaximumY();

//    FLine *Edges = AllocateArray(FLine, Poly->VertexCount);
//    FLine *Edges = LookupAllocate<FLine>(Poly->VertexCount);
    FLine *Edges = StaticAllocate<FLine>(EdgeBuffer, Poly->VertexCount);
    for (int e = 0; e < Poly->VertexCount; e++) {
      Edges[e].Start = Poly->Vertexes[e];
      Edges[e].End = Poly->Vertexes[WrapValue(e + 1,0,Poly->VertexCount - 1)];
//      FilterSimple_Line_AA(Image, Edges[e].Start.X, Edges[e].Start.Y, Edges[e].End.X, Edges[e].End.Y, Pixel(255, 0, 0, 255));
    }

    int SpanCount = ceil(abs(Bounds.Y2 - Bounds.Y1)) + 1;
    int cy = 0;
    if (SpanCount < 1) {
      delete Poly;
//      DeleteArray(Edges);
//      LookupDeallocate(Edges);
      ProfileStop("ConvexPolygon()");
      return Trivial_Success;
    }

    ProfileStart("ConvexPolygon() Scan Conversion");

//    ProfileStart("ConvexPolygon() AllocateArray");
//    ISpan *Spans = AllocateArray(ISpan, SpanCount);
//    ProfileStop("ConvexPolygon() AllocateArray");
//    ISpan *Spans = LookupAllocate<ISpan>(SpanCount);
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
            if ((cy >= 0) && (cy < SpanCount)) {
                Spans[cy].S = _Min(Spans[cy].S, x);
                Spans[cy].E = _Max(Spans[cy].E, x);
            }
        TRACELINE_END
    }

    ProfileStop("ConvexPolygon() Scan Conversion");

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
//    DeleteArray(Edges);
//    DeleteArray(Spans);
//    LookupDeallocate(Edges);
//    LookupDeallocate(Spans);

    ProfileStop("ConvexPolygon()");

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

Export int GetAdditiveRenderer() {
  return (int)RenderFunction_Additive;
}

Export int GetScreenRenderer() {
  return (int)RenderFunction_Screen;
}

Export int GetSubtractiveRenderer() {
  return (int)RenderFunction_Subtractive;
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
        } else {
        }
      }
    }

    {
        int overrideresult = Override::EnumOverrides(Override::FilterSimple_ConvexPolygon_Textured, 5, Dest, Texture, InPoly, Scaler, Renderer);
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
    TextureCoordinate *StartCoords = StaticAllocate<TextureCoordinate>(XBuffer, SpanCount);
    TextureCoordinate *EndCoords = StaticAllocate<TextureCoordinate>(YBuffer, SpanCount);
    for (int i = 0; i < SpanCount; i++) {
        Spans[i].S = 99999;
        Spans[i].E = -99999;
        StartCoords[i].U.setF(0);
        StartCoords[i].V.setF(0);
        EndCoords[i].U.setF(0);
        EndCoords[i].V.setF(0);
    }

    int YOffset = _Min(Bounds.Y1, Bounds.Y2);

    for (int l = 0; l < Poly->VertexCount; l++) {
        int ln = WrapValue(l + 1, 0, Poly->VertexCount-1);
        float fx = 0, fy = 0, fd = 0;
        float fxd = Edges[l].End.X - Edges[l].Start.X, fyd = Edges[l].End.Y - Edges[l].Start.Y;
        float U = 0, V = 0;
        float UI = 0, VI = 0;
        TRACELINE_INIT(Edges[l])
            U = Poly->Vertexes[l].U;
            V = Poly->Vertexes[l].V;
            UI = (Poly->Vertexes[ln].U - U) / (numpixels-1);
            VI = (Poly->Vertexes[ln].V - V) / (numpixels-1);
        TRACELINE_BEGIN
            fd = ((float)i / (float)numpixels);
            fx = (Edges[l].Start.X + (fxd * fd)) - x;
            fy = (Edges[l].Start.Y + (fyd * fd)) - y;
            cy = y - YOffset;
            if (i <= (numpixels-1)) {
              if ((cy >= 0) && (cy < SpanCount)) {
                if (x < Spans[cy].S) {
                  Spans[cy].S = x;
                  StartCoords[cy].U.setF(U - fx);
                  StartCoords[cy].V.setF(V - fy);
                }
                if (x > Spans[cy].E) {
                  Spans[cy].E = x;
                  EndCoords[cy].U.setF(U - fx);
                  EndCoords[cy].V.setF(V - fy);
                }
                max_span_width = _Max(max_span_width, (Spans[cy].E - Spans[cy].S) + 1);
              }
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
                    fXI = (EndCoords[i].U.F() - StartCoords[i].U.F()) / L;
                    fYI = (EndCoords[i].V.F() - StartCoords[i].V.F()) / L;
                    Scaler(Texture, StartCoords[i].U.H, StartCoords[i].V.H,
                    StartCoords[i].U.L, StartCoords[i].V.L,
                    (fXI), (fYI), (int)(fXI * 65535.0) % 65535, (int)(fYI * 65535.0) % 65535,
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

    delete Poly;

    return Success;
}

/*
Export int FilterSimple_ConvexPolygon_EdgeTex(Image *Dest, EdgeTexPolygon *InPoly, RenderFunction *Renderer) {
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
    for (int i = 0; i < SpanCount; i++) {
        Spans[i].S = 99999;
        Spans[i].E = -99999;
    }

    int YOffset = _Min(Bounds.Y1, Bounds.Y2);

    for (int l = 0; l < Poly->VertexCount; l++) {
        int ln = WrapValue(l + 1, 0, Poly->VertexCount-1);
        float I = 0;
        float II = 0;
        TRACELINE_INIT(Edges[l])
            I = 0;
            II = Poly->Vertexes[l].Tex->Width / (numpixels-1);
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
                    GenerateGradientTable(ScanlineTable, StartColor[i], EndColor[i], X, 0);
                    if (Renderer != Null) {
                        Renderer(Pointer, CurrentPixel, X);
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

    delete Poly;

    return Success;
}
*/

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