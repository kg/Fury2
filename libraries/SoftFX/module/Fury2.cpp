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
#include <string.h>
#include "../header/SortLL.hpp"
#include "../header/Fury2.hpp"
#include "../header/Blend.hpp"
#include "../header/Clip.hpp"
#include "../header/Resample.hpp"
#include "../header/Polygon.hpp"
#include "../header/Blitters.hpp"
#include "../header/Filters.hpp"
#include <math.h>

int ClipFloatLine(Rectangle *Rect, FLine *Line);
int ClipLine(Rectangle *Rect, ILine *Line);
Export int ClipLine(Image *Image, ILine *Line);
Export int ClipFloatLine(Image *Image, FLine *Line);
Export int CheckLineCollide(FRect * rct, FLine * lines, int linecount);

Image* ShadowImage = Null;

using namespace std;

Export void SetShadowImage(Image* NewImage) {
    ShadowImage = NewImage;
    return;
}

inline Image* Tileset::tile(int i) {
  if (this->Initialized) {
    if ((i >= 0) && (i < (int)this->Tiles->size())) {
      return (this->Tiles->at(i));
    }
  }
  return Null;
}


inline Image* Tileset::tile(int i, short* mapTable) {
  if (!mapTable) return this->tile(i);
  if (this->Initialized) {
    if ((i >= 0) && (i < (int)this->Tiles->size())) {
      int temp = mapTable[i];
      if ((temp >= 0) && (temp < (int)this->Tiles->size())) {
        return (this->Tiles->at(temp));
      }
    }
  }
  return Null;
}

inline void Tileset::setTile(int i, Image *newTile) {
    if (this->Initialized) {
      if (this->Tiles->at(i)) {
        if (this->Tiles->at(i)->Tags[3] == (DoubleWord)this) {
          delete (this->Tiles->at(i));
          this->Tiles->at(i) = Null;
        } 
      }
      this->Tiles->at(i) = newTile;
    }
}

Export Tileset* AllocateTileset(Image *pTileset, int TileWidth, int TileHeight) {
    return new Tileset(pTileset, TileWidth, TileHeight);
}

Export Tileset* AllocateEmptyTileset(int TileCount, int TileWidth, int TileHeight) {
    return new Tileset(TileCount, TileWidth, TileHeight);
}

Export void SetTile(Tileset *pTileset, int Index, Image* NewImage) {
    return pTileset->setTile(Index, NewImage);
}

Export Image* GetTile(Tileset *pTileset, int Index) {
    return pTileset->tile(Index);
}

Export int DeallocateTileset(Tileset *pTileset) {
    if (!pTileset) return Failure;
    delete pTileset;
    return Success;
}

typedef int TileBlitter(Image *Dest, Image *Source, Rectangle *DestRect, int SourceX, int SourceY, int Opacity);
typedef int TintedTileBlitter(Image *Dest, Image *Source, Rectangle *DestRect, int SourceX, int SourceY, Pixel Tint, int Opacity);

Export int RenderTilemapLayer(TilemapLayerParam *Layer, CameraParam *Camera) {
bool temporaryAnimationMap = false;
int camerax = 0, cameray = 0;
int sx = 0, sy = 0, ex = 0, ey = 0;
int cx = 0, cy = 0;
int dx = 0, dy = 0;
int alpha = 0;
short *pRow, *pTile;
Rectangle rctDest;
Rectangle oldRect;
int maxX = 0, maxY = 0;
int mx = 0, my = 0;
int cv = 0;
TileBlitter* Blitter = Null;
TintedTileBlitter* TintBlitter = Null;
bool yClip = false;
if (!Layer) return Failure;
    if (!Camera) return Failure;
    if (!Camera->pImage) return Failure;
    if (!Layer->pTileset) return Failure;
    ProfileStart("RenderTilemapLayer()");
    oldRect = Camera->pImage->ClipRectangle;
    Camera->pImage->ClipRectangle = Camera->Rectangle;
//    Camera->pImage->fill(Pixel(0), &(Camera->Rectangle));

    maxX = Layer->Width - 1;
    maxY = Layer->Height - 1;

    alpha = (Camera->Alpha * Layer->Alpha) / 255;

    if (!Layer->pAnimationMap) {
        temporaryAnimationMap = true;
//        Layer->pAnimationMap = AllocateArray(short, Layer->pTileset->TileCount);
//        Layer->pAnimationMap = LookupAllocate<short>(Layer->pTileset->TileCount);
        Layer->pAnimationMap = StaticAllocate<short>(MappingBuffer, Layer->pTileset->TileCount);
        for (int i = 0; i < Layer->pTileset->TileCount; i++) {
          Layer->pAnimationMap[i] = i;
        }
    }

    camerax = Camera->ViewportX;
    cameray = Camera->ViewportY;

    sx = Layer->X1;
    sy = Layer->Y1;
    mx = Layer->X2 - Layer->X1;
    my = Layer->Y2 - Layer->Y1;
    if (mx <= 0) mx = (Camera->Rectangle.Width / Layer->pTileset->TileWidth) + 2;
    if (my <= 0) my = (Camera->Rectangle.Height / Layer->pTileset->TileHeight) + 2;

    // if the camera value is above one tile, we can skip those tiles entirely and only offset by the remainder
    if (camerax > 0) {
        sx += (camerax / Layer->pTileset->TileWidth);
        camerax = (camerax % Layer->pTileset->TileWidth);
    }
    if (cameray > 0) { 
        sy += (cameray / Layer->pTileset->TileHeight);
        cameray = (cameray % Layer->pTileset->TileHeight);
    }

    // clip the start and end tile values so we don't try and draw stuff that isn't there
    if (Layer->WrapX) {
		ex = sx + (Camera->Rectangle.Width / Layer->pTileset->TileWidth) + 2;
    } else {
        ex = sx + ClipValue(mx, 0, Layer->Width);
    }
    if (Layer->WrapY) {
		ey = sy + (Camera->Rectangle.Height / Layer->pTileset->TileHeight) + 2;
    } else {
        ey = sy + ClipValue(my, 0, Layer->Height);
    }

    // some final clipping
    if (Layer->WrapX) {
        ex = ClipValue(ex, sx, ex);
    } else {
        sx = ClipValue(sx, Layer->X1, Layer->Width);
        ex = ClipValue(ex, sx, Layer->Width);
    }
    if (Layer->WrapY) {
        ey = ClipValue(ey, sy, ey);
    } else {
        sy = ClipValue(sy, Layer->Y1, Layer->Height);
        ey = ClipValue(ey, sy, Layer->Height);
    }

    switch(Layer->Effect) {
    default:
    case 0:
        Blitter = BlitSimple_Normal_Opacity;
        TintBlitter = BlitSimple_Normal_Tint_Opacity;
        break;
    case 1:
        Blitter = BlitSimple_Automatic_Matte_Opacity;
        TintBlitter = BlitSimple_Matte_Tint_Opacity;
        break;
    case 2:
        Blitter = BlitSimple_Automatic_SourceAlpha_Opacity;
        TintBlitter = BlitSimple_SourceAlpha_Tint_Opacity;
        break;
    case 3:
        Blitter = BlitSimple_Additive_Opacity;
        break;
    case 4:
        Blitter = BlitSimple_Subtractive_Opacity;
        break;
    case 6:
        Blitter = BlitSimple_Screen_Opacity;
        break;
    case 7:
        Blitter = BlitSimple_Multiply_Opacity;
        break;
    case 8:
        Blitter = BlitSimple_Lightmap_Opacity;
        break;
    }

    rctDest.Width = Layer->pTileset->TileWidth;
    rctDest.Height = Layer->pTileset->TileHeight;

    if (Layer->TintColor[::Alpha]) {
      if (TintBlitter == Null) return Failure;
    } else {
      if (Blitter == Null) return Failure;
    }

    // initialize the y coordinate
    dy = -cameray;
    for (cy = sy; cy < ey; ++cy) {
        rctDest.Top = dy;
        dx = -camerax;
        if (Layer->WrapY) {
            pRow = Layer->pData + (Layer->Width * (cy % maxY));
        } else {
            pRow = Layer->pData + (Layer->Width * cy);
        }
        yClip = (dy >= Camera->pImage->ClipRectangle.Top) && ((dy + Layer->pTileset->TileHeight) < Camera->pImage->ClipRectangle.bottom());
        pTile = pRow + sx;
        if (Layer->TintColor[::Alpha]) {
          for (cx = sx; cx < ex; ++cx) {
              rctDest.Left = dx;
              if (Layer->WrapX) {
                  pTile = pRow + (cx % maxX);
              }
              cv = *pTile;
              if ((cv == Layer->MaskedTile) || (cv >= Layer->pTileset->TileCount) || (cv < 0)) {
              } else {
                  enableClipping = !((dx >= Camera->pImage->ClipRectangle.Left) && ((dx + Layer->pTileset->TileWidth) < Camera->pImage->ClipRectangle.right()) && yClip);
                  TintBlitter(Camera->pImage, Layer->pTileset->tile(cv), &rctDest, 0, 0, Layer->TintColor, alpha);
              }
              dx += Layer->pTileset->TileWidth;
              pTile++;
          }
        } else {
          for (cx = sx; cx < ex; ++cx) {
              rctDest.Left = dx;
              if (Layer->WrapX) {
                  pTile = pRow + (cx % maxX);
              }
              cv = *pTile;
              if ((cv == Layer->MaskedTile) || (cv >= Layer->pTileset->TileCount) || (cv < 0)) {
              } else {
                  enableClipping = !((dx >= Camera->pImage->ClipRectangle.Left) && ((dx + Layer->pTileset->TileWidth) < Camera->pImage->ClipRectangle.right()) && yClip);
                  Blitter(Camera->pImage, Layer->pTileset->tile(cv), &rctDest, 0, 0, alpha);
              }
              dx += Layer->pTileset->TileWidth;
              pTile++;
          }
        }
        dy += Layer->pTileset->TileHeight;
    }
    if (temporaryAnimationMap) {
//        DeleteArray(Layer->pAnimationMap);
//        LookupDeallocate(Layer->pAnimationMap);
      Layer->pAnimationMap = Null;
    }
    enableClipping = true;
    Camera->pImage->ClipRectangle = oldRect;
    ProfileStop("RenderTilemapLayer()");
    return Success;
}

Export int CollisionCheck(SpriteParam *first, SpriteParam *check, bool mustbesolid, int requiredtype, int excludedtype) {
float cw = 0;
float sw = 0;
SpriteParam * sCurrent = first;
FRect rSource = {0, 0, 0, 0}, rDest = {0, 0, 0, 0};
    if (!first) return 0;
    if (!check) return 0;
    if (!first->pNext) return 0;
    ProfileStart("CollisionCheck()");
    sw = (check->Obstruction.W) / 2;
    rSource.X1 = (check->Position.X) - (sw);
    rSource.X2 = (check->Position.X) + (sw);
    rSource.Y1 = (check->Position.Y) - (check->Obstruction.H);
    rSource.Y2 = (check->Position.Y);
    while (sCurrent) {
        if (sCurrent != check) {
            if ((sCurrent->Type != excludedtype) && (((requiredtype >= 0) && (sCurrent->Type == requiredtype)) || (requiredtype < 0))) {
                if ((sCurrent->Stats.Solid) || (!mustbesolid)) {
                    cw = (sCurrent->Obstruction.W) / 2;
                    rDest.X1 = (sCurrent->Position.X) - (cw);
                    rDest.X2 = (sCurrent->Position.X) + (cw);
                    rDest.Y1 = (sCurrent->Position.Y) - (sCurrent->Obstruction.H);
                    rDest.Y2 = (sCurrent->Position.Y);
                    if (rSource.X1 > rDest.X2) goto nevar;
                    if (rSource.Y1 > rDest.Y2) goto nevar;
                    if (rSource.X2 < rDest.X1) goto nevar;
                    if (rSource.Y2 < rDest.Y1) goto nevar;
                    ProfileStop("CollisionCheck()");
                    return sCurrent->Index;
                }
            }
        }
nevar:
        sCurrent = sCurrent->pNext;
    }
    ProfileStop("CollisionCheck()");
    return 0;
}

#define _check iCollide = -Sprite->touches(Matrix); \
    if (iCollide == 0) { \
      iCollide = CollisionCheck(List, Sprite, true, -1, -1); \
    }

Export bool ResolveCollisions(SpriteParam *List, SpriteParam *Sprite, VelocityVector &ResolvedSpeed, CollisionMatrix *Matrix) {
SpritePosition spOldPosition = Sprite->Position;
int iCollide = 0;
float fX = ResolvedSpeed.X, fY = ResolvedSpeed.Y;
bool resolved = false;
    ProfileStart("ResolveCollisions()");
    resolved = false;
    Sprite->Position = spOldPosition;
    Sprite->Position.X += ResolvedSpeed.X;
    Sprite->Position.Y += ResolvedSpeed.Y;
    _check;
    if (iCollide == 0) {
        Sprite->Position = spOldPosition;
        ProfileStop("ResolveCollisions()");
        return false;
    } else {
    }
    resolved = false;
    if (abs(fX) > 0.1) {
      while (iCollide) {
          if (iCollide > 0) {
              Sprite->Events.CollidedWith = iCollide;
          } else {
              Sprite->Events.CollidedWithMap = true;
          }
          Sprite->Position = spOldPosition;
          Sprite->Position.X += fX;
          Sprite->Position.Y += fY;
          _check;
          if (iCollide == 0) {
              resolved = true; 
              break;
          }
          fX /= 2;
          if (fX == 0) {
            fX = ResolvedSpeed.X;
            break;
          }
          if ((abs(fX) < 1)) {
              fX = 0;
          }
      }
      if (resolved) {
          Sprite->Position = spOldPosition;
          ResolvedSpeed.X = fX;
          ResolvedSpeed.Y = fY;
          ProfileStop("ResolveCollisions()");
          return true;
      }
    }

    fX = ResolvedSpeed.X; fY = ResolvedSpeed.Y;

    resolved = false;
    if (abs(fY) > 0.1) {
      while (iCollide) {
          if (iCollide > 0) {
              Sprite->Events.CollidedWith = iCollide;
          } else {
              Sprite->Events.CollidedWithMap = true;
          }
          Sprite->Position = spOldPosition;
          Sprite->Position.X += fX;
          Sprite->Position.Y += fY;
          _check;
          if (iCollide == 0) {
              resolved = true; 
              break;
          }
          fY /= 2;
          if (fY == 0) {
            fY = ResolvedSpeed.Y;
            break;
          }
          if ((abs(fY) < 1)) {
              fY = 0;
          }
      }
      if (resolved) {
          Sprite->Position = spOldPosition;
          ResolvedSpeed.X = fX;
          ResolvedSpeed.Y = fY;
          ProfileStop("ResolveCollisions()");
          return true;
      }
    }

    fX = ResolvedSpeed.X; fY = ResolvedSpeed.Y;

    resolved = false;
    if ((abs(fX) > 0.1) || (abs(fY) > 0.1)) {
      while (iCollide) {
          if (iCollide > 0) {
              Sprite->Events.CollidedWith = iCollide;
          } else {
              Sprite->Events.CollidedWithMap = true;
          }
          Sprite->Position = spOldPosition;
          Sprite->Position.X += fX;
          Sprite->Position.Y += fY;
          _check;
          if (iCollide == 0) {
              resolved = true; 
              break;
          }
          fX /= 2;
          fY /= 2;
          if ((fX == 0) && (fY == 0)) break;
          if ((abs(fX) < 1)) {
              fX = 0;
          }
          if ((abs(fY) < 1)) {
              fY = 0;
          }
      }
      if (resolved) {
          ResolvedSpeed.X = fX;
          ResolvedSpeed.Y = fY;
      } else {
          ResolvedSpeed.X = 0;
          ResolvedSpeed.Y = 0;
      }
    }

    Sprite->Position = spOldPosition;
    ProfileStop("ResolveCollisions()");
    return true;
}

#undef _check

Export int UpdateSprites(SpriteParam *List, CollisionMatrix *Matrix) {
SpriteParam *pCurrent = List;
VelocityVector vSpeed;
int iCount = 0;
bool bCollided = false;
float fRadian = 3.14159265358979 / 180.0;
    if (!List) return Failure;
    if (!Matrix) return Failure;
    ProfileStart("UpdateSprites()");
    while (pCurrent) {
        iCount++;
        
        pCurrent->Events.CollidedWith = 0;
        pCurrent->Events.CollidedWithMap = false;
        pCurrent->Events.Moved = false;
        pCurrent->Events.FadedOut = false;

        if (!(pCurrent->Stats.Cull && pCurrent->Culled)) {

            vSpeed.X = pCurrent->Velocity.X + (sin(pCurrent->Velocity.B * fRadian) * pCurrent->Velocity.V);
            vSpeed.Y = pCurrent->Velocity.Y + (-cos(pCurrent->Velocity.B * fRadian) * pCurrent->Velocity.V);

            vSpeed.X *= pCurrent->Velocity.VM;
            vSpeed.Y *= pCurrent->Velocity.VM;

            if (pCurrent->Stats.Solid) {
              bCollided = ResolveCollisions(List, pCurrent, vSpeed, Matrix);
            } else {
              bCollided = false;
            }

            pCurrent->Events.Moved = ((abs(vSpeed.X) > 0) || (abs(vSpeed.Y) > 0));
            pCurrent->Events.Changed = ((abs(pCurrent->Velocity.A) > 0) || (abs(pCurrent->Velocity.BR) > 0));
            pCurrent->Position.X += vSpeed.X;
            pCurrent->Position.Y += vSpeed.Y;
            pCurrent->Position.Z += pCurrent->Velocity.Z;

            if (pCurrent->Velocity.A != 0) {
                pCurrent->Params.Alpha += pCurrent->Velocity.A;
                if (pCurrent->Velocity.A < 0) {
                    if (pCurrent->Params.Alpha <= pCurrent->Velocity.AT) {
                        pCurrent->Params.Alpha = pCurrent->Velocity.AT;
                        pCurrent->Velocity.A = 0;
                        pCurrent->Events.FadedOut = true;
                    }
                } else if (pCurrent->Velocity.A > 0) {
                    if (pCurrent->Params.Alpha >= pCurrent->Velocity.AT) {
                        pCurrent->Params.Alpha = pCurrent->Velocity.AT;
                        pCurrent->Velocity.A = 0;
                        pCurrent->Events.FadedOut = true;
                    }
                }
            }

            if (pCurrent->Velocity.BR != 0) {
                pCurrent->Velocity.B += pCurrent->Velocity.BR;
                if (pCurrent->Velocity.BR < 0) {
                    if (pCurrent->Velocity.B <= pCurrent->Velocity.BRT) {
                        pCurrent->Velocity.B = pCurrent->Velocity.BRT;
                        pCurrent->Velocity.BR = 0;
                    }
                } else if (pCurrent->Velocity.BR > 0) {
                    if (pCurrent->Velocity.B >= pCurrent->Velocity.BRT) {
                        pCurrent->Velocity.B = pCurrent->Velocity.BRT;
                        pCurrent->Velocity.BR = 0;
                    }
                }
            }

        }

        pCurrent = pCurrent->pNext;
    }
    ProfileStop("UpdateSprites()");
    return Success;
}

Export int CullSprites(SpriteParam *List, CameraParam *Camera) {
SpriteParam *pCurrent = List;
Rectangle rctDest, rctSource, rctCopy;
float x = 0, y = 0, w = 0, h = 0;
int iCount = 0;
    if (!List) return Failure;
    if (!Camera) return Failure;
    while (pCurrent) {
        iCount++;
        if (pCurrent->Params.Alpha != 0) {
            if (pCurrent->Graphic.pImage) {
                pCurrent->Culled = true;
                w = pCurrent->Graphic.Rectangle.Width;
                h = pCurrent->Graphic.Rectangle.Height;
                x = pCurrent->Position.X - Camera->ViewportX - ((w/2) - pCurrent->Graphic.XCenter);
                y = pCurrent->Position.Y - Camera->ViewportY + (h - pCurrent->Graphic.YCenter);
                if (pCurrent->Params.Scale == 1) {
                    w /= 2;
                    if ((y) < Camera->Rectangle.Top) goto nextsprite;
                    if ((y - h) > Camera->Rectangle.bottom()) goto nextsprite;
                    if ((x + w) < Camera->Rectangle.Left) goto nextsprite;
                    if ((x - w) > Camera->Rectangle.right()) goto nextsprite;
                    rctDest.Left = ceil(x - (w));
                    rctDest.Top = ceil(y - h);
                    rctDest.Width = pCurrent->Graphic.Rectangle.Width;
                    rctDest.Height = pCurrent->Graphic.Rectangle.Height;
                    rctSource = pCurrent->Graphic.Rectangle;
                    if (Clip2D_PairToRect(&rctDest, &rctSource, &(Camera->Rectangle))) {
                        pCurrent->Culled = false;
                    }
                } else if (pCurrent->Params.Scale > 0) {
                    w *= pCurrent->Params.Scale / 2;
                    if ((y) < Camera->Rectangle.Top) goto nextsprite;
                    if ((y - h) > Camera->Rectangle.bottom()) goto nextsprite;
                    if ((x + w) < Camera->Rectangle.Left) goto nextsprite;
                    if ((x - w) > Camera->Rectangle.right()) goto nextsprite;
                    rctDest.Left = ceil(x - (w));
                    rctDest.Top = ceil(y - (h * pCurrent->Params.Scale));
                    rctDest.Width = pCurrent->Graphic.Rectangle.Width * pCurrent->Params.Scale;
                    rctDest.Height = pCurrent->Graphic.Rectangle.Height * pCurrent->Params.Scale;
                    rctSource = pCurrent->Graphic.Rectangle;
                    rctCopy = rctDest;
                    if (ClipRectangle_Rect(&rctCopy, &(Camera->Rectangle))) {
                        pCurrent->Culled = false;
                    }
                }
            }
        }
nextsprite:        
        pCurrent = pCurrent->pNext;
    }
    return iCount;
}

Export SpriteParam* SortSprites(SpriteParam *List) {
SpriteParam *result = Null;
  ProfileStart("SortSprites()");
  result = SortLinkedList<SpriteParam>(List);
  ProfileStop("SortSprites()");
  return result;
}

Export int RenderSprites(SpriteParam *Start, CameraParam *Camera) {
SpriteParam *pCurrent = Start;
Rectangle rctDest, rctSource, rctCopy;
Image *pImage;
float x = 0, y = 0, w = 0, h = 0;
float s = 0, r = 0;
float px = 0, py = 0;
Polygon<TexturedVertex> poly;
RenderFunction *renderer = Null;
bool scaled = false, rotated = false;
int iCount = 0, iTemp = 0;
Pixel white = Pixel(255,255,255,255);
    if (!Start) return Failure;
    if (!Camera) return Failure;
    if (!Camera->pImage) return Failure;
    ProfileStart("RenderSprites()");
    poly.Allocate(4);
    while (pCurrent) {
      iCount++;
      if (pCurrent->Visible) {
        if ((pCurrent->Params.Alpha != 0)) {
          if (pCurrent->Graphic.pImage) {
            w = pCurrent->Graphic.Rectangle.Width;
            h = pCurrent->Graphic.Rectangle.Height;
            x = pCurrent->Position.X - Camera->ViewportX - ((w/2) - pCurrent->Graphic.XCenter);
            y = pCurrent->Position.Y - Camera->ViewportY + (h - pCurrent->Graphic.YCenter);
            switch  (pCurrent->Params.SpecialFX) {
            default:
            case 0:
              break;
            case fxHardShadow:
              break;
            case fxSoftShadow:
              if (ShadowImage) {
                rctDest.Left = x - (pCurrent->Obstruction.W / 2.0);
                rctDest.Top = y - (pCurrent->Obstruction.H);
                rctDest.Width = pCurrent->Obstruction.W;
                rctDest.Height = pCurrent->Obstruction.H;
                rctSource = ShadowImage->getRectangle();
                BlitResample_Subtractive_Opacity(Camera->pImage, ShadowImage, &rctDest, &rctSource, DefaultSampleFunction, pCurrent->Params.Alpha * Camera->Alpha);
              }
              break;
            case fxCastShadow:
              break;
            }
            s = pCurrent->Params.Scale;
            r = pCurrent->Params.Angle;
            scaled = (s != 1);
            rotated = (((int)r) % 360) != 0;
            if ((!scaled) && (!rotated)) {
              w /= 2;
              if ((y) < Camera->Rectangle.Top) goto nextsprite;
              if ((y - h) > Camera->Rectangle.bottom()) goto nextsprite;
              if ((x + w) < Camera->Rectangle.Left) goto nextsprite;
              if ((x - w) > Camera->Rectangle.right()) goto nextsprite;
              rctDest.Left = ceil(x - (w));
              rctDest.Top = ceil(y - h);
              rctDest.Width = pCurrent->Graphic.Rectangle.Width;
              rctDest.Height = pCurrent->Graphic.Rectangle.Height;
              rctSource = pCurrent->Graphic.Rectangle;
              if (Clip2D_PairToRect(&rctDest, &rctSource, &(Camera->Rectangle))) {
                  switch(pCurrent->Params.BlitMode) {
                  default:
                  case 0:
                      iTemp = pCurrent->Graphic.pImage->MatteColor.V;
                      pCurrent->Graphic.pImage->MatteColor = pCurrent->Graphic.MaskColor;
                      if (pCurrent->Params.Color[::Alpha] > 0) {
                        BlitSimple_Matte_Tint_Opacity(Camera->pImage, pCurrent->Graphic.pImage, &rctDest, rctSource.Left, rctSource.Top, pCurrent->Params.Color, abs(pCurrent->Params.Alpha) * Camera->Alpha);
                      } else {
                        BlitSimple_Matte_Opacity(Camera->pImage, pCurrent->Graphic.pImage, &rctDest, rctSource.Left, rctSource.Top, abs(pCurrent->Params.Alpha) * Camera->Alpha);
                      }
                      pCurrent->Graphic.pImage->MatteColor.V = iTemp;
                      break;
                  case 1:
                      if (pCurrent->Params.Color[::Alpha] > 0) {
                        BlitSimple_SourceAlpha_Tint_Opacity(Camera->pImage, pCurrent->Graphic.pImage, &rctDest, rctSource.Left, rctSource.Top, pCurrent->Params.Color, abs(pCurrent->Params.Alpha) * Camera->Alpha);
                      } else {
                        BlitSimple_Automatic_SourceAlpha_Opacity(Camera->pImage, pCurrent->Graphic.pImage, &rctDest, rctSource.Left, rctSource.Top, abs(pCurrent->Params.Alpha) * Camera->Alpha);
                      }
                      break;
                  case 2:
                      BlitSimple_Additive_Opacity(Camera->pImage, pCurrent->Graphic.pImage, &rctDest, rctSource.Left, rctSource.Top, abs(pCurrent->Params.Alpha) * Camera->Alpha);
                      break;
                  case 3:
                      BlitSimple_Subtractive_Opacity(Camera->pImage, pCurrent->Graphic.pImage, &rctDest, rctSource.Left, rctSource.Top, abs(pCurrent->Params.Alpha) * Camera->Alpha);
                      break;
                  case 4:
                      // Gamma
                      break;
                  case 5:
                      BlitSimple_Screen_Opacity(Camera->pImage, pCurrent->Graphic.pImage, &rctDest, rctSource.Left, rctSource.Top, abs(pCurrent->Params.Alpha) * Camera->Alpha);
                      break;
                  case 6:
                      BlitSimple_Multiply_Opacity(Camera->pImage, pCurrent->Graphic.pImage, &rctDest, rctSource.Left, rctSource.Top, abs(pCurrent->Params.Alpha) * Camera->Alpha);
                      break;
                  case 7:
//                      BlitSimple_Lightmap_RGB_Opacity(Camera->pImage, pCurrent->Graphic.pImage, &rctDest, rctSource.Left, rctSource.Top, abs(pCurrent->Params.Alpha) * Camera->Alpha);
                      // Lightmap
                      break;
                  case 8:
                      BlitSimple_Merge_Opacity(Camera->pImage, pCurrent->Graphic.pImage, &rctDest, rctSource.Left, rctSource.Top, abs(pCurrent->Params.Alpha) * Camera->Alpha);
                      break;
                  }
              }
            } else if (pCurrent->Params.Scale > 0) {
              if (rotated) {
                switch(pCurrent->Params.BlitMode) {
                default:
                  renderer = Null;
                  break;
                case 1:
                  renderer = RenderFunction_SourceAlpha;
                  break;
                case 2:
                  renderer = RenderFunction_Additive;
                  break;
                case 3:
                  renderer = RenderFunction_Subtractive;
                  break;
                case 5:
                  renderer = RenderFunction_Screen;
                  break;
                case 8:
                  renderer = RenderFunction_Merge;
                  break;
                }
                x = pCurrent->Position.X - Camera->ViewportX - ((w/2) - pCurrent->Graphic.XCenter);
                y = pCurrent->Position.Y - Camera->ViewportY + (h - pCurrent->Graphic.YCenter);
                r *= Radian;
                y -= (h * s / 2);
                s /= 2;
                w *= s; h *= s;
                poly.Empty();
                px = -w; py = -h;
                RotatePoint(px, py, r);
                px += x; py += y;
                poly.Append(TexturedVertex(px, py, pCurrent->Graphic.Rectangle.Left, pCurrent->Graphic.Rectangle.Top));
                px = w; py = -h;
                RotatePoint(px, py, r);
                px += x; py += y;
                poly.Append(TexturedVertex(px, py, pCurrent->Graphic.Rectangle.right_exclusive(), pCurrent->Graphic.Rectangle.Top));
                px = w; py = h;
                RotatePoint(px, py, r);
                px += x; py += y;
                poly.Append(TexturedVertex(px, py, pCurrent->Graphic.Rectangle.right_exclusive(), pCurrent->Graphic.Rectangle.bottom_exclusive()));
                px = -w; py = h;
                RotatePoint(px, py, r);
                px += x; py += y;
                poly.Append(TexturedVertex(px, py, pCurrent->Graphic.Rectangle.Left, pCurrent->Graphic.Rectangle.bottom_exclusive()));
                FilterSimple_ConvexPolygon_Textured(Camera->pImage, pCurrent->Graphic.pImage, &poly, DefaultSampleFunction, renderer, pCurrent->Params.Color.V);
              } else {
                w *= pCurrent->Params.Scale / 2;
                if ((y) < Camera->Rectangle.Top) goto nextsprite;
                if ((y - h) > Camera->Rectangle.bottom()) goto nextsprite;
                if ((x + w) < Camera->Rectangle.Left) goto nextsprite;
                if ((x - w) > Camera->Rectangle.right()) goto nextsprite;
                rctDest.Left = ceil(x - (w));
                rctDest.Top = ceil(y - (h * pCurrent->Params.Scale));
                rctDest.Width = pCurrent->Graphic.Rectangle.Width * pCurrent->Params.Scale;
                rctDest.Height = pCurrent->Graphic.Rectangle.Height * pCurrent->Params.Scale;
                rctSource = pCurrent->Graphic.Rectangle;
                rctCopy = rctDest;
                if (ClipRectangle_Rect(&rctCopy, &(Camera->Rectangle))) {
                  switch(pCurrent->Params.BlitMode) {
                  default:
                  case 0:
                    BlitResample_SourceAlpha_Opacity(Camera->pImage, pCurrent->Graphic.pImage, &rctDest, &rctSource, DefaultSampleFunction, pCurrent->Params.Alpha * Camera->Alpha);
                    break;
                  case 1:
                    BlitResample_SourceAlpha_Opacity(Camera->pImage, pCurrent->Graphic.pImage, &rctDest, &rctSource, DefaultSampleFunction, pCurrent->Params.Alpha * Camera->Alpha);
                    break;
                  case 2:
                    BlitResample_Additive_Opacity(Camera->pImage, pCurrent->Graphic.pImage, &rctDest, &rctSource, DefaultSampleFunction, pCurrent->Params.Alpha * Camera->Alpha);
                    break;
                  case 3:
                    BlitResample_Subtractive_Opacity(Camera->pImage, pCurrent->Graphic.pImage, &rctDest, &rctSource, DefaultSampleFunction, pCurrent->Params.Alpha * Camera->Alpha);
                    break;
                  }
                }
              }
            }
            if (pCurrent->pAttachedGraphic) {
              if (pCurrent->pAttachedGraphic->pFrames) {
                pImage = pCurrent->pAttachedGraphic->pFrames[ClipValue(pCurrent->pAttachedGraphic->Frame,0,pCurrent->pAttachedGraphic->FrameCount - 1)];
                if (pImage) {
                  rctDest.Left = ceil(x - pCurrent->pAttachedGraphic->XCenter);
                  rctDest.Top = ceil(y - (h + (float)pImage->Height) - pCurrent->pAttachedGraphic->YCenter);
                  rctDest.Width = pImage->Width;
                  rctDest.Height = pImage->Height;
                  ModedBlit((SFX_BlitModes)(pCurrent->pAttachedGraphic->BlitMode), Camera->pImage, pImage, &rctDest, 0, 0, pCurrent->pAttachedGraphic->Alpha);
                }
              }
            }
          }
        }
      }
nextsprite:        
      pCurrent = pCurrent->pSortedNext;
      if (pCurrent == Start) break;
    }
    ProfileStop("RenderSprites()");
    poly.Deallocate();
    return iCount;
}

template <class T> inline T max(T v1, T v2) {
    if (v1 > v2) {
        return v1;
    } else {
        return v2;
    }
}

Export int RenderWindow(Image *Dest, Rectangle *Area, WindowSkinParam * wp, int SectionFlags) {
int xs = 0, ys = 0;
int xm[2] = {0,0}, ym[2] = {0,0};
Rectangle dest, source, clipper, *clip;
Rectangle old_clip;
    if (!Dest) return Failure;
    if (!wp) return Failure;
    if (!wp->pImages) return Failure;
    if (!Area) return Failure;
    if (SectionFlags <= 0) {
      SectionFlags = sfAll;
    }
    enableClipping = true;
    old_clip = Dest->ClipRectangle;
    dest = *Area;
    clip = &(Dest->ClipRectangle);
    clipper = *clip;
    clipper.Left = ClipValue(Area->Left - wp->EdgeOffsets[0], clip->Left, clip->right());
    clipper.setRight(ClipValue(Area->right() + wp->EdgeOffsets[2], clip->Left, clip->right()));
    clipper.Top = ClipValue(Area->Top - wp->EdgeOffsets[1], clip->Top, clip->bottom());
    clipper.setBottom(ClipValue(Area->bottom() + wp->EdgeOffsets[3], clip->Top, clip->bottom()));
    Dest->ClipRectangle = clipper;
    xm[0] = _Max(_Max(wp->pImages[wsTopLeft]->Width, wp->pImages[wsLeft]->Width),wp->pImages[wsBottomLeft]->Width);
    xm[1] = _Max(_Max(wp->pImages[wsTopRight]->Width, wp->pImages[wsRight]->Width),wp->pImages[wsBottomRight]->Width);
    ym[0] = _Max(_Max(wp->pImages[wsTopLeft]->Height, wp->pImages[wsTop]->Height),wp->pImages[wsTopRight]->Height);
    ym[1] = _Max(_Max(wp->pImages[wsBottomLeft]->Height, wp->pImages[wsBottom]->Height),wp->pImages[wsBottomRight]->Height);
    xs = wp->pImages[wsMiddle]->Width;
    ys = wp->pImages[wsMiddle]->Height;
    if (SectionFlags & sfMiddle) {
      source = wp->pImages[wsMiddle]->getRectangle();
      dest.setValues(Area->Left - wp->EdgeOffsets[0], Area->Top - wp->EdgeOffsets[1], 
          Area->Width + wp->EdgeOffsets[0] + wp->EdgeOffsets[2], Area->Height + wp->EdgeOffsets[1] + wp->EdgeOffsets[3]);
      switch (wp->BackgroundMode) {
      default:
      case 0:
          if ((xs <= 1) && (ys <= 1)) {
              // alpha fill
              FilterSimple_Fill_SourceAlpha_Opacity(Dest, &dest, wp->pImages[wsMiddle]->getPixel(0,0), wp->Alpha);
          } else {
              // tiled blit
              ModedTiledBlit((SFX_BlitModes)wp->RenderMode, Dest, wp->pImages[wsMiddle], &dest, wp->TintColors[wsMiddle], wp->Alpha);
          }
          break;
      case 1:
          if ((xs <= 1) && (ys <= 1)) {
              // alpha fill
              FilterSimple_Fill_SourceAlpha_Opacity(Dest, &dest, wp->pImages[wsMiddle]->getPixel(0,0), wp->Alpha);
          } else {
              // scaled blit
              ModedResampleBlit((SFX_BlitModes)wp->RenderMode, Dest, wp->pImages[wsMiddle], &dest, &source, wp->TintColors[wsMiddle], wp->Alpha);
          }
          break;
      case 2:
          // gradient
          FilterSimple_Gradient_4Point_SourceAlpha(Dest, &dest, ScaleAlpha(wp->CornerColors[0], wp->Alpha), ScaleAlpha(wp->CornerColors[1], wp->Alpha), ScaleAlpha(wp->CornerColors[2], wp->Alpha), ScaleAlpha(wp->CornerColors[3], wp->Alpha));
          break;
      case 3:
          if ((xs <= 1) && (ys <= 1)) {
              // alpha fill
              FilterSimple_Fill_SourceAlpha_Opacity(Dest, &dest, wp->pImages[wsMiddle]->getPixel(0,0), wp->Alpha);
          } else {
              // tiled blit
              ModedTiledBlit((SFX_BlitModes)wp->RenderMode, Dest, wp->pImages[wsMiddle], &dest, wp->TintColors[wsMiddle], wp->Alpha);
          }
          FilterSimple_Gradient_4Point_SourceAlpha(Dest, &dest, ScaleAlpha(wp->CornerColors[0], wp->Alpha), ScaleAlpha(wp->CornerColors[1], wp->Alpha), ScaleAlpha(wp->CornerColors[2], wp->Alpha), ScaleAlpha(wp->CornerColors[3], wp->Alpha));
          break;
      case 4:
          if ((xs <= 1) && (ys <= 1)) {
              // alpha fill
              FilterSimple_Fill_SourceAlpha_Opacity(Dest, &dest, wp->pImages[wsMiddle]->getPixel(0,0), wp->Alpha);
          } else {
              // scaled blit
              ModedResampleBlit((SFX_BlitModes)wp->RenderMode, Dest, wp->pImages[wsMiddle], &dest, &source, wp->TintColors[wsMiddle], wp->Alpha);
          }
          FilterSimple_Gradient_4Point(Dest, &dest, ScaleAlpha(wp->CornerColors[0], wp->Alpha), ScaleAlpha(wp->CornerColors[1], wp->Alpha), ScaleAlpha(wp->CornerColors[2], wp->Alpha), ScaleAlpha(wp->CornerColors[3], wp->Alpha));
          break;
      }
    }

    Dest->ClipRectangle = old_clip;

    if (SectionFlags & sfTop) {
      dest.setValues(Area->Left, Area->Top - wp->pImages[wsTop]->Height, Area->Width, wp->pImages[wsTop]->Height);
      ModedTiledBlit((SFX_BlitModes)wp->RenderMode, Dest, wp->pImages[wsTop], &dest, wp->TintColors[wsTop], wp->Alpha);
    }

    if (SectionFlags & sfBottom) {
      dest.setValues(Area->Left, Area->bottom(), Area->Width, wp->pImages[wsBottom]->Height);
      ModedTiledBlit((SFX_BlitModes)wp->RenderMode, Dest, wp->pImages[wsBottom], &dest, wp->TintColors[wsBottom], wp->Alpha);
    }

    if (SectionFlags & sfLeft) {
      dest.setValues(Area->Left - wp->pImages[wsLeft]->Width, Area->Top, wp->pImages[wsLeft]->Width, Area->Height);
      ModedTiledBlit((SFX_BlitModes)wp->RenderMode, Dest, wp->pImages[wsLeft], &dest, wp->TintColors[wsLeft], wp->Alpha);
    }

    if (SectionFlags & sfRight) {
      dest.setValues(Area->right(), Area->Top, wp->pImages[wsRight]->Width, Area->Height);
      ModedTiledBlit((SFX_BlitModes)wp->RenderMode, Dest, wp->pImages[wsRight], &dest, wp->TintColors[wsRight], wp->Alpha);
    }

    Dest->ClipRectangle = old_clip;
      
    if (SectionFlags & sfBottomRight) {
      dest.setValues(Area->right(), Area->bottom(), wp->pImages[wsBottomRight]->Width, wp->pImages[wsBottomRight]->Height);
      ModedBlit((SFX_BlitModes)wp->RenderMode, Dest, wp->pImages[wsBottomRight], &dest, 0, 0, wp->TintColors[wsBottomRight], wp->Alpha);
    }

    if (SectionFlags & sfBottomLeft) {
      dest.setValues(Area->Left - wp->pImages[wsBottomLeft]->Width, Area->bottom(), wp->pImages[wsBottomLeft]->Width, wp->pImages[wsBottomLeft]->Height);
      ModedBlit((SFX_BlitModes)wp->RenderMode, Dest, wp->pImages[wsBottomLeft], &dest, 0, 0, wp->TintColors[wsBottomLeft], wp->Alpha);
    }

    if (SectionFlags & sfTopRight) {
      dest.setValues(Area->right(), Area->Top - wp->pImages[wsTopRight]->Height, wp->pImages[wsTopRight]->Width, wp->pImages[wsTopRight]->Height);
      ModedBlit((SFX_BlitModes)wp->RenderMode, Dest, wp->pImages[wsTopRight], &dest, 0, 0, wp->TintColors[wsTopRight], wp->Alpha);
    }

    if (SectionFlags & sfTopLeft) {
      dest.setValues(Area->Left - wp->pImages[wsTopLeft]->Width, Area->Top - wp->pImages[wsTopLeft]->Height, wp->pImages[wsTopLeft]->Width, wp->pImages[wsTopLeft]->Height);
      ModedBlit((SFX_BlitModes)wp->RenderMode, Dest, wp->pImages[wsTopLeft], &dest, 0, 0, wp->TintColors[wsTopLeft], wp->Alpha);
    }

    Dest->ClipRectangle = old_clip;

    return true;
}

Export int RenderText(wchar_t *Text, Image *Dest, Rectangle *Rect, FontParam *Font, TextParam *Options) {
  basic_string<wchar_t> buffer;
  wchar_t *current;
  CharacterParam *_current = Null;
  Rectangle dest, clip, shadow, old_clip;
  Pixel shadowColor;
  // configuration/data flags
  bool done = false, invisible = false, draw_shadow = false, broke = false;
  bool draw_caret = false, draw_selection = false, locate_char = false;
  // states
  bool _break = false, _render = false, _move_down = false;
  int line_count = 0;
  int current_X = 0, maximum_X = 0;
  int index = 1, buffer_index = 1;
  int charsDrawn = 0;
  int currentChar = 0;
  if (!Text) return Failure;
  if (!Rect) return Trivial_Success;
  if (!Font) return Failure;
  if (!(Font->MapPointer)) return Failure;
  if (Font->MapCount < 1) return Failure;

  clip = *Rect;

  if (!Dest) {
    invisible = true;
  } else {
    old_clip = Dest->ClipRectangle;
//    ClipRectangle_ImageClipRect(&clip, Dest);
    Dest->clipRectangle(&clip);
    if (clip.empty()) return Trivial_Success;
    Dest->setClipRectangle(&clip);
  }

  if (!invisible) {
    draw_caret = (Options->Caret_Position > 0);
    draw_selection = ((Options->Selection_End > Options->Selection_Start) && (Options->Selection_Start > 0));
  }

  if (Options->CharFromPoint == 1) {
    Options->CharFromPoint = 0;
    locate_char = true;
  }

  enableClipping = true;
  shadowColor = Font->ShadowColor;
  shadowColor[::Alpha] = AlphaLookup(shadowColor[::Alpha], Font->FillColor[::Alpha]);
  draw_shadow = (shadowColor[::Alpha] > 0);
  
  int X = 0, buffer_X = Rect->Left + Options->Scroll_X;
  int buffer_Y = Rect->Top + Options->Scroll_Y;
  int buffer_width = 0, last_width = 0, last_X = 0;

  current = Text;

  while (!done) {
    _break = false;
    _render = false;
    _move_down = false;

    if ((buffer_Y) >= Rect->bottom()) invisible = true;

    _current = (*current <= Font->MapCount) ? Font->MapPointer[*current] : Font->MapPointer[32];

    if (*((unsigned char *)current) == 0) {
      _render = true;
      done = true;
      broke = false;
    }

    if (!done) {
      switch (*current) {
        case 0:
          // null (who cares)
          break;
        case 10:
          // line feed (ignore)
          break;
        case 8:
          // backspace
          if (buffer_width > 0) {
            buffer_width = last_width;
          } else {
            buffer_X = last_X;
          }
          break;
        case 13:
          // carriage return
          _break = true;
          break;
        case ' ': case '.':
        case ',': case ';':
        case '-': case '~':
        case '+': case '*':
        case '/': case '=':
        case ':': case '(':
        case ')': case '<':
        case '>': case '!':
        case  9 : case '?':
        case '@': case '^':
        case '_': case '`':
          broke = false;
          buffer += (*current);
		  if (*current == 9) {
			// tab
			bool stopFound = false;
			if ((Options->TabStops != Null) && (Options->TabStopCount > 0)) {
			  for (int t = 0; t < Options->TabStopCount; t++) {
			    if (Options->TabStops[t] > (buffer_width + buffer_X - Rect->Left + Options->Scroll_X)) {
			      buffer_width = Options->TabStops[t] - (buffer_X - Rect->Left + Options->Scroll_X);
			      stopFound = true;
			      break;
			    }
			  }
			}
			if (!stopFound) {
		      buffer_width += _current->XIncrement;
			}
		  } else {
	        if (_current) {
		      buffer_width += _current->XIncrement;
			}
		  }
          if (buffer.size() > 1) {
            if (((buffer_width + current_X + Options->Scroll_X) > Rect->Width)) {
              if (Font->WrapMode == 1) { 
                done = true;
                _render = true;
              } else {
                if ((buffer_width) >= Rect->Width) {
                  _render = true;
                } else {
                  _move_down = true;
                }
              }
            }
          }
          _render = true;
          break;
        default:
          buffer += (*current);
          if (_current) {
            buffer_width += _current->XIncrement;
          }
          if (((buffer_width + current_X + Options->Scroll_X) > Rect->Width) && !broke) {
            if ((buffer_width) >= Rect->Width) {
              _render = true;
            } else {
              _move_down = true;
            }
          }
          break;
      }
    }

    _render = _render || _break;

    if ((_move_down) && (Font->WrapMode == 0)) {
      if (current_X > maximum_X) maximum_X = current_X;
      current_X = 0;
      buffer_X = Rect->Left + Options->Scroll_X;
      buffer_Y += Font->BaseHeight;
      if (buffer_width >= Rect->Width) _render = true;
      line_count++;
      broke = true;
    }

    if (_render) {
      X = buffer_X;
      for (DoubleWord i = 0; i < buffer.size(); i++) {
        _current = ((buffer[i] <= Font->MapCount) && (buffer[i] >= 0)) ? Font->MapPointer[buffer[i]] : Font->MapPointer[32];
		if ((currentChar > Options->MaxChars) && (Options->MaxChars > 0)) invisible = true;

        if ((_current) && (!invisible)) {
          if (draw_caret) {
            if (buffer_index == Options->Caret_Position) {
              dest.setValues(X, buffer_Y, 1, Font->BaseHeight);
              FilterSimple_Fill_SourceAlpha(Dest, &dest, Options->Caret_Color);
            }
          }

          if (draw_selection) {
            if ((buffer_index >= Options->Selection_Start) && (buffer_index < Options->Selection_End)) {
              dest.setValues(X, buffer_Y, _current->XIncrement, Font->BaseHeight);
              FilterSimple_Fill_SourceAlpha(Dest, &dest, Options->Selection_Color);
            }
          }

          if (_current->pImage) {
            dest.Left = X + _current->XOffset;
            if (Font->BaseMode == 1) {
              dest.Top = buffer_Y + _current->YOffset;
            } else {
              dest.Top = buffer_Y + _current->YOffset + (Font->BaseHeight - _current->pImage->Height);
            }
            dest.Width = _current->pImage->Width;
            dest.Height = _current->pImage->Height;

            if (draw_shadow) {
              shadow = dest;
              shadow.Left += 1;
              shadow.Top += 1;
              
              if (Font->EffectMode == 1) {
                BlitSimple_Font_Merge_RGB_Opacity(Dest, _current->pImage, &shadow, 0, 0, shadowColor, Font->Alpha);
              } else {
                BlitSimple_Font_SourceAlpha_RGB_Opacity(Dest, _current->pImage, &shadow, 0, 0, shadowColor, Font->Alpha);
              }
            }

			charsDrawn++;
            if (Font->EffectMode == 1) {
              if (Font->FillColor.V == (DoubleWord)0xFFFFFFFF) {
                BlitSimple_Merge_Opacity(Dest, _current->pImage, &dest, 0, 0, Font->Alpha);
              } else {
                BlitSimple_Font_Merge_RGB_Opacity(Dest, _current->pImage, &dest, 0, 0, Font->FillColor, Font->Alpha);
              }
            } else {
              if (Font->FillColor.V == (DoubleWord)0xFFFFFFFF) {
                BlitSimple_Automatic_SourceAlpha_Opacity(Dest, _current->pImage, &dest, 0, 0, Font->Alpha);
              } else {
                BlitSimple_Font_SourceAlpha_RGB_Opacity(Dest, _current->pImage, &dest, 0, 0, Font->FillColor, Font->Alpha);
              }
            }
          }

        }

        if (_current) {
          if (locate_char) {
            if ((Options->CharFromPoint_X < (X + _current->XIncrement))) {
//            if ((Options->CharFromPoint_X >= X) && (Options->CharFromPoint_X < (X + _current->XIncrement))) {
              if ((Options->CharFromPoint_Y >= buffer_Y) && (Options->CharFromPoint_Y < (buffer_Y + Font->BaseHeight))) {
                Options->CharFromPoint = buffer_index;
                if (Dest) {
                  Dest->ClipRectangle = old_clip;
                }
                return Success;
              }
            }
          }
          X += _current->XIncrement;
          current_X += _current->XIncrement;
        }
        currentChar++;
        buffer_index++;
      }
      if (current_X > maximum_X) maximum_X = current_X;
      if (done && draw_caret) {
        if (buffer_index == Options->Caret_Position) {
          dest.setValues(X, buffer_Y, 1, Font->BaseHeight);
          FilterSimple_Fill_SourceAlpha(Dest, &dest, Options->Caret_Color);
        }
      }
      if (done && locate_char) {
        if ((Options->CharFromPoint_X > X) || (Options->CharFromPoint_Y > buffer_Y)) {
          Options->CharFromPoint = buffer_index;
          if (Dest) {
            Dest->ClipRectangle = old_clip;
          }
          return Success;
        }
      }
      buffer_X = buffer_X + buffer_width;
      buffer.clear();
      buffer_width = 0;
      buffer_index = index + 1;
      if (done) line_count++;
    }

    if ((_break) && (Font->WrapMode == 0)) {
      broke = true;
      if (current_X > maximum_X) maximum_X = current_X;
      current_X = 0;
      buffer_X = Rect->Left + Options->Scroll_X;
      buffer_Y += Font->BaseHeight;
      line_count++;
    }

    current++;
    index++;

    last_width = buffer_width;
    last_X = buffer_X;

  }

  if (current_X > maximum_X) maximum_X = current_X;

  if (Dest) {
    Dest->ClipRectangle = old_clip;
  }

  Options->Width = maximum_X;
  Options->Height = line_count * Font->BaseHeight;
  Options->Lines = line_count;
  return Success;
}

Export int FindSpriteOnscreen(SpriteParam *first, FRect *area, SpriteParam *exclude, bool mustbesolid, int requiredtype, int excludedtype) {
SpriteParam * sCurrent = first;
float x, y, w, h;
    if (!first) return 0;
    if (!area) return 0;
FRect rSource = *area, rDest = {0, 0, 0, 0};
    while (sCurrent) {
        if (sCurrent != exclude) {
            if ((sCurrent->Type != excludedtype) && (((requiredtype >= 0) && (sCurrent->Type == requiredtype)) || (requiredtype < 0))) {
                if ((sCurrent->Stats.Solid) || (!mustbesolid)) {
					w = sCurrent->Graphic.Rectangle.Width / 2;
					h = sCurrent->Graphic.Rectangle.Height;
					x = sCurrent->Position.X - (w - sCurrent->Graphic.XCenter);
					y = sCurrent->Position.Y + (h - sCurrent->Graphic.YCenter);
                    rDest.X1 = x - w;
                    rDest.X2 = x + w;
                    rDest.Y1 = (sCurrent->Position.Y) - (h);
                    rDest.Y2 = (sCurrent->Position.Y);
                    if (rSource.X1 > rDest.X2) goto nevar;
                    if (rSource.Y1 > rDest.Y2) goto nevar;
                    if (rSource.X2 < rDest.X1) goto nevar;
                    if (rSource.Y2 < rDest.Y1) goto nevar;
                    return sCurrent->Index;
                }
            }
        }
nevar:
        sCurrent = sCurrent->pNext;
    }
    return 0;
}

Export int FindSprite(SpriteParam *first, FRect *area, SpriteParam *exclude, bool mustbesolid, int requiredtype, int excludedtype) {
SpriteParam * sCurrent = first;
float cw;
    if (!first) return 0;
    if (!area) return 0;
FRect rSource = *area, rDest = {0, 0, 0, 0};
    while (sCurrent) {
        if (sCurrent != exclude) {
            if ((sCurrent->Type != excludedtype) && (((requiredtype >= 0) && (sCurrent->Type == requiredtype)) || (requiredtype < 0))) {
                if ((sCurrent->Stats.Solid) || (!mustbesolid)) {
                    cw = (sCurrent->Obstruction.W) / 2;
                    rDest.X1 = (sCurrent->Position.X) - (cw);
                    rDest.X2 = (sCurrent->Position.X) + (cw);
                    rDest.Y1 = (sCurrent->Position.Y) - (sCurrent->Obstruction.H);
                    rDest.Y2 = (sCurrent->Position.Y);
                    if (rSource.X1 > rDest.X2) goto nevar;
                    if (rSource.Y1 > rDest.Y2) goto nevar;
                    if (rSource.X2 < rDest.X1) goto nevar;
                    if (rSource.Y2 < rDest.Y1) goto nevar;
                    return sCurrent->Index;
                }
            }
        }
nevar:
        sCurrent = sCurrent->pNext;
    }
    return 0;
}

Export int GetClosestSprite(SpriteParam *first, SpriteParam *target, bool mustbesolid, int requiredtype, int excludedtype, float *outdistance) {
int closest = 0;
float closestdist = 999999999.0;
float xdist = 0, ydist = 0, dist = 0;
SpriteParam * sCurrent = first;
    if (!first) return 0;
    if (!target) return 0;
    while (sCurrent) {
        if (sCurrent != target) {
            if ((sCurrent->Type != excludedtype) && (((requiredtype >= 0) && (sCurrent->Type == requiredtype)) || (requiredtype < 0))) {
                if ((sCurrent->Stats.Solid) || (!mustbesolid)) {
                    xdist = abs(sCurrent->Position.X - target->Position.X);
                    ydist = abs(sCurrent->Position.Y - target->Position.Y);
                    dist = sqrt(xdist * ydist);
                    if (dist < closestdist) {
                        closestdist = dist;
                        closest = sCurrent->Index;
                    }
                }
            }
        }
        sCurrent = sCurrent->pNext;
    }
    *outdistance = closestdist;
    return closest;
}

Export int GetFarthestSprite(SpriteParam *first, SpriteParam *target, bool mustbesolid, int requiredtype, int excludedtype, float *outdistance) {
int farthest = 0;
float farthestdist = 0.0;
float xdist = 0, ydist = 0, dist = 0;
SpriteParam * sCurrent = first;
    if (!first) return 0;
    if (!target) return 0;
    while (sCurrent) {
        if (sCurrent != target) {
            if ((sCurrent->Type != excludedtype) && (((requiredtype >= 0) && (sCurrent->Type == requiredtype)) || (requiredtype < 0))) {
                if ((sCurrent->Stats.Solid) || (!mustbesolid)) {
                    xdist = abs(sCurrent->Position.X - target->Position.X);
                    ydist = abs(sCurrent->Position.Y - target->Position.Y);
                    dist = sqrt(xdist * ydist);
                    if (dist > farthestdist) {
                        farthestdist = dist;
                        farthest = sCurrent->Index;
                    }
                }
            }
        }
        sCurrent = sCurrent->pNext;
    }
    *outdistance = farthestdist;
    return farthest;
}

//
// cohen sutherland
//

inline int PointRegionCode(FPoint *pt, FRect *rgn) {
int value = 0;
    if (pt->X < rgn->X1) {
        value |= csLeft;
    }
    else if (pt->X > rgn->X2) {
        value |= csRight;
    }
    if (pt->Y < rgn->Y1) {
        value |= csTop;
    }
    else if (pt->Y > rgn->Y2) {
        value |= csBottom;
    }
    return value;
}

Export int CheckLineCollide(FRect *rct, FLine *lines, int linecount) {
int sCode = 0, eCode = 0, nCode = 0;
FLine *line = lines;
int iCount;
FPoint pt;
FLine ln;
    if (!rct) return false;
    if (!lines) return false;
    for (iCount = 1; iCount <= linecount; iCount++) {
        ln = *line;
        while(true) {
            sCode = PointRegionCode(&ln.Start, rct);
            eCode = PointRegionCode(&ln.End, rct);
            if ((sCode | eCode) == 0) {
                // trivial accept
                return iCount;
            } else if ((sCode & eCode) != 0) {
                // trivial reject
                break;
            } else {
                if(sCode == 0)
                {
                    pt = ln.Start;
                    ln.Start = ln.End;
                    ln.End = pt;
                    nCode = sCode; 
                    sCode = eCode;
                    eCode = nCode;
                } 
                // nontrivial
                if (sCode & csTop) {
                    // top
                    ln.Start.X +=(ln.End.X - ln.Start.X) * (rct->Y1 - ln.Start.Y) / (ln.End.Y - ln.Start.Y);
                    ln.Start.Y = rct->Y1;
                } else if (sCode & csBottom) {
                    // bottom
                    ln.Start.X +=(ln.End.X - ln.Start.X) * (rct->Y2 - ln.Start.Y) / (ln.End.Y - ln.Start.Y);
                    ln.Start.Y = rct->Y2;
                } else if (sCode & csRight) {
                    // right
                    ln.Start.Y +=(ln.End.Y - ln.Start.Y) * (rct->X2 - ln.Start.X) / (ln.End.X - ln.Start.X);
                    ln.Start.X = rct->X2;
                } else {
                    // left
                    ln.Start.Y +=(ln.End.Y - ln.Start.Y) * (rct->X1 - ln.Start.X) / (ln.End.X - ln.Start.X);
                    ln.Start.X = rct->X1;
                }
            }
        }
        line++;
    }
    return false;
}

int CheckLineCollide(FRect *rct, std::vector<FLine> Lines) {
int sCode = 0, eCode = 0, nCode = 0;
DoubleWord iCount;
FPoint pt;
FLine ln;
    if (!rct) return false;
    for (iCount = 0; iCount < Lines.size(); iCount++) {
        ln = Lines[iCount];
        while(true) {
            sCode = PointRegionCode(&ln.Start, rct);
            eCode = PointRegionCode(&ln.End, rct);
            if ((sCode | eCode) == 0) {
                // trivial accept
                return iCount + 1;
            } else if ((sCode & eCode) != 0) {
                // trivial reject
                break;
            } else {
                if(sCode == 0)
                {
                    pt = ln.Start;
                    ln.Start = ln.End;
                    ln.End = pt;
                    nCode = sCode; 
                    sCode = eCode;
                    eCode = nCode;
                } 
                // nontrivial
                if (sCode & csTop) {
                    // top
                    ln.Start.X +=(ln.End.X - ln.Start.X) * (rct->Y1 - ln.Start.Y) / (ln.End.Y - ln.Start.Y);
                    ln.Start.Y = rct->Y1;
                } else if (sCode & csBottom) {
                    // bottom
                    ln.Start.X +=(ln.End.X - ln.Start.X) * (rct->Y2 - ln.Start.Y) / (ln.End.Y - ln.Start.Y);
                    ln.Start.Y = rct->Y2;
                } else if (sCode & csRight) {
                    // right
                    ln.Start.Y +=(ln.End.Y - ln.Start.Y) * (rct->X2 - ln.Start.X) / (ln.End.X - ln.Start.X);
                    ln.Start.X = rct->X2;
                } else {
                    // left
                    ln.Start.Y +=(ln.End.Y - ln.Start.Y) * (rct->X1 - ln.Start.X) / (ln.End.X - ln.Start.X);
                    ln.Start.X = rct->X1;
                }
            }
        }
    }
    return false;
}

FRect SpriteParam::getRect() {
float w = this->Obstruction.W / 2;
FRect v;
    v.X1 = (this->Position.X) - (w);
    v.X2 = (this->Position.X) + (w);
    v.Y1 = (this->Position.Y) - (this->Obstruction.H);
    v.Y2 = (this->Position.Y);
    return v;
}

bool SpriteParam::touches(SpriteParam *other) {
FRect rOther;
  rOther = other->getRect();
  return this->touches(&rOther);
}

bool SpriteParam::touches(FRect *other) {
FRect rMe;
  rMe = this->getRect();
  return rMe.intersect(other);
}

bool SpriteParam::touches(SpriteParam *other, VelocityVector *other_speed) {
  return false;
}

inline int SpriteParam::touches(FLine *lines, int line_count) {
FRect rct;
  rct = this->getRect();
  return CheckLineCollide(&rct, lines, line_count);
}

inline int SpriteParam::touches(CollisionMatrix *Matrix) {
FRect rct;
  rct = this->getRect();
  if (Matrix->collisionCheck(&rct)) {
    return 1;
  } else {
    return 0;
  }
}

Export int RenderPlaneOutlines(Image *Image, Lighting::Plane *Planes, Pixel Color, int Count, float XOffset, float YOffset) {
  if (!Image) return Failure;
  if (!Planes) return Failure;
  if (Count < 1) return Failure;
  Lighting::Plane Plane;
  for (int p = 0; p < Count; p++) {
    Plane = Planes[p];
    Plane.Start.X -= XOffset;
    Plane.Start.Y -= YOffset;
    Plane.End.X -= XOffset;
    Plane.End.Y -= YOffset;
    FilterSimple_Line_AA(Image, Plane.Start.X, Plane.Start.Y, Plane.End.X, Plane.Start.Y, Color);
    FilterSimple_Line_AA(Image, Plane.Start.X, Plane.Start.Y, Plane.Start.X, Plane.End.Y, Color);
    FilterSimple_Line_AA(Image, Plane.End.X, Plane.Start.Y, Plane.End.X, Plane.End.Y, Color);
    FilterSimple_Line_AA(Image, Plane.Start.X, Plane.End.Y, Plane.End.X, Plane.End.Y, Color);
    FilterSimple_Line_AA(Image, Plane.Start.X, Plane.Start.Y - Plane.Height, Plane.End.X, Plane.Start.Y - Plane.Height, Color);
    FilterSimple_Line_AA(Image, Plane.Start.X, Plane.Start.Y - Plane.Height, Plane.Start.X, Plane.End.Y - Plane.Height, Color);
    FilterSimple_Line_AA(Image, Plane.End.X, Plane.Start.Y - Plane.Height, Plane.End.X, Plane.End.Y - Plane.Height, Color);
    FilterSimple_Line_AA(Image, Plane.Start.X, Plane.End.Y - Plane.Height, Plane.End.X, Plane.End.Y - Plane.Height, Color);
  }
  return Success;
}

Export int RenderPlaneOutlines_Masked(Image *Image, Lighting::Plane *Planes, Byte *Mask, Pixel Color, int Count, float XOffset, float YOffset) {
  if (!Image) return Failure;
  if (!Planes) return Failure;
  if (Count < 1) return Failure;
  Lighting::Plane Plane;
  Pixel PlaneColor;
  for (int p = 0; p < Count; p++) {
    Plane = Planes[p];
    Plane.Start.X -= XOffset;
    Plane.Start.Y -= YOffset;
    Plane.End.X -= XOffset;
    Plane.End.Y -= YOffset;
    PlaneColor = ScaleAlpha(Color, Mask[p]);
    FilterSimple_Line_AA(Image, Plane.Start.X, Plane.Start.Y, Plane.End.X, Plane.Start.Y, PlaneColor);
    FilterSimple_Line_AA(Image, Plane.Start.X, Plane.Start.Y, Plane.Start.X, Plane.End.Y, PlaneColor);
    FilterSimple_Line_AA(Image, Plane.End.X, Plane.Start.Y, Plane.End.X, Plane.End.Y, PlaneColor);
    FilterSimple_Line_AA(Image, Plane.Start.X, Plane.End.Y, Plane.End.X, Plane.End.Y, PlaneColor);
    FilterSimple_Line_AA(Image, Plane.Start.X, Plane.Start.Y - Plane.Height, Plane.End.X, Plane.Start.Y - Plane.Height, PlaneColor);
    FilterSimple_Line_AA(Image, Plane.Start.X, Plane.Start.Y - Plane.Height, Plane.Start.X, Plane.End.Y - Plane.Height, PlaneColor);
    FilterSimple_Line_AA(Image, Plane.End.X, Plane.Start.Y - Plane.Height, Plane.End.X, Plane.End.Y - Plane.Height, PlaneColor);
    FilterSimple_Line_AA(Image, Plane.Start.X, Plane.End.Y - Plane.Height, Plane.End.X, Plane.End.Y - Plane.Height, PlaneColor);
  }
  return Success;
}

#define aceil(n) ((n > 0) ? ceil(n) : floor(n))
#define afloor(n) ((n > 0) ? floor(n) : ceil(n))

Export int SightCheck(Lighting::Environment *Env, float FromX, float FromY, float ToX, float ToY, SpriteParam *IgnoreSprite1, SpriteParam *IgnoreSprite2) {
  FLine LightRay;
  FPoint IntersectionPoint;
  SpriteParam *Sprite;
  FLine SpriteLine;
  bool Obscured = false;
  int mx1 = 0, my1 = 0, mx2 = 0, my2 = 0, mx = 0, my = 0;
  Lighting::Sector *Sector = Null;
  std::vector<Lighting::Obstruction>::iterator Obstruction;
  LightRay.Start.X = FromX;
  LightRay.Start.Y = FromY;
  LightRay.End.X = ToX;
  LightRay.End.Y = ToY;
	mx1 = ClipValue(floor(_Min(LightRay.Start.X, LightRay.End.X) / (float)Env->Matrix->SectorWidth), 0, Env->Matrix->Width - 1);
	my1 = ClipValue(floor(_Min(LightRay.Start.Y, LightRay.End.Y) / (float)Env->Matrix->SectorHeight), 0, Env->Matrix->Height - 1);
	mx2 = ClipValue(ceil(_Max(LightRay.Start.X, LightRay.End.X) / (float)Env->Matrix->SectorWidth), 0, Env->Matrix->Width - 1);
	my2 = ClipValue(ceil(_Max(LightRay.Start.Y, LightRay.End.Y) / (float)Env->Matrix->SectorHeight), 0, Env->Matrix->Height - 1);
	for (my = my1; my <= my2; my++) {
		for (mx = mx1; mx <= mx2; mx++) {
			Sector = Env->Matrix->getSector(mx, my);
			for (Obstruction = Sector->Obstructions.begin(); Obstruction != Sector->Obstructions.end(); ++Obstruction) {
				if (LightRay.intersect((*(FLine*)&(*Obstruction)), IntersectionPoint)) {
          return 0;
				}
			}
		}
	}

  if (Env->Sprites != Null) {
    Sprite = Env->Sprites;
    while (Sprite) {
      if (Sprite == IgnoreSprite1) {
        Sprite = Sprite->pNext; 
        continue;
      }
      if (Sprite == IgnoreSprite2) {
        Sprite = Sprite->pNext; 
        continue;
      }
      switch (Sprite->Params.SpecialFX) {
      default:
        Sprite = Sprite->pNext;
        continue;
        break;
      case fxCastShadow:
      case fxCastGraphicShadow:
        break;
      }
      float s = (Sprite->Obstruction.W + Sprite->Obstruction.H) / 4;
      float h = Sprite->Obstruction.H / 2;
      float a = Radians(AngleBetween(Sprite->Position, FPoint(FromX, FromY)));          
      float sdist = Sprite->ZHeight;
      if (sdist <= 0) sdist = 10000;
      SpriteLine.Start.X = (sin(a - Radians(90)) * s) + Sprite->Position.X;
      SpriteLine.Start.Y = (-cos(a - Radians(90)) * s) + Sprite->Position.Y - h;
      SpriteLine.End.X = (sin(a + Radians(90)) * s) + Sprite->Position.X;
      SpriteLine.End.Y = (-cos(a + Radians(90)) * s) + Sprite->Position.Y - h;
      if (LightRay.intersect(SpriteLine, IntersectionPoint)) {
        return 0;
      }
      Sprite = Sprite->pNext;
    }
  }
  return 1;
}

Export Pixel RaycastPoint(Lighting::Environment *Env, float X, float Y, SpriteParam *Ignore) {
  return Lighting::Raycast(Env, X, Y, Ignore, false);
}

Pixel Lighting::Raycast(Lighting::Environment *Env, float X, float Y, SpriteParam *IgnoreSprite, bool EnableCulling) {
  Pixel Color = Env->AmbientLight;
  Pixel LightColor;
  FLine LightRay;
  FPoint IntersectionPoint;
  AlphaLevel *aDistance;
  SpriteParam *Sprite;
  FLine SpriteLine;
#ifdef ACCURATE_PYTHAGORAS
  int XDistance, YDistance;
#else
  float XDistance, YDistance;
#endif
  Byte Distance;
  float DistanceMultiplier;
  bool Obscured, Falloff;
  int mx1 = 0, my1 = 0, mx2 = 0, my2 = 0, mx = 0, my = 0;
  Lighting::Sector *Sector = Null;
  std::vector<Lighting::Obstruction>::iterator Obstruction;
  if (Env->LightCount < 1) return Color;
  ProfileStart("Raycast");
  for (int l = 0; l < Env->LightCount; ++l) {
    Obscured = false;
	  if (((!Env->Lights[l].Culled) || (!EnableCulling)) && (Env->Lights[l].Visible)) {
      Falloff = Env->Lights[l].FalloffDistance > 0;
      if (Falloff) {
        DistanceMultiplier = (255.0F / Env->Lights[l].FalloffDistance);
      }
      LightColor = Env->Lights[l].Color;
      LightRay.Start.X = Env->Lights[l].X;
      LightRay.Start.Y = Env->Lights[l].Y;
      LightRay.End.X = X;
      LightRay.End.Y = Y;
	    mx1 = ClipValue(floor(_Min(LightRay.Start.X, LightRay.End.X) / (float)Env->Matrix->SectorWidth), 0, Env->Matrix->Width - 1);
  	  my1 = ClipValue(floor(_Min(LightRay.Start.Y, LightRay.End.Y) / (float)Env->Matrix->SectorHeight), 0, Env->Matrix->Height - 1);
	    mx2 = ClipValue(ceil(_Max(LightRay.Start.X, LightRay.End.X) / (float)Env->Matrix->SectorWidth), 0, Env->Matrix->Width - 1);
	    my2 = ClipValue(ceil(_Max(LightRay.Start.Y, LightRay.End.Y) / (float)Env->Matrix->SectorHeight), 0, Env->Matrix->Height - 1);
      if (Falloff) {
        YDistance = abs(LightRay.End.Y - LightRay.Start.Y) * DistanceMultiplier;
        XDistance = abs(LightRay.End.X - LightRay.Start.X) * DistanceMultiplier;
  #ifdef ACCURATE_PYTHAGORAS
        Distance = PythagorasLookup(ClipByte(XDistance), ClipByte(YDistance));
  #else
  //      Distance = ClipByte(sqrt((XDistance * XDistance) + (YDistance * YDistance)) / 0.707106781186547);
        Distance = ClipByte(sqrt((XDistance * XDistance) + (YDistance * YDistance)) / 0.95F);
  #endif
      } else {
        Distance = 0;
      }
      if (Distance < 255) {
        Obscured = false;
        if ((Env->Lights[l].Spread < 180) && (Distance > 0)) {
          // directional
          float ls = Env->Lights[l].Spread / 2;
          float a_l = NormalizeAngle(AngleBetween(LightRay.Start, LightRay.End));
          float a_s = NormalizeAngle(Env->Lights[l].Angle - ls);
          float a_e = NormalizeAngle(Env->Lights[l].Angle + ls);
          if (a_e < a_s) {
            float a_d = a_s - a_e;
            a_l = NormalizeAngle(a_l + a_d);
            a_s = NormalizeAngle(a_s + a_d);
            a_e = NormalizeAngle(a_e + a_d);
            if ((a_l < a_s) || (a_l > a_e)) Obscured = true;
          } else {
            if ((a_l < a_s) || (a_l > a_e)) Obscured = true;
          }
        }
        if (Obscured) goto found;

		  for (my = my1; my <= my2; my++) {
			  for (mx = mx1; mx <= mx2; mx++) {
				  Sector = Env->Matrix->getSector(mx, my);
				  for (Obstruction = Sector->Obstructions.begin(); Obstruction != Sector->Obstructions.end(); ++Obstruction) {
					  if (LightRay.intersect((*(FLine*)&(*Obstruction)), IntersectionPoint)) {
						  if (IntersectionPoint.distance(&(LightRay.End)) < 2) {
						  } else {
							  Obscured = true;
							  goto found;
						  }
					  }
				  }
			  }
		  }

        if (Env->Sprites != Null) {
          Sprite = Env->Sprites;
          while (Sprite) {
            if (Sprite == IgnoreSprite) {
              Sprite = Sprite->pNext; 
              continue;
            }
            if (Sprite == Env->Lights[l].Attached) {
              Sprite = Sprite->pNext; 
              continue;
            }
            switch (Sprite->Params.SpecialFX) {
            default:
              Sprite = Sprite->pNext;
              continue;
              break;
            case fxCastShadow:
            case fxCastGraphicShadow:
              if (Falloff) {
                XDistance = Sprite->Position.X - Env->Lights[l].X;
                YDistance = Sprite->Position.Y - Env->Lights[l].Y;
                if (sqrt((float)(XDistance*XDistance)+(YDistance*YDistance)) > (Env->Lights[l].FalloffDistance * 1.1)) {
                  Sprite = Sprite->pNext;
                  continue;
                }
              }
              break;
            }
            float s = (Sprite->Obstruction.W + Sprite->Obstruction.H) / 4;
            float h = Sprite->Obstruction.H / 2;
            float a = Radians(AngleBetween(Sprite->Position, Env->Lights[l]));          
            float sdist = Sprite->ZHeight;
            if (sdist <= 0) sdist = 10000;
            SpriteLine.Start.X = (sin(a - Radians(90)) * s) + Sprite->Position.X;
            SpriteLine.Start.Y = (-cos(a - Radians(90)) * s) + Sprite->Position.Y - h;
            SpriteLine.End.X = (sin(a + Radians(90)) * s) + Sprite->Position.X;
            SpriteLine.End.Y = (-cos(a + Radians(90)) * s) + Sprite->Position.Y - h;
            if (LightRay.intersect(SpriteLine, IntersectionPoint)) {
              if (IntersectionPoint.distance(&(LightRay.End)) < 2) {
              } else {
                if (IntersectionPoint.distance(&(LightRay.End)) < sdist) {
                  Obscured = true;
                  break;
                }
              }
            }
            Sprite = Sprite->pNext;
          }
        }
		  found:

        if (!Obscured) {
          if ((Falloff) && (Distance > 0)) {
            aDistance = AlphaLevelLookup(Distance ^ 0xFF);
            Color[::Blue] = ClipByteHigh(Color[::Blue] + AlphaFromLevel(aDistance, LightColor[::Blue]));
            Color[::Green] = ClipByteHigh(Color[::Green] + AlphaFromLevel(aDistance, LightColor[::Green]));
            Color[::Red] = ClipByteHigh(Color[::Red] + AlphaFromLevel(aDistance, LightColor[::Red]));
          } else {
            Color[::Blue] = ClipByteHigh(Color[::Blue] + LightColor[::Blue]);
            Color[::Green] = ClipByteHigh(Color[::Green] + LightColor[::Green]);
            Color[::Red] = ClipByteHigh(Color[::Red] + LightColor[::Red]);
          }
        }
      }
    }
  }
  ProfileStop("Raycast");
  return Color;
}

Export int RenderLightingEnvironment(Lighting::Camera *Camera, Lighting::Environment *Environment) {
Lighting::LightSource *Light;
std::vector<Lighting::Obstruction>::iterator Obstruction;
Lighting::Plane *Plane;
Polygon<FPoint> ShadowPoly;
Polygon<GradientVertex> GradientShadowPoly;
GradientVertex ShadowVertex;
FPoint Point, LightPoint, LinePoint[4], LineCenter;
FVector Vector;
Rectangle FillRect, LightRect, CameraRect, CacheRect;
FRect LightFRect;
float LightDistance, Falloff;
SpriteParam *Sprite;
float w = 0, h = 0;
int mx1 = 0, my1 = 0, mx2 = 0, my2 = 0, mx = 0, my = 0;
Lighting::Sector *Sector = Null;
bool Ignore = false, Scaling = false;
int SpriteCount = 0;
Pixel SavedColor, LightColor;
Image* RenderTarget;
int OffsetX = 0, OffsetY = 0;
int LightIterations = 0;
float FuzzyOffset = 0, FlickerAmount = 0;
  if (!Camera) return Failure;
  if (!Environment) return Failure;
  if (!Camera->OutputBuffer) return Failure;
  if (!Camera->ScratchBuffer) return Failure;
  if (!Environment->Lights) return Failure;
  Camera->OutputBuffer->setClipRectangle(Camera->OutputRectangle);
  Camera->ScratchBuffer->setClipRectangle(Camera->OutputRectangle);

  CameraRect = Camera->OutputRectangle;
  RenderTarget = Camera->ScratchBuffer;
  OffsetX = Camera->ScrollX;
  OffsetY = Camera->ScrollY;

  if (Environment->LightCount < 1) return Trivial_Success;

  Scaling = (Camera->OutputScaleRatio != 1.0);

  for (int l = 0; l < Environment->LightCount; l++) {
    Environment->Lights[l].Culled = false;
  }

  for (int l = 0; l < Environment->LightCount; l++) {

    bool Light_Clipped = false;

    OffsetX = Camera->ScrollX;
    OffsetY = Camera->ScrollY;
    Light = &(Environment->Lights[l]);
  	Light->Culled = true;
    if (Light->Attached) {
      Light->X = Light->Attached->Position.X;
      Light->Y = Light->Attached->Position.Y;
      Light->Angle = Light->Attached->Velocity.B;
    }
    Light->Angle = NormalizeAngle(Light->Angle);
    if ((Light->Cache != Null) && (Light->CacheValid == false)) {
      CameraRect = Light->Cache->getRectangle();
      RenderTarget = Light->Cache;
      OffsetX = Light->X - Light->FalloffDistance;
      OffsetY = Light->Y - Light->FalloffDistance;
    } else {
      CameraRect = Camera->OutputRectangle;
      RenderTarget = Camera->ScratchBuffer;
    }
    RenderTarget->setClipRectangle(RenderTarget->getRectangle());
    if (Light->Visible) {
      ProfileStart("Render Light Sources");
      LightColor = Light->Color;
      if (Light->FlickerLevel != 0) {
        FlickerAmount = rand() * Light->FlickerLevel / (float)(RAND_MAX);
        if ((Light->Cache != Null)) {
        } else {
          LightColor[::Alpha] = ClipByte(LightColor[::Alpha] - (FlickerAmount * LightColor[::Alpha]));
        }
      } else {
        FlickerAmount = 0;
      }
      if (Scaling) {
        LightPoint = FPoint((Light->X - OffsetX) * Camera->OutputScaleRatio, (Light->Y - OffsetY) * Camera->OutputScaleRatio);
        LightDistance = Light->FalloffDistance * 1.1 * Camera->OutputScaleRatio;
        Falloff = Light->FalloffDistance * Camera->OutputScaleRatio;
      } else {
        LightPoint = FPoint(Light->X - OffsetX, Light->Y - OffsetY);
        LightDistance = Light->FalloffDistance * 1.1;
        Falloff = Light->FalloffDistance;
      }

      if (abs(Light->Spread) > 90) {
        // unidirectional
        if (Light->FalloffDistance <= 0) {
          // no falloff (fill)
          LightRect = CameraRect;
          LightFRect.X1 = LightRect.Left + OffsetX;
          LightFRect.Y1 = LightRect.Top + OffsetY;
          LightFRect.X2 = LightRect.right() + OffsetX;
          LightFRect.Y2 = LightRect.bottom() + OffsetY;
          Light_Clipped = false;
          Camera->ScratchBuffer->fill(LightColor);
        } else {
          // falloff (radial gradient)

          LightRect.setValuesAbsolute(LightPoint.X - Falloff, LightPoint.Y - Falloff, LightPoint.X + Falloff, LightPoint.Y + Falloff);
          LightFRect.X1 = LightRect.Left + OffsetX;
          LightFRect.Y1 = LightRect.Top + OffsetY;
          LightFRect.X2 = LightRect.right() + OffsetX;
          LightFRect.Y2 = LightRect.bottom() + OffsetY;
          Light_Clipped = !ClipRectangle_Rect(&LightRect, &CameraRect);

          if (!Light_Clipped) {
            // render the light's sphere of illumination
            RenderTarget->setClipRectangle(LightRect);
            FillRect.setValuesAbsolute(LightPoint.X - Falloff, LightPoint.Y - Falloff, LightPoint.X + Falloff, LightPoint.Y + Falloff);
            if ((Light->CacheValid) && (Light->Cache != Null)) {
            } else {
              FilterSimple_Gradient_Radial(RenderTarget, &FillRect, Light->Color, Pixel(0, 0, 0, LightColor[::Alpha]));
            }
          }
        }
      } else {
        // directional
        Camera->ScratchBuffer->setClipRectangle(Camera->ScratchBuffer->getRectangle());
        RenderTarget->fill(0);
        if (Light->FalloffDistance <= 0) {
          // no falloff (solid filled convex polygon extended to infinity)
          LightRect = CameraRect;
          LightFRect.X1 = LightRect.Left + OffsetX;
          LightFRect.Y1 = LightRect.Top + OffsetY;
          LightFRect.X2 = LightRect.right() + OffsetX;
          LightFRect.Y2 = LightRect.bottom() + OffsetY;
          Light_Clipped = false;

          ShadowPoly.Allocate(3);

          ShadowPoly.Append(LightPoint);

          Vector.X = sin((Light->Angle - (Light->Spread / 2)) * Radian);
          Vector.Y = -cos((Light->Angle - (Light->Spread / 2)) * Radian);
          Vector.Multiply(10000);
          ShadowPoly.Append(FPoint(LightPoint.X + Vector.X, LightPoint.Y + Vector.Y));

          Vector.X = sin((Light->Angle + (Light->Spread / 2)) * Radian);
          Vector.Y = -cos((Light->Angle + (Light->Spread / 2)) * Radian);
          Vector.Multiply(10000);
          ShadowPoly.Append(FPoint(LightPoint.X + Vector.X, LightPoint.Y + Vector.Y));

          ShadowPoly.Finalize();

          Camera->ScratchBuffer->setClipRectangle(LightRect);
          FilterSimple_ConvexPolygon(Camera->ScratchBuffer, &ShadowPoly, LightColor, Null, 0);
        } else {
          // falloff (gradient filled convex polygon extended to FalloffDistance pixels away)
          LightIterations = Light->Fuzziness;
          FuzzyOffset = 0;

fuzzyrender:
          GradientShadowPoly.Allocate(3);
          ShadowVertex.X = LightPoint.X;
          ShadowVertex.Y = LightPoint.Y;
          if ((Light->Fuzziness > 0) && (FuzzyOffset > 0)) {
            int fa = (255 / Light->Fuzziness);
            ShadowVertex.Color = Premultiply(ScaleAlpha<Pixel>(LightColor, ClipByte(fa))).V;
          } else {
            ShadowVertex.Color = LightColor.V;
          }
          GradientShadowPoly.Append(ShadowVertex);

          Vector.X = sin((Light->Angle - (Light->Spread / 2) + (FuzzyOffset * 2.5)) * Radian);
          Vector.Y = -cos((Light->Angle - (Light->Spread / 2) + (FuzzyOffset * 2.5)) * Radian);
          Vector.Multiply(Light->FalloffDistance * Camera->OutputScaleRatio + FuzzyOffset);
          ShadowVertex.X = LightPoint.X + Vector.X;
          ShadowVertex.Y = LightPoint.Y + Vector.Y;
          ShadowVertex.Color = Pixel(0, 0, 0, LightColor[::Alpha]).V;
          GradientShadowPoly.Append(ShadowVertex);

          Vector.X = sin((Light->Angle + (Light->Spread / 2) - (FuzzyOffset * 2.5)) * Radian);
          Vector.Y = -cos((Light->Angle + (Light->Spread / 2) - (FuzzyOffset * 2.5)) * Radian);
          Vector.Multiply(Light->FalloffDistance * Camera->OutputScaleRatio + FuzzyOffset);
          ShadowVertex.X = LightPoint.X + Vector.X;
          ShadowVertex.Y = LightPoint.Y + Vector.Y;
          ShadowVertex.Color = Pixel(0, 0, 0, LightColor[::Alpha]).V;
          GradientShadowPoly.Append(ShadowVertex);

          GradientShadowPoly.Finalize();

          if (FuzzyOffset == 0) {
            LightRect.setValuesAbsolute(GradientShadowPoly.MinimumX(), GradientShadowPoly.MinimumY(), GradientShadowPoly.MaximumX(), GradientShadowPoly.MaximumY());
            LightFRect.X1 = LightRect.Left + OffsetX;
            LightFRect.Y1 = LightRect.Top + OffsetY;
            LightFRect.X2 = LightRect.right() + OffsetX;
            LightFRect.Y2 = LightRect.bottom() + OffsetY;
            Light_Clipped = !ClipRectangle_Rect(&LightRect, &CameraRect);
          }

          if (!Light_Clipped) {
            if (FuzzyOffset == 0) RenderTarget->setClipRectangle(LightRect);
            if ((Light->CacheValid) && (Light->Cache != Null)) {
              LightRect.setValuesAbsolute(GradientShadowPoly.MinimumX(), GradientShadowPoly.MinimumY(), GradientShadowPoly.MaximumX(), GradientShadowPoly.MaximumY());
              CacheRect = LightRect;
              CacheRect.Left = CacheRect.Top = 0;
              //BlitSimple_Normal(RenderTarget, Light->Cache, &LightRect, 0, 0);
              FilterSimple_Fill(RenderTarget, &LightRect, Pixel(0, 0, 0, 255));
            } else {
              FilterSimple_ConvexPolygon_Gradient(RenderTarget, &GradientShadowPoly, RenderFunction_Additive, 0);
              if (LightIterations > 0) {
                FuzzyOffset += 1;
                LightIterations--;
                goto fuzzyrender;
              }
            }
          }
        }
      }

      ProfileStop("Render Light Sources");

	    Light->Culled = Light_Clipped;
      if (!Light_Clipped) {

	      mx1 = ClipValue(floor((Light->X - Light->FalloffDistance) / (float)Environment->Matrix->SectorWidth), 0, Environment->Matrix->Width - 1);
	      my1 = ClipValue(floor((Light->Y - Light->FalloffDistance) / (float)Environment->Matrix->SectorHeight), 0, Environment->Matrix->Height - 1);
	      mx2 = ClipValue(ceil((Light->X + Light->FalloffDistance) / (float)Environment->Matrix->SectorWidth), 0, Environment->Matrix->Width - 1);
	      my2 = ClipValue(ceil((Light->Y + Light->FalloffDistance) / (float)Environment->Matrix->SectorHeight), 0, Environment->Matrix->Height - 1);

        ShadowPoly.Allocate(4);
        ShadowPoly.SetCount(4);

        ProfileStart("Render Shadows");

        if ((Environment->Matrix) && !(Light->CacheValid && (Light->Cache != Null))) {
			  float vs = 0;
			for (my = my1; my <= my2; my++) {
				for (mx = mx1; mx <= mx2; mx++) {
					Sector = Environment->Matrix->getSector(mx, my);
					for (Obstruction = Sector->Obstructions.begin(); Obstruction != Sector->Obstructions.end(); ++Obstruction) {

						LinePoint[0] = Obstruction->Line.Start;
						LinePoint[1] = Obstruction->Line.End;
						LinePoint[0].X -= OffsetX;
						LinePoint[1].X -= OffsetX;
						LinePoint[0].Y -= OffsetY;
						LinePoint[1].Y -= OffsetY;
						if (Scaling) {
							LinePoint[0].X *= Camera->OutputScaleRatio;
							LinePoint[0].Y *= Camera->OutputScaleRatio;
							LinePoint[1].X *= Camera->OutputScaleRatio;
							LinePoint[1].Y *= Camera->OutputScaleRatio;
						}

						Vector = FVector(LightPoint, LinePoint[0]);
						vs = (sqrt((Vector.X * Vector.X) + (Vector.Y * Vector.Y)));
						Vector.X = afloor(Vector.X / vs);
						Vector.Y = afloor(Vector.Y / vs);
						ShadowPoly.SetVertex(0, FPoint(LinePoint[0].X + Vector.X, LinePoint[0].Y + Vector.Y));

						Vector = FVector(LightPoint, LinePoint[0]);
						Vector.Multiply(10000);
						ShadowPoly.SetVertex(1, FPoint(LinePoint[0].X + Vector.X, LinePoint[0].Y + Vector.Y));

						Vector = FVector(LightPoint, LinePoint[1]);
						Vector.Multiply(10000);
						ShadowPoly.SetVertex(2, FPoint(LinePoint[1].X + Vector.X, LinePoint[1].Y + Vector.Y));

						Vector = FVector(LightPoint, LinePoint[1]);
						vs = (sqrt((Vector.X * Vector.X) + (Vector.Y * Vector.Y)));
						Vector.X = afloor(Vector.X / vs);
						Vector.Y = afloor(Vector.Y / vs);
						ShadowPoly.SetVertex(3, FPoint(LinePoint[1].X + Vector.X, LinePoint[1].Y + Vector.Y));

						FilterSimple_ConvexPolygon(RenderTarget, &ShadowPoly, Pixel(0,0,0,255), Null, 0);
												
					}
				}
			}
        }

        if ((Light->Cache != Null)) {
          LightRect.Left = Light->X - Light->FalloffDistance - Camera->ScrollX;
          LightRect.Top = Light->Y - Light->FalloffDistance - Camera->ScrollY;
          LightRect.setRight(Light->X + Light->FalloffDistance - Camera->ScrollX);
          LightRect.setBottom(Light->Y + Light->FalloffDistance - Camera->ScrollY);
          CacheRect = Light->Cache->getRectangle();
          Camera->ScratchBuffer->setClipRectangle(LightRect);
          if (FlickerAmount != 0) {
            BlitSimple_Normal_Gamma(Camera->ScratchBuffer, Light->Cache, &LightRect, 0, 0, 255 - (255 * FlickerAmount));
          } else {
            BlitSimple_Normal(Camera->ScratchBuffer, Light->Cache, &LightRect, 0, 0);
          }
        }
        LightPoint.X += OffsetX;
        LightPoint.Y += OffsetY;
        CameraRect = Camera->OutputRectangle;
        RenderTarget = Camera->ScratchBuffer;
        OffsetX = Camera->ScrollX;
        OffsetY = Camera->ScrollY;
        LightPoint.X -= OffsetX;
        LightPoint.Y -= OffsetY;
  
        Sprite = Environment->Sprites;
        SpriteCount = 0;
        while (Sprite) {
          SpriteCount++;
          if (Sprite->Visible) {
            switch (Sprite->Params.SpecialFX) {
            default:
              Ignore = true;
              break;
            case fxCastShadow:
            case fxCastGraphicShadow:
              FRect SpriteRect = Sprite->getRect();
              if (SpriteRect.intersect(&LightFRect)) {
                Ignore = false;
              } else {
                Ignore = true;
              }
              break;
            }
            if (Sprite == Light->Attached) Ignore = true;

            if (!Ignore) {
              float s = (Sprite->Obstruction.W + Sprite->Obstruction.H) / 4;
              float h = Sprite->Obstruction.H / 2;
              float a = Radians(AngleBetween(Sprite->Position, *Light));
              float mul = Sprite->ZHeight;
              if (mul <= 0) mul = 10000;
              LinePoint[0].X = (sin(a - Radians(90)) * s) + Sprite->Position.X;
              LinePoint[0].Y = (-cos(a - Radians(90)) * s) + Sprite->Position.Y - h;
              LinePoint[1].X = (sin(a + Radians(90)) * s) + Sprite->Position.X;
              LinePoint[1].Y = (-cos(a + Radians(90)) * s) + Sprite->Position.Y - h;
              LinePoint[0].X -= OffsetX;
              LinePoint[1].X -= OffsetX;
              LinePoint[0].Y -= OffsetY;
              LinePoint[1].Y -= OffsetY;
              if (Scaling) {
                LinePoint[0].X *= Camera->OutputScaleRatio;
                LinePoint[0].Y *= Camera->OutputScaleRatio;
                LinePoint[1].X *= Camera->OutputScaleRatio;
                LinePoint[1].Y *= Camera->OutputScaleRatio;
              }
              ShadowPoly.SetVertex(0, LinePoint[0]);

              Vector = FVector(LightPoint, LinePoint[0]);
              Vector.Multiply(mul);
              ShadowPoly.SetVertex(1, FPoint(LinePoint[0].X + Vector.X, LinePoint[0].Y + Vector.Y));

              Vector = FVector(LightPoint, LinePoint[1]);
              Vector.Multiply(mul);
              ShadowPoly.SetVertex(2, FPoint(LinePoint[1].X + Vector.X, LinePoint[1].Y + Vector.Y));

              ShadowPoly.SetVertex(3, LinePoint[1]);

              FilterSimple_ConvexPolygon(Camera->ScratchBuffer, &ShadowPoly, Pixel(0,0,0,255), Null, 0);
            }
          }

          Sprite = Sprite->pNext;
        }

        ProfileStop("Render Shadows");

        if ((Light->Cache != Null)) {
          Light->CacheValid = 1;
        }
        if (Camera->SaturationMode == 1) {
          BlitSimple_Screen_Opacity(Camera->OutputBuffer, Camera->ScratchBuffer, &LightRect, (LightRect.Left - Camera->OutputRectangle.Left), (LightRect.Top - Camera->OutputRectangle.Top), Light->Color[::Alpha]);
        } else {
          BlitSimple_Additive_Opacity(Camera->OutputBuffer, Camera->ScratchBuffer, &LightRect, (LightRect.Left - Camera->OutputRectangle.Left), (LightRect.Top - Camera->OutputRectangle.Top), Light->Color[::Alpha]);
        }
      }
    }
  }

  OffsetX = Camera->ScrollX;
  OffsetY = Camera->ScrollY;

  ProfileStart("Calculate Sprite Illumination");
  Sprite = Environment->Sprites;
  while (Sprite) {
    if (Sprite->Visible) {
      Sprite->Params.IlluminationLevel = Raycast(Environment, Sprite->Position.X, Sprite->Position.Y, Sprite, false);
    } else {
      Sprite->Params.IlluminationLevel = Environment->AmbientLight;      
    }
    Sprite = Sprite->pNext;
  }
  ProfileStop("Calculate Sprite Illumination");

  Lighting::sort_entry *SortEntries = Null;
  Lighting::sort_entry *FirstSortEntry = Null;
  int SortEntryCount = 0, SortBufferCount = 0;
  ProfileStart("Sort Planes & Sprites");
  {
    int y1 = 0, y2 = 0;
    SortBufferCount = Environment->PlaneCount + SpriteCount + 1;
    SortEntries = StaticAllocate<Lighting::sort_entry>(ListBuffer, SortBufferCount + 4) + 2;
	  _Fill<Byte>(SortEntries, 0, sizeof(Lighting::sort_entry) * SortBufferCount);
    if (Environment->Planes) {
      Rectangle rctWall, rctTop;
      for (int i = 0; i < Environment->PlaneCount; i++) {
        {
          Plane = &(Environment->Planes[i]);
          rctWall = Plane->fullRect();
          y2 = rctWall.bottom();
          rctWall.translate(-OffsetX, -OffsetY);
          if (ClipRectangle_Rect(&rctWall, &CameraRect)) {
            SortEntries[SortEntryCount].type = Lighting::plane;
            SortEntries[SortEntryCount].y = y2;
            SortEntries[SortEntryCount].Plane = Plane;
            SortEntries[SortEntryCount].pNext = &(SortEntries[SortEntryCount + 1]);
            SortEntryCount++;
          }
        }
      }
    }
    if (Environment->Sprites) {
      Rectangle rctSprite;
      Sprite = Environment->Sprites;
      while (Sprite) {
        if ((Sprite->Visible) && (Sprite->Params.DiffuseLight)) {
          if (Sprite->Params.Alpha != 0) {
            rctSprite = Sprite->getRectangle();
            y2 = rctSprite.bottom();
            rctSprite.translate(-OffsetX, -OffsetY);
            if (ClipRectangle_Rect(&rctSprite, &CameraRect)) {
              SortEntries[SortEntryCount].type = Lighting::sprite;
              SortEntries[SortEntryCount].y = y2 + Sprite->Position.Z;
              SortEntries[SortEntryCount].Sprite = Sprite;
              SortEntries[SortEntryCount].pNext = &(SortEntries[SortEntryCount + 1]);
              SortEntryCount++;
            }
          }
        }
        Sprite = Sprite->pNext;
      }
    }
    SortEntries[SortEntryCount - 1].pNext = Null;
    FirstSortEntry = SortLinkedList<Lighting::sort_entry>(&(SortEntries[0]));
  }
  ProfileStop("Sort Planes & Sprites");

  ProfileStart("Render Planes & Sprites");
  Lighting::sort_entry *SortEntry = FirstSortEntry;
  {
    Rectangle rctDest, rctSource, rctCopy;
    Rectangle rctWall, rctTop;
    float x = 0, y = 0, s = 0, r = 0;
  	int xo = 0;
    DoubleWord n = 0;
    bool scaled = false, rotated = false;
    DoubleWord iTemp = 0;

    TexturedPolygon PlanePoly;
    Image *PlaneTexture;
    FPoint TexPoint[2];
    PlanePoly.Allocate(4);
    PlanePoly.SetCount(4);
//    PlaneTexture = new Image(StaticAllocate<Pixel>(TextureBuffer, 256), 256, 1, 0);

    while (SortEntry) {
      if (SortEntry->type == Lighting::sprite) {
        ProfileStart("Render Sprites");
        Sprite = SortEntry->Sprite;
        SavedColor = Sprite->Params.IlluminationLevel;
        SavedColor[::Alpha] = 255;
        w = Sprite->Graphic.Rectangle.Width / 2; h = Sprite->Graphic.Rectangle.Height;
        x = Sprite->Position.X - OffsetX - ((w) - Sprite->Graphic.XCenter);
        y = Sprite->Position.Y - OffsetY + (h - Sprite->Graphic.YCenter);
        s = Sprite->Params.Scale; r = Sprite->Params.Angle;
        scaled = (s != 1); rotated = (((int)r) % 360) != 0;
        if ((!scaled) && (!rotated)) {
          rctDest.Left = ceil(x - (w)); rctDest.Top = ceil(y - h);
          rctDest.Width = Sprite->Graphic.Rectangle.Width; rctDest.Height = Sprite->Graphic.Rectangle.Height;
          rctSource = Sprite->Graphic.Rectangle;
          if (Clip2D_PairToRect(&rctDest, &rctSource, &(Camera->OutputRectangle))) {
            switch(Sprite->Params.BlitMode) {
            default: case 0:
              iTemp = Sprite->Graphic.pImage->MatteColor.V;
              Sprite->Graphic.pImage->MatteColor = Sprite->Graphic.MaskColor;
              BlitSimple_Matte_Tint_Opacity(Camera->OutputBuffer, Sprite->Graphic.pImage, &rctDest, rctSource.Left, rctSource.Top, SavedColor, abs(Sprite->Params.Alpha) * 255);
              Sprite->Graphic.pImage->MatteColor.V = iTemp;
              break;
            case 1:
              BlitSimple_SourceAlpha_Tint_Opacity(Camera->OutputBuffer, Sprite->Graphic.pImage, &rctDest, rctSource.Left, rctSource.Top, SavedColor, abs(Sprite->Params.Alpha) * 255);
              break;
            case 7:
              if (Camera->SaturationMode == 1) {
                BlitSimple_Screen_Opacity(Camera->OutputBuffer, Sprite->Graphic.pImage, &rctDest, rctSource.Left, rctSource.Top, abs(Sprite->Params.Alpha) * 255);
              } else {
                BlitSimple_Additive_Opacity(Camera->OutputBuffer, Sprite->Graphic.pImage, &rctDest, rctSource.Left, rctSource.Top, abs(Sprite->Params.Alpha) * 255);
              }
            case 2: case 3: case 4: case 5: case 6: case 8:
                break;
            }
          }
        }
        ProfileStop("Render Sprites");
      } else if (SortEntry->type == Lighting::plane) {
        ProfileStart("Render Planes");
        Plane = SortEntry->Plane;
        rctTop = Plane->topRect();
        rctWall = Plane->bottomRect();
        rctTop.translate(-OffsetX, -OffsetY);
        rctWall.translate(-OffsetX, -OffsetY);
    		x = rctTop.Left;
		    ClipRectangle_Image(&rctTop, Camera->OutputBuffer);
		    ClipRectangle_Image(&rctWall, Camera->OutputBuffer);
		    xo = (rctWall.Left - x);
		    if (Plane->Height != 0) FilterSimple_Fill(Camera->OutputBuffer, &rctTop, Environment->AmbientLight);
          if ((rctWall.Width > 0) && (rctWall.Height > 0)) {
            /*
            if (rctWall.Width > PlaneTexture->Width) {
              PlaneTexture->Data = StaticAllocate<Pixel>(TextureBuffer, rctWall.Width);
              PlaneTexture->Width = rctWall.Width;
            }
            rctSource.Width = rctWall.Width;
            rctSource.Height = 1;
            rctSource.Left = 0;
            rctSource.Top = 0;
            */
            rctSource.Width = 1;
            rctSource.Height = rctWall.Height;
            rctSource.Top = rctWall.Top;
            rctSource.Left =  rctWall.Left;
            float y = _Max<float>(Plane->Start.Y, Plane->End.Y);
            float ex = Plane->End.X;
            float sx = Plane->Start.X;
		        Pixel color = Pixel(0, 0, 0, 255);
		        int i = 0;
            /*
		        PlaneTexture->clear();
            for (float x = sx; x <= ex; x += 1.0) {
      			  if (i >= rctWall.Width) break;
			        color = Raycast(Environment, x + xo, y, Null, true);
              PlaneTexture->setPixelFast(i++, 0, color);
            }
            BlitResample_Normal(Camera->OutputBuffer, PlaneTexture, &rctWall, &rctSource, DefaultSampleFunction);
            */
            for (float x = sx; x <= ex; x += 1.0) {
      			  if (i >= rctWall.Width) break;
              rctSource.Left = rctWall.Left + i;
              if ((rctSource.Left >= Camera->OutputBuffer->ClipRectangle.Left) && (rctSource.Left < Camera->OutputBuffer->ClipRectangle.right())) {
  			        color = Raycast(Environment, x + xo, y, Null, true);
                FilterSimple_Fill(Camera->OutputBuffer, &rctSource, color);
              }
              i++;
            }
          }
          ProfileStop("Render Planes");
        }
        SortEntry = SortEntry->pSortedNext;
        n++;
      }
      PlanePoly.Deallocate();
//	    PlaneTexture->Data = Null;
//      delete PlaneTexture;
  }
  ProfileStop("Render Planes & Sprites");

  ShadowPoly.Deallocate();
  GradientShadowPoly.Deallocate();

  return Success;
}

Export int RenderLines_Masked(Image *Image, FLine *Lines, Byte *Mask, Pixel Color, int Count, float XOffset, float YOffset) {
  if (!Image) return Failure;
  if (!Lines) return Failure;
  if (!Mask) return Failure;
  if (Count < 1) return Failure;
  FLine Line;
  Pixel CurrentColor;
  for (int l = 0; l < Count; l++) {
    if (Mask[l]) {
      CurrentColor = Color;
      CurrentColor[::Alpha] = AlphaLookup(Mask[l], CurrentColor[::Alpha]);
      Line = Lines[l];
      Line.Start.X -= XOffset;
      Line.Start.Y -= YOffset;
      Line.End.X -= XOffset;
      Line.End.Y -= YOffset;
      if (ClipFloatLine(Image, &Line)) {
        int x_distance = Line.End.X - Line.Start.X, y_distance = Line.End.Y - Line.Start.Y;
        int pixel_count = (_Max(abs(x_distance), abs(y_distance)));
        float x_increment = (x_distance / (float)pixel_count), y_increment = (y_distance / (float)pixel_count);
        float current_x = Line.Start.X, current_y = Line.Start.Y;
        for (int i = 0; i <= pixel_count; i++) {
          Image->setPixelAA(current_x, current_y, CurrentColor);
          current_x += x_increment;
          current_y += y_increment;
        }
      }
    }
  }
  return Success;
}

Export int SelectLines(Rectangle *Area, FLine *Lines, Byte *Mask, int Count, float XOffset, float YOffset) {
  if (!Lines) return Failure;
  if (!Mask) return Failure;
  if (Count < 1) return Failure;
  Rectangle RealArea;
  FLine Line;
  RealArea = *Area;
  RealArea.normalize();
  for (int l = 0; l < Count; l++) {
    Line = Lines[l];
    Line.Start.X -= XOffset;
    Line.Start.Y -= YOffset;
    Line.End.X -= XOffset;
    Line.End.Y -= YOffset;
    if (ClipFloatLine(&RealArea, &Line)) {
      Mask[l] = 255;
    } else {
      Mask[l] = 0;
    }
  }
  return Success;
}

Export int SelectPlanes(Rectangle *Area, Lighting::Plane *Planes, Byte *Mask, int Count, float XOffset, float YOffset) {
  if (!Planes) return Failure;
  if (!Mask) return Failure;
  if (Count < 1) return Failure;
  Rectangle RealArea;
  Rectangle PlaneArea;
  Lighting::Plane Plane;
  RealArea = *Area;
  RealArea.normalize();
  for (int p = 0; p < Count; p++) {
    Plane = Planes[p];
    Plane.Start.X -= XOffset;
    Plane.Start.Y -= YOffset;
    Plane.End.X -= XOffset;
    Plane.End.Y -= YOffset;
    PlaneArea.Left = _Min(Plane.Start.X, Plane.End.X);
    PlaneArea.Top = _Min(Plane.Start.Y, Plane.End.Y);
    PlaneArea.setRight(_Max(Plane.Start.X, Plane.End.X));
    PlaneArea.setBottom(_Max(Plane.Start.Y, Plane.End.Y));
    if (PlaneArea.intersect(RealArea)) {
      Mask[p] = 255;
    } else {
      Mask[p] = 0;
    }
  }
  return Success;
}

Export int RenderCollisionLines(Image *Image, FLine *Lines, int Count, float XOffset, float YOffset) {
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
    if (ClipFloatLine(Image, &Line)) {
      int x_distance = Line.End.X - Line.Start.X, y_distance = Line.End.Y - Line.Start.Y;
      int pixel_count = (_Max(abs(x_distance), abs(y_distance)));
      float x_increment = (x_distance / (float)pixel_count), y_increment = (y_distance / (float)pixel_count);
      float current_x = Line.Start.X, current_y = Line.Start.Y;
      for (int i = 0; i <= pixel_count; i++) {
        Image->setPixelAA(current_x, current_y, Pixel((Image->getPixelAA(current_x, current_y).V ^ 0xFFFFFF) | 0xFF000000));
        current_x += x_increment;
        current_y += y_increment;
      }
    }
  }
  return Success;
}

Export CollisionMatrix* CreateCollisionMatrix(int Width, int Height) {
  return new CollisionMatrix(Width, Height);
}

Export int AppendLinesToCollisionMatrix(CollisionMatrix *Matrix, FLine *Lines, int Count) {
  if (Matrix == Null) return Failure;
  if (Count < 1) return Trivial_Success;
  if (Lines == Null) return Failure;
  if (Matrix->addLines(Lines, Count)) {
    return Success;
  }
  return Failure;
}

Export int EraseCollisionMatrix(CollisionMatrix *Matrix) {
  if (Matrix == Null) return Failure;
  Matrix->erase();
  return Success;
}

Export int DeleteCollisionMatrix(CollisionMatrix *Matrix) {
  if (Matrix == Null) return Failure;
  delete Matrix;
  return Success;
}

Export Lighting::Matrix* CreateLightingMatrix(int Width, int Height) {
  return new Lighting::Matrix(Width, Height);
}

Export int AppendObstructionsToLightingMatrix(Lighting::Matrix *Matrix, Lighting::Obstruction *Obstructions, int Count) {
  if (Matrix == Null) return Failure;
  if (Count < 1) return Trivial_Success;
  if (Obstructions == Null) return Failure;
  if (Matrix->addObstructions(Obstructions, Count)) {
    return Success;
  }
  return Failure;
}

Export int AppendPlanesToLightingMatrix(Lighting::Matrix *Matrix, Lighting::Plane *Planes, int Count) {
  if (Matrix == Null) return Failure;
  if (Count < 1) return Trivial_Success;
  if (Planes == Null) return Failure;
  if (Matrix->addPlanes(Planes, Count)) {
    return Success;
  }
  return Failure;
}

Export int EraseLightingMatrix(Lighting::Matrix *Matrix) {
  if (Matrix == Null) return Failure;
  Matrix->erase();
  return Success;
}

Export int DeleteLightingMatrix(Lighting::Matrix *Matrix) {
  if (Matrix == Null) return Failure;
  delete Matrix;
  return Success;
}

typedef void SegmentCallback(Image* Buffer, int XPass, int YPass);

Export int RenderMap(Map *Map, MapCamera *Camera, SegmentCallback *FlipCallback, SegmentCallback *BackgroundCallback) {
  if (!Map) return Failure;
  if (!Camera) return Failure;
  if (!Camera->ScratchBuffer) return Failure;
  if (Map->LayerCount < 1) return Failure;
  if ((Map->Width < 1) || (Map->Height < 1)) return Failure;
  if (!(Map->Layers)) return Failure;
  if (!(Map->Layers[0].Tileset)) return Failure;

  MapLayer *Layer;
  DoubleWord TileWidth = Map->Layers[0].Tileset->TileWidth, TileHeight = Map->Layers[0].Tileset->TileHeight;
  DoubleWord RenderXStep = Camera->ScratchBuffer->Width, RenderYStep = Camera->ScratchBuffer->Height;
  DoubleWord XPasses = ceil(Camera->Area.Width / (float)RenderXStep / Camera->ScaleRatioX), YPasses = ceil(Camera->Area.Height / (float)RenderYStep / Camera->ScaleRatioY);

  bool Scale = (Camera->Scaler != Null) && ((Camera->ScaleRatioX != 1) || (Camera->ScaleRatioY != 1));
  int CameraX = Camera->ViewportX, CameraY = Camera->ViewportY;
  int CameraXOffset = 0, CameraYOffset = 0;
  DoubleWord CameraTileX = 0, CameraTileY = 0;
  int RenderX = 0, RenderY = 0;
  DoubleWord RenderWidth = ceil(Camera->ScratchBuffer->Width / (float)TileWidth), RenderHeight = ceil(Camera->ScratchBuffer->Height / (float)TileHeight);
  DoubleWord BlockTileX = 0, BlockTileY = 0;
  DoubleWord TileX = 0, TileY = 0;

  Rectangle rDest;
  Rectangle rOutput, rScratch;

  short *BlockPointer = Null, *RowPointer = Null, *TilePointer = Null;

  TileBlitter** TileBlitters = Null;
  TintedTileBlitter** TintBlitters = Null;

  TileBlitters = LookupAllocate< TileBlitter* >(Map->LayerCount);
  TintBlitters = LookupAllocate< TintedTileBlitter* >(Map->LayerCount);

  TileBlitter* pTileBlitter = Null;
  TintedTileBlitter* pTintBlitter = Null;

  for (DoubleWord Layers = 0; Layers < Map->LayerCount; ++Layers) {
    pTileBlitter = Null;
    pTintBlitter = Null;
    switch(Map->Layers[Layers].BlitMode) {
    default:
    case 0:
        pTileBlitter = BlitSimple_Normal_Opacity;
        pTintBlitter = BlitSimple_Normal_Tint_Opacity;
        break;
    case 1:
        pTileBlitter = BlitSimple_Automatic_Matte_Opacity;
        pTintBlitter = BlitSimple_Matte_Tint_Opacity;
        break;
    case 2:
        pTileBlitter = BlitSimple_Automatic_SourceAlpha_Opacity;
        pTintBlitter = BlitSimple_SourceAlpha_Tint_Opacity;
        break;
    case 3:
        pTileBlitter = BlitSimple_Additive_Opacity;
        break;
    case 4:
        pTileBlitter = BlitSimple_Subtractive_Opacity;
        break;
    case 6:
        pTileBlitter = BlitSimple_Screen_Opacity;
        break;
    case 7:
        pTileBlitter = BlitSimple_Multiply_Opacity;
        break;
    case 8:
        pTileBlitter = BlitSimple_Lightmap_Opacity;
        break;
    }
    TileBlitters[Layers] = pTileBlitter;
    TintBlitters[Layers] = pTintBlitter;
  }

  rScratch = Camera->ScratchBuffer->getRectangle();
  rOutput = rScratch;

  enableClipping = true;

  BlockPointer = Null;

  DoubleWord MaxTileX = Map->Width - 1;
  DoubleWord MaxTileY = Map->Height - 1;

  CameraXOffset = CameraX % TileWidth;
  CameraYOffset = CameraY % TileHeight;
  CameraTileX = CameraX / TileWidth;
  CameraTileY = CameraY / TileWidth;

  Image* iTile = Null;
  short Tile = 0;

  for (DoubleWord YPass = 0; YPass < YPasses; ++YPass) {
    BlockTileY = (YPass * RenderHeight) + CameraTileY;
    RenderY = ((BlockTileY - CameraTileY) * TileHeight) - CameraYOffset + Camera->Area.Top;
    for (DoubleWord XPass = 0; XPass < XPasses; ++XPass) {
      BlockTileX = (XPass * RenderWidth) + CameraTileX;
      RenderX = ((BlockTileX - CameraTileX) * TileWidth) - CameraXOffset + Camera->Area.Left;
      if (BackgroundCallback) {
        BackgroundCallback(Camera->ScratchBuffer, RenderX, RenderY);
      } else {
        FilterSimple_Fill(Camera->ScratchBuffer, Null, Camera->BackgroundColor);
        if (Camera->OutputBuffer) {
          if (Camera->BackgroundOpacity) {
            int sx = RenderX, sy = RenderY;
            if (RenderX < 0) {
              rDest.Left = -RenderX;
              rDest.Width = Camera->ScratchBuffer->Width + RenderX;
              sx = 0;
            } else {
              rDest.Left = 0;
              rDest.Width = Camera->ScratchBuffer->Width;
            }
            if (RenderY < 0) {
              rDest.Top = -RenderY;
              rDest.Height = Camera->ScratchBuffer->Height + RenderY;
              sy = 0;
            } else {
              rDest.Top = 0;
              rDest.Height = Camera->ScratchBuffer->Height;
            }
            if (Scale) {
              rOutput.Left = rDest.Left;
              rOutput.Top = rDest.Top;
              rOutput.Width = rDest.Width;
              rOutput.Height = rDest.Height;
              rScratch.Left = ceil(RenderX * Camera->ScaleRatioX);
              rScratch.Top = ceil(RenderY * Camera->ScaleRatioY);
              rScratch.Width = ceil(rOutput.Width * Camera->ScaleRatioX);
              rScratch.Height = ceil(rOutput.Height * Camera->ScaleRatioY);
              BlitResample_Normal_Opacity(Camera->ScratchBuffer, Camera->OutputBuffer, &rOutput, &rScratch, Camera->Scaler, Camera->BackgroundOpacity);
            } else {
              BlitSimple_Normal_Opacity(Camera->ScratchBuffer, Camera->OutputBuffer, &rDest, sx, sy, Camera->BackgroundOpacity);
            }
          }
        }
      }

      enableClipping = false;
      rDest.Left = 0;
      rDest.Top = 0;
      rDest.Width = TileWidth;
      rDest.Height = TileHeight;
      for (DoubleWord Layers = 0; Layers < Map->LayerCount; ++Layers) {
        Layer = &(Map->Layers[Layers]);
        iTile = Null;
        Tile = Layer->IgnoredTile;
        pTileBlitter = TileBlitters[Layers];
        pTintBlitter = TintBlitters[Layers];

        rDest.Top = 0;

        for (TileY = 0; TileY < RenderHeight; ++TileY) {
          if (Layer->WrapY) {
              RowPointer = Layer->Tiles + (Map->Width * ((TileY + BlockTileY) % MaxTileY));
          } else {
              RowPointer = Layer->Tiles + (Map->Width * (TileY + BlockTileY));
          }
          TilePointer = RowPointer + BlockTileX;

          rDest.Left = 0;

          for (TileX = 0; TileX < RenderWidth; ++TileX) {
            if (Layer->WrapX) {
                TilePointer = RowPointer + ((TileX + BlockTileX) % MaxTileX);
            }

            if (*TilePointer != Tile) {
              Tile = *TilePointer;
              iTile = Layer->Tileset->tile(Tile, Layer->AnimationTable);
            }

            if (Tile) {
              pTileBlitter(Camera->ScratchBuffer, iTile, &rDest, 0, 0, Layer->Opacity);
            }

            ++TilePointer; // Isn't necessary if WrapX is true, but I suspect an If here would just hurt performance anyway. Increments are fast.
            rDest.Left += TileWidth;
          }

          rDest.Top += TileHeight;

        }

      }
      enableClipping = true;

      if (FlipCallback) {
        FlipCallback(Camera->ScratchBuffer, RenderX, RenderY);
      } else {
        if (Camera->OutputBuffer) {
          rOutput.Left = ceil(RenderX * Camera->ScaleRatioX);
          rOutput.Top = ceil(RenderY * Camera->ScaleRatioY);
          rOutput.Width = ceil(Camera->ScratchBuffer->Width * Camera->ScaleRatioX);
          rOutput.Height = ceil(Camera->ScratchBuffer->Height * Camera->ScaleRatioY);
          rScratch.Left = 0;
          rScratch.Top = 0;
          rScratch.Width = Camera->ScratchBuffer->Width;
          rScratch.Height = Camera->ScratchBuffer->Height;
          if (Scale) {
            BlitResample_Normal_Opacity(Camera->OutputBuffer, Camera->ScratchBuffer, &rOutput, &rScratch, Camera->Scaler, Camera->MapOpacity);
          } else {
            BlitSimple_Normal_Opacity(Camera->OutputBuffer, Camera->ScratchBuffer, &rOutput, 0, 0, Camera->MapOpacity);
          }
        }
      }
    }
  }

  LookupDeallocate< TileBlitter* >(TileBlitters);
  LookupDeallocate< TintedTileBlitter* >(TintBlitters);

  return Success;
}

//*/

void CollisionMatrix::resize(int W, int H) {
  this->deallocate();
  this->Width = W;
  this->Height = H;
  this->Sectors.resize(W * H);
  for (DoubleWord i = 0; i < this->Sectors.size(); i++) {
    this->Sectors[i] = new CollisionSector(this->SectorWidth, this->SectorHeight);
  }
}

bool CollisionMatrix::addLines(FLine *Lines, int Count) {
  if (Count < 1) return false;
  if (Lines == Null) return false;
  for (int y = 0; y < this->Height; y++) {
    for (int x = 0; x < this->Width; x++) {
      if (!this->getSector(x, y)->addLines(Lines, Count, x * this->SectorWidth, y * this->SectorHeight)) {
        return false;
      }
    }
  }
  return true;
}

bool CollisionMatrix::collisionCheck(FRect *Rectangle) {
  int xMin = floor(Rectangle->X1 / this->SectorWidth), yMin = floor(Rectangle->Y1 / this->SectorHeight), 
      xMax = floor(Rectangle->X2 / this->SectorWidth), yMax = floor(Rectangle->Y2 / this->SectorHeight);
  xMin = ClipValue(xMin, 0, this->Width - 1);
  yMin = ClipValue(yMin, 0, this->Height - 1);
  xMax = ClipValue(xMax, 0, this->Width - 1);
  yMax = ClipValue(yMax, 0, this->Height - 1);
  for (int y = yMin; y <= yMax; y++) {
    for (int x = xMin; x <= xMax; x++) {
      if (this->getSector(x, y)->collisionCheck(Rectangle, x * this->SectorWidth, y * this->SectorHeight)) {
        return true;
      }
    }
  }
  return false;
}

void CollisionMatrix::erase() {
  for (int y = 0; y < this->Height; y++) {
    for (int x = 0; x < this->Width; x++) {
	  this->getSector(x, y)->Lines.clear();
    }
  }
}

bool CollisionSector::collisionCheck(FRect *Rectangle, int XOffset, int YOffset) {
  if (CheckLineCollide(Rectangle, this->Lines)) {
    return true;
  }
  return false;
}

bool CollisionSector::addLines(FLine *Lines, int Count, int XOffset, int YOffset) {
  if (Count < 1) return false;
  if (Lines == Null) return false;
  FLine *Pointer = Lines, CurrentLine;
  Rectangle ThisRectangle;
  ThisRectangle.setValues(XOffset, YOffset, this->Width, this->Height);
  for (int i = 0; i < Count; i++) {
    CurrentLine = *Pointer;
    if (ClipFloatLine(&ThisRectangle, &CurrentLine)) {
      this->Lines.push_back(CurrentLine);
    }
    Pointer++;
  }
  return true;
}

void Lighting::Matrix::resize(int W, int H) {
  this->deallocate();
  this->Width = W;
  this->Height = H;
  this->Sectors.resize(W * H);
  for (DoubleWord i = 0; i < this->Sectors.size(); i++) {
    this->Sectors[i] = new Lighting::Sector(this->SectorWidth, this->SectorHeight);
  }
}

void Lighting::Matrix::erase() {
  for (int y = 0; y < this->Height; y++) {
    for (int x = 0; x < this->Width; x++) {
	  this->getSector(x, y)->Obstructions.clear();
	  this->getSector(x, y)->Planes.clear();
    }
  }
}

bool Lighting::Matrix::addObstructions(Lighting::Obstruction *Obstructions, int Count) {
  if (Count < 1) return false;
  if (Obstructions == Null) return false;
  for (int y = 0; y < this->Height; y++) {
    for (int x = 0; x < this->Width; x++) {
      if (!this->getSector(x, y)->addObstructions(Obstructions, Count, x * this->SectorWidth, y * this->SectorHeight)) {
        return false;
      }
    }
  }
  return true;
}

bool Lighting::Sector::addObstructions(Lighting::Obstruction *Obstructions, int Count, int XOffset, int YOffset) {
  if (Count < 1) return false;
  if (Obstructions == Null) return false;
  Lighting::Obstruction *Pointer = Obstructions, CurrentObstruction;
  Rectangle ThisRectangle;
  ThisRectangle.setValues(XOffset, YOffset, this->Width, this->Height);
  for (int i = 0; i < Count; i++) {
    CurrentObstruction = *Pointer;
    if (ClipFloatLine(&ThisRectangle, (FLine*)&CurrentObstruction)) {
      this->Obstructions.push_back(*Pointer);
    }
    Pointer++;
  }
  return true;
}

bool Lighting::Matrix::addPlanes(Lighting::Plane *Planes, int Count) {
  if (Count < 1) return false;
  if (Planes == Null) return false;
  for (int y = 0; y < this->Height; y++) {
    for (int x = 0; x < this->Width; x++) {
      if (!this->getSector(x, y)->addPlanes(Planes, Count, x * this->SectorWidth, y * this->SectorHeight)) {
        return false;
      }
    }
  }
  return true;
}

bool Lighting::Sector::addPlanes(Lighting::Plane *Planes, int Count, int XOffset, int YOffset) {
  if (Count < 1) return false;
  if (Planes == Null) return false;
  Lighting::Plane *Pointer = Planes, CurrentPlane;
  Rectangle ThisRectangle;
  ThisRectangle.setValues(XOffset, YOffset, this->Width, this->Height);
  for (int i = 0; i < Count; i++) {
    CurrentPlane = *Pointer;
/*
	if (ClipFloatLine(&ThisRectangle, (FLine*)&CurrentPlane)) {
      this->Planes.push_back(CurrentPlane);
    }
*/
    Pointer++;
  }
  return true;
}

Export int IterateSprites(SpriteParam *sprites, SpriteIterator *func) {
  if (!sprites) return Failure;
  SpriteParam *pCurrent = sprites;
  while (pCurrent) {
    func(pCurrent->Obj);
    pCurrent = pCurrent->pNext;
  }
  return Success;
}