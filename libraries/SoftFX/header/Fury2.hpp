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

#include "Fury2_Lighting.hpp"

static const int DefaultCollisionSectorSize = 128;

class Tileset;
class CollisionSector;
class CollisionMatrix;

extern Image* ShadowImage;

enum csRegions {
    csNone        = 0,
    csLeft        = 1,
    csTop         = 2,
    csRight       = 4,
    csBottom      = 8,
    csTopLeft     = 3,
    csTopRight    = 6,
    csBottomLeft  = 9,
    csBottomRight = 12,
    csAll         = 15
};

enum wsSections {
    wsTopLeft = 0,
    wsTop = 1,
    wsTopRight = 2,
    wsRight = 3,
    wsBottomRight = 4,
    wsBottom = 5,
    wsBottomLeft = 6,
    wsLeft = 7,
    wsMiddle = 8
};

enum wsSectionFlags {
    sfTopLeft = 1,
    sfTop = 2,
    sfTopRight = 4,
    sfRight = 8,
    sfBottomRight = 16,
    sfBottom = 32,
    sfBottomLeft = 64,
    sfLeft = 128,
    sfMiddle = 256,
    sfEdges = 127,
    sfAll = 511
};

enum spSpecialFX {
    fxHardShadow = 1,
    fxSoftShadow = 2,
    fxCastShadow = 4,
    fxCastGraphicShadow = 8
};

struct SpriteObstruction {
    float W;
    float H;
    Byte Type;
};

struct SpriteGraphic {
    Image *pImage;
    Rectangle Rectangle;
    float XCenter, YCenter;
    Pixel MaskColor;
};

struct SpritePosition {
    float X, Y, Z;
};

struct SpriteVelocity {
    float X, Y, Z; // x/y/z independent velocities
    float V, B; // forward velocity and bearing
    float BR, BRT; // bearing rotational velocity and rotational target
    float A, AT; // alpha velocity and target
    float VM; // velocity multiplier
    float XF, YF; // x and y forces
    float FW; // force weight
    float CXF, CYF; // constant x and y forces
    float CFM; // constant force multiplier
};

struct VisualParameters {
    short BlitMode;
    short SpecialFX;
    Pixel Color;
    float Alpha;
    float Scale;
    float Angle;
    Pixel IlluminationLevel;
    Byte DiffuseLight;
    Byte RenderTarget;
    Byte Reserved1;
    Byte Reserved2;
};

struct PhysicalParameters {
    Byte Solid;
    Byte Pushable;
    Byte CanPush;
    Byte Cull;
    float Weight;
};

struct AnimatedGraphicParam {
    int FrameCount;
    Image **pFrames;
    int Frame;
    int BlitMode;
    float Alpha;
    float XCenter, YCenter;
    Pixel MatteColor;
};

struct EventParameters {
    short CollidedWith;
    Byte CollidedWithMap;
    Byte FadedOut;
    Byte Moved;
    Byte Changed;
};

struct VelocityVector {
    float X, Y;
};

struct SpriteParam {
    SpritePosition Position;
    SpriteVelocity Velocity;
    SpriteObstruction Obstruction;
    SpriteGraphic Graphic;
    VisualParameters Params;
    PhysicalParameters Stats;
    EventParameters Events;
    Byte Type;
    Byte ProcessType;
    Byte Culled;
    Byte Visible;
    short Index;
    AnimatedGraphicParam *pAttachedGraphic;
    float ZHeight;
    unsigned short Reserved1;
    unsigned short Reserved2;
    SpriteParam *pNext;
    SpriteParam *pSortedNext;

    FRect getRect();

    inline Rectangle getRectangle() {
    		Rectangle rect;
        float w = Graphic.Rectangle.Width / 2;
        rect.setValuesAbsolute(floor(Position.X - w), floor(Position.Y - Graphic.Rectangle.Height), 
          floor(Position.X + w), floor(Position.Y));
        return rect;
    }

    bool touches(SpriteParam *other);
    bool touches(FRect *other);
    bool touches(SpriteParam *other, VelocityVector *other_speed);
    inline int touches(FLine *lines, int line_count);
    inline int touches(CollisionMatrix *Matrix);
};

inline bool operator<= (const SpriteParam& lhs, const SpriteParam& rhs) {
    if (lhs.Position.Y - lhs.Position.Z <= rhs.Position.Y - rhs.Position.Z) {
        return true;
    } else if (lhs.Position.Y - lhs.Position.Z == rhs.Position.Y - rhs.Position.Z) {
        if (lhs.Position.X < rhs.Position.X) {
            return true;
        } else if (lhs.Position.X == rhs.Position.X) {
            return (lhs.Index < rhs.Index);
        } else {
            return false;
        }
    } else {
        return false;
    }
}

template<>
struct std::greater<SpriteParam*> {
    inline bool operator() ( const SpriteParam*& l, const SpriteParam*& r ) const {
        return ( (*l) <= (*r) );
    }
};

struct CameraParam {
    Image **pRenderTargets;
    int RenderTargetCount;
    Rectangle Rectangle;
    float Alpha;
    int ViewportX;
    int ViewportY;

    inline Image* pImage() {
      if (RenderTargetCount >= 1) return pRenderTargets[0];
      return Null;
    }

    inline Image* pLightmap() {
      if (RenderTargetCount >= 2) return pRenderTargets[1];
      return Null;
    }

    inline Image* pLightmapScratch() {
      if (RenderTargetCount >= 3) return pRenderTargets[2];
      return Null;
    }
};

struct CharacterParam {
    Image *pImage;
    int MapValue;
    short XIncrement;
    short YIncrement;
    short XOffset;
    short YOffset;
};

struct TextParam {
    int Width;
    int Height;
    int Lines;
    int Scroll_X;
    int Scroll_Y;
    int Selection_Start;
    int Selection_End;
    Pixel Selection_Color;
    int Caret_Position;
    Pixel Caret_Color;
    int CharFromPoint_X;
    int CharFromPoint_Y;
    int CharFromPoint;
	  int MaxChars;
	  int CharsDrawn;
	  int TabStopCount;
	  int* TabStops;
};

struct KerningPair {
    wchar_t Characters[2];
    int XOffset;
};

struct FontParam {
    int EffectMode;
    Pixel MaskColor;
    Pixel FillColor;
    Pixel ShadowColor;
    int Alpha;
    int WrapMode;
    int BaseHeight;
    int BaseMode;
    int MapCount;
    CharacterParam **MapPointer;
//    int KerningPairCount;
//    KerningPair *KerningPairs;
};

enum CharacterTraits2 {
    
};

struct CharacterParam2 {
    Image *pImage;
    wchar_t MapValue;
    float XIncrement, YIncrement;
    float XOffset, YOffset;
    Byte Loaded;
};

struct TextParam2 {
    int BlitMode, AlignmentMode, WrappingMode, KerningMode, CoordinateMode;
    int DrawCharacters;
    int SelectionStart, SelectionEnd;
    int CaretPosition;
	  int TabStopCount;
	  float* TabStops;
    float ScrollX, ScrollY;
    float FindCharacterX, FindCharacterY;
    Pixel TextColor;
    Pixel ShadowColor;
    Pixel SelectionColor;
    Pixel CaretColor;
    float outWidth, outHeight;
    int outLines;
    int outFoundCharacter;
    int outDrawnCharacters;
};

struct KerningPair2 {
    wchar_t Characters[2];
    float XOffset;
};

struct FontParam2 {
    float Height;
    float LineHeight;
    int CharacterMapSize;
    CharacterParam2 **CharacterMap;
    int KerningPairCount;
    KerningPair2 *KerningPairs;
    void* LoadCharacterCallback;
};
    
struct WindowSkinParam {
    Image **pImages;
    int Alpha;
    Pixel MaskColor;
    Pixel CornerColors[4];
    int EdgeOffsets[4];
    Pixel TintColors[9];
    Byte BackgroundMode;
    Byte EdgeMode;
    Byte RenderMode;
};

class Tileset {
private:
    std::vector<Image*> *Tiles;
public:
    bool Initialized;
    int TilesPerRow, TilesPerCol, TileCount;
    int TileWidth, TileHeight;

    Tileset(Image *pImage, int TileWidth, int TileHeight) {
        if (!pImage) return;
        heapInUse = true;
        this->TileWidth = TileWidth;
        this->TileHeight = TileHeight;
        this->TilesPerRow = pImage->Width / TileWidth;
        this->TilesPerCol = pImage->Height / TileHeight;
        this->TileCount = this->TilesPerRow * this->TilesPerCol;
        this->Tiles = new std::vector<Image*>;
        this->Tiles->resize(this->TileCount);
        int i = 0;
        Image *iTile = Null;
        for (int cy = 0; (cy + TileHeight) <= pImage->Height; cy += TileHeight) {
          for (int cx = 0; (cx + TileWidth) <= pImage->Width; cx += TileWidth) {
            iTile = Null;
            iTile = new Image(pImage, cx, cy, TileWidth, TileHeight);
            if (iTile) {
              iTile->MatteColor = pImage->MatteColor;
              iTile->optimize();
              iTile->Tags[3] = (DoubleWord)this;
            }
            this->Tiles->at(i) = iTile;
            i++;
          }
        }
        this->Initialized = true;
        heapInUse = false;
    }

    Tileset(int TileCount, int TileWidth, int TileHeight) {
        this->TileWidth = TileWidth;
        this->TileHeight = TileHeight;
        this->TileCount = TileCount;
        this->Tiles = new std::vector<Image*>;
        this->Tiles->resize(this->TileCount);
        this->Initialized = true;
    }

    Image* tile(int i);
    Image* tile(int i, short* mapTable);
    void setTile(int i, Image* newTile);
    Image* createTile();
    void addTile(Image* newTile);
    void removeTile(int i);

    ~Tileset() {
        if (!this->Initialized) return;
        heapInUse = true;
        if ((this == Null) || (this == (class Tileset *const)0xFFFFFFFF)) return;
        for (int i = 0; i < this->TileCount; i++) {
          if (this->Tiles->at(i)) {
            if (this->Tiles->at(i)->Tags[3] == (DoubleWord)this) {
              delete this->Tiles->at(i);
            }
            this->Tiles->at(i) = Null;
          }
        }
        this->Initialized = false;
        this->TilesPerRow = this->TilesPerCol = this->TileCount = this->TileWidth = this->TileHeight = 0;
        delete this->Tiles;
        heapInUse = false;
    }
};

class CollisionSector {
public:
  std::vector<FLine> Lines;
  int Width, Height;

  CollisionSector(int W, int H) {
    this->Width = W;
    this->Height = H;
  }

  ~CollisionSector() {
    this->clear();
    return;
  }

  bool addLines(FLine *Lines, int Count, int XOffset, int YOffset);

  inline void clear() {
    Lines.clear();
  }

  inline int getLineCount() {
    return Lines.size();
  }

  bool collisionCheck(FRect *Rectangle, int XOffset = 0, int YOffset = 0);
};

class CollisionMatrix {
private:
  std::vector<CollisionSector*> Sectors;
public:
  int SectorWidth, SectorHeight;
  int Width, Height;

  CollisionMatrix(int W, int H, int SW = DefaultCollisionSectorSize, int SH = DefaultCollisionSectorSize) {
    this->SectorWidth = SW;
    this->SectorHeight = SH;
    if (W < 1) return;
    if (H < 1) return;
    float xc = W / (float)SW;
    float yc = H / (float)SH;
    this->resize(ceil(xc), ceil(yc));
  };

  inline void deallocate() {
    if (this->Sectors.size() > 0) {
      for (DoubleWord i = 0; i < this->Sectors.size(); i++) {
        delete this->Sectors[i];
      }
      this->Sectors.clear();
    }
  }

  void erase();

  void resize(int W, int H);

  bool addLines(FLine *Lines, int Count);

  bool collisionCheck(FRect *Rectangle);

  inline CollisionSector* getSector(int X, int Y) {
    if (X < 0) return Null;
    if (Y < 0) return Null;
    if (X >= Width) return Null;
    if (Y >= Height) return Null;
    return this->Sectors[(Y * this->Width) + X];
  };

  ~CollisionMatrix() {
    if ((this == Null) || (this == (class CollisionMatrix *const)0xFFFFFFFF)) return;
    this->deallocate();
    this->Width = 0;
    this->Height = 0;
  };
};

struct TilemapLayerParam {
    short *pData;
    int Alpha;
    int X1;
    int Y1;
    int X2;
    int Y2;
    int Width;
    int Height;
    Tileset *pTileset;
    int MaskedTile;
    int Effect;
    Byte WrapX;
    Byte WrapY;
    Byte RenderTarget;
    Byte Reserved;
    short *pAnimationMap;
    Pixel TintColor;
};

struct MapLayer {
    short *Tiles;
    Tileset *Tileset;
    short *AnimationTable;
    SpriteParam *Sprites;
    Byte Opacity;
    int BlitMode;
    short IgnoredTile;
    Byte WrapX, WrapY;
    Byte Prerendered;
    Byte Visible;
    float ParallaxX, ParallaxY;
    int TintColor;
};

struct Map {
    int Width, Height;
    int LayerCount;
    MapLayer *Layers;
};

struct MapCamera {
    Rectangle Area;
    Image *OutputBuffer;
    Image *ScratchBuffer;
    int ViewportX, ViewportY;
    int BackgroundColor;
    int BackgroundOpacity, MapOpacity;
    float ScaleRatioX, ScaleRatioY;
    ScalerFunction* Scaler;
};

struct ForceEntry {
  ForceEntry() {
    Items = std::list<ForceEntry>();
  }

  SpriteParam* Sprite;
  std::list<ForceEntry> Items;
};