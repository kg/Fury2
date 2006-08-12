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

struct SpriteParam;
class CollisionMatrix;

namespace Lighting {
  
  class Camera;
  class Environment;
  class LightSource;
  class Obstruction;
  class _Plane;
  class Plane;
  class Matrix;
  class Sector;

  class Camera {
  public:
    int ScrollX, ScrollY;
    Rectangle OutputRectangle;
    Image *OutputBuffer;
    Image *ScratchBuffer;
    float OutputScaleRatio;
    Byte SaturationMode;
    Byte AntiAlias;
  };

  class Environment {
  public:
    LightSource *Lights;
    int LightCount;
    Obstruction *Obstructions;
    int ObstructionCount;
    Plane *Planes;
    int PlaneCount;
    Pixel AmbientLight;
    SpriteParam *Sprites;
    Byte ForceRaycasting;
	  Matrix *Matrix;
    CollisionMatrix *CollisionMatrix;
  };

  class LightSourceRenderData {
  public:
    float ldist, ldist11;
    float xOffset, yOffset;
    float angleStart, angleEnd;
  };

  class LightSource {
  public:
    float X, Y;
    Pixel Color;
    int FalloffDistance;
    float Angle, Spread, SpinRate, FlickerLevel;
    Byte NoiseOpacity, Visible, Culled, PlaneCulled;
    SpriteParam *Attached;
    float AttachX, AttachY, AttachH, AttachV;
    Image *Cache;
    Byte CacheValid;
    Byte Fuzziness;
    Byte Reserved[2];
    Rectangle Rect;
    int FalloffDistance2;
    Image *Image;
    float ImageAlignX, ImageAlignY;
    float LightSize;
    LightSourceRenderData Data;
  };

  class Obstruction {
  public:
    FLine Line;
    inline FPoint center() {
      FPoint pt;
      pt.X = Line.Start.X + ((Line.End.X - Line.Start.X) / 2);
      pt.Y = Line.Start.Y + ((Line.End.Y - Line.Start.Y) / 2);
      return pt;
    }
    inline const FLine& getLine() const {
      return Line;
    }
  };

  class _Plane {
  public:
    FLine Base;
    FLine Top;
    inline FPoint center() {
      FPoint pt;
      pt.X = Base.Start.X + ((Base.End.X - Base.Start.X) / 2);
      pt.Y = Base.Start.Y + ((Base.End.Y - Base.Start.Y) / 2);
      return pt;
    }
  };

  class Plane {
  public:
    FPoint Start, End;
    int Height;
    inline FPoint center() {
      FPoint pt;
      pt.X = Start.X + ((End.X - Start.X) / 2);
      pt.Y = Start.Y + ((End.Y - Start.Y) / 2);
      return pt;
    }
    inline Rectangle topRect() {
    		Rectangle rect;
        rect.setValuesAbsolute(_Min(Start.X, End.X), _Min(Start.Y, End.Y) - Height, 
          _Max(Start.X, End.X) + 1, _Max(Start.Y, End.Y) - Height + 1);
        return rect;
    }
    inline Rectangle bottomRect() {
    		Rectangle rect;
        if (Height >= 0) {
          rect.setValuesAbsolute(_Min(Start.X, End.X), _Max(Start.Y, End.Y) - Height + 1, 
            _Max(Start.X, End.X) + 1, _Max(Start.Y, End.Y) + 1);
        } else {
          rect.setValuesAbsolute(_Min(Start.X, End.X), _Min(Start.Y, End.Y), 
            _Max(Start.X, End.X) + 1, _Min(Start.Y, End.Y) - Height + 1);
        }
        return rect;
    }
    inline Rectangle fullRect() {
    		Rectangle rect;
        if (Height >= 0) {
          rect.setValuesAbsolute(_Min(Start.X, End.X), _Min(Start.Y, End.Y) - Height, 
            _Max(Start.X, End.X) + 1, _Max(Start.Y, End.Y) + 1);
        } else {
          rect.setValuesAbsolute(_Min(Start.X, End.X), _Min(Start.Y, End.Y) - Height, 
            _Max(Start.X, End.X) + 1, _Min(Start.Y, End.Y) - Height + 1);
        }
        return rect;
    }
  };

  Pixel Raycast(Environment *Env, float X, float Y, SpriteParam *IgnoreSprite, bool EnableCulling);
  int RaycastStrip(Environment *Env, float X1, float Y1, float X2, float Y2, SpriteParam *IgnoreSprite, bool EnableCulling, int Count, Pixel* pOut);

  const enum sort_entry_types {
    none,
    plane,
    sprite
  };

  struct sort_entry {
    sort_entry_types type;
    int y;
    union {
      Plane *Plane;
      SpriteParam *Sprite;
    };
    sort_entry *pNext, *pSortedNext;
  };

  inline bool operator<= (const sort_entry& lhs, const sort_entry& rhs) {
    if (lhs.y <= rhs.y) {
      return true;
    } else if (lhs.y == rhs.y) {
      return (lhs.Plane <= rhs.Plane);
    } else {
      return false;
    }
  }

  static const int DefaultSectorSize = 64;

  class Sector {
	public:
	std::vector<Obstruction> Obstructions;
	std::vector<Plane> Planes;
	int Width, Height;

	Sector(int W, int H) {
		this->Width = W;
		this->Height = H;
	}

	~Sector() {
		this->clear();
		return;
	}

	bool addObstructions(Obstruction *Obstructions, int Count, int XOffset, int YOffset);
	bool addPlanes(Plane *Planes, int Count, int XOffset, int YOffset);

	inline void clear() {
		Obstructions.clear();
		Planes.clear();
	}
  };

  class Matrix {
	private:
	std::vector<Sector*> Sectors;
	public:
	int SectorWidth, SectorHeight;
	int Width, Height;

	Matrix(int W, int H, int SW = DefaultSectorSize, int SH = DefaultSectorSize) {
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

	bool addObstructions(Obstruction *Obstructions, int Count);
	bool addPlanes(Plane *Planes, int Count);

	inline Sector* getSector(int X, int Y) {
		if (X < 0) return Null;
		if (Y < 0) return Null;
		if (X >= Width) return Null;
		if (Y >= Height) return Null;
		return this->Sectors[(Y * this->Width) + X];
	};

	~Matrix() {
		if ((this == Null) || (this == (class Matrix *const)0xFFFFFFFF)) return;
		this->deallocate();
		this->Width = 0;
		this->Height = 0;
	};
  };

};

template<>
struct std::greater<Lighting::sort_entry*> {
    inline bool operator() ( const Lighting::sort_entry*& l, const Lighting::sort_entry*& r ) const {
        return ( (*l) <= (*r) );
    }
};
