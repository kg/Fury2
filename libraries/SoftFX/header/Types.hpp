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

typedef unsigned __int8 Byte;
typedef unsigned __int16 Word;
typedef unsigned __int32 DoubleWord;
typedef unsigned __int64 QuadWord;

#ifdef VISUALBASIC
    typedef int Size;
    typedef int Coordinate;
    typedef float FloatCoordinate;
    typedef void* Handle;
    typedef void* Pointer;
#else
    typedef unsigned int Size;
    typedef int Coordinate;
    typedef float FloatCoordinate;
    typedef void* Handle;
    typedef void* Pointer;
#endif

#ifdef UNICODE
    typedef unsigned short Character;
#else
    typedef unsigned char Character;
#endif

typedef Byte Channel;

class Pixel;
class Image;
class Rectangle;

struct FPoint;
struct FVector;
struct ILine;
struct FLine;

struct IPoint {
    int X;
    int Y;
};

struct IRect {
    int X1;
    int Y1;
    int X2;
    int Y2;
};

struct ISpan {
    int S;
    int E;
    inline bool intersection(ISpan& other) {
      if ((S <= other.E) && (E >= other.S)) {
        S = other.S < S ? other.S : S;
        E = other.E < E ? other.E : E;
        return true;
      }
      return false;
    }
};

struct AASpan {
    int S;
    int E;
    Byte A1, A2;
};

struct AAColSpan {
    int S;
    int E;
    Byte A1, A2;
};

struct ILine {
    inline ILine() {
        this->Start.X = 0;
        this->Start.Y = 0;
        this->End.X = 0;
        this->End.Y = 0;
    }
    ILine(FLine *Line);
    ILine(Rectangle *Rect);
    IPoint Start, End;
};

struct FPoint {
  inline FPoint() {
    X = 0;
    Y = 0;
  }
  inline FPoint(float X, float Y) {
    this->X = X;
    this->Y = Y;
  }
  float X;
  float Y;
  inline float distance(FPoint *Other) {
    float xd, yd;
    xd = Other->X - this->X;
    yd = Other->Y - this->Y;
    return (float)(sqrt((xd * xd) + (yd * yd)));
  }
  inline float length() {
    float v = (this->X * this->X) + (this->Y * this->Y);
    return sqrt(v);
  }
  inline float dot(FPoint B) {
    return (this->X * B.X + this->Y * B.Y);
  }
  inline FPoint perp() {
    FPoint temp;
    temp.X = -this->Y;
    temp.Y = this->X;
    return temp;
  }
  inline void normalize() {
    float l = this->length();
    if (l) {
      this->X /= l;
      this->Y /= l;
    }
  }
  inline FPoint rotate90r() {
    FPoint temp;
    temp.X = -this->Y;
    temp.Y = this->X;
    return temp;
  }
  inline FPoint rotate90l() {
    FPoint temp;
    temp.X = this->Y;
    temp.Y = -this->X;
    return temp;
  }
  bool inside(Rectangle *Area);
  bool inside(Rectangle *Area, int Edge);
  inline FPoint& operator-=(FPoint &rhs) {
    this->X -= rhs.X;
    this->Y -= rhs.Y;
    return *this;
  }
  inline FPoint& operator-=(float rhs) {
    this->X -= rhs;
    this->Y -= rhs;
    return *this;
  }
  inline FPoint& operator+=(FPoint &rhs) {
    this->X += rhs.X;
    this->Y += rhs.Y;
    return *this;
  }
  inline FPoint& operator+=(float rhs) {
    this->X += rhs;
    this->Y += rhs;
    return *this;
  }
  inline FPoint& operator*=(FPoint &rhs) {
    this->X *= rhs.X;
    this->Y *= rhs.Y;
    return *this;
  }
  inline FPoint& operator*=(float rhs) {
    this->X *= rhs;
    this->Y *= rhs;
    return *this;
  }
  inline FPoint& operator/=(FPoint &rhs) {
    if (rhs.X) {
      this->X /= rhs.X;
    } else {
      this->X = 0;
    }
    if (rhs.Y) {
      this->Y /= rhs.Y;
    } else {
      this->Y = 0;
    }
    return *this;
  }
  inline FPoint& operator/=(float rhs) {
    if (rhs) {
      this->X /= rhs;
      this->Y /= rhs;
    } else {
      this->X = 0;
      this->Y = 0;
    }
    return *this;
  }
  inline bool operator==(FPoint &rhs) {
    return ((rhs.X == this->X) && (rhs.Y == this->Y));
  }
  inline FPoint operator-() {
    return FPoint(-(this->X), -(this->Y));
  }
};

struct FVector {
  inline FVector() {
    X = 0;
    Y = 0;
  }
  FVector(FPoint &a, FPoint &b) {
    X = b.X - a.X;
    Y = b.Y - a.Y;
  }
  inline void Multiply(float Amount) {
    X *= Amount;
    Y *= Amount;
    return;
  }
  inline float length() {
    return sqrt((X * X) + (Y * Y));
  }
  inline void Rationalize() {
    if (X > Y) {
      Y /= X;
      X = 1.0;
    } else if (Y > X) {
      X /= Y;
      Y = 1.0;
    } else {
      X = Y = 1.0;
    }
  }
  float X;
  float Y;
};

struct FRect {
    inline bool intersect(FRect *that) {
      if (this->X1 > that->X2) return false;
      if (this->Y1 > that->Y2) return false;
      if (this->X2 < that->X1) return false;
      if (this->Y2 < that->Y1) return false;
      return true;
    }
    FLine edge(int index);
    float X1;
    float Y1;
    float X2;
    float Y2;
};

struct FSpan {
    float S;
    float E;
};

struct FLine {
    inline FLine() {
      this->Start.X = 0;
      this->Start.Y = 0;
      this->End.X = 0;
      this->End.Y = 0;
    }
    inline FLine(FPoint a, FPoint b) {
      this->Start = a;
      this->End = b;
    }
    FLine(ILine *Line);
	  inline bool intersect(FLine& that, FPoint& point) {
		  // Based on the 2d line intersection method from "comp.graphics.algorithms Frequently Asked Questions"
      if ((that.Start == this->Start) || (that.Start == this->End)) {
        point = that.Start;
        return true;
      }
      if ((that.End == this->Start) || (that.End == this->End)) {
        point = that.End;
        return true;
      }
      float thisx = this->End.X - this->Start.X;
      float thisy = this->End.Y - this->Start.Y;
		  float q = (this->Start.Y - that.Start.Y) * (that.End.X - that.Start.X) - (this->Start.X - that.Start.X) * (that.End.Y - that.Start.Y);
		  float d = (thisx) * (that.End.Y - that.Start.Y) - (thisy) * (that.End.X - that.Start.X);
		  if (d == 0.0f) return false;
      d = 1 / d;
		  float r = q * d;
      if (r < 0.0f || r > 1.0f) return false;
		  q = (this->Start.Y - that.Start.Y) * (thisx) - (this->Start.X - that.Start.X) * (thisy);
		  float s = q * d;
		  if (s < 0.0f || s > 1.0f ) return false;
		  point.X = this->Start.X + (r * thisx);
		  point.Y = this->Start.Y + (r * thisy);
		  return true;
	  }
    inline float slope() {
      return (End.Y - Start.Y) / (End.X - Start.X);
    }
    inline FPoint vector() {
      FPoint vec;
      vec.X = this->End.X - this->Start.X;
      vec.Y = this->End.Y - this->Start.Y;
      return vec;
    }
    inline FPoint rvector() {
      FPoint vec;
      vec.X = this->Start.X - this->End.X;
      vec.Y = this->Start.Y - this->End.Y;
      return vec;
    }
    inline FPoint bearing() {
      FPoint vec;
      vec.X = this->End.X - this->Start.X;
      vec.Y = this->End.Y - this->Start.Y;
      vec.normalize();
      return vec;
    }
    inline FPoint normal() {
      FPoint vec = vector();
      return vec.perp();
    }
    inline FRect bounds() {
      FRect area;
      area.X1 = Start.X > End.X ? End.X : Start.X;
      area.Y1 = Start.Y > End.Y ? End.Y : Start.Y;
      area.X2 = Start.X < End.X ? End.X : Start.X;
      area.Y2 = Start.Y < End.Y ? End.Y : Start.Y;
      return area;
    }
    inline void extend(float amount) {
      FPoint vec = this->vector();
      vec /= vec.length();
      vec *= amount;
      this->Start -= vec;
      this->End += vec;
      return;
    }
    FPoint Start, End;
};

struct FixedPoint {
  FixedPoint() {
    V = 0;
  }
  FixedPoint(int value) {
    H = value;
    L = 0;
  }
  FixedPoint(float value) {
    this->setF(value);
  }
  union {
    struct {
      short H;
      unsigned short L;
    };
    int V;
  };
  inline public short I() {
    return H;
  }
  inline void setI(short value) {
    H = value;
    L = 0;
  }
  inline public float F() {
    return ((float)L / 65535.0f) + (float)H;
  }
  inline void setF(float value) {
    H = (value);
    L = ((value - float(H)) * 65535.0f);
  }
  inline FixedPoint& operator += (FixedPoint &rhs) {
    V += rhs.V;
    return *this;
  }
};

struct TextureCoordinate {
    FixedPoint U, V;
};

struct TextureCoordinateF {
    float U, V;
};

struct TexturedVertex : FPoint {
    float U, V;
    TexturedVertex() {
      X = Y = U = V = 0;
    }
    TexturedVertex(float X, float Y) {
      this->X = X;
      this->Y = Y;
      U = V = 0;
    }
    TexturedVertex(float X, float Y, float U, float V) {
      this->X = X;
      this->Y = Y;
      this->U = U;
      this->V = V;
    }
};

struct GradientVertex : FPoint {
	GradientVertex() {
		this->X = this->Y = 0;
		this->Color = 0;
	}
	GradientVertex(float X, float Y, DoubleWord Color) {
		this->X = X;
		this->Y = Y;
		this->Color = Color;
	}
  DoubleWord Color;
  inline Pixel color();
  inline void setColor(Pixel V);
};

struct StrokePoint {
    float X, Y;
    float Thickness;
    DoubleWord Color;
};

struct Stroke {
    StrokePoint *Points;
    int PointCount;
    float Softness;
    Byte Loop;

    inline float MinimumX() {
      StrokePoint *CurrentPoint = Points;
      float Result = 99999999;
        for (int i = 0; i < PointCount; i++) {
          if ((CurrentPoint->X - CurrentPoint->Thickness) < Result) Result = CurrentPoint->X - CurrentPoint->Thickness;
          CurrentPoint++;
        }
        return Result;
    }
    inline float MinimumY() {
      StrokePoint *CurrentPoint = Points;
      float Result = 99999999;
        for (int i = 0; i < PointCount; i++) {
          if ((CurrentPoint->Y - CurrentPoint->Thickness) < Result) Result = CurrentPoint->Y - CurrentPoint->Thickness;
          CurrentPoint++;
        }
        return Result;
    }
    inline float MaximumX() {
      StrokePoint *CurrentPoint = Points;
      float Result = 0;
        for (int i = 0; i < PointCount; i++) {
          if (CurrentPoint->X + CurrentPoint->Thickness > Result) Result = CurrentPoint->X + CurrentPoint->Thickness;
          CurrentPoint++;
        }
        return Result;
    }
    inline float MaximumY() {
      StrokePoint *CurrentPoint = Points;
      float Result = 0;
        for (int i = 0; i < PointCount; i++) {
          if (CurrentPoint->Y + CurrentPoint->Thickness > Result) Result = CurrentPoint->Y + CurrentPoint->Thickness;
          CurrentPoint++;
        }
        return Result;
    }

};

struct StrokeSegment {
    StrokePoint* Start;
    StrokePoint* End;
    float XL, YL, L;
};

struct StrokeSector {
    int *Segments;
    int SegmentCount;
};

struct ColorFilter {
    DoubleWord Length;
    Byte Red[256];
    Byte Green[256];
    Byte Blue[256];
};

typedef void SpriteIterator(int Sprite);

typedef void ScalerFunction(Image *Source, int X, int Y, int XW, int YW, int XI, int YI, int XWI, int YWI, int Count, Pixel *Dest);

typedef void RenderFunction(Pixel *Dest, Pixel *Source, int Count, Pixel SolidColor, DoubleWord Argument);

typedef void LockedRenderFunction(Image *Dest, int X, int Y, Pixel *Source, int Count);