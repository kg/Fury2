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
    double xd, yd;
    xd = abs(Other->X - this->X);
    yd = abs(Other->Y - this->Y);
    return (float)(sqrt(pow(xd, 2) + pow(yd, 2)));
  }
  inline void Translate(float x, float y) {
	  this->X += x;
	  this->Y += y;
  }
  bool inside(Rectangle *Area);
  bool inside(Rectangle *Area, int Edge);
  inline bool operator==(FPoint &rhs) {
    return ((rhs.X == this->X) && (rhs.Y == this->Y));
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
    FLine(ILine *Line);
	inline bool intersect(FLine& that, FPoint& point) {
		// Based on the 2d line intersection method from "comp.graphics.algorithms Frequently Asked Questions"
		float q = (this->Start.Y - that.Start.Y) * (that.End.X - that.Start.X) - (this->Start.X - that.Start.X) * (that.End.Y - that.Start.Y);
		float d = (this->End.X - this->Start.X) * (that.End.Y - that.Start.Y) - (this->End.Y - this->Start.Y) * (that.End.X - that.Start.X);
		if( d == 0 ) return false;
		float r = q / d;
		q = (this->Start.Y - that.Start.Y) * (this->End.X - this->Start.X) - (this->Start.X - that.Start.X) * (this->End.Y - this->Start.Y);
		float s = q / d;
		if( r < 0 || r > 1 || s < 0 || s > 1 ) return false;
		point.X = this->Start.X + (int) (0.5f + r * (this->End.X - this->Start.X));
		point.Y = this->Start.Y + (int) (0.5f + r * (this->End.Y - this->Start.Y));
		return true;
	}
    inline float slope() {
      return (End.Y - Start.Y) / (End.X - Start.X);
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
    return ((float)L / (float)65535.0) + H;
  }
  inline void setF(float value) {
    H = floor(value);
    L = floor((value - float(H)) * 65536.0);
  }
  inline FixedPoint& operator += (FixedPoint &rhs) {
    V += rhs.V;
    return *this;
  }
};

struct TextureCoordinate {
    FixedPoint U, V;
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

struct EdgeTexVertex : FPoint {
    Image *Tex;
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
