#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>
#include <math.h>
#include "Initialize.hpp"
#include <GL/glew.h>
#include <algorithm>
#include <vector>
#include <string>
#include <map>

using namespace std;

typedef unsigned __int8 Byte;
typedef unsigned __int16 Word;
typedef unsigned __int32 DoubleWord;
typedef unsigned __int64 QuadWord;
typedef Byte Channel;

#define Null 0

#define Align(n) __declspec(align(n))

static const bool EnableTextureSharing = true;

static const int SmallTextureSize = 64;
static const int CacheTextureSize = 512;

static const int Trivial_Success = 2;
static const int Success = 1;
static const int Failure = 0;

const double Pi = 3.14159265358979;
const double Radian = 1.74532925199433E-02;
#define Radians(a) (a * Radian)

inline int powerOfTwo(int value) {
  float v = log10((float)value) / log10(2.0f);
  if (floor(v) != ceil(v)) {
    return 1 << (int)ceil(v);
  }
  return value;
}

#define Export extern "C" __declspec(dllexport)

#ifndef _DEBUG
    #pragma warning( 1 : 4189 )         // local variable initialized but not referenced (moved from lvl 4)
    #pragma warning( disable : 4102 )   // 'foo' : unreferenced label (but who cares?)
#endif
#pragma warning( disable : 4530 )       // disabled exception stuff
#pragma warning( disable : 4996 )       // 'foo' was declared deprecated (cry me a river, C++ standards committee)
#pragma warning( disable : 4786 )       // identifier truncated in the browser information (why the hell should I care?)
#pragma warning( disable : 4190 )       // 'foo' has C-linkage specified, but returns struct 'bar' which is incompatible with C (but who really cares?)
#pragma warning( disable : 4244 )       // (big val) converted to (small val). Possible loss of data  
#pragma warning( disable : 4305 )       // truncation from 'const double ' to 'float '  
#pragma warning( disable : 4514 )       // Unreferenced inline function has been removed (yeah, we know)  
#pragma warning( 4 : 4761 )             // (small value) assigned to (big val), Conversion Supplied  
#pragma warning( 4 : 4142 )             // benign redefinition of type (ex: unsigned char to char)   

#include "Classes.hpp"

struct IPoint {
  int X, Y;
};

struct FPoint {
  FPoint() {
    X = 0;
    Y = 0;
  }
  FPoint(float x, float y) {
    X = x;
    Y = y;
  }
  float X, Y;
};

struct TexturedVertex : FPoint {
  float U, V;
};

struct GradientVertex : FPoint {
  DoubleWord Color;
};

extern GLFXGlobal* Global;

#include "Override.hpp"
#include "Import_SoftFX.hpp"

#include "BlendModes.hpp"
#include "ScaleModes.hpp"
#include "Pixel.hpp"
#include "Rectangle.hpp"
#include "Texture.hpp"
#include "GLFXGlobal.hpp"
#include "GL.hpp"

static const Pixel White = Pixel(0xFFFFFFFF);
static const Pixel Black = Pixel(0x0);

template <class T> T inline _Max(T one, T two) {
    if (one > two) {
        return one;
    } else {
        return two;
    }
}

template <class T> T inline _Min(T one, T two) {
    if (one < two) {
        return one;
    } else {
        return two;
    }
}

inline int ClipValue(int value, int min, int max) {
int iClipped;
    iClipped = -(int)(value < min);
    value = (min & iClipped) | (value & ~iClipped);
    iClipped = -(int)(value > max);
    return (max & iClipped) | (value & ~iClipped);
}

inline Pixel MultiplyAlpha(Pixel Color, int Alpha) {
  Color[::Alpha] = Color[::Alpha] * Alpha / 255;
  return Color;
}

struct MeshPoint {
  float X, Y;
};

struct MeshParam {
  int Width;
  int Height;
  MeshPoint *pData;

  inline MeshPoint* getPoint(int X, int Y) {
    X = ClipValue(X, 0, Width - 1);
    Y = ClipValue(Y, 0, Height - 1);
    return &(pData[X + (Y * Width)]);
  }

  inline void get4Points(int X, int Y, MeshPoint** Points) {
    Points[0] = &(pData[ClipValue(X, 0, Width - 1) + (ClipValue(Y, 0, Height - 1) * Width)]);
    Points[1] = &(pData[ClipValue(X+1, 0, Width - 1) + (ClipValue(Y, 0, Height - 1) * Width)]);
    Points[2] = &(pData[ClipValue(X, 0, Width - 1) + (ClipValue(Y+1, 0, Height - 1) * Width)]);
    Points[3] = &(pData[ClipValue(X+1, 0, Width - 1) + (ClipValue(Y+1, 0, Height - 1) * Width)]);
    return;
  }
};