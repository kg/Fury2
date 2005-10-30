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

inline void _DebugTrace(const char *text) {
  OutputDebugStringA((LPCSTR)text);
  return;
}
inline void _DebugTrace(const wchar_t *text) {
  OutputDebugStringW((LPCWSTR)text);
  return;
}

#include "Classes.hpp"

struct Vec2 {
  float V[2];
};

struct Vec3 {
  Vec3() {
    V[0] = 0.0f;
    V[1] = 0.0f;
    V[2] = 0.0f;
  }

  Vec3(float value) {
    V[0] = value;
    V[1] = value;
    V[2] = value;
  }

  Vec3(float a, float b, float c) {
    V[0] = a;
    V[1] = b;
    V[2] = c;
  }
  inline float length() {
    float v = (V[0] * V[0]) + (V[1] * V[1]) + (V[2] * V[2]);
    return sqrt(v);
  }
  inline void normalize() {
    float l = this->length();
    if (l) {
      V[0] /= l;
      V[1] /= l;
      V[2] /= l;
    }
  }
  float V[3];
};

struct Vec4 {
  Vec4() {
    V[0] = 0.0f;
    V[1] = 0.0f;
    V[2] = 0.0f;
    V[3] = 0.0f;
  }

  Vec4(float value) {
    V[0] = value;
    V[1] = value;
    V[2] = value;
    V[3] = value;
  }

  Vec4(float value, float d) {
    V[0] = value;
    V[1] = value;
    V[2] = value;
    V[3] = d;
  }

  Vec4(float a, float b, float c, float d) {
    V[0] = a;
    V[1] = b;
    V[2] = c;
    V[3] = d;
  }

  float V[4];
};

struct Mat2 {
  float V[2][2];
};

struct Mat3 {
  float V[3][3];
};

struct Mat4 {
  float V[4][4];
};

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


template <class T> void inline _Swap(T& value1, T& value2) {
T temp;
    temp = value2;
    value2 = value1;
    value1 = temp;
    return;
}

Export inline Byte ClipByte(int value) {
#ifdef USEIFS
  if (value < 0) {
    return 0;
  } else if (value > 255) {
    return 255;
  } else {
    return value;
  }
#else
    value &= (-(int)!(value < 0));
    return ((255 & (-(int)(value > 255))) | (value)) & 0xFF;
#endif
}

Export inline Byte ClipByteLow(int value) {
#ifdef USEIFS
  if (value < 0) {
    return 0;
  } else {
    return value;
  }
#else
    return ((value) & (-(int)!(value < 0))) & 0xFF;
#endif
}

Export inline Byte ClipByteHigh(int value) {
#ifdef USEIFS
  if (value > 255) {
    return 255;
  } else {
    return value;
  }
#else
    return ((255 & (-(int)(value > 255))) | (value)) & 0xFF;
#endif
}

inline int ClipValue(int value, int maximum) {
#ifdef USEIFS
  if (value > maximum) {
    return maximum;
  } else {
    return value;
  }
#else
int iClipped;
    value = (value & (-(int)!(value < 0)));
    iClipped = -(int)(value > maximum);
    return (maximum & iClipped) | (value & ~iClipped);
#endif
}

Export inline int ClipValue(int value, int minimum, int maximum) {
#ifdef USEIFS
  if (value < minimum) {
    return minimum;
  } else if (value > maximum) {
    return maximum;
  } else {
    return value;
  }
#else
int iClipped;
    iClipped = -(int)(value < minimum);
    value = (minimum & iClipped) | (value & ~iClipped);
    iClipped = -(int)(value > maximum);
    return (maximum & iClipped) | (value & ~iClipped);
#endif
}

inline int InlineIf(bool condition, int ifTrue, int ifFalse) {
#ifdef USEIFS
    return condition ? ifTrue : ifFalse;
#else
int iMask;
    iMask = -(int)(condition);
    return (ifTrue & iMask) | (ifFalse & ~iMask);
#endif
}

inline int InlineIf(bool condition, int ifTrue) {
#ifdef USEIFS
    return condition ? ifTrue : 0;
#else
int iMask;
    iMask = -(int)(condition);
    return (ifTrue & iMask);
#endif
}

inline unsigned int InlineIf(bool condition, unsigned int ifTrue, unsigned int ifFalse) {
#ifdef USEIFS
    return condition ? ifTrue : ifFalse;
#else
int iMask;
    iMask = -(int)(condition);
    return (ifTrue & iMask) | (ifFalse & ~iMask);
#endif
}

inline unsigned int InlineIf(bool condition, unsigned int ifTrue) {
#ifdef USEIFS
    return condition ? ifTrue : 0;
#else
int iMask;
    iMask = -(int)(condition);
    return (ifTrue & iMask);
#endif
}

Export inline int WrapValue(int value, int minimum, int maximum) {
  bool b = value < minimum;
  int v = InlineIf(b, minimum - value, value - minimum);
  v = v % ((maximum - minimum) + 1);
  return InlineIf(b, maximum + 1 - v, minimum + v);
  /*
	if (value < minimum) {
		int v = ((minimum - value) % ((maximum - minimum) + 1));
		return maximum + 1 - v;
	} else {
		int v = ((value - minimum) % ((maximum - minimum) + 1));
		return minimum + v;
	}
  */
}

extern GLFXGlobal* Global;

typedef void (ShaderLoadCallback)(const char* filename, char** source);

#include "Override.hpp"
#include "Import_SoftFX.hpp"

#include "BlendModes.hpp"
#include "ScaleModes.hpp"
#include "Pixel.hpp"
#include "Rectangle.hpp"
#include "Texture.hpp"
#include "GLFXGlobal.hpp"
#include "GL.hpp"
#include "GLSL.hpp"
#include "Renderbuffer.hpp"
#include "Framebuffer.hpp"

static const Pixel White = Pixel(0xFFFFFFFF);
static const Pixel Black = Pixel(0x0);

inline Pixel MultiplyAlpha(Pixel Color, int Alpha) {
  Color[::Alpha] = Color[::Alpha] * Alpha / 255;
  return Color;
}

struct Filter {
  float *Weights;
  int Width, Height;
  int XOffset, YOffset;
  float Divisor;
  float Offset;
};

struct MeshPoint {
  float X, Y;
};

struct MeshParam {
  int Width;
  int Height;
  MeshPoint *pData;

  inline MeshPoint* getPoint(int X, int Y) {
    X = ClipValue(X, Width - 1);
    Y = ClipValue(Y, Height - 1);
    return &(pData[X + (Y * Width)]);
  }

  inline MeshPoint* getPointFast(int X, int Y) {
    return &(pData[X + (Y * Width)]);
  }

  inline void get4Points(int X, int Y, MeshPoint** Points) {
    int y = ClipValue(Y, Height - 1) * Width;
    int x1 = ClipValue(X, Width - 1), x2 = ClipValue(X+1, Width-1);
    Points[0] = &(pData[x1 + (y)]);
    Points[1] = &(pData[x2 + (y)]);
    y = ClipValue(Y+1, Height - 1) * Width;
    Points[2] = &(pData[x1 + (y)]);
    Points[3] = &(pData[x2 + (y)]);
    return;
  }
};
