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

#ifndef SOFTFX
#define SOFTFX

/*
    Configuration
*/

#define UNICODE
#define ASSEMBLY
#define VISUALBASIC
#define OVERRIDES
#ifdef _DEBUG
  #define TRACE
#else
#define TRACE
#endif
//#define SAFE_PIXEL
//#define VIRTUALALLOC
//#define USEIFS
//#define ACCURATE_PYTHAGORAS
//#define LOOKUPCACHE
//#define LIBMNG

/*
    End Configuration
*/

#ifndef _DEBUG
    #pragma warning( 1 : 4189 )         // local variable initialized but not referenced (moved from lvl 4)
    #pragma warning( disable : 4102 )   // 'foo' : unreferenced label (but who cares?)
#endif
#pragma warning( disable : 4786 )       // identifier truncated in the browser information (why the hell should I care?)
#pragma warning( disable : 4190 )       // 'foo' has C-linkage specified, but returns struct 'bar' which is incompatible with C (but who really cares?)
#pragma warning( disable : 4244 )       // (big val) converted to (small val). Possible loss of data  
#pragma warning( disable : 4305 )       // truncation from 'const double ' to 'float '  
#pragma warning( disable : 4514 )       // Unreferenced inline function has been removed (yeah, we know)  
#pragma warning( 4 : 4761 )             // (small value) assigned to (big val), Conversion Supplied  
#pragma warning( 4 : 4142 )             // benign redefinition of type (ex: unsigned char to char)   

#include <string>
#include <vector>
#include <assert.h>
#define NOMINMAX
#include <algorithm>
#define min _cpp_min
#define max _cpp_max
#include <list>
#include <cmath>
#include <math.h>

namespace win32 {
  #include <windows.h>
};
#undef RGB
#undef RGBA

#ifdef TRACE
inline void _DebugTrace(const char *text) {
  win32::OutputDebugStringA((win32::LPCSTR)text);
  return;
}
inline void _DebugTrace(const wchar_t *text) {
  win32::OutputDebugStringW((win32::LPCWSTR)text);
  return;
}
#else
inline void _DebugTrace(const char *text) {
  return;
}
inline void _DebugTrace(const wchar_t *text) {
  return;
}
#endif

// fix issue with using .lib instead of .dll
#define COR_DECL
#include "../../../3rdparty/headers/corona.h"

extern bool Initialized;

#define Null 0

#define Align(n) __declspec(align(n))

#define Trivial_Success 2
#define Success 1
#define Failure 0

const double Pi = 3.14159265358979;
const double Radian = 1.74532925199433E-02;
#define Radians(a) (a * Radian)

#define Export extern "C" __declspec(dllexport)

#define AllocateArray(type, count) \
    new type[count]

#define Delete(v) \
    if (v) { \
        delete v; \
        v = Null; \
    }

#define DeleteArray(v) \
    if (v) { \
        delete [] v; \
        v = Null; \
    }

#define IfLocked(code) \
  ImageLockManager ilThis(lockingMode, this); \
  if (!ilThis.performUnlock()) { \
    code; \
  }

#define forever while (true) 

#include "Types.hpp"

#include "Inline.hpp"

#include "Initialize.hpp"

#include "../header/Constant_Overrides.hpp"
#include "Override.hpp"

#include "Alpha.hpp"

#include "Pythagoras.hpp"

#include "LookupTable.hpp"

#include "Heap.hpp"

#include "Pixel.hpp"
#include "Rectangle.hpp"
#include "Image.hpp"

#include "Profiler_Configuration.hpp"
#include "Profiler.hpp"

extern int (*_CreateDIBSection) (int hDC, BitmapInfo *pInfo, DoubleWord iFlags, void **pPointer, int FileHandle, int FileOffset);
extern int (*_DeleteObject) (int Object);
#endif