/*
Compromise (COM virtual registration library)
Copyright (C) 2006 Kevin Gadd

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

#ifndef _GLOBAL_HPP
#define _GLOBAL_HPP

/*
    Configuration
*/

#define _SECURE_SCL 0
#define _SECURE_SCL_THROWS 0

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
#pragma warning( disable : 4311 )
#pragma warning( disable : 4312 )
#pragma warning( disable : 4996 ) // $WORTHLESS$ was declared deprecated (shut up, VC++)

#include <string.h>
#include <string>
#include <sstream>
#include <vector>
#include <map>
#include <assert.h>
#include <algorithm>
#include <list>
#include <cmath>
#include <math.h>

#include <windows.h>
#include <detours.h>

extern bool Initialized;

#define Null 0
#define NullString L""

#define Align(n) __declspec(align(n))

#define Trivial_Success 2
#define Success 1
#define Failure 0

const double Pi = 3.14159265358979;
const double Radian = 1.74532925199433E-02;
#define Radians(a) (a * Radian)

#define Export extern "C" __declspec(dllexport)

#define AllocateArray(type, count) \
  new Align(16) type[count]

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

inline void DebugOut(const char* string) {
#ifdef _DEBUG
  OutputDebugStringA(string);
#endif
}

inline void DebugOut(const wchar_t* string) {
#ifdef _DEBUG
  OutputDebugStringW(string);
#endif
}

#ifdef _DEBUG
#define DebugOut_(args) \
  { \
    std::wstringstream buffer; \
    buffer << args; \
    DebugOut(buffer.str().c_str()); \
  }
#else
#define DebugOut_(args) \
  { \
  }
#endif

#include "types.hpp"
#include "util.hpp"
#include "registry.hpp"
#include "statics.hpp"

#endif