#define WIN32_LEAN_AND_MEAN
#include <windows.h>
//#include <atlcomcli.h>
#include <D3DX8.h>
#include "Initialize.hpp"
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

#define Trivial_Success 2
#define Success 1
#define Failure 0

const double Pi = 3.14159265358979;
const double Radian = 1.74532925199433E-02;
#define Radians(a) (a * Radian)

#define Export extern "C" __declspec(dllexport)

template <class T> inline bool COMDestroy(T*& obj) {
  if (obj) {
    obj->Release();
    obj = Null;
    return true;
  }
  return false;
}

#ifndef _DEBUG
    #pragma warning( 1 : 4189 )         // local variable initialized but not referenced (moved from lvl 4)
    #pragma warning( disable : 4102 )   // 'foo' : unreferenced label (but who cares?)
#endif
#pragma warning( disable : 4996 )       // 'foo' was declared deprecated (cry me a river, C++ standards committee)
#pragma warning( disable : 4786 )       // identifier truncated in the browser information (why the hell should I care?)
#pragma warning( disable : 4190 )       // 'foo' has C-linkage specified, but returns struct 'bar' which is incompatible with C (but who really cares?)
#pragma warning( disable : 4244 )       // (big val) converted to (small val). Possible loss of data  
#pragma warning( disable : 4305 )       // truncation from 'const double ' to 'float '  
#pragma warning( disable : 4514 )       // Unreferenced inline function has been removed (yeah, we know)  
#pragma warning( 4 : 4761 )             // (small value) assigned to (big val), Conversion Supplied  
#pragma warning( 4 : 4142 )             // benign redefinition of type (ex: unsigned char to char)   

#include "Classes.hpp"

extern DX8FXGlobal* Global;

#include "Override.hpp"
#include "Import_SoftFX.hpp"

#include "BlendModes.hpp"
#include "Pixel.hpp"
#include "Rectangle.hpp"
#include "Vertex.hpp"
#include "Device.hpp"
#include "DX8FXGlobal.hpp"