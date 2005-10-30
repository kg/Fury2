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
#include "../../../3rdparty/lzo/minilzo.h"

Export float Interpolate(float a, float b, float x) {
  if (x <= 0) return a;
  else if (x >= 1) return b;
  else return a + ((b - a) * x);
}

Export float CosineInterpolate(float a, float b, float x) {
  if (x <= 0) return a;
  else if (x >= 1) return b;
  else {
    x *= Pi;
    x = (1 - cos(x)) * 0.5;
    return a + ((b - a) * x);
  }
}

Export float CubicInterpolate(float a, float b, float c, float d, float x) {
  if (x <= 0) return b;
  else if (x >= 1) return c;
  else {
    float x2 = x * x;
    float x3 = x2 * x;
    float p = (d - c) - (a - b);
	  return (p * x3) + ((a - b - p) * x2) + ((c - a) * x) + b;
  }
}

Export int CompressData(void* Source, int SourceLength, void* Dest, int DestLength) {
    //lzo1x_1_compress(Source, SourceLength, Dest, DestLength, temp);
    return Success;
}

Export void* AllocateMemory(int Size) {
    return malloc(Size);
}

Export int DeallocateMemory(void* Pointer) {
    free(Pointer);
    return Success;
}

Export int ReadUShort(unsigned short* Pointer) {
    if (!Pointer) return Failure;
    return *Pointer;
}

Export unsigned int AddUInts(unsigned int A, unsigned int B) {
  return A + B;
}

Export unsigned int SubtractUInts(unsigned int A, unsigned int B) {
  return A - B;
}

Export int FillMemoryI8(void* Pointer, Byte Value, int Count) {
    if (!Pointer) return Failure;
    _Fill<Byte>(Pointer, Value, Count);
    return Success;
}

Export int FillMemoryI16(void* Pointer, short Value, int Count) {
    if (!Pointer) return Failure;
    _Fill<Word>(Pointer, Value, Count);
    return Success;
}

Export int FillMemoryI32(void* Pointer, int Value, int Count) {
    if (!Pointer) return Failure;
    _Fill<DoubleWord>(Pointer, Value, Count);
    return Success;
}

Export int CopyBytes(void* Dest, void* Source, int Count) {
    if (!Dest) return Failure;
    if (!Source) return Failure;
    if (Count < 0) return Failure;
    _Copy<Byte>(Dest, Source, Count);
    return Success;
}

Export int GetStringLength(char* str) {
  return strlen(str);
}

Export char* StringCopy(char* dest, char* src) {
  return strcpy(dest, src);
}

Export int Floor(float Value) {
    return floor(Value);
}

Export int Ceil(float Value) {
    return ceil(Value);
}

Export int ShiftLeft(int Value, int Bits) {
  return Value << Bits;
}

Export int ShiftRight(int Value, int Bits) {
  return Value >> Bits;
}

Export int Bitmask(int Start, int Count) {
int v = 0, l = 0;
  if (Count <= 0) return 0;
  if (Start < 0) return 0;
  if (Start > 31) return 0;
  if ((Start + Count) > 32) Count = 32 - Start;
  for (int i = 1; i <= Count; i++) {
    l = (i + Start) - 1;
    if (l > 0) {
      v |= 1 << (l);
    } else if (l == 0) {
      v |= 1;
    }
  }
  return v;
}

Export int MaskBits(int Value, int Start, int Count) {
int m = Bitmask(Start, Count);
  if (Count <= 0) return 0;
  if (Start < 0) return 0;
  if (Start > 31) return 0;
  return (Value & m) >> Start;
}

Export int Depalettize(Pixel *Dest, Byte *Source, Pixel *Palette, int BPP, int Count) {
int m = Bitmask(0, BPP);
int o = 0, i = 0, s = 0;
int v = 0;
Word value = 0;
  if (!Dest) return Failure;
  if (!Source) return Failure;
  if (!Palette) return Failure;
  if (BPP > 8) return Failure;
  for (i = 0; i < Count; i++) {
    o = (i * BPP) % 8;
    s = (i * BPP) / 8;
    value = 0;
    if ((o + BPP) <= 8) {
      _Copy<Byte>(&value, (Source + s), 1);
    } else {
      _Copy<Byte>(&value, (Source + s), 2);
    }
    v = (value >> o) & m;
    *Dest = Palette[v];
    Dest++;
  }
  return Success;
}

Export int PlanarDepalettize(Pixel *Dest, Byte *Source, Pixel *Palette, int BPP, int Planes, int Count) {
  if (!Dest) return Failure;
  if (!Source) return Failure;
  if (!Palette) return Failure;
  if (BPP > 8) return Failure;
  if (Planes < 1) return Failure;
  if (Planes > BPP) return Failure;
Byte **source = new Byte*[Planes];
int *offset = new int[Planes];
int *m = new int[Planes];
int i = 0, p = 0;
int bits = BPP / Planes;
int plane_size = (Count * bits);
DoubleWord value = 0, v = 0;
  for (p = 0; p < Planes; p++) {
    m[p] = Bitmask(p * bits, bits);
    i = (plane_size * p) - (p * bits);
    source[p] = Source + (i / 8);
    offset[p] = i % 8;
  }
  for (i = 0; i < Count; i++) {
    v = 0;
    for (p = 0; p < Planes; p++) {
      value = 0;
//      if ((offset[p] + bits) < 8) {
//        _Copy<Byte>(&value, (source[p]), 1);
//      } else {
        _Copy<Byte>(&value, (source[p]), 2);
//      }
      value >>= offset[p];
      value &= m[p];
      v |= value;
      offset[p] += bits;
      if (offset[p] > 7) {
        source[p] += (offset[p] / 8);
        offset[p] = offset[p] % 8;
      }
    }
    *Dest = Palette[v];
    Dest++;
  }
  delete[] source;
  delete[] offset;
  delete[] m;
  return Success;
}