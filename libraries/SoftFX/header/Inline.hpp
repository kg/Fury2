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

#include "memcpy_amd.hpp"

namespace Processor {
    extern unsigned int Features;
    extern bool MMX;
    extern bool MMX_CMov;
    extern bool SSE;
    extern bool SSE2;

    enum Feature_Constants {
        Feature_MMX_CMov = 1 << 15,
        Feature_MMX = 1 << 23,
        Feature_SSE = 1 << 25,
        Feature_SSE2 = 1 << 26
    };
};

#define ref(x) *(&(x)) = x

FPoint inline AngleVector(float Angle) {
  FPoint temp;
  float r = Radians(Angle);
  temp.X = sin(r);
  temp.Y = -cos(r);
  return temp;
}

float inline NormalizeAngle(float Angle) {
  float n = Angle / 360.0f;
  return Angle - (floor(n) * 360.0f);
}

Export inline int VectorInRange(FPoint A, FPoint B, FPoint X) {
  bool sign = A.cross(B) >= 0.0f;
  if (sign) {
    bool a = A.cross(X) >= 0.0f;
    bool b = X.cross(B) >= 0.0f;
    return (a && b);
  } else {
    bool a = B.cross(X) > 0.0f;
    bool b = X.cross(A) > 0.0f;
    return (a && b);
  }
}

Export inline int AngleInRange(float A, float B, float X) {
  return VectorInRange(AngleVector(A), AngleVector(B), AngleVector(X));
}

template <class TA, class TB> float inline AngleBetween(TA a, TB b) {
  float Rx = 0.0f, Ry = 0.0f;
  Rx = (b.X - a.X);
  Ry = (b.Y - a.Y);
  if ((Rx == 0.0f) && (Ry == 0.0f)) {
    return 0.0f;
  } else if (Ry == 0.0f) {
    if (Rx > 0.0f) {
      return 90.0f;
    } else {
      return 270.0f;
    }
  } else if (Rx == 0.0f) {
    if (Ry > 0.0f) {
      return 180.0f;
    } else {
      return 0.0f;
    }
  }
  bool xl = (Rx < 0.0f);
  bool yl = (Ry < 0.0f);
  if (yl && xl) {
    return (atan(Ry / Rx) / Radian) + 270.0f;
  } else if ((!yl) && xl) {
    return (atan(Ry / Rx) / Radian) + 270.0f;
  } else if (yl && (!xl)) {
    return (atan(Ry / Rx) / Radian) + 90.0f;
  } else {
    return (atan(Ry / Rx) / Radian) + 90.0f;
  }
}

template <class T> T inline _Distance(T x, T y) {
  return sqrt((double)(x * x) + (y * y));
}

template <class T> T inline _Max(T one, T two) {
  return (one > two) ? one : two;
}

template <class T> T inline _One(T value) {
  return (value < 0) ? 0 : 
    (value > 1) ? 1 : value;
//int iClipped;
//int iValue = *reinterpret_cast<int*>(&value);
//float one = 1.0f;
//int iOne = *reinterpret_cast<int*>(&one);
//  iClipped = -(int)(value < 0);
//  iValue = (iValue & ~iClipped);
//  iClipped = -(int)(value > 0);
//  iValue = ((iOne & iClipped) | (iValue & ~iClipped));
//  return *reinterpret_cast<T*>(&iValue);
}

template <class T> T inline _Min(T one, T two) {
  return (one < two) ? one : two;
}

template <class T> DoubleWord inline _ToDoubleWord(const T& value) {
    DoubleWord v = 0;
    switch (sizeof(T)) {
    case 4:
        v = *(DoubleWord*)&value;
        break;
    case 3:
        v = ((*(Byte*)((DoubleWord)(&value)+2)) << 16) | *(Word*)&value;
        break;
    case 2:
        v = *(Word*)&value;
        break;
    case 1:
        v = *(Byte*)&value;
        break;
    }
    return v;
}

template <class T> void inline _Pack(void* dest, const DoubleWord bytes, const T& value) {
#if (defined(ASSEMBLY))
    DoubleWord sz = sizeof(T);
    if (sz <= 4) {
        DoubleWord v = _ToDoubleWord(value);
        if (sz == 4) {
            _asm {
                cld
                mov edi, dest
                mov eax, v
                mov ecx, bytes
                shr ecx, 2
                repnz stosd
            }
        } else if (sz == 2) {
            _asm {
                cld
                mov edi, dest
                mov eax, v
                mov ecx, bytes
                shr ecx, 1
                repnz stosw
            }
        } else {
            _asm {
                cld
                mov edi, dest
                mov eax, v
                mov ecx, bytes
                repnz stos
            }
        }
    } else {
        T *d = (T *)dest;
        T *e = (T *)((DoubleWord)dest + bytes);
        while(d < e) *d++ = value;
    }
#else
    T *d = (T *)dest;
    T *e = (T *)((DoubleWord)dest + bytes);
    while(d < e) *d++ = value;
#endif
}

void inline _SmallCopy(void* dest, void* source, DoubleWord bytes) {
  memcpy(dest, source, bytes);
}

template <class T> void inline _Copy(void* dest, void* source, DoubleWord count) {
#if (defined(ASSEMBLY))
    if ((Processor::SSE)) {
#if (defined(DISABLE_SSE))
        memcpy(dest, source, count * sizeof(T));
#else
        memcpy_amd(dest, source, count * sizeof(T));
#endif
    } else {
        // screw writing my own copy, pfft
        memcpy(dest, source, count * sizeof(T));
    }
#else
    memcpy(dest, source, count * sizeof(T));
#endif
    return;
}

/*
    DoubleWord sz = sizeof(T);
    DoubleWord bcount = count * sz;
    Align(16) DoubleWord v[4];
    _Pack<T>(&v[0], 16, value);
    if ((bcount >= 128) && ((bcount % 128) == 0) && (Processor::SSE)) {
        if ((DoubleWord)dest % 16 == 0) {
            _asm {
                cld
                mov edi, dest
                mov ecx, bcount
                shr ecx, 7
                movdqa xmm0, [v]
                
            loop128:
                movntdq [edi], xmm0
                movntdq [edi+16], xmm0
                movntdq [edi+32], xmm0
                movntdq [edi+48], xmm0
                movntdq [edi+64], xmm0
                movntdq [edi+80], xmm0
                movntdq [edi+96], xmm0
                movntdq [edi+112], xmm0

                add edi, 128
                loop loop128
                emms
            }
        } else {
            _asm {
                cld
                mov edi, dest
                mov ecx, bcount
                shr ecx, 7
                movdqa xmm0, [v]
                
            loop128u:
                movdqu [edi], xmm0
                movdqu [edi+16], xmm0
                movdqu [edi+32], xmm0
                movdqu [edi+48], xmm0
                movdqu [edi+64], xmm0
                movdqu [edi+80], xmm0
                movdqu [edi+96], xmm0
                movdqu [edi+112], xmm0

                add edi, 128
                loop loop128u
                emms
            }
        }
    } else if ((bcount >= 4) && ((bcount % 4) == 0)) {
        _asm {
            cld
            mov edi, dest
            mov eax, v[0]
            mov ecx, bcount
            shr ecx, 2
            repnz stosd
        }
    } else if ((bcount >= 2) && ((bcount % 2) == 0)) {
        _asm {
            cld
            mov edi, dest
            mov eax, v[0]
            mov ecx, bcount
            shr ecx, 1
            repnz stosw
        }
    } else {
        _asm {
            cld
            mov edi, dest
            mov eax, v[0]
            mov ecx, bcount
            repnz stos
        }
    }
*/

template <class T> void inline _Fill(void* dest, const T& value, DoubleWord count) {
#if (defined(ASSEMBLY))
    DoubleWord sz = sizeof(T);
    DoubleWord bcount = count * sz;
    Align(16) DoubleWord v[4];
    _Pack<T>(&v[0], 16, value);
#if (defined(DISABLE_SSE))
    if (0) {
#else
    if ((bcount >= 16) && ((bcount % 16) == 0) && (Processor::SSE)) {
        if ((DoubleWord)dest % 16 == 0) {
            _asm {
                cld
                mov edi, dest
                mov ecx, bcount
                shr ecx, 4
                movdqa xmm0, [v]
                
            loop16:
                movntdq [edi], xmm0

                add edi, 16
                loop loop16
                emms
            }
        } else {
            _asm {
                cld
                mov edi, dest
                mov ecx, bcount
                shr ecx, 4
                movdqa xmm0, [v]
                
            loop16u:
                movdqu [edi], xmm0

                add edi, 16
                loop loop16u
                emms
            }
        }
#endif
    } else if ((bcount >= 4) && ((bcount % 4) == 0)) {
        _asm {
            cld
            mov edi, dest
            mov eax, v[0]
            mov ecx, bcount
            shr ecx, 2
            repnz stosd
        }
    } else if ((bcount >= 2) && ((bcount % 2) == 0)) {
        _asm {
            cld
            mov edi, dest
            mov eax, v[0]
            mov ecx, bcount
            shr ecx, 1
            repnz stosw
        }
    } else {
        _asm {
            cld
            mov edi, dest
            mov eax, v[0]
            mov ecx, bcount
            repnz stos
        }
    }
#else
    T *d = (T *)dest;
    T s = value;
    while(count--) *d++ = s;
#endif
}

template <class T> void inline _Swap(T& value1, T& value2) {
T temp;
    temp = value2;
    value2 = value1;
    value1 = temp;
    return;
}

template <class T> void inline _Swap(void* dest, void* source, DoubleWord count) {
#if (defined(ASSEMBLY))
    DoubleWord sz = sizeof(T);
    DoubleWord bcount = count * sz;
    if ((bcount >= 64) && ((bcount % 64) == 0) && (Processor::SSE)) {
        if (((DoubleWord)dest % 16 == 0) && ((DoubleWord)source % 16 == 0)) {
            _asm {
                cld
                mov edi, dest
                mov esi, source
                mov ecx, bcount
                shr ecx, 6
                
            loop64:
                movdqa xmm0,[esi]
                movdqa xmm1,[esi+16]
                movdqa xmm2,[esi+32]
                movdqa xmm3,[esi+48]
                movdqa xmm4,[edi]
                movdqa xmm5,[edi+16]
                movdqa xmm6,[edi+32]
                movdqa xmm7,[edi+48]

                movdqa [edi], xmm0
                movdqa [edi+16], xmm1
                movdqa [edi+32], xmm2
                movdqa [edi+48], xmm3
                movdqa [esi], xmm4
                movdqa [esi+16], xmm5
                movdqa [esi+32], xmm6
                movdqa [esi+48], xmm7

                add edi, 64
                add esi, 64
                loop loop64
                emms
            }
        } else {
            _asm {
                cld
                mov edi, dest
                mov esi, source
                mov ecx, bcount
                shr ecx, 6
                
            loop64u:
                movdqu xmm0,[esi]
                movdqu xmm1,[esi+16]
                movdqu xmm2,[esi+32]
                movdqu xmm3,[esi+48]
                movdqu xmm4,[edi]
                movdqu xmm5,[edi+16]
                movdqu xmm6,[edi+32]
                movdqu xmm7,[edi+48]

                movdqu [edi], xmm0
                movdqu [edi+16], xmm1
                movdqu [edi+32], xmm2
                movdqu [edi+48], xmm3
                movdqu [esi], xmm4
                movdqu [esi+16], xmm5
                movdqu [esi+32], xmm6
                movdqu [esi+48], xmm7

                add edi, 64
                add esi, 64
                loop loop64u
                emms
            }
        }
    } else {
        T t;
        T *d = (T *)dest, *s = (T *)source;
        while(count--) {
            t = *d;
            *d++ = *s;
            *s++ = t;
        }
    }
#else
    T t;
    T *d = (T *)dest, *s = (T *)source;
    while(count--) {
        t = *d;
        *d++ = *s;
        *s++ = t;
    }
#endif
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
  return (value < 0) ? 0 : value;
#else
  return ((value) & (-(int)!(value < 0))) & 0xFF;
#endif
}

Export inline Byte ClipByteHigh(int value) {
#ifdef USEIFS
  return (value > 255) ? 255 : value;
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

template <class T> inline T InlineIf(bool condition, T ifTrue, T ifFalse) {
int mask;
    mask = -(int)(condition);
    return (ifTrue & mask) | (ifFalse & ~mask);
}

template <class T> inline T InlineIf(bool condition, T ifTrue) {
    return (ifTrue & -(int)(condition));
}

Export inline int WrapValue(int value, int minimum, int maximum) {
  int d = maximum - minimum + 1;
  if (value < minimum) {
    int n = abs(minimum - value);
    int i = n % d;
    return minimum + ((d - i) % d);
  } else if (value > maximum) {
    int n = abs(value - maximum - 1);
    int i = n % d;
    return minimum + i;
  } else {
    return value;
  }
}

inline float Round(float N) {
  return (N < 0) ? ceil(N - 0.5f) : floor(N + 0.5f);
}

inline void RotatePoint(float &X, float &Y, float AngleInRadians) {
  float theta = atan2(Y, X), distance = sqrt((X * X) + (Y * Y));
  if (X < 0) {
    theta += Pi;
  }
  theta += AngleInRadians;
  X = distance * cos(theta);
  Y = distance * sin(theta);
  return;
}

inline void Generate4Points(float W, float H, float *X, float *Y) {
  X[0] = -W;
  Y[0] = -H;
  X[1] = W;
  Y[1] = -H;
  X[2] = W;
  Y[2] = H;
  X[3] = -W;
  Y[3] = H;
};

inline void Rotate4Points(float W, float H, float AngleInRadians, float *X, float *Y, float t, float d) {
  if (AngleInRadians == 0) {
    Generate4Points(W, H, X, Y);
    return;
  }
  if (W < 0) 
    t += Pi;
  t += AngleInRadians;
  X[2] = cos(t) * d;
  Y[2] = sin(t) * d;
  t += Radians(90);
  X[3] = cos(t) * d;
  Y[3] = sin(t) * d;
  t += Radians(90);
  X[0] = cos(t) * d;
  Y[0] = sin(t) * d;
  t += Radians(90);
  X[1] = cos(t) * d;
  Y[1] = sin(t) * d;
}

inline void Rotate4Points(float W, float H, float AngleInRadians, float *X, float *Y, float t) {
  if (AngleInRadians == 0) {
    Generate4Points(W, H, X, Y);
    return;
  }
  Rotate4Points(W, H, AngleInRadians, X, Y, t, sqrt((W * W) + (H * H)));
}

inline void Rotate4Points(float W, float H, float AngleInRadians, float *X, float *Y) {
  if (AngleInRadians == 0) {
    Generate4Points(W, H, X, Y);
    return;
  }
  Rotate4Points(W, H, AngleInRadians, X, Y, atan2(H, W), sqrt((W * W) + (H * H)));
}

template <class T> inline T clamp(T value, T min, T max) 
{
  if (value < min) return min;
  else if (value > max) return max;
  else return value;
}

template <class T> struct fastqueue {
  T* Data;
  int InsertionPoint;
  int ReadPoint;
  int Max;

  fastqueue(int capacity) {
    Data = new T[capacity];
    InsertionPoint = 0;
    ReadPoint = 0;
    Max = capacity - 1;
  }

  ~fastqueue() {
    if (Data != Null) {
      delete[] Data;
      Data = Null;
      InsertionPoint = 0;
      ReadPoint = 0;
      Max = 0;
    }
  }

  inline void push(T value) {
    Data[InsertionPoint] = value;
    InsertionPoint = WrapValue(InsertionPoint + 1, 0, Max);
  }

  inline void pop(T& out) {
    out = Data[ReadPoint];
    ReadPoint = WrapValue(ReadPoint + 1, 0, Max);
  }

  inline void clear() {
    InsertionPoint = 0;
    ReadPoint = 0;
    Max = 0;
  }
};