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

float inline NormalizeAngle(float Angle) {
  float n = Angle / 360;
  return Angle - (floor(n) * 360);
}

template <class TA, class TB> float inline AngleBetween(TA a, TB b) {
  float Rx = 0, Ry = 0;
  Rx = (b.X - a.X);
  Ry = (b.Y - a.Y);
  if ((Rx == 0) && (Ry == 0)) {
    return 0;
  } else if (Ry == 0) {
    if (Rx > 0) {
      return 90;
    } else {
      return 270;
    }
  } else if (Rx == 0) {
    if (Ry > 0) {
      return 180;
    } else {
      return 0;
    }
  }
  if ((Ry < 0) && (Rx < 0)) {
    return (atan(Ry / Rx) / Radian) + 270;
  } else if ((Ry >= 0) && (Rx < 0)) {
    return (atan(Ry / Rx) / Radian) + 270;
  } else if ((Ry < 0) && (Rx >= 0)) {
    return (atan(Ry / Rx) / Radian) + 90;
  } else {
    return (atan(Ry / Rx) / Radian) + 90;
  }
}

template <class T> T inline _Distance(T x, T y) {
    return sqrt((double)(x * x) + (y * y));
}

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

template <class T> void inline _Copy(void* dest, void* source, DoubleWord count) {
#if (defined(ASSEMBLY))
    DoubleWord sz = sizeof(T);
    DoubleWord bcount = count * sz;
    if ((bcount >= 128) && ((bcount % 128) == 0) && (Processor::SSE)) {
        if (((DoubleWord)dest % 16 == 0) && ((DoubleWord)source % 16 == 0)) {
            _asm {
                cld
                mov edi, dest
                mov esi, source
                mov ecx, bcount
                shr ecx, 7
                
            loop128:
                movaps xmm0,[esi]
                movaps xmm1,[esi+16]
                movaps xmm2,[esi+32]
                movaps xmm3,[esi+48]
                movaps xmm4,[esi+64]
                movaps xmm5,[esi+80]
                movaps xmm6,[esi+96]
                movaps xmm7,[esi+112]

                movaps [edi], xmm0
                movaps [edi+16], xmm1
                movaps [edi+32], xmm2
                movaps [edi+48], xmm3
                movaps [edi+64], xmm4
                movaps [edi+80], xmm5
                movaps [edi+96], xmm6
                movaps [edi+112], xmm7

                add edi, 128
                add esi, 128
                loop loop128
                emms
            }
        } else {
            _asm {
                cld
                mov edi, dest
                mov esi, source
                mov ecx, bcount
                shr ecx, 7
                
            loop128u:
                movups xmm0,[esi]
                movups xmm1,[esi+16]
                movups xmm2,[esi+32]
                movups xmm3,[esi+48]
                movups xmm4,[esi+64]
                movups xmm5,[esi+80]
                movups xmm6,[esi+96]
                movups xmm7,[esi+112]

                movups [edi], xmm0
                movups [edi+16], xmm1
                movups [edi+32], xmm2
                movups [edi+48], xmm3
                movups [edi+64], xmm4
                movups [edi+80], xmm5
                movups [edi+96], xmm6
                movups [edi+112], xmm7

                add edi, 128
                add esi, 128
                loop loop128u
                emms
            }
        }
    } else {
        // screw writing my own copy, pfft
        memcpy(dest, source, count * sizeof(T));
    }
#else
    T *d = (T *)dest, *s = (T *)source;
    while(count--) *d++ = *s++;
#endif
    return;
}

template <class T> void inline _Fill(void* dest, const T& value, DoubleWord count) {
#if (defined(ASSEMBLY))
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
                movaps xmm0, [v]
                
            loop128:
                movaps [edi], xmm0
                movaps [edi+16], xmm0
                movaps [edi+32], xmm0
                movaps [edi+48], xmm0
                movaps [edi+64], xmm0
                movaps [edi+80], xmm0
                movaps [edi+96], xmm0
                movaps [edi+112], xmm0

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
                movaps xmm0, [v]
                
            loop128u:
                movups [edi], xmm0
                movups [edi+16], xmm0
                movups [edi+32], xmm0
                movups [edi+48], xmm0
                movups [edi+64], xmm0
                movups [edi+80], xmm0
                movups [edi+96], xmm0
                movups [edi+112], xmm0

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
                movaps xmm0,[esi]
                movaps xmm1,[esi+16]
                movaps xmm2,[esi+32]
                movaps xmm3,[esi+48]
                movaps xmm4,[edi]
                movaps xmm5,[edi+16]
                movaps xmm6,[edi+32]
                movaps xmm7,[edi+48]

                movaps [edi], xmm0
                movaps [edi+16], xmm1
                movaps [edi+32], xmm2
                movaps [edi+48], xmm3
                movaps [esi], xmm4
                movaps [esi+16], xmm5
                movaps [esi+32], xmm6
                movaps [esi+48], xmm7

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
                movups xmm0,[esi]
                movups xmm1,[esi+16]
                movups xmm2,[esi+32]
                movups xmm3,[esi+48]
                movups xmm4,[edi]
                movups xmm5,[edi+16]
                movups xmm6,[edi+32]
                movups xmm7,[edi+48]

                movups [edi], xmm0
                movups [edi+16], xmm1
                movups [edi+32], xmm2
                movups [edi+48], xmm3
                movups [esi], xmm4
                movups [esi+16], xmm5
                movups [esi+32], xmm6
                movups [esi+48], xmm7

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

inline int ClipValue(int value, int max) {
#ifdef USEIFS
  if (value > max) {
    return max;
  } else {
    return value;
  }
#else
int iClipped;
    value = (value & (-(int)!(value < 0)));
    iClipped = -(int)(value > max);
    return (max & iClipped) | (value & ~iClipped);
#endif
}

Export inline int ClipValue(int value, int min, int max) {
#ifdef USEIFS
  if (value < min) {
    return min;
  } else if (value > max) {
    return max;
  } else {
    return value;
  }
#else
int iClipped;
    iClipped = -(int)(value < min);
    value = (min & iClipped) | (value & ~iClipped);
    iClipped = -(int)(value > max);
    return (max & iClipped) | (value & ~iClipped);
#endif
}

Export inline int WrapValue(int value, int min, int max) {
	if (value < min) {
		int v = ((min - value) % ((max - min) + 1));
		return max + 1 - v;
	} else {
		int v = ((value - min) % ((max - min) + 1));
		return min + v;
	}
}

inline float Round(float N)
{
     return floor(N + .5);
}

inline void RotatePoint(float &X, float &Y, float AngleInRadians) {
  float theta = atan(Y / X), distance = sqrt((X * X) + (Y * Y));
  if (X < 0) {
    theta += Pi;
  }
  theta += AngleInRadians;
  X = distance * cos(theta);
  Y = distance * sin(theta);
  return;
}