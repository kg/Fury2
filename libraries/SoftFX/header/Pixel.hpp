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

#pragma pack(1)

enum ColorChannels {
	Red = 2,
	Green = 1,
	Blue = 0,
	Alpha = 3
};

class Pixel {
public:
    DoubleWord V;

    inline bool operator!=(Pixel &rhs) {
      return (this->V != rhs.V);
    }

    inline bool operator==(Pixel &rhs) {
      return (this->V == rhs.V);
    }

    inline Channel& operator[](ColorChannels i) {
#ifdef SAFE_PIXEL
      assert((i >= 0) && (i <= 3));
#endif
      return ((Channel*)&(this->V))[i];
//      return *(((Channel*)&(this->V) + i));
    }

    inline Pixel() {
        this->V = 0;
    }

    inline Pixel(DoubleWord Value) {
        this->V = Value;
    }

    inline Pixel(DoubleWord R, DoubleWord G, DoubleWord B, DoubleWord A) {
        (*this)[::Blue] = B;
        (*this)[::Green] = G;
        (*this)[::Red] = R;
        (*this)[::Alpha] = A;
    }

    inline Pixel(DoubleWord R, DoubleWord G, DoubleWord B) {
        (*this)[::Blue] = B;
        (*this)[::Green] = G;
        (*this)[::Red] = R;
        (*this)[::Alpha] = 255;
    }

    inline Pixel(Pixel P1, Pixel P2, float A1) {
        Byte a1 = ClipByte(A1 * 255.0f);
        AlphaLevel *al1, *al2;
        al1 = AlphaLevelLookup(a1 ^ 0xFF);
        al2 = AlphaLevelLookup(a1);
        (*this)[::Blue] = AlphaFromLevel2(al1, P1[::Blue], al2, P2[::Blue]);
        (*this)[::Green] = AlphaFromLevel2(al1, P1[::Green], al2, P2[::Green]);
        (*this)[::Red] = AlphaFromLevel2(al1, P1[::Red], al2, P2[::Red]);
        (*this)[::Alpha] = AlphaFromLevel2(al1, P1[::Alpha], al2, P2[::Alpha]);
    }

    inline Byte getHexByte(const wchar_t *ptr) {
      wchar_t buf[3];
      buf[0] = *ptr;
      buf[1] = *(ptr+1);
      buf[2] = 0;
      return wcstoul(buf, 0, 16);
    }

    inline Pixel(std::basic_string<wchar_t> &string) {
      Byte r, g, b, a;
      switch(string.size()) {
      case 2:
        r = g = b = getHexByte(string.c_str());
        a = 255;
        break;
      case 4:
        r = g = b = getHexByte(string.c_str());
        a = getHexByte(string.c_str() + 2);
        break;
      case 6:
        r = getHexByte(string.c_str());
        g = getHexByte(string.c_str() + 2);
        b = getHexByte(string.c_str() + 4);
        a = 255;
        break;
      case 8:
        r = getHexByte(string.c_str());
        g = getHexByte(string.c_str() + 2);
        b = getHexByte(string.c_str() + 4);
        a = getHexByte(string.c_str() + 6);
        break;
      default:
        this->V = 0;
        return;
      }
      (*this)[::Blue] = b;
      (*this)[::Green] = g;
      (*this)[::Red] = r;
      (*this)[::Alpha] = a;
    }

    inline void setGray(DoubleWord V) {
        (*this)[::Blue] = V;
        (*this)[::Green] = V;
        (*this)[::Red] = V;
    }
};

#pragma pack()
