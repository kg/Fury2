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
        (*this)[::Blue] = P1[::Blue] + ((P2[::Blue] - P1[::Blue]) * a1 / 255);
        (*this)[::Green] = P1[::Green] + ((P2[::Green] - P1[::Green]) * a1 / 255);
        (*this)[::Red] = P1[::Red] + ((P2[::Red] - P1[::Red]) * a1 / 255);
        (*this)[::Alpha] = P1[::Alpha] + ((P2[::Alpha] - P1[::Alpha]) * a1 / 255);
    }

    inline void setGray(DoubleWord V) {
        (*this)[::Blue] = V;
        (*this)[::Green] = V;
        (*this)[::Red] = V;
    }
};

#pragma pack()
