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

Export int GetRed(Pixel Color) {
    return Color[::Red];
}

Export int GetGreen(Pixel Color) {
    return Color[::Green];
}

Export int GetBlue(Pixel Color) {
    return Color[::Blue];
}

Export int GetGray(Pixel Color) {
    return (((int)Color[::Blue] * 11) + ((int)Color[::Green] * 59) + ((int)Color[::Red] * 30)) / 100;
}

Export int GetAlpha(Pixel Color) {
    return Color[::Alpha];
}

Export DoubleWord SetRed(Pixel Color, int Value) {
    Color[::Red] = ClipByte(Value);
    return Color.V;
}

Export DoubleWord SetGreen(Pixel Color, int Value) {
    Color[::Green] = ClipByte(Value);
    return Color.V;
}

Export DoubleWord SetBlue(Pixel Color, int Value) {
    Color[::Blue] = ClipByte(Value);
    return Color.V;
}

Export DoubleWord SetGray(Pixel Color, int Value) {
    Color[::Red] = Color[::Green] = Color[::Blue] = ClipByte(Value);
    return Color.V;
}

Export DoubleWord SetAlpha(Pixel Color, int Value) {
    Color[::Alpha] = ClipByte(Value);
    return Color.V;
}

Export DoubleWord BGRA(int R, int G, int B, int A) {
    return Pixel(ClipByte(R), ClipByte(G), ClipByte(B), ClipByte(A)).V;
}

Export DoubleWord BGR(int R, int G, int B) {
    return Pixel(ClipByte(R), ClipByte(G), ClipByte(B)).V;
}

Export DoubleWord RGBA(int R, int G, int B, int A) {
    return Pixel(ClipByte(B), ClipByte(G), ClipByte(R), ClipByte(A)).V;
}

Export DoubleWord RGB(int R, int G, int B) {
    return Pixel(ClipByte(B), ClipByte(G), ClipByte(R)).V;
}

Export DoubleWord Gray(int V) {
    return Pixel(ClipByte(V), ClipByte(V), ClipByte(V)).V;
}

Export DoubleWord GrayA(int V, int A) {
    return Pixel(ClipByte(V), ClipByte(V), ClipByte(V), ClipByte(A)).V;
}

Export DoubleWord SwapChannels(Pixel Color, int Channel1, int Channel2) {
    _Swap<Byte>(Color[(ColorChannels)ClipValue(Channel1, 0, 3)], Color[(ColorChannels)ClipValue(Channel2, 0, 3)]);
    return Color.V;
}

Export DoubleWord SetChannel(Pixel Color, int Channel, int Value) {
    Color[(ColorChannels)ClipValue(Channel, 0, 3)] = ClipByte(Value);
    return Color.V;
}

Export DoubleWord GetChannel(Pixel Color, int Channel) {
    return Color[(ColorChannels)ClipValue(Channel, 0, 3)];
}

Export DoubleWord BlendColors(Pixel Dest, Pixel Source, int Opacity) {
    Opacity = ClipByte(Opacity);
    Dest[::Blue] = AlphaLookup(Dest[::Blue], Opacity ^ 0xFF) + AlphaLookup(Source[::Blue], Opacity);
    Dest[::Green] = AlphaLookup(Dest[::Green], Opacity ^ 0xFF) + AlphaLookup(Source[::Green], Opacity);
    Dest[::Red] = AlphaLookup(Dest[::Red], Opacity ^ 0xFF) + AlphaLookup(Source[::Red], Opacity);
    Dest[::Alpha] = AlphaLookup(Dest[::Alpha], Opacity ^ 0xFF) + AlphaLookup(Source[::Alpha], Opacity);
    return Dest.V;
}

Export DoubleWord MultiplyColor(Pixel Color, Pixel Mul, int Opacity) {
    Opacity = ClipByte(Opacity);
    int White = Opacity ^ 0xFF;
    Color[::Blue] = AlphaLookup(Color[::Blue], AlphaLookup(Mul[::Blue], Opacity) + White);
    Color[::Green] = AlphaLookup(Color[::Green], AlphaLookup(Mul[::Green], Opacity) + White);
    Color[::Red] = AlphaLookup(Color[::Red], AlphaLookup(Mul[::Red], Opacity) + White);
    Color[::Alpha] = AlphaLookup(Color[::Alpha], AlphaLookup(Mul[::Alpha], Opacity) + White);
    return Color.V;
}

Export DoubleWord ColorToGrayscale(Pixel Color) {
    Color.setGray(((Color[::Blue] * 11) + (Color[::Green] * 59) + (Color[::Red] * 30)) / 100);
    return Color.V;
}

Export DoubleWord InvertColor(Pixel Color) {
    Color[::Blue] ^= 0xFF;
    Color[::Green] ^= 0xFF;
    Color[::Red] ^= 0xFF;
    Color[::Alpha] ^= 0xFF;
    return Color.V;
}

Export DoubleWord InvertColorRGB(Pixel Color) {
    Color[::Blue] ^= 0xFF;
    Color[::Green] ^= 0xFF;
    Color[::Red] ^= 0xFF;
    return Color.V;
}

Export DoubleWord InvertChannel(Pixel Color, int Channel) {
    Color[(ColorChannels)ClipValue(Channel, 0, 3)] ^= 0xFF;
    return Color.V;
}