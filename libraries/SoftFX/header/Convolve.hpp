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

namespace Convolution {
  struct Filter {
    float *Weights;
    int Width, Height;
    int XOffset, YOffset;
    float Divisor;
    float Offset;
  };
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