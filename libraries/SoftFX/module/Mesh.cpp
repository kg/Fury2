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
#include "../header/Convolve.hpp"
#include "../header/MersenneTwister.h"

Export int SetMesh(MeshParam* Mesh, float X, float Y) {
  if (!Mesh) return Failure;
  MeshPoint *pt;
  for (int iY = 0; iY < Mesh->Height; iY++) {
    for (int iX = 0; iX < Mesh->Width; iX++) {
      pt = Mesh->getPoint(iX, iY);
      pt->X = X;
      pt->Y = Y;
    }
  }
  return Success;
}

Export int TranslateMesh(MeshParam* Mesh, float X, float Y) {
  if (!Mesh) return Failure;
  MeshPoint *pt;
  for (int iY = 0; iY < Mesh->Height; iY++) {
    for (int iX = 0; iX < Mesh->Width; iX++) {
      pt = Mesh->getPoint(iX, iY);
      pt->X += X;
      pt->Y += Y;
    }
  }
  return Success;
}

Export int ScaleMesh(MeshParam* Mesh, float X, float Y) {
  if (!Mesh) return Failure;
  MeshPoint *pt;
  for (int iY = 0; iY < Mesh->Height; iY++) {
    for (int iX = 0; iX < Mesh->Width; iX++) {
      pt = Mesh->getPoint(iX, iY);
      pt->X *= X;
      pt->Y *= Y;
    }
  }
  return Success;
}

Export int RandomizeMesh(MeshParam* Mesh, float XOffset, float YOffset, float XScale, float YScale) {
  if (!Mesh) return Failure;
  MeshPoint *pt;
  MTRand mersenne = MTRand();
  for (int iY = 0; iY < Mesh->Height; iY++) {
    for (int iX = 0; iX < Mesh->Width; iX++) {
      pt = Mesh->getPoint(iX, iY);
      pt->X += (mersenne.rand() + XOffset) * XScale;
      pt->Y += (mersenne.rand() + YOffset) * YScale;
    }
  }
  return Success;
}

Export int MeshFromHeightmap(MeshParam* Mesh, Image* Source, float Scale, Rectangle* SourceRect) {
  if (!Mesh) return Failure;
  if (!Source) return Failure;
  if (!SourceRect) return Failure;
  float pX1 = SourceRect->Left + 1;
  float pY1 = SourceRect->Top + 1;
  float pX2 = SourceRect->right_exclusive();
  float pY2 = SourceRect->bottom_exclusive();
  float pXi = (pX2 - pX1) / (Mesh->Width);
  float pYi = (pY2 - pY1) / (Mesh->Height);
  float pY = pY1;
  int xOffset, yOffset;
  int ipX, ipY;
  Pixel a, b;
  MeshPoint* pt;
  Scale *= (1.0f / 255.0f);
  for (int iY = 0; iY < Mesh->Height; iY++) {
    ipY = pY;
    float pX = pX1;
    for (int iX = 0; iX < Mesh->Width; iX++) {
      ipX = pX;
      a = Source->getPixelClipNO(ipX - 1, ipY);
      b = Source->getPixelClipNO(ipX, ipY);
      xOffset = -(a[::Red]) + (b[::Red]);
      a = Source->getPixelClipNO(ipX, ipY - 1);
      yOffset = -(a[::Red]) + (b[::Red]);
      pt = Mesh->getPointFast(iX, iY);
      pt->X += (xOffset) * Scale;
      pt->Y += (yOffset) * Scale;
      pX += pXi;
    }
    pY += pYi;
  }
  return Success;
}