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
#include "../header/Clip.hpp"

bool enableClipping = true;

Export int Clip2D_Simple(Rectangle *Rect, Image *Dest, Image *Source, int &X, int &Y, int &SX, int &SY) {
    if (!Rect) return Failure;
    if (!Dest) return Failure;
    if (!Source) return Failure;
    Rect->Left = X;
    Rect->Top = Y;
    Rect->Width = Source->Width;
    Rect->Height = Source->Height;
    if (Rect->Top > Dest->ClipRectangle.bottom()) return Failure;
    if (Rect->Left > Dest->ClipRectangle.right()) return Failure;
    if (Rect->bottom() < Dest->ClipRectangle.Top) return Failure;
    if (Rect->right() < Dest->ClipRectangle.Left) return Failure;
    if (Dest->ClipRectangle.Width == 0) return Failure;
    if (Dest->ClipRectangle.Height == 0) return Failure;
    if (!enableClipping) return !Rect->empty();
    if (Rect->Left < Dest->ClipRectangle.Left) {
        Rect->Width -= (Dest->ClipRectangle.Left - Rect->Left);
        SX += (Dest->ClipRectangle.Left - Rect->Left);
        Rect->Left = Dest->ClipRectangle.Left;
    }
    if (Rect->Top < Dest->ClipRectangle.Top) {
        Rect->Height -= (Dest->ClipRectangle.Top - Rect->Top);
        SY += (Dest->ClipRectangle.Top - Rect->Top);
        Rect->Top = Dest->ClipRectangle.Top;
    }
    if (SX < 0) {
        Rect->Width += SX;
        SX = 0;
    }
    if (SY < 0) {
        Rect->Height += SY;
        SY = 0;
    }
    if ((SX + Rect->Width) > Source->Width) {
        Rect->Width = (Source->Width - SX);
    }
    if ((SY + Rect->Height) > Source->Height) {
        Rect->Height = (Source->Height - SY);
    }
    if (Rect->right() > Dest->ClipRectangle.right()) {
        Rect->Width -= (Rect->right() - Dest->ClipRectangle.right());
    }
    if (Rect->bottom() > Dest->ClipRectangle.bottom()) {
        Rect->Height -= (Rect->bottom() - Dest->ClipRectangle.bottom());
    }
    return !Rect->empty();
}

Export int Clip2D_SimpleRect(Rectangle *Rect, Image *Dest, Image *Source, Rectangle *DestRect, int &SX, int &SY) {
    if (!Rect) return Failure;
    if (!Dest) return Failure;
    if (!Source) return Failure;
    if (DestRect) {
        *Rect = *DestRect;
    } else {
        Rect->Left = Rect->Top = 0;
        Rect->Width = Source->Width;
        Rect->Height = Source->Height;
    }
    if (Rect->Top > Dest->ClipRectangle.bottom()) return Failure;
    if (Rect->Left > Dest->ClipRectangle.right()) return Failure;
    if (Rect->bottom() < Dest->ClipRectangle.Top) return Failure;
    if (Rect->right() < Dest->ClipRectangle.Left) return Failure;
    if (Dest->ClipRectangle.Width == 0) return Failure;
    if (Dest->ClipRectangle.Height == 0) return Failure;
    if (!enableClipping) return !Rect->empty();
    if (Rect->Left < Dest->ClipRectangle.Left) {
        Rect->Width -= (Dest->ClipRectangle.Left - Rect->Left);
        SX += (Dest->ClipRectangle.Left - Rect->Left);
        Rect->Left = Dest->ClipRectangle.Left;
    }
    if (Rect->Top < Dest->ClipRectangle.Top) {
        Rect->Height -= (Dest->ClipRectangle.Top - Rect->Top);
        SY += (Dest->ClipRectangle.Top - Rect->Top);
        Rect->Top = Dest->ClipRectangle.Top;
    }
    if (SX < 0) {
        Rect->Width += SX;
        SX = 0;
    }
    if (SY < 0) {
        Rect->Height += SY;
        SY = 0;
    }
    if ((SX + Rect->Width) > Source->Width) {
        Rect->Width = (Source->Width - SX);
    }
    if ((SY + Rect->Height) > Source->Height) {
        Rect->Height = (Source->Height - SY);
    }
    if (Rect->right() > Dest->ClipRectangle.right()) {
        Rect->Width -= (Rect->right() - Dest->ClipRectangle.right());
    }
    if (Rect->bottom() > Dest->ClipRectangle.bottom()) {
        Rect->Height -= (Rect->bottom() - Dest->ClipRectangle.bottom());
    }
    return !Rect->empty();
}

Export int Clip2D_PairedRect(Rectangle *Rect, Rectangle *RectS, Image *Dest, Image *Source, Rectangle *DestRect, Rectangle *SourceRect, int *CropOutput) {
    if (!Rect) return Failure;
    if (!RectS) return Failure;
    if (!Dest) return Failure;
    if (!Source) return Failure;
    if (DestRect) {
        *Rect = *DestRect;
    } else {
        Rect->Left = Rect->Top = 0;
        Rect->Width = Dest->Width;
        Rect->Height = Dest->Height;
    }
    if (SourceRect) {
        *RectS = *SourceRect;
    } else {
        RectS->Left = RectS->Top = 0;
        RectS->Width = Source->Width;
        RectS->Height = Source->Height;
    }    
    if (Rect->Top > Dest->ClipRectangle.bottom()) return Failure;
    if (Rect->Left > Dest->ClipRectangle.right()) return Failure;
    if (Rect->bottom() < Dest->ClipRectangle.Top) return Failure;
    if (Rect->right() < Dest->ClipRectangle.Left) return Failure;
    if (Dest->ClipRectangle.Width == 0) return Failure;
    if (Dest->ClipRectangle.Height == 0) return Failure;
    if (!enableClipping) return !Rect->empty();
    if (Rect->Left < Dest->ClipRectangle.Left) {
        if (CropOutput) CropOutput[0] = (Dest->ClipRectangle.Left - Rect->Left); 
        Rect->Width -= (Dest->ClipRectangle.Left - Rect->Left);
        Rect->Left = Dest->ClipRectangle.Left;
    }
    if (Rect->Top < Dest->ClipRectangle.Top) {
        if (CropOutput) CropOutput[1] = (Dest->ClipRectangle.Top - Rect->Top); 
        Rect->Height -= (Dest->ClipRectangle.Top - Rect->Top);
        Rect->Top = Dest->ClipRectangle.Top;
    }
    if (RectS->Left < 0) {
        RectS->Width += RectS->Left;
        RectS->Left = 0;
    }
    if (RectS->Top < 0) {
        RectS->Height += RectS->Top;
        RectS->Top = 0;
    }
    if ((RectS->Left + RectS->Width) > Source->Width) {
        RectS->Width = (Source->Width - RectS->Left);
    }
    if ((RectS->Top + RectS->Height) > Source->Height) {
        RectS->Height = (Source->Height - RectS->Top);
    }
    if (Rect->right() > Dest->ClipRectangle.right()) {
        if (CropOutput) CropOutput[2] = (Rect->right() - Dest->ClipRectangle.right()); 
        Rect->Width -= (Rect->right() - Dest->ClipRectangle.right());
    }
    if (Rect->bottom() > Dest->ClipRectangle.bottom()) {
        if (CropOutput) CropOutput[3] = (Rect->bottom() - Dest->ClipRectangle.bottom()); 
        Rect->Height -= (Rect->bottom() - Dest->ClipRectangle.bottom());
    }
    return (!Rect->empty()) && (!RectS->empty());
}

Export int Clip2D_SimpleRectWrap(Rectangle *Rect, Image *Dest, Image *Source, Rectangle *DestRect) {
    if (!Rect) return Failure;
    if (!Dest) return Failure;
    if (!Source) return Failure;
    if (DestRect) {
        *Rect = *DestRect;
    } else {
        Rect->Left = Rect->Top = 0;
        Rect->Width = Source->Width;
        Rect->Height = Source->Height;
    }
    if (!enableClipping) return !Rect->empty();
    if (Rect->Left < Dest->ClipRectangle.Left) {
        Rect->Width -= (Dest->ClipRectangle.Left - Rect->Left);
        Rect->Left = Dest->ClipRectangle.Left;
    }
    if (Rect->Top < Dest->ClipRectangle.Top) {
        Rect->Height -= (Dest->ClipRectangle.Top - Rect->Top);
        Rect->Top = Dest->ClipRectangle.Top;
    }
    if (Rect->right() > Dest->ClipRectangle.right()) {
        Rect->Width -= (Rect->right() - Dest->ClipRectangle.right());
    }
    if (Rect->bottom() > Dest->ClipRectangle.bottom()) {
        Rect->Height -= (Rect->bottom() - Dest->ClipRectangle.bottom());
    }
    return !Rect->empty();
}

Export int Clip2D_PairToRect(Rectangle *Dest, Rectangle *Source, Rectangle *Clip) {
    if (!Dest) return Failure;
    if (!Source) return Failure;
    if (!Clip) return Failure;
    return !Dest->empty();
}

Export int ClipRectangle_Image(Rectangle *Rect, Image *Image) {
    if (!Rect) return Failure;
    if (!Image) return Failure;
//    if (!enableClipping) return !Rect->empty();
    Rect->normalize();
    if (Rect->Left < 0) {
        Rect->Width += Rect->Left;
        Rect->Left = 0;
    }
    if (Rect->Top < 0) {
        Rect->Height += Rect->Top;
        Rect->Top = 0;
    }
    if (Rect->right() >= Image->Width) {
        Rect->Width -= (Rect->right() - Image->Width);
    }
    if (Rect->bottom() >= Image->Height) {
        Rect->Height -= (Rect->bottom() - Image->Height);
    }
    return !Rect->empty();
}

Export int ClipRectangle_Rect(Rectangle *Rect, Rectangle *Clip) {
    if (!Rect) return Failure;
    if (!Clip) return Failure;
//    if (!enableClipping) return !Rect->empty();
    Rect->normalize();
    if (Rect->Left < Clip->Left) {
          Rect->Width -= Clip->Left - Rect->Left;
          Rect->Left = 0;
    }
    if (Rect->Top < Clip->Top) {
          Rect->Height -= Clip->Top - Rect->Top;
          Rect->Top = Clip->Top;
    }
    if (Rect->right() > Clip->right()) {
      Rect->Width -= (Rect->right() - Clip->right());
    }
    if (Rect->bottom() > Clip->bottom()) {
      Rect->Height -= (Rect->bottom() - Clip->bottom());
    }
    return !Rect->empty();
}

Export int ClipRectangle_ImageClipRect(Rectangle *Rect, Image *Image) {
    if (!Rect) return Failure;
    if (!Image) return Failure;
    if (!ClipRectangle_Image(Rect, Image)) return Failure;
//    if (!enableClipping) return !Rect->empty();
    Rect->normalize();
    if (Image->ClipRectangle.Width == 0) {
      Rect->Width = 0;
    } else {
      if (Rect->Left < Image->ClipRectangle.Left) {
          Rect->Width -= Image->ClipRectangle.Left - Rect->Left;
          Rect->Left = Image->ClipRectangle.Left;
      }
      if (Rect->right() >= Image->ClipRectangle.right()) {
          Rect->Width -= (Rect->right() - Image->ClipRectangle.right());
      }
    }
    if (Image->ClipRectangle.Height == 0) {
      Rect->Height = 0;
    } else {
      if (Rect->Top < Image->ClipRectangle.Top) {
          Rect->Height -= Image->ClipRectangle.Top - Rect->Top;
          Rect->Top = Image->ClipRectangle.Top;
      }
      if (Rect->bottom() >= Image->ClipRectangle.bottom()) {
          Rect->Height -= (Rect->bottom() - Image->ClipRectangle.bottom());
      }
    }
    return !Rect->empty();
}

int ClipRectangle_ImageClipRect(Rectangle *Rect, Image *Image, int *CropOutput) {
    if (!Rect) return Failure;
    if (!Image) return Failure;
//    if (!enableClipping) return !Rect->empty();
    Rect->normalize();
    if (Image->ClipRectangle.Width == 0) {
      Rect->Width = 0;
    } else {
      if (Rect->Left < Image->ClipRectangle.Left) {
          if (CropOutput) CropOutput[0] = Image->ClipRectangle.Left - Rect->Left; 
          Rect->Width -= Image->ClipRectangle.Left - Rect->Left;
          Rect->Left = Image->ClipRectangle.Left;
      }
      if (Rect->right() >= Image->ClipRectangle.right()) {
          if (CropOutput) CropOutput[2] = (Rect->right() - Image->ClipRectangle.right()); 
          Rect->Width -= (Rect->right() - Image->ClipRectangle.right());
      }
    }
    if (Image->ClipRectangle.Height == 0) {
      Rect->Height = 0;
    } else {
      if (Rect->Top < Image->ClipRectangle.Top) {
          if (CropOutput) CropOutput[1] = Image->ClipRectangle.Top - Rect->Top; 
          Rect->Height -= Image->ClipRectangle.Top - Rect->Top;
          Rect->Top = Image->ClipRectangle.Top;
      }
      if (Rect->bottom() >= Image->ClipRectangle.bottom()) {
          if (CropOutput) CropOutput[3] = (Rect->bottom() - Image->ClipRectangle.bottom()); 
          Rect->Height -= (Rect->bottom() - Image->ClipRectangle.bottom());
      }
    }
    return !Rect->empty();
}