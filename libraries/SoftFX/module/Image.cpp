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
#include "../header/Blitters.hpp"
#include "../header/Filters.hpp"
#include "../header/Resample.hpp"
#include "../header/Blend.hpp"

LockingModes lockingMode = UserLocking;

void ResampleImage_Linear(Image* Source, Image* Target) {
  if (Override::EnumOverrides(Override::ResampleImage_Linear, 2, Source, Target)) return;
  ImageLockManager ilDest(lockingMode, Target);                 
  ImageLockManager ilSource(lockingMode, Source);             
  if (!ilDest.performUnlock()) {                              
      return;                                         
  }                                                           
  if (!ilSource.performUnlock()) {                            
      return;                                         
  }                                                           

  float x_multiplier = ((float)Source->Width / (float)Target->Width);
  float y_multiplier = ((float)Source->Height / (float)Target->Height);

  int *x_table = AllocateArray(int, Target->Width);
  int *y_table = AllocateArray(int, Target->Height);
  for (int y = 0; y < Target->Height; y++) {
    y_table[y] = y * y_multiplier;
  }
  for (int x = 0; x < Target->Width; x++) {
    x_table[x] = x * x_multiplier;
  }

  Pixel *target = Target->pointer(0, 0);

  for (int y = 0; y < Target->Height; y++) {
    for (int x = 0; x < Target->Width; x++) {
      *target = Source->getPixel(x_table[x], y_table[y]);
      target++;
    }
  }

  DeleteArray(x_table);
  DeleteArray(y_table);

  return;
}

void ResampleImage_Bilinear(Image* Source, Image* Target) {
  if (Override::EnumOverrides(Override::ResampleImage_BiLinear, 2, Source, Target)) return;
  ImageLockManager ilDest(lockingMode, Target);                 
  ImageLockManager ilSource(lockingMode, Source);             
  if (!ilDest.performUnlock()) {                              
      return;                                         
  }                                                           
  if (!ilSource.performUnlock()) {                            
      return;                                         
  }                                                           

  float x_multiplier = ((float)Source->Width / (float)Target->Width);
  float y_multiplier = ((float)Source->Height / (float)Target->Height);

  float *x_table = AllocateArray(float, Target->Width);
  float *y_table = AllocateArray(float, Target->Height);
  for (int y = 0; y < Target->Height; y++) {
    y_table[y] = y * y_multiplier;
  }
  for (int x = 0; x < Target->Width; x++) {
    x_table[x] = x * x_multiplier;
  }

  Pixel *target = Target->pointer(0, 0);

  for (int y = 0; y < Target->Height; y++) {
    for (int x = 0; x < Target->Width; x++) {
      *target = Source->getPixelAA(x_table[x], y_table[y]);
      target++;
    }
  }

  DeleteArray(x_table);
  DeleteArray(y_table);

  return;
}

void ResampleImage_Half(Image* Source, Image* Target) {
  if (((Source->Width % 2) != 0) || ((Source->Height % 2) != 0)) {
    ResampleImage_Bilinear(Source, Target);
    return;
  }

  if (Override::EnumOverrides(Override::ResampleImage_Half, 2, Source, Target)) return;
  ImageLockManager ilDest(lockingMode, Target);                 
  ImageLockManager ilSource(lockingMode, Source);             
  if (!ilDest.performUnlock()) {                              
      return;                                         
  }                                                           
  if (!ilSource.performUnlock()) {                            
      return;                                         
  }                                                           

  Pixel *target = Target->pointer(0, 0);
  Pixel *source[2] = {Source->pointer(0, 0), Source->pointer(0, 1)};

  int r, g, b, a;

  for (int y = 0; y < Target->Height; y++) {
    for (int x = 0; x < Target->Width; x++) {
      r = g = b = a = 0;
      
      b += (*source[0])[::Blue]; g += (*source[0])[::Green]; r += (*source[0])[::Red]; a += (*source[0])[::Alpha];
      source[0]++;

      b += (*source[1])[::Blue]; g += (*source[1])[::Green]; r += (*source[1])[::Red]; a += (*source[1])[::Alpha];
      source[1]++;

      b += (*source[0])[::Blue]; g += (*source[0])[::Green]; r += (*source[0])[::Red]; a += (*source[0])[::Alpha];
      source[0]++;

      b += (*source[1])[::Blue]; g += (*source[1])[::Green]; r += (*source[1])[::Red]; a += (*source[1])[::Alpha];
      source[1]++;

      (*target)[::Blue] = (b / 4);
      (*target)[::Green] = (g / 4);
      (*target)[::Red] = (r / 4);
      (*target)[::Alpha] = (a / 4);
      target++;
    }
    source[0] += Source->Width;
    source[1] += Source->Width;
  }

  return;
}

void ResampleImage_Double(Image* Source, Image* Target) {
  if (Override::EnumOverrides(Override::ResampleImage_Double, 2, Source, Target)) return;                                                              
  ImageLockManager ilDest(lockingMode, Target);                 
  ImageLockManager ilSource(lockingMode, Source);             
  if (!ilDest.performUnlock()) {                              
      return;                                         
  }                                                           
  if (!ilSource.performUnlock()) {                            
      return;                                         
  }                                                           

  Pixel *target[2] = {Target->pointer(0, 0), Target->pointer(0, 1)};
  Pixel source[4];

  for (int y = 0; y < Target->Height; y+=2) {
    for (int x = 0; x < Target->Width; x+=2) {
      Source->getPixels(x / 2, y / 2, 2, 2, source);

      *target[0] = source[0];
      target[0]++;

      (*target[0])[::Blue] = (source[0][::Blue] + source[1][::Blue]) / 2;
      (*target[0])[::Green] = (source[0][::Green] + source[1][::Green]) / 2;
      (*target[0])[::Red] = (source[0][::Red] + source[1][::Red]) / 2;
      (*target[0])[::Alpha] = (source[0][::Alpha] + source[1][::Alpha]) / 2;
      target[0]++;

      (*target[1])[::Blue] = (source[0][::Blue] + source[2][::Blue]) / 2;
      (*target[1])[::Green] = (source[0][::Green] + source[2][::Green]) / 2;
      (*target[1])[::Red] = (source[0][::Red] + source[2][::Red]) / 2;
      (*target[1])[::Alpha] = (source[0][::Alpha] + source[2][::Alpha]) / 2;
      target[1]++;

      (*target[1])[::Blue] = (source[0][::Blue] + source[1][::Blue] + source[2][::Blue] + source[3][::Blue]) / 4;
      (*target[1])[::Green] = (source[0][::Green] + source[1][::Green] + source[2][::Green] + source[3][::Green]) / 4;
      (*target[1])[::Red] = (source[0][::Red] + source[1][::Red] + source[2][::Red] + source[3][::Red]) / 4;
      (*target[1])[::Alpha] = (source[0][::Alpha] + source[1][::Alpha] + source[2][::Alpha] + source[3][::Alpha]) / 4;
      target[1]++;

    }
    target[0] += Target->Width;
    target[1] += Target->Width;
  }

  return;
}

void Image::resample(Image* Target, int Width, int Height, ResampleModes ResampleMode) {
  if (!Target) return;
  if ((this->Width < 1) || (this->Height < 1)) return;
  if ((Width < 1) || (Height < 1)) return;
  if (Override::EnumOverrides(Override::Resample, 5, this, Target, Width, Height, ResampleMode)) return;
  
  Target->resize(Width, Height);
  switch(ResampleMode) {
  case -1:
  case 0:
    break;
  case ResampleMode_Linear:
  case ResampleMode_Linear_Wrap:
  case ResampleMode_Linear_Clamp:
    ::ResampleImage_Linear(this, Target);
    break;
  case ResampleMode_Bilinear:
  case ResampleMode_Bilinear_Wrap:
  case ResampleMode_Bilinear_Clamp:
  case ResampleMode_Bilinear_High_Quality:
    if ((Width == (this->Width / 2)) && (Height == (this->Height / 2))) {
      ::ResampleImage_Half(this, Target);
    } else if ((Width == (this->Width * 2)) && (Height == (this->Height * 2))) {
      ::ResampleImage_Double(this, Target);
    } else {
      ::ResampleImage_Bilinear(this, Target);
    } 
    break;
  }
  return;
}

void Image::allocate(Size Width, Size Height) {
    
    this->deallocate();

    this->dirty();
    this->Pitch = 0;

    if (Override::EnumOverrides(Override::Allocate, 3, this, Width, Height)) {
    } else {
      if ((Width > 0) && (Height > 0)) {
          this->Data = (Pixel*)vm_alloc(Width * Height * sizeof(Pixel));
          this->DataSize = Width * Height * sizeof(Pixel);
      }

      if (this->Data) {
          this->Width = Width;
          this->Height = Height;
      }
    
      this->unlock();
    }

    this->setClipRectangle(this->getRectangle());
    this->clear();

    return;
}

void Image::getPixels(int X, int Y, int W, int H, Pixel* Target) {
int sy = 0, xr = 0;
Pixel* source;

  if (Override::EnumOverrides(Override::GetPixels, 6, this, X, Y, W, H, Target)) return;

  IfLocked(return);

  for (int cy = 0; cy < H; cy++) {
    sy = ClipValue(Y + cy, Height - 1);
    source = this->fast_pointer(X, sy);
    xr = this->Width - X - 1;
    for (int cx = 0; cx < W; cx++) {
      *Target = *source;
      if (xr) {
        xr--;
        source++;
      }
      Target++;
    }
  }

  return;
}

void Image::getPixelsClip(int X, int Y, int W, int H, Pixel* Target) {
  return;
}

void Image::allocateDIB(Size Width, Size Height, int DC) {
BitmapInfo bmi;
    
  this->deallocate();

  _Fill<Byte>(&(bmi), 0, sizeof(BitmapInfo));

  this->DIBDC = DC;

  bmi.Header.Size = sizeof(BitmapInfoHeader);
  bmi.Header.Width = Width;
  bmi.Header.Height = -Height;
  bmi.Header.BitCount = 32;
  bmi.Header.Planes = 1;
  bmi.Header.Compression = 0;
  
  if ((Width > 0) && (Height > 0)) {
    this->DIBSection = _CreateDIBSection(DC, &bmi, 0, reinterpret_cast<void**>(&(this->Data)), Null, 0);
  }

  if (this->Data) {
    this->Width = Width;
    this->Height = Height;
  }

  this->dirty();
  this->Pitch = 0;

  this->setClipRectangle(this->getRectangle());
  
  this->unlock();
  this->clear();

  return;
}

void Image::optimize() {
  bool transparentOnly = true, opaqueOnly = true, maskOnly = true, noMask = true, sameAlpha = true, grayscaleOnly = true, solidColor = true;
  DoubleWord fa = 0;
  DoubleWord fv = 0;
  if (!this) return;
  IfLocked(return);
  int i = 0, iMax = (this->Width * this->Height);
  Byte a;
  Pixel *pCurrent = this->pointer(0,0);
  if (!pCurrent) return;
  fa = (*pCurrent)[::Alpha];
  fv = pCurrent->V;
  this->dirty(); // default
  for (i = 0; i < iMax; i++) {
    a = ((*pCurrent)[::Alpha]);
    if ((a == 255) || (a == 0)) {
      if (a == 0) {
        opaqueOnly = false;
      } else {
        transparentOnly = false;
      }
    } else {
      opaqueOnly = false;
      maskOnly = false;
      transparentOnly = false;
    }
    if (pCurrent->V == this->MatteColor.V) {
      noMask = false;
    }
    if (fa != a) {
      sameAlpha = false;
    }
    if (fv != pCurrent->V) {
      solidColor = false;
    }
    if (grayscaleOnly) {
      if (((*pCurrent)[::Red] == (*pCurrent)[::Green]) && ((*pCurrent)[::Green] == (*pCurrent)[::Blue])) {
      } else {
          grayscaleOnly = false;
      }
    }
    pCurrent++;
  }
  this->OptimizeData.transparentOnly = transparentOnly;
  this->OptimizeData.opaqueOnly = opaqueOnly;
  this->OptimizeData.maskOnly = maskOnly;
  this->OptimizeData.grayscaleOnly = grayscaleOnly;
  this->OptimizeData.sameAlpha = sameAlpha;
  this->OptimizeData.noMask = noMask;
  this->OptimizeData.solidColor = solidColor;
}

void Image::resize(Size Width, Size Height) {
  Image* OldImage = new Image();
  IfLocked(return);
    OldImage->steal(this);

    if ((Width > 0) && (Height > 0)) {
        this->reallocate(Width, Height);
    }

    this->dirty();
    this->Pitch = 0;

    this->setClipRectangle(this->getRectangle());

    if (OldImage) {
      this->clear();
      Rectangle oldRect = OldImage->getRectangle();
      BlitSimple_Normal(this, OldImage, &oldRect, 0, 0);
      delete OldImage;
    }
    
    return;
}

void Image::slide(Coordinate X, Coordinate Y) {
  Image* OldImage = new Image();
  IfLocked(return);
    OldImage->steal(this);

    if ((OldImage->Width > 0) && (OldImage->Height > 0)) {
        this->reallocate(OldImage->Width, OldImage->Height);
    }

    this->dirty();
    this->Pitch = 0;

    this->setClipRectangle(this->getRectangle());

    this->clear();
    Rectangle oldRect = OldImage->getRectangle();
    oldRect.translate(X, Y);
    BlitSimple_Normal(this, OldImage, &oldRect, 0, 0);

    delete OldImage;
    
    return;
}

void Image::reallocate(Size Width, Size Height) {
  this->deallocate();
  if (this->DIBSection) {
    this->allocateDIB(Width, Height, DIBDC);
  } else {
    this->allocate(Width, Height);
  }
}

void Image::deallocate() {

  if (this->Unlocked) {
    this->lock();
  }

  if (Override::EnumOverrides(Override::Deallocate, 1, this)) return;

  if (this->Data == Null) return;

  if(this->CoronaImage != 0) {

    delete this->CoronaImage;
    this->CoronaImage = Null;
    this->Data = Null;
    this->DIBSection = 0;

  } else if ((this->DIBSection != 0) && (this->Data != 0)) {

    _DeleteObject(this->DIBSection);
    this->Data = Null;

  } else if ((this->DataSize > 0) && (this->Data != 0)) {

    vm_dealloc(this->Data);
    this->CoronaImage = Null;
    this->DIBSection = 0;
    this->Data = Null;
    this->DataSize = 0;

  }

  this->Data = Null;
  this->DataSize = 0;
  this->dirty();
  this->Width = this->Height = 0;
  this->Pitch = 0;
  this->ClipRectangle = Rectangle(0,0,0,0);
  
  this->Unlocked = false;

  return;
}

void Image::setClipRectangle(Rectangle *NewRectangle) {
Rectangle rClip;
    
    rClip = *NewRectangle;

    if (ClipRectangle_Image(&rClip, this)) {
        this->ClipRectangle = rClip;
    } else {
        this->ClipRectangle = this->getRectangle();
    }

    return;
}

void Image::setClipRectangle(Rectangle NewRectangle) {

    if (ClipRectangle_Image(&NewRectangle, this)) {
        this->ClipRectangle = NewRectangle;
    } else {
        this->ClipRectangle = this->getRectangle();
    }

    return;
}

void Image::clear() {

    if (Override::EnumOverrides(Override::Clear, 1, this)) return;
    IfLocked(return);

    _Fill<Pixel>(this->Data, Pixel(0), this->Width * this->Height);
    this->dirty();

    return;
}

void Image::fill(Pixel Value) {

    if (Override::EnumOverrides(Override::Fill, 2, this, Value)) return;
    IfLocked(return);

    _Fill<Pixel>(this->Data, Value, this->Width * this->Height);
    this->dirty();

    return;
}

void Image::fill(Pixel Value, Rectangle *Rectangle) {

    FilterSimple_Fill(this, Rectangle, Value);

    return;
}

int Image::clipRectangle(Rectangle *Rectangle) {

    if (Rectangle) return ClipRectangle_ImageClipRect(Rectangle, this);

    return false;
}

int Image::clipRectangle(Rectangle *Rectangle, int *CropOffsets) {

    if (Rectangle) return ClipRectangle_ImageClipRect(Rectangle, this, CropOffsets);

    return false;
}

Rectangle Image::getRectangle() {
    Rectangle r;
    r.Left = r.Top = 0;
    r.Width = this->Width;
    r.Height = this->Height;
    return r;
}

void Image::copy(Image *Source) {
  if (Source) {
    if (Override::EnumOverrides(Override::Copy, 2, this, Source)) return;
    ImageLockManager ilSource(lockingMode, Source);
    if (!ilSource.performUnlock()) return;
    if ((this->Width != Source->Width) || (this->Height != Source->Height)) {
      this->reallocate(Source->Width, Source->Height);
    }
    IfLocked(return);
    for (int i = 0; i < this->Height; i++) {
      _Copy<Byte>(this->fast_pointer(0, i), Source->pointer(0, i), this->Width * 4);
    }
    this->optimize();
  }
}

void Image::copy(Image* Source, int X, int Y, int W, int H) {
  if (Source) {
    if (Override::EnumOverrides(Override::CopyEx, 6, this, Source, X, Y, W, H)) return;
    ImageLockManager ilSource = ImageLockManager(lockingMode, Source);
    if (!ilSource.performUnlock()) return;
    if ((this->Width != W) || (this->Height != H)) {
      this->reallocate(W, H);
    }
    IfLocked(return);
    if (X < 0) X = 0;
    if (Y < 0) Y = 0;
    if ((W + X) >= (Source->Width)) {
      W -= (W + X) - Source->Width;
    }
    if ((H + Y) >= (Source->Height)) {
      H -= (H + Y) - Source->Height;
    }
    for (int i = 0; i < this->Height; i++) {
      _Copy<Byte>(this->fast_pointer(0, i), Source->pointer(X, Y + i), W * 4);
    }
    this->optimize();
  }
}

void Image::rotate(float Angle) {
  this->rotate(Angle, SampleRow_Bilinear_Rolloff);
  return;
}

// ruthlessly stolen from Sphere

#define RotateX(x, y, cr, sr) ((x * cr) - (y * sr))
#define RotateY(x, y, cr, sr) ((x * sr) + (y * cr))

void Image::rotate(float Angle, ScalerFunction *Scaler) {
  if (Override::EnumOverrides(Override::Rotate, 3, this, Angle, Scaler)) return;
  IfLocked(return);

  float ix[2], iy[2];
  float tx[2], ty;
  float xd, yd;
  int width, height;
  float xOff, yOff;
  double aCos, aSin, radians;
  
  radians = -Angle * ((22.0 / 7.0) / 180.0);
  aCos = cos(radians);
  aSin = sin(radians);

  {
    width = this->Width;
    height = this->Height;
    float x[4], y[4];
    float xNOff, yNOff;

    xOff = xNOff = 0;
    yOff = yNOff = 0;
    x[0] = RotateX(0, 0, aCos, aSin);
    y[0] = RotateY(0, 0, aCos, aSin);
    x[1] = RotateX(width, height, aCos, aSin);
    y[1] = RotateY(width, height, aCos, aSin);
    x[2] = RotateX(width, 0, aCos, aSin);
    y[2] = RotateY(width, 0, aCos, aSin);
    x[3] = RotateX(0, height, aCos, aSin);
    y[3] = RotateY(0, height, aCos, aSin);
    xOff = std::max(xOff, x[0]); xNOff = std::min(xNOff, x[0]);
    xOff = std::max(xOff, x[1]); xNOff = std::min(xNOff, x[1]);
    xOff = std::max(xOff, x[2]); xNOff = std::min(xNOff, x[2]);
    xOff = std::max(xOff, x[3]); xNOff = std::min(xNOff, x[3]);
    yOff = std::max(yOff, y[0]); yNOff = std::min(yNOff, y[0]);
    yOff = std::max(yOff, y[1]); yNOff = std::min(yNOff, y[1]);
    yOff = std::max(yOff, y[2]); yNOff = std::min(yNOff, y[2]);
    yOff = std::max(yOff, y[3]); yNOff = std::min(yNOff, y[3]);
    
    xOff = (xOff - xNOff) - (width);
    yOff = (yOff - yNOff) - (height);

    width  = width + (int)xOff;
    height = height + (int)yOff;
    xOff /= 2;
    yOff /= 2;
  }

  if (Angle >= 90) xOff -= 1;
  if (Angle >= 180) {
    yOff -= 1;
  }
  if (Angle >= 270) {
  }

  if ((width <= 0) || (height <= 0)) return;

Image* OldImage = new Image();
  OldImage->steal(this);

  if ((OldImage->Width > 0) && (OldImage->Height > 0)) {
    this->resize(width, height);
  } else {
    delete OldImage;
    return;
  }

  for (int y=0; y<height; y++) {
    // realigns the rotating axis to 0,0 (of a graphical point of view)
    tx[0] = ((0)       - (OldImage->Width/2) - xOff);
    tx[1] = ((width-1) - (OldImage->Width/2) - xOff);
    ty = (y - (OldImage->Height/2) - yOff);
    
    ix[0] = RotateX(tx[0], ty, aCos, aSin);
    ix[1] = RotateX(tx[1], ty, aCos, aSin);
    iy[0] = RotateY(tx[0], ty, aCos, aSin);
    iy[1] = RotateY(tx[1], ty, aCos, aSin);

    ix[0] += (OldImage->Width/2);
    ix[1] += (OldImage->Width/2);
    iy[0] += (OldImage->Height/2);
    iy[1] += (OldImage->Height/2);

    xd = (ix[1] - ix[0]) / width;
    yd = (iy[1] - iy[0]) / width;

    Scaler(OldImage, ix[0], iy[0], 
    (int)(ix[0] * 65536.0) % 65536, (int)(iy[0] * 65536.0) % 65536, 
    xd, yd,
    (int)(xd * 65536.0) % 65536, (int)(yd * 65536.0) % 65536,
    width, this->pointer(0, y));
  }

  delete OldImage;

  return;
}

bool Image::setPixelAA(int xi, int yi, Byte xw, Byte yw, Pixel Color) {
    if (xi < -1) return false;
    if (yi < -1) return false;
    if (xi >= Width) return false;
    if (yi >= Height) return false;
    if (Override::EnumOverrides(Override::SetPixelAA, 6, this, xi, yi, (int)xw, (int)yw, Color.V)) return true;
    IfLocked(return false)
    Byte w[4];
    {
      w[1] = AlphaLookup(AlphaLookup(xw, yw ^ 0xFF), Color[::Alpha]);
      w[2] = AlphaLookup(AlphaLookup(xw ^ 0xFF, yw), Color[::Alpha]);
      w[3] = AlphaLookup(AlphaLookup(xw, yw), Color[::Alpha]);
      w[0] = Color[::Alpha] - (w[1] + w[2] + w[3]);
    }
    Pixel *pColor = &Color;
    Pixel *p[4];
    {
      p[0] = this->pointer(xi,yi);
      p[1] = this->pointer(xi+1,yi);
      p[2] = this->pointer(xi,yi+1);
      p[3] = this->pointer(xi+1,yi+1);
    }
    AlphaLevel *LevelDest, *LevelSource;
    if ((p[0]) && (w[0])) {
      LevelDest = AlphaLevelLookup(w[0] ^ 0xFF);
      LevelSource = AlphaLevelLookup(w[0]);
      BLENDPIXEL_ALPHA_OPACITY(p[0], p[0], pColor, LevelDest, LevelSource);
    }
    if ((p[1]) && (w[1])) {
      LevelDest = AlphaLevelLookup(w[1] ^ 0xFF);
      LevelSource = AlphaLevelLookup(w[1]);
      BLENDPIXEL_ALPHA_OPACITY(p[1], p[1], pColor, LevelDest, LevelSource);
    }
    if ((p[2]) && (w[2])) {
      LevelDest = AlphaLevelLookup(w[2] ^ 0xFF);
      LevelSource = AlphaLevelLookup(w[2]);
      BLENDPIXEL_ALPHA_OPACITY(p[2], p[2], pColor, LevelDest, LevelSource);
    }
    if ((p[3]) && (w[3])) {
      LevelDest = AlphaLevelLookup(w[3] ^ 0xFF);
      LevelSource = AlphaLevelLookup(w[3]);
      BLENDPIXEL_ALPHA_OPACITY(p[3], p[3], pColor, LevelDest, LevelSource);
    }
    return false;
}

void Image::setBrushPattern(Image *Pattern) {
  
  if (!Pattern) return;

  this->BrushPattern = Pattern;
}

Image* Image::getBrushPattern() {

  return this->BrushPattern;
}

bool Image::lock() {
  if (Override::EnumOverrides(Override::Lock, 1, this)) return (this->Unlocked == false);
  this->Unlocked = false;
  return true;
}

bool Image::unlock() {
  if (Override::EnumOverrides(Override::Unlock, 1, this)) return (this->Unlocked == true);
  if (this->Data != Null) {
    this->Unlocked = true;
    return true;
  } else {
    this->Unlocked = false;
    return false;
  }
}

ImageLockManager::~ImageLockManager() {
  if (lockOnDestroy) {
    if (image) {
      image->lock();
    }
  }
}

bool ImageLockManager::performUnlock() {
  if (this->image) {
    switch(this->mode) {
      case AutoUnlock:
          if (image->Unlocked) {
            return true;
          } else {
            image->unlock();
            return image->Unlocked;
          }
        break;
      case AutoUnlockLockWhenDone:
          if (image->Unlocked) {
            return true;
          } else {
            image->unlock();
            this->lockOnDestroy = true;
            return image->Unlocked;
          }
        break;
      default:
          return image->Unlocked;
        break;
    }
  } else {
    return false;
  }
}