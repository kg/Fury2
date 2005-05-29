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

enum ResampleModes {
  ResampleMode_Default = -1,
  ResampleMode_None = 0,
  ResampleMode_Linear = 1,
  ResampleMode_Bilinear = 2,
  ResampleMode_Bilinear_High_Quality = 3,
  ResampleMode_Linear_Wrap = 4,
  ResampleMode_Bilinear_Wrap = 5,
  ResampleMode_Linear_Clamp = 6,
  ResampleMode_Bilinear_Clamp = 7
};

struct BitmapInfoHeader{
    int Size, Width, Height;
    short Planes, BitCount;
    int Compression, SizeImage, XPixelsPerMeter, YPixelsPerMeter, ColorUsed, ColorImportant;
};

struct BitmapInfo {
    BitmapInfoHeader Header;
    int Reserved;
};

struct OptimizationFlags {
    bool opaqueOnly : 1;
    bool transparentOnly : 1;
    bool maskOnly : 1;
    bool noMask : 1;
    bool sameAlpha : 1;
    bool grayscaleOnly : 1;
    bool solidColor : 1;

    OptimizationFlags() {
      this->reset();
    }

    inline void reset() {
      this->opaqueOnly = false;
      this->transparentOnly = false;
      this->maskOnly = false;
      this->noMask = false;
      this->sameAlpha = false;
      this->grayscaleOnly = false;
      this->solidColor = false;
    }
};

enum LockingModes {
  UserLocking = 0,
  AutoUnlock = 1,
  AutoUnlockLockWhenDone = 2
};

extern LockingModes lockingMode;

class ImageLockManager {
  public:
    LockingModes mode;
    Image *image;
    bool lockOnDestroy;

    ImageLockManager() {
      this->mode = lockingMode;
      this->image = Null;
      this->lockOnDestroy = false;
    }

    ImageLockManager(LockingModes LockingMode, Image* Image) {
      this->mode = LockingMode;
      this->image = Image;
      this->lockOnDestroy = false;
    }

    ~ImageLockManager();

    bool performUnlock();
};

class Image {
public:
  zeroinit<Size> Width, Height;
  zeroinit<int> Pitch;
  zeroinit<Pixel*> Data;
  zeroinit<DoubleWord> DataSize;
  Rectangle ClipRectangle;
  zeroinit<corona::Image*> CoronaImage;
  zeroinit<int> DIBSection, DIBDC;
  OptimizationFlags OptimizeData;
  Pixel MatteColor;
  zeroinit<Image*> BrushPattern;
  autoinit<bool, true> Unlocked;
  autoinit<bool, true> Dirty;

  static const int TagCount = 8;
  DoubleWord Tags[TagCount];

  Image() {
    AddToHeap(this);
    _Fill<DoubleWord>(this->Tags, 0, TagCount);
  }

  Image(Size Width, Size Height) {
    AddToHeap(this);
    _Fill<DoubleWord>(this->Tags, 0, TagCount);
    this->allocate(Width, Height);
  }

  Image(Size Width, Size Height, bool DIB, int DC) {
    AddToHeap(this);
    _Fill<DoubleWord>(this->Tags, 0, TagCount);
    if (DIB) {
      this->allocateDIB(Width, Height, DC);
    } else {
      this->allocate(Width, Height);
    }
  }

  Image(Pixel* Data, Size Width, Size Height, int Pitch) {
    AddToHeap(this);
    this->Data = Data;
    this->Width = Width;
    this->Height = Height;
    this->Unlocked = true;
    this->Pitch = Pitch;
    this->setClipRectangle(this->getRectangle());
    _Fill<DoubleWord>(this->Tags, 0, TagCount);
    this->optimize();
  }

  Image(Pixel* Data, Size Width, Size Height, int Pitch, int DIBSection) {
    AddToHeap(this);
    this->Data = Data;
    this->Width = Width;
    this->Height = Height;
    this->DIBSection = DIBSection;
    this->Unlocked = true;
    this->Pitch = Pitch;
    this->setClipRectangle(this->getRectangle());
    _Fill<DoubleWord>(this->Tags, 0, TagCount);
    this->optimize();
  }

  Image(Image *Source) {
    AddToHeap(this);
    this->MatteColor = Source->MatteColor;
    _Fill<DoubleWord>(this->Tags, 0, TagCount);
    this->copy(Source);
    this->Unlocked = (this->Data != Null);
    this->setClipRectangle(this->getRectangle());
  }

  Image(Image *Source, int x, int y, int w, int h) {
    AddToHeap(this);
    this->MatteColor = Source->MatteColor;
    _Fill<DoubleWord>(this->Tags, 0, TagCount);
    this->copy(Source, x, y, w, h);
    this->Unlocked = (this->Data != Null);
    this->setClipRectangle(this->getRectangle());
  }

  Image(corona::Image *Img) {
    AddToHeap(this);
    _Fill<DoubleWord>(this->Tags, 0, TagCount);
    if (Img) {
      if (Img->getFormat() != corona::PF_B8G8R8A8) {
          corona::Image *pImg = corona::CloneImage(Img, corona::PF_B8G8R8A8);
          if (pImg) {
              delete Img;
              Img = pImg;
          } else {
              return;
          }
      } 

      this->Width = Img->getWidth();
      this->Height = Img->getHeight();
      this->Data = static_cast<Pixel*>(Img->getPixels());
      this->CoronaImage = Img;
      this->Unlocked = true;
      this->setClipRectangle(this->getRectangle());
      this->optimize();
    }
  }

  ~Image() {
    this->deallocate();
    RemoveFromHeap(this);
  }

  inline bool initialized() {
    return (this->Width > 0) && (this->Height > 0);
  }

  inline void steal(Image *Source) {
    this->Data = Source->Data;
    this->DataSize = Source->DataSize;
    this->Width = Source->Width;
    this->Height = Source->Height;
    this->Pitch = Source->Pitch;
    this->DIBSection = Source->DIBSection;
    this->DIBDC = Source->DIBDC;
    this->CoronaImage = Source->CoronaImage;
    this->OptimizeData = Source->OptimizeData;
    this->Unlocked = Source->Unlocked;
    Source->Data = 0;
    Source->DataSize = 0;
    Source->Width = Source->Height = 0;
    Source->CoronaImage = 0;
    Source->Unlocked = false;
  }

  inline void dirty() {
    this->Dirty = true;
    this->OptimizeData.reset();
  }

  bool lock();
  bool unlock();

  void allocate(Size Width, Size Height);
  void allocateDIB(Size Width, Size Height, int DC);
  void reallocate(Size Width, Size Height);
  void resize(Size Width, Size Height);
  void slide(Coordinate X, Coordinate Y);
  void deallocate();

  void rotate(float Angle);
  void rotate(float Angle, ScalerFunction *Scaler);

  void resample(Image* Target, int Width, int Height, ResampleModes ResampleMode);

  void setBrushPattern(Image* Pattern);
  Image* getBrushPattern();

  void setClipRectangle(Rectangle NewRectangle);
  void setClipRectangle(Rectangle* NewRectangle);
  int clipRectangle(Rectangle *Rectangle);
  int clipRectangle(Rectangle *Rectangle, int *CropOffsets);

  void optimize();

  void clear();

  void copy(Image* Source);
  void copy(Image* Source, int X, int Y, int W, int H);

  void fill(Pixel Color);
  void fill(Pixel Color, Rectangle* Rectangle);

  Rectangle getRectangle();

  inline int getDIB() {
    return this->DIBSection;
  }

  inline Pixel* pointer() {
    return Data;
  }

  inline Pixel* fast_pointer(Coordinate X, Coordinate Y) {
    if (Data == Null) return Null;
    return ((Data) + ( (Y * (Width + Pitch)) + (X) ));
  }

  inline Pixel* pointer(Coordinate X, Coordinate Y) {
    if (Data == Null) return Null;
    if ((X < 0) || (Y < 0) || (X >= Width) || (Y >= Height)) return Null;
    return ((Data) + ( (Y * (Width + Pitch)) + (X) ));
  }

  void getPixels(int X, int Y, int W, int H, Pixel* Target);
  void getPixelsClip(int X, int Y, int W, int H, Pixel* Target);

  inline Pixel getPixelClip(Coordinate X, Coordinate Y) {
    if (X < 0) X = 0;
    if (Y < 0) Y = 0;
    if (X >= Width) X = Width - 1;
    if (Y >= Height) Y = Height - 1;
    Pixel pValue; if (Override::EnumOverrides(Override::GetPixel, 4, this, X, Y, &pValue)) return pValue;
    IfLocked(return 0)
    return *(this->fast_pointer(X, Y));
  }

  inline Pixel getPixelWrap(Coordinate X, Coordinate Y) {
    IfLocked(return 0)
    return *(this->fast_pointer(WrapValue(X, 0, this->Width - 1), WrapValue(Y, 0, this->Height - 1)));
  }

  inline Pixel getPixelClipRect(int X, int Y) {
    if (X < this->ClipRectangle.Left) X = this->ClipRectangle.Left;
    if (Y < this->ClipRectangle.Top) Y = this->ClipRectangle.Top;
    if (X >= this->ClipRectangle.right()) X = this->ClipRectangle.right_exclusive();
    if (Y >= this->ClipRectangle.bottom()) Y = this->ClipRectangle.bottom_exclusive();
    return this->getPixel(X, Y);
  }

  inline Pixel getPixelRolloff(int X, int Y) {
    bool c = false;
    if (X < 0) {
      X = 0;
      c = true;
    }
    if (Y < 0) {
      Y = 0;
      c = true;
    }
    if (X >= Width) {
      X = Width - 1;
      c = true;
    }
    if (Y >= Height) {
      Y = Height - 1;
      c = true;
    }
    if (c) {
      Pixel P = this->getPixel(X, Y);
      P[::Alpha] = 0;
      return P;
    } else {
      return this->getPixel(X, Y);
    }
  }

  inline Pixel getPixelAA(float X, float Y) {
    int xi = X * 255.0, yi = Y * 255.0;
    Byte xw = xi % 255, yw = yi % 255;
    xi /= 255;
    yi /= 255;
    return this->getPixelAA(xi, yi, xw, yw);
  }

  inline Pixel getPixelAA(int xi, int yi, Byte xw, Byte yw) {
    Pixel pValue; if (Override::EnumOverrides(Override::GetPixelAA, 4, this, xi, yi, (int)xw, (int)yw, &pValue)) return pValue;
    IfLocked(return 0)
    Byte w[4];
    {
      w[1] = AlphaLookup(xw, yw ^ 0xFF);
      w[2] = AlphaLookup(xw ^ 0xFF, yw);
      w[3] = AlphaLookup(xw, yw);
      w[0] = (w[1] + w[2] + w[3]) ^ 0xFF;
    }
    short b, g, r, a;
    Pixel S;
    AlphaLevel *Level;
    Level = AlphaLevelLookup(w[0]);
    S = this->getPixelClip(xi, yi);
    b = Level->V[S[::Blue]];
    g = Level->V[S[::Green]];
    r = Level->V[S[::Red]];
    a = Level->V[S[::Alpha]];
    Level = AlphaLevelLookup(w[1]);
    S = this->getPixelClip(++xi, yi);
    b += Level->V[S[::Blue]];
    g += Level->V[S[::Green]];
    r += Level->V[S[::Red]];
    a += Level->V[S[::Alpha]];
    Level = AlphaLevelLookup(w[2]);
    S = this->getPixelClip(--xi, ++yi);
    b += Level->V[S[::Blue]];
    g += Level->V[S[::Green]];
    r += Level->V[S[::Red]];
    a += Level->V[S[::Alpha]];
    Level = AlphaLevelLookup(w[3]);
    S = this->getPixelClip(++xi, yi);    
    S[::Blue] = ClipByte(b + Level->V[S[::Blue]]);
    S[::Green] = ClipByte(g + Level->V[S[::Green]]);
    S[::Red] = ClipByte(r + Level->V[S[::Red]]);
    S[::Alpha] = ClipByte(a + Level->V[S[::Alpha]]);
    return S;
  }

  inline Pixel getPixelAARolloff(float X, float Y) {
    int xi = X * 255.0, yi = Y * 255.0;
    Byte xw = xi % 255, yw = yi % 255;
    xi /= 255;
    yi /= 255;
    return this->getPixelAARolloff(xi, yi, xw, yw);
  }

  inline Pixel getPixelAARolloff(int xi, int yi, Byte xw, Byte yw) {
    Pixel pValue; if (Override::EnumOverrides(Override::GetPixelAA, 4, this, xi, yi, (int)xw, (int)yw, &pValue)) return pValue;
    IfLocked(return 0)
    Byte w[4];
    {
      w[1] = AlphaLookup(xw, yw ^ 0xFF);
      w[2] = AlphaLookup(xw ^ 0xFF, yw);
      w[3] = AlphaLookup(xw, yw);
      w[0] = (w[1] + w[2] + w[3]) ^ 0xFF;
    }
    short b, g, r, a;
    Pixel S;
    AlphaLevel *Level;
    Level = AlphaLevelLookup(w[0]);
    S = this->getPixelRolloff(xi, yi);
    b = Level->V[S[::Blue]];
    g = Level->V[S[::Green]];
    r = Level->V[S[::Red]];
    a = Level->V[S[::Alpha]];
    Level = AlphaLevelLookup(w[1]);
    S = this->getPixelRolloff(++xi, yi);
    b += Level->V[S[::Blue]];
    g += Level->V[S[::Green]];
    r += Level->V[S[::Red]];
    a += Level->V[S[::Alpha]];
    Level = AlphaLevelLookup(w[2]);
    S = this->getPixelRolloff(--xi, ++yi);
    b += Level->V[S[::Blue]];
    g += Level->V[S[::Green]];
    r += Level->V[S[::Red]];
    a += Level->V[S[::Alpha]];
    Level = AlphaLevelLookup(w[3]);
    S = this->getPixelRolloff(++xi, yi);    
    S[::Blue] = ClipByte(b + Level->V[S[::Blue]]);
    S[::Green] = ClipByte(g + Level->V[S[::Green]]);
    S[::Red] = ClipByte(r + Level->V[S[::Red]]);
    S[::Alpha] = ClipByte(a + Level->V[S[::Alpha]]);
    return S;
  }

  inline bool setPixelAA(float X, float Y, Pixel Color) {
    if (X < -1) return false;
    if (Y < -1) return false;
    if (X >= Width) return false;
    if (Y >= Height) return false;
    int xi = X * 255.0, yi = Y * 255.0;
    Byte xw = xi % 255, yw = yi % 255;
    xi /= 255;
    yi /= 255;
    return this->setPixelAA(xi, yi, xw, yw, Color);
  }

  bool setPixelAA(int xi, int yi, Byte xw, Byte yw, Pixel Color);

  inline Pixel getPixel(Coordinate X, Coordinate Y) {
    if ((X < 0) || (Y < 0) || (X >= Width) || (Y >= Height)) return Failure;
    Pixel pValue; if (Override::EnumOverrides(Override::GetPixel, 4, this, X, Y, &pValue)) return pValue;
    IfLocked(return 0)
    return *(this->fast_pointer(X, Y));
  }

  inline bool setPixel(Coordinate X, Coordinate Y, Pixel Value) {
    if ((X < 0) || (Y < 0) || (X >= Width) || (Y >= Height)) return false;
    this->dirty();
    if (Override::EnumOverrides(Override::SetPixel, 4, this, (int)X, (int)Y, Value.V)) return true;
    IfLocked(return false)
    *(this->fast_pointer(X, Y)) = Value;
    return true;
  }

  inline bool setPixel(Coordinate X, Coordinate Y, Pixel *Value) {
    return this->setPixel(X, Y, *Value);
  }

  inline void setPixelFast(Coordinate X, Coordinate Y, Pixel Value) {
    if ((X < 0) || (Y < 0) || (X >= Width) || (Y >= Height)) return;
    *(this->fast_pointer(X, Y)) = Value;
  }
};