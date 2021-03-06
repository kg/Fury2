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
#include "../header/Resample.hpp"
#include "../header/Filters.hpp"
#define TRANSLATION_TABLE
#include "../header/Debug_Flags.hpp"
#include "../header/MersenneTwister.h"
#undef SetDebugFlag
#undef GetDebugFlag

namespace DebugFlags {
  int Flags[DebugFlags::count];
  std::string FlagIToSTable[DebugFlags::count];
  SToFTable FlagSToITable;
}

// Variables
AlphaLevel*       AlphaTable          = Null;
AlphaLevel**      AlphaRootTable      = Null;
PythagorasLevel*  PythagorasTable     = Null;
PythagorasLevel** PythagorasRootTable = Null;
win32::LARGE_INTEGER CPUFreq;
bool Initialized = false;

MTRand mersenne = MTRand();

int __cdecl DllMain( int hModule, int ul_reason_for_call, void* lpReserved )
{
    return Success;
}

namespace Processor {

    // Variables
    unsigned int Features = 0;
    bool MMX_CMov = false;
    bool MMX = false;
    bool SSE = false;
    bool SSE2 = false;

    void DetectProcessor() {

      Features = 0;
      SSE = false;
      SSE2 = false;
      MMX = false;
      MMX_CMov = false;

      try {

        #if (defined(ASSEMBLY))
        _asm {
            push eax
            push ebx
            push ecx
            push edx

            mov    eax, 1
            cpuid
            mov    Features, edx

            pop    edx
            pop    ecx
            pop    ebx
            pop    eax
        }
        #endif

        SSE = ((Features & Feature_SSE) == Feature_SSE);
        SSE2 = ((Features & Feature_SSE2) == Feature_SSE2);
        MMX = ((Features & Feature_MMX) == Feature_MMX);
        MMX_CMov = ((Features & Feature_MMX_CMov) == Feature_MMX_CMov);


      }
      catch (...) {
      }

      return;
    }
}

void InitAlphaTable() {
    int a = 0, v = 0;
    AlphaTable = AllocateArray(AlphaLevel, 512);
    AlphaRootTable = AllocateArray(AlphaLevel*, 512);

    for (a = 0; a < 512; a++) {
        AlphaRootTable[a] = &(AlphaTable[a]);
        for (v = 0; v < 256; v++) {
            AlphaTable[a].V[v] = ClipByte( ( a * v ) / 255 );
        }
    }

    return;
}

void DestroyAlphaTable() {

    DeleteArray(AlphaTable);
    DeleteArray(AlphaRootTable);

    return;
}

void InitPythagorasTable() {
    int a = 0, b = 0;
    PythagorasTable = AllocateArray(PythagorasLevel, 256);
    PythagorasRootTable = AllocateArray(PythagorasLevel*, 256);

    for (a = 0; a < 256; a++) {
        PythagorasRootTable[a] = &(PythagorasTable[a]);
        for (b = 0; b < 256; b++) {
#ifdef ACCURATE_PYTHAGORAS
            PythagorasTable[a].V[b] = ClipByte( sqrt((float)(a * a) + (b * b)) * PythagorasConstant);
#else
            PythagorasTable[a].V[b] = ClipByte( sqrt((float)(a * a) + (b * b)) );
#endif
        }
    }

    return;
}

void DestroyPythagorasTable() {

    DeleteArray(PythagorasTable);
    DeleteArray(PythagorasRootTable);

    return;
}

Export ScalerFunction* GetDefaultSampleFunction() {

    return DefaultSampleFunction;
}

Export void SetDefaultSampleFunction(ScalerFunction *Default) {

    if (!Default) Default = SampleRow_Linear;

    DefaultSampleFunction = Default;

    return;
}

Export void SetDebugFlag(int id, int value) {
  if ((id < 0) || (id >= DebugFlags::count)) return;
  DebugFlags::Flags[id] = value;
  return;
}

Export int GetDebugFlag(int id) {
  if ((id < 0) || (id >= DebugFlags::count)) return 0;
  return DebugFlags::Flags[id];
}

Export int GetDebugFlagId(const char* name) {
  std::string key = std::string(name);
  DebugFlags::SToFTable::const_iterator iter = DebugFlags::FlagSToITable.find(key);
  if (iter != DebugFlags::FlagSToITable.end()) {
    return iter->second;
  } else {
    return -1;
  }
}

void InitDebugFlags() {
  InitDebugFlagTranslationTable<DebugFlags::SToFTable, std::string>(DebugFlags::FlagSToITable, DebugFlags::FlagIToSTable);
  SetDebugFlag(DebugFlags::RenderAmbientLight, true);
  SetDebugFlag(DebugFlags::RenderLightCoronas, true);
  SetDebugFlag(DebugFlags::RenderLightShadows, true);
  SetDebugFlag(DebugFlags::RenderLightSurfaces, true);
  SetDebugFlag(DebugFlags::RenderLightSprites, true);
  SetDebugFlag(DebugFlags::RenderLightSourceSingle, 0);
}

Export void Initialize() {

    if (Initialized) return;

    Processor::DetectProcessor();
    win32::QueryPerformanceFrequency(&CPUFreq);
    LookupInitialize();
    InitAlphaTable();
    InitPythagorasTable();
    Override::InitOverrides();
    InitHeap();
    StaticInit();
    InitDebugFlags();
 
    DefaultSampleFunction = SampleRow_Linear;

    Initialized = true;

    return;
}

Export void Uninitialize() {

    if (!Initialized) return;

    StaticCleanup();
    CleanupHeap();
    LookupUninitialize();
    DestroyAlphaTable();
    DestroyPythagorasTable();
    Override::CleanupOverrides();
    Initialized = false;

    return;
}

Export double GetTime() {
    win32::LARGE_INTEGER time;
    win32::QueryPerformanceCounter(&time);
    return ((double)time.QuadPart) / ((double)CPUFreq.QuadPart);
}

Export int GetInitialized() {
    return Initialized;
}


Export Image* AllocateEmptyImage() {

    if (!Initialized) return Failure;

    return new Image();
}

Export int LockImage(Image* Image) {

    if (!Initialized) return Failure;
    if (!Image) return Failure;

    if (Image->lock()) {
      return Success;
    }
    return Failure;
}

Export int UnlockImage(Image* Image) {

    if (!Initialized) return Failure;
    if (!Image) return Failure;

    if (Image->unlock()) {
      return Success;
    }
    return Failure;
}

Export void SetImageLocked(Image *Image, int NewState) {

  if (!Initialized) return;

  if (Image) {
    Image->Unlocked = (NewState == 0);
  }

  return;
}

Export void SetImageDirty(Image *Image, int NewState) {

  if (!Initialized) return;

  if (Image) {
    Image->Dirty = (NewState != 0);
  }

  return;
}

Export int GetImageDirty(Image *Image) {

  if (!Initialized) return false;

  if (Image) {
    if (Image->Dirty) {
      return true;
    }
  }

  return false;
}

Export void SetImagePremultiplied(Image *Image, int NewState) {

  if (!Initialized) return;

  if (Image) {
    Image->OptimizeData.premultiplied = (NewState != 0);
  }

  return;
}

Export int GetImagePremultiplied(Image *Image) {

  if (!Initialized) return false;

  if (Image) {
    if (Image->OptimizeData.premultiplied) {
      return true;
    }
  }

  return false;
}

Export void SetImageNormalsPrepared(Image *Image, int NewState) {

  if (!Initialized) return;

  if (Image) {
    Image->OptimizeData.normalsPrepared = (NewState != 0);
  }

  return;
}

Export int GetImageNormalsPrepared(Image *Image) {

  if (!Initialized) return false;

  if (Image) {
    if (Image->OptimizeData.normalsPrepared) {
      return true;
    }
  }

  return false;
}

Export int GetImageLocked(Image *Image) {

  if (!Initialized) return true;

  if (Image) {
    if (Image->Unlocked) {
      return false;
    }
  }

  return true;
}

Export Image* AllocateImage(int Width, int Height) {

    if (!Initialized) return Failure;

    return new Image(Width, Height);
}

Export Image* AllocateSharedImage(int Width, int Height, unsigned char* ID) {

    if (!Initialized) return Failure;

    Image* newImage = new Image();
    newImage->allocateShared(Width, Height, ID);
    return newImage;
}

Export Image* AllocateImageHandle(int Width, int Height, int Pitch, Pixel* Pointer) {

    if (!Initialized) return Failure;

    return new Image(Pointer, Width, Height, Pitch);
}

Export Image* AllocateDIBSection(int Width, int Height, int DC) {

    if (!Initialized) return Failure;

    return new Image(Width, Height, true, DC);
}

Image* AutoAddAlpha(corona::Image* img) {
  bool alpha = (img->getFormat() == corona::PF_R8G8B8A8 || img->getFormat() == corona::PF_B8G8R8A8);
  bool paletted = (img->getFormat() == corona::PF_I8);
  if (paletted) {
      alpha = alpha || (img->getPaletteFormat() == corona::PF_R8G8B8A8 || img->getPaletteFormat() == corona::PF_B8G8R8A8);
  }
  corona::Image* convertedImage = Null;
  if (img->getFormat() != corona::PF_B8G8R8A8) {
    convertedImage = corona::ConvertImage(img, corona::PF_B8G8R8A8);
    img = Null;
  } else {
    convertedImage = img;
  }
  Image* newImage = Null;
  newImage = new Image(convertedImage);
  if (alpha) {
  } else {
    FilterSimple_Fill_Channel(newImage, Null, ::Alpha, 255);
  }
  return newImage;
}

Export Image* AllocateImageFromFile(const Byte* Filename) {

    if (!Initialized) return Failure;
    if (!Filename) return Failure;

    return AutoAddAlpha(corona::OpenImage(reinterpret_cast<const char*>(Filename), corona::PF_DONTCARE));
}

Export Image* AllocateImageFromFileBuffer(const void* Buffer, int Size) {

    if (!Initialized) return Failure;
    if (!Buffer) return Failure;
    if (Size < 1) return Failure;

    corona::File* memFile = corona::CreateMemoryFile(Buffer, Size);
    if (!memFile) return Failure;
    Image* newImage = AutoAddAlpha(corona::OpenImage(memFile, corona::PF_B8G8R8A8));
    if (!newImage) return Failure;
    return newImage;
}

Export Image* AllocateImageFromPointer(Pixel *Data, int Width, int Height, int Pitch) {

    if (!Initialized) return Failure;
    if (!Data) return Failure;
    if ((Width <= 0) || (Height <= 0)) return Failure;

    Image* oldImage = new Image();
    oldImage->Width = Width;
    oldImage->Height = Height;
    oldImage->Pitch = Pitch;
    oldImage->Data = Data;
    Image* newImage = new Image(oldImage);
    oldImage->Data = Null;
    delete oldImage;
    return newImage;

}

Export int SaveImageToTGA(Image *Source, const Byte* Filename) {
    if (!Source) return Failure;
    if (!Source->initialized()) return Failure;
    if (!Source->Unlocked) return Failure;
    if (!Filename) return Failure;
    corona::Image *pImg = corona::CreateImage(Source->Width, Source->Height, corona::PF_B8G8R8A8);
    if (!pImg) return Failure;
    _Copy<Byte>(pImg->getPixels(), Source->pointer(0,0), Source->Width * Source->Height * 4);
    corona::SaveImage(reinterpret_cast<const char*>(Filename), corona::FF_TGA, pImg);
    delete pImg;
    pImg = Null;
    return Success;
}

Export int SaveImageToPNG(Image *Source, const Byte* Filename) {
    if (!Source) return Failure;
    if (!Source->initialized()) return Failure;
    if (!Source->Unlocked) return Failure;
    if (!Filename) return Failure;
    corona::Image *pImg = corona::CreateImage(Source->Width, Source->Height, corona::PF_B8G8R8A8);
    if (!pImg) return Failure;
    _Copy<Byte>(pImg->getPixels(), Source->pointer(0,0), Source->Width * Source->Height * 4);
    corona::SaveImage(reinterpret_cast<const char*>(Filename), corona::FF_PNG, pImg);
    delete pImg;
    pImg = Null;
    return Success;
}

Export DoubleWord* AllocateSharedMemory(int Size, unsigned char *ID) {
  bool isNew = true;
  win32::HANDLE mapping = 0;
  DoubleWord* pointer = 0;
  mapping = win32::CreateFileMappingA(((win32::HANDLE)(win32::LONG_PTR)-1), Null, PAGE_READWRITE, 0, Size + 4, (win32::LPCSTR)ID);
  if (win32::GetLastError() == ERROR_ALREADY_EXISTS) {
    isNew = false;
  }
  if (mapping) {
    pointer = reinterpret_cast<DoubleWord*>(win32::MapViewOfFile(mapping, FILE_MAP_ALL_ACCESS, 0, 0, 0));
    if (pointer) {
      if (isNew) {
        *pointer = reinterpret_cast<DoubleWord>(mapping);
        pointer++;
      } else {
        if (*pointer != reinterpret_cast<DoubleWord>(mapping)) {
          win32::UnmapViewOfFile(pointer);
          win32::CloseHandle(mapping);
        }
        pointer++;
      }
      return pointer;
    } else {
      win32::CloseHandle(mapping);
    }
  }
  return 0;
}

Export int DeallocateSharedMemory(DoubleWord* pointer) {
  win32::HANDLE mapping = 0;
  if (pointer) {
    pointer--;
    mapping = reinterpret_cast<win32::HANDLE>(*pointer);
    if (mapping) {
      win32::UnmapViewOfFile(pointer);
      win32::CloseHandle(mapping);
      return Success;
    }
  }
  return Failure;
}

Export Image* AllocateImageCopy(Image *Source) {

  if (!Initialized) return Failure;
  if (!Source) return Failure;

  return new Image(Source);
}

Export void DeallocateImage(Image *Image) {

  if (!Initialized) return;

  if (Image) delete Image;

  return;
}

Export void ReallocateImage(Image *Image, int Width, int Height) {

  if (!Initialized) return;

  if (Image) Image->allocate(Width, Height);

  return;
}

Export void ResizeImage(Image *Image, int Width, int Height) {

  if (!Initialized) return;

  if (Image) Image->resize(Width, Height);

  return;
}

Export void ResampleImage(Image *Target, Image *Source, int Width, int Height, ResampleModes ResampleMode) {

  if (!Initialized) return;

  if (!Target) return;
  if (!Source) return;

  Source->resample(Target, Width, Height, ResampleMode);

  return;
}

Export void SlideImage(Image *Image, int X, int Y) {

  if (!Initialized) return;

  if (Image) Image->slide(X, Y);

  return;
}

Export void OptimizeImage(Image *Image) {

  if (!Initialized) return;

  if (Image) Image->optimize();

  return;
}

Export void UnsizeImage(Image *Image) {

  if (!Initialized) return;

  if (Image) Image->deallocate();

  return;
}

Export void SetImageWidth(Image *Image, int Width) {

  if (!Initialized) return;

  if (Image) Image->Width = Width;

  return;
}

Export int GetImageWidth(Image *Image) {

  if (!Initialized) return Failure;

  if (Image) return Image->Width;

  return Failure;
}

Export void SetImageHeight(Image *Image, int Height) {

  if (!Initialized) return;

  if (Image) Image->Height = Height;

  return;
}

Export int GetImageHeight(Image *Image) {

  if (!Initialized) return Failure;

  if (Image) return Image->Height;

  return Failure;
}

Export void SetImagePitch(Image *Image, int Pitch) {

  if (!Initialized) return;

  if (Image) Image->Pitch = Pitch;

  return;
}

Export int GetImagePitch(Image *Image) {

  if (!Initialized) return Failure;

  if (Image) return Image->Pitch;

  return Failure;
}

Export void SetCallbacks(int reserved1, int reserved2) {

  return;
}

Export Pixel* GetImagePointer(Image *Image, int X, int Y) {

  if (!Initialized) return Failure;

  if (Image) return Image->pointer(X,Y);

  return Failure;
}

Export int GetImageDIBHandle(Image *Image) {

  if (!Initialized) return Failure;

  if (Image) return Image->getDIB();

  return Failure;
}

Export int GetImageSharedHandle(Image *Image) {

  if (!Initialized) return Failure;

  if (Image) return Image->getSharedHandle();

  return Failure;
}

Export void ClearImageTags(Image *Image) {

  if (!Initialized) return;

  if (Image) {
    for (int i = 0; i < Image::TagCount; i++) {
      Image->Tags[i] = 0;
    }
  }

  return;
}

Export void SetImageTag(Image *Image, int Index, DoubleWord Value) {

  if (!Initialized) return;
  if (Index < 0) return;
  if (Index >= Image::TagCount) return;

  if (Image) Image->Tags[Index] = Value;

  return;
}

Export DoubleWord GetImageTag(Image *Image, int Index) {

  if (!Initialized) return Failure;
  if (!Image) return Failure;
  if (Index < 0) return Failure;
  if (Index >= Image::TagCount) return 0;

  return Image->Tags[Index];
}

Export void SetImagePointer(Image *Image, DoubleWord Pointer) {

  if (!Initialized) return;

  if (Image) Image->Data = (Pixel*)Pointer;

  return;
}

Export void SetImageMatteColor(Image *Image, Pixel Color) {

  if (!Initialized) return;

  {
    int overrideresult = Override::EnumOverrides(Override::SetImageMatteColor, 2, Image, Color);
#ifdef OVERRIDES
    if (overrideresult != 0) return;
#endif
  }

  if (Image) {
    Image->MatteColor = Color;
  }

  return;
}

Export DoubleWord GetImageMatteColor(Image *Image) {

  if (!Initialized) return Failure;
  if (!Image) return Failure;

  return Image->MatteColor.V;
}

Export void SetImageBrushPattern(Image *Target, Image *Pattern) {

  if (!Initialized) return;
  if (!Pattern) return;
  if (!Target) return;

  Target->setBrushPattern(Pattern);

  return;
}

Export Image* GetImageBrushPattern(Image *Image) {

  if (!Initialized) return Null;
  if (!Image) return Null;

  return Image->getBrushPattern();
}

Export void SetImageClipRectangle(Image *Image, Rectangle *NewRectangle) {

  if (!Initialized) return;

  {
    int overrideresult = Override::EnumOverrides(Override::SetImageClipRectangle, 2, Image, NewRectangle);
#ifdef OVERRIDES
    if (overrideresult != 0) return;
#endif
  }

  if (Image) Image->ClipRectangle = *NewRectangle;

  return;
}

Export int GetImageClipRectangle(Image *Image, Rectangle *OutRectangle) {

  if (!Initialized) return Failure;
  if (!OutRectangle) return Failure;
  if (!Image) return Failure;

  *OutRectangle = Image->ClipRectangle;

  return Failure;
}

Export int GetImageRectangle(Image *Image, Rectangle *OutRectangle) {

  if (!Initialized) return Failure;
  if (!OutRectangle) return Failure;
  if (!Image) return Failure;

  *OutRectangle = Image->getRectangle();

  return Failure;
}

Export void NormalizeRectangle(Rectangle *Rect) {

  if (!Rect) return;

  Rect->normalize();

  return;
}

Export int ClearImage(Image *Image) {

  if (!Initialized) return Failure;
  if (!Image) return Failure;

  Image->clear();

  return Success;
}

Export int FillImage(Image *Image, Pixel Color) {

  if (!Initialized) return Failure;
  if (!Image) return Failure;

  Image->fill(Color);

  return Success;
}

Export int CopyImage(Image *Dest, Image *Source) {

  if (!Initialized) return Failure;
  if (!Dest) return Failure;
  if (!Source) return Failure;

  Dest->copy(Source);

  return Success;
}

Export int GetPixel(Image *Image, int X, int Y) {

  if (!Initialized) return Failure;
  if (!Image) return Failure;

  return Image->getPixel(X, Y).V;
}

Export int GetPixelAA(Image *Image, float X, float Y) {

  if (!Initialized) return Failure;
  if (!Image) return Failure;

  return Image->getPixelAA(X, Y).V;
}

Export int SetPixel(Image *Image, int X, int Y, Pixel Value) {

  if (!Initialized) return Failure;
  if (!Image) return Failure;

  if (Image->setPixel(X, Y, Value)) return Success;

  return Failure;
}

Export int SetPixelAA(Image *Image, float X, float Y, Pixel Value) {

  if (!Initialized) return Failure;
  if (!Image) return Failure;

  if (Image->setPixelAA(X, Y, Value)) return Success;

  return Failure;
}

Export int RotateImage(Image *Image, float Angle) {

  if (!Initialized) return Failure;
  if (!Image) return Failure;

  Image->rotate(Angle);

  return Success;
}

Export int GenerateColorFilter(ColorFilter *Filter, int RedGamma, int GreenGamma, int BlueGamma) {

  if (!Filter) return Failure;
  if (Filter->Length < 768) return Failure;

  for (int i = 0; i < 256; i++) {
      Filter->Red[i] = ClipByte( ( i * RedGamma ) / 255 );
      Filter->Green[i] = ClipByte( ( i * GreenGamma ) / 255 );
      Filter->Blue[i] = ClipByte( ( i * BlueGamma ) / 255 );
  }

  return Success;
}

ILine::ILine(Rectangle *Rect) {

  this->Start.X = Rect->Left;
  this->Start.Y = Rect->Top;
  this->End.X = Rect->right();
  this->End.Y = Rect->bottom();

}

ILine::ILine(FLine *Line) {

  this->Start.X = Line->Start.X;
  this->Start.Y = Line->Start.Y;
  this->End.X = Line->End.X;
  this->End.Y = Line->End.Y;

}

FLine::FLine(ILine *Line) {

  this->Start.X = Line->Start.X;
  this->Start.Y = Line->Start.Y;
  this->End.X = Line->End.X;
  this->End.Y = Line->End.Y;

}

Export int GetLockingMode() {
  return (int)lockingMode;
}

Export void SetLockingMode(int newMode) {
  lockingMode = (LockingModes)newMode;
  return;
}

Export void SeedMersenne(DoubleWord Seed) {
  mersenne.seed(Seed);
  return;
}

Export DoubleWord GetMersenneValue() {
  return mersenne.randInt();
}

Export float GetMersenneValueFloat() {
  return mersenne.rand();
}

Export double GetMersenneValueDouble() {
  return mersenne.rand();
}