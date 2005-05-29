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

namespace SoftFX {

#define defImports \
	defStart \
/* IMPORTS GO HERE */ \
	defImport(int, AddOverride, (const char * key, Override::Override value) , 8) \
	defImport(int, AddOverrideAtBack, (const char * key, Override::Override value) , 8) \
	defImport(int, RemoveOverride, (const char * key, Override::Override value) , 8) \
	defImport(int, GetOverrideCount, (const char * key) , 4) \
	defImport(int, GetOverrideIndex, (const char * key) , 4) \
	defImport(int, BypassOverrides, (int Adjust) , 4) \
	defImport(const char *, GetOverrideKey, (int index) , 4) \
	defImport(int, AllocateEmptyImage, () , 0) \
	defImport(int, AllocateImage, (int Width, int Height) , 8) \
	defImport(int, ReallocateImage, (int Image, int Width, int Height) , 12) \
  defImport(int, DeallocateImage, (int Image), 4) \
	defImport(int, GetImageWidth, (int Image) , 4) \
	defImport(int, GetImageHeight, (int Image) , 4) \
	defImport(int, GetImagePitch, (int Image) , 4) \
	defImport(void *, GetImagePointer, (int Image, int X, int Y) , 12) \
	defImport(int, GetImageTag, (int Image, int Index) , 8) \
	defImport(int, GetImageLocked, (int Image) , 4) \
	defImport(int, GetImageDirty, (int Image) , 4) \
	defImport(DoubleWord, GetImageMatteColor, (int Image) , 4) \
	defImport(void, SetImageMatteColor, (int Image, Pixel Color) , 8) \
  defImport(int, GetImageRectangle, (int Image, FX::Rectangle* Rect) , 8) \
  defImport(int, GetImageClipRectangle, (int Image, FX::Rectangle* Rect) , 8) \
  defImport(void, SetImageClipRectangle, (int Image, FX::Rectangle* Rect) , 8) \
	defImport(void, SetImageWidth, (int Image, int Value) , 8) \
	defImport(void, SetImageHeight, (int Image, int Value) , 8) \
	defImport(void, SetImagePitch, (int Image, int Value) , 8) \
	defImport(void, SetImagePointer, (int Image, void * Value) , 8) \
	defImportAliased(void, SetImageTag_, SetImageTag, (int Image, int Index, int Value) , 12) \
	defImport(void, SetImageLocked, (int Image, int Value) , 8) \
	defImport(void, SetImageDirty, (int Image, int Value) , 8) \
	defImport(int, LockImage, (int Image) , 4) \
	defImport(int, UnlockImage, (int Image) , 4) \
	defImport(int, GetDefaultSampleFunction, () , 0) \
	defImport(int, GetLinearScaler, () , 0) \
	defImport(int, GetBilinearScaler, () , 0) \
	defImport(int, GetLinearWrapScaler, () , 0) \
	defImport(int, GetBilinearWrapScaler, () , 0) \
	defImport(int, GetLinearClampScaler, () , 0) \
	defImport(int, GetBilinearClampScaler, () , 0) \
	defImport(int, GetMergeRenderer, () , 0) \
	defImport(int, GetSourceAlphaRenderer, () , 0) \
	defImport(int, GetAdditiveRenderer, () , 0) \
	defImport(int, GetSubtractiveRenderer, () , 0) \
	defImport(int, GetScreenRenderer, () , 0) \
  defImport(int, Clip2D_SimpleRect, (FX::Rectangle *Rect, int Dest, int Source, FX::Rectangle *DestRect, int &CX, int &CY), 24) \
  defImport(int, Clip2D_PairedRect, (FX::Rectangle *Rect, FX::Rectangle *RectS, int Dest, int Source, FX::Rectangle *DestRect, FX::Rectangle *SourceRect, int *CropOutput), 28) \
  defImport(int, ClipRectangle_ImageClipRect, (FX::Rectangle *Rect, int Image), 8) \
  defImport(int, BlitSimple_Normal, (int Dest, int Source, FX::Rectangle *Area, int SX, int SY), 20) \
  defImport(int, GetPolygonVertexCount, (int Polygon), 4) \
  defImport(int, GetGradientPolygonVertexCount, (int Polygon), 4) \
  defImport(int, GetTexturedPolygonVertexCount, (int Polygon), 4) \
  defImport(FPoint*, GetPolygonVertexPointer, (int Polygon, int Index), 8) \
  defImport(GradientVertex*, GetGradientPolygonVertexPointer, (int Polygon, int Index), 8) \
  defImport(TexturedVertex*, GetTexturedPolygonVertexPointer, (int Polygon, int Index), 8) \
  defImport(int, GetTile, (int Tileset, int Index), 8) \
  defImport(int, GetTileFast, (int Tileset, unsigned int Index, short* MapTable), 12) \
  defImport(int, GetTileCount, (int Tileset), 4) \
  defImport(int, GetTileWidth, (int Tileset), 4) \
  defImport(int, GetTileHeight, (int Tileset), 4) \
  defImport(int, FilterSimple_Replace, (int Image, FX::Rectangle* Area, Pixel Find, Pixel Replace), 16) \
/* IMPORTS GO HERE */ \
	defEnd

/* VAR DECLARATIONS */
#ifndef IMPORT_VARS
#define IMPORT_VARS
#define defStart \

#ifdef IMPORT_DECLARATIONS
	HINSTANCE Library;
#define defImport(ret, n, params, sz) \
	typedef ret (*n ## _ptr) params; \
	n ## _ptr n;
#define defImportAliased(ret, a, n, params, sz) \
	typedef ret (*n ## _ptr) params; \
	n ## _ptr a;
#else
	extern HINSTANCE Library;
#define defImport(ret, n, params, sz) \
	typedef ret (*n ## _ptr) params; \
	extern n ## _ptr n;
#define defImportAliased(ret, a, n, params, sz) \
	typedef ret (*n ## _ptr) params; \
	extern n ## _ptr a;
#endif
#define defEnd \

defImports

#endif

#undef defStart
#undef defImport
#undef defImportAliased
#undef defEnd

#ifdef IMPORT_INITIALIZER
#define defStart \
	inline void Load() { \
	Library = LoadLibrary("SoftFX"); \
	char sName[256];
#define defImport(ret, n, params, sz) \
	if (Library) { \
		sprintf(sName, "_%s@%i", #n, sz); \
		n = (n ## _ptr)GetProcAddress(Library, sName); \
	} else { \
		n = 0; \
	}
#define defImportAliased(ret, a, n, params, sz) \
	if (Library) { \
		sprintf(sName, "_%s@%i", #n, sz); \
		a = (n ## _ptr)GetProcAddress(Library, sName); \
	} else { \
		a = 0; \
	}
#define defEnd \
	return;	\
	}

defImports

	inline void Unload() {
		if (Library) {
			FreeLibrary(Library);
			Library = 0;
		}
	}
#endif

#undef defStart
#undef defImport
#undef defImportAliased
#undef defEnd
#undef defImports

inline void SetImageTag(int Image, int Index, void* Value) {
  return SetImageTag_(Image, Index, reinterpret_cast<int>(Value));
}

inline void SetImageTag(int Image, int Index, int Value) {
  return SetImageTag_(Image, Index, Value);
}

enum SFX_BlitModes {
    BlitMode_Default = -1,
    BlitMode_Normal = 0,
    BlitMode_Matte = 1,
    BlitMode_SourceAlpha = 2,
    BlitMode_Additive = 3,
    BlitMode_Subtractive = 4,
    BlitMode_AND = 5,
    BlitMode_OR = 6,
    BlitMode_XOR = 7,
    BlitMode_Lightmap = 8,
    BlitMode_Lightmap_RGB = 9,
    BlitMode_Matte_Tint = 10,
    BlitMode_SourceAlpha_Tint = 11,
    BlitMode_Font = 12,
    BlitMode_Font_SourceAlpha = 13,
    BlitMode_Dither = 14,
    BlitMode_Screen = 15,
    BlitMode_Multiply = 16,
    BlitMode_Merge = 17,
    BlitMode_Unerase = 18,
    BlitMode_Erase = 19,
    BlitMode_Font_Merge = 20,
    BlitMode_Behind = 21,
    BlitMode_Dodge = 22,
    BlitMode_Burn = 23,
    BlitMode_Normal_Tint = 24,
  	BlitMode_Additive_SourceAlpha = 25,
	  BlitMode_Subtractive_SourceAlpha = 26,
    BlitMode_SourceAlpha_ColorMask = 27
};

}