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
	defImport(int, GetImageWidth, (int Image) , 4) \
	defImport(int, GetImageHeight, (int Image) , 4) \
	defImport(int, GetImagePitch, (int Image) , 4) \
	defImport(void *, GetImagePointer, (int Image) , 4) \
	defImport(int, GetImageTag, (int Image, int Index) , 8) \
	defImport(int, GetImageLocked, (int Image) , 4) \
	defImport(int, GetImageDirty, (int Image) , 4) \
	defImport(void, SetImageWidth, (int Image, int Value) , 8) \
	defImport(void, SetImageHeight, (int Image, int Value) , 8) \
	defImport(void, SetImagePitch, (int Image, int Value) , 8) \
	defImport(void, SetImagePointer, (int Image, void * Value) , 8) \
	defImport(void, SetImageTag, (int Image, int Index, int Value) , 12) \
	defImport(void, SetImageLocked, (int Image, int Value) , 8) \
	defImport(void, SetImageDirty, (int Image, int Value) , 8) \
	defImport(int, LockImage, (int Image) , 4) \
	defImport(int, UnlockImage, (int Image) , 4) \
	defImport(int, GetDefaultSampleFunction, () , 0) \
  defImport(int, FilterSimple_Fill, (int Image, FX::Rectangle *Area, Pixel Color), 12) \
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
#else
	extern HINSTANCE Library;
#define defImport(ret, n, params, sz) \
	typedef ret (*n ## _ptr) params; \
	extern n ## _ptr n;
#endif
#define defEnd \

defImports

#endif

#undef defStart
#undef defImport
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
#undef defEnd
#undef defImports

}