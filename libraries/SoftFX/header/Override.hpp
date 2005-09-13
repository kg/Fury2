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

#define _SECURE_SCL 0
#define _SECURE_SCL_THROWS 0
#include <map>
#include <string>
#include <string.h>
#include <vector>
#include <list>
#include <algorithm>
#include "../header/Constant_Overrides.hpp"

#define SS_Start
#define SS_End

#ifndef Export
  #define Export extern "C" __declspec(dllexport)
#endif

inline unsigned int va_float(float value) {
  return *(reinterpret_cast<unsigned int*>(&value));
}

namespace Override {
typedef void* Override;
typedef std::vector<Override> OverrideList;
typedef std::map<std::string, OverrideIndex> SToITable;
extern std::string OverrideIToSTable[_count];
extern SToITable OverrideSToITable;
extern OverrideList Overrides[_count];
extern bool EnableOverrides;
extern int BypassOverrides;

struct OverrideParameters {
  int count;
  const char* key;
  int index;
  unsigned int p[16];
};

inline OverrideList* ResolveOverrides(const OverrideIndex key) {
#if (defined(OVERRIDES) || !defined(SOFTFX))
  SS_Start
    if (Overrides[key].size()) {
      return Overrides + key;
    }
  SS_End
  return 0;
#else
  return 0;
#endif
}

inline OverrideIndex OverrideKeyToIndex(const std::string& key) {
  SToITable::const_iterator iter = OverrideSToITable.find(key);
  if (iter != OverrideSToITable.end()) {
    return iter->second;
  } else {
    return (OverrideIndex)-1;
  }
}

inline OverrideIndex OverrideKeyToIndex(const char* key) {
  return OverrideKeyToIndex(std::string(key));
}

inline const std::string& OverrideIndexToKey(OverrideIndex index) {
  return (OverrideIToSTable[index]);
}

void InitOverrides();
void CleanupOverrides();
#if (defined(OVERRIDES) || !defined(SOFTFX))
  int EnumOverrides(Override::OverrideIndex key, int parameter_count, ...);
#else
  int EnumOverrides(Override::OverrideIndex key, int parameter_count, ...);
#endif

};

Export int AddOverride(const char* key, Override::Override value);
Export int AddOverrideAtBack(const char* key, Override::Override value);
Export int RemoveOverride(const char* key, Override::Override value);
Export int GetOverrideCount(const char* key);
Export int BypassOverrides(int Adjust);
