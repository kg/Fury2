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

#define TRANSLATION_TABLE
#include "../header/Override.hpp"
#undef TRANSLATION_TABLE
#include "../header/Profiler_Configuration.hpp"
#include "../header/Profiler.hpp"
#include <memory.h>
#include <stdarg.h>

namespace Override {
std::string OverrideIToSTable[_count];
SToITable OverrideSToITable;
OverrideList Overrides[_count];
bool EnableOverrides;
int BypassOverrides;
};

void Override::InitOverrides() {
  Override::EnableOverrides = false;
  Override::BypassOverrides = 0;
  SS_Start
    for (int i = _none; i < _count; i++) {
      Override::Overrides[i] = OverrideList();
    }
    InitOverrideTranslationTable<Override::SToITable, std::string>(Override::OverrideSToITable, Override::OverrideIToSTable);
  SS_End
}

Export int BypassOverrides(int Adjust) {
  Override::BypassOverrides += Adjust;
  return true;
}

Export int AddOverride(const char* key, Override::Override value) {
  SS_Start
  Override::OverrideIndex index = Override::OverrideKeyToIndex(key);
  if (index < 0) return false;
  if (find(Override::Overrides[index].begin(), Override::Overrides[index].end(), value) != Override::Overrides[index].end()) {
    return false;
  } else {
    Override::Overrides[index].insert(Override::Overrides[index].begin(), value);
    Override::EnableOverrides = true;
    return true;
  }
  SS_End
  return false;
}

Export int AddOverrideAtBack(const char* key, Override::Override value) {
  SS_Start
  Override::OverrideIndex index = Override::OverrideKeyToIndex(key);
  if (index < 0) return false;
  if (find(Override::Overrides[index].begin(), Override::Overrides[index].end(), value) != Override::Overrides[index].end()) {
    return false;
  } else {
    Override::Overrides[index].push_back(value);
    Override::EnableOverrides = true;
    return true;
  }
  SS_End
  return false;
}

Export int RemoveOverride(const char* key, Override::Override value) {
  SS_Start
  Override::OverrideIndex index = Override::OverrideKeyToIndex(key);
  if (index < 0) return false;
  Override::OverrideList::iterator iter;
  iter = find(Override::Overrides[index].begin(), Override::Overrides[index].end(), value);
  if (iter != Override::Overrides[index].end()) {
    Override::Overrides[index].erase(iter);
    return true;
  } else {
    return false;
  }
  SS_End
  return false;
}

Export int GetOverrideCount(const char* key) {
  SS_Start
  Override::OverrideIndex index = Override::OverrideKeyToIndex(key);
  if (index < 0) return false;
  return Override::Overrides[index].size();
  SS_End
  return 0;
}

void Override::CleanupOverrides() {
  SS_Start
  for (int i = _none; i < _count; i++) {
    while (Override::Overrides[i].size() > 0)
      Override::Overrides[i].pop_back();
  }
  SS_End
  Override::EnableOverrides = false;
  return;
}

static const int Max_Override_Parameters = 16;

typedef int (eofp)(Override::OverrideParameters* p);

// here be demons
int Override::EnumOverrides(Override::OverrideIndex index, int parameter_count, ...) {
  if (!Override::EnableOverrides) return 0;
  if (Override::BypassOverrides > 0) return 0;
  SS_Start
  Override::OverrideList* KeyOverrides;
  KeyOverrides = ResolveOverrides(index);
  const std::string& key = Override::OverrideIndexToKey(index);
  if (parameter_count < 0) parameter_count = 0;
  if (parameter_count > Max_Override_Parameters) parameter_count = Max_Override_Parameters;
  if (KeyOverrides) {
    if (KeyOverrides->empty()) {
      return 0;
    }
    va_list parameter_list;
    unsigned int parameters[Max_Override_Parameters];
    int result = 0;
    // all shall tremble as the dead dawn rises
    va_start(parameter_list, parameter_count);
    Override::OverrideParameters ps;
    memset(parameters, 0, Max_Override_Parameters * sizeof(unsigned int));
    if (parameter_count) {
      for (int p = 0; p < parameter_count; p++) {
        parameters[p] = va_arg(parameter_list, unsigned int);
      }
    }
    va_end(parameter_list);
    Override::OverrideList::iterator OverrideIter = KeyOverrides->begin();
    eofp *fpv;
    while (OverrideIter != KeyOverrides->end()) {
      if (*OverrideIter) {
        ps.index = index;
        ps.count = parameter_count;
        ps.key = key.c_str();
        memcpy(&(ps.p[0]), &(parameters[0]), Max_Override_Parameters * sizeof(unsigned int));
        // may thee rest in peace
        fpv = (eofp*)*OverrideIter;
        if (fpv) {
          result = fpv(&ps);
        }
        if (result != 0) {
          return result;
        }
      }
      ++OverrideIter;
    }
  }
  SS_End
  return 0;
}

Export int GetOverrideIndex(const char *key) {
  if (!key) return 0;
  return (int)Override::OverrideKeyToIndex(key);
}

Export const char * GetOverrideKey(int index) {
  return Override::OverrideIndexToKey((Override::OverrideIndex)index).c_str();
}

Export int GetOverrideKeyLength(int index) {
  return Override::OverrideIndexToKey((Override::OverrideIndex)index).length();
}

Export int KeyMatches(const char *key1, const char *key2) {
  SS_Start
  if (!key1) return false;
  if (!key2) return false;
  int result = (strcmpi(key1, key2) == 0);
  return result;
  SS_End
  return 0;
}