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

statictable staticTables[StaticTableCount];

inline void* vm_alloc(DoubleWord size) {
  DoubleWord *temp, *result;
#ifdef VIRTUALALLOC
  temp = reinterpret_cast<DoubleWord*>(win32::VirtualAlloc(Null, size + 8, MEM_COMMIT, PAGE_READWRITE));
#else
  temp = reinterpret_cast<DoubleWord*>(malloc(size + 8));
#endif
  if (temp) {
    // set beginning size tag
    *temp = size;
    temp += 1;
    result = temp;
    temp = (DoubleWord*)(((DoubleWord)temp) + size);
    // set end size tag
    *temp = size;
  }
  return result;
}

inline bool vm_dealloc(void* pointer) {
  if (pointer) {
    DoubleWord size_start, size_end;
    DoubleWord *temp;
    temp = reinterpret_cast<DoubleWord*>(pointer);
    temp -= 1;
    pointer = temp;
    size_start = *temp;
    temp += 1;
    temp = (DoubleWord*)(((DoubleWord)temp) + size_start);
    size_end = *temp;
    if (size_start == size_end) {
    #ifdef VIRTUALALLOC
      win32::VirtualFree(pointer, size_start + 8, MEM_DECOMMIT);
    #else
      free(pointer);
    #endif
      return true;
    } else {
      // memory corrupted!
      assert(false);
      return false;
    }
  }
  return false;
}

#ifdef LOOKUPCACHE
class LookupCacheEntry {
public:
  void* Data;
  zeroinit<DoubleWord> Size;
  zeroinit<bool> InUse;

  LookupCacheEntry() {
    Data = Null;
  }
};

typedef std::list<LookupCacheEntry> LCType;

static std::list<LookupCacheEntry> LookupCache;
static bool LookupCacheInitialized;
#endif

void LookupInitialize() {
#ifdef LOOKUPCACHE
  if (LookupCacheInitialized) return;
  LookupCache.clear();
  LookupCacheInitialized = true;
#endif
  return;
}

void LookupUninitialize() {
#ifdef LOOKUPCACHE
  if (!LookupCacheInitialized) return;
  LCType::iterator iter;
  iter = LookupCache.begin();
  while (iter != LookupCache.end()) {
    vm_dealloc(iter->Data);
    ++iter;
  }
  LookupCache.clear();
  LookupCacheInitialized = false;
#endif
  return;
}

#ifdef LOOKUPCACHE
class jack_the_ripper : public std::unary_function<LookupCacheEntry, bool> 
{
public:
   bool operator( ) ( LookupCacheEntry& val ) 
   {
   return ( val.Data == Null );
   }
};
#endif

void LookupGC() {
#ifdef LOOKUPCACHE
  if (LookupCacheInitialized) {
    LCType::iterator iter;
    iter = LookupCache.begin();
    while (iter != LookupCache.end()) {
      if (iter->InUse) {
      } else {
        if (iter->Data) {
          vm_dealloc(iter->Data);
          iter->Data = Null;
        }
      }
      ++iter;
    }
    LookupCache.remove_if(jack_the_ripper());
    return;
  }
#endif
  return;
}

void* _LookupAllocate(DoubleWord Size) {
#ifdef LOOKUPCACHE
  if (LookupCacheInitialized) {
    if (Size < 1) return 0;
    LCType::iterator iter;
    iter = LookupCache.begin();
    while (iter != LookupCache.end()) {
      if (!(iter->InUse)) {
        if (iter->Size >= Size) {
          iter->InUse = true;
          return iter->Data;
        }
      }
      ++iter;
    }
    LookupCacheEntry entry;
    entry.Data = vm_alloc(Size);
    entry.Size = Size;
    entry.InUse = true;
    LookupCache.push_front(entry);
    return entry.Data;
  } else {
    return Null;
  }
#endif
  return vm_alloc(Size);
}

bool _LookupDeallocate(void* Pointer) {
#ifdef LOOKUPCACHE
  if (!Pointer) return false;
  if (LookupCacheInitialized) {
    LCType::iterator iter;
    iter = LookupCache.begin();
    while (iter != LookupCache.end()) {
      if (iter->Data == Pointer) {
        iter->InUse = false;
        LookupGC();
        return true;
      }
      ++iter;
    }
    return false;
  } else {
    return false;
  }
#endif
  //free(reinterpret_cast<void*>(Pointer));
  return vm_dealloc(Pointer);
}

void StaticInit() {
  for (int i = 0; i < StaticTableCount; ++i) {
    staticTables[i].Pointer = Null;
    staticTables[i].Size = 0;
  }
}

void StaticCleanup() {
  for (int i = 0; i < StaticTableCount; ++i) {
    if (staticTables[i].Pointer != Null) {
      vm_dealloc(staticTables[i].Pointer);
      staticTables[i].Pointer = Null;
      staticTables[i].Size = 0;
    }
  }
}