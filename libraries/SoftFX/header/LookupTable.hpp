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

inline void* vm_alloc(DoubleWord size);

inline bool vm_dealloc(void* pointer);

const DoubleWord MaxReuseSize = 8192;

// allocates the cache
extern void LookupInitialize();
// allocates a block of cache memory (icky internal version)
extern void* _LookupAllocate(DoubleWord Size);
// deallocates a block of cache memory (icky internal version)
extern bool _LookupDeallocate(void* Pointer);
// deallocates the cache
extern void LookupUninitialize();
// frees unused entries
extern void LookupGC();

template<class Type>Type* LookupAllocate(DoubleWord Size) {
  return reinterpret_cast<Type*>(_LookupAllocate(sizeof(Type) * Size));
}

template<class Type>bool LookupDeallocate(Type* &Pointer) {
  if (!Pointer) return false;

  if (_LookupDeallocate(reinterpret_cast<void*>(Pointer))) {
    Pointer = 0;
    return true;
  } else {
    return false;
  }
}

const DoubleWord StaticTableCount = 16;

const enum StaticTables {
  PolyBuffer,
  EdgeBuffer,
  XBuffer,
  YBuffer,
  XColorBuffer,
  YColorBuffer,
  TextureBuffer,
  MappingBuffer,
  BlitterBuffer,
  ListBuffer,
  EdgeBuffer1,
  EdgeBuffer2,
  EdgeBuffer3,
  EdgeBuffer4,
  PolyClipBuffer1,
  PolyClipBuffer2
};

struct statictable {
  DoubleWord Size;
  void* Pointer;
};

extern statictable staticTables[StaticTableCount];

template<class Type>Type* StaticAllocate(StaticTables Table, DoubleWord Size) {
  Size *= sizeof(Type);
  if (staticTables[Table].Size < Size) {
    vm_dealloc(staticTables[Table].Pointer);
    staticTables[Table].Pointer = vm_alloc(Size);
    staticTables[Table].Size = Size;
  }
  return reinterpret_cast<Type*>(staticTables[Table].Pointer);
}

extern void StaticInit();
extern void StaticCleanup();