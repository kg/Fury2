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

HeapClass Heap;
bool heapInUse = false;

void InitHeap() {
  heapInUse = true;
  Heap.clear();
  heapInUse = false;
  return;
}
void CleanupHeap() {
  heapInUse = true;
  HeapClass::iterator iter;
  Image* ptr;
  iter = Heap.begin();
  while (iter != Heap.end()) {
    ptr = *iter;
    if (ptr) {
      delete ptr;
    }
    iter++;
  }
  Heap.clear();
  heapInUse = false;
  return;
}
void AddToHeap(Image* Image) {
  if (heapInUse) return;
  heapInUse = true;
  if (find(Heap.begin(), Heap.end(), Image) != Heap.end()) {
    heapInUse = false;
    return;
  } else {
    Heap.push_back(Image);
    heapInUse = false;
    return;
  }
  heapInUse = false;
  return;
}
void RemoveFromHeap(Image* Image) {
  if (heapInUse) return;
  heapInUse = true;
  HeapClass::iterator iter;
  iter = find(Heap.begin(), Heap.end(), Image);
  if (iter != Heap.end()) {
    Heap.remove(Image);
  }
  heapInUse = false;
  return;
}