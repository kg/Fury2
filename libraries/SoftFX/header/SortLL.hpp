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

template <class T> T* SortLinkedList(T *Head) {
  T *p, *q, *e, *tail;
  DoubleWord in_size = 1, num_merges = 0, p_size = 0, q_size = 0, i = 0;

  if (!Head) return Null;

  p = Head;
  while (p) {
    p->pSortedNext = p->pNext;
    p = p->pNext;
  }

  while (true) {
    p = Head;
    Head = Null;
    tail = Null;
    num_merges = 0;
    while (p) {
      num_merges++;
      q = p;
      p_size = 0;
      for (i = 0; i < in_size; i++) {
        p_size++;
        q = q->pSortedNext;
        if (!q) break;
      }
      q_size = in_size;
      while ((p_size > 0) || ((q_size > 0) && (q))) {
        if (p_size == 0) {
          e = q;
          if (q) q = q->pSortedNext;
          q_size--;
        } else if ((q_size == 0) || (!q) || (*p <= *q)) {
          e = p;
          if (p) p = p->pSortedNext;
          p_size--;
        } else {
          e = q;
          if (q) q = q->pSortedNext;
          q_size--;
        }
        if (tail) {
          tail->pSortedNext = e;
        } else {
          Head = e;
        }
        tail = e;
      }
      p = q;
    }
    if (tail) tail->pSortedNext = Null;
    if (num_merges <= 1) {
      return Head;
    }
    in_size *= 2;
  }
  return Null;
}
