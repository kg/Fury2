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

#include <cstdio>
#include <time.h>
#include "../../../3rdparty/headers/libmng_is_a_beast/libmng.h"
#undef MNG_DECL
#define MNG_DECL __stdcall

class MNG {
public:
  Image* Buffer;
  zeroinit<bool> Paused;
  zeroinit<bool> Dirty;
  autoinit<bool, true> Loop;
  zeroinit<DoubleWord> Delay;
  FILE *hFile;
  mng_handle hMNG;

  MNG();
  MNG(const char *filename);

  ~MNG();

  bool open(const char *filename);

  void sync();

private:
  static mng_ptr MNG_DECL allocate (mng_size_t len);
  static void MNG_DECL free (mng_ptr ptr, mng_size_t len);
  static mng_bool MNG_DECL openstream (mng_handle handle);
  static mng_bool MNG_DECL closestream (mng_handle handle);
  static mng_bool MNG_DECL readdata (mng_handle handle, mng_ptr buf, mng_uint32 len, mng_uint32p read);
  static mng_bool MNG_DECL processheader (mng_handle handle, mng_uint32 width, mng_uint32 height);
  static mng_ptr MNG_DECL getcanvasline (mng_handle handle, mng_uint32 line_number);
  static mng_bool MNG_DECL refresh (mng_handle handle, mng_uint32 x, mng_uint32 y, mng_uint32 width, mng_uint32 height);
  static mng_uint32 MNG_DECL gettickcount (mng_handle handle);
  static mng_bool MNG_DECL settimer (mng_handle handle, mng_uint32 msecs);

  void init();
  void uninit();
};