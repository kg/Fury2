/*
SoftFX (Software graphics manipulation library)
Copyright (C) 2003 Kevin Gadd

this library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

this library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include "../header/SoftFX Main.hpp"
#include "../header/LibMNG.hpp"

bool MNG::open(const char *filename) {
  this->hFile = fopen(filename, "rb");
  if (this->hFile) {
    this->init();
    if (this->hMNG) {
      return true;
    }
  }
  return false;
}

void MNG::sync() {
  if (!this->hMNG) return;
  if (this->Dirty) {
    this->Dirty = false;
    mng_display(this->hMNG);
  } else {
    if (mng_display_resume(this->hMNG) != MNG_NEEDTIMERWAIT) {
      if (this->Loop) {
        mng_display_reset(this->hMNG);
      }
    }
  }
}

void MNG::init() {
  if (!this) return;
  DoubleWord udata = reinterpret_cast<DoubleWord>(this);
  this->hMNG = mng_initialize(&udata, MNG::allocate, MNG::free, Null);
  mng_setcb_openstream(this->hMNG, MNG::openstream);
  mng_setcb_closestream(this->hMNG, MNG::closestream);
  mng_setcb_readdata(this->hMNG, MNG::readdata);
  mng_setcb_processheader(this->hMNG, MNG::processheader);
  mng_setcb_gettickcount(this->hMNG, MNG::gettickcount);
  mng_setcb_getcanvasline(this->hMNG, MNG::getcanvasline);
  mng_setcb_refresh(this->hMNG, MNG::refresh);
  mng_setcb_settimer(this->hMNG, MNG::settimer);
}

void MNG::uninit() {
  if (!this) return;
  if (this->hMNG) mng_cleanup(&(this->hMNG));
  this->hMNG = 0;
}

MNG::MNG() {
  this->Buffer = Null;
  this->hFile = Null;
}

MNG::MNG(const char *filename) {
  this->Buffer = Null;
  this->hFile = Null;
  this->open(filename);
}

MNG::~MNG() {
  if (this->hMNG) {
    this->uninit();
  }
  if (this->Buffer) {
    delete this->Buffer;
    this->Buffer = Null;
  }
}

mng_ptr MNG_DECL MNG::allocate  (mng_size_t len) {
  return vm_alloc(len);
}

 void MNG_DECL MNG::free (mng_ptr ptr, mng_size_t len) {
  vm_dealloc(ptr);
  return;
}

 mng_bool MNG_DECL MNG::openstream (mng_handle handle) {
  return MNG_TRUE;
}

 mng_bool MNG_DECL MNG::closestream (mng_handle handle) {
  MNG* pThis = (MNG*)mng_get_userdata(handle);
  if (pThis) {
    if (pThis->hFile) {
      fclose(pThis->hFile);
      pThis->hFile = Null;
      return MNG_TRUE;
    }
  }
  return MNG_FALSE;
}

 mng_bool MNG_DECL MNG::readdata (mng_handle handle, mng_ptr buf, mng_uint32 len, mng_uint32p read) {
  MNG* pThis = (MNG*)mng_get_userdata(handle);
  if (pThis) {
    if (pThis->hFile) {
      if (fread(buf, 1, len, pThis->hFile)) return MNG_TRUE;
    }
  }
  return MNG_FALSE;
}

 mng_bool MNG_DECL MNG::processheader (mng_handle handle, mng_uint32 width, mng_uint32 height) {
  MNG* pThis = (MNG*)mng_get_userdata(handle);
  if (pThis) {
    if (pThis->Buffer) {
      delete pThis->Buffer;
      pThis->Buffer = Null;
    }
    pThis->Buffer = new Image(width, height);
    if (pThis->Buffer) {
      pThis->Dirty = true;
      mng_set_canvasstyle(handle, MNG_CANVAS_BGRA8);
      return MNG_TRUE;
    }
  }
  return MNG_FALSE;
}

 mng_ptr MNG_DECL MNG::getcanvasline  (mng_handle handle, mng_uint32 line_number) {
  MNG* pThis = (MNG*)mng_get_userdata(handle);
  if (pThis) {
    if (pThis->Buffer) {
      return (mng_ptr)pThis->Buffer->pointer(0, line_number);
    }
  }
  return Null;
}

 mng_bool MNG_DECL MNG::refresh  (mng_handle handle, mng_uint32 x, mng_uint32 y, mng_uint32 width, mng_uint32 height) {
  return MNG_TRUE;
}

 mng_uint32 MNG_DECL MNG::gettickcount  (mng_handle handle) {
  return 1000 * (clock() / CLOCKS_PER_SEC);
}

 mng_bool MNG_DECL MNG::settimer  (mng_handle handle, mng_uint32 msecs) {
  MNG* pThis = (MNG*)mng_get_userdata(handle);
  if (pThis) {    pThis->Delay = msecs;  }  return MNG_TRUE;}