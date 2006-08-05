/*
Compromise (COM virtual registration library)
Copyright (C) 2006 Kevin Gadd

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

#include "global.hpp"

#pragma warning(disable:4100)   // Trampolines don't use formal parameters.

//
// Extern handlers
//

extern LONG Virtual_RegOpenKeyEx (HKEY a0, WString a1, DWORD a2, REGSAM a3, PHKEY a4);
extern LONG Virtual_RegCreateKeyEx (HKEY a0, WString a1, DWORD a2, DWORD a3, DWORD a4, REGSAM a5, LPSECURITY_ATTRIBUTES a6, PHKEY a7, LPDWORD a8);
extern LONG Virtual_RegSetValueEx (HKEY a0, WString a1, DWORD a2, DWORD a3, BYTE* a4, DWORD a5, bool wide);
extern LONG Virtual_RegQueryValueEx (HKEY a0, WString a1, LPDWORD a2, LPDWORD a3, LPBYTE a4, LPDWORD a5, bool wide);

//
// Trampoline definitions
//

DETOUR_TRAMPOLINE(LONG WINAPI Real_RegCreateKeyExA(HKEY a0,
                                                      LPCSTR a1,
                                                      DWORD a2,
                                                      LPSTR a3,
                                                      DWORD a4,
                                                      REGSAM a5,
                                                      LPSECURITY_ATTRIBUTES a6,
                                                      PHKEY a7,
                                                      LPDWORD a8),
                  RegCreateKeyExA);

DETOUR_TRAMPOLINE(LONG WINAPI Real_RegCreateKeyExW(HKEY a0,
                                                      LPCWSTR a1,
                                                      DWORD a2,
                                                      LPWSTR a3,
                                                      DWORD a4,
                                                      REGSAM a5,
                                                      LPSECURITY_ATTRIBUTES a6,
                                                      PHKEY a7,
                                                      LPDWORD a8),
                  RegCreateKeyExW);

DETOUR_TRAMPOLINE(LONG WINAPI Real_RegCloseKey(HKEY a0),
                  RegCloseKey);

DETOUR_TRAMPOLINE(LONG WINAPI Real_RegDeleteKeyA(HKEY a0,
                                                    LPCSTR a1),
                  RegDeleteKeyA);

DETOUR_TRAMPOLINE(LONG WINAPI Real_RegDeleteKeyW(HKEY a0,
                                                    LPCWSTR a1),
                  RegDeleteKeyW);

DETOUR_TRAMPOLINE(LONG WINAPI Real_RegDeleteValueA(HKEY a0,
                                                      LPCSTR a1),
                  RegDeleteValueA);


DETOUR_TRAMPOLINE(LONG WINAPI Real_RegDeleteValueW(HKEY a0,
                                                      LPCWSTR a1),
                  RegDeleteValueW);

DETOUR_TRAMPOLINE(LONG WINAPI Real_RegEnumKeyExA(HKEY a0,
                                                    DWORD a1,
                                                    LPSTR a2,
                                                    LPDWORD a3,
                                                    LPDWORD a4,
                                                    LPSTR a5,
                                                    LPDWORD a6,
                                                    struct _FILETIME* a7),
                  RegEnumKeyExA);

DETOUR_TRAMPOLINE(LONG WINAPI Real_RegEnumKeyExW(HKEY a0,
                                                    DWORD a1,
                                                    LPWSTR a2,
                                                    LPDWORD a3,
                                                    LPDWORD a4,
                                                    LPWSTR a5,
                                                    LPDWORD a6,
                                                    struct _FILETIME* a7),
                  RegEnumKeyExW);

DETOUR_TRAMPOLINE(LONG WINAPI Real_RegEnumValueA(HKEY a0,
                                                    DWORD a1,
                                                    LPSTR a2,
                                                    LPDWORD a3,
                                                    LPDWORD a4,
                                                    LPDWORD a5,
                                                    LPBYTE a6,
                                                    LPDWORD a7),
                  RegEnumValueA);

DETOUR_TRAMPOLINE(LONG WINAPI Real_RegEnumValueW(HKEY a0,
                                                    DWORD a1,
                                                    LPWSTR a2,
                                                    LPDWORD a3,
                                                    LPDWORD a4,
                                                    LPDWORD a5,
                                                    LPBYTE a6,
                                                    LPDWORD a7),
                  RegEnumValueW);

DETOUR_TRAMPOLINE(LONG WINAPI Real_RegOpenKeyA(HKEY a0,
                                                    LPCSTR a1,
                                                    PHKEY a2),
                  RegOpenKeyA);

DETOUR_TRAMPOLINE(LONG WINAPI Real_RegOpenKeyW(HKEY a0,
                                                    LPCWSTR a1,
                                                    PHKEY a2),
                  RegOpenKeyW);

DETOUR_TRAMPOLINE(LONG WINAPI Real_RegOpenKeyExA(HKEY a0,
                                                    LPCSTR a1,
                                                    DWORD a2,
                                                    REGSAM a3,
                                                    PHKEY a4),
                  RegOpenKeyExA);

DETOUR_TRAMPOLINE(LONG WINAPI Real_RegOpenKeyExW(HKEY a0,
                                                    LPCWSTR a1,
                                                    DWORD a2,
                                                    REGSAM a3,
                                                    PHKEY a4),
                  RegOpenKeyExW);

DETOUR_TRAMPOLINE(LONG WINAPI Real_RegQueryInfoKeyA(HKEY a0,
                                                       LPSTR a1,
                                                       LPDWORD a2,
                                                       LPDWORD a3,
                                                       LPDWORD a4,
                                                       LPDWORD a5,
                                                       LPDWORD a6,
                                                       LPDWORD a7,
                                                       LPDWORD a8,
                                                       LPDWORD a9,
                                                       LPDWORD a10,
                                                       struct _FILETIME* a11),
                  RegQueryInfoKeyA);

DETOUR_TRAMPOLINE(LONG WINAPI Real_RegQueryInfoKeyW(HKEY a0,
                                                       LPWSTR a1,
                                                       LPDWORD a2,
                                                       LPDWORD a3,
                                                       LPDWORD a4,
                                                       LPDWORD a5,
                                                       LPDWORD a6,
                                                       LPDWORD a7,
                                                       LPDWORD a8,
                                                       LPDWORD a9,
                                                       LPDWORD a10,
                                                       struct _FILETIME* a11),
                  RegQueryInfoKeyW);

DETOUR_TRAMPOLINE(LONG WINAPI Real_RegQueryValueA(HKEY a0,
                                                       LPCSTR a1,
                                                       LPBYTE a2,
                                                       LPDWORD a3),
                  RegQueryValueA);

DETOUR_TRAMPOLINE(LONG WINAPI Real_RegQueryValueW(HKEY a0,
                                                       LPCWSTR a1,
                                                       LPBYTE a2,
                                                       LPDWORD a3),
                  RegQueryValueW);

DETOUR_TRAMPOLINE(LONG WINAPI Real_RegQueryValueExA(HKEY a0,
                                                       LPCSTR a1,
                                                       LPDWORD a2,
                                                       LPDWORD a3,
                                                       LPBYTE a4,
                                                       LPDWORD a5),
                  RegQueryValueExA);

DETOUR_TRAMPOLINE(LONG WINAPI Real_RegQueryValueExW(HKEY a0,
                                                       LPCWSTR a1,
                                                       LPDWORD a2,
                                                       LPDWORD a3,
                                                       LPBYTE a4,
                                                       LPDWORD a5),
                  RegQueryValueExW);

DETOUR_TRAMPOLINE(LONG WINAPI Real_RegSetValueExA(HKEY a0,
                                                     LPCSTR a1,
                                                     DWORD a2,
                                                     DWORD a3,
                                                     BYTE* a4,
                                                     DWORD a5),
                  RegSetValueExA);

DETOUR_TRAMPOLINE(LONG WINAPI Real_RegSetValueExW(HKEY a0,
                                                     LPCWSTR a1,
                                                     DWORD a2,
                                                     DWORD a3,
                                                     BYTE* a4,
                                                     DWORD a5),
                  RegSetValueExW);

//
// Trampoline implementations
//

LONG WINAPI Mine_RegCreateKeyExA(HKEY a0,
                                    LPCSTR a1,
                                    DWORD a2,
                                    LPSTR a3,
                                    DWORD a4,
                                    REGSAM a5,
                                    LPSECURITY_ATTRIBUTES a6,
                                    PHKEY a7,
                                    LPDWORD a8)
{
  WString sA1;
  LONG rv = 0;
  if (EnableOverrides && ((RegKeyGetVirtual(a0) && (unsigned(a0) >= VirtualRegKeyBase)) || VirtualizeWrites())) {
    sA1 = _str(a1);
    rv = Virtual_RegCreateKeyEx(a0, _str(a1, false), a2, 0, a4, a5, a6, a7, a8);
  } else {
    rv = Real_RegCreateKeyExA(a0, a1, a2, a3, a4, a5, a6, a7, a8);
  }
  #ifdef _DEBUG
    WString* keyname; RegKeyNameResolve(unsigned(a0), &keyname);
    DebugOut_("RegCreateKeyExA(" << keyname << ", " << sA1 << ", " << a2 << ", " << _str(a3) << ", " << a4 << ", " << ", " << ", " << *a7 << ", " << a8 << ")=" << rv << "\n");
  #endif
  return rv;
}

LONG WINAPI Mine_RegCreateKeyExW(HKEY a0,
                                    LPCWSTR a1,
                                    DWORD a2,
                                    LPWSTR a3,
                                    DWORD a4,
                                    REGSAM a5,
                                    LPSECURITY_ATTRIBUTES a6,
                                    PHKEY a7,
                                    LPDWORD a8)
{
  LONG rv = 0;
  if (EnableOverrides && ((RegKeyGetVirtual(a0) && (unsigned(a0) >= VirtualRegKeyBase)) || VirtualizeWrites())) {
    rv = Virtual_RegCreateKeyEx(a0, _wstr(a1, false), a2, 0, a4, a5, a6, a7, a8);
  } else {
    rv = Real_RegCreateKeyExW(a0, a1, a2, a3, a4, a5, a6, a7, a8);
  }
  #ifdef _DEBUG
    WString* keyname; RegKeyNameResolve(unsigned(a0), &keyname);
    DebugOut_("RegCreateKeyExW(" << keyname << ", " << _wstr(a1) << ", " << a2 << ", " << _wstr(a3) << ", " << a4 << ", " << ", " << ", " << *a7 << ", " << a8 << ")=" << rv << "\n");
  #endif
  return rv;
}

LONG WINAPI Mine_RegCloseKey(HKEY a0)
{
  LONG rv = 0;
  if (!RegKeyGetVirtual(a0))
    rv = Real_RegCloseKey(a0);
  #ifdef _DEBUG
    WString* keyname; RegKeyNameResolve(unsigned(a0), &keyname);
    DebugOut_("RegCloseKey(" << keyname << ")=" << rv << "\n");
  #endif
  RegKeyUndefine(unsigned(a0));
  return rv;
}

LONG WINAPI Mine_RegDeleteKeyA(HKEY a0,
                                  LPCSTR a1)
{
  LONG rv = 0;
  if (EnableOverrides && (RegKeyGetVirtual(a0) || VirtualizeWrites())) {
    WString* keyname; RegKeyNameResolve(unsigned(a0), &keyname);
    wstringstream buffer;
    buffer << keyname;
    if (a1)
      buffer << L"\\" << _str(a1, false);
    VRegKeyCreate(WString(buffer.str().c_str()));
    VRegKeyDelete(WString(buffer.str().c_str()));
    rv = ERROR_SUCCESS;
  } else {
    rv = Real_RegDeleteKeyA(a0, a1);
  }
  #ifdef _DEBUG
    //WString* keyname; RegKeyNameResolve(unsigned(a0), &keyname);
    //DebugOut_("RegDeleteKeyA(" << keyname << ", " << _str(a1) << ")=" << rv << "\n");
  #endif
  return rv;
}

LONG WINAPI Mine_RegDeleteKeyW(HKEY a0,
                                  LPCWSTR a1)
{
  LONG rv = 0;
  if (EnableOverrides && (RegKeyGetVirtual(a0) || VirtualizeWrites())) {
    WString* keyname; RegKeyNameResolve(unsigned(a0), &keyname);
    wstringstream buffer;
    buffer << keyname;
    if (a1)
      buffer << L"\\" << _wstr(a1, false);
    VRegKeyCreate(WString(buffer.str().c_str()));
    VRegKeyDelete(WString(buffer.str().c_str()));
    rv = ERROR_SUCCESS;
  } else {
    rv = Real_RegDeleteKeyW(a0, a1);
  }
  #ifdef _DEBUG
    //WString* keyname; RegKeyNameResolve(unsigned(a0), &keyname);
    //DebugOut_("RegDeleteKeyW(" << keyname << ", " << _wstr(a1) << ")=" << rv << "\n");
  #endif
  return rv;
}

LONG WINAPI Mine_RegDeleteValueA(HKEY a0,
                                    LPCSTR a1)
{
  LONG rv = 0;
  if (EnableOverrides && (RegKeyGetVirtual(a0) || VirtualizeWrites())) {
  } else {
    rv = Real_RegDeleteValueA(a0, a1);
  }
  #ifdef _DEBUG
    //WString* keyname; RegKeyNameResolve(unsigned(a0), &keyname);
    //DebugOut_("RegDeleteValueA(" << keyname << ", " << _str(a1) << ")=" << rv << "\n");
  #endif
  return rv;
}

LONG WINAPI Mine_RegDeleteValueW(HKEY a0,
                                    LPCWSTR a1)
{
  LONG rv = 0;
  if (EnableOverrides && (RegKeyGetVirtual(a0) || VirtualizeWrites())) {
  } else {
    rv = Real_RegDeleteValueW(a0, a1);
  }
  #ifdef _DEBUG
    //WString* keyname; RegKeyNameResolve(unsigned(a0), &keyname);
    //DebugOut_("RegDeleteValueW(" << keyname << ", " << _wstr(a1) << ")=" << rv << "\n");
  #endif
  return rv;
}

LONG WINAPI Mine_RegEnumKeyExA(HKEY a0,
                                  DWORD a1,
                                  LPSTR a2,
                                  LPDWORD a3,
                                  LPDWORD a4,
                                  LPSTR a5,
                                  LPDWORD a6,
                                  struct _FILETIME* a7)
{
  LONG rv = 0;
  WString* keyname; RegKeyNameResolve(unsigned(a0), &keyname);
  if (EnableOverrides && (RegKeyGetVirtual(a0) || VirtualizeWrites())) {
    if (RegKeyIsDeleted(unsigned(a0)))
      rv = ERROR_NO_MORE_ITEMS;
    else {
      if (a0 > (HKEY)VirtualRegKeyBase)
        return ERROR_NO_MORE_ITEMS;
      bool repeat = true;
      while (repeat) {
        rv = Real_RegEnumKeyExA(a0, a1, a2, a3, a4, a5, a6, a7);
        if (rv == 0) {
          wstringstream buffer;
          buffer << keyname;
          if (a2)
            buffer << L"\\" << _str(a2, false);
          if (VRegKeyIsDeleted(WString(buffer.str().c_str()))) {
            a1++;
          } else {
            repeat = false;
          }
        } else {
          repeat = false;
        }
      }
    }
  } else {
    rv = Real_RegEnumKeyExA(a0, a1, a2, a3, a4, a5, a6, a7);
  }
  //DebugOut_("RegEnumKeyExA(" << keyname << ", " << a1 << ", " << unsigned(a2) << ", " << a3 << ", " << a4 << ", " << unsigned(a5) << ", " << a6 << ",)=" << rv << "\n");
  return rv;
}

LONG WINAPI Mine_RegEnumKeyExW(HKEY a0,
                                  DWORD a1,
                                  LPWSTR a2,
                                  LPDWORD a3,
                                  LPDWORD a4,
                                  LPWSTR a5,
                                  LPDWORD a6,
                                  struct _FILETIME* a7)
{
  LONG rv = 0;
  WString* keyname; RegKeyNameResolve(unsigned(a0), &keyname);
  if (EnableOverrides && (RegKeyGetVirtual(a0) || VirtualizeWrites())) {
    if (RegKeyIsDeleted(unsigned(a0)))
      rv = ERROR_NO_MORE_ITEMS;
    else {
      if (a0 > (HKEY)VirtualRegKeyBase)
        return ERROR_NO_MORE_ITEMS;
      bool repeat = true;
      while (repeat) {
        rv = Real_RegEnumKeyExW(a0, a1, a2, a3, a4, a5, a6, a7);
        if (rv == 0) {
          wstringstream buffer;
          buffer << keyname;
          if (a2)
            buffer << L"\\" << _wstr(a2, false);
          if (VRegKeyIsDeleted(WString(buffer.str().c_str()))) {
            a1++;
          } else {
            repeat = false;
          }
        } else {
          repeat = false;
        }
      }
    }
  } else {
    rv = Real_RegEnumKeyExW(a0, a1, a2, a3, a4, a5, a6, a7);
  }
  //DebugOut_("RegEnumKeyExW(" << keyname << ", " << a1 << ", " << unsigned(a2) << ", " << a3 << ", " << a4 << ", " << unsigned(a5) << ", " << a6 << ",)=" << rv << "\n");
  return rv;
}

LONG WINAPI Mine_RegEnumValueA(HKEY a0,
                                  DWORD a1,
                                  LPSTR a2,
                                  LPDWORD a3,
                                  LPDWORD a4,
                                  LPDWORD a5,
                                  LPBYTE a6,
                                  LPDWORD a7)
{
  LONG rv = 0;
  DebugOut("RegEnumValueExA\n");
  if (!RegKeyGetVirtual(a0))
    rv = Real_RegEnumValueA(a0, a1, a2, a3, a4, a5, a6, a7);
  return rv;
}

LONG WINAPI Mine_RegEnumValueW(HKEY a0,
                                  DWORD a1,
                                  LPWSTR a2,
                                  LPDWORD a3,
                                  LPDWORD a4,
                                  LPDWORD a5,
                                  LPBYTE a6,
                                  LPDWORD a7)
{
  LONG rv = 0;
  DebugOut("RegEnumValueExW\n");
  if (!RegKeyGetVirtual(a0))
    rv = Real_RegEnumValueW(a0, a1, a2, a3, a4, a5, a6, a7);
  return rv;
}

LONG WINAPI Mine_RegOpenKeyA(HKEY a0,
                                  LPCSTR a1,
                                  PHKEY a2)
{
  if (a0 == 0) {
    DebugOut("RegOpenKeyA(0, ...)=ERROR_INVALID_HANDLE\n");
    return ERROR_INVALID_HANDLE;
  }
  if (a2)
    *a2 = 0;
  LONG rv = 0;
  HKEY theKey = a0;
  WString* keyname = 0;
  if ((a1 != Null) && (strstr(a1, "CLSID\\{") == a1)) {
    // Hack to fix VB6 stupidity
    RegKeyDefine(unsigned(a0), unsigned(HKEY_CLASSES_ROOT));
    theKey = HKEY_CLASSES_ROOT;
  } else if ((a1 != Null) && (strstr(a1, "TYPELIB\\{") == a1)) {
    // Hack to fix VB6 stupidity
    RegKeyDefine(unsigned(a0), unsigned(HKEY_CLASSES_ROOT));
    theKey = HKEY_CLASSES_ROOT;
  }
  bool virt = RegKeyGetVirtual(a0);
  if ((a1 != Null) && (unsigned(a0) < 0x90000000)) {
    wstringstream buffer;
    buffer << "hkcr\\";
    buffer << _str(a1, false);
    if (VRegKeyResolve(WString(buffer.str().c_str()))) {
      RegKeyDefine(unsigned(a0), unsigned(HKEY_CLASSES_ROOT));
      theKey = HKEY_CLASSES_ROOT;
    }
  }
  RegKeyNameResolve(unsigned(a0), &keyname);
  if (EnableOverrides && (virt || VirtualizeWrites())) {
    rv = Virtual_RegOpenKeyEx(theKey, _str(a1, false), 0, 0, a2);
    if (rv == ERROR_SUCCESS)
      assert(*a2 != 0);
    if ((rv != ERROR_SUCCESS) && (a0 < (HKEY)VirtualRegKeyBase) && (rv != ERROR_ACCESS_DENIED))
      rv = Real_RegOpenKeyA(a0, a1, a2);
  } else {
    rv = Real_RegOpenKeyA(a0, a1, a2);
  }
  RegKeyNameResolve(unsigned(theKey), &keyname);
  if (a1 == Null) {
    if (*a2 != Null) {
      RegKeyNameDefine(unsigned(*a2), *keyname);
    }
  } else if (*a2 != Null) {
    wstringstream buffer;
    buffer << keyname << L"\\" << _str(a1, false);
    RegKeyNameDefine(unsigned(*a2), WString(buffer.str().c_str()));
  }
  DebugOut_("RegOpenKeyA(" << theKey << "=" << keyname << ", " << _str(a1) << ", " << *a2 << ")=" << rv << "\n");
  return rv;
}

LONG WINAPI Mine_RegOpenKeyW(HKEY a0,
                                  LPCWSTR a1,
                                  PHKEY a2)
{
  if (a0 == 0) {
    DebugOut("RegOpenKeyW(0, ...)=ERROR_INVALID_HANDLE\n");
    return ERROR_INVALID_HANDLE;
  }
  if (a2)
    *a2 = 0;
  LONG rv = 0;
  HKEY theKey = a0;
  WString* keyname = Null;
  if ((a1 != Null) && (wcsstr(a1, L"CLSID\\{") == a1)) {
    // Hack to fix VB6 stupidity
    RegKeyDefine(unsigned(a0), unsigned(HKEY_CLASSES_ROOT));
    theKey = HKEY_CLASSES_ROOT;
  } else if ((a1 != Null) && (wcsstr(a1, L"TYPELIB\\{") == a1)) {
    // Hack to fix VB6 stupidity
    RegKeyDefine(unsigned(a0), unsigned(HKEY_CLASSES_ROOT));
    theKey = HKEY_CLASSES_ROOT;
  }
  if ((a1 != Null) && (unsigned(a0) < 0x90000000)) {
    wstringstream buffer;
    buffer << "hkcr\\";
    buffer << _wstr(a1, false);
    if (VRegKeyResolve(WString(buffer.str().c_str()))) {
      RegKeyDefine(unsigned(a0), unsigned(HKEY_CLASSES_ROOT));
      theKey = HKEY_CLASSES_ROOT;
    }
  }
  RegKeyNameResolve(unsigned(a0), &keyname);
  bool virt = RegKeyGetVirtual(theKey);
  if (EnableOverrides && (virt || VirtualizeWrites())) {
    rv = Virtual_RegOpenKeyEx(theKey, _wstr(a1, false), 0, 0, a2);
    if (rv == ERROR_SUCCESS)
      assert(*a2 != 0);
    if ((rv != ERROR_SUCCESS) && (a0 < (HKEY)VirtualRegKeyBase) && (rv != ERROR_ACCESS_DENIED))
      rv = Real_RegOpenKeyW(a0, a1, a2);
  } else {
    rv = Real_RegOpenKeyW(a0, a1, a2);
  }
  RegKeyNameResolve(unsigned(theKey), &keyname);
  if (a1 == Null) {
    if (*a2 != Null) {
      RegKeyNameDefine(unsigned(*a2), *keyname);
    }
  } else if (*a2 != Null) {
    wstringstream buffer;
    buffer << keyname << L"\\" << _wstr(a1, false);
    RegKeyNameDefine(unsigned(*a2), WString(buffer.str().c_str()));
  }
  DebugOut_("RegOpenKeyW(" << theKey << "=" << keyname << ", " << _wstr(a1) << ", " << *a2 << ")=" << rv << "\n");
  return rv;
}

LONG WINAPI Mine_RegOpenKeyExA(HKEY a0,
                                  LPCSTR a1,
                                  DWORD a2,
                                  REGSAM a3,
                                  PHKEY a4)
{
  if (a0 == 0) {
    DebugOut("RegOpenKeyExA(0, ...)=ERROR_INVALID_HANDLE\n");
    return ERROR_INVALID_HANDLE;
  }
  if (a4)
    *a4 = 0;
  LONG rv = 0;
  HKEY theKey = a0;
  WString* keyname = Null;
  if ((a1 != Null) && (strstr(a1, "CLSID\\{") == a1)) {
    // Hack to fix VB6 stupidity
    RegKeyDefine(unsigned(a0), unsigned(HKEY_CLASSES_ROOT));
    theKey = HKEY_CLASSES_ROOT;
  } else if ((a1 != Null) && (strstr(a1, "TYPELIB\\{") == a1)) {
    // Hack to fix VB6 stupidity
    RegKeyDefine(unsigned(a0), unsigned(HKEY_CLASSES_ROOT));
    theKey = HKEY_CLASSES_ROOT;
  }
  if ((a1 != Null) && (unsigned(a0) < 0x90000000)) {
    wstringstream buffer;
    buffer << "hkcr\\";
    buffer << _str(a1, false);
    if (VRegKeyResolve(WString(buffer.str().c_str()))) {
      RegKeyDefine(unsigned(a0), unsigned(HKEY_CLASSES_ROOT));
      theKey = HKEY_CLASSES_ROOT;
    }
  }
  RegKeyNameResolve(unsigned(a0), &keyname);
  bool virt = RegKeyGetVirtual(a0);
  if (EnableOverrides && (virt || VirtualizeWrites())) {
    rv = Virtual_RegOpenKeyEx(theKey, _str(a1, false), a2, a3, a4);
    if (rv == ERROR_SUCCESS)
      assert(*a4 != 0);
    if ((rv != ERROR_SUCCESS) && (a0 < (HKEY)VirtualRegKeyBase) && (rv != ERROR_ACCESS_DENIED)) {
      rv = Real_RegOpenKeyExA(a0, a1, a2, a3, a4);
      theKey = a0;
    }
  } else {
    rv = Real_RegOpenKeyExA(a0, a1, a2, a3, a4);
    theKey = a0;
  }
  RegKeyNameResolve(unsigned(theKey), &keyname);
  if (a1 == Null) {
    if (*a4 != Null) {
      RegKeyDefine(unsigned(*a4), unsigned(a0));
    }
  } else if (*a4 != Null) {
    wstringstream buffer;
    buffer << keyname << L"\\" << _str(a1, false);
    RegKeyNameDefine(unsigned(*a4), WString(buffer.str().c_str()));
  }
  DebugOut_("RegOpenKeyExA(" << theKey << "=" << keyname << ", " << _str(a1) << ", " << a2 << ", " << a3 << ", " << *a4 << ")=" << rv << "\n");
  return rv;
}

LONG WINAPI Mine_RegOpenKeyExW(HKEY a0,
                                  LPCWSTR a1,
                                  DWORD a2,
                                  REGSAM a3,
                                  PHKEY a4)
{
  if (a0 == 0) {
    DebugOut("RegOpenKeyExW(0, ...)=ERROR_INVALID_HANDLE\n");
    return ERROR_INVALID_HANDLE;
  }
  if (a4)
    *a4 = 0;
  LONG rv = 0;
  HKEY theKey = a0;
  WString* keyname = Null;
  if ((a1 != Null) && (wcsstr(a1, L"CLSID\\{") == a1)) {
    // Hack to fix VB6 stupidity
    RegKeyDefine(unsigned(a0), unsigned(HKEY_CLASSES_ROOT));
    theKey = HKEY_CLASSES_ROOT;
  } else if ((a1 != Null) && (wcsstr(a1, L"TYPELIB\\{") == a1)) {
    // Hack to fix VB6 stupidity
    RegKeyDefine(unsigned(a0), unsigned(HKEY_CLASSES_ROOT));
    theKey = HKEY_CLASSES_ROOT;
  }
  if ((a1 != Null) && (unsigned(a0) < 0x90000000)) {
    wstringstream buffer;
    buffer << "hkcr\\";
    buffer << _wstr(a1, false);
    if (VRegKeyResolve(WString(buffer.str().c_str()))) {
      RegKeyDefine(unsigned(a0), unsigned(HKEY_CLASSES_ROOT));
      theKey = HKEY_CLASSES_ROOT;
    }
  }
  RegKeyNameResolve(unsigned(a0), &keyname);
  bool virt = RegKeyGetVirtual(theKey);
  if (EnableOverrides && (virt || VirtualizeWrites())) {
    rv = Virtual_RegOpenKeyEx(theKey, _wstr(a1, false), a2, a3, a4);
    if (rv == ERROR_SUCCESS)
      assert(*a4 != 0);
    if ((rv != ERROR_SUCCESS) && (a0 < (HKEY)VirtualRegKeyBase) && (rv != ERROR_ACCESS_DENIED)) {
      rv = Real_RegOpenKeyExW(a0, a1, a2, a3, a4);
      theKey = a0;
    }
  } else {
    rv = Real_RegOpenKeyExW(a0, a1, a2, a3, a4);
    theKey = a0;
  }
  RegKeyNameResolve(unsigned(theKey), &keyname);
  if (a1 == Null) {
    if (*a4 != Null) {
      RegKeyDefine(unsigned(*a4), unsigned(a0));
    }
  } else if (*a4 != Null) {
    wstringstream buffer;
    buffer << keyname << L"\\" << _wstr(a1, false);
    RegKeyNameDefine(unsigned(*a4), WString(buffer.str().c_str()));
  }
  DebugOut_("RegOpenKeyExW(" << theKey << "=" << keyname << ", " << _wstr(a1) << ", " << a2 << ", " << a3 << ", " << *a4 << ")=" << rv << "\n");
  return rv;
}

LONG WINAPI Mine_RegQueryInfoKeyA(HKEY a0,
                                     LPSTR a1,
                                     LPDWORD a2,
                                     LPDWORD a3,
                                     LPDWORD a4,
                                     LPDWORD a5,
                                     LPDWORD a6,
                                     LPDWORD a7,
                                     LPDWORD a8,
                                     LPDWORD a9,
                                     LPDWORD a10,
                                     struct _FILETIME* a11)
{
    LONG rv = 0;
    DebugOut("RegQueryInfoKeyA\n");
    if (!RegKeyGetVirtual(a0))
      rv = Real_RegQueryInfoKeyA(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11);
    return rv;
}

LONG WINAPI Mine_RegQueryInfoKeyW(HKEY a0,
                                     LPWSTR a1,
                                     LPDWORD a2,
                                     LPDWORD a3,
                                     LPDWORD a4,
                                     LPDWORD a5,
                                     LPDWORD a6,
                                     LPDWORD a7,
                                     LPDWORD a8,
                                     LPDWORD a9,
                                     LPDWORD a10,
                                     struct _FILETIME* a11)
{
    LONG rv = 0;
    DebugOut("RegQueryInfoKeyW\n");
    if (!RegKeyGetVirtual(a0))
      rv = Real_RegQueryInfoKeyW(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11);
    return rv;
}

LONG WINAPI Mine_RegQueryValueA(HKEY a0,
                                     LPCSTR a1,
                                     LPBYTE a2,
                                     LPDWORD a3)
{
  LONG rv = 0;
  bool virt = RegKeyGetVirtual(a0);
  if (EnableOverrides && (virt || VirtualizeWrites())) {
    rv = Virtual_RegQueryValueEx(a0, _str(a1, false), 0, 0, a2, a3, false);
    if ((rv != ERROR_SUCCESS) && (a0 < (HKEY)VirtualRegKeyBase))
      rv = Real_RegQueryValueA(a0, a1, a2, a3);
  } else {
    rv = Real_RegQueryValueA(a0, a1, a2, a3);
  }
  #ifdef _DEBUG
    WString* keyname; RegKeyNameResolve(unsigned(a0), &keyname);
    DebugOut_("RegQueryValueA(" << a0 << "=" << keyname << ", " << _str(a1) << ", " << a2 << ", " << *a3 << ")=" << rv << "\n");
    if (rv == 0)
      DebugOut_("result: " << _str((const char*)a2) << "\n");
  #endif
  return rv;
}

LONG WINAPI Mine_RegQueryValueW(HKEY a0,
                                     LPCWSTR a1,
                                     LPBYTE a2,
                                     LPDWORD a3)
{
  LONG rv = 0;
  bool virt = RegKeyGetVirtual(a0);
  if (EnableOverrides && (virt || VirtualizeWrites())) {
    rv = Virtual_RegQueryValueEx(a0, _wstr(a1, false), 0, 0, a2, a3, true);
    if ((rv != ERROR_SUCCESS) && (a0 < (HKEY)VirtualRegKeyBase))
      rv = Real_RegQueryValueW(a0, a1, a2, a3);
  } else {
    rv = Real_RegQueryValueW(a0, a1, a2, a3);
  }
#if _DEBUG
  WString *keyname;
  RegKeyNameResolve(unsigned(a0), &keyname);
  DebugOut_("RegQueryValueW(" << a0 << "=" << keyname << ", " << _wstr(a1) << ", " << a2 << ", " << *a3 << ")=" << rv << "\n");
  if (rv == 0)
    DebugOut_("result: " << _wstr((const wchar_t*)a2) << "\n");
#endif
  return rv;
}

LONG WINAPI Mine_RegQueryValueExA(HKEY a0,
                                     LPCSTR a1,
                                     LPDWORD a2,
                                     LPDWORD a3,
                                     LPBYTE a4,
                                     LPDWORD a5)
{
  LONG rv = 0;
  bool virt = RegKeyGetVirtual(a0);
  if (EnableOverrides && (virt || VirtualizeWrites())) {
    rv = Virtual_RegQueryValueEx(a0, _str(a1, false), a2, a3, a4, a5, false);
    if ((rv != ERROR_SUCCESS) && (a0 < (HKEY)VirtualRegKeyBase))
      rv = Real_RegQueryValueExA(a0, a1, a2, a3, a4, a5);
  } else {
    rv = Real_RegQueryValueExA(a0, a1, a2, a3, a4, a5);
  }
  #ifdef _DEBUG
    WString* keyname; RegKeyNameResolve(unsigned(a0), &keyname);
    unsigned pL = 0;
    if (a5)
      pL = *a5;
    DebugOut_("RegQueryValueExA(" << a0 << "=" << keyname << ", " << _str(a1) << ", " << a2 << ", " << a3 << ", " << a4 << ", " << pL << ")=" << rv << "\n");
    if ((rv == 0) && (pL > 0))
      DebugOut_("result: " << _str((const char*)a4, true) << "\n");
  #endif
  return rv;
}

LONG WINAPI Mine_RegQueryValueExW(HKEY a0,
                                     LPCWSTR a1,
                                     LPDWORD a2,
                                     LPDWORD a3,
                                     LPBYTE a4,
                                     LPDWORD a5)
{
  LONG rv = 0;
  bool virt = RegKeyGetVirtual(a0);
  if (EnableOverrides && (virt || VirtualizeWrites())) {
    rv = Virtual_RegQueryValueEx(a0, _wstr(a1, false), a2, a3, a4, a5, true);
    if ((rv != ERROR_SUCCESS) && (a0 < (HKEY)VirtualRegKeyBase))
      rv = Real_RegQueryValueExW(a0, a1, a2, a3, a4, a5);
  } else {
    rv = Real_RegQueryValueExW(a0, a1, a2, a3, a4, a5);
  }
  #ifdef _DEBUG
    WString* keyname; RegKeyNameResolve(unsigned(a0), &keyname);
    unsigned pL = 0;
    if (a5)
      pL = *a5;
    DebugOut_("RegQueryValueExW(" << a0 << "=" << keyname << ", " << _wstr(a1) << ", " << a2 << ", " << a3 << ", " << a4 << ", " << pL << ")=" << rv << "\n");
    if ((rv == 0) && (pL > 0))
      DebugOut_("result: " << _wstr((const wchar_t*)a4, true) << "\n");
  #endif
  return rv;
}

LONG WINAPI Mine_RegSetValueExA(HKEY a0,
                                   LPCSTR a1,
                                   DWORD a2,
                                   DWORD a3,
                                   BYTE* a4,
                                   DWORD a5)
{
  LONG rv = 0;
  if (EnableOverrides && (RegKeyGetVirtual(a0) || VirtualizeWrites())) {
    rv = Virtual_RegSetValueEx(a0, _str(a1, false), a2, a3, a4, a5, false);
  } else {
    rv = Real_RegSetValueExA(a0, a1, a2, a3, a4, a5);
  }
  #ifdef _DEBUG
    WString* keyname; RegKeyNameResolve(unsigned(a0), &keyname);
    DebugOut_("RegSetValueExA(" << a0 << "=" << keyname << ", " << _str(a1) << ", " << a2 << ", " << a3 << ", " << a4 << ", " << a5 << ")=" << rv << "\n");
  #endif
  return rv;
}

LONG WINAPI Mine_RegSetValueExW(HKEY a0,
                                   LPCWSTR a1,
                                   DWORD a2,
                                   DWORD a3,
                                   BYTE* a4,
                                   DWORD a5)
{
  LONG rv = 0;
  if (EnableOverrides && (RegKeyGetVirtual(a0) || VirtualizeWrites())) {
    rv = Virtual_RegSetValueEx(a0, _wstr(a1, false), a2, a3, a4, a5, true);
  } else {
    rv = Real_RegSetValueExW(a0, a1, a2, a3, a4, a5);
  }
  #ifdef _DEBUG
    WString* keyname; RegKeyNameResolve(unsigned(a0), &keyname);
    DebugOut_("RegSetValueExW(" << a0 << "=" << keyname << ", " << _wstr(a1) << ", " << a2 << ", " << a3 << ", " << a4 << ", " << a5 << ")=" << rv << "\n");
  #endif
  return rv;
}

//
// Trampoline globals
//

Export extern int GetEnabled();

void InstallTrampolines() {
  if (GetEnabled() == 0)
    return;
  if (TrampolinesInstalled) 
    return;
  TrampolinesInstalled = true;
  OutputDebugStringA("InstallTrampolines()\n");
  DetourFunctionWithTrampoline((PBYTE)Real_RegCreateKeyExA,
                               (PBYTE)Mine_RegCreateKeyExA);
  DetourFunctionWithTrampoline((PBYTE)Real_RegCreateKeyExW,
                               (PBYTE)Mine_RegCreateKeyExW);
  DetourFunctionWithTrampoline((PBYTE)Real_RegCloseKey,
                               (PBYTE)Mine_RegCloseKey);
  DetourFunctionWithTrampoline((PBYTE)Real_RegDeleteKeyA,
                               (PBYTE)Mine_RegDeleteKeyA);
  DetourFunctionWithTrampoline((PBYTE)Real_RegDeleteKeyW,
                               (PBYTE)Mine_RegDeleteKeyW);
  DetourFunctionWithTrampoline((PBYTE)Real_RegDeleteValueA,
                               (PBYTE)Mine_RegDeleteValueA);
  DetourFunctionWithTrampoline((PBYTE)Real_RegDeleteValueW,
                               (PBYTE)Mine_RegDeleteValueW);
  DetourFunctionWithTrampoline((PBYTE)Real_RegEnumKeyExA,
                               (PBYTE)Mine_RegEnumKeyExA);
  DetourFunctionWithTrampoline((PBYTE)Real_RegEnumKeyExW,
                               (PBYTE)Mine_RegEnumKeyExW);
  DetourFunctionWithTrampoline((PBYTE)Real_RegEnumValueA,
                               (PBYTE)Mine_RegEnumValueA);
  DetourFunctionWithTrampoline((PBYTE)Real_RegEnumValueW,
                               (PBYTE)Mine_RegEnumValueW);
  //DetourFunctionWithTrampoline((PBYTE)Real_RegOpenKeyA,
  //                             (PBYTE)Mine_RegOpenKeyA);
  //DetourFunctionWithTrampoline((PBYTE)Real_RegOpenKeyW,
  //                             (PBYTE)Mine_RegOpenKeyW);
  DetourFunctionWithTrampoline((PBYTE)Real_RegOpenKeyExA,
                               (PBYTE)Mine_RegOpenKeyExA);
  DetourFunctionWithTrampoline((PBYTE)Real_RegOpenKeyExW,
                               (PBYTE)Mine_RegOpenKeyExW);
  DetourFunctionWithTrampoline((PBYTE)Real_RegQueryInfoKeyA,
                               (PBYTE)Mine_RegQueryInfoKeyA);
  DetourFunctionWithTrampoline((PBYTE)Real_RegQueryInfoKeyW,
                               (PBYTE)Mine_RegQueryInfoKeyW);
  DetourFunctionWithTrampoline((PBYTE)Real_RegQueryValueA,
                               (PBYTE)Mine_RegQueryValueA);
  DetourFunctionWithTrampoline((PBYTE)Real_RegQueryValueW,
                               (PBYTE)Mine_RegQueryValueW);
  DetourFunctionWithTrampoline((PBYTE)Real_RegQueryValueExA,
                               (PBYTE)Mine_RegQueryValueExA);
  DetourFunctionWithTrampoline((PBYTE)Real_RegQueryValueExW,
                               (PBYTE)Mine_RegQueryValueExW);
  DetourFunctionWithTrampoline((PBYTE)Real_RegSetValueExA,
                               (PBYTE)Mine_RegSetValueExA);
  DetourFunctionWithTrampoline((PBYTE)Real_RegSetValueExW,
                               (PBYTE)Mine_RegSetValueExW);
}