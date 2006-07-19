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

typedef int procDllVoid();

bool GetDLLRegistered(const wstring& Filename);
void SetDLLRegistered(const wstring& Filename, bool state);

int DoDllInvoke(const wchar_t* Filename, const char* FunctionName) {
  HMODULE hLibrary = LoadLibraryW(Filename);
  if (hLibrary) {
    procDllVoid* pFunction = (procDllVoid*)GetProcAddress(hLibrary, FunctionName);
    int result = Failure;
    if (pFunction) {
      int returnCode = pFunction();
      if (returnCode == S_OK) {
        result = Success;
      } else {
        result = Failure;
        SetLastError(returnCode);
      }
    }
    FreeLibrary(hLibrary);
    return result;
  } else {
    return Failure;
  }
}

Export int Register(const wchar_t* Filename) {
  wstring filename(Filename);
  if (GetDLLRegistered(filename))
    return 1;
  VirtualizeWrites = true;
  int result = DoDllInvoke(Filename, "DllRegisterServer");
  SetDLLRegistered(Filename, true);
  VirtualizeWrites = false;
  return result;
}

Export int Unregister(const wchar_t* Filename) {
  VirtualizeWrites = true;
  int result = DoDllInvoke(Filename, "DllUnregisterServer");
  SetDLLRegistered(Filename, false);
  VirtualizeWrites = false;
  return result;
}
