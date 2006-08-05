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

bool GetDLLRegistered(const WString& Filename);
void SetDLLRegistered(const WString& Filename, bool state);

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
  WString filename(Filename);
  if (GetDLLRegistered(filename))
    return 1;
  VirtualizedThread = GetCurrentThread();
  int result = DoDllInvoke(Filename, "DllRegisterServer");
  SetDLLRegistered(Filename, true);
  VirtualizedThread = 0;
  if (result==0)
    OutputDebugStringA("Register()=0\n");
  else
    OutputDebugStringA("Register()=1\n");
  return result;
}

Export int Unregister(const wchar_t* Filename) {
  VirtualizedThread = GetCurrentThread();
  int result = DoDllInvoke(Filename, "DllUnregisterServer");
  SetDLLRegistered(Filename, false);
  VirtualizedThread = 0;
  if (result==0)
    OutputDebugStringA("Unregister()=0\n");
  else
    OutputDebugStringA("Unregister()=1\n");
  return result;
}

Export void* MemAllocate(unsigned Count) {
//return new unsigned char[Count];
  return malloc(Count);
}

Export void MemDeallocate(void* Ptr) {
  //if (Ptr)
  //  delete[] ((unsigned char*)Ptr);
  if (Ptr)
    free(Ptr);
}