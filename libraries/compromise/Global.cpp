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

static const RegistryRoot RegistryRoots[] = {
  {HKEY_CLASSES_ROOT, L"hkcr"},
  {HKEY_CURRENT_USER, L"hkcu"},
  {HKEY_LOCAL_MACHINE, L"hklm"}
};
static const unsigned RegistryRootCount = 3;

unsigned GetRegistryRootCount() {
  return RegistryRootCount;
}

const RegistryRoot& GetRegistryRoot(unsigned index) {
  return RegistryRoots[index];
}

void InstallTrampolines ();

bool VirtualizeWrites() {
  return (GetCurrentThread() == VirtualizedThread);
}

void VRegInitialize() {
  for (unsigned r = 0; r < RegistryRootCount; r++) {
    WString name(RegistryRoots[r].Name);
    pRegistry->Keys[name.ToLower()] = name;
    VirtualRegKey* ptr = get(pRegistry->Keys, name);
    (*pRegistryHandles)[unsigned(RegistryRoots[r].ID)] = new RegKeyHandle(unsigned(RegistryRoots[r].ID), ptr, ptr->Name);
//    RegKeyDefine(unsigned(RegistryRoots[r].ID), get(pRegistry->Keys, name));
  }
}

BOOL WINAPI DllMain(
  HINSTANCE hinstDLL,
  DWORD fdwReason,
  LPVOID lpvReserved
  );

class _Global {
public:
  bool initialized;
  std::vector<WString> RegisteredDLLs;
  unsigned VirtualHandleOffset;

  _Global() {
    VirtualHandleOffset = VirtualRegKeyBase;
    pRegistry = new VirtualRegistry();
    pRegistryHandles = new RegKeyHandles();
    initialized = true;
  }
};

static _Global Global;
VirtualRegistry* pRegistry;
RegKeyHandles* pRegistryHandles;
bool TrampolinesInstalled = false;
bool DisableTrampolines = false;
HANDLE VirtualizedThread = 0;
bool EnableOverrides = false;
HKEY LastEnumKey = 0;
unsigned LastEnumIndex = 0xFFFFFFFF;
unsigned EnumOffset = 0;

unsigned GetRegKeyHandle() {
  return Global.VirtualHandleOffset++;
}

bool GetDLLRegistered(const WString& Filename) {
  std::vector<WString>::iterator iter = std::find(Global.RegisteredDLLs.begin(), Global.RegisteredDLLs.end(), Filename);
  if (iter != Global.RegisteredDLLs.end()) {
    return true;
  }
  return false;
}

void SetDLLRegistered(const WString& Filename, bool State) {
  if (State) {
    SetDLLRegistered(Filename, false);
    Global.RegisteredDLLs.push_back(Filename);
  } else {
    std::vector<WString>::iterator iter = std::find(Global.RegisteredDLLs.begin(), Global.RegisteredDLLs.end(), Filename);
    if (iter != Global.RegisteredDLLs.end()) {
      Global.RegisteredDLLs.erase(iter);
    }
  }
}

Export int GetEnabled() {
  return DisableTrampolines == false;
}

Export void SetEnabled(int Value) {
  DisableTrampolines = (Value == 0);
}

Export int IsSupported() {
  OutputDebugStringA("IsSupported()\n");
  OSVERSIONINFO info;
  memset(&info, 0, sizeof(OSVERSIONINFO));
  info.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
  GetVersionEx(&info);
  return (info.dwPlatformId >= 2);
}

Export int Initialize() {
  if (!EnableOverrides) {
    if (IsSupported()) {
      InstallTrampolines();
      VRegInitialize();
      EnableOverrides = true;
      OutputDebugStringA("Initialize()=1\n");
      return Success;
    }
    OutputDebugStringA("Initialize()=0\n");
    return Failure;
  } else {
    return Failure;
  }
}

Export int Uninitialize() {
  if (EnableOverrides) {
    OutputDebugStringA("Uninitialize()\n");
#ifdef _DEBUG
    VRegDump();
#endif
    VRegFree();
    EnableOverrides = false;
    return Success;
  } else {
    return Failure;
  }
}

WString _wstr(const LPCWSTR ptr, bool escape) {
  if (ptr) {
    int len = wcslen(ptr);
    std::wstringstream buffer;
    if (escape) {
      buffer << L"\"" << WString((const wchar_t*)ptr, len) << L"\"";
    } else {
      buffer << WString((const wchar_t*)ptr, len);
    }
    return WString(buffer.str().c_str());
  } else {
    if (escape) {
      return WString(L"<null>");
    } else {
      return WString(L"");
    }
  }
}

WString _wstr(const LPCWSTR ptr) {
  return _wstr(ptr, true);
}

WString _str(const LPCSTR ptr, bool escape) {
  if (ptr) {
    int len = strlen(ptr);
    std::wstringstream buffer;
    if (escape) {
      buffer << L"\"" << WString((const char*)ptr, len) << L"\"";
      return WString(buffer.str().c_str());
    } else {
      return WString((const char*)ptr, len);
    }
  } else {
    if (escape) {
      return WString(L"<null>");
    } else {
      return WString(L"");
    }
  }
}

WString _str(const LPCSTR ptr) {
  return _str(ptr, true);
}